#pragma once

#include <cerrno>
#include <cstddef>
#include <cstring>
#include <iostream>
#include <stdexcept>
#include <string>

#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/un.h>
#include <unistd.h>

// These names are retained as direction/role identifiers so existing call
// sites do not need to change. Communication now uses one full-duplex Unix
// domain SOCK_SEQPACKET socket rather than two FIFOs.
constexpr const char* PIPE_OFA_TO_OFO = "openfast_pos.fifo";
constexpr const char* PIPE_OFO_TO_OFA = "openfoam_forces.fifo";
constexpr const char* IPC_SOCKET_PATH = "openfast_openfoam.sock";

// A simple tagged message for debuggability.
struct SimMessage {
    int    step;           // Simulation timestep
    int    n_values;       // Number of doubles following
    double values[64];     // Coupling data
    char   tag[32];        // Human-readable label for debugging
};

class PipeChannel {
public:
    // Retained for source compatibility. SOCK_SEQPACKET paths are created by
    // bind(), so this now only removes a stale socket left by a previous run.
    // Call it only before either endpoint constructs its PipeChannel.
    static void create_pipes() {
        remove_stale_socket();
    }

    // Existing direction arguments select the endpoint role:
    //   OpenFAST: PipeChannel(PIPE_OFA_TO_OFO, PIPE_OFO_TO_OFA) -> client
    //   OpenFOAM: PipeChannel(PIPE_OFO_TO_OFA, PIPE_OFA_TO_OFO) -> server
    //
    // Construction blocks until the other endpoint is available, matching the
    // startup behavior of the previous FIFO implementation.
    PipeChannel(const char* write_path, const char* read_path) {
        const bool openfast_side =
            std::strcmp(write_path, PIPE_OFA_TO_OFO) == 0 &&
            std::strcmp(read_path, PIPE_OFO_TO_OFA) == 0;
        const bool openfoam_side =
            std::strcmp(write_path, PIPE_OFO_TO_OFA) == 0 &&
            std::strcmp(read_path, PIPE_OFA_TO_OFO) == 0;

        if (!openfast_side && !openfoam_side) {
            throw std::invalid_argument(
                "PipeChannel paths do not identify a valid endpoint");
        }

        try {
            if (openfoam_side) {
                accept_client();
            } else {
                connect_to_server();
            }
        } catch (...) {
            cleanup();
            throw;
        }
    }

    PipeChannel(const PipeChannel&) = delete;
    PipeChannel& operator=(const PipeChannel&) = delete;

    ~PipeChannel() {
        cleanup();
    }

    void send(const SimMessage& msg) {
        if (msg.n_values < 0 || msg.n_values > 64) {
            throw std::runtime_error("Invalid SimMessage n_values");
        }

        ssize_t bytes;
        do {
            bytes = ::send(fd_, &msg, sizeof(msg), MSG_NOSIGNAL);
        } while (bytes < 0 && errno == EINTR);

        if (bytes < 0) {
            throw_system_error("seqpacket send failed");
        }
        if (bytes != static_cast<ssize_t>(sizeof(msg))) {
            throw std::runtime_error("Incomplete seqpacket send");
        }
    }

    SimMessage recv() {
        SimMessage msg{};
        iovec data{};
        data.iov_base = &msg;
        data.iov_len = sizeof(msg);

        msghdr header{};
        header.msg_iov = &data;
        header.msg_iovlen = 1;

        ssize_t bytes;
        do {
            bytes = ::recvmsg(fd_, &header, 0);
        } while (bytes < 0 && errno == EINTR);

        if (bytes < 0) {
            throw_system_error("seqpacket receive failed");
        }
        if (bytes == 0) {
            throw std::runtime_error("Socket closed by remote process");
        }
        if ((header.msg_flags & MSG_TRUNC) != 0 ||
            bytes != static_cast<ssize_t>(sizeof(msg))) {
            throw std::runtime_error("Invalid SimMessage packet size");
        }
        if (msg.n_values < 0 || msg.n_values > 64) {
            throw std::runtime_error("Received invalid SimMessage n_values");
        }

        return msg;
    }

private:
    static sockaddr_un socket_address() {
        sockaddr_un address{};
        address.sun_family = AF_UNIX;

        if (std::strlen(IPC_SOCKET_PATH) >= sizeof(address.sun_path)) {
            throw std::runtime_error("IPC socket path is too long");
        }
        std::strcpy(address.sun_path, IPC_SOCKET_PATH);
        return address;
    }

    static socklen_t socket_address_length(const sockaddr_un& address) {
        return static_cast<socklen_t>(
            offsetof(sockaddr_un, sun_path) +
            std::strlen(address.sun_path) + 1);
    }

    static void remove_stale_socket() {
        struct stat info{};
        if (::lstat(IPC_SOCKET_PATH, &info) != 0) {
            if (errno == ENOENT) {
                return;
            }
            throw_system_error("could not inspect IPC socket path");
        }

        if (!S_ISSOCK(info.st_mode)) {
            throw std::runtime_error(
                std::string("Refusing to remove non-socket path: ") +
                IPC_SOCKET_PATH);
        }
        if (::unlink(IPC_SOCKET_PATH) != 0) {
            throw_system_error("could not remove stale IPC socket");
        }
    }

    static int make_socket() {
        const int fd = ::socket(AF_UNIX, SOCK_SEQPACKET, 0);
        if (fd < 0) {
            throw_system_error("could not create seqpacket socket");
        }
        return fd;
    }

    void accept_client() {
        remove_stale_socket();
        listen_fd_ = make_socket();

        const sockaddr_un address = socket_address();
        if (::bind(listen_fd_, reinterpret_cast<const sockaddr*>(&address),
                   socket_address_length(address)) != 0) {
            throw_system_error("could not bind seqpacket socket");
        }
        owns_socket_path_ = true;

        if (::listen(listen_fd_, 1) != 0) {
            throw_system_error("could not listen on seqpacket socket");
        }

        std::cout << "waiting for IPC connection on " << IPC_SOCKET_PATH
                  << "\n";
        do {
            fd_ = ::accept(listen_fd_, nullptr, nullptr);
        } while (fd_ < 0 && errno == EINTR);

        if (fd_ < 0) {
            throw_system_error("could not accept seqpacket connection");
        }

        ::close(listen_fd_);
        listen_fd_ = -1;
        std::cout << "accepted IPC connection on " << IPC_SOCKET_PATH << "\n";
    }

    void connect_to_server() {
        fd_ = make_socket();
        const sockaddr_un address = socket_address();

        std::cout << "waiting for IPC server on " << IPC_SOCKET_PATH << "\n";
        for (;;) {
            if (::connect(fd_, reinterpret_cast<const sockaddr*>(&address),
                          socket_address_length(address)) == 0) {
                break;
            }

            const int error = errno;
            if (error == EINTR) {
                continue;
            }
            if (error != ENOENT && error != ECONNREFUSED) {
                errno = error;
                throw_system_error("could not connect seqpacket socket");
            }

            // Match the old FIFO's blocking startup behavior without spinning.
            ::usleep(50'000);
        }
        std::cout << "connected IPC socket " << IPC_SOCKET_PATH << "\n";
    }

    void cleanup() noexcept {
        if (fd_ >= 0) {
            ::close(fd_);
            fd_ = -1;
        }
        if (listen_fd_ >= 0) {
            ::close(listen_fd_);
            listen_fd_ = -1;
        }
        if (owns_socket_path_) {
            ::unlink(IPC_SOCKET_PATH);
            owns_socket_path_ = false;
        }
    }

    [[noreturn]] static void throw_system_error(const char* operation) {
        const int error = errno;
        throw std::runtime_error(std::string(operation) + ": " +
                                 std::strerror(error));
    }

    int fd_ = -1;
    int listen_fd_ = -1;
    bool owns_socket_path_ = false;
};
