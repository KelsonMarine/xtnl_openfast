#pragma once
#include <string>
#include <vector>
#include <stdexcept>
#include <cstring>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

// Adjust these paths as needed
constexpr const char* PIPE_OFA_TO_OFO = "openfast_pos.fifo";
constexpr const char* PIPE_OFO_TO_OFA = "openfoam_forces.fifo";

// A simple tagged message for debuggability
struct SimMessage {
    int    step;           // Simulation timestep
    int    n_values;       // Number of doubles following
    double values[32];     // Your coupling data (resize as needed)
    char   tag[32];        // Human-readable label for debugging
};

class PipeChannel {
public:
    // Call once, from whichever process starts first (or a launcher)
    static void create_pipes() {
        for (const char* path : {PIPE_OFA_TO_OFO, PIPE_OFO_TO_OFA}) {
            ::unlink(path); // Remove stale pipes
            if (::mkfifo(path, 0666) != 0)
                throw std::runtime_error(std::string("mkfifo failed: ") + strerror(errno));
        }
    }

    // 'write_path': pipe this process writes to
    // 'read_path':  pipe this process reads from
    PipeChannel(const char* write_path, const char* read_path) {
        // Open order matters! Both sides must open write-end before read-end
        // to avoid deadlock. Open write end first (non-blocking), then read.
        write_fd_ = ::open(write_path, O_WRONLY); // blocks until reader opens
        if (write_fd_ < 0)
            throw std::runtime_error(std::string("open write pipe failed: ") + strerror(errno));

        read_fd_ = ::open(read_path, O_RDONLY);   // blocks until writer opens
        if (read_fd_ < 0)
            throw std::runtime_error(std::string("open read pipe failed: ") + strerror(errno));
    }

    ~PipeChannel() {
        ::close(write_fd_);
        ::close(read_fd_);
    }

    void send(const SimMessage& msg) {
        ssize_t bytes = ::write(write_fd_, &msg, sizeof(msg));
        if (bytes != sizeof(msg))
            throw std::runtime_error("Pipe write failed or partial write");
    }

    SimMessage recv() {
        SimMessage msg{};
        ssize_t bytes = ::read(read_fd_, &msg, sizeof(msg));
        if (bytes == 0)
            throw std::runtime_error("Pipe closed by remote process");
        if (bytes != sizeof(msg))
            throw std::runtime_error("Pipe read failed or partial read");
        return msg;
    }

private:
    int write_fd_ = -1;
    int read_fd_  = -1;
};