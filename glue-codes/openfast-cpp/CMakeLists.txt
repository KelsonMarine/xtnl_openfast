#
# Copyright 2016 National Renewable Energy Laboratory
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

if (${CMAKE_VERSION} VERSION_GREATER_EQUAL "3.12.0")
  cmake_policy(SET CMP0074 NEW)
endif()

enable_language(CXX Fortran)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

option(USE_MPI "Compile cpp glue codes with mpi" ON)

if (USE_MPI)
  find_package(MPI REQUIRED)
else()
  set(MPI_LIBRARIES "")
  set(MPI_INCLUDE_PATH "")
  set(MPI_INCLUDE_PATH "")
endif()

find_package(LibXml2 REQUIRED)
find_package(ZLIB REQUIRED)
find_package(HDF5 REQUIRED)
find_package(NetCDF REQUIRED COMPONENTS C)

add_library(openfastcpplib SHARED src/OpenFAST.cpp)
set_property(TARGET openfastcpplib PROPERTY POSITION_INDEPENDENT_CODE ON)

if (MPI_FOUND)
  target_compile_definitions(openfastcpplib PUBLIC -DUSE_MPI)
endif()

target_link_libraries(openfastcpplib
  openfastlib
  ${HDF5_LIBRARIES}
  ${NETCDF_LIBRARIES}
  ${ZLIB_LIBRARIES}
  ${LIBXML2_LIBRARIES}
  ${MPI_LIBRARIES}
)
target_include_directories(openfastcpplib PUBLIC 
  ${HDF5_INCLUDE_DIRS}
  ${NETCDF_INCLUDE_DIRS}
  ${ZLIB_INCLUDES}
  ${LIBXML2_INCLUDE_DIR}
  ${MPI_INCLUDE_PATH}
)
set_target_properties(openfastcpplib PROPERTIES PUBLIC_HEADER "src/OpenFAST.H")

install(TARGETS openfastcpplib
  EXPORT "${CMAKE_PROJECT_NAME}Libraries"
  RUNTIME DESTINATION bin
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
  PUBLIC_HEADER DESTINATION include
)

# Build driver if requested
if (BUILD_OPENFAST_CPP_DRIVER)

  find_package(yaml-cpp REQUIRED)
  add_executable(openfastcpp src/FAST_Prog.cpp)
  target_link_libraries(openfastcpp 
    ${YAML_CPP_LIBRARIES}
    openfastcpplib 
  )
  target_include_directories(openfastcpp PRIVATE ${YAML_CPP_INCLUDE_DIR})
  set_target_properties(openfastcpp PROPERTIES LINKER_LANGUAGE CXX)

  if (MPI_FOUND)
    target_compile_definitions(openfastcpp PUBLIC -DUSE_MPI)
  endif()

  if(MPI_COMPILE_FLAGS)
    set_target_properties(openfastcpp PROPERTIES
      COMPILE_FLAGS "${MPI_COMPILE_FLAGS}")
  endif(MPI_COMPILE_FLAGS)

  if(MPI_LINK_FLAGS)
    set_target_properties(openfastcpp PROPERTIES
      LINK_FLAGS "${MPI_LINK_FLAGS}")
  endif(MPI_LINK_FLAGS)

  install(TARGETS openfastcpp 
    EXPORT "${CMAKE_PROJECT_NAME}Libraries"
    RUNTIME DESTINATION bin
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
    PUBLIC_HEADER DESTINATION include
  )

  add_executable(safewindopenfastcpp src/SafeWind_Fast_Prog.cpp)
  target_link_libraries(safewindopenfastcpp 
    ${YAML_CPP_LIBRARIES}
    openfastcpplib 
  )
  target_include_directories(safewindopenfastcpp PRIVATE ${YAML_CPP_INCLUDE_DIR})
  set_target_properties(safewindopenfastcpp PROPERTIES LINKER_LANGUAGE CXX)

  if (MPI_FOUND)
    target_compile_definitions(safewindopenfastcpp PUBLIC -DUSE_MPI)
  endif()

  if(MPI_COMPILE_FLAGS)
    set_target_properties(safewindopenfastcpp PROPERTIES
      COMPILE_FLAGS "${MPI_COMPILE_FLAGS}")
  endif(MPI_COMPILE_FLAGS)

  if(MPI_LINK_FLAGS)
    set_target_properties(safewindopenfastcpp PROPERTIES
      LINK_FLAGS "${MPI_LINK_FLAGS}")
  endif(MPI_LINK_FLAGS)

  install(TARGETS safewindopenfastcpp 
    EXPORT "${CMAKE_PROJECT_NAME}Libraries"
    RUNTIME DESTINATION bin
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
    PUBLIC_HEADER DESTINATION include
  )
endif()
