cmake_minimum_required(VERSION 3.26)
project(Eureka)

add_library(EurekaCore board.cpp constants.cpp search.cpp tt.cpp board.h constants.h search.h tt.h)

add_executable(EurekaTerminal terminal.cpp runInTerminal.cpp terminal.h)
target_link_libraries(EurekaTerminal PUBLIC EurekaCore)

add_executable(EurekaUCI runUCI.cpp uci.cpp uci.h)
target_link_libraries(EurekaUCI PUBLIC EurekaCore)

enable_testing()
add_subdirectory("ctest")