cmake_minimum_required(VERSION 3.26)
project(Eureka)

add_library(EurekaCore board.cpp constants.cpp search.cpp tt.cpp board.h constants.h search.h tt.h)

add_executable(EurekaUCI runUCI.cpp uci.cpp uci.h)
target_link_libraries(EurekaUCI PUBLIC EurekaCore)

add_executable(Datagen runDatagen.cpp datagen.cpp datagen.h)
target_link_libraries(Datagen PUBLIC EurekaCore)

enable_testing()
add_subdirectory("ctest")