cmake_minimum_required(VERSION 3.26)

add_executable(perft tests.cpp perft.cpp perft.h)
target_link_libraries(perft PUBLIC EurekaCore)

enable_testing()
foreach(i RANGE 0 4)
    add_test("perft_${i}" perft ${i})
endforeach()