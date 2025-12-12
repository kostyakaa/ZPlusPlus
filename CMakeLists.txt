cmake_minimum_required(VERSION 4.0)
project(ZPlusPlus)

set(CMAKE_CXX_STANDARD 20)

add_executable(ZPlusPlus main.cpp
        lexer/lexer.cpp
        lexer/lexer.h)
