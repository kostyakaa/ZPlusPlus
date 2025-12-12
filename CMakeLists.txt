cmake_minimum_required(VERSION 3.31.6)
project(ZPlusPlus)

set(CMAKE_CXX_STANDARD 20)

add_executable(ZPlusPlus main.cpp
        lexer/lexer.cpp
        lexer/lexer.h
        parser/parser.h
        parser/parser.cpp)
