cmake_minimum_required(VERSION 3.31)
project(game_of_life)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_executable(game_of_life src/main.cpp
        src/gol/Game.cpp
        src/gol/Game.hpp
)

find_package(Curses REQUIRED)
target_link_libraries(game_of_life
        PRIVATE ${CURSES_LIBRARIES}
)
