cmake_minimum_required(VERSION 3.15)

project(SectionProperties)

enable_language(Fortran)

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(dialect "-ffree-form -std=f2008 -fimplicit-none")
  set(bounds "-fbounds-check")
endif()

set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${bounds}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${dialect}")
set(CMAKE_Fortran_MODULE_DIRECTORY "/home/jimmy/opt/scifor/gnu/4.6.33-2-gcb22c67/include/")

include_directories(${CMAKE_CURRENT_SOURCE_DIR})

file(GLOB_RECURSE sources  *.f90)
add_executable(SectionProperties ${sources})