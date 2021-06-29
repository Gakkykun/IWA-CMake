
cmake_minimum_required(VERSION 3.0)

enable_language(Fortran)

project(simple-dependency Fortran)

set(EXECUTABLE IWA)

add_executable(${EXECUTABLE}
  matrix-coeff.f90
  reaction-calc.f90
  river-characteristics.f90
  main.f90
  )
