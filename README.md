# complex-convert

Clang tool to convert C++ `std::complex` types to C99 complex types.

This tool is incomplete, only converting a subset of `std::complex` constructs.
The original goal was to make it easier to port C++ code to GPUs, though GPU C++
support is so good these days that this is probably no longer necessary.
