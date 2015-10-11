# eazy-opencl - Opencl Binding for Common Lisp

*Can You see the dawn of lisp-based FPGA ?*

Both [cl-opencl](https://github.com/malkia/cl-opencl) and [cl-opencl-3b](https://github.com/3b/cl-opencl-3b) seems the abondoned projects. I'm trying to supersede it with something better.

## Testing

`(asdf:test-system :eazy-opencl)`

## Supported OpenCL Versions

+ opencl-1.2
+ opencl-2.0

OpenCL is a framework for running a massively parallel computation enhanced by coprocessor, including not only GPGPU but also FPGA and ASIC. Apple started this, and AMD is very supportive, and now even several embedded devices (e.g. qualcomm snapdragon) support this framework.

While NVIDIA used to be largely inactive in supporting OpenCL and like to maintain the vendor-lock-in using CUDA, as of 2015-05-07, there are [large number of NVIDIA's products finally passed the opencl 1.2 conformance test](https://www.khronos.org/conformance/adopters/conformant-products). 

This library is developped with AMD APPSDK v3.0 (OpenCL 2.0) and Radeon HD 5770.


## Dependencies
This library is at least tested on implementation listed below:

+ SBCL 1.2.16 on X86-64 Linux 3.13.0-65-generic (author's environment)

Also, it depends on the following lisp libraries:

+ iterate by ** :
    Jonathan Amsterdam's iterator/gatherer/accumulator facility
+ alexandria by ** :
    Alexandria is a collection of portable public domain utilities.
+ trivia :
    
## Author

* Masataro Asai (guicho2.71828@gmail.com)

Part of the C-like source generator is forked from those in [Takagi@CL-CUDA](https://github.com/takagi/cl-cuda).

Also, CFFI-binding in binding-src/ is forked from those in cl-opencl-3b . Credit goes to https://github.com/3b/.



## Copyright

Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)

# License

Licensed under the LLGPL License.


