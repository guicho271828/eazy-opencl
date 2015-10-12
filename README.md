# eazy-opencl - Opencl Binding for Common Lisp

*Can You see the dawn of lisp-based FPGA ?*

Both [cl-opencl](https://github.com/malkia/cl-opencl) and [cl-opencl-3b](https://github.com/3b/cl-opencl-3b) seems the abondoned projects. I'm trying to supersede it with something better.

## Installation

Before testing and using this library, you have to install OpenCL developmental library suitable for your hardware. Run `clinfo` which comes with your OpenCL implementation to verify everything is working correctly.

Other than that, just load it in the quicklisp local-project directory.

## Supported OpenCL Versions

+ opencl-1.0
+ opencl-1.1
+ opencl-1.2
+ opencl-2.0

This library is developped with AMD APPSDK v3.0 (OpenCL 2.0), Radeon HD 5770 and PhenomII X6.

OpenCL has a *specification* independent from *implementations*, much like in ANSI Common Lisp.
It generally maintains the backward-compatibility: If you have a conformant OpenCL 2.0 implementation, then any programs written with earlier versions of OpenCL should work. However, my CFFI interface may try to find nonexistent symbol when you have an earlier version only. Drop some bug report to the [issues](issues/) page in that case.

## Testing

`(asdf:test-system :eazy-opencl)`

For platforms whose OpenCL versions are different from MY environment (OpenCL 2.0), there might be some issues regarding CFFI. Bug/Trouble Reports should go to [github issues](issues/) page.

## Devices

OpenCL supports wide variety of hardware. As far as I know, it works on the following hardwares. The official, comprehensive device list is available [here](https://www.khronos.org/conformance/adopters/conformant-products).

+ CPU with SSE2.X instruction (AMD), SSE4.2 instruction (Intel), many-core ARM mobile CPUs (qualcomm snapdragon etc.)
+ CPU with embedded GPU (latest Intel, or AMD A-series)
+ FPGA, ASIC
+ GPGPU (Radeon, GeForce)
+ DSP

While NVIDIA used to be largely inactive in supporting OpenCL and like to maintain the vendor-lock-in with CUDA, as of 2015-05-07, there are [large number of NVIDIA's products passing the opencl 1.2 conformance test](https://www.khronos.org/conformance/adopters/conformant-products). 

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


