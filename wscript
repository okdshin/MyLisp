APPNAME = ''
VERSION = '1.0.0'

srcdir = '.'
blddir = 'build'

def options(opt):
    opt.load("compiler_cxx")

def configure(conf):
    conf.load("compiler_cxx")
    #conf.check_cfg(package="opencv", uselib_store="OPENCV", args=["--cflags", "--libs"])

def build(bld):
    bld.program(features = "cxx cprogram",
        source = "main.cpp",
        cxxflags = ["-std=c++14", "-g", "-O3", "-Wall"],
        #lib = ["boost_system", "boost_filesystem"],
        #uselib=["OPENCV"]
        target = "a.out",
        includes = "../../../")
