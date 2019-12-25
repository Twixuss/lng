#pragma once
#include "common.h"
#include "compiler.h"
#define TRANSLATE_OK 0
#define TRANSLATE_ERROR 1
#ifndef BUILD_LNG
extern "C" int Translate();
extern "C" int _Translate(compiler* Compiler) {
    compiler::Instance = Compiler;
    return Translate();
}
#endif