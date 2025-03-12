// -*- C++ Header -*-
/*
==================================================
Authors: A.Mithran;
Emails: mithran@fias.uni-frankfurt.de
==================================================
*/

#ifndef SIMD_DETECT_H
#define SIMD_DETECT_H

#include <x86intrin.h>

#ifndef KFP_SIMD_LEVEL

#if defined(__AVX2__)
#define KFP_SIMD_AVX2 1
#elif defined(__AVX__)
#define KFP_SIMD_AVX 1
#else
    #if defined(__SSE4_2__)
    #define KFP_SIMD_SSE_4p2 1
    #elif defined(__SSE4_1__) || defined(__SSE2__)
    #define KFP_SIMD_SSE_4p1 1
    #else
    #define KFP_SIMD_Scalar 1
    #endif
#endif

#else // KFP_SIMD_LEVEL

#if KFP_SIMD_LEVEL > 3 // AVX2 supersedes SSE
#define KFP_SIMD_AVX2 1
#elif KFP_SIMD_LEVEL > 2 // AVX supersedes SSE
#define KFP_SIMD_AVX 1
#elif KFP_SIMD_LEVEL > 1
#define KFP_SIMD_SSE_4p2 1
#elif KFP_SIMD_LEVEL > 0
#define KFP_SIMD_SSE_4p1 1
#else
#define KFP_SIMD_Scalar 1
#endif
#undef KFP_SIMD_LEVEL
#endif // KFP_SIMD_LEVEL

#if defined(KFP_SIMD_AVX2)
#define KFP_SIMD_AVX 1
#endif

#if defined(KFP_SIMD_AVX)
#define KFP_SIMD_SSE_4p2 1
#endif

#if defined(KFP_SIMD_SSE_4p2)
#define KFP_SIMD_SSE_4p1 1
#endif

#if defined(KFP_SIMD_SSE_4p1)
#define KFP_SIMD_SSE 1
#endif

#if !defined(KFP_SIMD_Scalar) && !defined(KFP_SIMD_SSE)
#error \
    "[Error] (simd_detect.hpp): Invalid KFParticle SIMD implementation value was selected."
#endif

#endif // !SIMD_DETECT_H
