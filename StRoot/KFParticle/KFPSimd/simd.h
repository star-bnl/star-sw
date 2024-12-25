// -*- C++ Header -*-
/*
==================================================
Authors: A.Mithran;
Emails: mithran@fias.uni-frankfurt.de
==================================================
*/

#ifndef KFP_SIMD_H
#define KFP_SIMD_H

// Determine instruction set, and define platform-dependent functions
#include "Utils/macros.h"
// #include "Utils/memory.h"

// Select appropriate header files depending on instruction set
#if defined(KFP_SIMD_AVX)
#error "[Error] (simd.h): KFParticle SIMD AVX not implemented."
// #include "AVX/types.h"
#elif defined(KFP_SIMD_SSE)
#include "SSE/types.h"
#else
#error "[Error] (simd.h): KFParticle SIMD Scalar not implemented."
// #include "Scalar/types.h"
#include "Scalar/types.h"
#endif

#endif // !KFP_SIMD_H
