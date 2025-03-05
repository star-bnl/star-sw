// -*- C++ Header -*-
/*
==================================================
Authors: A.Mithran;
Emails: mithran@fias.uni-frankfurt.de
==================================================
*/

#ifndef SIMD_SSE_TYPE_H
#define SIMD_SSE_TYPE_H

#include "mask32.h"
#include "int32.h"
#include "float32.h"
#include <stdexcept>
#include <type_traits>

namespace KFP {
namespace SIMD {

using simd_mask = Mask32_128;

using simd_float = Float32_128;
static_assert(std::is_same<simd_float::value_type, float>::value,
              "[Error]: Invalid value type for SSE float SimdClass.");

using simd_int = Int32_128;
static_assert(std::is_same<simd_int::value_type, int>::value,
              "[Error]: Invalid value type for SSE int SimdClass.");

KFP_SIMD_INLINE Int32_128 toInt(const Float32_128& a) { return _mm_cvtps_epi32(a.simd()); }
KFP_SIMD_INLINE Float32_128 toFloat(const Int32_128& a) { return _mm_cvtepi32_ps(a.simd()); }

KFP_SIMD_INLINE Int32_128 reinterpretAsInt(const Float32_128& a) { return _mm_castps_si128(a.simd()); }
KFP_SIMD_INLINE Float32_128 reinterpretAsFloat(const Int32_128& a) { return _mm_castsi128_ps(a.simd()); }

} // namespace SIMD
} // namespace KFP

#endif // !SIMD_SSE_TYPE_H
