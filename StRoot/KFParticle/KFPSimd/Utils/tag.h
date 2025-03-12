// -*- C++ Header -*-
/*
==================================================
Authors: A.Mithran;
Emails: mithran@fias.uni-frankfurt.de
==================================================
*/

#ifndef SIMD_TAG_H
#define SIMD_TAG_H

#include "macros.h"

namespace KFP {
namespace SIMD {

struct UninitializeTag {};

enum class Tag
{
    /// uses only fundamental types
    Scalar,
    // Entry to SSE versions
    SSE,
    /// x86 SSE + SSE2
    SSE2,
    /// x86 SSE + SSE2 + SSE3
    SSE3,
    /// x86 SSE + SSE2 + SSE3 + SSSE3
    SSSE3,
    /// x86 SSE + SSE2 + SSE3 + SSSE3 + SSE4.1
    SSE41,
    /// x86 SSE + SSE2 + SSE3 + SSSE3 + SSE4.1 + SSE4.2
    SSE42,
    /// x86 AVX
    AVX,
    /// x86 AVX + AVX2
    AVX2,
};

// constexpr inline bool validateTag(Tag tag)
// {
//     switch (tag) {
//     case Tag::Scalar:
//         return true;
//     case Tag::SSE:
//         return true;
//     case Tag::SSE2:
//         return true;
//     case Tag::SSE3:
//         return true;
//     case Tag::SSSE3:
//         return true;
//     case Tag::SSE41:
//         return true;
//     case Tag::SSE42:
//         return true;
//     case Tag::AVX:
//         return true;
//     case Tag::AVX2:
//         return true;
//     default:
//         return false;
//     };
// }

constexpr inline Tag getTag()
{
#if defined(__KFP_SIMD__AVX2)
    return Tag::AVX;
#elif defined(__KFP_SIMD__AVX)
    return Tag::AVX;
#elif defined(__KFP_SIMD__SSE)
    #if defined(__KFP_SIMD__SSE4_2)
        return Tag::SSE;
    #elif defined(__KFP_SIMD__SSE4_1)
        return Tag::SSE;
    #elif defined(__KFP_SIMD__SSSE3)
        return Tag::SSE;
    #elif defined(__KFP_SIMD__SSE3)
        return Tag::SSE;
    #elif defined(__KFP_SIMD__SSE2)
        return Tag::SSE;
    #endif
#else
    return Tag::Scalar;
#endif
}

constexpr inline const char* getTagStr()
{
#if defined(__KFP_SIMD__AVX2)
    return "AVX2";
#elif defined(__KFP_SIMD__AVX)
    return "AVX";
#elif defined(__KFP_SIMD__SSE)
    #if defined(__KFP_SIMD__SSE4_2)
        return "SSE4.2";
    #elif defined(__KFP_SIMD__SSE4_1)
        return "SSE4.1";
    #elif defined(__KFP_SIMD__SSSE3)
        return "SSSE3";
    #elif defined(__KFP_SIMD__SSE3)
        return "SSE3";
    #elif defined(__KFP_SIMD__SSE2)
        return "SSE2";
    #endif
#elif defined(__KFP_SIMD__Scalar)
        return "Scalar";
#else
    return nullptr;
#endif
}

} // namespace SIMD
} // namespace KFP

#endif // !SIMD_TAG_H
