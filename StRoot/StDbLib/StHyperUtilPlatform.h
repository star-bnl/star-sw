// StHyperUtilPlatform.h
#ifndef __ST_HYPERUTILPLATFORM_H
#define __ST_HYPERUTILPLATFORM_H

#include <string>

namespace StHyperUtilPlatform
{

// very simple define copied from coreutils
#define st_hyper_util_platform_alignof(type) offsetof (struct { char c; type x; }, x)

// another way to do that via pointer arithmetics
template <typename T> struct StHyperUtilPlatformAlignTest {
    char m0;
    T m1;
};
#define st_hyper_util_platform_alignof_p(TYPE)     (size_t)&(((StHyperUtilPlatformAlignTest<TYPE>*)0)->m1)

//  Endian-ness test: little,big, or unknown?
//  ABCD : Litte Endian - less significant byte first
//  DCBA : Big Endian   - most significant byte first
namespace Endian
{
	enum EndianEnum { UNDEFINED = 0, LITTLE = 1, BIG = 2 };
} // namespace Endian

StHyperUtilPlatform::Endian::EndianEnum test_endian();

std::string endian_as_string();

std::string Arch();

} // namespace StHyperUtilPlatform

#endif // __ST_HYPERUTILPLATFORM_H
