#include "StHyperUtilPlatform.h"

namespace StHyperUtilPlatform
{

StHyperUtilPlatform::Endian::EndianEnum test_endian()
{
    union {
        int i;
        char c[sizeof(int)];
    } x;
    x.i = 1;
    if(x.c[0] == 1 && !x.c[sizeof(int)-1]) return StHyperUtilPlatform::Endian::LITTLE;
    if(x.c[sizeof(int)-1] == 1 && !x.c[0]) return StHyperUtilPlatform::Endian::BIG;
    return StHyperUtilPlatform::Endian::UNDEFINED;
}

std::string endian_as_string()
{
    switch(StHyperUtilPlatform::test_endian()) {
    case StHyperUtilPlatform::Endian::BIG:
        return "big";
        break;
    case StHyperUtilPlatform::Endian::LITTLE:
        return "little";
        break;
    case StHyperUtilPlatform::Endian::UNDEFINED:
    default:
		break;
    }
	return "undefined";
}

std::string Arch()
{
#ifdef __i386__
    return "i386";
#elif __i486__
    return "i386";
#elif __i586__
    return "i386";
#elif __i686__
    return "i386";
#elif _M_IX86
    return "i386";
#elif __X86__
    return "i386";
#elif _X86_
    return "i386";
#elif __x86_64__
    return "x86_64";
#elif __amd64__
    return "x86_64";
#elif _M_X64
    return "x86_64";
#elif __ia64__
    return "ia64";
#elif _M_IA64
    return "ia64";
#elif __arm__
    return "arm";
#elif __powerpc__
    return "ppc";
#elif _M_PPC
    return "ppc";
#else
    return "unknown";
#endif
}

} // namespace StHyperUtilPlatform
