#ifndef StHbtString_hh
#define StHbtString_hh

#ifndef __CINT__

#ifndef StHbtString_noCint
#define StHbtString_noCint
#include <string>

#if !defined(ST_NO_NAMESPACES)
using std::string;
#endif

typedef string StHbtString; //!
#endif

#else

#ifndef StHbtString_yesCint
#define StHbtString_yesCint
class StHbtString; //!
#endif

#endif

#endif
