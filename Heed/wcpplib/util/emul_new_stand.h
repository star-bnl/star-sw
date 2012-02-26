#ifndef EMUL_NEW_STAND_H
#define EMUL_NEW_STAND_H
/*
Copyright (c) 2001 I. B. Smirnov

Permission to use, copy, modify, distribute and sell this file
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice, this permission notice, 
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
It is provided "as is" without express or implied warranty.
*/

//#define S_ENWSD    // switch to emulation of cast operators:
// instead of static_cast as well as dynamic_cast it will be direct type
// transformations: ((type)(name))

//#define S_NO_TYPEID   // no type identification and therefore no dynamic_cast

#ifndef S_ENWSD    // if no emulation
#define statcast(type, name) static_cast<type>(name)
#ifdef S_NO_TYPEID
#define dyncast(type, name) static_cast<type>(name)
#else
#define dyncast(type, name) dynamic_cast<type>(name)
#endif
#else              // emulation
#define statcast(type, name) ((type)(name))
#define dyncast(type, name) ((type)(name))     // very bad emulation
#endif

//#define S_EEXCEPT      // sign to emulate exceptions

//#define S_EMUTABLE     // sign to emulate mutable directive

#ifdef S_EMUTABLE
#include "util/FunNameStack.h"
#define mutable 
// This discards const
#define convmut(type) type& t=(type)(*this); \
if(&t != this) \
{mcerr<<"macro convmut(type): emulation of mutable does not work\n"; \
spexit(mcerr);}
#else
#define convmut(type) const type& t=(*this);
#endif

// For compiler of Solaris, to avoid errors:
// Error: Function templates may not have default parameters.
// Abbreviation of Ban of Default Parameters in Function Templates 
//#define BAN_DEFAULT_PAR_FUN_TEMPL
#endif 


