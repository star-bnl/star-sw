#ifndef ENUMTYPE_HH
#define ENUMTYPE_HH
enum passType {NULLTYPE=0,  SHORT, LONG, FLOAT, DOUBLE, CHAR, BLOB, TEXT};

#ifdef SOLARIS

#define _ByteSwap_

# ifndef false
typedef int bool;
#define false 0
#define true 1

# endif
#endif


#endif
