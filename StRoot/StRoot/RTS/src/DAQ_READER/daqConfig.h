#ifndef _EVP_DAQCONFIG_HH_
#define _EVP_DAQCONFIG_HH_
// Borrow from $ROOTSYS/include/RConfig.h

#ifndef ANSICPP
#  define ANSICPP
#endif

#ifndef _NAME1_
#ifdef ANSICPP
    /* symbol concatenation operator */
#   define _NAME1_(name) name
#   define _NAME2_(name1,name2) name1##name2
#   define _NAME3_(name1,name2,name3) name1##name2##name3

    /* stringizing */
#   define _QUOTE_(name) #name

#else

#   define _NAME1_(name) name
#   define _NAME2_(name1,name2) _NAME1_(name1)name2
#   define _NAME3_(name1,name2,name3) _NAME2_(name1,name2)name3

#   define _QUOTE_(name) "name"

#endif
#endif
#endif
