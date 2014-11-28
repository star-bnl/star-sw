#ifndef ENDIAN
#define ENDIAN
/*************************************************************************
 *                                                                       *
 * based on ROOT 2.22.10 RConfig.h                                       *
 *                                                                       *
 *************************************************************************/

#ifdef __hpux
#   define BIG_ENDIAN
#endif

#ifdef _AIX
#   define BIG_ENDIAN
#endif

#ifdef __alpha
#   ifndef __VMS
#      define LITTLE_ENDIAN
#   else
#      define BIG_ENDIAN
#   endif
#endif

#ifdef __sun
#   ifdef __i386
#      define LITTLE_ENDIAN
#   else
#      define BIG_ENDIAN
#   endif
#endif

#ifdef __sgi
#   define BIG_ENDIAN
#endif

#if defined(__linux) && !defined(__powerpc) && !defined(LITTLE_ENDIAN)
#   define LITTLE_ENDIAN
#endif

#if defined(__linux) && defined(__powerpc)
#   define BIG_ENDIAN
#endif

#if defined(__FreeBSD__)
#   define LITTLE_ENDIAN
#endif

#ifdef BORLAND           /* 16-bit MSDOS case */
#   define LITTLE_ENDIAN
#endif

#ifdef _WIN32
#   define LITTLE_ENDIAN
#endif


#endif

