#ifndef INLINEC_H
#define INLINEC_H

/* It this file the specification "inline" is made controlled, that is
it can dissapear from the code if macro WCPPLIB_NO_INLINE is defined
and if instead of "inline" the user use "wl_inline" statement.
*/
//#define WCPPLIB_NO_INLINE

#ifdef WCPPLIB_NO_INLINE
#undef WCPPLIB_INLINE
#define wl_inline
#else
#define WCPPLIB_INLINE
#define wl_inline inline
#endif

#endif
