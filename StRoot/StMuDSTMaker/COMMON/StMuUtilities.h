/***********************************************************************
 *
 *  StMuUtilities.h,v 1.0 1999/09/07
 * Authors: Frank Laue, BNL, laue@bnl.gov
 *
 ***********************************************************************/

#ifndef StMuUtilities_h
#define StMuUtilities_h

#include <math.h>

template<class T> inline T max(T a, T b) { return (a>b) ? a : b;}
template<class T> inline T min(T a, T b) { return (a<b) ? a : b;}
template<class T> inline T fabsMax(T a, T b)  { return (fabs(a)>fabs(b)) ? a : b;}
template<class T> inline T fabsMin(T a, T b)  { return (fabs(a)<fabs(b)) ? a : b;}

template<class T> 
inline unsigned short pack2UnsignedShort(T t, int scale) {
  return (unsigned short) (t*scale +0.5);
}

template<class T> 
inline short pack2Short(T t, int scale) {
  return (unsigned short) (t*scale +0.5);
}

template<class T> 
inline unsigned int pack2UnsignedInt(T t, int scale) {
  return (unsigned int) (t*scale +0.5);
}

template<class T, class U> 
inline int pack2Int(T t, U scale) {
  return (int) (t*scale +0.5);
}

template<class T, class U> 
inline double unPack(T v, U scale) {
  return ((double)v)/scale;
}



#endif

/***********************************************************************
 *
 * $Log: StMuUtilities.h,v $
 * Revision 1.2  2002/04/15 22:29:28  laue
 * updates
 *
 * Revision 1.1  2002/03/08 17:04:18  laue
 * initial revision
 *
 *
 ***********************************************************************/
