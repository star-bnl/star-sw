/***********************************************************************
 *
 * $Id: StMuUtilities.h,v 1.0 1999/09/07
 * Authors: Frank Laue, BNL, laue@bnl.gov
 *
 ***********************************************************************/

#ifndef StMuUtilities_h
#define StMuUtilities_h

#include <math.h>

//* minimum and maximum functions */
template<class T> inline T fabsMax(T a, T b)  { return (fabs(a)>fabs(b)) ? a : b;} ///< max of the absolut two values
template<class T> inline T fabsMin(T a, T b)  { return (fabs(a)<fabs(b)) ? a : b;} ///< min of the absolut two values

//* pack and unpack functions */
template<class T, class U> 
inline unsigned short pack2UnsignedShort(T t, U scale) {
  return (unsigned short) (t*scale +0.5);
}

template<class T, class U> 
inline short pack2Short(T t, U scale) {
  return (unsigned short) (t*scale +0.5);
}

template<class T, class U> 
inline unsigned int pack2UnsignedInt(T t, U scale) {
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
 * Revision 1.4  2007/02/20 18:31:14  mvl
 * Removed templates for min() and max(), which conflict with STL (requested by Pibero Djawotho)
 *
 * Revision 1.3  2002/09/11 21:02:42  laue
 * added cut on track encoded method for ITTF
 *
 * Revision 1.2  2002/04/15 22:29:28  laue
 * updates
 *
 * Revision 1.1  2002/03/08 17:04:18  laue
 * initial revision
 *
 *
 ***********************************************************************/
