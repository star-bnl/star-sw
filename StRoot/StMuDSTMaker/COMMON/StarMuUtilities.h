/***********************************************************************
 *
 *  StarMuUtilities.h,v 1.0 1999/09/07
 * Authors: Frank Laue, BNL, laue@bnl.gov
 *
 ***********************************************************************/

#ifndef StarMuUtilities_h
#define StarMuUtilities_h

#include <math>

#define USHORTMAX 65536
#define USHORTMIN 0
#define SHORTMAX 256
#define SHORTMIN -127

template<class T> max(T a, T b) { return (a>b) ? a : b;}
template<class T> min(T a, T b) { return (a<b) ? a : b;}

template<class T> 
short packShort(T t, int scale) {
  return (short) floor(t*scale +0.5);
}

template<class T> 
unsigned short packUnsignedShort(T t, int scale) {
  return (short) floor(t*scale +0.5);
}

#endif

/***********************************************************************
 *
 * $Log: StarMuUtilities.h,v $
 * Revision 1.1  2002/03/05 15:41:10  jeromel
 * First version of Frank's Commone MicroDST.
 *
 *
 ***********************************************************************/
