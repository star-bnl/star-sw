/***************************************************************************
 *
 * $Id: StMath.cc,v 1.3 2004/01/27 02:51:35 perev Exp $
 *
 * Author: Victor Perev - Created: 19 Nov 2003
 *       
 ***************************************************************************
 *
 * Description:
 *               Set of math utilities as a static members of StMath class
 * 
 ***************************************************************************
 *
 * $Log: StMath.cc,v $
 * Revision 1.3  2004/01/27 02:51:35  perev
 * Add finite() for float
 *
 * Revision 1.2  2003/11/25 04:21:54  perev
 * finite(float) implemented
 *
 * Revision 1.1  2003/11/20 03:02:07  perev
 * New utility class StMath
 *
 *
 **************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "StMath.hh"
//_____________________________________________________________________________
int StMath::Finite(const double &f)
{return ::finite(f);}

//_____________________________________________________________________________
int StMath::Finite(const float &f)
{
/*
The IEEE single precision floating point standard representation requires a 32 bit word, which may be represented as numbered from 0 to 31, left to right. The first bit is the sign bit, S, the next eight bits are the exponent bits, 'E', and the final 23 bits are the fraction 'F':

  S EEEEEEEE FFFFFFFFFFFFFFFFFFFFFFF
  0 1      8 9                    31

The value V represented by the word may be determined as follows:


If E=255 and F is nonzero, then V=NaN ("Not a number") 
If E=255 and F is zero and S is 1, then V=-Infinity 
If E=255 and F is zero and S is 0, then V=Infinity 
If 0<E<255 then V=(-1)**S * 2 ** (E-127) * (1.F) where "1.F" is intended to represent the binary number created by prefixing F with an implicit leading 1 and a binary point. 
If E=0 and F is nonzero, then V=(-1)**S * 2 ** (-126) * (0.F) These are "unnormalized" values. 
If E=0 and F is zero and S is 1, then V=-0 
If E=0 and F is zero and S is 0, then V=0 
*/	
  unsigned int w,F,S,E;
  w = *((int*)&f);
  if (w==0) 			return 1;
  E = (w<<1)>>24;
  if (E!=255 && E!=0) 		return 1;
  F = (w<<9)>>9;
  S = (w>>31);

  if(E==255 && F!=0 ) 		return 0; 	// NaN
  if(E==255 && F==0 && S==1) 	return 0; 	//-infinity
  if(E==255 && F==0 && S==0)	return 0; 	//+infinity
  if(E==  0 && F==0 && S==1) 	return 0; 	//-0
  return 1;
}


//_____________________________________________________________________________
int StMath::tooBig(float  *arr, int narr, double toobig) 
{
  for (int i=0;i<narr;i++) {
    if (!  Finite(arr[i])         )	return 1;
    if ( ::fabs  (arr[i]) > toobig)	return 2;
  }
  return 0;
}
//_____________________________________________________________________________
int StMath::tooBig(double  *arr, int narr, double toobig) 
{
  for (int i=0;i<narr;i++) {
    if (!::finite(arr[i])         )	return 1;
    if ( ::fabs  (arr[i]) > toobig)	return 2;
  }
  return 0;
}




