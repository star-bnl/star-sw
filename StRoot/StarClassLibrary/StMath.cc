/***************************************************************************
 *
 * $Id: StMath.cc,v 1.1 2003/11/20 03:02:07 perev Exp $
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
int StMath::tooBig(float  *arr, int narr, double toobig) 
{
  double qwe;
  for (int i=0;i<narr;i++) {
    qwe = arr[i];
    if (!::finite(qwe)) 	return 1;
    if (::fabs(qwe) > toobig) 	return 2;
  }
  return 0;
}
//_____________________________________________________________________________
int StMath::tooBig(double  *arr, int narr, double toobig) 
{
  double qwe;
  for (int i=0;i<narr;i++) {
    qwe = arr[i];
    if (!::finite(qwe)) 	return 1;
    if (::fabs(qwe) > toobig) 	return 2;
  }
  return 0;
}




