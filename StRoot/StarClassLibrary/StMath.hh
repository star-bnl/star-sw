/***************************************************************************
 *
 * $Id: StMath.hh,v 1.2 2003/11/20 03:02:07 perev Exp $
 *
 * Author: Thomas Ullrich, Apr 2000
 ***************************************************************************
 *
 * Description: Prototypes for various specialized math routines.
 *
 ***************************************************************************
 *
 * $Log: StMath.hh,v $
 * Revision 1.2  2003/11/20 03:02:07  perev
 * New utility class StMath
 *
 * Revision 1.1  2000/04/06 22:23:29  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StMath_hh
#define StMath_hh

double probChiSquared(double, unsigned int);

class StMath 
{
public:
static int tooBig(float  *arr, int narr, double toobig = 1.e+6); 
static int tooBig(double *arr, int narr, double toobig = 1.e+6); 
};


#endif
