/***************************************************************************
 *
 * $Id: StMath.hh,v 1.4 2004/01/27 02:51:57 perev Exp $
 *
 * Author: Thomas Ullrich, Apr 2000
 ***************************************************************************
 *
 * Description: Prototypes for various specialized math routines.
 *
 ***************************************************************************
 *
 * $Log: StMath.hh,v $
 * Revision 1.4  2004/01/27 02:51:57  perev
 * Add finite() for float
 *
 * Revision 1.3  2003/11/25 04:22:33  perev
 * finite(float) implemented
 *
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
static int Finite(const float  &f); 
static int Finite(const double &f); 
};


#endif
