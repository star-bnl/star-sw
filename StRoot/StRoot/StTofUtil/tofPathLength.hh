 /***************************************************************************
 *
 * $Id: tofPathLength.hh,v 1.5 2009/02/11 18:02:27 dongx Exp $ tofPathLength.cc,v 1.2 2003/07/11 05:08:49 geurts Exp $
 *
 * Author: Frank Geurts
 ***************************************************************************
 *
 * Description: Calculate helix path length between to points.
 *              requires begin and end point StThreeVectors and helix curvature
 *
 ***************************************************************************
 *
 * $Log: tofPathLength.hh,v $
 * Revision 1.5  2009/02/11 18:02:27  dongx
 * fix the mis-overwriting in the last release
 *
 * Revision 1.3  2005/07/06 19:20:01  fisyak
 * StThreeVectorD == StThreeVector<double>
 *
 * Revision 1.2  2004/03/17 01:49:56  dongx
 * add tofPathLength(StThreeVectorD*, StThreeVectorF*, double)
 *
 * Revision 1.1  2003/08/06 23:42:56  geurts
 * function definitions in seperate header file
 *
 *
 **************************************************/
#ifndef TOFPATHLENGTH_HH
#define TOFPATHLENGTH_HH
#include "StThreeVector.hh"
#include "StThreeVectorD.hh"
#include "StThreeVectorF.hh"

double tofPathLength(const StThreeVector<double>*, const StThreeVector<double>*, const double);
//double tofPathLength(const StThreeVectorD*, const StThreeVectorD*, const double);
double tofPathLength(const StThreeVectorD*, const StThreeVectorF*, const double);
double tofPathLength(const StThreeVectorF*, const StThreeVectorD*, const double);
double tofPathLength(const StThreeVectorF*, const StThreeVectorF*, const double);
#endif
