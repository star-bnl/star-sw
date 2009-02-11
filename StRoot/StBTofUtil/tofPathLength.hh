 /***************************************************************************
 *
 * $Id: tofPathLength.hh,v 1.1 2009/02/11 17:31:14 dongx Exp $ tofPathLength.cc,v 1.2 2003/07/11 05:08:49 geurts Exp $
 *
 * Author: Frank Geurts
 ***************************************************************************
 *
 * Description: Calculate helix path length between to points.
 *              requires begin and end point StThreeVectors and helix curvature
 *
 ***************************************************************************
 *
 **************************************************/
#ifndef BTOFPATHLENGTH_HH
#define BTOFPATHLENGTH_HH
#include "StThreeVector.hh"
#include "StThreeVectorD.hh"
#include "StThreeVectorF.hh"

/*!
 * Functions to calculate the pathLength using the starting and ending point positions
 * and the track helix curvature
 */
double tofPathLength(const StThreeVector<double>*, const StThreeVector<double>*, const double);
//double tofPathLength(const StThreeVectorD*, const StThreeVectorD*, const double);
double tofPathLength(const StThreeVectorD*, const StThreeVectorF*, const double);
double tofPathLength(const StThreeVectorF*, const StThreeVectorD*, const double);
double tofPathLength(const StThreeVectorF*, const StThreeVectorF*, const double);
#endif
