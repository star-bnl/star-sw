/***************************************************************************
 *
 * $Id: helixTest3.cc,v 1.1 1999/02/17 12:43:59 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1998 
 ***************************************************************************
 *
 * Description:
 * Program to test new StHelix methods/features.
 *
 ***************************************************************************
 *
 * $Log: helixTest3.cc,v $
 * Revision 1.1  1999/02/17 12:43:59  ullrich
 * New Revision
 *
 * Revision 1.1  1999/01/23 00:26:46  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StGlobals.hh"
#include "StHelix.hh"
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"

#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

int main()
{
    double radius    = 2*meter; 
    double dipAngle  = 7*degree;
    double phase     = 124*degree;
    int    H = -1;

    StThreeVector<double> origin; 

    StHelix helix(1/(radius),
		  dipAngle,
		  phase,
		  origin,
		  H);
  
    cout << "helix = " << helix << endl;

    double s = 0;
    for (int i=0; i<20; i++) {
	cout << s << '\t' << helix.at(s) << endl;
	s += 20*centimeter;
    }
    
    //
    //  Intersection with a plane
    //

    // r is the position of the center of the plane
    double sprime = 1.2*meter;
    StThreeVector<double> r = helix.at(sprime);
    // n is the normal vector
    StThreeVector<double> n(0,1,1);
    
    double ss = helix.pathLength(r, n.unit());
    
    cout << "path length at intersection s = " << ss << endl;
    cout << "precision = " << (sprime-ss)/micrometer << " um" << endl;
 
    return 0;
}
