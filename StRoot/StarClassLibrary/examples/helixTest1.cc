/***************************************************************************
 *
 * $Id: helixTest1.cc,v 1.4 2000/03/17 13:57:32 ullrich Exp $
 *
 * Author: Thomas Ullrich, April 1998 
 ***************************************************************************
 *
 * Description:
 * Program to test StHelix
 *
 ***************************************************************************
 *
 * $Log: helixTest1.cc,v $
 * Revision 1.4  2000/03/17 13:57:32  ullrich
 * Updated. Include now also test of method which returns
 * intersection with plane. Also made input more flexible.
 *
 * Revision 1.3  2000/02/02 19:05:15  ullrich
 * Changed macros for CC5/CC4.2 compatibility
 *
 * Revision 1.2  1999/12/21 15:14:48  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 1.1  1999/02/17 12:43:58  ullrich
 * New Revision
 *
 * Revision 1.1  1999/01/23 00:26:44  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StHelix.hh"
#include "StPrompt.hh"
#include "SystemOfUnits.h"

#ifndef ST_NO_NAMESPACES
using namespace units;
#endif
int main(int, char* argc[])
{
    double curvature = 0;
    double dipAngle  = 0;
    double phase     = 0;
    double x0 = 0;
    double y0 = 0;
    double z0 = 0;
    int    H = -1;
    pair<double, double> s;
    
    StHelix  *helix = 0;
    bool     moreHelices = true;
    bool     moreTests   = true;
    bool     radunits    = true;
    double   r = 0.1;
    char     selection = 'v';
    double   slow = 0, sup = 0, ds = 0, ss = 0;
    StThreeVector<double> origin, point, mmpoint, rv, nv;

    cout << "This is test program " << argc[0] << endl << endl;
    
    while(moreHelices) {
	moreTests = true;
	cout << "Enter helix parameter:" << endl;
	StPrompt("Angles in radians (else degrees)", radunits);
	StPrompt("Enter curvature", curvature);
	if (radunits) {
	    StPrompt("dipAngle in rad", dipAngle);
	    StPrompt("phase in rad", phase);
	}
	else {
	    StPrompt("dipAngle in degree", dipAngle);
	    StPrompt("phase in degree", phase);
	}
	StPrompt("-sign(q*B)", H);
	StPrompt("xyz of origin", origin);

	delete helix;
	helix = new StHelix(curvature,
			    (radunits ? dipAngle : dipAngle*degree),
			    (radunits ? phase : phase*degree),
			    origin,
			    H);
	if (!helix->valid()) {
	    cerr << "Error: parametrization is not valid" << endl;
	}
	else {
	    //
	    //	Print helix parameters
	    //
	    cout << endl;
	    cout << "The helix parameter are:" << endl;
	    cout << *helix << endl;
	    cout << "The period of the helix is: " << helix->period() << endl;

	    while(moreTests) {
		//
		//  Selection menu
		//
		cout << endl;
		cout << "Select one of the following:" << endl;
		cout << "Print helix points along its path ..... v" << endl;
		cout << "Test pathLength(r) method ............. r" << endl;
		cout << "Test pathLength(point) method ......... p" << endl;
		cout << "Test pathLength(plane) method ......... e" << endl;
		cout << "Test distance(point) method ........... d" << endl;
		StPrompt("Enter your selection", selection);

		switch (selection) {
		case 'e':
		    StPrompt("vector r to a point in the plane", rv);
		    StPrompt("normal vector n for plane", nv);
		    ss = helix->pathLength(rv, nv);
		    cout << "s = " << ss << " -> " << helix->at(ss) << endl;		    
		    break;
		case 'v':
		    StPrompt("lower s", slow);
		    StPrompt("upper s", sup);
		    StPrompt("step size", ds);
		    for (ss = slow; ss < sup; ss+= ds)
			cout << "s = " << ss << " -> " << helix->at(ss) << endl;
		    break;
		case 'r':
		    StPrompt("Enter a radius", r);
		    s = helix->pathLength(r);
		    cout << "The helix reaches r at s1 = " << s.first
			 << " and s2 = " << s.second << endl;
		    cout << "Crosscheck radius1 = " << helix->at(s.first).perp()
			 << ", delta_r = " << r*meter-helix->at(s.first).perp() << endl;
		    cout << "Crosscheck radius2 = " << helix->at(s.second).perp()
			 << ", delta_r = " << r*meter-helix->at(s.second).perp() << endl;
		    break;
		case 'p':
		    StPrompt("xyz of point", point);
		    mmpoint = point;
		    ss = helix->pathLength(mmpoint);
		    cout << "The helix reaches r at s = " << ss << endl;
		    cout << "Crosscheck point = " << helix->at(ss)
			 << ", delta = " << abs(mmpoint-helix->at(ss)) << endl;
		    break;
		case 'd':
		    StPrompt("xyz of point", point);
		    mmpoint = point;
		    cout << "The closest distance from the helix to the point is: "
			 << helix->distance(mmpoint) << endl;
		    break;
		default:
		    break;
		}
		cout << endl;
#if defined (__SUNPRO_CC) && __SUNPRO_CC < 0x500  
		StBoolPrompt("another test", moreTests);
#else
		StPrompt("another test", moreTests);
#endif
	    }
	    cout << endl;
#if defined (__SUNPRO_CC) && __SUNPRO_CC < 0x500  
	    StBoolPrompt("define another helix", moreHelices);
#else
	    StPrompt("define another helix", moreHelices);
#endif
	}
    }
    cout << "end of example" << endl;
    return 0;
}
