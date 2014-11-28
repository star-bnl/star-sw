/***************************************************************************
 *
 * $Id: helixTest2.cc,v 1.2 1999/12/21 15:14:51 ullrich Exp $
 *
 * Author: Thomas Ullrich, April 1998 
 ***************************************************************************
 *
 * Description:
 * Program to test StHelix
 *
 ***************************************************************************
 *
 * $Log: helixTest2.cc,v $
 * Revision 1.2  1999/12/21 15:14:51  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 1.1  1999/02/17 12:43:59  ullrich
 * New Revision
 *
 * Revision 1.1  1999/01/23 00:26:45  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StHelix.hh"
#include "SystemOfUnits.h"

#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

int main()
{
    double radius    = 2;
    double dipAngle  = 10;
    double phase     = 10;
    double x0 = 0;
    double y0 = 0;
    double z0 = 0;
    int    H = -1;
    pair<double, double> s;
    
    StHelix  *helix = 0;
    double   r = 0.1;
 
    double   slow, sup, ds, ss;
    StThreeVector<double> origin, point, mmpoint;

    delete helix;
    helix = new StHelix(1/(radius*meter),
			dipAngle*degree,
			phase*degree,
			origin*millimeter,
			H);

    if (!helix->valid()) {
	cerr << "Error: parametrization is not valid" << endl;
    }
    else {
	cout << "The helix parameter are:" << endl;
	cout << *helix << endl;
	cout << "The period of the helix is: " << helix->period() << endl;
    }

    ds=100*centimeter;
    cout << "ds = " << ds << " -> " << helix->at(ds) << endl;

    r=1*meter;
    s = helix->pathLength(r);
    cout << "The helix reaches r=1 m at s1 = " << s.first
	 << " and s2 = " << s.second << endl;

    mmpoint = StThreeVector<double>(100, 100, 100);
    ss = helix->pathLength(mmpoint*millimeter);
    cout << "The helix reaches r at s = " << ss << endl;
    cout << "Crosscheck point = " << helix->at(ss)
	 << ", delta = " << abs(mmpoint-helix->at(ss)) << endl;

    cout << "end of example" << endl;
    return 0;
}
