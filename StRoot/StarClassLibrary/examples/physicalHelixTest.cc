/***************************************************************************
 *
 * $Id: physicalHelixTest.cc,v 1.1 1999/02/17 12:44:00 ullrich Exp $
 *
 * Author: Thomas Ullrich, April 1998 
 ***************************************************************************
 *
 * Description:
 * Program to test StPhysicalHelix
 *
 ***************************************************************************
 *
 * $Log: physicalHelixTest.cc,v $
 * Revision 1.1  1999/02/17 12:44:00  ullrich
 * New Revision
 *
 * Revision 1.1  1999/01/23 00:26:48  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StPhysicalHelix.hh"
#include "SystemOfUnits.h"

#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

int main(int, char*)
{
    StThreeVector<double> p(1*GeV, 1.2*GeV, 0.03*GeV);
    StThreeVector<double> o;  // defaults to (0,0,0)
    const double B = 0.5*tesla;
    const double q = -1;

    cout << "Creating helix with:" << endl;
    cout << "momentum = " << p << endl;
    cout << "origin   = " << o << endl;
    cout << "charge   = " << q << endl;
    cout << "B field  = " << B << endl;

    StPhysicalHelix track(p, o, B, q);

    cout << "The helix parameters are:" << endl;
    cout << track << endl;
    
    cout << "Scanning from s=0 to s=2 m:" << endl;

    for (double s=0; s<=2*meter; s+=40*centimeter) {
        p = track.momentumAt(s, B);
        cout << "s=" << s << " mm  " << "\t-> p = " << p
             << " MeV" << "\t |p| = " << abs(p) << endl; 
    }
    
    return 0;
}
