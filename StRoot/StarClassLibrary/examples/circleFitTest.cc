/***************************************************************************
 *
 * $Id: circleFitTest.cc,v 1.2 2003/09/02 17:59:37 perev Exp $
 *
 * Author: Thomas Ullrich, Dec 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: circleFitTest.cc,v $
 * Revision 1.2  2003/09/02 17:59:37  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  1999/12/21 16:29:39  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StFastCircleFitter.hh"
#include <Stiostream.h>
#include <cstdlib>

int main()
{
    cout << "This program tests the StFastCircleFitter class" << endl;
    cout << "-----------------------------------------------" << endl;

    StFastCircleFitter fitter;

    const unsigned int nPoints = 42;
    const double x0 = 10.23;
    const double y0 = 0.018;
    const double r  = 19.91;
    double phi;
    int i;

    cout << "Input:    x0 = " << x0 << endl;
    cout << "          y0 = " << y0 << endl;
    cout << "          r  = " << r << endl;
    cout << "          n  = " << nPoints << endl;
    
    for (i=0; i<nPoints; i++) {
	phi = drand48();
	fitter.addPoint(r*cos(phi)+x0, r*sin(phi)+y0);
    }

    fitter.fit();
    cout << "Fit 1:    x0 = " << fitter.xcenter() << endl;
    cout << "          y0 = " << fitter.ycenter() << endl;
    cout << "          r  = " << fitter.radius() << endl;
    cout << "          n  = " << fitter.numberOfPoints() << endl;

    fitter.clear();
    
    for (i=0; i<nPoints; i++) {
	phi = drand48();
	fitter.addPoint(r*cos(phi)+x0, r*sin(phi)+y0);
    }

    fitter.fit();
    cout << "Fit 2:    x0 = " << fitter.xcenter() << endl;
    cout << "          y0 = " << fitter.ycenter() << endl;
    cout << "          r  = " << fitter.radius() << endl;
    cout << "          n  = " << fitter.numberOfPoints() << endl;

    cout << "done\n" << endl;
    return 0;
}
