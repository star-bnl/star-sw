/***************************************************************************
 *
 * $Id: StFastCircleFitter.hh,v 1.1 1999/12/21 16:28:50 ullrich Exp $
 *
 * Author: Thomas Ullrich, Dec 1999
 ***************************************************************************
 *
 * Description:
 *
 * Fast fitting routine using a iterational linear regression 
 * method (ILRM). Reference: N.Chernov, G.A.Ososkov, Computer  
 * Physics Communication 33 (1984) 329-333.                   
 *
 * Return codes:  0    fit ok 
 *                1-4  error occured, no results
 *
 * StFastCircleFitter::fit() returns true only if rc = 0
 *
 ***************************************************************************
 *
 * $Log: StFastCircleFitter.hh,v $
 * Revision 1.1  1999/12/21 16:28:50  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_FAST_CIRCLE_FITTER_HH
#define ST_FAST_CIRCLE_FITTER_HH

#include <vector>
#if !defined(ST_NO_NAMESPACES)
using std::vector;
#endif

class StFastCircleFitter {
public:
    StFastCircleFitter();
    ~StFastCircleFitter();

    bool   fit();
    void   clear();	 // full reset

    void   addPoint(double x, double y);  
    
    double radius() const;   // returns fitted radius
    double xcenter() const;  // returns x of fitted center
    double ycenter() const;  // returns y of fitted center
    double variance() const; // variance estimate
    int    rc() const;       // return code of fit
    unsigned int numberOfPoints() const;
    
private:
#if defined(ST_NO_TEMPLATE_DEF_ARGS)
    vector<double, allocator<double> > mX; // x-coordinates of points
    vector<double, allocator<double> > mY; // y-coordinates of points
#else
    vector<double> mX; // x-coordinates of points
    vector<double> mY; // y-coordinates of points
#endif
    double  mRadius;     
    double  mXCenter;    
    double  mYCenter;
    double  mVariance; // the estimate of variance      
    int     mRC;
};

#endif
