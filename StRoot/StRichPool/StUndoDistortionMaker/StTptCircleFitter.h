/***************************************************************************
 *
 * $Id: StTptCircleFitter.h,v 1.1 2002/11/19 18:44:28 dunlop Exp $
 *
 * 
 * Modified by bum by copying the tpc circle fitting algorithm.
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
 *                     1: not enough points (<minPoint;)
 *                     2: cant find a solution
 *                     3: one of the matrix elements is 0
 *                     4: 0 radius
 * StTpcCircleFitter::fit() returns true only if rc = 0
 *
 ***************************************************************************
 *
 * $Log: StTptCircleFitter.h,v $
 * Revision 1.1  2002/11/19 18:44:28  dunlop
 * This undoes the distortions and refits.
 * Modifications made to Bum's refitter in order to do a "primary" fit.
 * Much like non-Kalman primary fit used to be.
 *
 * Revision 1.1  1999/12/21 16:28:50  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_TPT_CIRCLE_FITTER_HH
#define ST_TPT_CIRCLE_FITTER_HH

#include <vector>
#if !defined(ST_NO_NAMESPACES)
using std::vector;
#endif

class StTptCircleFitter {
public:
  StTptCircleFitter();
  ~StTptCircleFitter();
  
  bool   fit();
  void   clear();	 // full reset
  
  void   addPoint(double x, double y,double err);  
  
  double radius() const;   // returns fitted radius
  double xcenter() const;  // returns x of fitted center
  double ycenter() const;  // returns y of fitted center
  double variance() const; // variance estimate
  int    rc() const;       // return code of fit
  unsigned int numberOfPoints() const;
  int    sign() const;
  
private:
  //bool   helper(); // does some coordinate transforms,gauss brackets, etc.

#if defined(ST_NO_TEMPLATE_DEF_ARGS)
  vector<double, allocator<double> > mX; // x-coordinates of points
  vector<double, allocator<double> > mY; // y-coordinates of points
  vector<double, allocator<double> > mError; // error of point
#else
  vector<double> mX; // x-coordinates of points
  vector<double> mY; // y-coordinates of points
  vector<double> mError;
#endif
  double  mRadius;     
  double  mXCenter;    
  double  mYCenter;
  double  mVariance; // the estimate of variance      
  int     mRC;
  int     mSign;

};
#endif
