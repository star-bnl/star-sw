/**********************************************************
 * $Id: StRichMinimization.h,v 2.2 2000/10/19 18:11:12 lasiuk Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichMinimization.h,v $
 *  Revision 2.2  2000/10/19 18:11:12  lasiuk
 *  definition of degree
 *
 *  Revision 2.2  2000/10/19 18:11:12  lasiuk
 *  definition of degree
 *
 *  Revision 2.1  2000/09/29 01:35:37  horsley
 *  Many changes, added StRichRingHits, StRichMcSwitch, TpcHitvecUtilities
 *  Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
 *
 *  Revision 1.1  2000/04/03 19:36:08  horsley
 *  initial revision
 *
 *  
 *
 **********************************************************/

#ifndef StRichMinimization_h
#define StRichMinimization_h

#include "StRichRingPoint.h"
#include "StThreeVectorF.hh"
#include "StRichTrack.h"

class  StRichMinimization {

public:
          StRichMinimization(StRichRingPoint* rp);
          ~StRichMinimization();
          StThreeVectorF rotatedMin(StThreeVectorF& point);
          double getPsi();
          double getMeanPathInRadiator();
          double getMeanPathInQuartz();

private:
  // minimization routine
  double brent(double ax, double bx, double cx, double *xmin);
  
  // data members
  StRichRingPoint* ringPoint;
  double psi,returnPsi;
  double minDistance,mTolerance;
  double mMeanPathInRadiator;
  double mMeanPathInQuartz;
  bool status;
  StThreeVectorF returnThisPoint;
    double degree;
};

#endif
