/**********************************************************
 * $Id: StRichMinimization.h,v 2.0 2000/08/09 16:26:18 gans Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichMinimization.h,v $
 *  Revision 2.0  2000/08/09 16:26:18  gans
 *  Naming Convention for TDrawable Ojects. All drawable objects now in StRichDisplayMaker
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
#include "StThreeVector.hh"
 **********************************************************/

#ifndef StRichMinimization_h
#define StRichMinimization_h

#include "StRichRingPoint.h"
#include "StThreeVectorF.hh"
          StThreeVector<double> rotatedMin(StThreeVector<double>& point);

  
          StRichMinimization(StRichRingPoint* rp);
          ~StRichMinimization();
          StThreeVectorF rotatedMin(StThreeVectorF& point);
          double getPsi();
          double getMeanPathInRadiator();
          double getMeanPathInQuartz();

private:
  
  StThreeVector<double> returnThisPoint;
  StRichRingPoint* ringPoint;
  double psi,returnPsi;
  double minDistance,mTolerance;
  double mMeanPathInRadiator;
  bool status;
  StThreeVectorF returnThisPoint;
    double degree;
};

#endif
