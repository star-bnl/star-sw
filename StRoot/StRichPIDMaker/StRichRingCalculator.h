/**********************************************************
 * $Id: StRichRingCalculator.h,v 2.0 2000/08/09 16:26:20 gans Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichRingCalculator.h,v $
 *  Revision 2.0  2000/08/09 16:26:20  gans
 *  Naming Convention for TDrawable Ojects. All drawable objects now in StRichDisplayMaker
 *
 *  Revision 2.1  2000/09/29 01:35:37  horsley
 *  Many changes, added StRichRingHits, StRichMcSwitch, TpcHitvecUtilities
 *  Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
 *
 *  Revision 1.2  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *  Revision 1.1  2000/04/03 19:36:08  horsley
 *  initial revision
 **********************************************************/

#ifndef StRichRingCalculator_h
#include "StThreeVector.hh"

#include "StRichRingPoint.h"
#include "StThreeVectorF.hh"
#include "StRichMinimization.h"
#include "StParticleDefinition.hh"
#include "StRichRingHit.h"



public:
  double         calculateArea(double cut=0, bool gapCorrection=true);

  ~StRichRingCalculator();


  double         calculateArea(double , bool);
  vector<StThreeVector<double> >& getPtsToDraw();

  double         getTotalArea();
  
  void setParticleType(StParticleDefinition* particle);
  double         getPadPlaneArea();
  vector<StThreeVectorF >& getPtsToDraw();
    
  double getInnerDistance(StThreeVector<double>& testPoint, 
			         double& innerAngle);
  
  double getOuterDistance(StThreeVector<double>& testPoint, 
			         double& outerAngle);
  
  double getMeanDistance(StThreeVector<double>& testPoint, 
			        double& meanAngle);

  void clear();
  double getMeanDistance(StThreeVectorF& testPoint, double& meanAngle);
  double getMeanPathInRadiator();
  double getMeanPathInQuartz();
  StThreeVector<double> getInnerRingPoint();
  StThreeVector<double> getOuterRingPoint();
  StThreeVector<double> getMeanRingPoint();
  StThreeVectorF getInnerRingPoint();
  StThreeVectorF getOuterRingPoint();
  StThreeVectorF getMeanRingPoint();
 


private:
  StRichRingPoint* mInnerRing;
  StRichRingPoint* mOuterRing;
  StRichRingPoint* mMeanRing;

  vector<StThreeVector<double> > vectorOfPtsToDraw;
  StRichMinimization* mOuterMinimization;
  StThreeVector<double>  closestInnerRingPoint;
  StThreeVector<double>  closestOuterRingPoint;
  StThreeVector<double>  closestMeanRingPoint;

  StThreeVectorF  closestInnerRingPoint;
  StThreeVectorF  closestOuterRingPoint;
  StThreeVectorF  closestMeanRingPoint;
  double mTotalAngleOnPadPlane;
  double mConstantAreaAngle;
  double mMeanPathInRadiator;
  double mMeanPathInQuartz;
};

#endif
