/**********************************************************
 * $Id: StRichRingCalculator.h,v 2.1 2000/09/29 01:35:37 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichRingCalculator.h,v $
 *  Revision 2.1  2000/09/29 01:35:37  horsley
 *  Many changes, added StRichRingHits, StRichMcSwitch, TpcHitvecUtilities
 *  Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
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
#define StRichRingCalculator_h

#include "StRichRingPoint.h"
#include "StRichTrack.h"
#include "StThreeVectorF.hh"
#include "StRichMinimization.h"
#include "StParticleDefinition.hh"
#include "StRichRingHit.h"


class  StRichRingCalculator {

public:
  StRichRingCalculator(StRichTrack* track);
  StRichRingCalculator(StRichTrack* track, StParticleDefinition* );
  ~StRichRingCalculator();

  double         calculateArea(double , bool);
  double         calculateConstantArea(double , bool, double&);
  double         getTotalArea();
  double         getPadPlaneArea();

  vector<StThreeVectorF >& getPtsToDraw();
  StRichRingPoint* getRing(StRichRingDefinition ringType);

  void   setParticleType(StParticleDefinition* particle);
  double getTotalAngle();
  double getConstantAreaAngle();

  double getInnerDistance(StThreeVectorF& testPoint, double& innerAngle);
  double getOuterDistance(StThreeVectorF& testPoint, double& outerAngle);
  double getMeanDistance(StThreeVectorF& testPoint, double& meanAngle);

  double getMeanPathInRadiator();
  double getMeanPathInQuartz();

  void   clear();
  double getRingWidth();
  
  StThreeVectorF getInnerRingPoint();
  StThreeVectorF getOuterRingPoint();
  StThreeVectorF getMeanRingPoint();
 


private:
  StRichRingPoint* mInnerRing;
  StRichRingPoint* mOuterRing;
  StRichRingPoint* mMeanRing;

  StRichMinimization* mInnerMinimization;
  StRichMinimization* mOuterMinimization;
  StRichMinimization* mMeanMinimization;

  vector<StThreeVectorF > vectorOfPtsToDraw;

  StThreeVectorF  closestInnerRingPoint;
  StThreeVectorF  closestOuterRingPoint;
  StThreeVectorF  closestMeanRingPoint;

  double mTotalArea;
  double mPadPlaneArea;
  double mTotalAngleOnPadPlane;
  double mConstantAreaAngle;
  double mMeanPathInRadiator;
  double mMeanPathInQuartz;
};

#endif
