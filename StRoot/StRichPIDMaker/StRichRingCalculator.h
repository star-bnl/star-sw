/**********************************************************
 * $Id: StRichRingCalculator.h,v 1.2 2000/05/19 19:06:11 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichRingCalculator.h,v $
 *  Revision 1.2  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *
 *  Revision 1.2  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *  Revision 1.1  2000/04/03 19:36:08  horsley
 *  initial revision
 **********************************************************/

#include "StThreeVector.hh"

#include "StRichRingPoint.h"
#include "StThreeVectorF.hh"
#include "StRichMinimization.h"
#include "StParticleDefinition.hh"
#include "StRichRingHit.h"



public:
  double         calculateArea(double cut);
  ~StRichRingCalculator();

  double         calculateArea(double , bool);
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
