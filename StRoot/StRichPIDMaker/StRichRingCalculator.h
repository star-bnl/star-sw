/**********************************************************
 * $Id: StRichRingCalculator.h,v 1.1 2000/04/03 19:36:08 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichRingCalculator.h,v $
 *  Revision 1.1  2000/04/03 19:36:08  horsley
 *  initial revision
 *
 *  
 *
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *  Revision 1.1  2000/04/03 19:36:08  horsley
 *  initial revision
 **********************************************************/

#include "StThreeVector.hh"

#include "StRichRingPoint.h"
#include "StThreeVectorF.hh"
#include "StParticleDefinition.hh"
#include "StRichRingHit.h"



public:
  void    calculateArea(double cut); 
  double getTotalArea();
  double getPadPlaneArea();
  double         calculateArea(double , bool);
  double         getTotalArea();
  
  void setParticleType(StParticleDefinition* particle);
    
  double getInnerDistance(StThreeVector<double>& testPoint, 
			         double& innerAngle);
  
  double getOuterDistance(StThreeVector<double>& testPoint, 
			         double& outerAngle);
  double getMeanDistance(StThreeVectorF& testPoint, double& meanAngle);
  double getMeanPathInRadiator();
  double getMeanPathInQuartz();
  StThreeVectorF getMeanRingPoint();
 

private:
  StRichRingPoint* mInnerRing;
  StRichRingPoint* mOuterRing;
  
  StThreeVector<double>  closestInnerRingPoint;
  StThreeVector<double>  closestOuterRingPoint;

  StThreeVectorF  closestInnerRingPoint;
  StThreeVectorF  closestOuterRingPoint;
  double mTotalAngleOnPadPlane;
  double mConstantAreaAngle;
  double mMeanPathInRadiator;
  double mMeanPathInQuartz;
};

#endif
