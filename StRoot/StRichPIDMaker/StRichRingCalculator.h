/**********************************************************
 * $Id: StRichRingCalculator.h,v 2.2 2000/11/01 17:40:52 lasiuk Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichRingCalculator.h,v $
 *  Revision 2.2  2000/11/01 17:40:52  lasiuk
 *  add const to access member functions
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

    void   clear();
    double calculateArea(double , bool);
    double calculateConstantArea(double , bool, double&);

    void   setParticleType(StParticleDefinition* particle);

    double getTotalArea()          const;
    double getPadPlaneArea()       const;
    double getTotalAngle()         const;
    double getRingWidth()          const;
    double getConstantAreaAngle()  const;
    double getMeanPathInRadiator() const;
    double getMeanPathInQuartz()   const;

    StThreeVectorF& getInnerRingPoint();
    StThreeVectorF& getOuterRingPoint();
    StThreeVectorF& getMeanRingPoint();

    double getInnerDistance(StThreeVectorF& testPoint, double& innerAngle);
    double getOuterDistance(StThreeVectorF& testPoint, double& outerAngle);
    double getMeanDistance(StThreeVectorF& testPoint, double& meanAngle);

    vector<StThreeVectorF >& getPtsToDraw();
 
    StRichRingPoint* getRing(StRichRingDefinition ringType);

private:
  StRichRingPoint* mInnerRing;
  StRichRingPoint* mOuterRing;
  StRichRingPoint* mMeanRing;

  StRichMinimization* mInnerMinimization;
  StRichMinimization* mOuterMinimization;
  StRichMinimization* mMeanMinimization;

  vector<StThreeVectorF> vectorOfPtsToDraw;

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

inline double StRichRingCalculator::getTotalArea() const { return mTotalArea;}
inline double StRichRingCalculator::getPadPlaneArea() const { return mPadPlaneArea;}
inline double StRichRingCalculator::getTotalAngle() const { return mTotalAngleOnPadPlane;}

inline double StRichRingCalculator::getConstantAreaAngle() const {return mConstantAreaAngle;}

inline double StRichRingCalculator::getMeanPathInRadiator() const { return mMeanPathInRadiator;}
inline double StRichRingCalculator::getMeanPathInQuartz() const { return mMeanPathInQuartz;}

inline StThreeVectorF& StRichRingCalculator::getOuterRingPoint() { return closestOuterRingPoint;}
inline StThreeVectorF& StRichRingCalculator::getInnerRingPoint() { return closestInnerRingPoint;}
inline StThreeVectorF& StRichRingCalculator::getMeanRingPoint()  { return closestMeanRingPoint;}
#endif
