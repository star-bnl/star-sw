/**********************************************************
 * $Id: StRichRingCalculator.h,v 2.3 2000/11/21 16:24:23 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichRingCalculator.h,v $
 *  Revision 2.3  2000/11/21 16:24:23  horsley
 *  Major overhaul of StRichArea, introduced monte carlo integration cross check,
 *  all possible areas, angles calculated together. StRichRingCalculator, StRichPIDMaker modified to support new StRichArea. StRichPIDMaker's hit finder
 *  typo corrected.
 *
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
#include "StRichAreaSegment.h"
#include "TArrayD.h"


class StRichMaterialsDb;
class StRichGeometryDb;

class  StRichRingCalculator {

public:
  StRichRingCalculator(StRichTrack* track);
  StRichRingCalculator(StRichTrack* track, StParticleDefinition* );
  ~StRichRingCalculator();
  
  void init(StRichTrack*);

  void   clear();
  double calculateArea(bool = true, double = 0, int = 3600);
  
  void   setParticleType(StParticleDefinition* particle);
  
  double getConstantAreaAngle()  const;
  
  double getTotalArea() const;
  double getTotalAngle() const;

  double getTotalAreaOnPadPlane() const;
  double getTotalAngleOnPadPlane() const;

  double getTotalAreaOnActivePadPlane() const;
  double getTotalAngleOnActivePadPlane() const;

  double getTotalConstantArea() const;
  double getTotalConstantAngle() const;

  double getTotalConstantAreaOnPadPlane() const;
  double getTotalConstantAngleOnPadPlane() const;

  double getTotalConstantAreaOnActivePadPlane() const;
  double getTotalConstantAngleOnActivePadPlane() const;


  double getMeanPathInRadiator() const;
  double getMeanPathInQuartz()   const;
  
  StThreeVectorF& getInnerRingPoint();
  StThreeVectorF& getOuterRingPoint();
  StThreeVectorF& getMeanRingPoint();
  
  double getInnerDistance(StThreeVectorF& testPoint, double& innerAngle);
  double getOuterDistance(StThreeVectorF& testPoint, double& outerAngle);
  double getMeanDistance(StThreeVectorF& testPoint, double& meanAngle);
  double getRingWidth()          const;
  
  double getNormalArea();
  
  void setMonteCarloSwitch(bool);
  void drawRingPoints(bool);
  
  //
  // monte carlo
  //
  TArrayD getMonteCarloArea();
  vector<StThreeVectorF>& getMonteCarloPoints();



  vector<StRichAreaSegment >& getPtsToDraw();  
  StRichRingPoint* getRing(StRichRingDefinition ringType);

private:

  bool mDrawRingPoints;

  StRichRingPoint* mInnerRing;
  StRichRingPoint* mOuterRing;
  StRichRingPoint* mMeanRing;

  StRichMinimization* mInnerMinimization;
  StRichMinimization* mOuterMinimization;
  StRichMinimization* mMeanMinimization;

  vector<StRichAreaSegment> vectorOfPtsToDraw;
  vector<StThreeVectorF> mMonteCarloPoints;
  TArrayD mMonteCarloArea;


  StThreeVectorF  closestInnerRingPoint;
  StThreeVectorF  closestOuterRingPoint;
  StThreeVectorF  closestMeanRingPoint;


  StRichMaterialsDb*   mRichMaterialsDb;
  StRichGeometryDb*   mRichGeometryDb;

  
  double mTotalArea,mTotalAngle;
  double mTotalAreaOnPadPlane,mTotalAngleOnPadPlane;
  double mTotalAreaOnActivePadPlane,mTotalAngleOnActivePadPlane;

  double mTotalConstantArea,mTotalConstantAngle;
  double mTotalConstantAreaOnPadPlane,mTotalConstantAngleOnPadPlane;
  double mTotalConstantAreaOnActivePadPlane,mTotalConstantAngleOnActivePadPlane;

  bool mMonteCarloSwitch;

  double mConstantAreaAngle;
  double mMeanPathInRadiator;
  double mMeanPathInQuartz;
};



inline double StRichRingCalculator::getConstantAreaAngle() const {return mConstantAreaAngle;}


inline double StRichRingCalculator::getTotalArea() const {return mTotalArea;}
inline double StRichRingCalculator::getTotalAngle() const {return mTotalAngle;}

inline double StRichRingCalculator::getTotalAreaOnPadPlane() const {return mTotalAreaOnPadPlane;}
inline double StRichRingCalculator::getTotalAngleOnPadPlane() const {return mTotalAngleOnPadPlane;}

inline double StRichRingCalculator::getTotalAreaOnActivePadPlane() const {return mTotalAreaOnActivePadPlane;}
inline double StRichRingCalculator::getTotalAngleOnActivePadPlane() const {return mTotalAngleOnActivePadPlane;}


inline double StRichRingCalculator::getTotalConstantArea() const {return mTotalConstantArea;}
inline double StRichRingCalculator::getTotalConstantAngle() const {return mTotalConstantAngle;}

inline double StRichRingCalculator::getTotalConstantAreaOnPadPlane() const {return mTotalConstantAreaOnPadPlane;}
inline double StRichRingCalculator::getTotalConstantAngleOnPadPlane() const {return mTotalConstantAngleOnPadPlane;}

inline double StRichRingCalculator::getTotalConstantAreaOnActivePadPlane() const {return mTotalConstantAreaOnActivePadPlane;}
inline double StRichRingCalculator::getTotalConstantAngleOnActivePadPlane() const {return mTotalConstantAngleOnActivePadPlane;}



inline double StRichRingCalculator::getMeanPathInRadiator() const { return mMeanPathInRadiator;}
inline double StRichRingCalculator::getMeanPathInQuartz() const { return mMeanPathInQuartz;}

inline StThreeVectorF& StRichRingCalculator::getOuterRingPoint() { return closestOuterRingPoint;}
inline StThreeVectorF& StRichRingCalculator::getInnerRingPoint() { return closestInnerRingPoint;}
inline StThreeVectorF& StRichRingCalculator::getMeanRingPoint()  { return closestMeanRingPoint;}
#endif
