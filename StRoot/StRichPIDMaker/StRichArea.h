/**********************************************************
 * $Id: StRichArea.h,v 2.2 2000/11/21 16:24:22 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichArea.h,v $
 *  Revision 2.2  2000/11/21 16:24:22  horsley
 *  Major overhaul of StRichArea, introduced monte carlo integration cross check,
 *  all possible areas, angles calculated together. StRichRingCalculator, StRichPIDMaker modified to support new StRichArea. StRichPIDMaker's hit finder
 *  typo corrected.
 *
 *  Revision 2.1  2000/09/29 01:35:36  horsley
 *  Many changes, added StRichRingHits, StRichMcSwitch, TpcHitvecUtilities
 *  Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
 *
 *  Revision 1.2  2000/05/19 19:06:10  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *  Revision 1.1  2000/04/03 19:36:07  horsley
 *  initial revision
**********************************************************/

#ifndef StRichArea_H
#define StRichArea_H

#include "StThreeVectorF.hh"
#include "StRichRingPoint.h"
#include "StRichAreaSegment.h"


#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif


// root stuff (sorry!!)
#include "TRandom.h"



class StRichGeometryDb;
class StRichMinimization;


class StRichArea {

public:
  StRichArea(StRichRingPoint*, StRichRingPoint*);
  ~StRichArea();
  void    setPoints(int);
  double  getConstantAreaAngle();
  double  calculateArea(double=0, double=0);

  double  getTotalAngle();
  double  getTotalArea();

  double  getTotalAngleOnPadPlane();
  double  getTotalAreaOnPadPlane();

  double  getTotalAngleOnActivePadPlane();
  double  getTotalAreaOnActivePadPlane();

  double  getTotalConstantAngle();
  double  getTotalConstantArea();

  double  getTotalConstantAngleOnPadPlane();
  double  getTotalConstantAreaOnPadPlane();

  double  getTotalConstantAngleOnActivePadPlane();
  double  getTotalConstantAreaOnActivePadPlane();



  void    correctForGap(bool);
  vector<StRichAreaSegment >& getPtsToDraw();
  void drawAreaRingPoints(bool);
  
  //
  // monte carlo
  //
  void getMonteCarloArea(double, TArrayD&, int);
  vector<StThreeVectorF> getMonteCarloPoints(); 


private:
    
  double getStoppingAngle(double);
  
  bool   getRingPoints(double&,double&, 
		       StThreeVectorF&, 
		       StThreeVectorF&, 
		       int);
  
  void getAreaSegment(StRichAreaSegment&, double, double,
		      StThreeVectorF& , 
		      StThreeVectorF& ,
		      StThreeVectorF& , 
		      StThreeVectorF&,
		      double&, double&, double&);
  
  void swapPoints(StThreeVectorF&, 
		  StThreeVectorF&,
		  StThreeVectorF&, 
		  StThreeVectorF& );
  
  bool fullGapCorrectionNecessary(StThreeVectorF&, 
				  StThreeVectorF&,
				  StThreeVectorF&,
				  StThreeVectorF&);
  
  bool partialGapCorrection(StThreeVectorF&, 
			    StThreeVectorF&,
			    StThreeVectorF&,
			    StThreeVectorF&);

  bool nonAdjacentGapCorrection(StThreeVectorF&, 
				StThreeVectorF&,
				StThreeVectorF&,
				StThreeVectorF&,
				StThreeVectorF&,
				StThreeVectorF&);
  
  bool quadCheck(StThreeVectorF&, StThreeVectorF&);
  
  bool adjacentCheck(StThreeVectorF&,StThreeVectorF&);
  
  bool gapCheck(StThreeVectorF&);
  
  bool outOfBoundsCorrection(StThreeVectorF&, StThreeVectorF&);
  
  bool inBounds(StThreeVectorF&);
  
  bool sanityCheck(StThreeVectorF& , StThreeVectorF& ,StThreeVectorF& );
  

  //
  // monte carlo
  //
  int monteCarloHitFinder(double, double&);
  void getMonteCarloPadPlanePoint(TRandom*);



  //
  // data members
  //  
  bool   mCorrectForGap;
  bool   mDrawAreaRingPoints;

  int    mTooManyCounts;
  int    mStopCounter;  
 
  double mNumberOfSteps;
  double mNotEnoughAngleForCalculation;
  
  double mConstantAreaAngleCut;
  
  double mTotalArea,mTotalAngle;
  double mTotalAreaOnPadPlane,mTotalAngleOnPadPlane;
  double mTotalAreaOnActivePadPlane,mTotalAngleOnActivePadPlane;
  
  double mTotalConstantArea,mTotalConstantAngle;
  double mTotalConstantAreaOnPadPlane,mTotalConstantAngleOnPadPlane;
  double mTotalConstantAreaOnActivePadPlane,mTotalConstantAngleOnActivePadPlane;
  

  double mStartAngle;
  double mStopAngle;
  double mAngleIncrement;
  double mSmallAngleStep;
 
  double mSmallDistance;
  double mPositiveDirection;
  double mNegativeDirection;

  double mAreaSegment;
  double mCorrectedAreaSegment;
  double mUnCorrectedAreaSegment;
  
  
  StThreeVectorF mInXYA;
  StThreeVectorF mOutXYA;
  StThreeVectorF mInXYB;
  StThreeVectorF mOutXYB;

  StThreeVectorF mInnerPtForDrawingA,mInnerPtForDrawingB,mInnerPtForDrawingC;
  StThreeVectorF mOuterPtForDrawingA,mOuterPtForDrawingB,mOuterPtForDrawingC;
  vector<StRichAreaSegment > vectorOfPtsToDraw; //!

  StRichRingPoint* mInnerRing;
  StRichRingPoint* mOuterRing;
  
  StRichGeometryDb* myGeometryDb;


  // 
  // for monte carlo integration
  //
  StRichMinimization* mInnerMinimization;
  StRichMinimization* mOuterMinimization;
  StRichMinimization* mMeanMinimization;
  
  StThreeVectorF mMonteCarloPoint;
  vector<StThreeVectorF> mMonteCarloPointCollection;

  double mXGapWidth;
  double mYGapWidth;
  
  double mRadX;
  double mRadY;
};




inline void StRichArea::setPoints(int npts) {
  mNumberOfSteps  = npts;
}

inline void StRichArea::drawAreaRingPoints(bool drawSwitch) {
  mDrawAreaRingPoints = drawSwitch;
}

inline double StRichArea::getTotalArea() {
  return mTotalArea;
}

inline double StRichArea::getTotalAngle() {
  return mTotalAngle;
}


inline double StRichArea::getTotalAreaOnPadPlane() {
  return mTotalAreaOnPadPlane;
}

inline double StRichArea::getTotalAngleOnPadPlane() {
  return mTotalAngleOnPadPlane;
}


inline double StRichArea::getTotalAreaOnActivePadPlane() {
  return mTotalAreaOnActivePadPlane;
}

inline double StRichArea::getTotalAngleOnActivePadPlane() {
  return mTotalAngleOnActivePadPlane;
}


inline double StRichArea::getTotalConstantArea() {
  return mTotalConstantArea;
}

inline double StRichArea::getTotalConstantAngle() {
  return mTotalConstantAngle;
}


inline double StRichArea::getTotalConstantAreaOnPadPlane() {
  return mTotalConstantAreaOnPadPlane;
}

inline double StRichArea::getTotalConstantAngleOnPadPlane() {
  return mTotalConstantAngleOnPadPlane;
}


inline double StRichArea::getTotalConstantAreaOnActivePadPlane() {
  return mTotalConstantAreaOnActivePadPlane;
}

inline double StRichArea::getTotalConstantAngleOnActivePadPlane() {
  return mTotalConstantAngleOnActivePadPlane;
}


inline double StRichArea::getConstantAreaAngle() {
  return mConstantAreaAngleCut;
}


inline void StRichArea::correctForGap(bool gapSwitch) {
  mCorrectForGap = gapSwitch;
}




#endif









