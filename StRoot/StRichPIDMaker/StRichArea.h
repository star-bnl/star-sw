/**********************************************************
 * $Id: StRichArea.h,v 2.1 2000/09/29 01:35:36 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichArea.h,v $
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


class StRichGeometryDb;

class StRichArea {

public:
  StRichArea(StRichRingPoint*, StRichRingPoint*);
  ~StRichArea();
  double  calculateArea(double);
  double  getConstantAreaAngle();
  double  calculateConstantArea(double);
  double  getTotalAngleOnPadPlane();
  double  getTotalArea();
  void    correctForGap(bool);
  vector<StThreeVectorF >& getPtsToDraw();

  
private:
    
  double getStoppingAngle(double);
  
  bool   getRingPoints(double&, 
		       StThreeVectorF&, 
		       StThreeVectorF&, 
		       int);
  
  void getAreaSegment(StThreeVectorF& , 
		      StThreeVectorF& ,
		      StThreeVectorF& , 
		      StThreeVectorF&,
		      double&, double&);
  
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
  
  
  void drawAreaRingPoints(bool);
  
  // data members
  bool   mCorrectForGap;
  bool   mDrawAreaRingPoints;

  int    mTooManyCounts;
  int    mStopCounter;  
 
  double mNumberOfSteps;
  double mNotEnoughAngleForCalculation;
  double mConstantAreaAngleCut;
  double mTotalAngleOnPadPlane;
  
  double mStartAngle;
  double mStopAngle;
  double mAngleIncrement;
  double mSmallAngleStep;
 
  double mSmallDistance;
  double mPositiveDirection;
  double mNegativeDirection;

  double mCorrectedAreaSegment;
  double mUnCorrectedAreaSegment;
  double mTotalArea;
  double mTotalAreaOnPadPlane;
  
  StThreeVectorF mInXYA;
  StThreeVectorF mOutXYA;
  StThreeVectorF mInXYB;
  StThreeVectorF mOutXYB;

  StThreeVectorF mInnerPtForDrawingA,mInnerPtForDrawingB,mInnerPtForDrawingC;
  StThreeVectorF mOuterPtForDrawingA,mOuterPtForDrawingB,mOuterPtForDrawingC;
  vector<StThreeVectorF > vectorOfPtsToDraw; //!

  StRichRingPoint* mInnerRing;
  StRichRingPoint* mOuterRing;
  
  StRichGeometryDb* myGeometryDb;
};




#endif









