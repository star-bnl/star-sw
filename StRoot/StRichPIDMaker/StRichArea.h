/**********************************************************
 * $Id: StRichArea.h,v 2.0 2000/08/09 16:26:17 gans Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichArea.h,v $
 *  Revision 2.0  2000/08/09 16:26:17  gans
 *  Naming Convention for TDrawable Ojects. All drawable objects now in StRichDisplayMaker
 *
 *  Revision 1.2  2000/05/19 19:06:10  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *  Revision 1.1  2000/04/03 19:36:07  horsley
 *  initial revision
**********************************************************/

#ifndef StRichArea_H
#define StRichArea_H

#include "StThreeVector.hh"
#include "StRichRingPoint.h"


class StRichGeometryDb;

class StRichArea {

public:
  StRichArea(StRichRingPoint*, StRichRingPoint*);
  ~StRichArea();
  double    calculateArea(double);
  double getTotalAngleOnPadPlane();
  vector<StThreeVector<double> >& getPtsToDraw();
  void correctForGap(bool);
  
private:
    
  double getStoppingAngle(double);
  
  bool   getRingPoints(double&, 
		       StThreeVector<double>&, 
		       StThreeVector<double>&, 
		       int);
  
  void getAreaSegment(StThreeVector<double>& , 
		      StThreeVector<double>& ,
		      StThreeVector<double>& , 
		      StThreeVector<double>&,
		      double&);
  
  void swapPoints(StThreeVector<double>&, 
		  StThreeVector<double>&,
		  StThreeVector<double>&, 
		  StThreeVector<double>& );
  
  bool fullGapCorrectionNecessary(StThreeVector<double>&, 
				            StThreeVector<double>&,
				            StThreeVector<double>&,
				            StThreeVector<double>&);
  
  bool partialGapCorrection(StThreeVector<double>&, 
			            StThreeVector<double>&,
			            StThreeVector<double>&,
			            StThreeVector<double>&);

  bool nonAdjacentGapCorrection(StThreeVector<double>&, 
				                 StThreeVector<double>&,
				                 StThreeVector<double>&,
				                 StThreeVector<double>&,
				                 StThreeVector<double>&,
				                 StThreeVector<double>&);
  
  bool quadCheck(StThreeVector<double>&, StThreeVector<double>&);
  
  bool adjacentCheck(StThreeVector<double>&,StThreeVector<double>&);
  
  bool gapCheck(StThreeVector<double>&);
  
  bool outOfBoundsCorrection(StThreeVector<double>&, StThreeVector<double>&);
  
  bool inBounds(StThreeVector<double>&);
  
  bool sanityCheck(StThreeVector<double>& ,
		        StThreeVector<double>& ,
		        StThreeVector<double>& );
  
  // data members
  bool    mCorrectForGap;
  double mNumberOfSteps;
  
  int mTooManyCounts;
  double mNotEnoughAngleForCalculation;

  double mTotalAngleOnPadPlane;
  double mStartAngle;
  double mStopAngle;
  double mAngleIncrement;
  double mSmallAngleStep;
  int     mStopCounter;  

  double mSmallDistance;
  double mPositiveDirection;
  double mNegativeDirection;
  
  double mAreaSegment;
  double mTotalArea;
  
  StThreeVector<double> mInXYA;
  StThreeVector<double> mOutXYA;
  StThreeVector<double> mInXYB;
  StThreeVector<double> mOutXYB;

  StThreeVector<double> mInnerPtForDrawingA,mInnerPtForDrawingB,mInnerPtForDrawingC;
  StThreeVector<double> mOuterPtForDrawingA,mOuterPtForDrawingB,mOuterPtForDrawingC;
  vector<StThreeVector<double> > vectorOfPtsToDraw; //!

  StRichRingPoint* mInnerRing;
  StRichRingPoint* mOuterRing;
  
  StRichGeometryDb* myGeometryDb;
};




#endif









