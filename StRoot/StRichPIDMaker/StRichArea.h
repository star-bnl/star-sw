/**********************************************************
 * $Id: StRichArea.h,v 1.1 2000/04/03 19:36:07 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichArea.h,v $
 *  Revision 1.1  2000/04/03 19:36:07  horsley
 *  initial revision
 *
 *  
 *
 **********************************************************/

#ifndef StRichArea_H
#define StRichArea_H

#include "StThreeVector.hh"
#include "StRichRingPoint.h"
#include "TNtuple.h"


class StRichGeometryDb;

class StRichArea {

public:
  StRichArea(StRichRingPoint* irp, StRichRingPoint* orp);
  ~StRichArea();
  void    calculateArea(double psiCut);
  double getPadPlaneArea();
  double getTotalArea();
  
private:

 
  void getStoppingAngles(double& ipsi, double& opsi, 
			        double psiCut);
  
  bool   getRingPoints(double& psi1, double& psi2,
		             StThreeVector<double>& ixy, StThreeVector<double>& oxy,
		             int direction);
  
  void getAreaSegment(StThreeVector<double>& ixya, StThreeVector<double>& oxya,
		            StThreeVector<double>& ixyb, StThreeVector<double>& oxyb,
		            double& totArea, double& padArea);

  void swapPoints(StThreeVector<double>& ixya, StThreeVector<double>& oxya,
		      StThreeVector<double>& ixyb, StThreeVector<double>& oxyb);
  
  bool fullGapCorrectionNecessary(StThreeVector<double>& ixy, 
                                            StThreeVector<double>& oxy,
			                    StThreeVector<double>& itemp,
				            StThreeVector<double>& otemp);

  bool partialGapCorrectionNecessary(StThreeVector<double>& ixy, 
				                StThreeVector<double>& oxy);
  
  bool quadCheck(StThreeVector<double>& ixy, StThreeVector<double>& oxy);

  bool gapCheck(StThreeVector<double>& xy);
  
  bool outOfBoundsCorrection(StThreeVector<double>& ixy, StThreeVector<double>& oxy);
  
  bool inBounds(StThreeVector<double> xy);

  // data members
  double mInnerAngleIncrement;
  double mOuterAngleIncrement;
  double mSmallAngleStep;
  double mSmallDistance;
  double mPositiveDirection;
  double mNegativeDirection;
  double mSmallAngleIncrement; 
  double mAreaElement;
  double mTotalArea;
  double mPadPlaneArea;
  double mNumberOfSteps;
  
  StThreeVector<double> mInXYA;
  StThreeVector<double> mOutXYA;
  StThreeVector<double> mInXYB;
  StThreeVector<double> mOutXYB;
  
  bool mStatus;
  bool mCorrectForGap;
  
  StRichRingPoint* mInnerRing;
  StRichRingPoint* mOuterRing;
  
  StRichGeometryDb* myGeometryDb;
  
  //  TNtuple* ntup;
  
};




#endif









