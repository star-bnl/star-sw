#ifndef StFlowTagMaker_HH
#define StFlowTagMaker_HH

///////////////////////////////////////////////////////////////////////////////
//
// StFlowTagMaker
//
// Description: 
//  Maker to access and analyze StEvent and fill Tag for flow analysis
//
// Environment:
//  Software developed for the STAR Detector at LBNL
//
// Author List: 
//  Raimond Snellings, LBNL, 6/99
//
// History:
//
///////////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include "StMaker.h"
#include "tables/FlowTag.h"

class StEvent;

class StFlowTagMaker : public StMaker 
{

public:

  StFlowTagMaker(const Char_t *name="FlowTag", const Char_t *title="FlowTag");
  ~StFlowTagMaker();
  Int_t  Make();
  void   PrintInfo();

  FlowTag_st* tag(); // returns pointer to the tag table

  // output Tag info to screen
  void printTag(ostream& = cout);

protected:

  void fillFlowTag();
  Int_t phiRapidityWeight(float PhiAngle, float PseudoRapidity, 
			  float Pt, float *weight);
  Int_t eventPlane(long Multiplicity, float *mPseudoRapidity, float *mPhiAngle, 
		   float *mPt, double *mQx, double *mQy, double *mEventPlaneAngle,
		   int OrderParameter=2, int PhiYWeigt=0);
  void SWAP(long &a,long &b);
  void indexx(long n,float arr[], long indx[]);

private:

  FlowTag_st*   mFlowTag;        //! the tag table to fill
  StEvent*      mEvent;           //! pointer to DST data


  ClassDef(StFlowTagMaker, 1)  // macro for rootcint
};

#endif
