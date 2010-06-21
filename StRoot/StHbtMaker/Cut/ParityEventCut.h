/***************************************************************************
 *
 * $Id: ParityEventCut.h,v 1.4 2010/06/21 12:57:00 fine Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   A simple event-wise cut that selects on multiplicity and z-position
 *   of primary vertex           
 *
 ***************************************************************************
 **************************************************************************/

#ifndef ParityEventCut_hh
#define ParityEventCut_hh

// do I need these lines ?
//#ifndef StMaker_H
//#include "StMaker.h"
//#endif

#include "StHbtMaker/Base/StHbtEventCut.h"

class ParityEventCut : public StHbtEventCut {

public:

  ParityEventCut(const char* title = "Parity Cut", const int& nbins =50 , const float& Lo =-100, const float& Hi =100);
  ~ParityEventCut();

  void SetEventMult(const int& lo,const int& hi);
  void SetVertZPos(const float& lo, const float& hi);

  virtual StHbtString Report();

  virtual bool Pass(const StHbtEvent*);


  // note that these data are public-- the CorrFctn will access (increment) them...
  double RealQuantity;
  long nReals;
  double MixedQuantity;
  long nMixed;

  StHbt1DHisto* RealHisto();
  StHbt1DHisto* MixedHisto();

private:   // here are the quantities I want to cut on...

  int mEventMult[2];      // range of multiplicity
  float mVertZPos[2];     // range of z-position of vertex

  long mNEventsPassed;
  long mNEventsFailed;

  StHbt1DHisto* mReals;  
  StHbt1DHisto* mMixed;

#ifdef __ROOT__
  ClassDef(ParityEventCut, 0)
#endif
};

inline void ParityEventCut::SetEventMult(const int& lo, const int& hi){mEventMult[0]=lo; mEventMult[1]=hi;}
inline void ParityEventCut::SetVertZPos(const float& lo, const float& hi){mVertZPos[0]=lo; mVertZPos[1]=hi;}

inline StHbt1DHisto* ParityEventCut::RealHisto(){return mReals;}
inline StHbt1DHisto* ParityEventCut::MixedHisto(){return mMixed;}


#endif
