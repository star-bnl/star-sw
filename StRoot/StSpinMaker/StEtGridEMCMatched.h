//////////////////////////////////////////////////////////////////////
//
// $Id: StEtGridEMCMatched.h,v 1.1 2002/02/11 20:30:48 akio Exp $
// $Log: StEtGridEMCMatched.h,v $
// Revision 1.1  2002/02/11 20:30:48  akio
// Many updates, including very first version of jet finder.
//
//
// Revision 1.0  2002/01/25 Akio Ogawa
// First Version of StEtGridEMCMatched 
//
//////////////////////////////////////////////////////////////////////
//
// StEtGridEMCMatched
//
// Event class for a StEtGridEMCMatched
//
//////////////////////////////////////////////////////////////////////
#ifndef StEtGridEMCMatched_h
#define StEtGridEMCMatched_h

#include "TObject.h"
#include "math.h"
#include "StEtGrid.h"

class StEtGridEMCMatched : public StEtGrid{

public:
  StEtGridEMCMatched(){};
  virtual ~StEtGridEMCMatched(){};
  void createKeys();
  int findKey(float, float);

private:
  int mNEta;
  int mNPhi;
  float mEtaMin;
  float mEtaMax;
  float mPhiMin;
  float mPhiMax;
  int mNEtaE;
  int mNPhiE;
  float mEtaMinE;
  float mEtaMaxE;
  float mPhiMinE;
  float mPhiMaxE;

  ClassDef(StEtGridEMCMatched,1)
};

#endif
