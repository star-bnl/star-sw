//////////////////////////////////////////////////////////////////////
//
// $Id: StEtGridFlat.h,v 1.2 2003/09/07 03:49:05 perev Exp $
// $Log: StEtGridFlat.h,v $
// Revision 1.2  2003/09/07 03:49:05  perev
// gcc 3.2 + WarnOff
//
// Revision 1.1  2002/02/11 20:30:48  akio
// Many updates, including very first version of jet finder.
//
//
// Revision 1.0  2002/01/25 Akio Ogawa
// First Version of StEtGridFlat 
//
//////////////////////////////////////////////////////////////////////
//
// StEtGrid
//
// Event class for a EtGridFlat
//
//////////////////////////////////////////////////////////////////////
#ifndef StEtGridFlat_h
#define StEtGridFlat_h

#include "TObject.h"
#include "math.h"
#include "StEtGrid.h"

class StEtGridFlat : public StEtGrid{

public:
  StEtGridFlat() {};
  virtual ~StEtGridFlat() {};
  void createKeys(int neta/*=56*/, int nphi=120, 
		  float eta1=-1.4, float eta2=1.4, 
		  float phi1=-M_PI, float phi2=M_PI);
  void createKeys(){createKeys(56);}
  int findKey(float, float);

private:
  int mNEta;
  int mNPhi;
  float mEtaMin;
  float mEtaMax;
  float mPhiMin;
  float mPhiMax;

  ClassDef(StEtGridFlat,1)
};

#endif
