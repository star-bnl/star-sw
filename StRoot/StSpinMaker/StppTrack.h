//////////////////////////////////////////////////////////////////////
//
// $Id: StppTrack.h,v 1.1 2002/01/16 20:22:54 akio Exp $
// $Log: StppTrack.h,v $
// Revision 1.1  2002/01/16 20:22:54  akio
// First version
//
//
// Revision 1.0  2001/06/14 Akio Ogawa
// First Version of StppTrack 
//
//////////////////////////////////////////////////////////////////////
//
// StppTrack
//
// Light weighted Track class for Spin pp uDst
//
//////////////////////////////////////////////////////////////////////
#ifndef StppTrack_h
#define StppTrack_h

#include "TObject.h"
class StTrack;

class StppTrack:public TObject{
 public:
  StppTrack();
  virtual ~StppTrack();
  
#ifndef __CINT__
  StppTrack(StTrack *trk);
  void     fill(StTrack *trk);
  StTrack* getStTrack();
#endif /* __CINT__ */
  
  Int_t    flag;
  Int_t    key ;
  Int_t    charge;
  Float_t  pt;
  Float_t  p;
  Float_t  eta;
  Float_t  psi; 
  Float_t  phi0;
  Float_t  r0;
  Float_t  z0;
  Int_t    nHits;
  Int_t    nHitsMax;
  Float_t  chi2;
  Float_t  vertexDCAx;
  Float_t  vertexDCAy;
  Float_t  vertexDCAz;
  Float_t  dedx;
  
#ifndef __CINT__
  Float_t  getZdEdx(Float_t mass);
#endif /* __CINT__ */
  
  //Float_t emcEnergy;
  //Float_t smdWidth;
    
  ClassDef(StppTrack,1)

};
#endif
