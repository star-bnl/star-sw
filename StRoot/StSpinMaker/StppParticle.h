//////////////////////////////////////////////////////////////////////
//
// $Id: StppParticle.h,v 1.1 2002/01/16 20:22:53 akio Exp $ 
// $Log: StppParticle.h,v $
// Revision 1.1  2002/01/16 20:22:53  akio
// First version
//
//
// Revision 1.0  2001/06/14 Akio Ogawa
// First Version of StppParticle
//
//////////////////////////////////////////////////////////////////////
//
// StppParticle
//
// geant track class for Spin pp uDst
//
//////////////////////////////////////////////////////////////////////
#ifndef StppParticle_h
#define StppParticle_h

#include "TObject.h"
class g2t_track_st ;

class StppParticle : public TObject {
 public:
          StppParticle();
  virtual ~StppParticle();

#ifndef __CINT__
          StppParticle (g2t_track_st *trk);
  void    set(g2t_track_st* trk);
#endif /*__CINT__*/

  Int_t    key ;
  Short_t  pid ;
  Short_t  charge ;
  Float_t  pt ;
  Float_t  eta ;
  Float_t  psi ; 
  Float_t  e   ;
  Float_t  nTpcHits ;
  Float_t  nFtpHits ;
  Float_t  nSvtHits ;

  ClassDef(StppParticle,1)
};

#endif
