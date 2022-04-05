//////////////////////////////////////////////////////////////////////
//
// Revision 1.0  2000/12/19 yepes
// First Version of StPeCParticle 
//
//////////////////////////////////////////////////////////////////////
//
// StPeCParticle
//
// Mc Particle class for Peripheral Collisions
//
//////////////////////////////////////////////////////////////////////
#ifndef StPeCParticle_h
#define StPeCParticle_h
#include "Rtypes.h"
#include "TObject.h"
#include "StMuDSTMaker/COMMON/StMuMcTrack.h"
class g2t_track_st ;

class StPeCParticle : public TObject {

public:

                                  StPeCParticle();
  virtual                         ~StPeCParticle();

#ifndef __CINT__
                                  StPeCParticle ( g2t_track_st *trk);
  void                            set        ( g2t_track_st* trk);
                                  StPeCParticle ( StMuMcTrack * trk);
  void                            set           ( StMuMcTrack * trk);
#endif /*__CINT__*/
  Int_t                           key ;
  Short_t                         pid ;
  Short_t                         charge ;
  Int_t                           vertexId ;
  Float_t                         pt ;
  Float_t                         eta ;
  Float_t                         psi ; 
  Float_t                         e   ;
  Float_t                         nTpcHits ;
  Float_t                         nFtpHits ;
  Float_t                         nSvtHits ;

  ClassDef(StPeCParticle,1)
};

#endif





