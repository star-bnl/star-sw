//////////////////////////////////////////////////////////////////////
//
// $Id: StppTrack.h,v 1.3 2003/09/07 03:49:05 perev Exp $
// $Log: StppTrack.h,v $
// Revision 1.3  2003/09/07 03:49:05  perev
// gcc 3.2 + WarnOff
//
// Revision 1.2  2002/12/04 20:28:09  thenry
// StppuDstMaker was modified to allow multiple jet analysis modules to be
// run simultaneosly with various parameters while the Maker loads the events
// and analyses them.  Four different jet analyzers exist:
//
// Konstanin's Analyzers:
//     Kt type: StppKonstKtJetAnalyzer
//     Cone type: StppKonstConeJetAnalyzer
//
// Mike's Analyzers:
//     Kt type: StppMikeKtJetAnalyzer
//     Cone type: StppMikeConeJetAnalyzer
//
// These modules all require the StJetFinder modules.
//
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

//VP #define MAXANALYZERS 4 //enum instead

class StTrack;

class StppTrack:public TObject{
enum {MAXANALYZERS=4};
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

    //key into the jet arrays
    Int_t jetIndex;

    //Int_t coneJetIndex;
    //Int_t clusterJetIndex;
    Int_t anaJetIndex[MAXANALYZERS];
  
#ifndef __CINT__
    Float_t  getZdEdx(Float_t mass);
#endif /* __CINT__ */
    
    //Float_t emcEnergy;
    //Float_t smdWidth;
    
    ClassDef(StppTrack,1)

	};
#endif
