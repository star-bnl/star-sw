//////////////////////////////////////////////////////////////////////
//
// $Id: StppEvent.h,v 1.1 2002/01/16 20:22:53 akio Exp $ 
// $Log: StppEvent.h,v $
// Revision 1.1  2002/01/16 20:22:53  akio
// First version
//
//
// Revision 1.0  2001/06/14 Akio Ogawa
// First Version of StppEvent 
//
//////////////////////////////////////////////////////////////////////
//
// StppEvent
//
// Event class for Spin pp uDst
//
//////////////////////////////////////////////////////////////////////
#ifndef StppEvent_h
#define StppEvent_h

#define _Offline_tracks_
#define _L3_tracks_

#include "TObject.h"
#include "TClonesArray.h"
class StppTrack;
class StEvent;

class StppEvent : public TObject {
 public:  
  StppEvent();
  virtual ~StppEvent();
  
#ifndef __CINT__
  Int_t fill(StEvent* event);
#endif /*__CINT__*/
  void clear();
  void reset();
  
  
  Int_t        runN;
  Int_t        eventN;
  Int_t        token;
  Int_t        triggerWord;
  Long_t       time;
  Int_t        bunchId;
  Int_t        bunchId7bit;
  Int_t        doubleSpinIndex;

#ifdef _Offline_tracks_
  TClonesArray *pTracks;
  Int_t        nPrimTrack;
  Int_t        nGoodTrack;
  Float_t      xVertex;
  Float_t      yVertex;
  Float_t      zVertex;  
  Int_t        LCP;
  Float_t      sumPt;
  Float_t      vectorSumPt;
  Float_t      weightedEta;
  Float_t      weightedPhi;
#endif

#ifdef _L3_tracks_
  TClonesArray *pTracksL3;
  Int_t        nPrimTrackL3;
  Int_t        nGoodTrackL3;
  Float_t      xVertexL3;
  Float_t      yVertexL3;
  Float_t      zVertexL3;
  Int_t        LCPL3;
  Float_t      sumPtL3;
  Float_t      vectorSumPtL3;
  Float_t      weightedEtaL3;
  Float_t      weightedPhiL3;
#endif

  Int_t        bbcAdcSum;   
  Int_t        bbcNHit;   
  Float_t      zVertexBbc;

  Int_t        fpdAdcSumNorth;   
  Int_t        fpdAdcSumSouth;   
  Int_t        fpdAdcSumTop;   
  Int_t        fpdAdcSumBottom;   
  Int_t        fpdAdcSumPres1;   
  Int_t        fpdAdcSumPres2;   
  Int_t        fpdAdcSumSmdX;   
  Int_t        fpdAdcSumSmdY;   
  Int_t        fpdSouthVeto;   

  Float_t      ctbAdcSum;   
  Int_t        ctbNHit;   

  Int_t        svtNHit;   
  Float_t      emcHighTower;   
  
  void setInfoLevel(int level) {infoLevel = level;};  
  
 private:
  Int_t infoLevel;//!  

  ClassDef(StppEvent,1)
};

#endif
