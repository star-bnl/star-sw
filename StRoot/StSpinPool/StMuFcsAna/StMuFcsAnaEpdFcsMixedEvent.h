/*
  AUTHOR
  David Kapukchyan

  PURPOSE
  The purpose of this class is to do the the mixed event testing to compare how well the EPD hits match to FCS points

  DESCRIPTION
  This analysis module inherits from #StMuFcsVirtualAna and adds histograms and code to do the mixed event analysis. The mixing happens when the current event EPD hits get compared to the FCS points from the previous event. This means first event is not mixed. It will utilize the matching in #StMuFcsEpdMatch for points from this event and the last event and use the projection method in #StMuFcsAnaData. It creates a separate #TClonesArray named #mMixedPhArr which is filled with points from the old event at the lower indicies and the current event at the higher indicies, which is easy to separate by using the value of #mNOldPoints. It also keeps track of the old vertex for the projection in #mOldVertex

  LOG
  @[December 23, 2025] > Copied from #StMuFcsPi0TreeMaker and modified to use new class name
  @[January 19, 2026] > Obsolete class as part of the experimentation with using #StMuFcsTreeMaker for doing the EPD mixed event analysis. Still haven't implemented into the new framework and want to keep this for keeping track of the history of how the analysis worked and as a learning tool for how I was experimenting to find a new framework
  @[May 13, 2026] > Copied old StMuFcsPointEpdMixedEventMaker into StMuFcsAnaEpdFcsMixedEvent which utilizes new #StMuFcsVirtualAna framework and methods

*/


#ifndef STMUFCSANAEPDFCSMIXEDEVENT_HH
#define STMUFCSANAEPDFCSMIXEDEVENT_HH

//C/C++ Headers
#include <iostream>

//ROOT Headers
#include "TString.h"
#include "TPolyLine.h"
#include "TEllipse.h"
#include "TFile.h"
#include "TTree.h"
#include "TLeaf.h"
#include "TH1F.h"
#include "TLegend.h"
#include "TF1.h"
#include "TGeoPolygon.h"

//STAR Headers
#include "StEnumerations.h"
#include "StMaker.h"
#include "StSpinPool/StSpinDbMaker/StSpinDbMaker.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuTriggerIdCollection.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StEvent/StTriggerData.h"
#include "StEvent/StTriggerId.h"
#include "StMessMgr.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"
#include "Stypes.h"
#include "StFcsDbMaker/StFcsDbMaker.h"
#include "StFcsDbMaker/StFcsDb.h"
#include "StMuDSTMaker/COMMON/StMuFcsCollection.h"
#include "StMuDSTMaker/COMMON/StMuFcsHit.h"
#include "StMuDSTMaker/COMMON/StMuFcsCluster.h"
#include "StMuDSTMaker/COMMON/StMuFcsPoint.h"

#include "StSpinPool/StFcsTreeManager/StMuFcsPi0Data.h"
#include "StMuFcsAnaEpdMatch.h"
//#include "StFcsRun22TriggerMap.h"

class StEpdGeom;

class StMuFcsAnaEpdFcsMixedEvent : public StMuFcsVirtualAna {
public:
  
  StMuFcsAnaEpdFcsMixedEvent();
  ~StMuFcsAnaEpdFcsMixedEvent();

  virtual UInt_t LoadHists(TFile* file, HistManager* histman, StMuFcsAnaData* data);
  virtual Int_t DoMake(StMuFcsAnaData* mufcsdata);

  //virtual void Print(Option_t* opt="") const; //"e" for event, "t" for trigger, "g" for photon, "p" for pi0, "a" for all

  void PaintEpdAllDistQa(TCanvas* canv, const char* savename = "testepdalldistqa.png") const;
  void PaintEpdAllDistQaLowMult(TCanvas* canv, const char* savename = "testepdalldistqalowmult.png" ) const;
  void PaintEpdTileDistQa(TCanvas* canv, const char* savename = "testepdtiledistqa.png") const;
  void PaintEpdDistAnaQa(TCanvas* canv, const char* savename = "testepddistanaqa.png") const;
  
protected:
  TClonesArray* mMixedPhArr = 0;        ///< #TClonesArray of #FcsPhotonCandidate from last event used for event mixing

  TH1* mH2F_PointProj_nmipValldx=0;        ///< nMIP vs. FCS projected point to EPD x-position minus EPD x-position of all hits
  TH1* mH2F_PointProj_nmipValldy=0;        ///< nMIP vs. FCS projected point to EPD y-position minus EPD y-position of all hits
  TH1* mH2F_PointProj_nmipValldr=0;        ///< nMIP vs. FCS projected point to EPD, r difference to all other hits
  TH1* mH2F_PointProj_nmipValldphi=0;      ///< nMIP vs. FCS projected point to EPD, angle difference between all other hits
  TH1* mH2F_MixedPointProj_nmipValldr=0;        ///< Mixed event nMIP vs. FCS projected point to EPD, r difference to all other hits
  TH1* mH2F_MixedPointProj_nmipValldphi=0;      ///< Mixed event nMIP vs. FCS projected point to EPD, angle difference between all other hits
  TH1* mH2F_PointProj_nmipVtiledx=0;       ///< nMIP vs. FCS projected point to EPD x-position minus EPD x-position of center tile
  TH1* mH2F_PointProj_nmipVtiledy=0;       ///< nMIP vs. FCS projected point to EPD y-position minus EPD y-position of center tile
  TH1* mH2F_PointProj_nmipVtiledr=0;       ///< nMIP vs. FCS projected point to EPD, r difference to tile hit
  TH1* mH2F_PointProj_nmipVtiledphi=0;     ///< nMIP vs. FCS projected point to EPD, angle difference to tile hit
  TH1* mH2F_MixedPointProj_nmipVtiledr=0;       ///< Mixed event nMIP vs. FCS projected point to EPD, r difference to tile hit
  TH1* mH2F_MixedPointProj_nmipVtiledphi=0;     ///< Mixed event nMIP vs. FCS projected point to EPD, angle difference to tile hit

  TH1* mH2F_PointProj_LowMult_nmipValldr=0;          ///< nMIP vs. FCS projected point to EPD, r difference to tile hit
  TH1* mH2F_PointProj_LowMult_nmipValldphi=0;        ///< nMIP vs. FCS projected point to EPD, angle difference to tile hit
  TH1* mH2F_MixedPointProj_LowMult_nmipValldr=0;     ///< Mixed event nMIP vs. FCS projected point to EPD, r difference to tile hit
  TH1* mH2F_MixedPointProj_LowMult_nmipValldphi=0;   ///< Mixed event nMIP vs. FCS projected point to EPD, angle difference to tile hit

private:
  Double_t mOldVertex = -999.0;                ///< Vertex from last event needed for event mixing
  Int_t mNOldPoints = 0;                       ///< Number of points in previous event
  
  ClassDef(StMuFcsAnaEpdFcsMixedEvent, 1)
};

#endif

