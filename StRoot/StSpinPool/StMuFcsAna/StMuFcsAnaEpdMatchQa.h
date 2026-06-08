/*
  AUTHOR
  David Kapukchyan

  PURPOSE
  The purpose of this class is to do some QA tests on the EPD match to find a usuable EPD cut to separate the charged and neutral particles.

  DESCRIPTION
  to the FCS point by comparing the distance from the projected FCS points to the EPD tiles vs. EPD hits. Also, 
  This analysis module inherits from #StMuFcsVirtualAna and adds histograms and code to check the hit distribution distance to all EPD tile distance. The idea is that when looking at the distance of the point to all EPD tiles vs. the distance to all EPD hits you see some correlation. Then you can add a MIP cut to see how things improve. Added another feature of the maximum nmip and the total sum of all adjacent tiles. Added a feature of the maximum adjacent nmip value compared to the tiles nmip value.

  LOG
  @[June 3, 2026] > Copied from #StMuFcsAnaEpdFcsMixedEvent and modified to use new class name and implemented the analysis code
  @[June 4, 2026] > Got rid of the distance checks since that was not working and realized that comparing the hit tiles to all tiles is not much different than just doing the signed r and signed phi correlations, which already show promise from #StMuFcsAnaEpdFcsMixedEvent. Copied DrawEpdProjection() from old #StMuFcsTreeMaker to check if I am still getting the same results as before with new code base.
  @[June 5, 2026] > Copied the algorithm in #StMuFcsAnaEpdMatch::CheckInsideEpdTile() and implemented the nmip tile, nmip adjacency sum, and nmip adjacency max to see if these work better than just the "tile" nmip cut that #StMuFcsAnaEpdMatch uses. Implemented histograms to see if any correlation exists between such variables.

*/


#ifndef STMUFCSANAEPDMATCHQA_HH
#define STMUFCSANAEPDMATCHQA_HH

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
#include "TPolyLine.h"
#include "TMarker.h"

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

class StMuFcsAnaEpdMatchQa : public StMuFcsVirtualAna {
public:
  
  StMuFcsAnaEpdMatchQa();
  ~StMuFcsAnaEpdMatchQa();

  virtual UInt_t LoadHists(TFile* file, HistManager* histman, StMuFcsAnaData* data);
  virtual Int_t DoMake(StMuFcsAnaData* mufcsdata);

  void setCanvas(TCanvas* canv, const char* savename=""){ mCanvas=canv; mCanvSaveName=savename; } ///< Should be specified externally by user so cleaned up by user too

  //virtual void Print(Option_t* opt="") const; //"e" for event, "t" for trigger, "g" for photon, "p" for pi0, "a" for all

  void PaintPointEpdDistQa(TCanvas* canv, const char* savename = "test_pointepddistqa.png") const;
  void PaintPointEpdDistQaProj(TCanvas* canv, const char* savename = "test_pointepddistqaproj.png") const;
  void PaintPointEpdDist(TCanvas* canv, const char* savename = "test_pointepddistqa.png") const;
  void PaintProjEpdAdjQa(TCanvas* canv, const char* savename = "test_pointepdadjqa.png") const;

  void CheckInsideEpdTile(StEpdGeom* epdgeo, FcsPhotonCandidate* photon, Double_t projx, Double_t projy );
  Int_t DrawEpdProjection(StMuFcsAnaData* anadata, TCanvas* canvas, const char* savename);
  
protected:
  TH1* mH2F_PointProj_nmipVdrtile   = 0;     ///< signed r distance of projected point to all EPD tiles
  TH1* mH2F_PointProj_nmipVdrhit    = 0;     ///< signed r distance of projected point to all EPD hits
  TH1* mH2F_PointProj_nmipVdphitile = 0;     ///< signed phi distance of projected point to all EPD tiles
  TH1* mH2F_PointProj_nmipVdphihit  = 0;     ///< signed phi distance of projected point to all EPD hits
  
  //TH1* mH2F_PointProj_rtileVrpoint   = 0;     ///< signed r distance of projected point to all EPD tiles
  //TH1* mH2F_PointProj_rhitVrpoint    = 0;     ///< signed r distance of projected point to all EPD hits
  //TH1* mH2F_PointProj_phitileVphipoint = 0;     ///< signed phi distance of projected point to all EPD tiles
  //TH1* mH2F_PointProj_phitileVphipoint  = 0;     ///< signed phi distance of projected point to all EPD hits
  TH1* mH1F_PointProjDistToEpdTiles = 0;
  TH1* mH1F_PointProjDistToEpdHits = 0;

  TH1* mH2F_ClusProjEpdAdj_maxVsum = 0;
  TH1* mH2F_PointProjEpdAdj_maxVsum = 0;

  TH1* mH2F_ClusProjEpdAdj_maxVtile = 0;
  TH1* mH2F_PointProjEpdAdj_maxVtile = 0;
  
  TH1* mH1F_ClusProjEpdAdjRedMip = 0;
  TH1* mH1F_PointProjEpdAdjRedMip = 0;

  TH1* mH1F_ClusProjEpdAdjRedMax = 0;
  TH1* mH1F_PointProjEpdAdjRedMax = 0;

  TCanvas* mCanvas = 0;   ///< Canvas for plotting built externally
  TString mCanvSaveName = "test.png";
  Int_t mEvent = 0;  ///< Move to #FcsEventInfo or #StMuFcsAnaData?

  float mAllEpdNmip[12][31];
  std::map<Int_t,TPolyLine*> mEpdCcwLines;
  std::map<Int_t,TPolyLine*> mEpdTileMap;    ///< EPD "tile key" to polyline for drawing
  
  ClassDef(StMuFcsAnaEpdMatchQa, 1)
};

#endif

