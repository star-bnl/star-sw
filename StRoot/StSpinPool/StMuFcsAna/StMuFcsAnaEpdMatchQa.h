/*
  AUTHOR
  David Kapukchyan

  PURPOSE
  The purpose of this class is to do some QA tests on the EPD match algorithm coming from #StMuFcsAnaEpdMatch to find a usuable EPD cut to separate the charged and neutral particles.

  DESCRIPTION
  This analysis module inherits from #StMuFcsVirtualAna and adds histograms and code to check the hit distribution distance to all EPD tile distance. The idea is that when looking at the distance of the point to all EPD tiles vs. the distance to all EPD hits you see some correlation. Then you can add a MIP cut to see how things improve. It also makes histograms related to the nmip sum of adjacent intersecting tiles and nmip max of adjacent tiles. See #StMuFcsAnaEpdMatch::CheckInsideEpdTile() for more information.

  LOG
  @[June 3, 2026] > Copied from #StMuFcsAnaEpdFcsMixedEvent and modified to use new class name and implemented the analysis code
  @[June 4, 2026] > Got rid of the distance checks since that was not working and realized that comparing the hit tiles to all tiles is not much different than just doing the signed r and signed phi correlations, which already show promise from #StMuFcsAnaEpdFcsMixedEvent. Copied DrawEpdProjection() from old #StMuFcsTreeMaker to check if I am still getting the same results as before with new code base.
  @[June 5, 2026] > Copied the algorithm in #StMuFcsAnaEpdMatch::CheckInsideEpdTile() and implemented the nmip tile, nmip adjacency sum, and nmip adjacency max to see if these work better than just the "tile" nmip cut that #StMuFcsAnaEpdMatch uses. Implemented histograms to see if any correlation exists between such variables.
  @[June 8, 2026] > Moved the 'CheckInsideEpdTile()' algorithm to #StMuFcsAnaEpdMatch::CheckInsideEpdTile(). Moved 'mAllEpdNmip' to #StMuFcsAnaEpdMatch. Moved the histograms of bad EPD projections from #StMuFcsAnaMakePairs to here and created separate histograms for clusters and points.

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

  void setCanvas(TCanvas* canv, const char* savename=""){ mCanvas=canv; mCanvSaveName=savename; } ///< For drawing while in #DoMake() the distance histograms on the event level. Should be specified externally by user and so cleaned up by user too. When savename is empty will use an internally generated one

  //virtual void Print(Option_t* opt="") const; //"e" for event, "t" for trigger, "g" for photon, "p" for pi0, "a" for all

  void PaintPointEpdDistQa(TCanvas* canv, const char* savename = "test_pointepddistqa.png") const;
  void PaintPointEpdDistQaProj(TCanvas* canv, const char* savename = "test_pointepddistqaproj.png") const;
  void PaintPointEpdDist(TCanvas* canv, const char* savename = "test_pointepddistqa.png") const;
  void PaintProjEpdAdjQa(TCanvas* canv, const char* savename = "test_pointepdadjqa.png") const;
  void PaintBadProjections(TCanvas* canv, const char* savename = "test_badprojections.png") const;
  Int_t DrawEpdProjection(StMuFcsAnaData* anadata, TCanvas* canvas, const char* savename);       ///< For drawing event level QA of FCS clusters and points to EPD hits and tiles
  
protected:
  TH1* mH1F_ClusNBadEpdProj = 0;        ///< Number of clusters that did not have a valid projection to an EPD tile in a given event
  TH1* mH1F_ClusNBadEpdProjVcut = 0;    ///< Number of clusters that did not have a valid projection to an EPD tile in a given event with cut |vertex|<150
  
  TH1* mH1F_PointNBadEpdProj = 0;       ///< Number of points that did not have a valid projection to an EPD tile in a given event
  TH1* mH1F_PointNBadEpdProjVcut = 0;   ///< Number of points that did not have a valid projection to an EPD tile in a given event with cut |vertex|<150
  
  TH1* mH2F_PointProj_nmipVdrtile   = 0;     ///< signed r distance of projected point to all EPD tiles
  TH1* mH2F_PointProj_nmipVdrhit    = 0;     ///< signed r distance of projected point to all EPD hits
  TH1* mH2F_PointProj_nmipVdphitile = 0;     ///< signed phi distance of projected point to all EPD tiles
  TH1* mH2F_PointProj_nmipVdphihit  = 0;     ///< signed phi distance of projected point to all EPD hits
  
  //TH1* mH2F_PointProj_rtileVrpoint   = 0;     ///< signed r distance of projected point to all EPD tiles
  //TH1* mH2F_PointProj_rhitVrpoint    = 0;     ///< signed r distance of projected point to all EPD hits
  //TH1* mH2F_PointProj_phitileVphipoint = 0;     ///< signed phi distance of projected point to all EPD tiles
  //TH1* mH2F_PointProj_phitileVphipoint  = 0;     ///< signed phi distance of projected point to all EPD hits
  TH1* mH1F_PointProjDistToEpdTiles = 0;          ///< Euclidean distance between a projected point and all EPD tiles
  TH1* mH1F_PointProjDistToEpdHits = 0;           ///< Euclidean distance between a projected point and all EPD hits

  TH1* mH2F_ClusProjEpdAdj_maxVsum = 0;           ///< Grab the EPD tile that an FCS cluster projected to and check itself and its 8 adjacent tiles to find which one has the maximum nmip vs. the sum of the nmip of all 9 tiles (intersected tile + 8 adjacent tiles)
  TH1* mH2F_PointProjEpdAdj_maxVsum = 0;          ///< Grab the EPD tile that an FCS point projected to and check itself and its 8 adjacent tiles to find which one has the maximum nmip vs. the sum of the nmip of all 9 tiles (intersected tile + 8 adjacent tiles)
  TH1* mH2F_ClusProjEpdAdj_tileVsum = 0;          ///< Grab the EPD tile that an FCS cluster projected to and grab its nmip vs. the sum of the nmip of all 9 tiles (intersected tile + 8 adjacent tiles)
  TH1* mH2F_PointProjEpdAdj_tileVsum = 0;         ///< Grab the EPD tile that an FCS point projected to and grab its nmip vs. the sum of the nmip of all 9 tiles (intersected tile + 8 adjacent tiles)

  TH1* mH2F_ClusProjEpdAdj_maxVtile = 0;          ///< Grab the EPD tile that an FCS cluster projected to and check itself and its 8 adjacent tiles to find which one has the maximum nmip vs. the nmip of the found intersecting tile
  TH1* mH2F_PointProjEpdAdj_maxVtile = 0;         ///< Grab the EPD tile that an FCS point projected to and check itself and its 8 adjacent tiles to find which one has the maximum nmip vs. the nmip of the found intersecting tile
  
  TH1* mH1F_ClusProjEpdAdjRedMip = 0;             ///< Histogram of the REDuced nMIP (RedMIP) for a FCS cluster. RedMip is the maximum nmip (center+8 adjcacent) divided by sum of nmips (center+8 adjacent)
  TH1* mH1F_PointProjEpdAdjRedMip = 0;            ///< Histogram of the REDuced nMIP (RedMIP) for a FCS point. RedMip is the maximum nmip (center+8 adjcacent) divided by sum of nmips (center+8 adjacent)

  TH1* mH1F_ClusProjEpdAdjRedMax = 0;             ///< Histogram of the REDuced MAX nmip (RedMax) for a FCS cluster. RedMax is nmip of the intersected tile (#FcsPhotonCandidate::mEpdHitNmip[0]) divided by the the maximum nmip (center+8 adjcacent)
  TH1* mH1F_PointProjEpdAdjRedMax = 0;            ///< Histogram of the REDuced MAX nmip (RedMax) for a FCS point. RedMax is nmip of the intersected tile (#FcsPhotonCandidate::mEpdHitNmip[0]) divided by the the maximum nmip (center+8 adjcacent)

  TCanvas* mCanvas = 0;                           ///< Canvas for plotting some of the distance histograms since these need to be done on an event by event level. Needs to be provided externally
  TString mCanvSaveName = "test.png";             ///< The savename that #mCanvas will use

  std::map<Int_t,TPolyLine*> mEpdTileMap;         ///< EPD "tile key" to polyline for drawing EPD tiles in #DrawEpdProjection()
  
  ClassDef(StMuFcsAnaEpdMatchQa, 1)
};

#endif

