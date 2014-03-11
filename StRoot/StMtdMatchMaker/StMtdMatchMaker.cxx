/*******************************************************************
 * $Id: StMtdMatchMaker.cxx,v 1.10 2014/03/11 22:18:11 geurts Exp $
 * Author: Bingchu Huang
 *****************************************************************
 *
 * Description: BTof Match Maker to do the matching between the 
 *              fired celles and TPC tracks
 *
 *****************************************************************
 *
 * $Log: StMtdMatchMaker.cxx,v $
 * Revision 1.10  2014/03/11 22:18:11  geurts
 * corrected pvtx retrieval in StEvent environment
 *
 * Revision 1.9  2014/03/11 02:15:53  geurts
 * Protect against potentially non-existing primary vertex in StEvent [Bingchu]
 *
 * Revision 1.8  2013/12/09 22:53:25  geurts
 * update: enable filling of MTD Pid traits and include a few more protections against zero-pointers [Bingchu]
 *
 * Revision 1.7  2013/11/25 16:10:43  geurts
 * Remove AddHist for uninitialized histogram [Jason Webb]
 *
 * Revision 1.6  2013/11/19 22:30:30  jeromel
 * Added name
 *
 * Revision 1.5  2013/11/19 00:17:16  geurts
 * include protection against zero triggerIdCollection() pointers. This is relevant when using simulated data as input.
 *
 * Revision 1.4  2013/04/25 14:52:13  geurts
 * Minor adjustments that address SL44 compiler warnings
 *
 * Revision 1.3  2013/04/18 21:01:10  geurts
 * Bugfix (RT#2575): protection against events that have tracks, but no vertex.
 *  - Warning messages
 *  - Consistently reset variables that depend on a vertex to -9999.
 *
 *
 *
 *******************************************************************/
#include <iostream>//.h>
#include <fstream>//.h>
#include <stdlib.h>
#include <string>
#include <vector>
#include <math.h>
#include <cmath>

#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TChain.h"
#include "TSystem.h"
#include "TTree.h"
#include "TBranch.h"

//Random generator
//#include "Random.h"
//#include "RanluxEngine.h"
//#include "RandFlat.h"
//#include "RandGauss.h"

#include "StEvent.h"
#include "StTrackNode.h"
#include "StContainers.h"
#include "StPrimaryVertex.h"
#include "StVertex.h"
#include "StMeasuredPoint.h"
#include "StDedxPidTraits.h"
#include "StTrackPidTraits.h"
#include "StTrackGeometry.h"
#include "StTrackDetectorInfo.h"
//#include "StMuMtdCollection.h"
#include "StGlobalTrack.h"
#include "StParticleTypes.hh"
#include "StThreeVector.hh"
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include "StLorentzVectorD.hh"
#include "StGlobals.hh"                 // for PR()
#include "StGetConfigValue.hh"
#include "StTimer.hh"
#include "StMemoryInfo.hh"
#include "StMessMgr.h"
#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "StEventTypes.h"
#include "StTpcDedxPidAlgorithm.h"
#include "StParticleDefinition.hh"
#include "StPhysicalHelix.hh"

#include "PhysicalConstants.h"
#include "SystemOfUnits.h"   // has "tesla" in it

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuMtdCollection.h"
#include "StMuDSTMaker/COMMON/StMuMtdRawHit.h"
#include "StMuDSTMaker/COMMON/StMuMtdHeader.h"
#include "StMuDSTMaker/COMMON/StMuMtdHit.h"
#include "StMuDSTMaker/COMMON/StMuBTofHit.h"
#include "StMuDSTMaker/COMMON/StMuBTofPidTraits.h"
#include "StMuDSTMaker/COMMON/StMuMtdPidTraits.h"

#include "StEventMaker/StEventMaker.h"
#include "StAssociationMaker/StAssociationMaker.h"
#include "StMcEventMaker/StMcEventMaker.h"
#include "tables/St_vertexSeed_Table.h" //

#include "StBTofPidTraits.h"
#include "StBTofCollection.h"
#include "StBTofHit.h"
#include "StBTofRawHit.h"
#include "StBTofHeader.h"
#include "StBTofUtil/tofPathLength.hh"
#include "StBTofUtil/StBTofGeometry.h"
#include "StBTofUtil/StBTofDaqMap.h"
#include "StBTofUtil/StBTofHitCollection.h"
#include "StarGenerator/StarLight/starlightconstants.h"
#include "StMtdPidTraits.h"

#include "StMtdUtil/StMtdGeometry.h"
#include "StMtdMatchMaker.h"
//!extern TSystem* gSystem;
using namespace std;


/// Default constructor: set default values
StMtdMatchMaker::StMtdMatchMaker(const Char_t *name): StMaker(name)
{
   doPrintMemoryInfo = kFALSE;
   doPrintCpuInfo    = kFALSE;
   mMinFitPointsPerTrack=15;
   mMindEdxFitPoints=10;
	mMinEta=-0.8;
	mMaxEta=0.8;
   mMinPt = 1.0;
   mMinFitPointsOverMax=0.52;
   mCosmicFlag=kFALSE;
   
   mnNeighbors = 2;
   //mZLocalCut = 43.5;
   mNSigReso = 3.; // n sigma of z and y resolution.
   mSaveTree = kTRUE;
   mHisto = kTRUE;

   fZReso = new TF1("fZReso","sqrt([0]/x/x+[1])",0,100);
   fZReso->SetParameters(148.7,1.654); //cm
   fPhiReso = new TF1("fPhiReso","sqrt([0]/x/x+[1])",0,100);
   fPhiReso->SetParameters(9.514e-4,7.458e-6); //rad
   
   return;
}

StMtdMatchMaker::~StMtdMatchMaker(){
	return;
}


void StMtdMatchMaker::Clear(const char*){
	StMaker::Clear();
}

/// Init: initialize Vdrift and book histograms
Int_t StMtdMatchMaker::Init(){

	for(int i=0;i<mNAllTrays;i++){
		for(int j=0;j<mNStrips;j++){
			mVDrift[i][j] = 60.;
		}
	}
	if(mHisto) bookHistograms();
	
	if(mSaveTree) bookTree();
	
	return StMaker::Init();
}

/// Book various QA histograms, use StMaker::GetHistList->Write() to store.
void StMtdMatchMaker::bookHistograms(){

	mEventCounterHisto = new TH1D("eventCounter","eventCounter",20,0,20);
	AddHist(mEventCounterHisto);

	mCellsMultInEvent = new TH1D("cellsPerEvent","cellsPerEvent",1000,0,1000);
	AddHist(mCellsMultInEvent);

	mHitsMultInEvent  = new TH1D("hitsPerEvent","hitsPerEvent",1000,0,1000);
	AddHist(mHitsMultInEvent);

	mHitsPrimaryInEvent  = new TH1D("hitsPrimaryPerEvent","hitsPrimaryPerEvent",1000,0,1000);
	AddHist(mHitsPrimaryInEvent);   

	mHitsMultPerTrack = new TH1D("hitsPerTrack","hitsPerTrack",10,0,10);
	AddHist(mHitsMultPerTrack);

	mHitsPosition     = new TH2D("hitsPosition","hitsPositions",1000,-500.,500.,1000,-500.,500);
	AddHist(mHitsPosition);

	// histogram not created, memory location invalid
	//	AddHist(mHitsGlobalInEvent);    



	// occupancy
	for(int i=0;i<mNBacklegs;i++) {
		char hisname[100];
		sprintf(hisname,"Occupancy_Backleg_%d",i+1);
		mDaqOccupancy[i]     = new TH1D(hisname,";fired cell(module*12+stripId)",60,0,60);
		sprintf(hisname,"OccupancyProj_Backleg_%d",i+1);
		mDaqOccupancyProj[i] = new TH1D(hisname,";projected cell",60,0,60);

		AddHist(mDaqOccupancy[i]);
		AddHist(mDaqOccupancyProj[i]);
	}

	// correlation
	for(int i=0;i<mNBacklegs;i++) {
		char hisname[100];
		sprintf(hisname,"Corr_Backleg_%d",i+1);
		mHitCorr[i] = new TH2D(hisname,";project stripId;fired stripId",60,0,60,60,0,60);
		sprintf(hisname,"Corr_Backleg_%d_module",i+1);
		mHitCorrModule[i] = new TH2D(hisname,";project moduleId;fired moduleId",6,0,6,6,0,6);
		AddHist(mHitCorr[i]);
		AddHist(mHitCorrModule[i]);
	}

	// project hit position
	for(int i=0;i<mNBacklegs;i++) {
		char hisname[100];
		sprintf(hisname,"LocalYZ_Backleg_%d",i+1);
		mDeltaHitFinal[i] = new TH2D(hisname,";localY;localZ",300,-15.,15.,320,-80.,80);
		AddHist(mDeltaHitFinal[i]);
	}

	mTrackPtEta = new TH2D("trackPtEta",";p_{T} (GeV/c);#eta",500,0.,10.,60,-1.5,1.5);
	mTrackPtPhi = new TH2D("trackPtPhi",";p_{T} (GeV/c);#phi",500,0.,10.,120,0.,2.*M_PI);
	mTrackNFitPts = new TH1D("trackNFitPts",";nHitsFit",50,0.,50.);
	mTrackdEdxvsp = new TH2D("trackdEdxvsp",";p_{T} (GeV/c);dE/dx",500,0.,10.,1000,0.,10.);
	mNSigmaPivsPt = new TH2D("nSigmaPivsPt",";p_{T} (GeV/c);n#sigma_{#pi}",500,0.,10.,1000,-10.,10.);

	/// association  
	mCellsPerEventMatch1 = new TH1D("cellsPerEventMatch1","cellPerEventMatch1;# hits matched with track(s) per event",10,0,10);
	mHitsPerEventMatch1 = new TH1D("hitsPerEventMatch1","hitsPerEventMatch1;# tracks matched with hits per event",10,0,10);
	mCellsPerTrackMatch1 = new TH1D("cellsPerTrackMatch1","cellsPerTrackMatch1;# tracks matched with same hit",10,0,10);
	mTracksPerCellMatch1 = new TH1D("tracksPerCellMatch1","tracksPerCellMatch1;# hits matched with same track",10,0,10);
	mDaqOccupancyMatch1 = new TH1D("daqPerCellMatch1","daqPerCellMatch1;stripId",60,0,60);
	mDeltaHitMatch1 = new TH2D("deltaHitMatch1","deltaHitMatch1;localY;localZ",300,-15,15,800,-80.,80);

	/// kick out multi-hit
	mCellsPerEventMatch2 = new TH1D("cellsPerEventMatch2","cellPerEventMatch2;# hits matched with track(s) per event",10,0,10);
	mHitsPerEventMatch2 = new TH1D("hitsPerEventMatch2","hitsPerEventMatch2;# tracks matched with hits per event",10,0,10);
	mCellsPerTrackMatch2 = new TH1D("cellsPerTrackMatch2","cellsPerTrackMatch2;# tracks matched with same hit",10,0,10);
	mTracksPerCellMatch2 = new TH1D("tracksPerCellMatch2","tracksPerCellMatch2;# hits matched with same track",10,0,10);
	mDaqOccupancyMatch2 = new TH1D("daqPerCellMatch2","daqPerCellMatch2;stripId",60,0,60);
	mDeltaHitMatch2 = new TH2D("deltaHitMatch2","deltaHitMatch2;localY;localZ",300,-15,15,800,-80.,80);

	/// sort out multi matched cells
	mCellsPerEventMatch3 = new TH1D("cellsPerEventMatch3","cellPerEventMatch3;# hits matched with track(s) per event",10,0,10);
	mHitsPerEventMatch3 = new TH1D("hitsPerEventMatch3","hitsPerEventMatch3;# tracks matched with hits per event",10,0,10);
	mCellsPerTrackMatch3 = new TH1D("cellsPerTrackMatch3","cellsPerTrackMatch3;# tracks matched with same hit",10,0,10);
	mTracksPerCellMatch3 = new TH1D("tracksPerCellMatch3","tracksPerCellMatch3;# hits matched with same track",10,0,10);
	mDaqOccupancyMatch3 = new TH1D("daqPerCellMatch3","daqPerCellMatch3;stripId",60,0,60);
	mDeltaHitMatch3 = new TH2D("deltaHitMatch3","deltaHitMatch3;localY;localZ",300,-15,15,800,-80.,80);

	mCellsPrimaryPerEventMatch3 = new TH1D("cellsPrimaryPerEventMatch3","cellsPrimaryPerEventMatch3",10,0,10);

	hphivsz =new TH2F("hphivsz","hphivsz",500,0,TMath::Pi()*2,500,-500,500);
	hTofPhivsProj=new TH2F("hTofPhivsProj","hTofPhivsProj",100,0,TMath::Pi()*2,100,0,TMath::Pi()*2);
	hTofZvsProj=new TH2F("hTofZvsProj","hTofzvsProj",600,-300,300,600,-300,300);
	hMtdPhivsProj=new TH2F("hMtdPhivsProj","hMtdPhivsProj;projected #phi; fired #phi",360,-TMath::Pi(),TMath::Pi(),180,0,2.*TMath::Pi());
	hMtddPhivsBackleg=new TH2F("hMtddPhivsBackleg","hMtddPhivsBackleg;backleg; d#phi",30,0,30,1000,-2.*TMath::Pi(),2.*TMath::Pi());
	hMtddZvsBackleg =new TH2F("hMtddZvsBackleg","hMtddZvsBackleg;backleg; dz",30,0,30,400,-200,200);
	hMtdZvsProj=new TH2F("hMtdZvsProj","hMtdzvsProj;projected z(cm); fired z(cm)",600,-300,300,600,-300,300);

	AddHist(mTrackPtEta);
	AddHist(mTrackPtPhi);
	AddHist(mTrackNFitPts);
	AddHist(mTrackdEdxvsp);
	AddHist(mNSigmaPivsPt);

	AddHist(mCellsPerEventMatch1);
	AddHist(mHitsPerEventMatch1);
	AddHist(mCellsPerTrackMatch1);
	AddHist(mTracksPerCellMatch1);
	AddHist(mDaqOccupancyMatch1);
	AddHist(mDeltaHitMatch1);

	AddHist(mCellsPerEventMatch2);
	AddHist(mHitsPerEventMatch2);
	AddHist(mCellsPerTrackMatch2);
	AddHist(mTracksPerCellMatch2);
	AddHist(mDaqOccupancyMatch2);
	AddHist(mDeltaHitMatch2);

	AddHist(mCellsPerEventMatch3);
	AddHist(mHitsPerEventMatch3);
	AddHist(mCellsPerTrackMatch3);
	AddHist(mTracksPerCellMatch3);
	AddHist(mDaqOccupancyMatch3);
	AddHist(mDeltaHitMatch3);

	AddHist(mCellsPrimaryPerEventMatch3);

	AddHist(hphivsz);
	AddHist(hTofPhivsProj);
	AddHist(hTofZvsProj);
	AddHist(hMtdZvsProj);
	AddHist(hMtdPhivsProj);
	AddHist(hMtddPhivsBackleg);
	AddHist(hMtddZvsBackleg);

	return;
}

void StMtdMatchMaker::bookTree(){

		mMtdEvt = new TTree("mtdEvent","mtdEvent");
		mMtdEvt->SetAutoSave(1000000);
		mMtdEvt->Branch("run",&mMtdEvtData.run,"run/I");
		mMtdEvt->Branch("evt",&mMtdEvtData.evt,"evt/I");
		mMtdEvt->Branch("nTrgIds",&mMtdEvtData.nTrgIds,"nTrgIds/I");
		mMtdEvt->Branch("trgId",&mMtdEvtData.trgId,"trgId[nTrgIds]/I");
		mMtdEvt->Branch("bField",&mMtdEvtData.bField,"bField/F");
		mMtdEvt->Branch("vertexX",&mMtdEvtData.vertexX,"vertexX/F");
		mMtdEvt->Branch("vertexY",&mMtdEvtData.vertexY,"vertexY/F");
		mMtdEvt->Branch("vertexZ",&mMtdEvtData.vertexZ,"vertexZ/F");
		mMtdEvt->Branch("nMtdRawHits",&mMtdEvtData.nMtdRawHits,"nMtdRawHits/I");
		mMtdEvt->Branch("nMtdHits",&mMtdEvtData.nMtdHits,"nMtdHits/I");
		mMtdEvt->Branch("triggerTime",&mMtdEvtData.triggerTime,"triggerTime[2]/D");
		mMtdEvt->Branch("vpdVz",&mMtdEvtData.vpdVz,"vpdVz/F");
		mMtdEvt->Branch("tStart",&mMtdEvtData.tStart,"tStart/F");

		/// raw hits
		mMtdEvt->Branch("flag",&mMtdEvtData.flag,"flag[nMtdRawHits]/B");
		mMtdEvt->Branch("backlegRaw",&mMtdEvtData.backlegRaw,"backlegRaw[nMtdRawHits]/b");
		mMtdEvt->Branch("chn",&mMtdEvtData.chn,"chn[nMtdRawHits]/b");
		mMtdEvt->Branch("tdc",&mMtdEvtData.tdc,"tdc[nMtdRawHits]/D");

		/// sorted hits
		mMtdEvt->Branch("backleg",&mMtdEvtData.backleg,"backleg[nMtdHits]/b");
		mMtdEvt->Branch("module",&mMtdEvtData.module,"module[nMtdHits]/b");
		mMtdEvt->Branch("cell",&mMtdEvtData.cell,"cell[nMtdHits]/b");

		mMtdEvt->Branch("leTimeWest",&mMtdEvtData.leTimeWest,"leTimeWest[nMtdHits]/D");
		mMtdEvt->Branch("leTimeEast",&mMtdEvtData.leTimeEast,"leTimeEast[nMtdHits]/D");
		mMtdEvt->Branch("totWest",&mMtdEvtData.totWest,"totWest[nMtdHits]/D");
		mMtdEvt->Branch("totEast",&mMtdEvtData.totEast,"totEast[nMtdHits]/D");

		/// global tracks
		mMtdEvt->Branch("ngTracks",&mMtdEvtData.ngTracks,"ngTracks/I");
		mMtdEvt->Branch("gpt",&mMtdEvtData.gpt,"gpt[ngTracks]/F");
		mMtdEvt->Branch("geta",&mMtdEvtData.geta,"geta[ngTracks]/F");
		mMtdEvt->Branch("gphi",&mMtdEvtData.gphi,"gphi[ngTracks]/F");
		mMtdEvt->Branch("ppt",&mMtdEvtData.ppt,"ppt[ngTracks]/F");
		mMtdEvt->Branch("peta",&mMtdEvtData.peta,"peta[ngTracks]/F");
		mMtdEvt->Branch("pphi",&mMtdEvtData.pphi,"pphi[ngTracks]/F");

		mMtdEvt->Branch("ghelixpx",&mMtdEvtData.ghelixpx,"ghelixpx[ngTracks]/F");
		mMtdEvt->Branch("ghelixpy",&mMtdEvtData.ghelixpy,"ghelixpy[ngTracks]/F");
		mMtdEvt->Branch("ghelixpz",&mMtdEvtData.ghelixpz,"ghelixpz[ngTracks]/F");
		mMtdEvt->Branch("ghelixox",&mMtdEvtData.ghelixox,"ghelixox[ngTracks]/F");
		mMtdEvt->Branch("ghelixoy",&mMtdEvtData.ghelixoy,"ghelixoy[ngTracks]/F");
		mMtdEvt->Branch("ghelixoz",&mMtdEvtData.ghelixoz,"ghelixoz[ngTracks]/F");

		mMtdEvt->Branch("gdedx",&mMtdEvtData.gdedx,"gdedx[ngTracks]/F");
		mMtdEvt->Branch("gnSigmaPi",&mMtdEvtData.gnSigmaPi,"gnSigmaPi[ngTracks]/F");
		mMtdEvt->Branch("gnSigmaK",&mMtdEvtData.gnSigmaK,"gnSigmaK[ngTracks]/F");
		mMtdEvt->Branch("gnSigmaP",&mMtdEvtData.gnSigmaP,"gnSigmaP[ngTracks]/F");
		mMtdEvt->Branch("gnSigmaE",&mMtdEvtData.gnSigmaE,"gnSigmaE[ngTracks]/F");
		mMtdEvt->Branch("gq",&mMtdEvtData.gq,"gq[ngTracks]/B");
		mMtdEvt->Branch("gtrackId",&mMtdEvtData.gtrackId,"gtrackId[ngTracks]/I");
		mMtdEvt->Branch("gIndex2Primary",&mMtdEvtData.gIndex2Primary,"gIndex2Primary[ngTracks]/I");
		mMtdEvt->Branch("gnFtPts",&mMtdEvtData.gnFtPts,"gnFtPts[ngTracks]/B");
		mMtdEvt->Branch("gnDedxPts",&mMtdEvtData.gnDedxPts,"gnDedxPts[ngTracks]/B");

		mMtdEvt->Branch("gchannel",&mMtdEvtData.gchannel,"gchannel[ngTracks]/I");
		mMtdEvt->Branch("gyLocal",&mMtdEvtData.gyLocal,"gyLocal[ngTracks]/F");
		mMtdEvt->Branch("gzLocal",&mMtdEvtData.gzLocal,"gzLocal[ngTracks]/F");
		mMtdEvt->Branch("gtdc",&mMtdEvtData.gtdc,"gtdc[ngTracks]/F");
		mMtdEvt->Branch("gtot",&mMtdEvtData.gtot,"gtot[ngTracks]/F");
		mMtdEvt->Branch("gtof",&mMtdEvtData.gtof,"gtof[ngTracks]/F");
		mMtdEvt->Branch("gpathLength",&mMtdEvtData.gpathLength,"gpathLength[ngTracks]/F");
		mMtdEvt->Branch("gbeta",&mMtdEvtData.gbeta,"gbeta[ngTracks]/F");
		mMtdEvt->Branch("gtdiff",&mMtdEvtData.gtdiff,"gtdiff[ngTracks]/F");


		/// project to MTD
		mMtdEvt->Branch("gdca",&mMtdEvtData.gdca,"gdca[ngTracks]/F");
		mMtdEvt->Branch("gprojMtdBackLeg",&mMtdEvtData.gprojMtdBackLeg,"gprojMtdBackLeg[ngTracks]/b");
		mMtdEvt->Branch("gprojMtdModule",&mMtdEvtData.gprojMtdModule,"gprojMtdModule[ngTracks]/b");
		mMtdEvt->Branch("gprojMtdCell",&mMtdEvtData.gprojMtdCell,"gprojMtdCell[ngTracks]/b");
		mMtdEvt->Branch("gprojMtdPhi",&mMtdEvtData.gprojMtdPhi,"gprojMtdPhi[ngTracks]/F");
		mMtdEvt->Branch("gprojMtdZ",&mMtdEvtData.gprojMtdZ,"gprojMtdZ[ngTracks]/F");
		mMtdEvt->Branch("gprojMtdLength",&mMtdEvtData.gprojMtdLength,"gprojMtdLength[ngTracks]/F");
		mMtdEvt->Branch("gprojTofPhi",&mMtdEvtData.gprojTofPhi,"gprojTofPhi[ngTracks]/F");
		mMtdEvt->Branch("gprojTofZ",&mMtdEvtData.gprojTofZ,"gprojTofZ[ngTracks]/F");
		mMtdEvt->Branch("gprojTofLength",&mMtdEvtData.gprojTofLength,"gprojTofLength[ngTracks]/F");
		mMtdEvt->Branch("gtof2Tof",&mMtdEvtData.gtof2Tof,"gtof2Tof[ngTracks]/F");
		mMtdEvt->Branch("gtof2Mtd",&mMtdEvtData.gtof2Mtd,"gtof2Mtd[ngTracks]/F");

		/// Matched Mtd Hits
		mMtdEvt->Branch("gnMatchMtdHits",&mMtdEvtData.gnMatchMtdHits,"gnMatchMtdHits[ngTracks]/I");
		mMtdEvt->Branch("gmMtdHitIndex",&mMtdEvtData.gmMtdHitIndex,"gmMtdHitIndex[ngTracks]/I");
		mMtdEvt->Branch("gmBackLeg",&mMtdEvtData.gmBackLeg,"gmBackLeg[ngTracks]/b");
		mMtdEvt->Branch("gmModule",&mMtdEvtData.gmModule,"gmModule[ngTracks]/b");
		mMtdEvt->Branch("gmCell",&mMtdEvtData.gmCell,"gmCell[ngTracks]/b");
		mMtdEvt->Branch("gmLeTimeWest",&mMtdEvtData.gmLeTimeWest,"gmLeTimeWest[ngTracks]/F");
		mMtdEvt->Branch("gmTotWest",&mMtdEvtData.gmTotWest,"gmTotWest[ngTracks]/F");
		mMtdEvt->Branch("gmLeTimeEast",&mMtdEvtData.gmLeTimeEast,"gmLeTimeEast[ngTracks]/F");
		mMtdEvt->Branch("gmTotEast",&mMtdEvtData.gmTotEast,"gmTotEast[ngTracks]/F");
		mMtdEvt->Branch("gmLocalZ",&mMtdEvtData.gmLocalZ,"gmLocalZ[ngTracks]/F");
		mMtdEvt->Branch("gmLocalY",&mMtdEvtData.gmLocalY,"gmLocalY[ngTracks]/F");
		AddObj(mMtdEvt,".hist");
}

/// InitRun: initialize geometries (retrieve beam line constraint from database)
Int_t StMtdMatchMaker::InitRun(int runnumber) {

	//========== Set Beam Line ===================== 
	double x0 = 0.;
	double y0 = 0.;
	double dxdz = 0.;
	double dydz = 0.;

	//Get Current Beam Line Constraint from database
	TDataSet* dbDataSet = this->GetDataBase("Calibrations/rhic");

	if (dbDataSet) {
		vertexSeed_st* vSeed = ((St_vertexSeed*) (dbDataSet->FindObject("vertexSeed")))->GetTable();

		x0 = vSeed->x0;
		y0 = vSeed->y0;
		dxdz = vSeed->dxdz;
		dydz = vSeed->dydz;
	}
	else {
		LOG_INFO << "StMtdMatchMaker -- No Database for beamline" << endm;
	}

	LOG_INFO << "BeamLine Constraint (StMtdMatchMaker): " << endm;
	LOG_INFO << "x(z) = " << x0 << " + " << dxdz << " * z" << endm;
	LOG_INFO << "y(z) = " << y0 << " + " << dydz << " * z" << endm;

	StThreeVectorD origin(x0,y0,0.0);
	double pt = 88889999;
	double nxy=::sqrt(dxdz*dxdz +  dydz*dydz);
	if(nxy<1.e-5){ // beam line _MUST_ be tilted
		LOG_WARN << "StMtdMatchMaker:: Beam line must be tilted!" << endm;
		nxy=dxdz=1.e-5;
	}
	double p0=pt/nxy;
	double px   = p0*dxdz;
	double py   = p0*dydz;
	double pz   = p0; // approximation: nx,ny<<0
	StThreeVectorD MomFstPt(px*GeV, py*GeV, pz*GeV);
	//delete mBeamHelix;
	mBeamHelix = new StPhysicalHelixD(MomFstPt,origin,0.5*tesla,1.);
	//mBeamX = x0;
	//mBeamY = y0;


	return kStOK;
}


/// FinishRun: clean up BeamHelix (will be reinitialized at the next initRun)
Int_t StMtdMatchMaker::FinishRun(int runnumber)
{

	delete mBeamHelix;

	return kStOK;
}


/// Finish up at the end of a job: write and close the QA histogram file
Int_t StMtdMatchMaker::Finish(){
	return kStOK;
}


/// Make: match extrapolated TPC tracks to MRPCs in the MTD trays.
Int_t StMtdMatchMaker::Make(){

	mEvent=(StEvent *) GetInputDS("StEvent");
	if(mEvent){
		mMuDstIn = kFALSE;
	}else{
		mMuDst = (StMuDst *)GetInputDS("MuDst");
		mMuDstIn = kTRUE;
	}
	if(mSaveTree) initEventData();

	if(mHisto) mEventCounterHisto->Fill(0);
	StTimer timer;
	if(doPrintCpuInfo) timer.start();
	if(doPrintMemoryInfo) StMemoryInfo::instance()->snapshot();
	// read data from StMtdHit
	/// A. build vector of candidate cells
	//
	mtdCellHitVector daqCellsHitVec;
	idVector validModuleVec;
	if(!readMtdHits(daqCellsHitVec,validModuleVec)) return kStOK;
	//end of Sect.A
	if(Debug()){
		LOG_INFO<<" Sect.A: =============================="<<endm;
		LOG_INFO <<" total # of cells =" << daqCellsHitVec.size() << endm;
		for(size_t iv=0;iv<validModuleVec.size();iv++){
			LOG_INFO << " module# "<< validModuleVec[iv] <<" Valid! "<<endm;
		}
		LOG_INFO<<" daqCellsHitVec:"<<endm;
		mtdCellHitVectorIter daqIter = daqCellsHitVec.begin();
		for(unsigned int idaq=0;idaq<daqCellsHitVec.size();idaq++, daqIter++) {
			int daqChn = (daqIter->module-1)*12 + (daqIter->cell);
			int daqBackleg = daqIter->backleg;
			LOG_INFO<<" 	 daq backleg:"<<daqBackleg<<" chn:"<<daqChn<<endm;
		}
	}

	if(mHisto) {
		mCellsMultInEvent->Fill(daqCellsHitVec.size());
		if(daqCellsHitVec.size()) mEventCounterHisto->Fill(6);
	}
	if(doPrintCpuInfo) {
		timer.stop();
		LOG_INFO <<"CPU time after Step A - loading hits: "
			<< timer.elapsedTime() <<" sec"<<endm;
		timer.start();
	}
	//...................................................................................
	//
	mtdCellHitVector allCellsHitVec;
	Int_t nPrimaryHits(0);
	/// B. Project to MTD and find all cell-track matches
	project2Mtd(daqCellsHitVec,allCellsHitVec,nPrimaryHits);

	if(mHisto) {
		mHitsMultInEvent->Fill(allCellsHitVec.size());
		mHitsPrimaryInEvent->Fill(nPrimaryHits);
		if(allCellsHitVec.size()) mEventCounterHisto->Fill(7);
	}

	if(Debug()){
		LOG_INFO<<" Sect.B: =============================="<<endm;
		LOG_INFO<<" allCellsHitVec:"<<endm;
		mtdCellHitVectorIter proIter = allCellsHitVec.begin();
		for(unsigned int ipro=0;ipro<allCellsHitVec.size();ipro++, proIter++) {
			int proChn = (proIter->module-1)*12 + (proIter->cell);
			int proBackleg = proIter->backleg;
			LOG_INFO<<" 	proj backleg:"<<proBackleg<<" chn:"<<proChn<<endm;
		}
	}
	// end of Sect.B
	if(doPrintCpuInfo) {
		timer.stop();
		LOG_INFO << "CPU time after Step B - projection:"
			<< timer.elapsedTime() << "sec" <<endm;
		timer.start();
	}
	//....................................................................................
	//
	/// C. Match find neighbours 
	mtdCellHitVector matchHitCellsVec;
	matchMtdHits(daqCellsHitVec,allCellsHitVec,matchHitCellsVec);
	if(Debug()){
		LOG_INFO<<" Sect.C: =============================="<<endm;
		LOG_INFO<<" matchCellsHitVec:"<<endm;
		mtdCellHitVectorIter matIter = matchHitCellsVec.begin();
		for(unsigned int imat=0;imat<matchHitCellsVec.size();imat++, matIter++) {
			int matChn = (matIter->module-1)*12 + (matIter->cell);
			int matBackleg = matIter->backleg;
			LOG_INFO<<"		match backleg:"<<matBackleg<<" chn:"<<matChn<<endm;
		}
	}

	//end of Sect.C
	if(Debug()){ LOG_INFO <<"C:before/after:"<< allCellsHitVec.size()<< "/"<<matchHitCellsVec.size()<<endm;}
	if(mHisto&&matchHitCellsVec.size()) mEventCounterHisto->Fill(8);
	if(doPrintCpuInfo){
		timer.stop();
		LOG_INFO << "CPU time after Step C - matching:"
			<<timer.elapsedTime()<< "sec" <<endm;
		timer.start();
	}
	//....................................................................................
	/// D. Sort hit vectors and discard cells matched with multiple tracks
	//
	mtdCellHitVector singleHitCellsVec;
	mtdCellHitVector multiHitsCellsVec;
	sortSingleAndMultiHits(matchHitCellsVec,singleHitCellsVec,multiHitsCellsVec);
	if(Debug()){
		LOG_INFO<<" Sect.D: =============================="<<endm;
		LOG_INFO<<" singleCellsHitVec:"<<endm;
		mtdCellHitVectorIter singleIter = singleHitCellsVec.begin();
		for(unsigned int isingle=0;isingle<singleHitCellsVec.size();isingle++, singleIter++) {
			int singleChn = (singleIter->module-1)*12 + (singleIter->cell);
			int singleBackleg = singleIter->backleg;
			LOG_INFO<<" 	single backleg:"<<singleBackleg<<" chn:"<<singleChn<<endm;
			LOG_INFO<<" 	LeTimeWest:"<<singleIter->leadingEdgeTime.first<<" LeTimeEast:"<<singleIter->leadingEdgeTime.second<<endm;
		}
		LOG_INFO<<"	multiCellsHitVec:"<<endm;
		mtdCellHitVectorIter multiIter = multiHitsCellsVec.begin();
		for(unsigned int imulti=0;imulti<multiHitsCellsVec.size();imulti++, multiIter++) {
			int multiChn = (multiIter->module-1)*12 + (multiIter->cell);
			int multiBackleg = multiIter->backleg;
			LOG_INFO<<" 	multi backleg:"<<multiBackleg<<" chn:"<<multiChn<<endm;
		}
	}
	//end of Sect.D
	if(doPrintCpuInfo){
		timer.stop();
		LOG_INFO <<" CPU time after Step D - sorting:"
			<<timer.elapsedTime()<< " sec"<<endm;
		timer.start();
	}
	//......................................................................................
	/// E. Sort and deal singleHitCellsVector for multiple cells associated with a single track
	//
	mtdCellHitVector finalMatchedCellsVec;
	finalMatchedMtdHits(singleHitCellsVec,finalMatchedCellsVec);
	if(Debug()){
		LOG_INFO<<" Sect.E: =============================="<<endm;
		LOG_INFO<<"	finalMatchedCellsHitVec:"<<endm;
		mtdCellHitVectorIter finalIter = finalMatchedCellsVec.begin();
		for(unsigned int ifinal=0;ifinal<finalMatchedCellsVec.size();ifinal++, finalIter++) {
			int finalChn = (finalIter->module-1)*12 + (finalIter->cell);
			int finalBackleg = finalIter->backleg;
			LOG_INFO<<" 	final backleg:"<<finalBackleg<<" chn:"<<finalChn<<endm;
		}
	}
	//end of Sect.E
	if(Debug()){ LOG_INFO <<"E: before/after:"<< singleHitCellsVec.size() <<"/" <<finalMatchedCellsVec.size()<<endm;}
	if(doPrintCpuInfo){
		LOG_INFO <<"CPU time after Step E - final matched:"
			<<timer.elapsedTime() <<" sec"<<endm;
		timer.start();
	}
	//.......................................................................
	/// F. Fill QA histograms, and PID traits
	//
	if(mHisto) {
		if(finalMatchedCellsVec.size()) mEventCounterHisto->Fill(10);
		mCellsPerEventMatch3->Fill(finalMatchedCellsVec.size());
	}
	Int_t nValidSingleHitCells(0),nValidSinglePrimHitCells(0);
	fillPidTraits(finalMatchedCellsVec,nValidSingleHitCells,nValidSinglePrimHitCells);
	//end of Sect.F
	if(Debug()){
		LOG_INFO<<" Sect.F: =============================="<<endm;
		LOG_INFO<<"	nValidSingleHitCells:"<<nValidSingleHitCells<<endm;

		LOG_INFO <<"#(daq):" <<daqCellsHitVec.size()
							   << "#(proj):"<<allCellsHitVec.size()
											  << "#(prim proj):" <<nPrimaryHits
																   << "#(single valid):"<<nValidSingleHitCells <<endm;
		LOG_INFO <<"F: before/after"<< finalMatchedCellsVec.size()<< "/"<<nValidSingleHitCells <<endm;
	}
	if(doPrintCpuInfo){
		timer.stop();
		LOG_INFO << "CPU time after Step F - final :"
			<< timer.elapsedTime()<< "sec"<<endm;
		timer.start();
	}
	/// Look for the mtdCollection in either the MuDST or StEvent environment
	if(mMuDstIn){
		StMuMtdCollection *theMtd = mMuDst->MtdCollection();
		if(theMtd){
			if(theMtd->hitsPresent()){
				LOG_DEBUG <<" MtdCollection: hit container present. "<<endm;
				if(Debug()){
					LOG_INFO << "# of hits in this event:" << theMtd->hitsPresent() <<endm;
					for(Int_t i=0;i<theMtd->hitsPresent();i++){
						StMuMtdHit* aHit = theMtd->MtdHit(i) ;
						if(!aHit) continue;
						LOG_INFO <<"backleg:"<<aHit->backleg()
							<<" module:"<<aHit->backleg()
							<<" cell:"<<aHit->cell()
							<<" tof:"<<aHit->tof()<<endm;
					}
				}
			}
		}
	}else{
		StMtdCollection *theMtd = mEvent->mtdCollection();
		if(theMtd){
			if(theMtd->hitsPresent()){
				LOG_DEBUG <<" MtdCollection: hit container present. "<<endm;
				if(Debug()){
					StSPtrVecMtdHit& tmpCellMtdVec = theMtd->mtdHits();
					LOG_INFO << "# of hits in this event:" << tmpCellMtdVec.size()<<endm;
					for(size_t i=0;i<tmpCellMtdVec.size();i++){
						StMtdHit* p=tmpCellMtdVec[i];
						LOG_INFO <<(*p)<<endm;
					}
				}
			}
		}
	}
	//-- end check
	if(doPrintMemoryInfo){
		StMemoryInfo::instance()->snapshot();
		StMemoryInfo::instance()->print();
	}
	if(doPrintCpuInfo){
		timer.stop();
		LOG_INFO<< "CPU time for StMtdMatchMaker::Make():"
			<<timer.elapsedTime()<< "sec"<<endm;
	}
	if(mSaveTree){
		mMtdEvt->Fill();
	}
	if(Debug()){
		LOG_INFO<<"StMtdMatchMaker -- bye--bye"<<endm;
	}

	return kStOK;
}

//---------------------------------------------------------------------------
/// Read MTD hits from either StEvent or MuDST environments
Bool_t StMtdMatchMaker::readMtdHits(mtdCellHitVector& daqCellsHitVec,idVector& validModuleVec){

	// StMuMtdCollection can't save Hits yet. Pending MuDst Mode.
	if(mMuDstIn){
		if(!mMuDst){
			LOG_INFO << "No Mudst ... bye-bye" <<endm;
			return kFALSE;
		}
                /// A. build vector of candidate cells
		int nMtdHits = mMuDst->numberOfMTDHit();
		if(nMtdHits<=0) return kFALSE;
		for(Int_t i=0;i<nMtdHits;i++){
			StMuMtdHit* aHit = mMuDst->mtdHit(i) ;
			if(!aHit) continue;
			if(aHit->backleg()<=0||aHit->backleg()>mNBacklegs) continue;   // barrel BackLeg hits
			int backlegId = aHit->backleg();
			int moduleId = aHit->module();

			int cellId = aHit->cell();
			if(Debug()) {LOG_INFO <<"A: fired hit in " << "backleg=" << backlegId <<" module="<<moduleId<<" cell="<<cellId<<endm;}
			StructCellHit aDaqCellHit;
			aDaqCellHit.backleg = backlegId;
			aDaqCellHit.module= moduleId;
			aDaqCellHit.cell=cellId;
			aDaqCellHit.tot=aHit->tot();
			aDaqCellHit.leadingEdgeTime=aHit->leadingEdgeTime();
			aDaqCellHit.index2MtdHit=i;
			daqCellsHitVec.push_back(aDaqCellHit);

			//additional valid number configuration
			int id=backlegId*100+moduleId;
			if(find(validModuleVec.begin(),validModuleVec.end(),id) == validModuleVec.end())
				validModuleVec.push_back(id);
			int hisIndex = backlegId - 1;
			if(mHisto) {
				mDaqOccupancy[hisIndex]->Fill((moduleId-1)*12+cellId);
			}
		}
		StMuMtdHeader* mtdHeader=mMuDst->mtdHeader();
		unsigned int trgTime[2];
		if(mtdHeader){
			trgTime[0]=mtdHeader->triggerTime(1);
			trgTime[1]=mtdHeader->triggerTime(2);
		}
		Double_t triggerTime[2]={0};
		triggerTime[0]=25.*(trgTime[0]&0xfff);//ns
		triggerTime[1]=25.*(trgTime[1]&0xfff);//ns

		if(mSaveTree){

		  int nTrgIds(0);
		  //fg mMtdEvtData.trgId[nTrgIds] = 0; // make sure to zero the first entry
		  // protect against zero pointers (in simulated data, should not be necessary for MuDST)
		  //fg      if (mEvent->triggerIdCollection() && mEvent->triggerIdCollection()->nominal()) {
			  for(int i=0;i<kMaxTriggerIds;i++){
			    int trgId = mMuDst->event()->triggerIdCollection().nominal().triggerId(i);
			    if(trgId>0){
			      mMtdEvtData.trgId[nTrgIds] = trgId;
			      nTrgIds++;
			    }
			  }
		   //fg	}
			mMtdEvtData.nTrgIds = nTrgIds;       
			mMtdEvtData.run = mMuDst->event()->runNumber();       // the run number
			mMtdEvtData.evt = mMuDst->event()->eventId();       // the event number
			mMtdEvtData.bField= mMuDst->event()->runInfo().magneticField()/10.; 

			StPrimaryVertex *pVtx = mEvent->primaryVertex();
			float xvtx = -999.;
			float yvtx = -999.;
			float zvtx = -999.;
			if (pVtx){
			  xvtx = mEvent->primaryVertex()->position().x();
			  yvtx = mEvent->primaryVertex()->position().y();
			  zvtx = mEvent->primaryVertex()->position().z();
			}
			else {
			  LOG_WARN << "No (default) primary vertex information for this (st-) event"  << endm;
			};

			mMtdEvtData.vertexX = xvtx;        
			mMtdEvtData.vertexY = yvtx;              
			mMtdEvtData.vertexZ = zvtx;              

			StBTofHeader *mBTofHeader = mMuDst->btofHeader();
			if(mBTofHeader) {
				mMtdEvtData.vpdVz  = mBTofHeader->vpdVz();
				mMtdEvtData.tStart = mBTofHeader->tStart();
			}
			mMtdEvtData.triggerTime[0] = triggerTime[0]; // ns 
			mMtdEvtData.triggerTime[1] = triggerTime[1]; // ns 

			int nMtdRawHits = mMuDst->numberOfBMTDRawHit();
			mMtdEvtData.nMtdRawHits = nMtdRawHits;  
			mMtdEvtData.nMtdHits = nMtdHits; 

			for(Int_t i=0;i<nMtdRawHits;i++){
				StMuMtdRawHit* aRawHit=mMuDst->mtdRawHit(i);
				int mrflag=aRawHit->flag();
				int mrbackleg=aRawHit->backleg();
				int mrchn=aRawHit->channel();
				int mrtdc=aRawHit->tdc();
				mMtdEvtData.flag[i] = mrflag;
				mMtdEvtData.backlegRaw[i] = mrbackleg;
				mMtdEvtData.chn[i] = mrchn;
				mMtdEvtData.tdc[i] = mrtdc;
			}

			LOG_DEBUG << "Number of Mtd Raw Hits = " << nMtdRawHits <<endm;
			LOG_DEBUG << "Number of Mtd Hits = " << nMtdHits <<endm;
			for(Int_t i=0;i<nMtdHits;i++){
				StMuMtdHit* aHit = mMuDst->mtdHit(i);
				if(!aHit) continue;
				if(aHit->backleg()<=0||aHit->backleg()>mNBacklegs) continue;   // barrel BackLeg hits
				int backlegId = aHit->backleg();
				int moduleId = aHit->module();
				int cellId = aHit->cell();

				float mleadingwest=aHit->leadingEdgeTime().first;
				float mleadingeast=aHit->leadingEdgeTime().second;
				float mtrailingwest=aHit->trailingEdgeTime().first;
				float mtrailingeast=aHit->trailingEdgeTime().second;
				mMtdEvtData.backleg[i] = backlegId;
				mMtdEvtData.module[i] = moduleId;
				mMtdEvtData.cell[i] = cellId;
				mMtdEvtData.leTimeWest[i] = mleadingwest;
				mMtdEvtData.leTimeEast[i] = mleadingeast;
				mMtdEvtData.totWest[i] = mtrailingwest-mleadingwest;
				mMtdEvtData.totEast[i] = mtrailingeast-mleadingeast;
			}
		}
	}else{  
		if(!mEvent||!(mEvent->mtdCollection())||!(mEvent->mtdCollection()->hitsPresent())){
			if(!mEvent){LOG_INFO << "no StEvent" <<endm; return kFALSE;}
			else if(!(mEvent->mtdCollection())) {
				LOG_INFO << "no MTD Collection" <<endm;
				return kFALSE;
			}
			else if(!(mEvent->mtdCollection()->hitsPresent())){LOG_INFO << "no MTD hits present" <<endm; return kFALSE;}
			return kFALSE;
		}

		/// QA test
		//.........................................................................
		/// check for mtdCollection and fill local copy with ADC and TDC data
		StMtdCollection *theMtd = mEvent->mtdCollection();
		StMtdHeader* mtdHeader=theMtd->mtdHeader();
		unsigned int trgTime[2];
		if(mtdHeader){
			trgTime[0]=mtdHeader->triggerTime(0);
			trgTime[1]=mtdHeader->triggerTime(1);
		}
		Double_t triggerTime[2]={0};
		triggerTime[0]=25.*(trgTime[0]&0xfff);//ns
		triggerTime[1]=25.*(trgTime[1]&0xfff);//ns




		//.........................................................................
		/// read data from StMtdHit
		StSPtrVecMtdHit& mtdHits= theMtd->mtdHits();
		StSPtrVecMtdRawHit& mtdRawHits=theMtd->mtdRawHits();
		if(mSaveTree){

		  int nTrgIds(0);
		  mMtdEvtData.trgId[nTrgIds] = 0; // make sure to zero the first entry
		  // protect against zero pointers (in simulated data)
		        if (mEvent->triggerIdCollection() && mEvent->triggerIdCollection()->nominal()) {
			  for(int i=0;i<kMaxTriggerIds;i++){
			    int trgId = mEvent->triggerIdCollection()->nominal()->triggerId(i);
			    if(trgId>0){ 
			      mMtdEvtData.trgId[nTrgIds] = trgId;
			      nTrgIds++;
			    }
			  }
			}
			mMtdEvtData.nTrgIds = nTrgIds;       
			mMtdEvtData.run = mEvent->runId();       // the run number
			mMtdEvtData.evt = mEvent->id();       // the event number
			mMtdEvtData.bField= mEvent->runInfo()->magneticField()/10.; 

			StPrimaryVertex *pVtx = mEvent->primaryVertex();
			float xvtx = -999.;
			float yvtx = -999.;
			float zvtx = -999.;
			if (pVtx){
			  xvtx = mEvent->primaryVertex()->position().x();
			  yvtx = mEvent->primaryVertex()->position().y();
			  zvtx = mEvent->primaryVertex()->position().z();
			}
			else {
			  LOG_WARN << "No (default) primary vertex information for this (st-) event"  << endm;
			};

			mMtdEvtData.vertexX = xvtx;        
			mMtdEvtData.vertexY = yvtx;              
			mMtdEvtData.vertexZ = zvtx;              

			mMtdEvtData.triggerTime[0] = triggerTime[0]; // ns 
			mMtdEvtData.triggerTime[1] = triggerTime[1]; // ns 
			mMtdEvtData.nMtdRawHits = mtdRawHits.size();  
			mMtdEvtData.nMtdHits = mtdHits.size(); 
			for(size_t i=0;i<mtdRawHits.size();i++){
				StMtdRawHit* aRawHit=mtdRawHits[i];
				int mrflag=aRawHit->flag();
				int mrbackleg=aRawHit->backleg();
				int mrchn=aRawHit->channel();
				int mrtdc=aRawHit->tdc();
				mMtdEvtData.flag[i] = mrflag;
				mMtdEvtData.backlegRaw[i] = mrbackleg;
				mMtdEvtData.chn[i] = mrchn;
				mMtdEvtData.tdc[i] = mrtdc;
			}

			LOG_DEBUG << "Number of Mtd Raw Hits = " << mtdRawHits.size() <<endm;
			LOG_DEBUG << "Number of Mtd Hits = " << mtdHits.size() <<endm;
			for(size_t i=0;i<mtdHits.size();i++){
				StMtdHit* aHit = mtdHits[i];
				if(!aHit) continue;
				if(aHit->backleg()<=0||aHit->backleg()>mNBacklegs) continue;   // barrel BackLeg hits
				int backlegId = aHit->backleg();
				int moduleId = aHit->module();
				int cellId = aHit->cell();

				float mleadingwest=aHit->leadingEdgeTime().first;
				float mleadingeast=aHit->leadingEdgeTime().second;
				float mtrailingwest=aHit->trailingEdgeTime().first;
				float mtrailingeast=aHit->trailingEdgeTime().second;
				mMtdEvtData.backleg[i] = backlegId;
				mMtdEvtData.module[i] = moduleId;
				mMtdEvtData.cell[i] = cellId;
				mMtdEvtData.leTimeWest[i] = mleadingwest;
				mMtdEvtData.leTimeEast[i] = mleadingeast;
				mMtdEvtData.totWest[i] = mtrailingwest-mleadingwest;
				mMtdEvtData.totEast[i] = mtrailingeast-mleadingeast;
			}
		}
		for(size_t i=0;i<mtdHits.size();i++){
			StMtdHit* aHit = mtdHits[i];
			if(!aHit) continue;
			if(aHit->backleg()<=0||aHit->backleg()>mNBacklegs) continue;   // barrel BackLeg hits
			int backlegId = aHit->backleg();
			int moduleId = aHit->module();
			int cellId = aHit->cell();

			if(Debug()) {LOG_INFO <<"A: fired hit in " << "backleg=" << backlegId <<" module="<<moduleId<<" cell="<<cellId<<endm;}
			LOG_DEBUG <<"A: fired hit in " << "backleg=" << backlegId <<" module="<<moduleId<<" cell="<<cellId<<" leadingWest="<<aHit->leadingEdgeTime().first<<" leadingEast="<<aHit->leadingEdgeTime().second<<endm;
			StructCellHit aDaqCellHit;
			aDaqCellHit.backleg = backlegId;
			aDaqCellHit.module= moduleId;
			aDaqCellHit.cell=cellId;
			aDaqCellHit.tot=aHit->tot();
			aDaqCellHit.leadingEdgeTime=aHit->leadingEdgeTime();
			aDaqCellHit.index2MtdHit=i;
			daqCellsHitVec.push_back(aDaqCellHit);

			//additional valid number configuration
			int id=backlegId*100+moduleId;
			if(find(validModuleVec.begin(),validModuleVec.end(),id) == validModuleVec.end())
				validModuleVec.push_back(id);

			int hisIndex = backlegId - 1;
			if(mHisto) {
				mDaqOccupancy[hisIndex]->Fill((moduleId-1)*12+cellId);
			}
		}
	}
	return kTRUE;
}


/// Project TPC tracks to the MTD trays
void StMtdMatchMaker::project2Mtd(mtdCellHitVector daqCellsHitVec,mtdCellHitVector& allCellsHitVec,Int_t& nPrimaryHits){
	int nAllTracks=0;
	ngTracks = 0;

	UInt_t Nnodes = 0;
	if(mMuDstIn){
		Nnodes = mMuDst->numberOfGlobalTracks();
	}else{
		Nnodes = mEvent->trackNodes().size();
	}
	for(UInt_t iNode=0;iNode<Nnodes;iNode++){

		float gdEdx=-999.;
		float gpt=-999.,geta=-999.,gphi=-999.;
		float ppt=-999.,peta=-999.,pphi=-999.;
		float nSigmaE = -999.,nSigmaPi = -999.,nSigmaK = -999.,nSigmaP = -999.;
		float gyLocal=-999.,gzLocal=-999.;
		float gtdc=-999.,gtof=-999.,gtot=-999.;
		float gpathLength=-999.,gbeta=-999.;
		int   gq = 0,gndEdxpts=0,gnFtPts=0;
		int idx2primary = -1;
		int gchannel=-1;
		StThreeVectorD globalPos;
		StPhysicalHelixD helix;

		if(mMuDstIn){
			StMuTrack *theTrack=mMuDst->globalTracks(iNode);
			if(!theTrack) continue;
			bool isPrimary=kFALSE;
			const StMuTrack *thePrimaryTrack=theTrack->primaryTrack();
			if(thePrimaryTrack) isPrimary=kTRUE;
			if(!validTrack(theTrack)) continue;

			nSigmaE  = theTrack->nSigmaElectron(); 
			nSigmaPi = theTrack->nSigmaPion();
			nSigmaK  = theTrack->nSigmaKaon();
			nSigmaP  = theTrack->nSigmaProton();

			gdEdx 		= theTrack->dEdx();
			gndEdxpts 	= theTrack->nHitsDedx();
			gnFtPts 	= theTrack->nHitsFit();
			gq 			= theTrack->charge();
			gpt 		= theTrack->momentum().perp();
			geta 		= theTrack->momentum().pseudoRapidity();
			gphi 		= theTrack->momentum().phi();
			
			if(isPrimary){
				ppt 	= thePrimaryTrack->momentum().perp();
				peta 	= thePrimaryTrack->momentum().pseudoRapidity();
				pphi 	= thePrimaryTrack->momentum().phi();
			}
			while(gphi<0.)gphi+=2.*(TMath::Pi());
			while(gphi>2*(TMath::Pi()))gphi-=2.*(TMath::Pi());
			if(isPrimary) idx2primary = thePrimaryTrack->id();

			helix = theTrack->outerHelix();

			const StMuBTofPidTraits tofpid = theTrack->btofPidTraits();
			const StMuBTofHit* aHit = theTrack->tofHit();
			if(aHit){
				int tray = aHit->tray();	
				int module = aHit->module();	
				int cell = aHit->cell();	
				gchannel = (tray-1)*192+(module-1)*6+cell-1;
				gyLocal = tofpid.yLocal();
				gzLocal = tofpid.zLocal();
				globalPos = tofpid.position();
				LOG_DEBUG<<" globalPos x,y,z :"<<globalPos.x()<<","<<globalPos.y()<<","<<globalPos.z()<<endm;
				gtdc = aHit->leadingEdgeTime();
				gtot = aHit->tot();
				gtof = tofpid.timeOfFlight();
				gpathLength = tofpid.pathLength();
				gbeta = tofpid.beta();
				LOG_DEBUG <<" project2MTD() Found matched TOF hit: tray:"<< tray << " module:"<<module<<" cell:"<<cell<<" gtdc:"<<aHit->leadingEdgeTime()<<" gtof:"<<aHit->tot()<<" gtof:"<<tofpid.timeOfFlight()<<" gpathLength:"<<tofpid.pathLength()<<" gbeta:"<<tofpid.beta()<<endm;
			}

			if(matchTrack2Mtd(daqCellsHitVec,theTrack,allCellsHitVec,iNode,globalPos)){
				nAllTracks++;
				if(isPrimary) nPrimaryHits++;
			}

		}else{
			StSPtrVecTrackNode& nodes=mEvent->trackNodes();
			StGlobalTrack *theTrack = dynamic_cast<StGlobalTrack*>(nodes[iNode]->track(global));
			if(!theTrack) continue;
			bool isPrimary =kFALSE;
			StPrimaryTrack *pTrack =dynamic_cast<StPrimaryTrack*>(theTrack->node()->track(primary));
			if(pTrack) isPrimary = kTRUE;
			if(!validTrack(theTrack)) continue;

			static StTpcDedxPidAlgorithm PidAlgorithm;
			static StElectron* Electron = StElectron::instance();
			static StPionPlus* Pion = StPionPlus::instance();
			static StKaonPlus* Kaon = StKaonPlus::instance();
			static StProton* Proton = StProton::instance();
			const StParticleDefinition* pd = theTrack->pidTraits(PidAlgorithm);

			if (pd) {
				nSigmaE  = PidAlgorithm.numberOfSigma(Electron);
				nSigmaPi = PidAlgorithm.numberOfSigma(Pion);
				nSigmaK  = PidAlgorithm.numberOfSigma(Kaon);
				nSigmaP  = PidAlgorithm.numberOfSigma(Proton);
			}

			if(PidAlgorithm.traits()){
				gdEdx = PidAlgorithm.traits()->mean();
				gndEdxpts=PidAlgorithm.traits()->numberOfPoints();
			}

			gq		= theTrack->geometry()->charge();
			gpt		= theTrack->geometry()->momentum().perp();
			geta 	= theTrack->geometry()->momentum().pseudoRapidity();
			gphi 	= theTrack->geometry()->momentum().phi();
			if(isPrimary){

				ppt		= pTrack->geometry()->momentum().perp();
				peta 	= pTrack->geometry()->momentum().pseudoRapidity();
				pphi 	= pTrack->geometry()->momentum().phi();

			}
			gnFtPts	= theTrack->fitTraits().numberOfFitPoints(kTpcId);
			while(gphi<0.)gphi+=2.*(TMath::Pi());
			while(gphi>2*(TMath::Pi()))gphi-=2.*(TMath::Pi());

			if(isPrimary) idx2primary = pTrack->key();
			
			helix = theTrack->outerGeometry()->helix();

			StSPtrVecTrackPidTraits& traits = theTrack->pidTraits();
			LOG_DEBUG <<" project2MTD() Found track: gpt "<< gpt << " gq:"<<gq<<" geta:"<<geta<<" gphi:"<<gphi<<" gnFtPts:"<<gnFtPts<<endm;
			for (unsigned int it=0;it<traits.size();it++){
				if (traits[it]->detector() == kTofId) {
					StBTofPidTraits* tofpid = dynamic_cast<StBTofPidTraits*>(traits[it]);

					StBTofHit* aHit = tofpid->tofHit();

					if(tofpid && aHit){
						int tray = aHit->tray();	
						int module = aHit->module();	
						int cell = aHit->cell();	
						gchannel = (tray-1)*192+(module-1)*6+cell-1;
						gyLocal  = tofpid->yLocal();
						gzLocal  = tofpid->zLocal();
						globalPos = tofpid->position();
						LOG_DEBUG<<" globalPos x,y,z :"<<globalPos.x()<<","<<globalPos.y()<<","<<globalPos.z()<<endm;
						gtdc = aHit->leadingEdgeTime();
						gtot = aHit->tot();
						gtof = tofpid->timeOfFlight();
						gpathLength = tofpid->pathLength();
						gbeta = tofpid->beta();
						LOG_DEBUG <<" project2MTD() Found matched TOF hit: tray:"<< tray << " module:"<<module<<" cell:"<<cell<<" gtdc:"<<gtdc<<" gtof:"<<gtot<<" gtof:"<<gtof<<" gpathLength:"<<gpathLength<<" gbeta:"<<gbeta<<endm;
					}
				}
			}

			if(matchTrack2Mtd(daqCellsHitVec,theTrack,allCellsHitVec,iNode,globalPos)){
				nAllTracks++;
				if(isPrimary) nPrimaryHits++;
			}
		}
		if(mHisto){
			mTrackPtEta->Fill(gpt, geta);
			mTrackPtPhi->Fill(gpt, gphi);
			mTrackNFitPts->Fill(gnFtPts);
			if(gdEdx>0.) mTrackdEdxvsp->Fill(gpt, gdEdx*1.e6);
			if(fabs(nSigmaPi)<5.) mNSigmaPivsPt->Fill(gpt, nSigmaPi+5.*gq);
		}
		if(mSaveTree){

			float mField = 0;
			if(mMuDstIn) mField = mMuDst->event()->runInfo().magneticField();
			else mField = mEvent->runInfo()->magneticField();

			StThreeVector<double> helixOrigin = helix.origin();
			StThreeVector<double> helixMomentum = helix.momentum(mField*kilogauss);
			float ghelixpx  = helixMomentum.x();
			float ghelixpy  = helixMomentum.y();
			float ghelixpz  = helixMomentum.z();
			float ghelixox  = helixOrigin.x();
			float ghelixoy  = helixOrigin.y();
			float ghelixoz  = helixOrigin.z();

			mMtdEvtData.gq[ngTracks] = gq;
			mMtdEvtData.gtrackId[ngTracks] = iNode;
			mMtdEvtData.gpt[ngTracks] = gpt;
			mMtdEvtData.geta[ngTracks] = geta;
			mMtdEvtData.gphi[ngTracks] = gphi;
			mMtdEvtData.ppt[ngTracks] = ppt;
			mMtdEvtData.peta[ngTracks] = peta;
			mMtdEvtData.pphi[ngTracks] = pphi;
			mMtdEvtData.ghelixpx[ngTracks] = ghelixpx;
			mMtdEvtData.ghelixpy[ngTracks] = ghelixpy;
			mMtdEvtData.ghelixpz[ngTracks] = ghelixpz;
			mMtdEvtData.ghelixox[ngTracks] = ghelixox;
			mMtdEvtData.ghelixoy[ngTracks] = ghelixoy;
			mMtdEvtData.ghelixoz[ngTracks] = ghelixoz;
			mMtdEvtData.gIndex2Primary[ngTracks] = idx2primary;
			mMtdEvtData.gnFtPts[ngTracks] 	= gnFtPts;
			mMtdEvtData.gdedx[ngTracks] 	= gdEdx;
			mMtdEvtData.gnDedxPts[ngTracks] = gndEdxpts;

			mMtdEvtData.gchannel[ngTracks]= gchannel;
			mMtdEvtData.gyLocal[ngTracks] = gyLocal;
			mMtdEvtData.gzLocal[ngTracks] = gzLocal;
			mMtdEvtData.gtdc[ngTracks]   = gtdc;
			mMtdEvtData.gtot[ngTracks]   = gtot;
			mMtdEvtData.gtof[ngTracks]   = gtof; 
			mMtdEvtData.gpathLength[ngTracks] = gpathLength;
			mMtdEvtData.gbeta[ngTracks] = gbeta;
			mMtdEvtData.gnSigmaE[ngTracks] = nSigmaE;
			mMtdEvtData.gnSigmaPi[ngTracks] = nSigmaPi;
			mMtdEvtData.gnSigmaK[ngTracks] = nSigmaK;
			mMtdEvtData.gnSigmaP[ngTracks] = nSigmaP;

		}
		ngTracks++;
	}
	if(mSaveTree){
		mMtdEvtData.ngTracks = ngTracks;
		LOG_DEBUG <<" project2MTD() Found "<<ngTracks<<" global tracks in this event"<<endm;
	}
}


/// Match extrapolated TPC tracks to hits in the MTD
bool StMtdMatchMaker::matchTrack2Mtd(mtdCellHitVector daqCellsHitVec,StPhysicalHelixD helix, Int_t gq, mtdCellHitVector& allCellsHitVec, unsigned int iNode, StThreeVectorD globalPos){
	float mField = 0;
	if(mMuDstIn) mField = mMuDst->event()->runInfo().magneticField();
	else mField = mEvent->runInfo()->magneticField();

	StThreeVector<double> helixOrigin = helix.origin();
	StThreeVector<double> helixMomentum = helix.momentum(mField*kilogauss);
	float ghelixpx  = helixMomentum.x();
	float ghelixpy  = helixMomentum.y();
	float ghelixpz  = helixMomentum.z();
	float ghelixox  = helixOrigin.x();
	float ghelixoy  = helixOrigin.y();
	float ghelixoz  = helixOrigin.z();

	float bField = mField/10.;
	StThreeVector<double> g1P(ghelixpx,ghelixpy,ghelixpz);//momentum 
	StThreeVector<double> g1O(ghelixox,ghelixoy,ghelixoz);//origin
	StPhysicalHelixD gHelixTpc(g1P,g1O,bField*tesla,gq); 
	LOG_DEBUG <<"StMtdMatchMaker::matchTrack2Mtd() "<<" bField"<<bField<<endm;
	StThreeVectorF vertexPos;
	if(mMuDstIn) vertexPos	= mMuDst->event()->primaryVertexPosition();
	else{
	  if (mEvent->primaryVertex()){
	    vertexPos = mEvent->primaryVertex()->position();
	  }
	}
	double length2Vtx = -99999.;
	length2Vtx = TMath::Abs(gHelixTpc.pathLength(vertexPos));
	LOG_DEBUG<<" vertex x,y,z:"<<vertexPos.x()<<","<<vertexPos.y()<<","<<vertexPos.z()<<endm;
	LOG_DEBUG<<" gq:"<<gq<<" ghelix ox,oy,oz:"<<ghelixox<<","<<ghelixoy<<","<<ghelixoz
		<<" ghelix p,pt,eta,phi:"<<helixMomentum.mag()<<","<<helixMomentum.perp()<<","<<helixMomentum.pseudoRapidity()<<","<<helixMomentum.phi()
		<<" length2vertex:"<<length2Vtx<<endm;



	StThreeVector<double> dcaPos  = gHelixTpc.at(gHelixTpc.pathLength(vertexPos));
	StThreeVector<double> dca 	  = dcaPos - vertexPos;
	if(!mCosmicFlag && dca.mag()>10) return kFALSE;

	//project track to TOF radius
	double rTof = 0;
	StThreeVector<double> tofPos;
	if(globalPos.perp()>100){
		rTof = gHelixTpc.pathLength(globalPos);
		tofPos = globalPos;
	}else{
		pairD sTof = gHelixTpc.pathLength(tofRadius);	
		if(sTof.first<=0 && sTof.second<=0){
		}else{
			rTof =  (sTof.first < 0 || sTof.second < 0) 
				? max(sTof.first, sTof.second) : min(sTof.first, sTof.second); 
		}
		if(rTof>0) tofPos = gHelixTpc.at(rTof);
	}
	StThreeVector<double> tofMom = gHelixTpc.momentumAt(rTof,bField*tesla);

	double betaGam = g1P.mag()/muonMass;
	double vInner  = sqrt(betaGam*betaGam/(1+betaGam*betaGam))*c_light*1e-9;
	double tof2Tof = -9999.;
	tof2Tof = (length2Vtx+rTof)/vInner;

	LOG_DEBUG<<" to TOF: pos x,y,z:"<<tofPos.x()<<","<<tofPos.y()<<","<<tofPos.z()<<endm;
	LOG_DEBUG<<" to TOF: mom p,pt,eta,phi:"<<tofMom.mag()<<","<<tofMom.perp()<<","<<tofMom.pseudoRapidity()<<","<<tofMom.phi()<<endm;
	LOG_DEBUG<<" to TOF: tof:"<<tof2Tof<<endm;

	double EMCenergyloss;
	//project track to MTD radius *with* reducing magnetic field and energy loss.
	//step -1:inner EMC
	if(mCosmicFlag){
		EMCenergyloss=-mEMCenergyloss;
	}
	else{
		EMCenergyloss=mEMCenergyloss;
	}
	pairD sInnerEMC = gHelixTpc.pathLength(innerEMCRadius);
	if(sInnerEMC.first<=0 && sInnerEMC.second<=0) return kFALSE;
	double rInnerEMC =  (sInnerEMC.first < 0 || sInnerEMC.second < 0) 
		? max(sInnerEMC.first, sInnerEMC.second) : min(sInnerEMC.first, sInnerEMC.second); 
	StThreeVector<double> innerEMCPos = gHelixTpc.at(rInnerEMC);
	StThreeVector<double> innerEMCMom = gHelixTpc.momentumAt(rInnerEMC,bField*tesla);
	StPhysicalHelixD helixInEMC(innerEMCMom,innerEMCPos,bField*tesla,gq);

	double tof2InnerEMC = -9999.;
	tof2InnerEMC = (length2Vtx+rInnerEMC)/vInner;

	LOG_DEBUG<<" to EMCinner: pos x,y,z:"<<innerEMCPos.x()<<","<<innerEMCPos.y()<<","<<innerEMCPos.z()<<endm;
	LOG_DEBUG<<" to EMCinner: mom p,pt,eta,phi:"<<innerEMCMom.mag()<<","<<innerEMCMom.perp()<<","<<innerEMCMom.pseudoRapidity()<<","<<innerEMCMom.phi()<<endm;
	LOG_DEBUG<<" to EMCinner: tof:"<<tof2InnerEMC<<endm;

	//step 0: outer EMC
	pairD sInnerBSMD = helixInEMC.pathLength(innerBSMDRadius);
	if(sInnerBSMD.first<=0 && sInnerBSMD.second<=0) return kFALSE;
	double rInnerBSMD =  (sInnerBSMD.first < 0 || sInnerBSMD.second < 0) 
		? max(sInnerBSMD.first, sInnerBSMD.second) : min(sInnerBSMD.first, sInnerBSMD.second); 

	StThreeVector<double> innerBSMDPos = helixInEMC.at(rInnerBSMD);
	StThreeVector<double> innerBSMDMom = helixInEMC.momentumAt(rInnerBSMD,bField*tesla);
	innerBSMDMom=(sqrt(pow((sqrt(pow((abs(innerBSMDMom)),2)+pow(muonMass,2))-rInnerBSMD*EMCenergyloss),2)-pow(muonMass,2)))/(abs(innerBSMDMom))*innerBSMDMom;

	betaGam = innerBSMDMom.mag()/muonMass;
	vInner  = sqrt(betaGam*betaGam/(1+betaGam*betaGam))*c_light*1e-9;
	double tof2InnerBSMD = rInnerBSMD/vInner;

	LOG_DEBUG<<" to BSMDinner: pos x,y,z:"<<innerBSMDPos.x()<<","<<innerBSMDPos.y()<<","<<innerBSMDPos.z()<<endm;
	LOG_DEBUG<<" to BSMDinner: mom p,pt,eta,phi:"<<innerBSMDMom.mag()<<","<<innerBSMDMom.perp()<<","<<innerBSMDMom.pseudoRapidity()<<","<<innerBSMDMom.phi()<<endm;
	LOG_DEBUG<<" to BSMDinner: tof:"<<tof2InnerBSMD<<endm;

	StPhysicalHelixD helixInBSMD(innerBSMDMom,innerBSMDPos,bField*tesla,gq);
	pairD sOuterBSMD = helixInBSMD.pathLength(outerBSMDRadius);
	if(sOuterBSMD.first<=0 && sOuterBSMD.second<=0) return kFALSE;
	double rOuterBSMD =  (sOuterBSMD.first < 0 || sOuterBSMD.second < 0) 
		? max(sOuterBSMD.first, sOuterBSMD.second) : min(sOuterBSMD.first, sOuterBSMD.second); 


	StThreeVector<double> outerBSMDPos = helixInBSMD.at(rOuterBSMD);
	StThreeVector<double> outerBSMDMom = helixInBSMD.momentumAt(rOuterBSMD,bField*tesla);
	StPhysicalHelixD helixOutBSMD(outerBSMDMom,outerBSMDPos,bField*tesla,gq);

	betaGam = outerBSMDMom.mag()/muonMass;
	vInner  = sqrt(betaGam*betaGam/(1+betaGam*betaGam))*c_light*1e-9;
	double tof2OuterBSMD = rOuterBSMD/vInner;
	LOG_DEBUG<<" to BSMDouter: pos x,y,z:"<<outerBSMDPos.x()<<","<<outerBSMDPos.y()<<","<<outerBSMDPos.z()<<endm;
	LOG_DEBUG<<" to BSMDouter: mom p,pt,eta,phi:"<<outerBSMDMom.mag()<<","<<outerBSMDMom.perp()<<","<<outerBSMDMom.pseudoRapidity()<<","<<outerBSMDMom.phi()<<endm;
	LOG_DEBUG<<" to BSMDouter: tof:"<<tof2OuterBSMD<<endm;

	StThreeVector<double> EMCLayerPos = outerBSMDPos;
	StThreeVector<double> EMCLayerMom = outerBSMDMom;
	StThreeVector<double> tmpEMCLayerPos = outerBSMDPos;
	StThreeVector<double> tmpEMCLayerMom = outerBSMDMom;

	double rEMCStep = 5; //cm
	const int nEMCStep = 3;
	double EMClengthLayer[nEMCStep];
	double EMCtofLayer[nEMCStep];
	double vEMC;
	double betaGamEMC;
	for( int i=0; i<nEMCStep; i++){
		double EMCLayerRadius = outerBSMDRadius+rEMCStep*(i+1);
		StPhysicalHelixD helixInEMC(EMCLayerMom,EMCLayerPos,bField*tesla,gq);

		pairD sEMCLayer = helixInEMC.pathLength(EMCLayerRadius);
		if(sEMCLayer.first<=0 && sEMCLayer.second<=0) return kFALSE;

		double rEMCLayer = (sEMCLayer.first < 0 || sEMCLayer.second < 0) 
			? max(sEMCLayer.first, sEMCLayer.second) : min(sEMCLayer.first, sEMCLayer.second); 

		betaGamEMC 	= EMCLayerMom.mag()/muonMass;
		vEMC  	    = sqrt(betaGamEMC*betaGamEMC/(1.+betaGamEMC*betaGamEMC))*c_light*1e-9;
		EMClengthLayer[i]  = rEMCLayer;
		EMCtofLayer[i]     = rEMCLayer/vEMC;

		EMCLayerPos = helixInEMC.at(rEMCLayer);
		EMCLayerMom = helixInEMC.momentumAt(rEMCLayer,bField*tesla);
		EMCLayerMom=(sqrt(pow((sqrt(pow((abs(EMCLayerMom)),2)+pow(muonMass,2))-rEMCLayer*EMCenergyloss),2)-pow(muonMass,2)))/(abs(EMCLayerMom))*EMCLayerMom;
	}

	double tof2OuterEMC = 0.;
	for(int i=0;i<nEMCStep;i++) tof2OuterEMC += EMCtofLayer[i];
	LOG_DEBUG<<" to EMCouter: pos x,y,z:"<<EMCLayerPos.x()<<","<<EMCLayerPos.y()<<","<<EMCLayerPos.z()<<endm;
	LOG_DEBUG<<" to EMCouter: mom p,pt,eta,phi:"<<EMCLayerMom.mag()<<","<<EMCLayerMom.perp()<<","<<EMCLayerMom.pseudoRapidity()<<","<<EMCLayerMom.phi()<<endm;
	LOG_DEBUG<<" to EMCouter: tof:"<<tof2OuterEMC<<endm;

	//step 1: inner steel
	StPhysicalHelixD helixOutEMC(EMCLayerMom,EMCLayerPos,bField*tesla,gq);
	pairD sInnerSteel = helixOutEMC.pathLength(innerSteelRadius);
	if(sInnerSteel.first<=0 && sInnerSteel.second<=0) return kFALSE;
	double rInnerSteel =  (sInnerSteel.first < 0 || sInnerSteel.second < 0) 
		? max(sInnerSteel.first, sInnerSteel.second) : min(sInnerSteel.first, sInnerSteel.second); 
	StThreeVector<double> innerSteelPos = helixOutEMC.at(rInnerSteel);
	StThreeVector<double> innerSteelMom = helixOutEMC.momentumAt(rInnerSteel,bField*tesla);

	betaGam = innerSteelMom.mag()/muonMass;
	vInner  = sqrt(betaGam*betaGam/(1+betaGam*betaGam))*c_light*1e-9;
	double tof2InnerSteel = rInnerSteel/vInner;

	LOG_DEBUG<<" to InnerSteel: pos x,y,z:"<<innerSteelPos.x()<<","<<innerSteelPos.y()<<","<<innerSteelPos.z()<<endm;
	LOG_DEBUG<<" to InnerSteel: mom p,pt,eta,phi:"<<innerSteelMom.mag()<<","<<innerSteelMom.perp()<<","<<innerSteelMom.pseudoRapidity()<<","<<innerSteelMom.phi()<<endm;
	LOG_DEBUG<<" to InnerSteel: tof:"<<tof2InnerSteel<<endm;

	//step 2: outer steel, 10 steps for 60cm stell by 6 cm per step. 0.074GeV of energy loss for momentum degradation.
	double bFieldInSteel = bField/bFieldFF*bFieldInSteelFF;

	StThreeVector<double> steelLayerPos = innerSteelPos;
	StThreeVector<double> steelLayerMom = innerSteelMom;
	StThreeVector<double> tmpLayerPos = innerSteelPos;
	StThreeVector<double> tmpLayerMom = innerSteelMom;
	double eLoss = eLossInSteel;
	if(mCosmicFlag) eLoss *= -1.;
	double lengthLayer[nStep];
	double tofLayer[nStep];
	double vSteel;
	double betaGamSteel;
	double rStep = (outerSteelRadius-innerSteelRadius)/nStep; //cm
	for( int i=0; i<nStep; i++){
		double steelLayerRadius = innerSteelRadius+rStep*(i+1);
		StPhysicalHelixD helixInSteel(steelLayerMom,steelLayerPos,bFieldInSteel*tesla,gq);

		pairD sSteelLayer = helixInSteel.pathLength(steelLayerRadius);
		if(sSteelLayer.first<=0 && sSteelLayer.second<=0) return kFALSE;

		double rSteelLayer = (sSteelLayer.first < 0 || sSteelLayer.second < 0) 
			? max(sSteelLayer.first, sSteelLayer.second) : min(sSteelLayer.first, sSteelLayer.second); 

		betaGamSteel    = steelLayerMom.mag()/muonMass;
		vSteel  	    = sqrt(betaGamSteel*betaGamSteel/(1+betaGamSteel*betaGamSteel))*c_light*1e-9;
		lengthLayer[i]  = rSteelLayer;
		tofLayer[i]     = rSteelLayer/vSteel;

		steelLayerPos = helixInSteel.at(rSteelLayer);
		steelLayerMom = helixInSteel.momentumAt(rSteelLayer,bFieldInSteel*tesla);

		double momMag = abs(steelLayerMom);
		if(momMag<eLoss) return kFALSE;
		steelLayerMom = (momMag-eLoss)/momMag*steelLayerMom;
	}

	double tof2OuterSteel = 0.;
	for(int i=0;i<nStep;i++) tof2OuterSteel += tofLayer[i];
	LOG_DEBUG<<" to OuterSteel: pos x,y,z:"<<steelLayerPos.x()<<","<<steelLayerPos.y()<<","<<steelLayerPos.z()<<endm;
	LOG_DEBUG<<" to OuterSteel: mom p,pt,eta,phi:"<<steelLayerMom.mag()<<","<<steelLayerMom.perp()<<","<<steelLayerMom.pseudoRapidity()<<","<<steelLayerMom.phi()<<endm;
	LOG_DEBUG<<" to OuterSteel: tof:"<<tof2OuterSteel<<endm;
	//step 3: outside of magnetic
	const double bFieldOutSteel = 0.;
	StPhysicalHelixD helixOutSteel(steelLayerMom,steelLayerPos,bFieldOutSteel*tesla,gq);
	pairD sMtd = helixOutSteel.pathLength(mtdRadius);
	if(sMtd.first<=0 && sMtd.second<=0) return kFALSE;
	double rMtd =  (sMtd.first < 0 || sMtd.second < 0) 
		? max(sMtd.first, sMtd.second) : min(sMtd.first, sMtd.second); 

	StThreeVector<double> mtdPos = helixOutSteel.at(rMtd);
	StThreeVector<double> mtdMom = steelLayerMom;
	double betaGamOuter = steelLayerMom.mag()/muonMass;
	double vOuter  	    = sqrt(betaGamOuter*betaGamOuter/(1+betaGamOuter*betaGamOuter))*c_light*1e-9;
	double tof2Outer = rMtd/vOuter;

	LOG_DEBUG<<" to MTD: pos x,y,z:"<<mtdPos.x()<<","<<mtdPos.y()<<","<<mtdPos.z()<<endm;
	LOG_DEBUG<<" to MTD: mom p,pt,eta,phi:"<<mtdMom.mag()<<","<<mtdMom.perp()<<","<<mtdMom.pseudoRapidity()<<","<<mtdMom.phi()<<endm;
	LOG_DEBUG<<" to MTD: tof:"<<tof2Outer<<endm;


	double length2Tof = -9999.;
	length2Tof = length2Vtx+rTof;
	double length2Mtd[2] = {0};
	double length2SteelOuter = -9999.;
	length2SteelOuter += length2Vtx+rInnerEMC;
	length2SteelOuter += rInnerBSMD+rOuterBSMD;
	for(int i=0;i<nEMCStep;i++) length2SteelOuter += EMClengthLayer[i];
	length2SteelOuter += rInnerSteel;
	for(int i=0;i<nStep;i++) length2SteelOuter += lengthLayer[i];
	for(int i=0;i<2;i++) length2Mtd[i] = length2SteelOuter;

	double tof2Mtd[2] = {0};
	double tof2SteelOuter = -9999.;
	tof2SteelOuter += tof2InnerEMC; 
	tof2SteelOuter += tof2InnerBSMD+tof2OuterBSMD; 
	for(int i=0;i<nEMCStep;i++) tof2SteelOuter += EMCtofLayer[i];
	tof2SteelOuter += tof2InnerSteel; 
	for(int i=0;i<nStep;i++) tof2SteelOuter += tofLayer[i];
	for(int i=0;i<2;i++) tof2Mtd[i] = tof2SteelOuter;

	LOG_DEBUG<<" pathLength from vertex to MTD:"<<length2SteelOuter+rMtd<<endm;
	LOG_DEBUG<<" timeOfFlight from vertex to MTD:"<<tof2SteelOuter+tof2Outer<<endm;

	double projTofPhi = tofPos.phi();
	double projTofZ   = tofPos.z();

	if(projTofPhi<0) projTofPhi+=2*TMath::Pi();

	double projMtdPhi = mtdPos.phi();
	double projMtdZ   = mtdPos.z();

	if(projTofPhi<0) 	 projTofPhi 	+= 2.*(TMath::Pi()); // -pi,pi --> 0,2*pi
	if(projMtdPhi<0) 	 projMtdPhi 	+= 2.*(TMath::Pi()); // -pi,pi --> 0,2*pi

	double dphi = backLegPhiWidth+backLegPhiGap;

	int projMtdBackLeg = -1;
	projMtdBackLeg = (int)(projMtdPhi/dphi);
	projMtdBackLeg += 24;
	if(projMtdBackLeg>30) projMtdBackLeg -= 30;

	int projMtdModule = -1;
	projMtdModule = (int)((projMtdZ+2.5*stripLength)/stripLength+1);
	double projMtdModuleZcen = (projMtdModule-3.)*stripLength;

	StThreeVector<double> localnew[2]; //local vector of module
	StThreeVector<double> localnewn[2]; //normal of module plane
	int nghbrMtdModule = -1; //neighbor module id
	int moduleCan[2]; // candidate modules
	if(projMtdZ>projMtdModuleZcen) nghbrMtdModule = projMtdModule+1;
	else if(projMtdZ<=projMtdModuleZcen) nghbrMtdModule = projMtdModule-1;
	moduleCan[0]=projMtdModule;
	moduleCan[1]=nghbrMtdModule;
	for(Int_t i=0;i<2;i++){
		modulePos(projMtdBackLeg,moduleCan[i],localnew[i]);
		localnewn[i].setX(localnew[i].x());
		localnewn[i].setY(localnew[i].y());
		localnewn[i].setZ(0);
	}

	StThreeVector<double> mtdPosNew[2];
	double rMtdNew[2];
	for(int i=0;i<2;i++){
		rMtdNew[i] = helixOutSteel.pathLength(localnew[i],localnewn[i]);
		mtdPosNew[i] = helixOutSteel.at(rMtdNew[i]);
		length2Mtd[i] += rMtdNew[i];
		tof2Mtd[i] += rMtdNew[i]/vOuter;
	}

	StThreeVector<double> local[2];
	for(int i=0;i<2;i++){
		global2Local(mtdPosNew[i],projMtdBackLeg,moduleCan[i],local[i]);
	}

	int cellCan[2]={-1,-1};
	for(int i=0;i<2;i++){
		if(local[i].y()+(stripWidth+stripGap)*nStrips/2.>0){
		  cellCan[i]=(int)((local[i].y()+(stripWidth+stripGap)*nStrips/2.)/(stripWidth+stripGap));
		}
	}
	
	double LowEdge = mFirstBackLegPhi+(projMtdBackLeg-1.)*(backLegPhiWidth+backLegPhiGap)-(nChannels/4.)*(stripWidth+stripGap)/mtdRadius;
	if(LowEdge > 2.*(TMath::Pi())) LowEdge -= 2.*(TMath::Pi());
	if(LowEdge < 0) 	LowEdge += 2.*(TMath::Pi());
	double dCellPhi = projMtdPhi - LowEdge;
	if(dCellPhi<0) 		dCellPhi += 2.*(TMath::Pi());
	if(dCellPhi>2.*(TMath::Pi()))  dCellPhi -= 2.*(TMath::Pi());
	int projMtdCell = -1;
	projMtdCell	= (int)(dCellPhi/((stripWidth+stripGap)/mtdRadius));
	for(int i=0;i<2;i++){
		LOG_DEBUG<<"i "<<i<<" projMtdBackleg:"<<projMtdBackLeg<<" projMtdModule: "<<moduleCan[i]<<" projMtdCell: "<<cellCan[i]<<endm;
	}

	if(projMtdModule>3) projMtdCell = 11 - projMtdCell; // reversed 
	LOG_DEBUG<<"projMtdModule:"<<projMtdModule<<" projMtdCell:"<<projMtdCell<<" cellCan[0]:"<<cellCan[0]<<" cellCan[1]"<<cellCan[1]<<endm;
	Int_t nCells;

	StructCellHit cellHit;
	for(int i=0;i<2;i++){
		//if(moduleCan[i]<1||moduleCan[i]>5) continue;
		cellHit.backleg=projMtdBackLeg;
		cellHit.module=moduleCan[i];
		cellHit.cell=cellCan[i];
		cellHit.trackIdVec.push_back((Int_t)iNode);
		cellHit.hitPosition=mtdPosNew[i];
		cellHit.zhit=(Float_t)local[i].z();
		cellHit.yhit=(Float_t)local[i].y();
		cellHit.theta=(Double_t)length2Mtd[i];
		allCellsHitVec.push_back(cellHit);
		nCells++;
		if(mHisto) {
			Int_t channel = mMtdEvtData.gchannel[ngTracks];

			Double_t muTofPhi,muTofZ;
			int tray = channel/192;       //0-119
			int module = (channel%192)/6; //0-31
			int cell = (channel%192)%6;   //0-5
			//if(tray>119 || tray<0) return kFALSE;
			//if(module>31 || module<0)  return kFALSE;
			//if(cell>5 || cell<0) return kFALSE;

			int deg = 0;
			if(tray<60) deg = 72-tray*6;
			else 		deg = 108+tray*6;
			while(deg>=360) deg -= 360;
			while(deg<-360) deg += 360;
			double mTrayPhi = TMath::Pi()*deg/180.;
			while(mTrayPhi<0)     mTrayPhi += 2.*TMath::Pi();
			while(mTrayPhi>2.*TMath::Pi()) mTrayPhi -= 2.*TMath::Pi();

			double mCellPhi = 0.;
			if(tray<60) mCellPhi = mTrayPhi	- (2.5-cell)*mTofPadWidth/tofRadius;
			else		mCellPhi = mTrayPhi + (2.5-cell)*mTofPadWidth/tofRadius;
			double mModuleZ = 0.;
			if(tray<60) mModuleZ = mTofTrayZ0[tray]+mTofModuleLocalZ[module];
			else		mModuleZ = -mTofTrayZ0[tray]-mTofModuleLocalZ[module];

			muTofPhi = mCellPhi;
			muTofZ   = mModuleZ;
			if(muTofPhi<0) muTofPhi+=2*TMath::Pi();

			int hisIndex =  projMtdBackLeg - 1;
			mDaqOccupancyProj[hisIndex]->Fill((moduleCan[i]-1)*nTrays+cellCan[i]);
			mHitsPosition->Fill(mtdPosNew[i].y(), mtdPosNew[i].z());
			hphivsz->Fill(projMtdPhi,projMtdZ);
			hTofPhivsProj->Fill(muTofPhi,projTofPhi);
			hTofZvsProj->Fill(muTofZ,projTofZ);
		}
	}
	if(nCells>0&&mHisto) mHitsMultPerTrack->Fill(nCells);

	if(mSaveTree){
		mMtdEvtData.gdca[ngTracks] = dca.mag();	
		mMtdEvtData.gprojMtdBackLeg[ngTracks] = projMtdBackLeg;	
		mMtdEvtData.gprojMtdModule[ngTracks] = projMtdModule;	
		mMtdEvtData.gprojMtdCell[ngTracks] = projMtdCell;
		mMtdEvtData.gprojMtdPhi[ngTracks] = projMtdPhi;	
		mMtdEvtData.gprojMtdZ[ngTracks] = projMtdZ;	
		mMtdEvtData.gprojMtdLength[ngTracks] = length2Mtd[0];	
		mMtdEvtData.gprojTofLength[ngTracks] = length2Tof;	
		mMtdEvtData.gprojTofPhi[ngTracks] = projTofPhi;	
		mMtdEvtData.gprojTofZ[ngTracks] = projTofZ;	
		mMtdEvtData.gtof2Tof[ngTracks] = tof2Tof;	
		mMtdEvtData.gtof2Mtd[ngTracks] = tof2Mtd[0];	
	}
	return kTRUE;
}

/// Match StTrack-based track to MTD
bool StMtdMatchMaker::matchTrack2Mtd(mtdCellHitVector daqCellsHitVec,StTrack *theTrack, mtdCellHitVector& allCellsHitVec, unsigned int iNode, StThreeVectorD globalPos){
	int   gq		= theTrack->geometry()->charge();
	StPhysicalHelixD helix = theTrack->outerGeometry()->helix();
	return matchTrack2Mtd(daqCellsHitVec,helix,gq,allCellsHitVec,iNode,globalPos);

}
bool StMtdMatchMaker::matchTrack2Mtd(mtdCellHitVector daqCellsHitVec,StMuTrack *theTrack, mtdCellHitVector& allCellsHitVec, unsigned int iNode, StThreeVectorD globalPos){
	int   gq		= theTrack->charge();
	StPhysicalHelixD helix = theTrack->outerHelix();
	return matchTrack2Mtd(daqCellsHitVec,helix,gq,allCellsHitVec,iNode,globalPos);
}


/// Match the MTD hits
void StMtdMatchMaker::matchMtdHits(mtdCellHitVector& daqCellsHitVec,mtdCellHitVector& allCellsHitVec,mtdCellHitVector& matchHitCellsVec){
	StructCellHit cellHit;
	mtdCellHitVectorIter daqIter = daqCellsHitVec.begin();

	for(unsigned int idaq=0;idaq<daqCellsHitVec.size();idaq++, daqIter++) {
		mtdCellHitVectorIter proIter = allCellsHitVec.begin();
		for(unsigned int ipro=0;ipro<allCellsHitVec.size();ipro++, proIter++) {

			int daqIndex = (daqIter->module-1)*12 + (daqIter->cell);
			int proIndex = (proIter->module-1)*12 + (proIter->cell);
			int hisIndex = daqIter->backleg - 1;
			if (mHisto) {
				double backLegPhiCen = mFirstBackLegPhi+(daqIter->backleg-1)*(backLegPhiWidth+backLegPhiGap);
					if(backLegPhiCen>2.*TMath::Pi()) backLegPhiCen -= 2.*TMath::Pi();

					double stripPhiCen = 0.;
					int trayId=daqIter->module;
					int channel = daqIter->cell;
					if(trayId>0&&trayId<4){
						stripPhiCen = backLegPhiCen-(nChannels/4.-0.5-channel)*(stripWidth+stripGap)/mtdRadius; // approximation
					}else{
						stripPhiCen = backLegPhiCen+(nChannels/4.-0.5-channel)*(stripWidth+stripGap)/mtdRadius; 
					}
					double mLeTimeWest = daqIter->leadingEdgeTime.first;
					double mLeTimeEast = daqIter->leadingEdgeTime.second;
					double stripZCen   = (trayId-3.)*stripLength - (mLeTimeWest-mLeTimeEast)/2./vDrift*1000.;

					if(stripPhiCen>2.*TMath::Pi()) stripPhiCen -= 2.*TMath::Pi();
					if(stripPhiCen<0.)    stripPhiCen += 2.*TMath::Pi();
					double daqphi = stripPhiCen;
					double daqz   = stripZCen;

					hMtdZvsProj->Fill(proIter->hitPosition.z(),daqz);
					hMtdPhivsProj->Fill(proIter->hitPosition.phi(),daqphi);
					hMtddPhivsBackleg->Fill(hisIndex,proIter->hitPosition.phi()-daqphi);
					hMtddZvsBackleg->Fill(hisIndex,proIter->hitPosition.z()-daqz);

			}

			if(daqIter->backleg==proIter->backleg) {
				if (mHisto) {
					if(hisIndex>=0&&hisIndex<30) {
						mHitCorr[hisIndex]->Fill(proIndex,daqIndex);
						mHitCorrModule[hisIndex]->Fill(proIter->module-1,daqIter->module-1);
					} else {
						LOG_WARN << " weird tray # " << daqIter->module<< endm;
					}
				}
			}

			double p = 0.;
			int iNode = proIter->trackIdVec[0];
			if(mMuDstIn){
				StMuTrack *theTrack=mMuDst->globalTracks(iNode);
				p	= theTrack->momentum().mag();
			}else{
				StSPtrVecTrackNode& nodes=mEvent->trackNodes();
				StGlobalTrack *theTrack = dynamic_cast<StGlobalTrack*>(nodes[iNode]->track(global));
				p	= theTrack->geometry()->momentum().mag();
			}
			double zsig = fZReso->Eval(p);
			double phisig = fPhiReso->Eval(p);
			double ysig = proIter->module<4? phisig*mtdRadius:phisig*(mtdRadius+rModuleDiff);
			StThreeVector<double> hitPos;

			Int_t   ibackleg = daqIter->backleg;
			Int_t   imodule  = daqIter->module;
			Int_t   icell    = daqIter->cell;

			double zdaq = (daqIter->leadingEdgeTime.first-daqIter->leadingEdgeTime.second)/2./mVDrift[(ibackleg-1)*nTrays+imodule-1][icell]*1e3;
			double ydaq = (daqIter->cell-nStrips/2.+0.5)*(stripWidth+stripGap);
			if( (daqIter->backleg==proIter->backleg)&& 
					(daqIter->module==proIter->module)) {

				double rDiff = 99.;
				if(mCosmicFlag){
					rDiff = 0.;
					//rDiff = pow((ydaq-proIter->yhit)/mNSigReso/ysig,2.);
				}else{ 
					rDiff = pow((zdaq-proIter->zhit)/mNSigReso/zsig,2.)+pow((ydaq-proIter->yhit)/mNSigReso/ysig,2.);
				}
				//if(rDiff>1.) continue; // dr selection not use yet.
				cellHit.backleg = daqIter->backleg;
				cellHit.module = daqIter->module;
				cellHit.cell = daqIter->cell;
				cellHit.hitPosition = proIter->hitPosition;
				cellHit.trackIdVec = proIter->trackIdVec;
				cellHit.zhit = proIter->zhit;
				cellHit.yhit = proIter->yhit;
				cellHit.tot = daqIter->tot;
				cellHit.leadingEdgeTime = daqIter->leadingEdgeTime;
				cellHit.index2MtdHit = daqIter->index2MtdHit;
				cellHit.theta = proIter->theta;
				cellHit.pathLength = proIter->pathLength;
				matchHitCellsVec.push_back(cellHit);
			}
		} 
	} //end {sec. C}'
}


/// Loop over and identify single and multiple-hit cells
void StMtdMatchMaker::sortSingleAndMultiHits(mtdCellHitVector& matchHitCellsVec,mtdCellHitVector& singleHitCellsVec,mtdCellHitVector& multiHitsCellsVec){
	Int_t nSingleHitCells(0);
	Int_t nMultiHitsCells(0);
	StructCellHit cellHit;
	mtdCellHitVector tempVec = matchHitCellsVec;
	mtdCellHitVector erasedVec = tempVec;
	while (tempVec.size() != 0) {
		Int_t nTracks = 0;
		idVector trackIdVec;

		mtdCellHitVectorIter tempIter=tempVec.begin();
		mtdCellHitVectorIter erasedIter=erasedVec.begin();
		while(erasedIter!= erasedVec.end()) {
			if(tempIter->backleg == erasedIter->backleg &&
					tempIter->module == erasedIter->module &&
					tempIter->cell == erasedIter->cell) {
				nTracks++;
				trackIdVec.push_back(erasedIter->trackIdVec.back());  // merge
				erasedVec.erase(erasedIter);
				erasedIter--;
			}
			erasedIter++;
		}

		cellHit.cell = tempIter->cell;
		cellHit.module = tempIter->module;
		cellHit.backleg = tempIter->backleg;
		cellHit.hitPosition = tempIter->hitPosition;
		cellHit.trackIdVec = trackIdVec;
		cellHit.zhit = tempIter->zhit;
		cellHit.yhit = tempIter->yhit;
		cellHit.tot = tempIter->tot;
		cellHit.leadingEdgeTime = tempIter->leadingEdgeTime;
		cellHit.index2MtdHit = tempIter->index2MtdHit;
		cellHit.theta = tempIter->theta;
		cellHit.pathLength = tempIter->pathLength;

		if(mHisto) {
			Float_t ycenter = (tempIter->cell-nStrips/2+0.5)*(stripWidth+stripGap);
			Float_t dy = tempIter->yhit - ycenter;
			Float_t dz = tempIter->zhit;
			mTracksPerCellMatch1->Fill(trackIdVec.size());
			mDaqOccupancyMatch1->Fill((tempIter->module-1)*nStrips+tempIter->cell);
			mDeltaHitMatch1->Fill(dy, dz);
		}

		if (nTracks==1){
			nSingleHitCells++;      
			singleHitCellsVec.push_back(cellHit);
		} else if (nTracks>1){
			nMultiHitsCells++;
			multiHitsCellsVec.push_back(cellHit);
			// for multiple hit cells either discard (yes) or
			// find the most likely candidate.
		} else {
			LOG_WARN << "D: no tracks extrapolate to matched cell ... should not happen!" << endm;
		}
		if(Debug()) { 
			LOG_DEBUG << "D: backleg=" << cellHit.backleg << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:";
			idVectorIter ij=trackIdVec.begin();
			while (ij != trackIdVec.end()) { LOG_DEBUG << " " << *ij; ij++; }
			LOG_DEBUG << endm;
		}
		tempVec = erasedVec;
	}
}


/// Sort and in the case of tracks with multiple matched cells pick the best candidate and set the MatchFlag
void StMtdMatchMaker::finalMatchedMtdHits(mtdCellHitVector& singleHitCellsVec,mtdCellHitVector& finalMatchedCellsVec){
	mtdCellHitVector tempVec = singleHitCellsVec;

	if(mHisto) {
		mCellsPerEventMatch2->Fill(tempVec.size());
		for(unsigned int ii=0;ii<tempVec.size();ii++) {
			mTracksPerCellMatch2->Fill(tempVec[ii].trackIdVec.size());
			mDaqOccupancyMatch2->Fill((tempVec[ii].module-1)*nStrips+tempVec[ii].cell);
			Float_t ycenter = (tempVec[ii].cell-nStrips/2+0.5)*(stripWidth+stripGap);
			Float_t dy = tempVec[ii].yhit-ycenter;
			Float_t dz = tempVec[ii].zhit;
			mDeltaHitMatch2->Fill(dy, dz);
		}
	}

	mtdCellHitVector erasedVec = tempVec;

	while (tempVec.size() != 0) {
		StructCellHit cellHit;
		Int_t nCells = 0;
		idVector vTrackId;
		vector<StThreeVectorD> vPosition;
		vector<Int_t> vchannel, vbackleg, vmodule, vcell;
		vector<Float_t> vzhit, vyhit;
		vector<pairD> vtot; 
		vector<pairD> vtdc; 
		vector<Double_t> vtheta;
		vector<Double_t> vpathLength;
		vector<Int_t> vindex2MtdHit;

		mtdCellHitVectorIter tempIter=tempVec.begin();
		mtdCellHitVectorIter erasedIter=erasedVec.begin();
		while(erasedIter!= erasedVec.end()) {
			if(tempIter->trackIdVec.back() == erasedIter->trackIdVec.back()) {
				nCells++;
				vbackleg.push_back(erasedIter->backleg);
				vmodule.push_back(erasedIter->module);
				vcell.push_back(erasedIter->cell);
				vPosition.push_back(erasedIter->hitPosition);
				vTrackId.push_back(erasedIter->trackIdVec.back());
				vzhit.push_back(erasedIter->zhit);
				vyhit.push_back(erasedIter->yhit);
				vtot.push_back(erasedIter->tot);
				vtdc.push_back(erasedIter->leadingEdgeTime);
				vindex2MtdHit.push_back(erasedIter->index2MtdHit);
				vtheta.push_back(erasedIter->theta);
				vpathLength.push_back(erasedIter->pathLength);

				erasedVec.erase(erasedIter);
				erasedIter--;
			}
			erasedIter++;
		}

		if (nCells==1){
			// for singly hit cell, copy data in singleHitCellsVec
			cellHit.backleg = vbackleg[0];
			cellHit.module = vmodule[0];
			cellHit.cell = vcell[0];
			cellHit.trackIdVec.push_back(vTrackId[0]);
			cellHit.hitPosition = vPosition[0];
			cellHit.matchFlag = 1; 
			cellHit.zhit = vzhit[0];
			cellHit.yhit = vyhit[0];
			cellHit.tot = vtot[0];
			cellHit.leadingEdgeTime = vtdc[0];
			cellHit.index2MtdHit = vindex2MtdHit[0];
			cellHit.theta = vtheta[0];
			cellHit.pathLength = vpathLength[0];

			finalMatchedCellsVec.push_back(cellHit);

			// debugging output
			if(Debug()) {
				LOG_DEBUG << "E: ibackleg=" << cellHit.backleg << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:";
				idVectorIter ij=vTrackId.begin();
				while (ij != vTrackId.end()) { LOG_DEBUG << " " << *ij; ij++; }
				LOG_DEBUG << endm;
			}

		}
		else if (nCells>1){   // for multiple hit cells  find the most likely candidate.
			Int_t thiscandidate(-99);
			Int_t thisMatchFlag(0);

			// sort on tot
			pair<Double_t,Double_t> tot(0.,0.);
			vector<Int_t> ttCandidates;
			for (Int_t i=0;i<nCells;i++) {
				pair<Double_t,Double_t> tt = vtot[i];
				if(tt.first<40.&&tt.first>tot.first) {    // open the ToT cut to 40 ns
					tot = tt;
					ttCandidates.clear();
					ttCandidates.push_back(i);
				} else if (tt.first==tot.first) {
					ttCandidates.push_back(i);
				}
			}
			if (ttCandidates.size()==1) {
				thiscandidate = ttCandidates[0];
				thisMatchFlag = 2;
			} else if (ttCandidates.size()>1) {  // sort on hitposition
				Float_t ss(999.);
				vector<Int_t> ssCandidates;
				for(size_t j=0;j<ttCandidates.size();j++) {
					Float_t yy = vyhit[ttCandidates[j]];
					Float_t ycell = (vcell[ttCandidates[j]]-nStrips/2+0.5)*(stripWidth+stripGap);
					Float_t ll = fabs(yy-ycell);
					Float_t mLeadingWest = vtdc[ttCandidates[j]].first;
					Float_t mLeadingEast = vtdc[ttCandidates[j]].second;
					Int_t   ibackleg = vbackleg[ttCandidates[j]];
					Int_t   imodule = vmodule[ttCandidates[j]];
					Int_t   icell = vcell[ttCandidates[j]];
					Float_t zcell = (mLeadingEast- mLeadingWest)/2./mVDrift[(ibackleg-1)*nTrays+imodule-1][icell]*1e3;

					Float_t zz = vzhit[ttCandidates[j]];
					Float_t ww = fabs(zz-zcell);
					Float_t rr = 9999.;
					if(mCosmicFlag) rr = ll;
					else rr = sqrt(ll*ll+ww*ww);
					if(rr<ss) {
						ss = rr; 
						ssCandidates.clear();
						ssCandidates.push_back(ttCandidates[j]);
					}else if  (rr==ss)
						ssCandidates.push_back(ttCandidates[j]);
				}
				if (ssCandidates.size()==1){
					thiscandidate = ssCandidates[0];
					thisMatchFlag = 3;
				}
			}

			if (thiscandidate>=0) {
				cellHit.backleg = vbackleg[thiscandidate];
				cellHit.module = vmodule[thiscandidate];
				cellHit.cell = vcell[thiscandidate];
				cellHit.trackIdVec.push_back(vTrackId[thiscandidate]);
				cellHit.hitPosition = vPosition[thiscandidate];
				cellHit.matchFlag = thisMatchFlag;
				cellHit.zhit = vzhit[thiscandidate];
				cellHit.yhit = vyhit[thiscandidate];
				cellHit.tot = vtot[thiscandidate];
				cellHit.leadingEdgeTime = vtdc[thiscandidate];
				cellHit.index2MtdHit = vindex2MtdHit[thiscandidate];
				cellHit.theta = vtheta[thiscandidate];
				cellHit.pathLength = vpathLength[thiscandidate];

				finalMatchedCellsVec.push_back(cellHit);

				// debugging output
				if(Debug()) { LOG_DEBUG << "E: ibackleg=" << cellHit.backleg << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:" << vTrackId[thiscandidate] << endm; }
			}

		} else {
			LOG_WARN << "E: no cells belong to this track ... should not happen!" << endm;
		}

		tempVec = erasedVec;
	}

	// end of Sect.E
}


/// Take final matched MTD hits and update the track PID traits with MTD information
void StMtdMatchMaker::fillPidTraits(mtdCellHitVector& finalMatchedCellsVec,Int_t& nValidSingleHitCells,Int_t& nValidSinglePrimHitCells){
	for (size_t ii=0; ii < finalMatchedCellsVec.size(); ii++){
		Int_t backleg = finalMatchedCellsVec[ii].backleg;
		Int_t module = finalMatchedCellsVec[ii].module;
		Int_t cell = finalMatchedCellsVec[ii].cell;

		if (finalMatchedCellsVec[ii].trackIdVec.size()!=1)
			LOG_WARN << "F: WHAT!?!  mult.matched cell in single cell list " << backleg << " " << module << " " << cell << endm;

		Float_t ycenter = (cell-nStrips/2+0.5)*(stripWidth+stripGap);
		Float_t dy = finalMatchedCellsVec[ii].yhit - ycenter;
		Float_t dz = finalMatchedCellsVec[ii].zhit;
		if(mHisto) {
			mTracksPerCellMatch3->Fill(finalMatchedCellsVec[ii].trackIdVec.size());
			//      mDaqOccupancyMatch3->Fill((module-1)*mNCell+(cell-1));
			mDeltaHitMatch3->Fill(dy, dz);
			int hisIndex =  backleg-1;
			mDeltaHitFinal[hisIndex]->Fill(dy,dz);      
		}

		// get track-id from cell hit vector
		int trackNode = finalMatchedCellsVec[ii].trackIdVec[0];
		if(mMuDstIn){
			StMuTrack *gTrack = mMuDst->globalTracks(trackNode);
			StMuTrack *pTrack = (StMuTrack*)gTrack->primaryTrack();
			if(!gTrack) {
				LOG_WARN << "Wrong global track!" << endm;
				continue;
			}
			StMuMtdHit *mtdHit = mMuDst->mtdHit(finalMatchedCellsVec[ii].index2MtdHit);
			if(mtdHit->backleg()!=backleg || mtdHit->module()!=module || mtdHit->cell()!=cell) {
				LOG_WARN << "Wrong hit in the MtdHitCollection!" << endm;
				continue;
			}
			nValidSingleHitCells++;

			mtdHit->setAssociatedTrackKey(gTrack->id());

			StMuMtdPidTraits pidMtd = gTrack->mtdPidTraits();
			pidMtd.setMatchFlag(finalMatchedCellsVec[ii].matchFlag);
			pidMtd.setYLocal(dy);
			pidMtd.setZLocal(dz);
			pidMtd.setThetaLocal(finalMatchedCellsVec[ii].theta);
			pidMtd.setPosition(finalMatchedCellsVec[ii].hitPosition);
			pidMtd.setPathLength(finalMatchedCellsVec[ii].pathLength);
			gTrack->setMtdPidTraits(pidMtd);

			if(pTrack){
				StMuMtdPidTraits ppidMtd = pTrack->mtdPidTraits();
				ppidMtd.setMatchFlag(finalMatchedCellsVec[ii].matchFlag);
				ppidMtd.setYLocal(dy);
				ppidMtd.setZLocal(dz);
				ppidMtd.setThetaLocal(finalMatchedCellsVec[ii].theta);
				ppidMtd.setPosition(finalMatchedCellsVec[ii].hitPosition);
				ppidMtd.setPathLength(finalMatchedCellsVec[ii].pathLength);
				pTrack->setMtdPidTraits(ppidMtd);
			}

		}else{
			// get track-id from cell hit vector
			StSPtrVecTrackNode &nodes = mEvent->trackNodes();
			StGlobalTrack *globalTrack = dynamic_cast<StGlobalTrack*>(nodes[trackNode]->track(global));
			StPrimaryTrack *primaryTrack =dynamic_cast<StPrimaryTrack*>(globalTrack->node()->track(primary));
			if(!globalTrack) {
				LOG_WARN << "Wrong global track!" << endm;
				continue;
			}

			// Fill association in MTD Hit Collection

			StMtdCollection *theMtd  = mEvent->mtdCollection();
			StSPtrVecMtdHit& mtdHits = theMtd->mtdHits();
			StMtdHit *mtdHit = mtdHits[finalMatchedCellsVec[ii].index2MtdHit];
			if(mtdHit->backleg()!=backleg || mtdHit->module()!=module || mtdHit->cell()!=cell) {
				LOG_WARN << "Wrong hit in the MtdHitCollection!" << endm;
				continue;
			}
			nValidSingleHitCells++;

			mtdHit->setAssociatedTrack(globalTrack);

			StMtdPidTraits *pidMtd = new StMtdPidTraits();
			pidMtd->setMtdHit(mtdHit);
			pidMtd->setMatchFlag(finalMatchedCellsVec[ii].matchFlag);
			pidMtd->setYLocal(dy);
			pidMtd->setZLocal(dz);
			pidMtd->setThetaLocal(finalMatchedCellsVec[ii].theta);
			pidMtd->setPosition(finalMatchedCellsVec[ii].hitPosition);
			pidMtd->setPathLength(finalMatchedCellsVec[ii].pathLength);
			globalTrack->addPidTraits(pidMtd);

			if(primaryTrack){
				StMtdPidTraits *ppidMtd = new StMtdPidTraits();
				ppidMtd->setMtdHit(mtdHit);
				ppidMtd->setMatchFlag(finalMatchedCellsVec[ii].matchFlag);
				ppidMtd->setYLocal(dy);
				ppidMtd->setZLocal(dz);
				ppidMtd->setThetaLocal(finalMatchedCellsVec[ii].theta);
				ppidMtd->setPosition(finalMatchedCellsVec[ii].hitPosition);
				ppidMtd->setPathLength(finalMatchedCellsVec[ii].pathLength);
				primaryTrack->addPidTraits(ppidMtd);
			}
		}

		if(mSaveTree){
			int iNode = trackNode;	
			int iTrk;
			for(iTrk=0;iTrk<ngTracks;iTrk++){
				if(mMtdEvtData.gtrackId[iTrk] == iNode) break;
			}
			mMtdEvtData.gnMatchMtdHits[iTrk] = 1;
			mMtdEvtData.gmMtdHitIndex[iTrk] = finalMatchedCellsVec[ii].index2MtdHit;

			mMtdEvtData.gmBackLeg[iTrk] = backleg;
			mMtdEvtData.gmModule[iTrk] = module;
			mMtdEvtData.gmCell[iTrk] = cell;
			mMtdEvtData.gmLeTimeWest[iTrk] = finalMatchedCellsVec[ii].leadingEdgeTime.first;
			mMtdEvtData.gmLeTimeEast[iTrk] = finalMatchedCellsVec[ii].leadingEdgeTime.second;
			mMtdEvtData.gmTotWest[iTrk] = finalMatchedCellsVec[ii].tot.first;
			mMtdEvtData.gmTotEast[iTrk] = finalMatchedCellsVec[ii].tot.second;
			mMtdEvtData.gmLocalZ[iTrk] = finalMatchedCellsVec[ii].zhit;
			mMtdEvtData.gmLocalY[iTrk] = finalMatchedCellsVec[ii].yhit;
		}

	}
}

/// StTrack-based Track selection for initial extrapolation to MTD
bool StMtdMatchMaker::validTrack(StTrack *track){
	return validTrack(MtdTrack(track));
}

/// StMuTrack-based Track selection for initial extrapolation to MTD
bool StMtdMatchMaker::validTrack(StMuTrack *track){
	return validTrack(MtdTrack(track));
}

/// MtdTrack-based Track selection for initial extrapolation to MTD
bool StMtdMatchMaker::validTrack(MtdTrack mtt){
	/// 1. no track, no go.
	float gpt 		= mtt.pt;
	float geta 		= mtt.eta;
	int   gnFtPts 	= mtt.nFtPts;
	int   gnDedxPts	= mtt.nDedxPts;
	if(gnDedxPts<mMindEdxFitPoints) return kFALSE;
	if(geta<mMinEta||geta>mMaxEta) return kFALSE;
	if(gpt<mMinPt) return kFALSE;
	/// 2. track quality flag, should be >0
	if (mtt.flag<=0 || mtt.flag>=1000) return kFALSE;

	/// 3. minimum #hits per track - obsolete
	//  if (mtt->topologyMap().numberOfHits(kTpcId) < mMinHitsPerTrack) return kFALSE;
	/// 4. minimum #fit points per track
	if (gnFtPts < mMinFitPointsPerTrack) return kFALSE;
	/// 5. minimum #fit points over #maximum points
	//fg float ratio = (1.0*mtt->fitTraits().numberOfFitPoints(kTpcId)) / (1.0*mtt->numberOfPossiblePoints(kTpcId));
	float ratio = gnFtPts / (1.0*mtt.nHitsPoss);
	if (ratio < mMinFitPointsOverMax) return kFALSE;

	return kTRUE;
}


/// Transform global (extrapolation) coordinates to local module coordinate
void StMtdMatchMaker::global2Local(StThreeVector<double> global,int backleg,int module,StThreeVector<double>& local){
	double r,theta,z;
	if(module%2){
		r=mtdRadius+rModuleDiff;
	}
	else{
		r=mtdRadius;
	}	
	z=(module-3)*stripLength;
	theta=(backleg-1)*(backLegPhiWidth+backLegPhiGap)+(TMath::Pi())/2.0;
	while(theta<0) theta+=(TMath::Pi())*2.0;
	while(theta>2*(TMath::Pi())) theta-=(TMath::Pi())*2.0;
	double local_z=global.z()-z;
	double medi_x=global.x()-r*(TMath::Cos(theta));
	double medi_y=global.y()-r*(TMath::Sin(theta));
	double local_x=medi_x*(TMath::Cos(-theta))-medi_y*(TMath::Sin(-theta));
	double local_y=medi_x*(TMath::Sin(-theta))+medi_y*(TMath::Cos(-theta));
	if(module>3) local_y = -local_y;
	local.setX(local_x);
	local.setY(local_y);
	local.setZ(local_z);
}


/// calculate module position from DAQ coordinates
void StMtdMatchMaker::modulePos(int backleg,int module,StThreeVector<double>& local){
	double r,theta,z;
	if(module%2){
		r=mtdRadius+rModuleDiff;
	}
	else{
		r=mtdRadius;
	}	
	z=(module-3)*stripLength;
	theta=(backleg-1)*(backLegPhiWidth+backLegPhiGap)+(TMath::Pi())/2.0;
	while(theta<0) theta+=(TMath::Pi())*2.0;
	while(theta>2*(TMath::Pi())) theta-=(TMath::Pi())*2.0;
	double medi_z=z;
	double medi_x=r*(TMath::Cos(theta));
	double medi_y=r*(TMath::Sin(theta));
	local.setX(medi_x);
	local.setY(medi_y);
	local.setZ(medi_z);
}

/// initialize MTD Event Data
void StMtdMatchMaker::initEventData(){
	memset(&mMtdEvtData,0,sizeof(mMtdEvtData));
}
//___________________________________________________

MtdTrack::MtdTrack(StTrack *stt){

	if(stt){
		pt 		= stt->geometry()->momentum().perp();
		eta 	= stt->geometry()->momentum().pseudoRapidity();
		nFtPts 	= stt->fitTraits().numberOfFitPoints(kTpcId);
		static StTpcDedxPidAlgorithm PidAlgorithm;
		const StParticleDefinition* pd=stt->pidTraits(PidAlgorithm);
		if(pd){
			if(PidAlgorithm.traits()){
				nDedxPts=PidAlgorithm.traits()->numberOfPoints();
			}
		}
		flag        = stt->flag();
		nHitsPoss	= stt->numberOfPossiblePoints(kTpcId);
	}else{
		pt = -999.; eta = -999.; nFtPts = 0;
		nDedxPts = 0; flag = 0; nHitsPoss = 999;
	}
}
//------------------------------------------------
MtdTrack::MtdTrack(StMuTrack *mut){

	if(mut){
		pt 		= mut->momentum().perp();
		eta 	= mut->momentum().pseudoRapidity();
		nFtPts 	= mut->nHitsFit(kTpcId);
		nDedxPts	= mut->nHitsDedx();
		flag        = mut->flag();
		nHitsPoss	= mut->nHitsPoss(kTpcId);
	}else{
		pt = -999.; eta = -999.; nFtPts = 0;
		nDedxPts = 0; flag = 0; nHitsPoss = 999;
	}
}
