/*******************************************************************
 * $Id: StMtdMatchMaker.cxx,v 1.20 2014/08/18 17:44:45 marr Exp $
 * Author: Bingchu Huang
 *****************************************************************
 *
 * Description: BTof Match Maker to do the matching between the 
 *              fired celles and TPC tracks
 *
 *****************************************************************
 *
 * $Log: StMtdMatchMaker.cxx,v $
 * Revision 1.20  2014/08/18 17:44:45  marr
 * Add assert() statement to abort if the loaded geometry is wrong
 *
 * Revision 1.19  2014/07/29 19:23:47  marr
 * Remove the dependency on "StarGenerator/StarLight/starlightconstants.h" as it is not needed anymore.
 *
 * Revision 1.18  2014/07/24 02:53:04  marr
 * 1) Add log info of the matched track-hit pair
 * 2) Set DeltaY and DeltaZ in PidTraits
 *
 * Revision 1.17  2014/07/18 15:52:00  marr
 * Initialize trgTime[2]
 *
 * Revision 1.16  2014/07/16 15:43:53  huangbc
 * use mMtdGeom->SetLockBField();
 *
 * Revision 1.15  2014/07/15 02:01:04  huangbc
 * Implement multi-tracks to 1 hit matching algo. Set neighbor module matching and 3 extra cells as default.
 *
 * Revision 1.14  2014/07/10 20:50:35  huangbc
 * Use new MTD geometry class. Load geometry volume from geant.
 * Choose closest one for multi-tracks which associated with same hit.
 * Remove dca cut
 *
 * Revision 1.13  2014/07/08 03:09:07  huangbc
 * Change dca<10 to dca2Beam_R<10.
 *
 * Revision 1.12  2014/06/19 19:16:27  huangbc
 * Fixed an issue of reading SL12d production data. Add expTof for MTD pidtraits.
 *
 * Revision 1.11  2014/04/16 02:23:39  huangbc
 * 1. fixed a bug of un-initialized variable nDedxPts in MtdTrack construction function.
 * 2. reoriganized project2Mtd function. Made it more readable.
 * 3. save pathlengths of extrapolated tracks.
 * 4. tot selection < 40 ns. drop off maximum tot selection.
 * 5. add hits <-> track index association in mMuDstIn=true mode.
 *
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
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuMtdPidTraits.h"

#include "StEventMaker/StEventMaker.h"
#include "StAssociationMaker/StAssociationMaker.h"
#include "StMcEventMaker/StMcEventMaker.h"
#include "StDetectorDbMaker/St_MagFactorC.h"
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
#include "StMtdPidTraits.h"

#include "StMtdUtil/StMtdGeometry.h"
#include "StMtdMatchMaker.h"

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
	mELossFlag=kTRUE;
	mLockBField = kFALSE;
	mMtdGeom = 0;

	mnNeighbors = kTRUE;
	mNExtraCells = 3;
	//mZLocalCut = 43.5;
	mNSigReso = 3.; // n sigma of z and y resolution.
	mSaveTree = kFALSE;
	mHisto = kFALSE;
	ngTracks = 0;
	mEvent = NULL;
	mMuDst = NULL;
	mMtdEvt = NULL;
	mGeomTag = "y2014a";

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
	mMtdEvt->Branch("triggerTime",&mMtdEvtData.triggerTime,"triggerTime[2]/D");
	mMtdEvt->Branch("vpdVz",&mMtdEvtData.vpdVz,"vpdVz/F");
	mMtdEvt->Branch("tStart",&mMtdEvtData.tStart,"tStart/F");
	mMtdEvt->Branch("refMult",&mMtdEvtData.refMult,"refMult/I");

	/// raw hits
	mMtdEvt->Branch("nMtdRawHits",&mMtdEvtData.nMtdRawHits,"nMtdRawHits/I");
	mMtdEvt->Branch("flag",&mMtdEvtData.flag,"flag[nMtdRawHits]/B");
	mMtdEvt->Branch("backlegRaw",&mMtdEvtData.backlegRaw,"backlegRaw[nMtdRawHits]/b");
	mMtdEvt->Branch("chn",&mMtdEvtData.chn,"chn[nMtdRawHits]/b");
	mMtdEvt->Branch("tdc",&mMtdEvtData.tdc,"tdc[nMtdRawHits]/D");

	/// sorted hits
	mMtdEvt->Branch("nMtdHits",&mMtdEvtData.nMtdHits,"nMtdHits/I");
	mMtdEvt->Branch("backleg",&mMtdEvtData.backleg,"backleg[nMtdHits]/b");
	mMtdEvt->Branch("module",&mMtdEvtData.module,"module[nMtdHits]/b");
	mMtdEvt->Branch("cell",&mMtdEvtData.cell,"cell[nMtdHits]/b");

	mMtdEvt->Branch("leTimeWest",&mMtdEvtData.leTimeWest,"leTimeWest[nMtdHits]/D");
	mMtdEvt->Branch("leTimeEast",&mMtdEvtData.leTimeEast,"leTimeEast[nMtdHits]/D");
	mMtdEvt->Branch("totWest",&mMtdEvtData.totWest,"totWest[nMtdHits]/D");
	mMtdEvt->Branch("totEast",&mMtdEvtData.totEast,"totEast[nMtdHits]/D");

	/// global tracks
	mMtdEvt->Branch("ngTrks",&mMtdEvtData.ngTrks,"ngTrks/I");
	mMtdEvt->Branch("gpt",&mMtdEvtData.gpt,"gpt[ngTrks]/F");
	mMtdEvt->Branch("geta",&mMtdEvtData.geta,"geta[ngTrks]/F");
	mMtdEvt->Branch("gphi",&mMtdEvtData.gphi,"gphi[ngTrks]/F");
	mMtdEvt->Branch("ppt",&mMtdEvtData.ppt,"ppt[ngTrks]/F");
	mMtdEvt->Branch("peta",&mMtdEvtData.peta,"peta[ngTrks]/F");
	mMtdEvt->Branch("pphi",&mMtdEvtData.pphi,"pphi[ngTrks]/F");

	mMtdEvt->Branch("ghelixpx",&mMtdEvtData.ghelixpx,"ghelixpx[ngTrks]/F");
	mMtdEvt->Branch("ghelixpy",&mMtdEvtData.ghelixpy,"ghelixpy[ngTrks]/F");
	mMtdEvt->Branch("ghelixpz",&mMtdEvtData.ghelixpz,"ghelixpz[ngTrks]/F");
	mMtdEvt->Branch("ghelixox",&mMtdEvtData.ghelixox,"ghelixox[ngTrks]/F");
	mMtdEvt->Branch("ghelixoy",&mMtdEvtData.ghelixoy,"ghelixoy[ngTrks]/F");
	mMtdEvt->Branch("ghelixoz",&mMtdEvtData.ghelixoz,"ghelixoz[ngTrks]/F");

	mMtdEvt->Branch("gdedx",&mMtdEvtData.gdedx,"gdedx[ngTrks]/F");
	mMtdEvt->Branch("gnSigmaPi",&mMtdEvtData.gnSigmaPi,"gnSigmaPi[ngTrks]/F");
	mMtdEvt->Branch("gnSigmaK",&mMtdEvtData.gnSigmaK,"gnSigmaK[ngTrks]/F");
	mMtdEvt->Branch("gnSigmaP",&mMtdEvtData.gnSigmaP,"gnSigmaP[ngTrks]/F");
	mMtdEvt->Branch("gnSigmaE",&mMtdEvtData.gnSigmaE,"gnSigmaE[ngTrks]/F");
	mMtdEvt->Branch("gq",&mMtdEvtData.gq,"gq[ngTrks]/B");
	mMtdEvt->Branch("gtrackId",&mMtdEvtData.gtrackId,"gtrackId[ngTrks]/I");
	mMtdEvt->Branch("gIndex2Primary",&mMtdEvtData.gIndex2Primary,"gIndex2Primary[ngTrks]/I");
	mMtdEvt->Branch("gnFtPts",&mMtdEvtData.gnFtPts,"gnFtPts[ngTrks]/B");
	mMtdEvt->Branch("gnDedxPts",&mMtdEvtData.gnDedxPts,"gnDedxPts[ngTrks]/B");

	mMtdEvt->Branch("gchannel",&mMtdEvtData.gchannel,"gchannel[ngTrks]/I");
	mMtdEvt->Branch("gyLocal",&mMtdEvtData.gyLocal,"gyLocal[ngTrks]/F");
	mMtdEvt->Branch("gzLocal",&mMtdEvtData.gzLocal,"gzLocal[ngTrks]/F");
	mMtdEvt->Branch("gtdc",&mMtdEvtData.gtdc,"gtdc[ngTrks]/F");
	mMtdEvt->Branch("gtot",&mMtdEvtData.gtot,"gtot[ngTrks]/F");
	mMtdEvt->Branch("gtof",&mMtdEvtData.gtof,"gtof[ngTrks]/F");
	mMtdEvt->Branch("gpathLength",&mMtdEvtData.gpathLength,"gpathLength[ngTrks]/F");
	mMtdEvt->Branch("gbeta",&mMtdEvtData.gbeta,"gbeta[ngTrks]/F");
	mMtdEvt->Branch("gtdiff",&mMtdEvtData.gtdiff,"gtdiff[ngTrks]/F");


	/// project to MTD
	mMtdEvt->Branch("gdca",&mMtdEvtData.gdca,"gdca[ngTrks]/F");
	mMtdEvt->Branch("gtrackindex",&mMtdEvtData.gtrackindex,"gtrackindex[ngTrks]/I");//
	mMtdEvt->Branch("gTrkMatchNum",&mMtdEvtData.gTrkMatchNum,"gTrkMatchNum[ngTrks]/I");

	mMtdEvt->Branch("gprojMtdBackLeg",&mMtdEvtData.gprojMtdBackLeg,"gprojMtdBackLeg[ngTrks][2]/b");
	mMtdEvt->Branch("gprojMtdModule",&mMtdEvtData.gprojMtdModule,"gprojMtdModule[ngTrks][2]/b");
	mMtdEvt->Branch("gprojMtdCell",&mMtdEvtData.gprojMtdCell,"gprojMtdCell[ngTrks][2]/b");
	mMtdEvt->Branch("gprojMtdPhi",&mMtdEvtData.gprojMtdPhi,"gprojMtdPhi[ngTrks][2]/F");
	mMtdEvt->Branch("gprojMtdZ",&mMtdEvtData.gprojMtdZ,"gprojMtdZ[ngTrks][2]/F");
	mMtdEvt->Branch("gprojMtdLength",&mMtdEvtData.gprojMtdLength,"gprojMtdLength[ngTrks][2]/F");
	mMtdEvt->Branch("gtof2Mtd",&mMtdEvtData.gtof2Mtd,"gtof2Mtd[ngTrks][2]/F");


	/// Matched Mtd Hits
	mMtdEvt->Branch("gnMatchMtdHits",&mMtdEvtData.gnMatchMtdHits,"gnMatchMtdHits[ngTrks]/I");
	mMtdEvt->Branch("gmMtdHitIndex",&mMtdEvtData.gmMtdHitIndex,"gmMtdHitIndex[ngTrks]/I");
	mMtdEvt->Branch("gmBackLeg",&mMtdEvtData.gmBackLeg,"gmBackLeg[ngTrks]/b");
	mMtdEvt->Branch("gmModule",&mMtdEvtData.gmModule,"gmModule[ngTrks]/b");
	mMtdEvt->Branch("gmCell",&mMtdEvtData.gmCell,"gmCell[ngTrks]/b");
	mMtdEvt->Branch("gmLeTimeWest",&mMtdEvtData.gmLeTimeWest,"gmLeTimeWest[ngTrks]/F");
	mMtdEvt->Branch("gmTotWest",&mMtdEvtData.gmTotWest,"gmTotWest[ngTrks]/F");
	mMtdEvt->Branch("gmLeTimeEast",&mMtdEvtData.gmLeTimeEast,"gmLeTimeEast[ngTrks]/F");
	mMtdEvt->Branch("gmTotEast",&mMtdEvtData.gmTotEast,"gmTotEast[ngTrks]/F");
	mMtdEvt->Branch("gmLocalZ",&mMtdEvtData.gmLocalZ,"gmLocalZ[ngTrks]/F");
	mMtdEvt->Branch("gmLocalY",&mMtdEvtData.gmLocalY,"gmLocalY[ngTrks]/F");
	AddObj(mMtdEvt,".hist");
}

/// InitRun: initialize geometries (retrieve beam line constraint from database)
Int_t StMtdMatchMaker::InitRun(int runnumber) {

	LOG_INFO << "Initializing MTD Geometry:" << endm;
	//=======================================================//
	//  			MTD Geometry initialization
	//=======================================================//
	mMtdGeom = 0;
	mMtdGeom = new StMtdGeometry("mtdGeometry","mtdGeometry in StMtdMatchMaker");
	mMtdGeom->SetCosmicFlag(mCosmicFlag);
	mMtdGeom->SetELossFlag(mELossFlag);
	mMtdGeom->SetNExtraCells(mNExtraCells);
	mMtdGeom->SetGeomTag(mGeomTag);

	Float_t fScale = -1.;
	if(mLockBField){
		mMtdGeom->SetLockBField(mLockBField);
		LOG_INFO<<" Initializing locked mag.field for simulation! "<<endm;
	}else if(StarMagField::Instance()){
		//fScale = St_MagFactorC::instance()->ScaleFactor();
		fScale = StarMagField::Instance()->GetFactor();
		if (TMath::Abs(fScale) < 1e-3) fScale = 1e-3;
		else{
			mMtdGeom->SetBFactor(fScale);
			LOG_INFO<<" Initializing mag.field from StarMagField! fScale = "<<fScale<<endm;
		}
	}else{
		assert(StarMagField::Instance());
		LOG_ERROR<<" ******* MagField uninitialized, have you loaded StMagFMaker ?! *******"<<endm;
	}
	mMtdGeom->Init(this);
	LOG_INFO<<" Created a new mtdGeometry ..."<<endm;

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

			//clean up any association done before
			aHit->setIndex2Primary(-1);
			aHit->setIndex2Global(-1);
			aHit->setAssociatedTrackKey(-1);
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
		unsigned int trgTime[2] = {0,0};
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
			mMtdEvtData.refMult = mMuDst->event()->refMult();

			StMuPrimaryVertex *pVtx = mMuDst->primaryVertex();
			float xvtx = -999.;
			float yvtx = -999.;
			float zvtx = -999.;
			if(pVtx){
				xvtx = pVtx->position().x();
				yvtx = pVtx->position().y();
				zvtx = pVtx->position().z();
			}else {
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
		unsigned int trgTime[2] = {0,0};
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
				xvtx = pVtx->position().x();
				yvtx = pVtx->position().y();
				zvtx = pVtx->position().z();
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

			if(Debug()){
				LOG_INFO << "Number of Mtd Raw Hits = " << mtdRawHits.size() <<endm;
				LOG_INFO << "Number of Mtd Hits = " << mtdHits.size() <<endm;
			}
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

	float mField = 0;
	UInt_t Nnodes = 0;
	if(mMuDstIn){

		index2Primary.clear();
		Int_t pNnodes = mMuDst->numberOfPrimaryTracks();
		for(Int_t ii=0;ii<pNnodes;ii++){
			StMuTrack *pTrack = mMuDst->primaryTracks(ii); 
			if(!pTrack) continue;
			Int_t index2Global = pTrack->index2Global();
			StMuTrack *gTrack = mMuDst->globalTracks(index2Global);
			if(!gTrack) continue;
			index2Primary[index2Global] = ii;
		}

		Nnodes = mMuDst->numberOfGlobalTracks();
		mField = mMuDst->event()->runInfo().magneticField();
		for(UInt_t iNode=0;iNode<Nnodes;iNode++){

			StThreeVectorD globalPos(-999,-999,-999);
			StMuTrack *theTrack=mMuDst->globalTracks(iNode);
			if(!theTrack) continue;

			//clean up any association done before
			StMuMtdPidTraits pidMtd;
			theTrack->setMtdPidTraits(pidMtd);
			theTrack->setIndex2MtdHit(-999);

			bool isPrimary=kFALSE;
			Int_t pIndex = -999;
			map<Int_t, Int_t>::iterator it = index2Primary.find(iNode);
			if(it!=index2Primary.end()){
				pIndex = it->second;
			}
			if(pIndex>=0){
				isPrimary=kTRUE;
				StMuTrack *thePrimaryTrack= mMuDst->primaryTracks(pIndex);
				thePrimaryTrack->setMtdPidTraits(pidMtd);
				thePrimaryTrack->setIndex2MtdHit(-999);
			}

			if(!validTrack(theTrack)) continue;
			const StMuBTofPidTraits tofpid = theTrack->btofPidTraits();
			globalPos = tofpid.position();
			if(matchTrack2Mtd(daqCellsHitVec,theTrack->outerHelix(),theTrack->charge(),allCellsHitVec,iNode,globalPos)){
				nAllTracks++;
				if(isPrimary) nPrimaryHits++;
			}
			if(mSaveTree){
				fillTrackInfo(theTrack, mField, iNode);
			}
			ngTracks++;
		}
	}else{
		Nnodes = mEvent->trackNodes().size();
		mField = mEvent->runInfo()->magneticField();
		for(UInt_t iNode=0;iNode<Nnodes;iNode++){
			StSPtrVecTrackNode& nodes=mEvent->trackNodes();
			StGlobalTrack *theTrack = dynamic_cast<StGlobalTrack*>(nodes[iNode]->track(global));
			if(!theTrack) continue;
			bool isPrimary =kFALSE;
			StPrimaryTrack *pTrack =dynamic_cast<StPrimaryTrack*>(theTrack->node()->track(primary));
			if(pTrack) isPrimary = kTRUE;
			if(!validTrack(theTrack)) continue;

			StThreeVectorD globalPos(-999,-999,-999);
			StSPtrVecTrackPidTraits& traits = theTrack->pidTraits();
			for (unsigned int it=0;it<traits.size();it++){
				if (traits[it]->detector() == kTofId) {
					StBTofPidTraits* tofpid = dynamic_cast<StBTofPidTraits*>(traits[it]);
					if(tofpid) globalPos = tofpid->position();
				}
			}

			if(matchTrack2Mtd(daqCellsHitVec,theTrack->outerGeometry()->helix(),theTrack->geometry()->charge(),allCellsHitVec,iNode,globalPos)){
				nAllTracks++;
				if(isPrimary) nPrimaryHits++;
			}

			if(mSaveTree){
				fillTrackInfo(theTrack, mField, iNode);
			}
			ngTracks++;
		} // end for
	} // end if (mMuDstIn)

	if(mSaveTree){
		mMtdEvtData.ngTrks = ngTracks;
	}

}


/// Match extrapolated TPC tracks to hits in the MTD
bool StMtdMatchMaker::matchTrack2Mtd(mtdCellHitVector daqCellsHitVec,const StPhysicalHelixD &helix, Int_t gq, mtdCellHitVector& allCellsHitVec, unsigned int iNode, StThreeVectorD globalPos){
	float mField = 0;
	if(mMuDstIn) mField = mMuDst->event()->runInfo().magneticField();
	else mField = mEvent->runInfo()->magneticField();

	StThreeVectorD pVtx(0,0,0);
	if(mMuDstIn) pVtx = mMuDst->event()->primaryVertexPosition();
	else{
		if (mEvent->primaryVertex()){
			pVtx = mEvent->primaryVertex()->position();
		}
	}

	StThreeVector<double> dcaPos  = helix.at(helix.pathLength(pVtx));
	StThreeVector<double> dca     = dcaPos - pVtx;
	//LOG_INFO<<"******************* B Field from data = "<<mField/10.<<" charge = "<<gq<<" **********************"<<endm;

	float mFieldFromGeom = mMtdGeom->GetFieldZ(100,100,0.);
	if(fabs(mFieldFromGeom-mField/10.)>0.2){
		LOG_WARN<<"Wrong magnetc field mField = "<<mField/10.<<" mFieldFromGeom = "<<mFieldFromGeom<<"  check the magF input!!!"<<endm;
	}
	IntVec idVec;
	DoubleVec pathVec;
	DoubleVec tofVec;
	PointVec  crossVec;
	if(mCosmicFlag){ 
		mMtdGeom->HelixCrossCellIds(helix,idVec,pathVec,crossVec,tofVec);
	}else{
		mMtdGeom->HelixCrossCellIds(helix,pVtx,idVec,pathVec,crossVec,tofVec);
	}

	if((idVec.size()!=pathVec.size()) || idVec.size()!=crossVec.size() || idVec.size()!=tofVec.size()){ 
		if(Debug()){
			LOG_INFO<<"Inconsistent size idVec = "<<idVec.size()<<" pathVec = "<<pathVec.size()<<" crossVec = "<<crossVec.size()<<" tofVec = "<<tofVec.size()<<endm;
		}
		return kFALSE; 
	}

	mMtdEvtData.gdca[ngTracks] = dca.mag();	
	mMtdEvtData.gTrkMatchNum[ngTracks] = idVec.size();	
	int nCells = 0;
	if(Debug()){
		LOG_INFO<<"dca:"<<dca.mag()<<endm;
		LOG_INFO<<"idVec:"<<idVec.size()<<endm;
		LOG_INFO<<"iNode:"<<iNode<<endm;
		if(idVec.size()>1) LOG_INFO<<"MARK: one track cross two modules!"<<endm;
	}
	for (UInt_t i = 0; i < idVec.size(); i++) {
		Int_t iBL = -9999;
		Int_t iMod = -9999;
		Int_t iCell = -9999;
		mMtdGeom->DecodeCellId(idVec[i],iBL,iMod,iCell);
		StMtdGeoModule *mMtdGeoModule = mMtdGeom->GetGeoModule(iBL,iMod);
		Double_t local[3]={0,0,0};
		Double_t global[3]={crossVec[i].x(),crossVec[i].y(),crossVec[i].z()};
		if(mMtdGeoModule) mMtdGeoModule->MasterToLocal(global,local);
		StThreeVectorD ol(local[0],local[1],local[2]);

		StructCellHit cellHit;
		cellHit.backleg  = iBL;
		cellHit.module   = iMod;
		cellHit.cell     = iCell;
		cellHit.trackIdVec.push_back((Int_t)iNode);
		cellHit.hitPosition = crossVec[i];
		cellHit.zhit	=	ol.z();
		cellHit.yhit	=	ol.y();
		cellHit.theta	=	ol.theta();
		cellHit.pathLength = pathVec[i];
		allCellsHitVec.push_back(cellHit);
		nCells++;

		if(Debug()){
			if(idVec.size()>1) LOG_INFO<<"iBL:iMod:iCell="<<iBL<<" "<<iMod<<" "<<iCell<<endm;
		}
		//attention: only saved last matched hit
		if(mSaveTree){

			mMtdEvtData.gprojMtdBackLeg[ngTracks][i] = iBL;	
			mMtdEvtData.gprojMtdModule[ngTracks][i] = iMod;	
			mMtdEvtData.gtrackindex[ngTracks]= (Int_t)iNode;//test Qian Yang
			mMtdEvtData.gprojMtdCell[ngTracks][i] = iCell;
			mMtdEvtData.gprojMtdPhi[ngTracks][i] = crossVec[i].phi();	
			mMtdEvtData.gprojMtdZ[ngTracks][i] = crossVec[i].z();	
			mMtdEvtData.gprojMtdLength[ngTracks][i] = pathVec[i];	
			mMtdEvtData.gtof2Mtd[ngTracks][i] = tofVec[i];	
		}
	}
	return kTRUE;
}

/// Match the MTD hits
void StMtdMatchMaker::matchMtdHits(mtdCellHitVector& daqCellsHitVec,mtdCellHitVector& allCellsHitVec,mtdCellHitVector& matchHitCellsVec){
	StructCellHit cellHit;
	mtdCellHitVectorIter daqIter = daqCellsHitVec.begin();

	for(unsigned int idaq=0;idaq<daqCellsHitVec.size();idaq++, daqIter++) {
		mtdCellHitVectorIter proIter = allCellsHitVec.begin();
		assert(mMtdGeom->GetGeoModule(daqIter->backleg,daqIter->module));
		for(unsigned int ipro=0;ipro<allCellsHitVec.size();ipro++, proIter++) {

			int daqIndex = (daqIter->module-1)*12 + (daqIter->cell);
			int proIndex = (proIter->module-1)*12 + (proIter->cell);
			int hisIndex = daqIter->backleg - 1;
			if (mHisto) {

				double stripPhiCen = 0.;
				//int trayId=daqIter->module;
				int channel = daqIter->cell;
				//LOG_INFO<<" backleg = "<<daqIter->backleg<<", module = "<<daqIter->module<<" cell = "<<channel<<endm;
				//double backLegPhiCen = mFirstBackLegPhi+(daqIter->backleg-1)*(backLegPhiWidth+backLegPhiGap);
				//if(backLegPhiCen>2.*TMath::Pi()) backLegPhiCen -= 2.*TMath::Pi();
				//if(trayId>0&&trayId<4){
				//	stripPhiCen = backLegPhiCen-(mMtdGeom->GetNCells()/2.-0.5-channel)*(mCellWidth+mCellGap)/mtdRadius; // approximation
				//}else{
				//	stripPhiCen = backLegPhiCen+(mMtdGeom->GetNCells()/2.-0.5-channel)*(mCellWidth+mCellGap)/mtdRadius; 
				//}
				//if(stripPhiCen>2.*TMath::Pi()) stripPhiCen -= 2.*TMath::Pi();
				//if(stripPhiCen<0.)    stripPhiCen += 2.*TMath::Pi();
				stripPhiCen = mMtdGeom->GetGeoModule(daqIter->backleg,daqIter->module)->GetCellPhiCenter(channel);

				double mLeTimeWest = daqIter->leadingEdgeTime.first;
				double mLeTimeEast = daqIter->leadingEdgeTime.second;
				StThreeVectorD modCen = mMtdGeom->GetGeoModule(daqIter->backleg,daqIter->module)->GetNodePoint();
				double stripZCen   = modCen.z() - (mLeTimeWest-mLeTimeEast)/2./vDrift*1000.;

				double daqphi = stripPhiCen;
				double daqz   = stripZCen;

				//LOG_INFO<<"Test: strip phi center old = "<<stripPhiCen<<" new = "<<modCenPhi<<endm;
				//LOG_INFO<<"Test: strip z center old = "<<stripZCen<<" new = "<<modCen.z()- (mLeTimeWest-mLeTimeEast)/2./vDrift*1000.<<endm;

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

			StThreeVectorD modCen = mMtdGeom->GetGeoModule(daqIter->backleg,daqIter->module)->GetNodePoint();
			double ysig = phisig*modCen.perp();
			//StThreeVector<double> hitPos;

			Int_t   ibackleg = daqIter->backleg;
			Int_t   imodule  = daqIter->module;
			Int_t   icell    = daqIter->cell;

			double zdaq = (daqIter->leadingEdgeTime.second-daqIter->leadingEdgeTime.first)/2./mVDrift[(ibackleg-1)*mMtdGeom->GetNModules()+imodule-1][icell]*1e3;
			//double ydaq = (daqIter->cell-mMtdGeom->GetNCells()/2.+0.5)*(mCellWidth+mCellGap);
			double ydaq = mMtdGeom->GetGeoModule(ibackleg,imodule)->GetCellLocalYCenter(icell);
			bool isMatch = false;


			if(daqIter->backleg==proIter->backleg){
				if(daqIter->module==proIter->module) isMatch = true;
			}

			if(mnNeighbors){
				if(daqIter->backleg==proIter->backleg && daqIter->module!=proIter->module){
					if(zdaq<0){
						if((daqIter->module-1>0)&&daqIter->module-1==proIter->module) isMatch = true; 
					}else if(zdaq==0){
						if(abs(daqIter->module-proIter->module)<=1) isMatch = true; 
					}else{
						if((daqIter->module+1<6)&&daqIter->module+1==proIter->module) isMatch = true; 
					}
				}
			}


			if(isMatch){
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
	StructCellHit cellHit_candidate;
	mtdCellHitVector tempVec = matchHitCellsVec;
	mtdCellHitVector erasedVec = tempVec;
	mtdCellHitVector multiHitsCellsVec_temp;
	//if(Debug()){
	//	LOG_INFO<<" matchHitCellsVec: "<<endm; 
	//	mtdCellHitVectorIter ij=tempVec.begin();
	//	while (ij != tempVec.end()) { 
	//		LOG_INFO<<" backleg = " << ij->backleg<<" module = "<<ij->module<<" cell = "<<ij->cell<<endm; 
	//		LOG_INFO<<" trackIdVec: "<<endm;
	//		idVector tIdVec = ij->trackIdVec;
	//		for(int i=0;i<tIdVec.size();i++) LOG_INFO<<tIdVec[i]<<endm;
	//		++ij; 
	//	}
	//}
	while (tempVec.size() != 0) {
		Int_t nTracks = 0;
		idVector trackIdVec;

		mtdCellHitVectorIter tempIter=tempVec.begin();
		mtdCellHitVectorIter erasedIter=erasedVec.begin();
		while(erasedIter!= erasedVec.end()) {
			if(tempIter->backleg == erasedIter->backleg &&
					tempIter->module == erasedIter->module &&
					tempIter->cell == erasedIter->cell) {
				//
				if(nTracks>0)
				{
					if(nTracks==1)
					{
						cellHit.cell = tempIter->cell;
						cellHit.module = tempIter->module;
						cellHit.backleg = tempIter->backleg;
						cellHit.hitPosition = tempIter->hitPosition;
						cellHit.trackIdVec.push_back(tempIter->trackIdVec.back());
						cellHit.zhit = tempIter->zhit;
						cellHit.matchFlag = -999; 
						cellHit.yhit = tempIter->yhit;
						cellHit.tot = tempIter->tot;
						cellHit.leadingEdgeTime = tempIter->leadingEdgeTime;
						cellHit.index2MtdHit = tempIter->index2MtdHit;
						cellHit.theta = tempIter->theta;
						cellHit.pathLength = tempIter->pathLength;
						if(Debug())
						{
							LOG_INFO<<"track Info: "<<cellHit.zhit<<" "<<cellHit.yhit<<endm;
							LOG_INFO<<"hit Info: "<<cellHit.backleg<<" "<<cellHit.module<<" "<<cellHit.cell<<endm;
						}
						multiHitsCellsVec_temp.push_back(cellHit);
					}

					cellHit_candidate.cell = erasedIter->cell;
					cellHit_candidate.module = erasedIter->module;
					cellHit_candidate.backleg = erasedIter->backleg;
					cellHit_candidate.hitPosition = erasedIter->hitPosition;
					cellHit_candidate.trackIdVec.push_back(erasedIter->trackIdVec.back());
					cellHit_candidate.zhit = erasedIter->zhit;
					cellHit_candidate.matchFlag = -999; 
					cellHit_candidate.yhit = erasedIter->yhit;
					cellHit_candidate.tot = erasedIter->tot;
					cellHit_candidate.leadingEdgeTime = erasedIter->leadingEdgeTime;
					cellHit_candidate.index2MtdHit = erasedIter->index2MtdHit;
					cellHit_candidate.theta = erasedIter->theta;
					cellHit_candidate.pathLength = erasedIter->pathLength;
					multiHitsCellsVec_temp.push_back(cellHit_candidate);
					if(Debug())
					{
						LOG_INFO<<"track Info: "<<cellHit_candidate.zhit<<" "<<cellHit_candidate.yhit<<endm;
						LOG_INFO<<"hit Info: "<<cellHit_candidate.backleg<<" "<<cellHit_candidate.module<<" "<<cellHit_candidate.cell<<endm;
					}
				}
				//
				nTracks++;
				trackIdVec.push_back(erasedIter->trackIdVec.back());  // merge
				erasedVec.erase(erasedIter);
				--erasedIter;
			}
			++erasedIter;
		}

		cellHit.cell = tempIter->cell;
		cellHit.module = tempIter->module;
		cellHit.backleg = tempIter->backleg;
		cellHit.hitPosition = tempIter->hitPosition;
		cellHit.trackIdVec = trackIdVec;
		cellHit.zhit = tempIter->zhit;
		cellHit.matchFlag = -999; 
		cellHit.yhit = tempIter->yhit;
		cellHit.tot = tempIter->tot;
		cellHit.leadingEdgeTime = tempIter->leadingEdgeTime;
		cellHit.index2MtdHit = tempIter->index2MtdHit;
		cellHit.theta = tempIter->theta;
		cellHit.pathLength = tempIter->pathLength;

		if(mHisto) {
			//Float_t ycenter = (tempIter->cell-mMtdGeom->GetNCells()/2+0.5)*(mCellWidth+mCellGap);
			Float_t ycenter = mMtdGeom->GetGeoModule(tempIter->backleg,tempIter->module)->GetCellLocalYCenter(tempIter->cell);
			Float_t dy = tempIter->yhit - ycenter;
			Float_t dz = tempIter->zhit;
			mTracksPerCellMatch1->Fill(trackIdVec.size());
			mDaqOccupancyMatch1->Fill((tempIter->module-1)*mMtdGeom->GetNCells()+tempIter->cell);
			mDeltaHitMatch1->Fill(dy, dz);
		}

		//double modLocalYCen = mMtdGeom->GetGeoModule(tempIter->backleg,tempIter->module)->GetCellLocalYCenter(tempIter->cell);

		if (nTracks==1){
			nSingleHitCells++;      
			singleHitCellsVec.push_back(cellHit);
		} else if (nTracks>1){
			idVector tmpIdVec = trackIdVec;
			//if(Debug()){
			//	LOG_INFO<<" trackIdVec before: "<<endm; 
			//	idVectorIter ij=tmpIdVec.begin();
			//	while (ij != tmpIdVec.end()) { LOG_INFO<< " " << *ij<<endm; ++ij; }
			//}
			UInt_t temsize1 = tmpIdVec.size(); 
			sort(tmpIdVec.begin(),tmpIdVec.end());
			tmpIdVec.erase(unique(tmpIdVec.begin(),tmpIdVec.end()),tmpIdVec.end());
			//if(Debug()){
			//	LOG_INFO<<" trackIdVec after: "<<endm; 
			//	idVectorIter ij=tmpIdVec.begin();
			//	while (ij != tmpIdVec.end()) { LOG_INFO<< " " << *ij<<endm; ++ij; }
			//}
			//cellHit.trackIdVec = tmpIdVec;
			if(tmpIdVec.size()!=temsize1)
			{
				if(Debug())
				{
					LOG_INFO<<"mark here ="<<temsize1<<"  "<<tmpIdVec.size()<<endm;
				}
			}
			if(tmpIdVec.size()==1){
				cellHit.trackIdVec = tmpIdVec;
				nSingleHitCells++;      
				singleHitCellsVec.push_back(cellHit);
			}else if(tmpIdVec.size()>1){
				mtdCellHitVector MutVec = multiHitsCellsVec_temp;
				for(idVectorIter trkIdIter = tmpIdVec.begin();trkIdIter != tmpIdVec.end(); ++trkIdIter)
				{	
					int markflag = 0;
					if(Debug()) LOG_INFO<<"trackId ="<<*trkIdIter<<endm;

					for(mtdCellHitVectorIter temIter=MutVec.begin();temIter != MutVec.end(); ++temIter)
					{
						idVector ID = temIter->trackIdVec;
						if(Debug()) LOG_INFO<<"MutVecId ="<<ID.back()<<endm;
						if(*trkIdIter == ID.back())
						{
							markflag++;
							if(markflag==1)
							{
								nMultiHitsCells++;
								multiHitsCellsVec.push_back(*temIter);
								if(Debug()){
									LOG_INFO<<"Accept VecId ="<<ID.back()<<endm;
								}
							}
						}
					}
				}
			}else{
				LOG_WARN<<"D: something wrong here!"<<endm;
			}
			// for multiple hit cells either discard (yes) or
			// find the most likely candidate.
		} else {
			LOG_WARN << "D: no tracks extrapolate to matched cell ... should not happen!" << endm;
		}
		if(Debug()) { 
			LOG_DEBUG << "D: backleg=" << cellHit.backleg << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:";
			idVectorIter ij=trackIdVec.begin();
			while (ij != trackIdVec.end()) { LOG_DEBUG<< " " << *ij<<endm; ij++; }
		}
		tempVec = erasedVec;
		multiHitsCellsVec_temp.clear();
	}
	if(Debug()){
		LOG_INFO<<"multiHitsCellsVec size="<<multiHitsCellsVec.size()<<endm;
	}
	/////////////////////////////////////////////////////////////////////assign hits to the closest track
	mtdCellHitVector tempVec_2Trck = multiHitsCellsVec;
	mtdCellHitVector erasedVec_2Trck = tempVec_2Trck;
	while(tempVec_2Trck.size()!=0)
	{
		StructCellHit Cellhit;
		Int_t ntracks = 0;
		idVector vTrackId;
		vector<StThreeVectorD> vPosition;
		vector<Int_t> vchannel, vbackleg, vmodule, vcell;
		vector<Float_t> vzhit, vyhit;
		vector<pairD> vtot; 
		vector<pairD> vtdc; 
		vector<Double_t> vtheta;
		vector<Double_t> vpathLength;
		vector<Int_t> vindex2MtdHit;

		mtdCellHitVectorIter temp_Iter=tempVec_2Trck.begin();
		mtdCellHitVectorIter erased_Iter=erasedVec_2Trck.begin();

		while(erased_Iter!= erasedVec_2Trck.end()) {
			if(temp_Iter->backleg == erased_Iter->backleg &&
					temp_Iter->module == erased_Iter->module &&
					temp_Iter->cell == erased_Iter->cell) {
				ntracks++;
				vbackleg.push_back(erased_Iter->backleg);
				vmodule.push_back(erased_Iter->module);
				vcell.push_back(erased_Iter->cell);
				vPosition.push_back(erased_Iter->hitPosition);
				vTrackId.push_back(erased_Iter->trackIdVec.back());
				vzhit.push_back(erased_Iter->zhit);
				vyhit.push_back(erased_Iter->yhit);
				vtot.push_back(erased_Iter->tot);
				vtdc.push_back(erased_Iter->leadingEdgeTime);
				vindex2MtdHit.push_back(erased_Iter->index2MtdHit);
				vtheta.push_back(erased_Iter->theta);
				vpathLength.push_back(erased_Iter->pathLength);
				if(Debug())
				{
					LOG_INFO<<"ntracks ="<<ntracks<<endm;
					LOG_INFO<<"tack INFO: "<<vzhit[ntracks-1]<<" "<<vyhit[ntracks-1]<<endm;
					LOG_INFO<<"hits INFO: "<<vbackleg[ntracks-1]<<" "<<vmodule[ntracks-1]<<" "<<vcell[ntracks-1]<<endm;
				}
				erasedVec_2Trck.erase(erased_Iter);
				erased_Iter--;
			}
			erased_Iter++;
		}// end of the multitrack finding loop

		if (ntracks<2)
			LOG_INFO<<"multi::  what?  one hit just match with one track in multimatch!!!!!!! "<<endm;

		if(ntracks>1){
			Int_t thiscandidate(-99);
			Int_t thisMatchFlag(0);

			// sort on tot
			vector<Int_t> ttCandidates;
			for (Int_t i=0;i<ntracks;i++) {
				ttCandidates.push_back(i);
			}
			if(Debug())
			{
				LOG_INFO<<"ttcandidates: "<<ttCandidates.size()<<endm;
			}
			if (ttCandidates.size()>1){  // sort on hitposition
				Float_t ss(9999.);
				vector<Int_t> ssCandidates;
				for(int j=0;j<ntracks;j++) {
					Int_t   ibackleg = vbackleg[ttCandidates[j]];
					Int_t   imodule = vmodule[ttCandidates[j]];
					Int_t   icell = vcell[ttCandidates[j]];
					Float_t yy = vyhit[ttCandidates[j]];
					Float_t ycell = mMtdGeom->GetGeoModule(ibackleg,imodule)->GetCellLocalYCenter(icell);
					Float_t ll = fabs(yy-ycell);
					Float_t mLeadingWest = vtdc[ttCandidates[j]].first;
					Float_t mLeadingEast = vtdc[ttCandidates[j]].second;
					float zcell = (mLeadingEast- mLeadingWest)/2./mVDrift[(ibackleg-1)*mMtdGeom->GetNModules()+imodule-1][icell]*1e3;
					Float_t zz = vzhit[ttCandidates[j]];
					Float_t ww = fabs(zz-zcell);
					Float_t rr = 9999.;
					if(Debug()){
						LOG_INFO<<"hit information::"<<" "<<ibackleg<<" "<<imodule<<" "<<icell<<endm;
						LOG_INFO<<"track information::"<<" "<<zz<<" "<<yy<<endm;
					}
					if(mCosmicFlag) rr = ll;
					else rr = sqrt(ll*ll+ww*ww);
					if(Debug()){
						LOG_INFO<<"distance::"<<" "<<ww<<" "<<ll<<" "<<rr<<endm;
					}
					if(rr<ss){
						ss = rr; 
						ssCandidates.clear();
						ssCandidates.push_back(ttCandidates[j]);
					}else if(rr==ss)
						ssCandidates.push_back(ttCandidates[j]);
				}
				if (ssCandidates.size()>=1){
					thiscandidate = ssCandidates[0];
					thisMatchFlag = 7;
				}
			}//sort on hitposition

			if (thiscandidate>=0) {
				Cellhit.backleg = vbackleg[thiscandidate];
				Cellhit.module = vmodule[thiscandidate];
				Cellhit.cell = vcell[thiscandidate];
				Cellhit.trackIdVec.push_back(vTrackId[thiscandidate]);
				Cellhit.hitPosition = vPosition[thiscandidate];
				Cellhit.matchFlag = thisMatchFlag;
				Cellhit.zhit = vzhit[thiscandidate];
				Cellhit.yhit = vyhit[thiscandidate];
				Cellhit.tot = vtot[thiscandidate];
				Cellhit.leadingEdgeTime = vtdc[thiscandidate];
				Cellhit.index2MtdHit = vindex2MtdHit[thiscandidate];
				Cellhit.theta = vtheta[thiscandidate];
				Cellhit.pathLength = vpathLength[thiscandidate];
				singleHitCellsVec.push_back(Cellhit);
				if(Debug())
				{
					LOG_INFO<<"flag 0:"<<thisMatchFlag<<endm;
				}
			}
		}
		tempVec_2Trck = erasedVec_2Trck;
	}// end of the multihitscellsvec loop	
}


/// Sort and in the case of tracks with multiple matched cells pick the best candidate and set the MatchFlag
void StMtdMatchMaker::finalMatchedMtdHits(mtdCellHitVector& singleHitCellsVec,mtdCellHitVector& finalMatchedCellsVec){
	mtdCellHitVector tempVec = singleHitCellsVec;

	if(mHisto) {
		mCellsPerEventMatch2->Fill(tempVec.size());
		for(unsigned int ii=0;ii<tempVec.size();ii++) {
			mTracksPerCellMatch2->Fill(tempVec[ii].trackIdVec.size());
			mDaqOccupancyMatch2->Fill((tempVec[ii].module-1)*mMtdGeom->GetNCells()+tempVec[ii].cell);
			//Float_t ycenter = (tempVec[ii].cell-mMtdGeom->GetNCells()/2+0.5)*(mCellWidth+mCellGap);
			Float_t ycenter = mMtdGeom->GetGeoModule(tempVec[ii].backleg,tempVec[ii].module)->GetCellLocalYCenter(tempVec[ii].cell);
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
		vector<Int_t> vflag;
		Int_t thisMatchFlag_tem(0);
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
				vflag.push_back(erasedIter->matchFlag);
				vpathLength.push_back(erasedIter->pathLength);
				if(Debug())
				{
					LOG_INFO<<"flag 1 ::"<<" "<<nCells<<" "<<erasedIter->matchFlag<<endm;
				}
				erasedVec.erase(erasedIter);
				--erasedIter;
			}
			++erasedIter;
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

			if(Debug())
			{
				LOG_INFO<<"flag 2::"<<vflag[0]<<endm;
			}
			if(cellHit.matchFlag==7)
				finalMatchedCellsVec.push_back(cellHit);	
			else
			{
				cellHit.matchFlag = 1; 
				finalMatchedCellsVec.push_back(cellHit);
			}

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
			vector<Int_t> ttCandidates;
			for (Int_t i=0;i<nCells;i++) {
				pair<Double_t,Double_t> tt = vtot[i];
				if(tt.first<40.) {    // open the ToT cut to 40 ns
					ttCandidates.push_back(i);
				}
			}
			if (ttCandidates.size()==1) {
				if(Debug())
				{
					LOG_INFO<<"flag 3 ::"<<vflag[ttCandidates[0]]<<endm;
				}
				if(vflag[ttCandidates[0]]==7)
				{
					thiscandidate = ttCandidates[0];
					thisMatchFlag = 8;
					thisMatchFlag_tem = thisMatchFlag;
				}
				else
				{
					thiscandidate = ttCandidates[0];
					thisMatchFlag = 2;
					thisMatchFlag_tem = thisMatchFlag;
				}
			} else if (ttCandidates.size()>1){  // sort on hitposition
				Float_t ss(9999.);
				vector<Int_t> ssCandidates;
				for(size_t j=0;j<ttCandidates.size();j++) {
					Int_t   ibackleg = vbackleg[ttCandidates[j]];
					Int_t   imodule = vmodule[ttCandidates[j]];
					Int_t   icell = vcell[ttCandidates[j]];
					Float_t yy = vyhit[ttCandidates[j]];
					//Float_t ycell = (vcell[ttCandidates[j]]-mMtdGeom->GetNCells()/2+0.5)*(mCellWidth+mCellGap);
					Float_t ycell = mMtdGeom->GetGeoModule(ibackleg,imodule)->GetCellLocalYCenter(icell);
					Float_t ll = fabs(yy-ycell);
					Float_t mLeadingWest = vtdc[ttCandidates[j]].first;
					Float_t mLeadingEast = vtdc[ttCandidates[j]].second;
					Float_t zcell = (mLeadingEast- mLeadingWest)/2./mVDrift[(ibackleg-1)*mMtdGeom->GetNModules()+imodule-1][icell]*1e3;

					Float_t zz = vzhit[ttCandidates[j]];
					Float_t ww = fabs(zz-zcell);
					Float_t rr = 9999.;
					if(mCosmicFlag) rr = ll;
					else rr = sqrt(ll*ll+ww*ww);
					if(rr<ss){
						ss = rr; 
						ssCandidates.clear();
						ssCandidates.push_back(ttCandidates[j]);
					}else if(rr==ss)
						ssCandidates.push_back(ttCandidates[j]);
				}
				if (ssCandidates.size()>=1){
					if(Debug())
					{
						LOG_INFO<<"flag 4::"<<ssCandidates.size()<<" "<<vflag[ttCandidates[0]]<<endm;
					}
					if(vflag[ssCandidates[0]]==7)
					{
						thiscandidate = ssCandidates[0];
						thisMatchFlag = 9;
						thisMatchFlag_tem = thisMatchFlag;
					}
					else
					{
						thiscandidate = ssCandidates[0];
						thisMatchFlag = 3;
						thisMatchFlag_tem = thisMatchFlag;
					}
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

		//Float_t ycenter = (cell-mMtdGeom->GetNCells()/2+0.5)*(mCellWidth+mCellGap);
		Float_t ycenter = mMtdGeom->GetGeoModule(backleg,module)->GetCellLocalYCenter(cell);
		Float_t yLocal = finalMatchedCellsVec[ii].yhit;
		Float_t zLocal = finalMatchedCellsVec[ii].zhit;

		Float_t LeTimeWest = finalMatchedCellsVec[ii].leadingEdgeTime.first;
		Float_t LeTimeEast = finalMatchedCellsVec[ii].leadingEdgeTime.second;
		Float_t fireZLocal = (LeTimeEast-LeTimeWest)/2./vDrift*1000.;
		Float_t dy = finalMatchedCellsVec[ii].yhit - ycenter;
		Float_t dz = finalMatchedCellsVec[ii].zhit - fireZLocal;
		if(mHisto) {
			mTracksPerCellMatch3->Fill(finalMatchedCellsVec[ii].trackIdVec.size());
			//      mDaqOccupancyMatch3->Fill((module-1)*mNCell+(cell-1));
			mDeltaHitMatch3->Fill(dy, dz);
			int hisIndex =  backleg-1;
			mDeltaHitFinal[hisIndex]->Fill(dy,dz);      
		}

		// get track-id from cell hit vector
		Int_t trackNode = finalMatchedCellsVec[ii].trackIdVec[0];
		if(mMuDstIn){

		        LOG_INFO<<"In StMuDst mode: mtd hit matched with track successfully : track nodeId:"<<finalMatchedCellsVec[ii].trackIdVec[0]<<"   mtd hitId:"<<finalMatchedCellsVec[ii].index2MtdHit<<endm;

			StMuTrack *gTrack = mMuDst->globalTracks(trackNode);
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
			mtdHit->setIndex2Global(trackNode);
			gTrack->setIndex2MtdHit(finalMatchedCellsVec[ii].index2MtdHit);

			StMuMtdPidTraits pidMtd = gTrack->mtdPidTraits();
			pidMtd.setMatchFlag(finalMatchedCellsVec[ii].matchFlag);
			pidMtd.setYLocal(yLocal);
			pidMtd.setZLocal(zLocal);
			pidMtd.setDeltaY(dy);
			pidMtd.setDeltaZ(dz);
			pidMtd.setThetaLocal(finalMatchedCellsVec[ii].theta);
			pidMtd.setPosition(finalMatchedCellsVec[ii].hitPosition);
			pidMtd.setPathLength(finalMatchedCellsVec[ii].pathLength);
			gTrack->setMtdPidTraits(pidMtd);

			Int_t pNode = -999;
			map<Int_t, Int_t>::iterator it = index2Primary.find(trackNode);
			if(it!=index2Primary.end()){
				pNode = it->second;
			}
			StMuTrack *pTrack = mMuDst->primaryTracks(pNode);
			if(pTrack && pNode>-1){
				mtdHit->setIndex2Primary(pNode);
				pTrack->setIndex2MtdHit(finalMatchedCellsVec[ii].index2MtdHit);
				StMuMtdPidTraits ppidMtd = pTrack->mtdPidTraits();
				ppidMtd.setMatchFlag(finalMatchedCellsVec[ii].matchFlag);
				ppidMtd.setYLocal(yLocal);
				ppidMtd.setZLocal(zLocal);
				ppidMtd.setDeltaY(dy);
				ppidMtd.setDeltaZ(dz);
				ppidMtd.setThetaLocal(finalMatchedCellsVec[ii].theta);
				ppidMtd.setPosition(finalMatchedCellsVec[ii].hitPosition);
				ppidMtd.setPathLength(finalMatchedCellsVec[ii].pathLength);
				pTrack->setMtdPidTraits(ppidMtd);
			}

		}else{

			LOG_INFO<<"In StEvent mode: mtd hit matched with track successfully : track nodeId:"<<finalMatchedCellsVec[ii].trackIdVec[0]<<"   mtd hitId:"<<finalMatchedCellsVec[ii].index2MtdHit<<endm;

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
			pidMtd->setYLocal(yLocal);
			pidMtd->setZLocal(zLocal);
			pidMtd->setDeltaY(dy);
			pidMtd->setDeltaZ(dz);
			pidMtd->setThetaLocal(finalMatchedCellsVec[ii].theta);
			pidMtd->setPosition(finalMatchedCellsVec[ii].hitPosition);
			pidMtd->setPathLength(finalMatchedCellsVec[ii].pathLength);
			globalTrack->addPidTraits(pidMtd);

			if(primaryTrack){
				StMtdPidTraits *ppidMtd = new StMtdPidTraits();
				ppidMtd->setMtdHit(mtdHit);
				ppidMtd->setMatchFlag(finalMatchedCellsVec[ii].matchFlag);
				ppidMtd->setYLocal(yLocal);
				ppidMtd->setZLocal(zLocal);
				ppidMtd->setDeltaY(dy);
				ppidMtd->setDeltaZ(dz);
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
			mMtdEvtData.gmLocalY[iTrk] = finalMatchedCellsVec[ii].yhit - ycenter;
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
	float ratio = 99.;
	if(mtt.nHitsPoss!=0) ratio = gnFtPts / (1.0*mtt.nHitsPoss);
	if (ratio < mMinFitPointsOverMax) return kFALSE;

	return kTRUE;
}


/// initialize MTD Event Data
void StMtdMatchMaker::initEventData(){
	memset(&mMtdEvtData,0,sizeof(mMtdEvtData));
}

/// fill track info to QA tree 
void StMtdMatchMaker::fillTrackInfo(StTrack *t, float mField, UInt_t iNode){
	if(!t) return;
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
	float ghelixpx  = -999.;
	float ghelixpy  = -999.;
	float ghelixpz  = -999.;
	float ghelixox  = -999.;
	float ghelixoy  = -999.;
	float ghelixoz  = -999.;

	bool isPrimary =kFALSE;
	StPrimaryTrack *pTrack =dynamic_cast<StPrimaryTrack*>(t->node()->track(primary));
	if(pTrack) isPrimary = kTRUE;
	if(!validTrack(t)) return;

	static StTpcDedxPidAlgorithm PidAlgorithm;
	static StElectron* Electron = StElectron::instance();
	static StPionPlus* Pion = StPionPlus::instance();
	static StKaonPlus* Kaon = StKaonPlus::instance();
	static StProton* Proton = StProton::instance();
	const StParticleDefinition* pd = t->pidTraits(PidAlgorithm);

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

	gq		= t->geometry()->charge();
	gpt		= t->geometry()->momentum().perp();
	geta 	= t->geometry()->momentum().pseudoRapidity();
	gphi 	= t->geometry()->momentum().phi();
	//LOG_INFO  <<" project2MTD() Found track: gpt "<< gpt << " gq:"<<gq<<" geta:"<<geta<<" gphi:"<<gphi<<" gnFtPts:"<<gnFtPts<<endm;
	if(isPrimary){

		ppt		= pTrack->geometry()->momentum().perp();
		peta 	= pTrack->geometry()->momentum().pseudoRapidity();
		pphi 	= pTrack->geometry()->momentum().phi();

	}
	gnFtPts	= t->fitTraits().numberOfFitPoints(kTpcId);
	while(gphi<0.)gphi+=2.*(TMath::Pi());
	while(gphi>2*(TMath::Pi()))gphi-=2.*(TMath::Pi());

	if(isPrimary) idx2primary = pTrack->key();

	StPhysicalHelixD helix = t->outerGeometry()->helix();
	StThreeVector<double> helixOrigin = helix.origin();
	StThreeVector<double> helixMomentum = helix.momentum(mField*kilogauss);
	ghelixpx  = helixMomentum.x();
	ghelixpy  = helixMomentum.y();
	ghelixpz  = helixMomentum.z();
	ghelixox  = helixOrigin.x();
	ghelixoy  = helixOrigin.y();
	ghelixoz  = helixOrigin.z();

	StSPtrVecTrackPidTraits& traits = t->pidTraits();
	LOG_DEBUG <<" project2MTD() Found track: gpt "<< gpt << " gq:"<<gq<<" geta:"<<geta<<" gphi:"<<gphi<<" gnFtPts:"<<gnFtPts<<endm;
	for (unsigned int it=0;it<traits.size();it++){
		if (traits[it]->detector() == kTofId) {
			StBTofPidTraits* tofpid = dynamic_cast<StBTofPidTraits*>(traits[it]);

			if(tofpid){
				StBTofHit* aHit = tofpid->tofHit();

				if(tofpid && aHit){
					int tray = aHit->tray();	
					int module = aHit->module();	
					int cell = aHit->cell();	
					gchannel = (tray-1)*192+(module-1)*6+cell-1;
					gyLocal  = tofpid->yLocal();
					gzLocal  = tofpid->zLocal();
					gtdc = aHit->leadingEdgeTime();
					gtot = aHit->tot();
					gtof = tofpid->timeOfFlight();
					gpathLength = tofpid->pathLength();
					gbeta = tofpid->beta();
					LOG_DEBUG <<" project2MTD() Found matched TOF hit: tray:"<< tray << " module:"<<module<<" cell:"<<cell<<" gtdc:"<<gtdc<<" gtof:"<<gtot<<" gtof:"<<gtof<<" gpathLength:"<<gpathLength<<" gbeta:"<<gbeta<<endm;
				}
			}
		}
	}

	if(mHisto){
		mTrackPtEta->Fill(gpt, geta);
		mTrackPtPhi->Fill(gpt, gphi);
		mTrackNFitPts->Fill(gnFtPts);
		if(gdEdx>0.) mTrackdEdxvsp->Fill(gpt, gdEdx*1.e6);
		if(fabs(nSigmaPi)<5.) mNSigmaPivsPt->Fill(gpt, nSigmaPi+5.*gq);
	}


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

void StMtdMatchMaker::fillTrackInfo(StMuTrack *t, float mField, UInt_t iNode){

	if(!t) return;
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
	float ghelixpx  = -999.;
	float ghelixpy  = -999.;
	float ghelixpz  = -999.;
	float ghelixox  = -999.;
	float ghelixoy  = -999.;
	float ghelixoz  = -999.;

	bool isPrimary=kFALSE;
	const StMuTrack *thePrimaryTrack=t->primaryTrack();
	if(thePrimaryTrack) isPrimary=kTRUE;
	if(!validTrack(t)) return;

	nSigmaE  = t->nSigmaElectron(); 
	nSigmaPi = t->nSigmaPion();
	nSigmaK  = t->nSigmaKaon();
	nSigmaP  = t->nSigmaProton();

	gdEdx 		= t->dEdx();
	gndEdxpts 	= t->nHitsDedx();
	gnFtPts 	= t->nHitsFit();
	gq 			= t->charge();
	gpt 		= t->momentum().perp();
	geta 		= t->momentum().pseudoRapidity();
	gphi 		= t->momentum().phi();

	if(isPrimary){
		ppt 	= thePrimaryTrack->momentum().perp();
		peta 	= thePrimaryTrack->momentum().pseudoRapidity();
		pphi 	= thePrimaryTrack->momentum().phi();
	}
	while(gphi<0.)gphi+=2.*(TMath::Pi());
	while(gphi>2*(TMath::Pi()))gphi-=2.*(TMath::Pi());
	if(isPrimary) idx2primary = thePrimaryTrack->id();

	StPhysicalHelixD helix = t->outerHelix();
	StThreeVector<double> helixOrigin = helix.origin();
	StThreeVector<double> helixMomentum = helix.momentum(mField*kilogauss);
	ghelixpx  = helixMomentum.x();
	ghelixpy  = helixMomentum.y();
	ghelixpz  = helixMomentum.z();
	ghelixox  = helixOrigin.x();
	ghelixoy  = helixOrigin.y();
	ghelixoz  = helixOrigin.z();

	const StMuBTofPidTraits tofpid = t->btofPidTraits();
	const StMuBTofHit* aHit = t->tofHit();
	if(aHit){
		int tray = aHit->tray();	
		int module = aHit->module();	
		int cell = aHit->cell();	
		gchannel = (tray-1)*192+(module-1)*6+cell-1;
		gyLocal = tofpid.yLocal();
		gzLocal = tofpid.zLocal();
		gtdc = aHit->leadingEdgeTime();
		gtot = aHit->tot();
		gtof = tofpid.timeOfFlight();
		gpathLength = tofpid.pathLength();
		gbeta = tofpid.beta();
		LOG_DEBUG <<" project2MTD() Found matched TOF hit: tray:"<< tray << " module:"<<module<<" cell:"<<cell<<" gtdc:"<<aHit->leadingEdgeTime()<<" gtof:"<<aHit->tot()<<" gtof:"<<tofpid.timeOfFlight()<<" gpathLength:"<<tofpid.pathLength()<<" gbeta:"<<tofpid.beta()<<endm;
	}

	if(mHisto){
		mTrackPtEta->Fill(gpt, geta);
		mTrackPtPhi->Fill(gpt, gphi);
		mTrackNFitPts->Fill(gnFtPts);
		if(gdEdx>0.) mTrackdEdxvsp->Fill(gpt, gdEdx*1.e6);
		if(fabs(nSigmaPi)<5.) mNSigmaPivsPt->Fill(gpt, nSigmaPi+5.*gq);
	}

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

//___________________________________________________

MtdTrack::MtdTrack(StTrack *stt){

	pt = -999.; eta = -999.; nFtPts = 0;
	nDedxPts = 0; flag = 0; nHitsPoss = 999;
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
	}				
}
//------------------------------------------------
MtdTrack::MtdTrack(StMuTrack *mut){

	pt = -999.; eta = -999.; nFtPts = 0;
	nDedxPts = 0; flag = 0; nHitsPoss = 999;
	if(mut){
		pt 		= mut->momentum().perp();
		eta 	= mut->momentum().pseudoRapidity();
		nFtPts 	= mut->nHitsFit(kTpcId);
		nDedxPts	= mut->nHitsDedx();
		flag        = mut->flag();
		nHitsPoss	= mut->nHitsPoss(kTpcId);
	}
}
