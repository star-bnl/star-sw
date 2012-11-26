#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuMtdCollection.h"
#include "StMuDSTMaker/COMMON/StMuMtdRawHit.h"
#include "StMuDSTMaker/COMMON/StMuMtdHeader.h"

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
#include <iostream>//.h>
#include <fstream>//.h>
#include <stdlib.h>
#include <string>
#include <vector>
#include <math.h>
#include <cmath>
#include "SystemOfUnits.h"   // has "tesla" in it
#include "StEventMaker/StEventMaker.h"
#include "StAssociationMaker/StAssociationMaker.h"

#include "StMcEventMaker/StMcEventMaker.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StThreeVector.hh"
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include "StLorentzVectorD.hh"
#include "StGlobals.hh"                 // for PR()
#include "StGetConfigValue.hh"

#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "StEventTypes.h"
#include "StTpcDedxPidAlgorithm.h"
#include "StParticleDefinition.hh"
#include "StPhysicalHelix.hh"

#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TChain.h"
#include "TSystem.h"
#include <iostream>

#include "TRandom.h"
//added by ruanlj

//#include "TNtuple.h"

//Random generator
#include "Random.h"
#include "RanluxEngine.h"
#include "RandFlat.h"
#include "RandGauss.h"



#include "StStrangeMuDstMaker/StStrangeMuDstMaker.h"
#include "StStrangeMuDstMaker/StStrangeEvMuDst.hh"
#include "StStrangeMuDstMaker/StV0MuDst.hh"
#include "StStrangeMuDstMaker/StV0Mc.hh"
#include "StStrangeMuDstMaker/StXiMuDst.hh"
#include "StStrangeMuDstMaker/StXiMc.hh"
#include "StStrangeMuDstMaker/StKinkMuDst.hh"
#include "StStrangeMuDstMaker/StKinkMc.hh"
#include "StMuDSTMaker/COMMON/StMuMtdHit.h"

#include "tables/St_vertexSeed_Table.h" //

#include "StMuDSTMaker/COMMON/StMuBTofHit.h"
#include "StMuDSTMaker/COMMON/StMuBTofPidTraits.h"
#include "StBTofPidTraits.h"
#include "StBTofCollection.h"
#include "StBTofHit.h"
#include "StBTofRawHit.h"
#include "StBTofHeader.h"
#include "StBTofUtil/tofPathLength.hh"
#include "StBTofUtil/StBTofGeometry.h"
#include "StBTofUtil/StBTofDaqMap.h"
#include "StBTofUtil/StBTofHitCollection.h"


#include "StMtdMatchMaker.h"
#include "StTimer.hh"
#include "StMemoryInfo.hh"
#include "StMessMgr.h"
#include "TTree.h"
#include "TBranch.h"
#include "mMtdGeom.h"
//!extern TSystem* gSystem;
using namespace std;


StMtdMatchMaker::StMtdMatchMaker(StMuDstMaker* maker, const Char_t *outname="test.root"){
	mMuDstMaker = maker; 
	mMuDstIn = kFALSE;
	doPrintMemoryInfo = kFALSE;
	doPrintCpuInfo    = kFALSE;
	mMinFitPointsPerTrack=20;
	mMinFitPointsOverMax=0.52;
	mCosmicFlag=kFALSE;

	mnNeighbors = 2;
	//mZLocalCut = 43.5;
	mNSigReso = 3.; // n sigma of z and y resolution.
	mOutName = outname;
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
Int_t StMtdMatchMaker::Init(){

	for(int i=0;i<mNAllTrays;i++){
		for(int j=0;j<mNStrips;j++){
			mVDrift[i][j] = 60.;
		}
	}
	bookHistograms();
	return StMaker::Init();
}

void StMtdMatchMaker::bookHistograms(){

	if(mOutName==""){ 
		LOG_WARN <<"Invalid output file! "<<endm;
		return;
	}
	fout = new TFile(mOutName.c_str(),"recreate");
	mEventCounterHisto = new TH1D("eventCounter","eventCounter",20,0,20);

	mCellsMultInEvent = new TH1D("cellsPerEvent","cellsPerEvent",1000,0,1000);
	mHitsMultInEvent  = new TH1D("hitsPerEvent","hitsPerEvent",1000,0,1000);
	mHitsPrimaryInEvent  = new TH1D("hitsPrimaryPerEvent","hitsPrimaryPerEvent",1000,0,1000);
	mHitsMultPerTrack = new TH1D("hitsPerTrack","hitsPerTrack",10,0,10);
	mHitsPosition     = new TH2D("hitsPosition","hitsPositions",1000,-500.,500.,1000,-500.,500);

	// occupancy
	for(int i=0;i<mNBacklegs;i++) {
		char hisname[100];
		sprintf(hisname,"Occupancy_Backleg_%d",i+1);
		mDaqOccupancy[i]     = new TH1D(hisname,";fired cell(module*12+stripId)",60,0,60);
		sprintf(hisname,"OccupancyProj_Backleg_%d",i+1);
		mDaqOccupancyProj[i] = new TH1D(hisname,";projected cell",60,0,60);
	}

	// correlation
	for(int i=0;i<mNBacklegs;i++) {
		char hisname[100];
		sprintf(hisname,"Corr_Backleg_%d",i+1);
		mHitCorr[i] = new TH2D(hisname,";project stripId;fired stripId",60,0,60,60,0,60);
		sprintf(hisname,"Corr_Backleg_%d_module",i+1);
		mHitCorrModule[i] = new TH2D(hisname,";project moduleId;fired moduleId",6,0,6,6,0,6);
	}

	// project hit position
	for(int i=0;i<mNBacklegs;i++) {
		char hisname[100];
		sprintf(hisname,"LocalYZ_Backleg_%d",i+1);
		mDeltaHitFinal[i] = new TH2D(hisname,";localY;localZ",300,-15.,15.,320,-80.,80);
	}

	mTrackPtEta = new TH2D("trackPtEta",";p_{T} (GeV/c);#eta",500,0.,10.,60,-1.5,1.5);
	mTrackPtPhi = new TH2D("trackPtPhi",";p_{T} (GeV/c);#phi",500,0.,10.,120,0.,2.*M_PI);
	mTrackNFitPts = new TH1D("trackNFitPts",";nHitsFit",50,0.,50.);
	mTrackdEdxvsp = new TH2D("trackdEdxvsp",";p_{T} (GeV/c);dE/dx",500,0.,10.,1000,0.,10.);
	mNSigmaPivsPt = new TH2D("nSigmaPivsPt",";p_{T} (GeV/c);n#sigma_{#pi}",500,0.,10.,1000,-10.,10.);


	// association  
	mCellsPerEventMatch1 = new TH1D("cellsPerEventMatch1","cellPerEventMatch1;# hits matched with track(s) per event",10,0,10);
	mHitsPerEventMatch1 = new TH1D("hitsPerEventMatch1","hitsPerEventMatch1;# tracks matched with hits per event",10,0,10);
	mCellsPerTrackMatch1 = new TH1D("cellsPerTrackMatch1","cellsPerTrackMatch1;# tracks matched with same hit",10,0,10);
	mTracksPerCellMatch1 = new TH1D("tracksPerCellMatch1","tracksPerCellMatch1;# hits matched with same track",10,0,10);
	mDaqOccupancyMatch1 = new TH1D("daqPerCellMatch1","daqPerCellMatch1;stripId",60,0,60);
	mDeltaHitMatch1 = new TH2D("deltaHitMatch1","deltaHitMatch1;localY;localZ",300,-15,15,800,-80.,80);

	// kick out multi-hit
	mCellsPerEventMatch2 = new TH1D("cellsPerEventMatch2","cellPerEventMatch2;# hits matched with track(s) per event",10,0,10);
	mHitsPerEventMatch2 = new TH1D("hitsPerEventMatch2","hitsPerEventMatch2;# tracks matched with hits per event",10,0,10);
	mCellsPerTrackMatch2 = new TH1D("cellsPerTrackMatch2","cellsPerTrackMatch2;# tracks matched with same hit",10,0,10);
	mTracksPerCellMatch2 = new TH1D("tracksPerCellMatch2","tracksPerCellMatch2;# hits matched with same track",10,0,10);
	mDaqOccupancyMatch2 = new TH1D("daqPerCellMatch2","daqPerCellMatch2;stripId",60,0,60);
	mDeltaHitMatch2 = new TH2D("deltaHitMatch2","deltaHitMatch2;localY;localZ",300,-15,15,800,-80.,80);

	// sort out multi matched cells
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
	hMtdPhivsProj=new TH2F("hMtdPhivsProj","hMtdPhivsProj",1000,0,TMath::Pi()*2,1000,0,TMath::Pi()*2);
	hMtdZvsProj=new TH2F("hMtdZvsProj","hMtdzvsProj",600,-300,300,600,-300,300);


	mMtdEvt = new TTree("mtdEvent","mtdEvent");
	mMtdEvt->SetAutoSave(1000000);
	mMtdEvt->Branch("run",&mMtdEvtData.run,"run/I");
	mMtdEvt->Branch("evt",&mMtdEvtData.evt,"evt/I");
	mMtdEvt->Branch("trgId",&mMtdEvtData.trgId,"trgId/I");
	mMtdEvt->Branch("bField",&mMtdEvtData.bField,"bField/F");
	mMtdEvt->Branch("vertexX",&mMtdEvtData.vertexX,"vertexX/F");
	mMtdEvt->Branch("vertexY",&mMtdEvtData.vertexY,"vertexY/F");
	mMtdEvt->Branch("vertexZ",&mMtdEvtData.vertexZ,"vertexZ/F");
	mMtdEvt->Branch("nMtdRawHits",&mMtdEvtData.nMtdRawHits,"nMtdRawHits/I");
	mMtdEvt->Branch("nMtdHits",&mMtdEvtData.nMtdHits,"nMtdHits/I");
	mMtdEvt->Branch("triggerTime",&mMtdEvtData.triggerTime,"triggerTime/D");

	//raw hits
	mMtdEvt->Branch("flag",&mMtdEvtData.flag,"flag[nMtdRawHits]/B");
	mMtdEvt->Branch("backlegRaw",&mMtdEvtData.backlegRaw,"backlegRaw[nMtdRawHits]/b");
	mMtdEvt->Branch("chn",&mMtdEvtData.chn,"chn[nMtdRawHits]/b");
	mMtdEvt->Branch("tdc",&mMtdEvtData.tdc,"tdc[nMtdRawHits]/D");

	//sorted hits
	mMtdEvt->Branch("backleg",&mMtdEvtData.backleg,"backleg[nMtdHits]/b");
	mMtdEvt->Branch("module",&mMtdEvtData.module,"module[nMtdHits]/b");
	mMtdEvt->Branch("cell",&mMtdEvtData.cell,"cell[nMtdHits]/b");

	mMtdEvt->Branch("leTimeWest",&mMtdEvtData.leTimeWest,"leTimeWest[nMtdHits]/D");
	mMtdEvt->Branch("leTimeEast",&mMtdEvtData.leTimeEast,"leTimeEast[nMtdHits]/D");
	mMtdEvt->Branch("totWest",&mMtdEvtData.totWest,"totWest[nMtdHits]/D");
	mMtdEvt->Branch("totEast",&mMtdEvtData.totEast,"totEast[nMtdHits]/D");

	//global tracks
	mMtdEvt->Branch("ngTracks",&mMtdEvtData.ngTracks,"ngTracks/I");
	mMtdEvt->Branch("gpt",&mMtdEvtData.gpt,"gpt[ngTracks]/F");
	mMtdEvt->Branch("geta",&mMtdEvtData.geta,"geta[ngTracks]/F");
	mMtdEvt->Branch("gphi",&mMtdEvtData.gphi,"gphi[ngTracks]/F");

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


	//project to MTD
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

	//Matched Mtd Hits
	mMtdEvt->Branch("gnMatchMtdHits",&mMtdEvtData.gnMatchMtdHits,"gnMatchMtdHits[ngTracks]/I");
	mMtdEvt->Branch("gmMtdHitIndex",&mMtdEvtData.gmMtdHitIndex,"gmMtdHitIndex[ngTracks]/I");
	mMtdEvt->Branch("gmBackLeg",&mMtdEvtData.gmBackLeg,"gmBackLeg[ngTracks]g/b");
	mMtdEvt->Branch("gmModule",&mMtdEvtData.gmModule,"gmModule[ngTracks]g/b");
	mMtdEvt->Branch("gmCell",&mMtdEvtData.gmCell,"gmCell[ngTracks]g/b");
	mMtdEvt->Branch("gmLeTimeWest",&mMtdEvtData.gmLeTimeWest,"gmLeTimeWest[ngTracks]g/F");
	mMtdEvt->Branch("gmTotWest",&mMtdEvtData.gmTotWest,"gmTotWest[ngTracks]g/F");
	mMtdEvt->Branch("gmLeTimeEast",&mMtdEvtData.gmLeTimeEast,"gmLeTimeEast[ngTracks]g/F");
	mMtdEvt->Branch("gmTotEast",&mMtdEvtData.gmTotEast,"gmTotEast[ngTracks]g/F");
	mMtdEvt->Branch("gmLocalZ",&mMtdEvtData.gmLocalZ,"gmLocalZ[ngTracks]g/F");
	mMtdEvt->Branch("gmLocalY",&mMtdEvtData.gmLocalY,"gmLocalY[ngTracks]g/F");
}

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

Int_t StMtdMatchMaker::FinishRun(int runnumber)
{

	delete mBeamHelix;

	return kStOK;
}

Int_t StMtdMatchMaker::Finish(){
	if(mOutName!=""){
		//LOG_INFO<<" writing file "<<endl;
		fout->cd();
		mMtdEvt->Write();
		fout->Write();
		fout->Close();
	}

	return kStOK;
}
Int_t StMtdMatchMaker::Make(){
	LOG_DEBUG<< "StMtdMatchMaker -- welcome" << endm;
	if(mSaveTree) initEventData();

	if(mMuDstIn) processMuDst();
	else         processStEvent();

	return kStOK;
}

//---------------------------------------------------------------------------
void StMtdMatchMaker::processStEvent(){

	if(mHisto) mEventCounterHisto->Fill(0);
	StTimer timer;
	if(doPrintCpuInfo) timer.start();
	if(doPrintMemoryInfo) StMemoryInfo::instance()->snapshot();
	// read data from StMtdHit
	// A. build vector of candidate cells
	//
	mtdCellHitVector daqCellsHitVec;
	idVector validModuleVec;
	if(!readMtdHits(daqCellsHitVec,validModuleVec)) return;
	if(mHisto) mEventCounterHisto->Fill(1);
	//end of Sect.A
	if(Debug()){
		LOG_INFO<<" Sect.A: =============================="<<endm;
		LOG_INFO <<" total # of cells =" << daqCellsHitVec.size() << endm;
		for(size_t iv=0;iv<validModuleVec.size();iv++){
			LOG_INFO << " module# "<< validModuleVec[iv] <<"Valid! "<<endm;
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
	//
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
	StMtdCollection *theMtd = mEvent->mtdCollection();
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
	return;
}
//---------------------------------------------------------------------------
Bool_t StMtdMatchMaker::readMtdHits(mtdCellHitVector& daqCellsHitVec,idVector& validModuleVec){

	// StMuMtdCollection can't save Hits yet. Do not support MuDst Mode.

	/*
	if(mMuDstIn){
		StMuDstMaker *mMuDstMaker = (StMuDstMaker *)GetMaker("MuDst");
		if(!mMuDstMaker){
			LOG_INFO << "No MuDstMaker ..bye-bye..."<<endm;
			return kFALSE;
		}
		mMuDst=mMuDstMaker->muDst();
		if(!mMuDst){
			LOG_INFO << "No Mudst ... bye-bye" <<endm;
			return kFALSE;
		}
		if(!(mMuDst->MtdCollection())){
			LOG_INFO <<"no mtd collection ... bye-bye"<<endm;
			return kFALSE;
		}
		if(!(mMuDst->MtdCollection()->hitsPresent())){
			LOG_INFO << "no mtd hit present! ... bye-bye"<<endm;
			return kFALSE;
		}
		StMuMtdCollection* muMtdCollection=mMuDst->MtdCollection();
		//StMuMtdHeader* MuMtdHeader = muMtdCollection->mtdHeader();
		LOG_DEBUG << "Number of Mtd Hits = " << muMtdCollection->hitsPresent() <<endm;
		for(Int_t i=0;i<muMtdCollection->hitsPresent();i++){
			StMuMtdHit* aHit =muMtdCollection->MtdHit(i) ;
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
			aDaqCellHit.index2MtdHit=i;
			daqCellsHitVec.push_back(aDaqCellHit);

			//additional valid number configuration
			int id=backlegId*100+moduleId;
			if(find(validModuleVec.begin(),validModuleVec.end(),id) == validModuleVec.end())
				validModuleVec.push_back(id);
		}
		StMuMtdHeader* mtdHeader=muMtdCollection->mtdHeader();
		unsigned int trgTime=mtdHeader->triggerTime(0);
		Double_t triggerTime=25.*(trgTime&0xfff);//ns

		if(mSaveTree){

			int trgId = 0;
			if(mMuDst->event()->triggerIdCollection().nominal().isTrigger(310810)) trgId = 310810;
			if(mMuDst->event()->triggerIdCollection().nominal().isTrigger(310811)) trgId = 310811;
			if(mMuDst->event()->triggerIdCollection().nominal().isTrigger(310812)) trgId = 310812;
			if(mMuDst->event()->triggerIdCollection().nominal().isTrigger(310813)) trgId = 310813;
			mMtdEvtData.run = mEvent->runId();       // the run number
			mMtdEvtData.evt = mEvent->id();       // the event number
			mMtdEvtData.trgId = trgId;       // 
			mMtdEvtData.bField= mEvent->runInfo()->magneticField()/10.; 
			float xvtx = -999.;
			xvtx = mEvent->primaryVertex()->position().x();
			float yvtx = -999.;
			yvtx = mEvent->primaryVertex()->position().y();
			float zvtx = -999.;
			zvtx = mEvent->primaryVertex()->position().z();

			mMtdEvtData.vertexX = xvtx;        
			mMtdEvtData.vertexY = yvtx;              
			mMtdEvtData.vertexZ = zvtx;              

			mMtdEvtData.triggerTime = triggerTime; // ns 

			mMtdEvtData.nMtdRawHits = muMtdCollection->rawHitsPresent();  
			mMtdEvtData.nMtdHits = muMtdCollection->hitsPresent(); 

			for(Int_t i=0;i<muMtdCollection->rawHitsPresent();i++){
				StMuMtdRawHit* aRawHit=muMtdCollection->RawMtdHit(i);
				int mrflag=aRawHit->flag();
				int mrbackleg=aRawHit->backleg();
				int mrchn=aRawHit->channel();
				int mrtdc=aRawHit->tdc();
				mMtdEvtData.flag[i] = mrflag;
				mMtdEvtData.backlegRaw[i] = mrbackleg;
				mMtdEvtData.chn[i] = mrchn;
				mMtdEvtData.tdc[i] = mrtdc;
				//cout<<"i:"<<i<<" mrflag:"<<mrflag<<" mrbackleg:"<<mrbackleg<<" mrchannel:"<<mrchn<<" mrtdc:"<<mrtdc<<endl;
			}

			LOG_DEBUG << "Number of Mtd Raw Hits = " << muMtdCollection->rawHitsPresent() <<endm;
			LOG_DEBUG << "Number of Mtd Hits = " << muMtdCollection->hitsPresent()<<endm;
			for(Int_t i=0;i<muMtdCollection->hitsPresent();i++){
				StMuMtdHit* aHit = muMtdCollection->MtdHit(i);
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
		for(Int_t i=0;i<muMtdCollection->hitsPresent();i++){
			StMuMtdHit* aHit = muMtdCollection->MtdHit(i);
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

	}else{  
		*/

		mEvent=(StEvent *) GetInputDS("StEvent");
		if(!mEvent||!(mEvent->mtdCollection())||!(mEvent->mtdCollection()->hitsPresent())){
			if(!mEvent){LOG_INFO << "no StEvent" <<endm; return kFALSE;}
			else if(!(mEvent->mtdCollection())) {
				LOG_INFO << "no MTD Collection" <<endm;
				return kFALSE;
			}
			else if(!(mEvent->mtdCollection()->hitsPresent())){LOG_DEBUG << "no MTD hits present" <<endm;}
			return kFALSE;
		}

		StMuDstMaker *mMuDstMaker = (StMuDstMaker *)GetMaker("MuDst");
		if(!mMuDstMaker){
			LOG_INFO << "No MuDstMaker ..bye-bye..."<<endm;
			return kFALSE;
		}
		mMuDst=mMuDstMaker->muDst();
		if(!mMuDst){
			LOG_INFO << "No Mudst ... bye-bye" <<endm;
			return kFALSE;
		}

		//QA test
		//.........................................................................
		// check for mtdCollection and fill local copy with ADC and TDC data
		StMtdCollection *theMtd = mEvent->mtdCollection();
		//.........................................................................
		// read data from StMtdHit
		//

		//multi-tray system
		//meventId++;
		//test
		StMtdHeader* mtdHeader=theMtd->mtdHeader();
		unsigned int trgTime=mtdHeader->triggerTime(0);
		Double_t triggerTime=25.*(trgTime&0xfff);//ns

		StSPtrVecMtdHit& mtdHits= theMtd->mtdHits();
		StSPtrVecMtdRawHit& mtdRawHits=theMtd->mtdRawHits();
		if(mSaveTree){

			int trgId = 0;
			if(mMuDst->event()->triggerIdCollection().nominal().isTrigger(310810)) trgId = 310810;
			if(mMuDst->event()->triggerIdCollection().nominal().isTrigger(310811)) trgId = 310811;
			if(mMuDst->event()->triggerIdCollection().nominal().isTrigger(310812)) trgId = 310812;
			if(mMuDst->event()->triggerIdCollection().nominal().isTrigger(310813)) trgId = 310813;
			mMtdEvtData.run = mEvent->runId();       // the run number
			mMtdEvtData.evt = mEvent->id();       // the event number
			mMtdEvtData.trgId = trgId;       // 
			mMtdEvtData.bField= mEvent->runInfo()->magneticField()/10.; 
			float xvtx = -999.;
			xvtx = mEvent->primaryVertex()->position().x();
			float yvtx = -999.;
			yvtx = mEvent->primaryVertex()->position().y();
			float zvtx = -999.;
			zvtx = mEvent->primaryVertex()->position().z();

			mMtdEvtData.vertexX = xvtx;        
			mMtdEvtData.vertexY = yvtx;              
			mMtdEvtData.vertexZ = zvtx;              

			mMtdEvtData.triggerTime = triggerTime; // ns 

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
	//}
	return kTRUE;
}
void StMtdMatchMaker::project2Mtd(mtdCellHitVector daqCellsHitVec,mtdCellHitVector& allCellsHitVec,Int_t& nPrimaryHits){
	if(mMuDstIn){
		int nAllTracks=0;
		ngTracks = 0;
		for(UInt_t iNode=0;iNode<mMuDst->numberOfGlobalTracks();iNode++){
			StMuTrack *theTrack=mMuDst->globalTracks(iNode);
			if(!theTrack) continue;
			bool isPrimary=kFALSE;
			const StMuTrack *thePrimaryTrack=theTrack->primaryTrack();
			if(thePrimaryTrack) isPrimary=kTRUE;
			if(!validTrack(theTrack)) continue;

			StThreeVectorF mom = theTrack->momentum();
			float pt=mom.perp();
			float eta=mom.pseudoRapidity();
			float phi=mom.phi();
			while(phi<0.)phi+=2.*(TMath::Pi());
			while(phi>2*(TMath::Pi()))phi-=2.*(TMath::Pi());
			float dEdx=-999.;
			int ndEdxpts=0;

			float nSigmaE = -999.,nSigmaPi = -999.,nSigmaK = -999.,nSigmaP = -999.;
			nSigmaE  = theTrack->nSigmaElectron(); 
			nSigmaPi = theTrack->nSigmaPion();
			nSigmaK  = theTrack->nSigmaKaon();
			nSigmaP  = theTrack->nSigmaProton();

			dEdx = theTrack->dEdx();
			ndEdxpts = theTrack->nHitsDedx();
			int nfitpts = theTrack->nHitsFit();

			if(mHisto){
				mTrackPtEta->Fill(pt, eta);
				mTrackPtPhi->Fill(pt, phi);
				mTrackNFitPts->Fill(nfitpts);
				if(dEdx>0.) mTrackdEdxvsp->Fill(mom.mag(), dEdx*1.e6);
				if(fabs(nSigmaPi)<5.) mNSigmaPivsPt->Fill(pt, nSigmaPi+5.*theTrack->charge());
			}
			StThreeVectorD globalPos;
			if(mSaveTree){
				float gpt 		= theTrack->momentum().perp();
				int   gq		= theTrack->charge();
				float geta 		= theTrack->momentum().pseudoRapidity();
				float gphi 		= theTrack->momentum().phi();
				int   gnFtPts 	= nfitpts;

				StPhysicalHelixD helix = theTrack->outerHelix();
				StThreeVector<double> helixOrigin = helix.origin();
				StThreeVector<double> helixMomentum = helix.momentum(mEvent->runInfo()->magneticField()*kilogauss);
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
				mMtdEvtData.ghelixpx[ngTracks] = ghelixpx;
				mMtdEvtData.ghelixpy[ngTracks] = ghelixpy;
				mMtdEvtData.ghelixpz[ngTracks] = ghelixpz;
				mMtdEvtData.ghelixox[ngTracks] = ghelixox;
				mMtdEvtData.ghelixoy[ngTracks] = ghelixoy;
				mMtdEvtData.ghelixoz[ngTracks] = ghelixoz;
				int idx2primary = -1;
				if(isPrimary) idx2primary = thePrimaryTrack->id();
				mMtdEvtData.gIndex2Primary[ngTracks] = idx2primary;

				mMtdEvtData.gnFtPts[ngTracks] = gnFtPts;
				mMtdEvtData.gdedx[ngTracks] = dEdx;
				mMtdEvtData.gnDedxPts[ngTracks] =  ndEdxpts;

				const StMuBTofPidTraits tofpid=theTrack->btofPidTraits();
				const StMuBTofHit* aHit = theTrack->tofHit();
				if(aHit){
					int tray = aHit->tray();	
					int module = aHit->module();	
					int cell = aHit->cell();	
					mMtdEvtData.gchannel[ngTracks] = (tray-1)*192+(module-1)*6+cell-1;
					mMtdEvtData.gyLocal[ngTracks] = tofpid.yLocal();
					mMtdEvtData.gzLocal[ngTracks] = tofpid.zLocal();
					globalPos = tofpid.position();
					//mMtdEvtData.xGlobal[ngTracks] = globalPos.x();
					//mMtdEvtData.yGlobal[ngTracks] = globalPos.y();
					//mMtdEvtData.zGlobal[ngTracks] = globalPos.z();
					LOG_DEBUG<<" globalPos x,y,z :"<<globalPos.x()<<","<<globalPos.y()<<","<<globalPos.z()<<endm;
					mMtdEvtData.gtdc[ngTracks] = aHit->leadingEdgeTime();
					mMtdEvtData.gtot[ngTracks] = aHit->tot();
					mMtdEvtData.gtof[ngTracks] = tofpid.timeOfFlight();
					mMtdEvtData.gpathLength[ngTracks] = tofpid.pathLength();
					mMtdEvtData.gbeta[ngTracks] = tofpid.beta();
					LOG_DEBUG <<" project2MTD() Found matched TOF hit: tray:"<< tray << " module:"<<module<<" cell:"<<cell<<" gtdc:"<<aHit->leadingEdgeTime()<<" gtof:"<<aHit->tot()<<" gtof:"<<tofpid.timeOfFlight()<<" gpathLength:"<<tofpid.pathLength()<<" gbeta:"<<tofpid.beta()<<endm;

				}
				mMtdEvtData.gnSigmaE[ngTracks] = nSigmaE;
				mMtdEvtData.gnSigmaPi[ngTracks] = nSigmaPi;
				mMtdEvtData.gnSigmaK[ngTracks] = nSigmaK;
				mMtdEvtData.gnSigmaP[ngTracks] = nSigmaP;

				if(matchTrack2Mtd(daqCellsHitVec,theTrack,allCellsHitVec,iNode,globalPos)){

					nAllTracks++;
					if(isPrimary) nPrimaryHits++;
				}
				ngTracks++;
			}
		}
		if(mSaveTree){
			mMtdEvtData.ngTracks = ngTracks;
			LOG_DEBUG <<" project2MTD() Found "<<ngTracks<<" global tracks in this event"<<endm;
		}

	}else{

		StBTofCollection *btofColl = mEvent->btofCollection();
		double mTriggerTime[4] ;
		for(int i=0;i<4;i++) mTriggerTime[i] = btofColl->tofHeader()->triggerTime(i);	
		StSPtrVecTrackNode& nodes=mEvent->trackNodes();
		Int_t nAllTracks=0;
		ngTracks = 0;
		//StSPtrVecBTofHit& tofHits = btofColl->tofHits();
		for(unsigned int iNode=0;iNode<nodes.size();iNode++){
			//		mtdCellHitVector cellHitVec;
			StGlobalTrack *theTrack = dynamic_cast<StGlobalTrack*>(nodes[iNode]->track(global));
			if(!theTrack) continue;
			bool isPrimary =kFALSE;
			StPrimaryTrack *pTrack =dynamic_cast<StPrimaryTrack*>(theTrack->node()->track(primary));
			if(pTrack) isPrimary = kTRUE;
			StThreeVectorF mom = theTrack->geometry()->momentum();
			float pt=mom.perp();
			float eta=mom.pseudoRapidity();
			float phi=mom.phi();
			while(phi<0.)phi+=2.*(TMath::Pi());
			while(phi>2*(TMath::Pi()))phi-=2.*(TMath::Pi());
			float dEdx=-999.;
			int ndEdxpts=0;

			static StTpcDedxPidAlgorithm PidAlgorithm;
			static StElectron* Electron = StElectron::instance();
			static StPionPlus* Pion = StPionPlus::instance();
			static StKaonPlus* Kaon = StKaonPlus::instance();
			static StProton* Proton = StProton::instance();
			const StParticleDefinition* pd = theTrack->pidTraits(PidAlgorithm);

			float nSigmaE = -999.,nSigmaPi = -999.,nSigmaK = -999.,nSigmaP = -999.;
			if (pd) {
				nSigmaE  = PidAlgorithm.numberOfSigma(Electron);
				nSigmaPi = PidAlgorithm.numberOfSigma(Pion);
				nSigmaK  = PidAlgorithm.numberOfSigma(Kaon);
				nSigmaP  = PidAlgorithm.numberOfSigma(Proton);
			}

			if(PidAlgorithm.traits()){
				dEdx = PidAlgorithm.traits()->mean();
				ndEdxpts=PidAlgorithm.traits()->numberOfPoints();
			}
			int nfitpts=theTrack->fitTraits().numberOfFitPoints(kTpcId);
			if(!validTrack(theTrack)) continue;
			//-- nSigma

			if(mHisto){
				mTrackPtEta->Fill(pt, eta);
				mTrackPtPhi->Fill(pt, phi);
				mTrackNFitPts->Fill(nfitpts);
				if(dEdx>0.) mTrackdEdxvsp->Fill(mom.mag(), dEdx*1.e6);
				if(fabs(nSigmaPi)<5.) mNSigmaPivsPt->Fill(pt, nSigmaPi+5.*theTrack->geometry()->charge());
			}

			StThreeVectorD globalPos;
			if(mSaveTree){
				float gpt 		= theTrack->geometry()->momentum().perp();
				int   gq		= theTrack->geometry()->charge();
				float geta 		= theTrack->geometry()->momentum().pseudoRapidity();
				float gphi 		= theTrack->geometry()->momentum().phi();
				int   gnFtPts 	= theTrack->fitTraits().numberOfFitPoints(kTpcId);

				StPhysicalHelixD helix = theTrack->outerGeometry()->helix();
				StThreeVector<double> helixOrigin = helix.origin();
				StThreeVector<double> helixMomentum = helix.momentum(mEvent->runInfo()->magneticField()*kilogauss);
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
				mMtdEvtData.ghelixpx[ngTracks] = ghelixpx;
				mMtdEvtData.ghelixpy[ngTracks] = ghelixpy;
				mMtdEvtData.ghelixpz[ngTracks] = ghelixpz;
				mMtdEvtData.ghelixox[ngTracks] = ghelixox;
				mMtdEvtData.ghelixoy[ngTracks] = ghelixoy;
				mMtdEvtData.ghelixoz[ngTracks] = ghelixoz;
				int idx2primary = -1;
				if(isPrimary) idx2primary = pTrack->key();
				mMtdEvtData.gIndex2Primary[ngTracks] = idx2primary;

				mMtdEvtData.gnFtPts[ngTracks] = gnFtPts;
				StSPtrVecTrackPidTraits& traits = theTrack->pidTraits();
				//StSPtrVecTrackPidTraits& ptraits = pTrack->pidTraits();
				LOG_DEBUG <<" project2MTD() Found track: gpt "<< gpt << " gq:"<<gq<<" geta:"<<geta<<" gphi:"<<gphi
					<<" gnFtPts:"<<gnFtPts<<endm;
				for (unsigned int it=0;it<traits.size();it++){
					if (traits[it]->detector() == kTpcId){
						StDedxPidTraits *pid = dynamic_cast<StDedxPidTraits*>(traits[it]);
						if (pid && pid->method() ==kTruncatedMeanId){
							mMtdEvtData.gdedx[ngTracks] = pid->mean()*1e6;
							mMtdEvtData.gnDedxPts[ngTracks] =  pid->numberOfPoints();
						}
					} else if (traits[it]->detector() == kTofId) {
						StBTofPidTraits* tofpid = dynamic_cast<StBTofPidTraits*>(traits[it]);

						StBTofHit* aHit = tofpid->tofHit();

						if(tofpid && aHit){
							int tray = aHit->tray();	
							int module = aHit->module();	
							int cell = aHit->cell();	
							mMtdEvtData.gchannel[ngTracks] = (tray-1)*192+(module-1)*6+cell-1;
							mMtdEvtData.gyLocal[ngTracks] = tofpid->yLocal();
							mMtdEvtData.gzLocal[ngTracks] = tofpid->zLocal();
							globalPos = tofpid->position();
							//mMtdEvtData.xGlobal[ngTracks] = globalPos.x();
							//mMtdEvtData.yGlobal[ngTracks] = globalPos.y();
							//mMtdEvtData.zGlobal[ngTracks] = globalPos.z();
							LOG_DEBUG<<" globalPos x,y,z :"<<globalPos.x()<<","<<globalPos.y()<<","<<globalPos.z()<<endm;
							mMtdEvtData.gtdc[ngTracks] = aHit->leadingEdgeTime();
							mMtdEvtData.gtot[ngTracks] = aHit->tot();
							mMtdEvtData.gtof[ngTracks] = tofpid->timeOfFlight();
							mMtdEvtData.gpathLength[ngTracks] = tofpid->pathLength();
							mMtdEvtData.gbeta[ngTracks] = tofpid->beta();
						}
					}
				}

				mMtdEvtData.gnSigmaE[ngTracks] = nSigmaE;
				mMtdEvtData.gnSigmaPi[ngTracks] = nSigmaPi;
				mMtdEvtData.gnSigmaK[ngTracks] = nSigmaK;
				mMtdEvtData.gnSigmaP[ngTracks] = nSigmaP;

			}
			if(matchTrack2Mtd(daqCellsHitVec,theTrack, allCellsHitVec,iNode,globalPos)){
				nAllTracks++;
				if(isPrimary) nPrimaryHits++;
			}
			ngTracks++;
		}
		if(mSaveTree){
			mMtdEvtData.ngTracks = ngTracks;
			LOG_DEBUG <<" project2MTD() Found "<<ngTracks<<" global tracks in this event"<<endm;
		}
	}
}

bool StMtdMatchMaker::matchTrack2Mtd(mtdCellHitVector daqCellsHitVec,StPhysicalHelixD helix, Int_t gq, mtdCellHitVector& allCellsHitVec, unsigned int iNode, StThreeVectorD globalPos){

	StThreeVector<double> helixOrigin = helix.origin();
	StThreeVector<double> helixMomentum = helix.momentum(mEvent->runInfo()->magneticField()*kilogauss);
	float ghelixpx  = helixMomentum.x();
	float ghelixpy  = helixMomentum.y();
	float ghelixpz  = helixMomentum.z();
	float ghelixox  = helixOrigin.x();
	float ghelixoy  = helixOrigin.y();
	float ghelixoz  = helixOrigin.z();

	float bField = mEvent->runInfo()->magneticField()/10.;
	StThreeVector<double> g1P(ghelixpx,ghelixpy,ghelixpz);//momentum 
	StThreeVector<double> g1O(ghelixox,ghelixoy,ghelixoz);//origin
	StPhysicalHelixD gHelixTpc(g1P,g1O,bField*tesla,gq); 


	StThreeVectorF vertexPos = mEvent->summary()->primaryVertexPosition();
	double length2Vtx 	  = TMath::Abs(gHelixTpc.pathLength(vertexPos));
	LOG_DEBUG <<"StMtdMatchMaker::matchTrack2Mtd() "<<" bField"<<bField<<endm;
	LOG_DEBUG<<" vertex x,y,z:"<<vertexPos.x()<<","<<vertexPos.y()<<","<<vertexPos.z()<<endm;
	LOG_DEBUG<<" gq:"<<gq<<" ghelix ox,oy,oz:"<<ghelixox<<","<<ghelixoy<<","<<ghelixoz
		<<" ghelix p,pt,eta,phi:"<<helixMomentum.mag()<<","<<helixMomentum.perp()<<","<<helixMomentum.pseudoRapidity()<<","<<helixMomentum.phi()
		<<" length2vertex:"<<length2Vtx<<endm;

	StThreeVector<double> dcaPos  = gHelixTpc.at(gHelixTpc.pathLengths(*mBeamHelix).first);
	StThreeVector<double> beamPos = mBeamHelix->at(gHelixTpc.pathLengths(*mBeamHelix).second);
	StThreeVector<double> dca 	  = dcaPos - beamPos;

	//project track to TOF radius

	double rTof = gHelixTpc.pathLength(globalPos);
	StThreeVector<double> tofPos  = globalPos;
	StThreeVector<double> tofMom = gHelixTpc.momentumAt(rTof,bField*tesla);

	double betaGam = g1P.mag()/massMu;
	double vInner  = sqrt(betaGam*betaGam/(1+betaGam*betaGam))*vLight;
	double tof2Tof = (length2Vtx+rTof)/vInner;

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

	double tof2InnerEMC = (length2Vtx+rInnerEMC)/vInner;

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

	betaGam = innerBSMDMom.mag()/massMu;
	vInner  = sqrt(betaGam*betaGam/(1+betaGam*betaGam))*vLight;
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

	betaGam = outerBSMDMom.mag()/massMu;
	vInner  = sqrt(betaGam*betaGam/(1+betaGam*betaGam))*vLight;
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

		betaGamEMC 	= EMCLayerMom.mag()/massMu;
		vEMC  	    = sqrt(betaGamEMC*betaGamEMC/(1.+betaGamEMC*betaGamEMC))*vLight;
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
	//Float_t mInnerSteelOx,mInnerSteelOy,mInnerSteelOz,mInnerSteelPx,mInnerSteelPy,mInnerSteelPz;
	double rInnerSteel =  (sInnerSteel.first < 0 || sInnerSteel.second < 0) 
		? max(sInnerSteel.first, sInnerSteel.second) : min(sInnerSteel.first, sInnerSteel.second); 
	StThreeVector<double> innerSteelPos = helixOutEMC.at(rInnerSteel);
	StThreeVector<double> innerSteelMom = helixOutEMC.momentumAt(rInnerSteel,bField*tesla);

	betaGam = innerSteelMom.mag()/massMu;
	vInner  = sqrt(betaGam*betaGam/(1+betaGam*betaGam))*vLight;
	double tof2InnerSteel = rInnerSteel/vInner;

	LOG_DEBUG<<" to InnerSteel: pos x,y,z:"<<innerSteelPos.x()<<","<<innerSteelPos.y()<<","<<innerSteelPos.z()<<endm;
	LOG_DEBUG<<" to InnerSteel: mom p,pt,eta,phi:"<<innerSteelMom.mag()<<","<<innerSteelMom.perp()<<","<<innerSteelMom.pseudoRapidity()<<","<<innerSteelMom.phi()<<endm;
	LOG_DEBUG<<" to InnerSteel: tof:"<<tof2InnerSteel<<endm;

	//step 2: outer steel, 10 steps for 60cm stell by 6 cm per step. 0.074GeV of energy loss for momentum degradation.
	double bFieldInSteel = -1.26;
	if(bField>0) bFieldInSteel = -1.26;
	if(bField<0) bFieldInSteel = 1.26;

	StThreeVector<double> steelLayerPos = innerSteelPos;
	StThreeVector<double> steelLayerMom = innerSteelMom;
	StThreeVector<double> tmpLayerPos = innerSteelPos;
	StThreeVector<double> tmpLayerMom = innerSteelMom;
	double eLoss = 0.074;
	if(mCosmicFlag) eLoss *= -1.;
	double rStep = 6.096; //cm
	const int nStep = 10;
	double lengthLayer[nStep];
	double tofLayer[nStep];
	double vSteel;
	double betaGamSteel;
	for( int i=0; i<nStep; i++){
		double steelLayerRadius = innerSteelRadius+rStep*(i+1);
		StPhysicalHelixD helixInSteel(steelLayerMom,steelLayerPos,bFieldInSteel*tesla,gq);

		pairD sSteelLayer = helixInSteel.pathLength(steelLayerRadius);
		if(sSteelLayer.first<=0 && sSteelLayer.second<=0) return kFALSE;

		double rSteelLayer = (sSteelLayer.first < 0 || sSteelLayer.second < 0) 
			? max(sSteelLayer.first, sSteelLayer.second) : min(sSteelLayer.first, sSteelLayer.second); 

		betaGamSteel    = steelLayerMom.mag()/massMu;
		vSteel  	    = sqrt(betaGamSteel*betaGamSteel/(1+betaGamSteel*betaGamSteel))*vLight;
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
	double bFieldOutSteel = 0.;
	StPhysicalHelixD helixOutSteel(steelLayerMom,steelLayerPos,bFieldOutSteel*tesla,gq);
	pairD sMtd = helixOutSteel.pathLength(mtdRadius);
	if(sMtd.first<=0 && sMtd.second<=0) return kFALSE;
	double rMtd =  (sMtd.first < 0 || sMtd.second < 0) 
		? max(sMtd.first, sMtd.second) : min(sMtd.first, sMtd.second); 

	StThreeVector<double> mtdPos = helixOutSteel.at(rMtd);
	StThreeVector<double> mtdMom = steelLayerMom;
	double betaGamOuter = steelLayerMom.mag()/massMu;
	double vOuter  	    = sqrt(betaGamOuter*betaGamOuter/(1+betaGamOuter*betaGamOuter))*vLight;
	double tof2Outer = rMtd/vOuter;

	LOG_DEBUG<<" to MTD: pos x,y,z:"<<mtdPos.x()<<","<<mtdPos.y()<<","<<mtdPos.z()<<endm;
	LOG_DEBUG<<" to MTD: mom p,pt,eta,phi:"<<mtdMom.mag()<<","<<mtdMom.perp()<<","<<mtdMom.pseudoRapidity()<<","<<mtdMom.phi()<<endm;
	LOG_DEBUG<<" to MTD: tof:"<<tof2Outer<<endm;

	double length2Tof     = length2Vtx+rTof;
	double length2Mtd[2] = {0};
	double length2SteelOuter = 0.;
	length2SteelOuter += length2Vtx+rInnerEMC;
	length2SteelOuter += rInnerBSMD+rOuterBSMD;
	for(int i=0;i<nEMCStep;i++) length2SteelOuter += EMClengthLayer[i];
	length2SteelOuter += rInnerSteel;
	for(int i=0;i<nStep;i++) length2SteelOuter += lengthLayer[i];
	for(int i=0;i<2;i++) length2Mtd[i] = length2SteelOuter;

	double tof2Mtd[2] = {0};
	double tof2SteelOuter = 0.;
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

	Int_t channel = mMtdEvtData.gchannel[ngTracks];

	Double_t muTofPhi,muTofZ;
	decodeTofPhiZ(channel,muTofPhi,muTofZ);
	if(muTofPhi<0) muTofPhi+=2*TMath::Pi();
	if(projTofPhi<0) projTofPhi+=2*TMath::Pi();

	double projMtdPhi = mtdPos.phi();
	double projMtdZ   = mtdPos.z();

	if(projTofPhi<0) 	 projTofPhi 	+= 2.*(TMath::Pi()); // -pi,pi --> 0,2*pi
	if(projMtdPhi<0) 	 projMtdPhi 	+= 2.*(TMath::Pi()); // -pi,pi --> 0,2*pi

	double dphi = backLegPhiWidth+backLegPhiGap;

	int projMtdBackLeg = -1;
	projMtdBackLeg = projMtdPhi/dphi;
	projMtdBackLeg += 24;
	if(projMtdBackLeg>30) projMtdBackLeg -= 30;

	int projMtdModule = -1;
	projMtdModule = (projMtdZ+2.5*stripLength)/stripLength+1;
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
			cellCan[i]=(local[i].y()+(stripWidth+stripGap)*nStrips/2.)/(stripWidth+stripGap);
		}
	}

	double LowEdge = mFirstBackLegPhi+(projMtdBackLeg-1.)*(backLegPhiWidth+backLegPhiGap)-(nChannels/4.)*(stripWidth+stripGap)/mtdRadius;
	if(LowEdge > 2.*(TMath::Pi())) LowEdge -= 2.*(TMath::Pi());
	if(LowEdge < 0) 	LowEdge += 2.*(TMath::Pi());
	double dCellPhi = projMtdPhi - LowEdge;
	if(dCellPhi<0) 		dCellPhi += 2.*(TMath::Pi());
	if(dCellPhi>2.*(TMath::Pi()))  dCellPhi -= 2.*(TMath::Pi());
	int projMtdCell = -1;
	projMtdCell	= dCellPhi/((stripWidth+stripGap)/mtdRadius);
	for(int i=0;i<2;i++){
		LOG_DEBUG<<"i "<<i<<" projMtdBackleg:"<<projMtdBackLeg<<" projMtdModule: "<<moduleCan[i]<<" projMtdCell: "<<cellCan[i]<<endm;
	}

	if(projMtdModule>3) projMtdCell = 11 - projMtdCell; // reversed 
	LOG_DEBUG<<"projMtdModule:"<<projMtdModule<<" projMtdCell:"<<projMtdCell<<" cellCan[0]:"<<cellCan[0]<<" cellCan[1]"<<cellCan[1]<<endm;
	Int_t nCells;

	StructCellHit cellHit;
	for(int i=0;i<2;i++){
		if(moduleCan[i]<1||moduleCan[i]>5) continue;
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

bool StMtdMatchMaker::matchTrack2Mtd(mtdCellHitVector daqCellsHitVec,StTrack *theTrack, mtdCellHitVector& allCellsHitVec, unsigned int iNode, StThreeVectorD globalPos){
	int   gq		= theTrack->geometry()->charge();
	StPhysicalHelixD helix = theTrack->outerGeometry()->helix();
	//double p = theTrack->geometry()->momentum().mag();
	return matchTrack2Mtd(daqCellsHitVec,helix,gq,allCellsHitVec,iNode,globalPos);

}
bool StMtdMatchMaker::matchTrack2Mtd(mtdCellHitVector daqCellsHitVec,StMuTrack *theTrack, mtdCellHitVector& allCellsHitVec, unsigned int iNode, StThreeVectorD globalPos){
	int   gq		= theTrack->charge();
	StPhysicalHelixD helix = theTrack->outerHelix();
	return matchTrack2Mtd(daqCellsHitVec,helix,gq,allCellsHitVec,iNode,globalPos);
}

void StMtdMatchMaker::matchMtdHits(mtdCellHitVector& daqCellsHitVec,mtdCellHitVector& allCellsHitVec,mtdCellHitVector& matchHitCellsVec){
	StructCellHit cellHit;
	mtdCellHitVectorIter daqIter = daqCellsHitVec.begin();

	for(unsigned int idaq=0;idaq<daqCellsHitVec.size();idaq++, daqIter++) {
		mtdCellHitVectorIter proIter = allCellsHitVec.begin();
		for(unsigned int ipro=0;ipro<allCellsHitVec.size();ipro++, proIter++) {

			int daqIndex = (daqIter->module-1)*12 + (daqIter->cell);
			int proIndex = (proIter->module-1)*12 + (proIter->cell);
			int hisIndex = daqIter->backleg - 1;
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
				matchHitCellsVec.push_back(cellHit);
			}
		} 
	} //end {sec. C}'
}
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
void StMtdMatchMaker::fillPidTraits(mtdCellHitVector& finalMatchedCellsVec,Int_t& nValidSingleHitCells,Int_t& nValidSinglePrimHitCells){
	StSPtrVecTrackNode& nodes=mEvent->trackNodes();
	StMtdCollection *theMtd = mEvent->mtdCollection();
	StSPtrVecMtdHit& mtdHits= theMtd->mtdHits();
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
		StGlobalTrack *globalTrack = dynamic_cast<StGlobalTrack*>(nodes[trackNode]->track(global));
		if(!globalTrack) {
			LOG_WARN << "Wrong global track!" << endm;
			continue;
		}

		// Fill association in MTD Hit Collection
		StMtdHit *mtdHit = mtdHits[finalMatchedCellsVec[ii].index2MtdHit];
		if(mtdHit->backleg()!=backleg || mtdHit->module()!=module || mtdHit->cell()!=cell) {
			LOG_WARN << "Wrong hit in the MtdHitCollection!" << endm;
			continue;
		}
		nValidSingleHitCells++;

		mtdHit->setAssociatedTrack(globalTrack);

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

bool StMtdMatchMaker::validTrack(StTrack *track){

	// 1. no track, no go.
	if (!track) return false;
	float gpt 		= track->geometry()->momentum().perp();
	float geta 		= track->geometry()->momentum().pseudoRapidity();
	//float gphi 		= track->geometry()->momentum().phi();
	int   gnFtPts 	= track->fitTraits().numberOfFitPoints(kTpcId);
	int gnDedxPts=0;
	static StTpcDedxPidAlgorithm PidAlgorithm;
	const StParticleDefinition* pd=track->pidTraits(PidAlgorithm);
	if(pd){
	}
	if(PidAlgorithm.traits()){
		gnDedxPts=PidAlgorithm.traits()->numberOfPoints();
	}

	if(gnFtPts<15) return kFALSE;
	if(gnDedxPts<10) return kFALSE;
	if(fabs(geta)>1.5) return kFALSE;
	if(gpt<1.) return kFALSE;
	// 2. track quality flag, should be >0
	if (track->flag()<=0 || track->flag()>=1000) return false;

	// 3. minimum #hits per track - obsolete
	//  if (track->topologyMap().numberOfHits(kTpcId) < mMinHitsPerTrack) return false;
	// 4. minimum #fit points per track
	if (track->fitTraits().numberOfFitPoints(kTpcId) < mMinFitPointsPerTrack) return false;
	// 5. minimum #fit points over #maximum points
	//fg float ratio = (1.0*track->fitTraits().numberOfFitPoints(kTpcId)) / (1.0*track->numberOfPossiblePoints(kTpcId));
	float ratio = (float)track->fitTraits().numberOfFitPoints(kTpcId) / (1.0*track->numberOfPossiblePoints(kTpcId));
	if (ratio < mMinFitPointsOverMax) return false;

	return true;
}
bool StMtdMatchMaker::validTrack(StMuTrack *track){
	// 1. no track, no go.
	if (!track) return false;
	float gpt 		= track->momentum().perp();
	float geta 		= track->momentum().pseudoRapidity();
	//float gphi 		= track->momentum().phi();
	int   gnFtPts 	= track->nHitsFit(kTpcId);
	int gnDedxPts=track->nHitsDedx();
	if(gnFtPts<15) return kFALSE;
	if(gnDedxPts<10) return kFALSE;
	if(fabs(geta)>1.5) return kFALSE;
	if(gpt<1.) return kFALSE;
	// 2. track quality flag, should be >0
	if (track->flag()<=0 || track->flag()>=1000) return false;

	// 3. minimum #hits per track - obsolete
	//  if (track->topologyMap().numberOfHits(kTpcId) < mMinHitsPerTrack) return false;
	// 4. minimum #fit points per track
	if (gnFtPts < mMinFitPointsPerTrack) return false;
	// 5. minimum #fit points over #maximum points
	//fg float ratio = (1.0*track->fitTraits().numberOfFitPoints(kTpcId)) / (1.0*track->numberOfPossiblePoints(kTpcId));
	float ratio = gnFtPts / (1.0*track->nHitsPoss(kTpcId));
	if (ratio < mMinFitPointsOverMax) return false;

	return true;
}


int  StMtdMatchMaker::decodeStripPhiZ(Int_t backLeg, Int_t trayId, Int_t channel, double &phi, double &z){
	if(backLeg>30 || backLeg<1) return 0;
	if(trayId>5   || trayId<1)  return 0;
	if(channel>11 || channel<0) return 0;
	double backLegPhiCen = mFirstBackLegPhi+(backLeg-1)*(backLegPhiWidth+backLegPhiGap);
	if(backLegPhiCen>2.*TMath::Pi()) backLegPhiCen -= 2.*TMath::Pi();

	double stripPhiCen = 0.;
	if(trayId>0&&trayId<4){
		stripPhiCen = backLegPhiCen-(nChannels/4.-0.5-channel)*(stripWidth+stripGap)/mtdRadius; // approximation
	}else{
		stripPhiCen = backLegPhiCen+(nChannels/4.-0.5-channel)*(stripWidth+stripGap)/mtdRadius; 
	}
	double stripZCen   = (trayId-3.)*stripLength;

	if(stripPhiCen>2.*TMath::Pi()) stripPhiCen -= 2.*TMath::Pi();
	if(stripPhiCen<0.)    stripPhiCen += 2.*TMath::Pi();
	phi = stripPhiCen;
	z   = stripZCen;

	return 1;
}

//-------------------------------------------------------------------//
int  StMtdMatchMaker::decodeTofPhiZ(int channel, double &phi, double &z){

	int tray = channel/192;       //0-119
	int module = (channel%192)/6; //0-31
	int cell = (channel%192)%6;   //0-5

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

	phi = mCellPhi;
	z   = mModuleZ;

	return 1;
}
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
//---------------------------------------------------------------------------
void StMtdMatchMaker::processMuDst(){

	StTimer timer;
	if(doPrintCpuInfo) timer.start();
	if(doPrintMemoryInfo) StMemoryInfo::instance()->snapshot();
	// read data from StMtdHit
	// A. build vector of candidate cells
	//
	mtdCellHitVector daqCellsHitVec;
	idVector validModuleVec;
	if(!readMtdHits(daqCellsHitVec,validModuleVec)) return;
	//end of Sect.A
	if(Debug()){
		LOG_INFO<<" Sect.A: =============================="<<endm;
		LOG_INFO <<" total # of cells =" << daqCellsHitVec.size() << endm;
		for(size_t iv=0;iv<validModuleVec.size();iv++){
			LOG_INFO << " module# "<< validModuleVec[iv] <<"Valid! "<<endm;
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

	StMuMtdCollection *theMtd = mMuDst->MtdCollection();
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
}

void StMtdMatchMaker::initEventData(){
	memset(&mMtdEvtData,0,sizeof(mMtdEvtData));
}
//___________________________________________________

ClassImp(StMtdMatchMaker)
