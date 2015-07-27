/*!
 * \class  StGmtMatchMaker
 * \brief  A typical Analysis Class
 * \author Bingchu Huang, Xin Li BNL
 * \date   Jul 2014;
 */
//  Include header files. What has to be included strongly depends
//  on your implementation. StEventTypes.h contains all includes
//  you need to use StEvent.
//
#include "StGmtMatchMaker.h"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "SystemOfUnits.h"

#if ROOT_VERSION_CODE < 334081
#include "TArrayL.h"
#else
#include "TArrayL64.h"
#endif
#include "TClassTable.h"
#include "TNtuple.h"
#include "StThreeVectorF.hh"
#include "StDetectorName.h"
#include <vector>
#include <iostream>
#include <fstream>
using std::vector;
//
float StGmtMatchMaker::tofRadius=210.968+3.6;
//
const Int_t mSeqIdToChId[128] = {
  0,32,64,96,8,40,72,104,16,48,80,112,24,56,88,120,1,
  33,65,97,9,41,73,105,17,49,81,113,25,57,89,121,2,
  34,66,98,10,42,74,106,18,50,82,114,26,58,90,122,3,
  35,67,99,11,43,75,107,19,51,83,115,27,59,91,123,4,
  36,68,100,12,44,76,108,20,52,84,116,28,60,92,124,5,
  37,69,101,13,45,77,109,21,53,85,117,29,61,93,125,6,
  38,70,102,14,46,78,110,22,54,86,118,30,62,94,126,7,
  39,71,103,15,47,79,111,23,55,87,119,31,63,95,127
};

ClassImp(StGmtMatchMaker);
ClassImp(StGmtData);
//________________________________________________________________________________
Int_t StGmtMatchMaker::InitRun(Int_t /* run */) {
  static Bool_t InitDone = kFALSE;
  if (InitDone) return kStOK;
  InitDone = kTRUE;
  TFile *f = GetTFile();
  if (! f) return kStOK;
  f->cd();
  for(int i=0;i<nModules*2;i++){
    char name[256],title[256];
    sprintf(name,"hADCvsChannel_%d",i);
    int iARM = 0;
    if(i>7) iARM = 1;
    sprintf(title,"pulse height for all channels in ARM:%d, APV%d",iARM,i);
    hADCvsChannel[i] = new TH2D(name,title,128,-0.5,127.5,500,0,4000);
  }
  for(int i=0;i<nModules*2;i++){
    char name[256],title[256];
    sprintf(name,"hChannelCorr_%d",i);
    int iARM = 0;
    if(i>7) iARM = 1;
    sprintf(title,"Channel1 vs Channel2 in ARM:%d, APV%d; iCoord; <iCoord>;",iARM,i);
    hChannelCorr[i] = new TH2D(name,title,257,0,257,257,0,257);
  }
  
  hChannelCorrAll = new TH2D("hChannelCorrAll","hChannelCorrAll;iCoord; <iCoord>;",257,0,257,257,0,257);
  
  for(int i=0;i<nModules;i++){
    char name[256],title[256];
    sprintf(name,"hProjCorrX_%d",i);
    sprintf(title,"hProjCorrX_%d;coordX (cm);projected coordX (cm)",i);
    hProjCorrX[i] = new TH2D(name,title,100,-5,15,100,-5,15);
    sprintf(name,"hProjCorrY_%d",i);
    sprintf(title,"hProjCorrY_%d;coordY (cm);projected coordY (cm)",i);
    hProjCorrY[i] = new TH2D(name,title,100,-5,15,100,-10,10);
  }
  hProjCorrXAll = new TH2D("hProjCorrXAll","hProjCorrXAll;coordX (cm);projected coordX (cm)",100,-5,15,100,-15,15);
  hProjCorrYAll = new TH2D("hProjCorrYAll","hProjCorrYAll;coordY (cm);projected coordY (cm)",100,-5,15,100,-10,10);
  hProjGlCorrPhiAll = new TH2D("hProjGlCorrPhiAll","hProjGlCorrPhiAll; phi;projected phi",1000,-3.2,3.2,1000,-3.2,3.2);
  hProjGlCorrZAll = new TH2D("hProjGlCorrZAll","hProjGlCorrZAll;Z (cm);projected coordZ (cm)",3000,-300,300,3000,-300,300);
  
  hProjModuleCorr = new TH2D("hProjModuleCorr","hProjModuleCorr;module ID;projected module ID",8,0,8,8,0,8);
  
  hProjCoordXCorr = new TH2D("hProjCoordXCorr","hProjCoordXCorr;module*12.+coordX;projected module*12.+coordX",1000,0,96,1000,-5,96);
  
  hProjCoordYCorr = new TH2D("hProjCoordYCorr","hProjCoordYCorr;module*12.+coordY;projected module*12.+coordY",1000,0,96,1000,-5,96);
  
  hdCoordXvsId = new TH2D("hdCoordXvsId","hdCoordXvsId;module*12 + coordX;(projected coordX - gmt coordX)",1000,0,96,1000,-10,10);
  hdCoordYvsId = new TH2D("hdCoordYvsId","hdCoordYvsId;module*12 + coordY;(projected coordY - gmt coordY)",1000,0,96,1000,-10,10);
  
  hdCoordXvsPt = new TH2D("hdCoordXvsPt","hdCoordXvsPt;q*p_{T};(projected coordX - gmt coordX)",1000,-10,10,1000,-10,10);
  hdCoordYvsPt = new TH2D("hdCoordYvsPt","hdCoordYvsPt;q*p_{T};(projected coordY - gmt coordY)",1000,-10,10,1000,-10,10);
  
#if 0
  Int_t split  = 9;       // by default, split Event in sub branches
  //  Int_t split  = -2;       // by default, split Event in sub branches << old style
  Int_t bufsize = 64000;
  if (split)  bufsize /= 4;
  Int_t branchStyle = 1; //new style by default
  if (split < 0) {branchStyle = 0; split = -1-split;}
  TTree::SetBranchStyle(branchStyle);
  StGmtData *pmGmtData = &mGmtData;
  mGmtTuple = new TTree("gmt","gmt data");
  mGmtTuple->SetAutoSave(1000000000); // autosave when 1 Gbyte written
  mGmtTuple->SetCacheSize(10000000);  // set a 10 MBytes cache (useless when writing local files)
  TBranch *branch = mGmtTuple->Branch("StGmtData","StGmtData",&pmGmtData, bufsize,split);
#else
  mGmtTuple = new TTree("gmt","gmt data");
  mGmtTuple->SetAutoSave(100000);
  mGmtTuple->Branch("run",&mGmtData.run,"run/I");
  mGmtTuple->Branch("evt",&mGmtData.evt,"evt/I");
  //mGmtTuple->Branch("trgId",&mGmtData.trgId,"trgId/I");
  mGmtTuple->Branch("bField",&mGmtData.bField,"bField/F");
  mGmtTuple->Branch("vertexX",&mGmtData.vertexX,"vertexX/F");
  mGmtTuple->Branch("vertexY",&mGmtData.vertexY,"vertexY/F");
  mGmtTuple->Branch("vertexZ",&mGmtData.vertexZ,"vertexZ/F");
  mGmtTuple->Branch("vpdVz",&mGmtData.vpdVz,"vpdVz/F");
  
  //raw hits
  mGmtTuple->Branch("nGmtRawHits",&mGmtData.nGmtRawHits,"nGmtRawHits/I");
  mGmtTuple->Branch("arm",&mGmtData.arm,"arm[nGmtRawHits]/B");
  mGmtTuple->Branch("apv",&mGmtData.apv,"apv[nGmtRawHits]/B");
  mGmtTuple->Branch("layer",&mGmtData.layer,"layer[nGmtRawHits]/B");
  mGmtTuple->Branch("tb",&mGmtData.tb,"tb[nGmtRawHits]/B");
  mGmtTuple->Branch("channel",&mGmtData.channel,"channel[nGmtRawHits]/S");
  mGmtTuple->Branch("coordNum",&mGmtData.coordNum,"coordNum[nGmtRawHits]/S");
  mGmtTuple->Branch("adc",&mGmtData.adc,"adc[nGmtRawHits]/S");
  mGmtTuple->Branch("pos",&mGmtData.pos,"pos[nGmtRawHits]/S");
  mGmtTuple->Branch("ped",&mGmtData.ped,"ped[nGmtRawHits]/F");
  mGmtTuple->Branch("pedDev",&mGmtData.pedDev,"pedDev[nGmtRawHits]/F");
  
  //hits
  mGmtTuple->Branch("nGmtCluster",&mGmtData.nGmtCluster,"nGmtCluster/I");
  mGmtTuple->Branch("coordNX",&mGmtData.coordNX,"coordNX[nGmtCluster]/F");
  mGmtTuple->Branch("coordNY",&mGmtData.coordNY,"coordNY[nGmtCluster]/F");
  mGmtTuple->Branch("coordX",&mGmtData.coordX,"coordX[nGmtCluster]/F");
  mGmtTuple->Branch("coordY",&mGmtData.coordY,"coordY[nGmtCluster]/F");
  mGmtTuple->Branch("phi",&mGmtData.phi,"phi[nGmtCluster]/F");
  mGmtTuple->Branch("z",&mGmtData.z,"z[nGmtCluster]/F");
  mGmtTuple->Branch("nStrHits",&mGmtData.nStrHits,"nStrHits[nGmtCluster]/S");
  mGmtTuple->Branch("maxCoordNX",&mGmtData.maxCoordNX,"maxCoordNX[nGmtCluster]/S");
  mGmtTuple->Branch("maxCoordNY",&mGmtData.maxCoordNY,"maxCoordNY[nGmtCluster]/S");
  mGmtTuple->Branch("maxAdcX",&mGmtData.maxAdcX,"maxAdcX[nGmtCluster]/S");
  mGmtTuple->Branch("maxAdcY",&mGmtData.maxAdcY,"maxAdcY[nGmtCluster]/S");
  mGmtTuple->Branch("module",&mGmtData.module,"module[nGmtCluster]/B");
  mGmtTuple->Branch("nStrHits",&mGmtData.nStrHits,"nStrHits[nGmtCluster]/S");
  //mGmtTuple->Branch("adcCl",&mGmtData.adcCl,"adcCl[nGmtCluster][2000]/S");
  //mGmtTuple->Branch("chanCl",&mGmtData.chanCl,"chanCl[nGmtCluster][2000]/S");
  //mGmtTuple->Branch("tbCl",&mGmtData.tbCl,"tbCl[nGmtCluster][2000]/B");
  
  //tracks
  mGmtTuple->Branch("ngTracks",&mGmtData.ngTracks,"ngTracks/I");
  mGmtTuple->Branch("gq",&mGmtData.gq,"gq[ngTracks]/B");
  mGmtTuple->Branch("gpt",&mGmtData.gpt,"gpt[ngTracks]/F");
  mGmtTuple->Branch("geta",&mGmtData.geta,"geta[ngTracks]/F");
  mGmtTuple->Branch("gphi",&mGmtData.gphi,"gphi[ngTracks]/F");
  mGmtTuple->Branch("nMax",&mGmtData.nMax,"nMax[ngTracks]/B");
  mGmtTuple->Branch("nFit",&mGmtData.nFit,"nFit[ngTracks]/B");
  mGmtTuple->Branch("gX",&mGmtData.gX,"gX[ngTracks]/F");
  mGmtTuple->Branch("gY",&mGmtData.gY,"gY[ngTracks]/F");
  mGmtTuple->Branch("gZ",&mGmtData.gZ,"gZ[ngTracks]/F");
  mGmtTuple->Branch("gmtMid",&mGmtData.gmtMid,"gmtMid[ngTracks]/B");
  mGmtTuple->Branch("gmtX",&mGmtData.gmtX,"gmtX[ngTracks]/F");
  mGmtTuple->Branch("gmtY",&mGmtData.gmtY,"gmtY[ngTracks]/F");
  mGmtTuple->Branch("gmtZ",&mGmtData.gmtZ,"gmtZ[ngTracks]/F");
  mGmtTuple->Branch("gmtLocalX",&mGmtData.gmtLocalX,"gmtLocalX[ngTracks]/F");
  mGmtTuple->Branch("gmtLocalY",&mGmtData.gmtLocalY,"gmtLocalY[ngTracks]/F");
  mGmtTuple->Branch("gmtCoordNX",&mGmtData.gmtCoordNX,"gmtCoordNX[ngTracks]/S");  
  mGmtTuple->Branch("gmtCoordNY",&mGmtData.gmtCoordNY,"gmtCoordNY[ngTracks]/S");      
  mGmtTuple->Branch("gMatMod",&mGmtData.gMatMod,"gMatMod[ngTracks]/B");      
  mGmtTuple->Branch("gMatCoordNX",&mGmtData.gMatCoordNX,"gMatCoordNX[ngTracks]/S");      
  mGmtTuple->Branch("gMatCoordNY",&mGmtData.gMatCoordNY,"gMatCoordNY[ngTracks]/S");      
  mGmtTuple->Branch("gMatCoordX",&mGmtData.gMatCoordX,"gMatCoordX[ngTracks]/F");      
  mGmtTuple->Branch("gMatCoordY",&mGmtData.gMatCoordY,"gMatCoordY[ngTracks]/F");      
#endif  
  fstream inf;
  inf.open("pedestal_XinLi.txt",ios::in);
  if(!inf) LOG_WARN<<"No pedestal files!"<<endm;
  for(int i=0;i<16;i++){
    for(int j=0;j<128;j++){
      int iApv=0, ich=0;
      inf>>iApv>>ich>>mPeds[i][j]>>mPedDevs[i][j];
      //LOG_INFO<<"iApv:"<<iApv<<" ich:"<<ich<<" mPeds :"<<mPeds[i][j]<<" mPedDevs :"<<mPedDevs[i][j]<<endm;
    }
  }
  inf.close();
  return StMaker::Init();
}
//________________________________________________________________________________
/*!
 *  This method is called every event. That's the
 *  right place to plug in your analysis. 
 */
Int_t StGmtMatchMaker::Make() {
  mEventCounter++;  // increase counter
  
  memset(&mGmtData,0,sizeof(mGmtData));
  //
  //	Get pointer to StEvent
  //
  
  event = (StEvent *) GetInputDS("StEvent");
  if (!event){
    gMessMgr->Warning() << "StGmtMatchMaker::Make : No StEvent" << endm;
    return kStOK;        // if no event, we're done
  }
  
  //
  //  The following is only needed since the
  //  QA folks use this maker for their QA runs.
  //  You do not need this.
  //  
  StSPtrVecTrackNode& trackNode = event->trackNodes();
  UInt_t nTracks = trackNode.size();
  
  //if(nTracks<=0) return kStOK;
  
  mGmtData.run = event->runId();       // the run number
  mGmtData.evt = event->id();       // the event number
  mGmtData.bField = event->runInfo()->magneticField()/10.;
  StPrimaryVertex *pVtx = event->primaryVertex();
  if (pVtx){
    mGmtData.vertexX = pVtx->position().x();
    mGmtData.vertexY = pVtx->position().y();
    mGmtData.vertexZ = pVtx->position().z();
  }
  
  mGmtData.vpdVz = -999.;
  const StBTofCollection* tof = event->btofCollection();
  if(tof){
    if (tof->tofHeader() && tof->tofHeader()->vpdVz() > -250) {
      mGmtData.vpdVz = tof->tofHeader()->vpdVz();
    }
  }
  
  StGmtCollection *theGmt = event->gmtCollection();
  if (!theGmt) return kStWarn;
  if (Debug()) {
     UShort_t NumModules = theGmt->getNumModules();
     for (UShort_t m = 0; m < NumModules; m++) {
       const StGmtHitCollection *coll = theGmt->getHitCollection(m);
       if (! coll) continue;
       const StSPtrVecGmtHit &hits = coll->getHitVec();
       UInt_t NoHits = hits.size();
       for (UInt_t l = 0; l < NoHits; l++) {
	 const StGmtHit *hit = hits[l];
	 if (hit) {
	   hit->Print("");
	 }
       }
     }
   }
  //LOG_INFO<<"nModules = "<<theGmt->getNumModules()<<endm;
  vector<int> nCoordNXVec[nModules*2];
  vector<int> nCoordNYVec[nModules*2];
  
  //for(int i=0;i<nModules*2;i++){
  //	LOG_INFO<<"nCoordNXVec["<<i<<"].size()="<<nCoordNXVec[i].size()<<endm;
  //	LOG_INFO<<"nCoordNYVec["<<i<<"].size()="<<nCoordNYVec[i].size()<<endm;
  //}
  int nGmtRawHits =  0;
  int maxChan[nModules][2];
  double maxAdc[nModules][2];
  memset(maxChan,0,sizeof(maxChan));
  memset(maxAdc,0,sizeof(maxAdc));
  for(int im=0;im<theGmt->getNumModules();im++){
    StGmtHitCollection *gmtHitColl = theGmt->getHitCollection(im);
    StGmtStripCollection *gmtStripColl = theGmt->getStripCollection(im);
    StSPtrVecGmtStrip strips = gmtStripColl->getStripVec();
    //LOG_INFO<<"nStrips = "<<strips.size()<<endm;
    for(int i=0;i<strips.size();i++){
      StGmtStrip *aStrip = strips[i];
      if (! aStrip) continue;
      int iApv = aStrip->getApv();
      if(iApv>=12) iApv-=8;
      int iArm = aStrip->getArm();
      if(iArm>0) iApv+=8;
      int iMod = iApv/2;
      //LOG_INFO<<"im="<<im<<" iMod="<<iMod<<endm;
      for (int j = 0; j < nTimebins; j++) {
	double xj = mSeqIdToChId[aStrip->getChannel()] + j*1./nTimebins;
	double adc = aStrip->getAdc(j);
	hADCvsChannel[iApv]->Fill(xj,adc);
	int ich = mSeqIdToChId[aStrip->getChannel()];
	int icoord = aStrip->getCoordNum();
	double pedMin = mPeds[iApv][ich]+12.*mPedDevs[iApv][ich];
	if(adc>pedMin&&adc<4000){
	  double adcCor = adc - mPeds[iApv][ich];
	  if(icoord<128){
	    nCoordNXVec[iApv].push_back(icoord);
	    if(maxAdc[iMod][0]<adcCor){ 
	      maxAdc[iMod][0] = adcCor;
	      maxChan[iMod][0] = icoord;
	    }
	  } else{
	    nCoordNYVec[iApv].push_back(icoord);
	    if(maxAdc[iMod][1]<adcCor){ 
	      maxAdc[iMod][1] = adcCor;
	      maxChan[iMod][1] = icoord;
	    }
	  }
	}
	mGmtData.arm[nGmtRawHits] = iArm;
	mGmtData.apv[nGmtRawHits] = iApv;
	mGmtData.layer[nGmtRawHits] = aStrip->isY();
	mGmtData.tb[nGmtRawHits] = j;
	mGmtData.channel[nGmtRawHits] = aStrip->getChannel();
	mGmtData.adc[nGmtRawHits] = adc;
	mGmtData.coordNum[nGmtRawHits] = icoord;
	mGmtData.pos[nGmtRawHits] = aStrip->getPosition();
	nGmtRawHits++;
      }
    }
#if 0    
    StSPtrVecGmtHit hits = gmtHitColl->getHitVec();
    //LOG_INFO<<"nHits = "<<hits.size()<<endm;
    for(int i=0;i<gmtHitColl->getNumHits();i++){
      StGmtHit *aHit = hits[i];
      if (!aHit) continue;
      int adcx = aHit->getAdcX();
      //LOG_INFO<<"hit "<<i<<" adc = "<<adcx<<endm;	
    }
#endif
  }
  mGmtData.nGmtRawHits = nGmtRawHits;
  
  int adcCorSumXMod[nModules][nTimebins];
  int adcCorSumYMod[nModules][nTimebins];
  int adcCorWtSumXMod[nModules][nTimebins];
  int adcCorWtSumYMod[nModules][nTimebins];
  Short_t adcCorXMod[nModules][nTimebins][2*kMaxNgbhrChan+1];
  Short_t adcCorYMod[nModules][nTimebins][2*kMaxNgbhrChan+1];
  Short_t coordNXMod[nModules][nTimebins][2*kMaxNgbhrChan+1];
  Short_t coordNYMod[nModules][nTimebins][2*kMaxNgbhrChan+1];
  
  memset(adcCorSumXMod,0,sizeof(adcCorSumXMod));
  memset(adcCorSumYMod,0,sizeof(adcCorSumYMod));
  memset(adcCorWtSumXMod,0,sizeof(adcCorWtSumXMod));
  memset(adcCorWtSumYMod,0,sizeof(adcCorWtSumYMod));
  memset(adcCorXMod,0,sizeof(adcCorXMod));
  memset(adcCorYMod,0,sizeof(adcCorYMod));
  memset(coordNXMod,0,sizeof(coordNXMod));
  memset(coordNYMod,0,sizeof(coordNYMod));
  for(int im=0;im<theGmt->getNumModules();im++){
    StGmtHitCollection *gmtHitColl = theGmt->getHitCollection(im);
    StGmtStripCollection *gmtStripColl = theGmt->getStripCollection(im);
    StSPtrVecGmtStrip strips = gmtStripColl->getStripVec();
    for(int i=0;i<strips.size();i++){
      StGmtStrip *aStrip = strips[i];
      if (!aStrip) continue;
      int iApv = aStrip->getApv();
      if(iApv>=12) iApv-=8;
      int iArm = aStrip->getArm();
      if(iArm>0) iApv+=8;
      int iMod = iApv/2;
      for (int j = 0; j < nTimebins; j++) {
	double adc = aStrip->getAdc(j);
	int ich = mSeqIdToChId[aStrip->getChannel()];
	int icoord = aStrip->getCoordNum();
	if((icoord<128&&abs(icoord-maxChan[iMod][0])<=kMaxNgbhrChan)|| 
	   (icoord>=128&&abs(icoord-maxChan[iMod][1])<=kMaxNgbhrChan)){
	  double pedMin = mPeds[iApv][ich]+12.*mPedDevs[iApv][ich];
	  //if(pedMin<1000) pedMin = 1000;
	  int idx = -1;
	  if(icoord<128) idx = icoord-maxChan[iMod][0] + kMaxNgbhrChan;
	  else           idx = icoord-maxChan[iMod][1] + kMaxNgbhrChan;
	  if(idx<0||idx>10){
	    LOG_INFO<<"ERROR: idx out of range! idx = "<<idx<<endm;
	  }
	  if(adc>pedMin&&adc<4000){
	    double adcCor = adc - mPeds[iApv][ich];
	    if(icoord<128){
	      adcCorWtSumXMod[iMod][j]   += adcCor*icoord;
	      adcCorSumXMod[iMod][j] += adcCor;
	      adcCorXMod[iMod][j][idx] = adcCor;
	      coordNXMod[iMod][j][idx] = icoord;
	    }else{
	      adcCorWtSumYMod[iMod][j]   += adcCor*(icoord-128);
	      adcCorSumYMod[iMod][j] += adcCor;
	      adcCorYMod[iMod][j][idx] = adcCor;
	      coordNYMod[iMod][j][idx] = icoord;
	    }
	  }
	}
      }
    }
  }
  int nGmtHits =  0;
  for(int i=0;i<nModules;i++){
    double adcCorSumX = 0.;
    double adcCorSumY = 0.;
    double adcCorWtSumX = 0.;
    double adcCorWtSumY = 0.;
    for(int j=0;j<nTimebins;j++){
      if(adcCorWtSumXMod[i][j]>1e-10) adcCorWtSumX+=adcCorWtSumXMod[i][j];	
      if(adcCorWtSumYMod[i][j]>1e-10) adcCorWtSumY+=adcCorWtSumYMod[i][j];	
      if(adcCorSumXMod[i][j]>1e-10) adcCorSumX+=adcCorSumXMod[i][j];	
      if(adcCorSumYMod[i][j]>1e-10) adcCorSumY+=adcCorSumYMod[i][j];	
    }
    double coordNX = 0.,coordNY = 0.,coordX=0.,coordY=0.;
    if(adcCorSumX>0.) coordNX = adcCorWtSumX/adcCorSumX;
    if(adcCorSumY>0.) coordNY = adcCorWtSumY/adcCorSumY;
    coordX = 0.04+coordNX*0.08;//cm
    coordY = 0.04+coordNY*0.08;//cm
    double phi = -999.,z = -999.;
    local2Global(i,coordNX,coordNY,phi,z); 
    mGmtData.module[nGmtHits] = i;
    mGmtData.coordNX[nGmtHits] = coordNX;
    mGmtData.coordNY[nGmtHits] = coordNY;
    mGmtData.coordX[nGmtHits] = coordX;
    mGmtData.coordY[nGmtHits] = coordY;
    mGmtData.maxCoordNX[nGmtHits] = maxChan[i][0];
    mGmtData.maxCoordNY[nGmtHits] = maxChan[i][1];
    mGmtData.maxAdcX[nGmtHits] = maxAdc[i][0];
    mGmtData.maxAdcY[nGmtHits] = maxAdc[i][1];
    mGmtData.phi[nGmtHits] = phi;
    mGmtData.z[nGmtHits] = z;
    int nStrHits = 0;
    for(int j=0;j<nTimebins;j++){
      for(int k = 0;k<2*kMaxNgbhrChan+1;k++){
	if(coordNXMod[i][j][k] >0 ){
	  nStrHits++;
	}
	if(coordNYMod[i][j][k] >0 ){
	  nStrHits++;
	}
      }
    }
    if(nStrHits>=2000) LOG_INFO<<"ERROR: nStrHits out of range!"<<endm;
    mGmtData.nStrHits[nGmtHits] = nStrHits;
    nGmtHits++;
  }
  mGmtData.nGmtCluster = nGmtHits;
  LOG_INFO<<"--------------------------------"<<endm;
  for(int i=0;i<nModules*2;i++){
    double meanCoorX = 0.;
    if(nCoordNXVec[i].size()>0){
      for(unsigned int j=0;j<nCoordNXVec[i].size();j++){
	meanCoorX+=nCoordNXVec[i][j];
      }
      meanCoorX = meanCoorX/nCoordNXVec[i].size();
    }
    double meanCoorY = 0.;
    if(nCoordNYVec[i].size()>0){
      for(unsigned int j=0;j<nCoordNYVec[i].size();j++){
	meanCoorY+=nCoordNYVec[i][j];
      }
      meanCoorY = meanCoorY/nCoordNYVec[i].size();
    }
    
    for(unsigned int j=0;j<nCoordNXVec[i].size();j++){
      int ch1 = nCoordNXVec[i][j];
      hChannelCorr[i]->Fill(ch1,meanCoorX);
      hChannelCorrAll->Fill(ch1,meanCoorX);
    }
    for(unsigned int j=0;j<nCoordNYVec[i].size();j++){
      int ch2 = nCoordNYVec[i][j];
      hChannelCorr[i]->Fill(ch2,meanCoorY);
      hChannelCorrAll->Fill(ch2,meanCoorY);
    }
  }
  
  project2GMT();
  
  if(nGmtRawHits>0) mGmtTuple->Fill();
  for(int i=0;i<nModules*2;i++){
    nCoordNXVec[i].clear();
    nCoordNYVec[i].clear();
  }
  return kStOK;
}
//________________________________________________________________________________
void StGmtMatchMaker::project2GMT(){
  
  float mField = 0;
  UInt_t Nnodes = 0;
  
  Nnodes = event->trackNodes().size();
  mField = event->runInfo()->magneticField();
  int ntrks=0;
  if (Debug()) {LOG_INFO<<"Nnodes = "<<Nnodes<<endm;}
  for(UInt_t iNode=0;iNode<Nnodes;iNode++){
    
    StSPtrVecTrackNode& nodes=event->trackNodes();
    StGlobalTrack *theTrack = dynamic_cast<StGlobalTrack*>(nodes[iNode]->track(global));
    if(!theTrack) continue;
    if(!theTrack->geometry()) continue;
    
    int nmax = theTrack->numberOfPossiblePoints();
    StTrackFitTraits& fitTraits = theTrack->fitTraits();
    int nfit = fitTraits.numberOfFitPoints();
    if(nfit<20) continue;
    //if(nfit/nmax<0.52) continue;
    if(theTrack->geometry()->momentum().perp()<0.2) continue;
    
    mGmtData.gq[ntrks]		= theTrack->geometry()->charge();
    mGmtData.gpt[ntrks]		= theTrack->geometry()->momentum().perp();
    mGmtData.geta[ntrks] 	= theTrack->geometry()->momentum().pseudoRapidity();
    mGmtData.gphi[ntrks] 	= theTrack->geometry()->momentum().phi();	
    
    
    mGmtData.nMax[ntrks] 	= nmax;
    mGmtData.nFit[ntrks] 	= nfit;	
    
    int    gq  = theTrack->geometry()->charge();
    double gpt = theTrack->geometry()->momentum().perp();
    
    StThreeVector<double> pos(-999,-999,-999);
    StThreeVector<double> mpos(-999,-999,-999);
    StThreeVector<double> local_pos(-999,-999,-999);
    projectTrack2GmtR(theTrack->outerGeometry()->helix(), mField, pos);
    
    mGmtData.gX[ntrks] 	= pos.x();	
    mGmtData.gY[ntrks] 	= pos.y();	
    mGmtData.gZ[ntrks] 	= pos.z();	
    
    int proj_moduleID = -1;
    proj_moduleID = getModuleID(pos);
    
    if (proj_moduleID==-1)continue;
    
    mGmtData.gmtMid[ntrks] 	= proj_moduleID;	
    if(proj_moduleID>-1) LOG_INFO<<"projectTrack2GmtR: projected module ID="<<proj_moduleID<<endm;
    
    if(proj_moduleID>-1) projectTrack2GmtModule(theTrack->outerGeometry()->helix(), mField, proj_moduleID, mpos);
    
    mGmtData.gmtX[ntrks] 	= mpos.x();	
    mGmtData.gmtY[ntrks] 	= mpos.y();	
    mGmtData.gmtZ[ntrks] 	= mpos.z();	  
    
    if(proj_moduleID>-1) global2Local(mpos, local_pos,proj_moduleID);
    
    StThreeVector<float> coord(-999,-999,-999); 
    if(proj_moduleID>-1){ 
      LOG_INFO<<"------------------------------------------------------------------------------------------"<<endm;
      LOG_INFO<<"track "<<iNode<<" pt:"<<theTrack->geometry()->momentum().perp()<<" eta:"<<theTrack->geometry()->momentum().pseudoRapidity()<<" phi:"<<theTrack->geometry()->momentum().phi()<<" nHitsFit:"<<nfit<<endm;
      getCoord(local_pos, coord);
      LOG_INFO<<"found track hit GMT: module = "<<proj_moduleID<<" global="<<mpos.x()<<","<<mpos.y()<<","<<mpos.z()<<" global phi = "<<mpos.phi()<<" local:"<<local_pos.x()<<","<<local_pos.y()<<","<<local_pos.z()<<" coord:"<<coord.x()<<","<<coord.y()<<","<<coord.z()<<endm;
    }
    
    mGmtData.gmtLocalX[ntrks] 	= local_pos.x();	
    mGmtData.gmtLocalY[ntrks] 	= local_pos.y();	
    //mGmtData.gmtLocalZ[ntrks] 	= local_pos.z();
    
    mGmtData.gmtCoordNX[ntrks] 	= coord.x();	
    mGmtData.gmtCoordNY[ntrks] 	= coord.y();	
    //mGmtData.gmtChan[ntrks] 	= coord.z();	
    
    LOG_INFO<<"nGmtCluster = "<<mGmtData.nGmtCluster<<endm;
    for(int ic = 0;ic<mGmtData.nGmtCluster;++ic){
      if(proj_moduleID>-1){
	if(proj_moduleID == mGmtData.module[ic]){
	  hProjCorrX[1*mGmtData.module[ic]]->Fill(mGmtData.coordX[ic],local_pos.x());
	  hProjCorrY[1*mGmtData.module[ic]]->Fill(mGmtData.coordY[ic],local_pos.y());
	  hProjCorrXAll->Fill(mGmtData.coordX[ic],local_pos.x());
	  hProjCorrYAll->Fill(mGmtData.coordY[ic],local_pos.y());
	  hdCoordXvsId->Fill(mGmtData.module[ic]*12.+mGmtData.coordX[ic],local_pos.x()-mGmtData.coordX[ic]);
	  hdCoordYvsId->Fill(mGmtData.module[ic]*12.+mGmtData.coordY[ic],local_pos.y()-mGmtData.coordY[ic]);
	  hdCoordXvsPt->Fill(gq*gpt,local_pos.x()-mGmtData.coordX[ic]);
	  hdCoordYvsPt->Fill(gq*gpt,local_pos.y()-mGmtData.coordY[ic]);
	}
	hProjModuleCorr->Fill(mGmtData.module[ic],proj_moduleID);
	hProjCoordXCorr->Fill(mGmtData.module[ic]*12.+mGmtData.coordX[ic],proj_moduleID*12.+local_pos.x());
	hProjCoordYCorr->Fill(mGmtData.module[ic]*12.+mGmtData.coordY[ic],proj_moduleID*12.+local_pos.y());
      }
      hProjGlCorrPhiAll->Fill(mGmtData.phi[ic],pos.phi());
      hProjGlCorrZAll->Fill(mGmtData.z[ic],pos.z());
    }
    ntrks++;
    
  } // end for
  
  mGmtData.ngTracks = ntrks;
  
}
//________________________________________________________________________________
void StGmtMatchMaker::projectTrack2GmtR(const StPhysicalHelixD helix, float bfield, StThreeVector<double> &pos){
  StThreeVector<double> helixOrigin = helix.origin();
  int gq = helix.charge(bfield);
  
  StThreeVector<double> helixMomentum = helix.momentum(bfield*kilogauss);
  float ghelixpx  = helixMomentum.x();
  float ghelixpy  = helixMomentum.y();
  float ghelixpz  = helixMomentum.z();
  float ghelixox  = helixOrigin.x();
  float ghelixoy  = helixOrigin.y();
  float ghelixoz  = helixOrigin.z();
  
  float bField = bfield/10.;
  
  StThreeVector<double> g1P(ghelixpx,ghelixpy,ghelixpz);//momentum 
  StThreeVector<double> g1O(ghelixox,ghelixoy,ghelixoz);//origin
  StPhysicalHelixD gHelixTpc(g1P,g1O,bField*tesla,gq); 
  //LOG_INFO<<"projectTrack2GmtR() "<<" bField"<<bField<<endm;
  //LOG_INFO<<"input track gpt,geta,gphi:"<<helixMomentum.perp()<<","<<helixMomentum.pseudoRapidity()<<","<<helixMomentum.phi()<<endm;
  
  //project track to TOF radius
  double rTof = -9999.;
  pairD sTof = gHelixTpc.pathLength(tofRadius);	
  if(sTof.first > 0 || sTof.second > 0) {
    rTof =  (sTof.first < 0 || sTof.second < 0) 
      ? max(sTof.first, sTof.second) : min(sTof.first, sTof.second); 
    if(rTof>0) pos = gHelixTpc.at(rTof);
  }
}
//________________________________________________________________________________
void StGmtMatchMaker::projectTrack2GmtModule(const StPhysicalHelixD helix, float bfield, int mid, StThreeVector <double> &mpos){
  StThreeVector<double> n;
  StThreeVector<double> cen;
  
  double a1 = TMath::Pi()*(-1./3.);
  double a2 = TMath::Pi()*(1./6.);	
  
  if(mid>=0&&mid<=3){
    n.setX(cos(a1));
    n.setY(sin(a1));
    n.setZ(0);
    cen.setX(tofRadius*cos(a1));
    cen.setY(tofRadius*sin(a1));
    cen.setZ(0);	 	
  } else if(mid>=4&&mid<=7){
    n.setX(cos(a2));
    n.setY(sin(a2));
    n.setZ(0);
    cen.setX(tofRadius*cos(a2));
    cen.setY(tofRadius*sin(a2));
    cen.setZ(0);	
  }
  
  double rGmt = helix.pathLength(cen, n);
  mpos = helix.at(rGmt);
}
//________________________________________________________________________________
int StGmtMatchMaker::getModuleID(StThreeVector<double> global){
  
  double phi = global.phi()*180/TMath::Pi();
  double originR = 3.; //1.3*5./tofRadius*180./TMath::Pi(); 
  int id = -1;	
  
  double dz = 6.;//cm
  if	((global.z()>=6.93-dz&& global.z()<=6.93+10+dz)&&(phi>=30.-originR &&phi<=30.+originR))id=5;
  if	((global.z()>=6.93-dz&& global.z()<=6.93+10+dz)&&(phi>=-60.-originR&&phi<=-60.+originR))id=1;	
  
  if	((global.z()<=-6.93+dz&& global.z()>=-6.93-10-dz)&&(phi>=30.-originR&&phi<=30.+originR))id=7;
  if	((global.z()<=-6.93+dz&& global.z()>=-6.93-10-dz)&&(phi>=-60.-originR&&phi<=-60.+originR))id=3;	
  
  if	((global.z()>=197.53-dz&& global.z()<=197.53+10+dz)&&(phi>=30.-originR&&phi<=30.+originR))id=4;
  if	((global.z()>=197.53-dz&& global.z()<=197.53+10+dz)&&(phi>=-60.-originR&&phi<=-60.+originR))id=0;			
  
  if	((global.z()<=-197.53+dz&& global.z()>=-197.53-10-dz)&&(phi>=30.-originR&&phi<=30.+originR))id=6;
  if	((global.z()<=-197.53+dz&& global.z()>=-197.53-10-dz)&&(phi>=-60.-originR&&phi<=-60.+originR))id=2;			
  
  return id;
  
}
//________________________________________________________________________________
void StGmtMatchMaker::global2Local(StThreeVector<double> global, StThreeVector<double>& local, int mid){
  double r,theta,z;
  
  r= tofRadius;
  
  if(mid==4||mid==5)theta = TMath::Pi()*1./6.;
  if(mid==6||mid==7)theta = TMath::Pi()*1./6. ;	
  if(mid==0||mid==1)theta = TMath::Pi()*-1./3. ;
  if(mid==2||mid==3)theta = TMath::Pi()*-1./3. ;		
  
  if(mid==1||mid==5)z=6.93;
  if(mid==3||mid==7)z=-6.93;
  if(mid==0||mid==4)z=197.53;
  if(mid==2||mid==6)z=-197.53;	
  
  double local_x=global.z()-z;
  double medi_x=global.x()-r*(TMath::Cos(theta));
  double medi_y=global.y()-r*(TMath::Sin(theta));
  double local_z=medi_x*(TMath::Cos(-theta))-medi_y*(TMath::Sin(-theta));
  double local_y=medi_x*(TMath::Sin(-theta))+medi_y*(TMath::Cos(-theta));
  if(mid==2||mid==3||mid==6||mid==7){ 
    local_x = -local_x; //east
    local_y = -local_y;
  }
  
  local.setX(local_x);
  local.setY(local_y);
  local.setZ(local_z);
  
}
//________________________________________________________________________________
void StGmtMatchMaker::local2Global(int mid, double coordNX, double coordNY, double &phi, double &z){
  
  double r,theta,zo;
  
  r= tofRadius;
  
  if(mid==4||mid==5)theta = TMath::Pi()*1./6.;
  if(mid==6||mid==7)theta = TMath::Pi()*1./6. ;	
  if(mid==0||mid==1)theta = TMath::Pi()*-1./3. ;
  if(mid==2||mid==3)theta = TMath::Pi()*-1./3. ;		
  
  if(mid==1||mid==5)zo=6.93;
  if(mid==3||mid==7)zo=-6.93;
  if(mid==0||mid==4)zo=197.53;
  if(mid==2||mid==6)zo=-197.53;	
  
  if(mid==2||mid==3||mid==6||mid==7){ 
    z = zo - 0.04 - coordNX*0.08;
    phi = theta - (coordNY-64.)*0.08/r;
  }else{
    z = zo + 0.04 + coordNX*0.08;
    phi = theta + (coordNY-64.)*0.08/r;
  }
  
}
//________________________________________________________________________________
void StGmtMatchMaker::getCoord(StThreeVector<double> local, StThreeVector<float> & coord){
  
  int coord_x = abs(local.x()/0.08);
  int coord_y = abs(local.y()/0.08);
  
  coord.setX(coord_x);
  coord.setY(coord_y);
  coord.setZ(-1);
  
}
