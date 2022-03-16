// $Id: StvMaker.cxx,v 1.61 2017/09/29 16:54:51 perev Exp $
/*!
\author V Perev 2010

A maker StvMaker is a steering maker for Stv package.
<br>
Main tasks:				
<ul>
<li> Create StvHits;			
<li> Make tracks;				
<li> Make Primary vertices;		
<li> Make Primary tracks;			
<li> Save produced data into StEvent.	
</ul>
More detailed: 				<br>
<ul>
<li>On Init:				
<ul>
<li> Detectors initialization. 

     SetAttr("useEventFiller"      ,kTRUE);		// default On
     SetAttr("useTracker"          ,kTRUE);		// default On
     SetAttr("useVertexFinder"     ,kTRUE);		// default On
     SetAttr("makePulls"           ,kFALSE);		// default Off

     SetAttr("noTreeSearch",kFALSE);	// treeSearch default ON
</ul>
 
<li>On InitRun:				
<ul>
 <li> Build detectors;			
 <li> Init seed finder;			
 <li> Init hit loader;			
 <li> Init tracker;  			
 <li> Init StEvent filler;			 
 <li> Init vertex finder;			 
</ul>
<li>In Make:				
<ul>
 <li> Load hits; 
 <li> Find seeds; 
 <li> Create global tracks; 
 <li> Save tracks into StEvent;
 <li> Find vertecies; 
 <li> Create and assign primaries tracks; 
 <li> Save primary tracks into StEvent;

*/
#include <Stiostream.h>
#include <math.h>
#include <string>
#include "TSystem.h"
#include "TROOT.h"
#include "TFile.h"
#include "TTree.h"
#include "TTable.h"
#include "TCernLib.h"
#include "TGeoManager.h"
#include "StDetectorId.h"
#include "StEvent.h"
#include "StEnumerations.h"
#include "StChainOpt.h"
#include "StvMaker.h"
#include "StarVMC/GeoTestMaker/StVMCApplication.h"
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"
//#include "StvMCInitApp.h"
#include "Stv/StvConst.h"
#include "Stv/StvHit.h"
#include "StvUtil/StvPullEvent.h"
#include "Stv/StvDiver.h"
#include "StvHitLoader.h"
//#include "StvFtsHitLoader.h"
//#include "StvUtil/StvFtsHitErrCalculator.h"
#include "Stv/StvToolkit.h"
#include "Stv/StvSeedFinder.h"
#include "Stv/StvKalmanTrackFinder.h"
#include "StvUtil/StvHitErrCalculator.h"
#include "StvUtil/StvHitErrCalculator.h"
#include "StvUtil/StvDebug.h"
#include "Stv/StvFitter.h"
#include "Stv/StvKalmanTrackFitter.h"
#include "StvStEventFiller.h"
#include "StvStarVertexFinder.h"
#include "StvTpcActive.h"
#include "Stv/StvTrack.h"
#include "Stv/StvNode.h"
#include "StvStEventMaker.h"
/// Definion of minimal primary vertex errors.
/// Typical case,vertex got from simulations with zero errors.
/// But zero errors could to unpredicted problems
/// Now minimal possible error is 1 micron
static const float MIN_VTX_ERR2 = 1e-4*1e-4;
enum {kMidEta=1,kForwEta=2};

ClassImp(StvMaker)  
//_____________________________________________________________________________
StvMaker::StvMaker(const char *name) : StMaker(name)

{
  assert(strcmp(gProgName,"root.exe")==0);
  memset(mBeg,0,mEnd-mBeg+1);
  cout <<"StvMaker::StvMaker() -I- Starting"<<endl;

  SetAttr("activeTpc"		,1);
  SetAttr("useEventFiller"      ,kTRUE);
  SetAttr("useTracker"          ,kTRUE);
  SetAttr("useVertexFinder"     ,kTRUE);
//SetAttr("makePulls"           ,kTRUE);
  SetAttr("refit"           	,kTRUE);
  SetAttr("HitLoadOpt"          ,0);

}

//_____________________________________________________________________________
StvMaker::~StvMaker() 
{
  cout <<"StvMaker::~StvMaker() -I- Started/Done"<<endl;
}

//_____________________________________________________________________________
void StvMaker::Clear(const char*)
{
  if (mPullEvent) mPullEvent->Clear();
  StMaker::Clear();
}

//_____________________________________________________________________________
Int_t StvMaker::Finish()
{
  StTGeoProxy::Inst()->Finish();
  return StMaker::Finish();
}

//_____________________________________________________________________________
Int_t StvMaker::Init()
{
//		Add maker immediately after Input maker to cleanup StEvent
  StvStEventMaker *mk =StvStEventMaker::Inst();
  if (mk) mk->Init();
  return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StvMaker::InitDetectors()
{
  
  StTGeoProxy *tgh = StTGeoProxy::Inst();

//	Check is Stv is running in fit hit error utility

  mFETracks = IAttr("fiterr");
  if (mFETracks>0) SetAttr(".privilege",1,"");
  
//		TGeo herlper is ready, add error calculators
  int actTpc = IAttr("activeTpc");
  mEtaRegion = 0;
  if (actTpc) {	//TPC error calculators
    mHitLoader[0]->AddDetector(kTpcId);

    mEtaRegion|= kMidEta;
    
    const char*  innOutNames[]  = {"StvTpcInnerHitErrs"    ,"StvTpcOuterHitErrs" 
                                  ,"StvTpcInnerPromptErrs" ,"StvTpcOuterPromptErrs",0};
    const char* innOut = 0;
    for (int io=0;(innOut=innOutNames[io]);io++) {
      TTable *tt = 0;
      StvHitErrCalculator *hec = 0;
      int nDo = !!mFETracks;
      for (int iDo=0; iDo<=nDo; iDo++) {
	TString myName(innOut); if (mFETracks && !iDo) myName+="FE";
	switch (io) {
          case 0:; case 1: hec = new StvTpcHitErrCalculator(myName); break;
          case 2:; case 3: hec = new StvHitErrCalculator   (myName); break;
          default: assert(0 && "Wrong tpcActive value");
	}
	TString ts("Calibrations/tracker/");
	ts+=myName;
	tt = (TTable*)GetDataBase(ts);
	if (!tt) Error("Make","Table %s NOT FOUND",ts.Data());
      }
      assert(tt);
      hec->SetPars((double*)tt->GetArray());
      StvTpcSelector*sel = new StvTpcSelector(innOut);
      int nHP = tgh->SetHitErrCalc(kTpcId,hec,sel);
      Info("Init","%s: %d HitPlanes",innOut,nHP);
      assert(nHP);
    }
  
//	After 2009 tpcegeo3 is used
    mHitLoader[0]->SetHitActor(new StvTpcHitActor);
  }

  if (IAttr("activeEtr")) {	//Etr error calculators
    mHitLoader[0]->AddDetector(kEtrId);
    TString myName("EtrHitErrs"); if (mFETracks) myName+="FE";
    StvHitErrCalculator *hec = new StvHitErrCalculator(myName,2);
    double etrPars[StvHitErrCalculator::kMaxPars]={9e-4,9e-4};
    hec->SetPars(etrPars);
    int nHP = tgh->SetHitErrCalc(kEtrId,hec,0);
    Info("Init","%s: %d HitPlanes","EtrHitErrs",nHP);
    assert(nHP);
  }

  if (IAttr("activeFgt")) {    // FGT error calculator
    mHitLoader[1]->AddDetector(kFgtId);
    mEtaRegion |= kForwEta;
    TString myName("FgtHitErrs"); if (mFETracks) myName+="FE";
    StvHitErrCalculator *hec = new StvHitErrCalculator(myName, 2);
    Double_t fgtPars[ StvHitErrCalculator::kMaxPars]={
      9e-4,9e-4,
    };
    hec -> SetPars( fgtPars );
    Int_t nHP = tgh->SetHitErrCalc(kFgtId,hec,0);
    Info("Init","%s: %d Hitplanes", "FgtHitErrs", nHP);
  }
  if (IAttr("activeSst")) {    // Sst error calculator
     mHitLoader[0]->AddDetector(kSstId);

  }
  if (IAttr("activeIst")) {    // IST error calculator
    mHitLoader[0]->AddDetector(kIstId);
    mEtaRegion |= kMidEta;
    TString myName("IstHitErrs"); if (mFETracks) myName+="FE";
    StvHitErrCalculator *hec = new StvHitErrCalculator(myName, 2);
    TString ts("Calibrations/tracker/Stv");ts+=myName;
    TTable *tt = (TTable*)GetDataBase(ts);
    if (!tt) Error("Make","Table %s NOT FOUND",ts.Data());
    assert(tt);
    hec->SetPars((double*)tt->GetArray());
    Int_t nHP = tgh->SetHitErrCalc(kIstId,hec,0);
    Info("Init","%s: %d Hitplanes", "IstHitErrs", nHP);
  }
     
  if (IAttr("activePxl")) {    // PXL error calculator
    mHitLoader[0]->AddDetector(kPxlId);
    TString myName("PxlHitErrs"); if (mFETracks) myName+="FE";
    StvHitErrCalculator *hec = new StvHitErrCalculator(myName, 2);
    TString ts("Calibrations/tracker/Stv");ts+=myName;
    TTable *tt = (TTable*)GetDataBase(ts);
    if (!tt) Error("Make","Table %s NOT FOUND",ts.Data());
    assert(tt);
    hec->SetPars((double*)tt->GetArray());
    Int_t nHP = tgh->SetHitErrCalc(kPxlId,hec,0);
    Info("Init","%s: %d Hitplanes", "PxlHitErrs", nHP);
  }
#ifndef kFtsIdentifier
#error
#endif
#ifdef kFtsIdentifier
  if (IAttr("activeFts")) {    // FTS error calculator
    mHitLoader[1]->AddDetector(kFtsId);
    TString myName("FtsHitErrs"); 
    auto *hec = (StvHitErrCalculator*)gROOT->ProcessLineFast("new StvFtsHitErrCalculator()");
    Int_t nHP = tgh->SetHitErrCalc(kFtsId,hec,0);
    Info("Init","%s: %d Hitplanes", "FtsHitErrs", nHP);
  }
#endif

//		In case of fithiterr utility working, selects special hits to speedup
  if (mFETracks) mHitLoader[0]->SetHitSelector();

  return kStOk;
} 

//_____________________________________________________________________________
Int_t StvMaker::InitRun(int run)
{
static int initialized = 0;
  if (initialized) return 0;

// 		Geometry via DBMaker
  TDataSet *myGeo = GetDataBase("VmcGeometry"); if (myGeo){};
  assert (gGeoManager);


  StTGeoProxy *tgh = StTGeoProxy::Inst();
  if (*SAttr("HitLoadOpt")) tgh->SetOpt(IAttr("HitLoadOpt"));

//	What is the geo version
  TString geoName(gGeoManager->GetName()); geoName.ToLower();

//		Activate detectors
#ifdef kFtsIdentifier
  if (IAttr("activeFts")) { int nakt = tgh->SetActive(kFtsId               );
                            assert(nakt);
			    SetAttr("activeTpc",0);
			    tgh->ls("SscA");
			  }
#endif
  if (IAttr("activeTpc")) { assert(tgh->SetActive(kTpcId,1,new StvTpcActive));}
  if (IAttr("activeEtr")) { assert(tgh->SetActive(kEtrId                   ));}
  if (IAttr("activeFgt")) { assert(tgh->SetActive(kFgtId                   ));}
  if (IAttr("activeSst")) { assert(tgh->SetActive(kSstId                   ));}
  if (IAttr("activeIst")) { assert(tgh->SetActive(kIstId                   ));}
  if (IAttr("activePxl")) { assert(tgh->SetActive(kPxlId                   ));}
  if (IAttr("activePixel")){assert(tgh->SetActive(kPxlId                   ));}

//		Now Initialize TGeo proxy
  tgh->Init(1+2+4);

  if (IAttr("activeTpc")) { 	//prompt hits for geo >=y2009
    StvTpcPrompt promp;
    tgh->InitHitPlane(&promp);

//	TPC has non standard TGeo. Edit it
    StvTpcEdit tpce;
    int nEdit = tgh->Edit(kTpcId,&tpce);	//Disable fake padrows
    Info("InitDetectors","%d fake TPC padrows disabled",nEdit);
  }//End Tpc special


  tgh->InitLayers();
  tgh->InitHitShape();

  tgh->Summary();


  StVMCApplication *app = new StVMCApplication("StVMC", "StVMC application");
  StvMCInitApp     *ini = new StvMCInitApp();
  app->SetInit(ini);
  app->Init();


//		Choose seed finders
  assert(gSystem->Load("StvSeed.so")>=0);
  const char *seedAtt[2]={"seedFinders","SeedFinders.fw"};
  mMaxTimes = IAttr("setMaxTimes");
  for (int jreg=0;jreg<2; jreg++) {	//0=midEta,1=forwardEta
    mHitLoader[jreg] = new StvHitLoader;
    if (mMaxTimes>1)mHitLoader[jreg]->SetMaxTimes(mMaxTimes);
    mSeedFinders[jreg] = new StvSeedFinders;
    if (IAttr("useEventFiller")) 
      mEventFiller[jreg]= new StvStEventFiller;
    if (IAttr("useVertexFinder")) 
      mVertexFinder[jreg] = new StvStarVertexFinder("GenericVertex");
    mTrackFinder[jreg] = new StvKalmanTrackFinder;
    mTrackFitter[jreg] = new StvKalmanTrackFitter;
    int iRefit = IAttr("Refit");
    mTrackFinder[jreg]->SetRefit(iRefit);


    TString seeds = SAttr(seedAtt[jreg]);
    if (!seeds.Length()) seeds = "Default";
    TObjArray *tokens = seeds.Tokenize(" .,");
    int seedErr=0; 
    const char *seedNick[]={"CA"                 ,"Default"                 ,"KNN"                ,"Fts"                     ,0};
    const char *seedNews[]={"new StvCASeedFinder","new StvDefaultSeedFinder","new StvKNSeedFinder","new StvDefaultSeedFinder",0};


    for (int idx=0;idx<=tokens->GetLast();idx++) {
    TString &chunk = ((TObjString*)tokens->At(idx))->String();
      for (int nick=0;seedNick[nick];nick++) {
	if (chunk.CompareTo(seedNick[nick],TString::kIgnoreCase)!=0) continue;
	if (nick==0) {
          assert(gSystem->Load("Vc.so")>=0);
          assert(gSystem->Load("TPCCATracker.so")	>=0);
	}
	StvSeedFinder *mySeedFinder = (StvSeedFinder*)gROOT->ProcessLineFast(seedNews[nick],&seedErr);
        if (IAttr("truthSeedFinder")) mySeedFinder->SetIdTruth();       
        if (TString(seedNick[nick])=="Fts") mySeedFinder->SetSgn(-1);
	assert(mySeedFinder && !seedErr);
        mSeedFinders[jreg]->Add(mySeedFinder);
	Info("InitRun","Added %s seed finder",mySeedFinder->GetName());
      }
    };
    delete tokens;
  }//end of Eta regions
   

  InitDetectors();


  int reg = 0;
  do {
    if (mHitLoader[reg] && mHitLoader[reg]->NumDetectors()==0) mHitLoader[reg]=0;		; 	
    if (mHitLoader[reg]) break;
    mSeedFinders[reg] = 0;
    mEventFiller[reg] = 0;
    mTrackFinder[reg] = 0;
    mTrackFitter[reg] = 0;
  } while (0);

  reg = 1;
  do {
    if (mHitLoader[reg] && mHitLoader[reg]->NumDetectors()==0) mHitLoader[reg]=0;		; 	
    if (mHitLoader[reg]) break;
    mSeedFinders[reg] = 0;
    mEventFiller[reg] = 0;
    mTrackFinder[reg] = 0;
    mTrackFitter[reg] = 0;
    mVertexFinder[reg]= 0;
  } while(0);

  
  InitPulls();

  new StvFitter();
       
  return StMaker::InitRun(run);
}

//_____________________________________________________________________________
Int_t StvMaker::Make()
{
static const StvConst *kons = new StvConst();
static StvToolkit* kit = StvToolkit::Inst();
  cout <<"StvMaker::Make() -I- Starting on new event"<<endl;
  int nVtx = 0;
  const StvHits *vertexes = 0;

  StEvent   *event = dynamic_cast<StEvent*>(GetInputDS("StEvent"));

  if (!event) return kStWarn;

  for (int reg=0;reg<2;reg++) { //Loop over eta regions
    const auto *par = kons->At(reg);
    if (mHitLoader[reg]){
      mSeedFinders [reg]->SetCons(par);
      mEventFiller [reg]->SetCons(par);
      mTrackFitter [reg]->SetCons(par);
      mCurTrackFitter = mTrackFitter[reg];
      mTrackFinder [reg]->SetCons(par);
      mCurTrackFinder = mTrackFinder[reg];
      mCurTrackFinder->SetFitter(mCurTrackFitter);
    }
    if (mVertexFinder[reg]) 
      mVertexFinder[reg]->SetCons(par);

    if (mHitLoader[reg]) {
      mHitLoader[reg]->LoadHits(event);
      kit->SetSeedFinders(mSeedFinders[reg] );
      kit->Reset();
      int n = (nVtx)? nVtx:1;
      for (int i=0;i<n;i++) {
        const float *V = (nVtx)? (*vertexes)[i]->x():0;
        mSeedFinders[reg]->SetVtx(V);
        int nTks = mTrackFinder[reg]->FindTracks();
        if (mMaxTimes>1) nTks = CleanGlobalTracks();
        TestGlobalTracks();
        mToTracks += nTks;
      }
    }
    if (mEventFiller[reg]) {
      mEventFiller[reg]->Set(event,&kit->GetTracks());
      mEventFiller[reg]->fillEvent();
    }

    do {//pseudo loop
      if (!mVertexFinder[reg]) 	break;
      nVtx = mVertexFinder[reg]->Fit(event);
      if (!nVtx) 		break;
      Info("Make","VertexFinder found %d vertices",nVtx);
      vertexes = &mVertexFinder[reg]->Result();
      if (!vertexes->size()) 	break;       
  //Set minimal errors
      for (size_t i=0;i<vertexes->size();i++) {
	StvHit *vtx=(*vertexes)[i];
	float *vtxErr = vtx->errMtx();
	if (vtxErr[5]>MIN_VTX_ERR2) continue;
	memset(vtxErr,0,sizeof(vtxErr[0])*6);
	vtxErr[0]=MIN_VTX_ERR2;
	vtxErr[2]=MIN_VTX_ERR2;
	vtxErr[5]=MIN_VTX_ERR2;
      }
    } while(0);
    //cout << "StvMaker::Make() -I- Got Vertex; extend Tracks"<<endl;
    if (nVtx) {
      if (mTrackFinder[reg]) 
        mTrackFinder[reg]->FindPrimaries(*vertexes);
      if (mEventFiller[reg]) 
        mEventFiller[reg]->fillEventPrimaries();
    }
  }//end regions
  if (mPullTTree) {FillPulls();}

  cout<< "StvMaker::Make() -I- Done"<<endl;
  StMaker::Make();
  kit->Clear();
  StTGeoProxy::Inst()->Clear();
  if (mFETracks && mToTracks>mFETracks) return kStEOF;
  return kStOK;
}
//_____________________________________________________________________________
Int_t StvMaker::InitPulls()
{
  if (!IAttr("makePulls")) 	return 0;
  
  const StChainOpt *bfc = GetChainOpt();
  assert(bfc);
  TFile *tfile  = GetTFile();
  if (!tfile) {
    TString ts  = bfc->GetFileIn();
    ts= gSystem->BaseName(ts);
    int ext = ts.Index(".");
    if (ext>0) ts.Replace(ext,999,"");
    ts +=".stipull.root";
    tfile = mPullFile = new TFile(ts,"RECREATE","TTree Stv Pulls ROOT file");
  }
  tfile->cd();
  mPullTTree = new TTree("StvPulls","TTree Stv pulls");
  mPullTTree->SetAutoSave(10000000);  // autosave when 0.01 Gbyte written
  mPullEvent = new StvPullEvent;
  TBranch *branch = mPullTTree->Branch("event", mPullEvent->ClassName(),&mPullEvent, 16000,99);
  branch->SetAutoDelete(kFALSE);
  if (mEventFiller[0])mEventFiller[0]->Set(mPullEvent);
  if (mEventFiller[1])mEventFiller[1]->Set(mPullEvent);
  return 0;
}
//_____________________________________________________________________________
Int_t StvMaker::FillPulls()
{
  StEvtHddr   *hddr = GetEvtHddr();
  mPullEvent->mRun  = hddr->GetRunNumber();
  mPullEvent->mEvt  = hddr->GetEventNumber();
  mPullEvent->mDate = hddr->GetDateTime();	//DAQ time (GMT)
  const StvHit *vertex = 0; int nMaxTks=0,ivertex=0;
  if (mVertexFinder[0] && mVertexFinder[0]->Result().size()) {
    int nVtx = mVertexFinder[0]->Result().size();
    for (int iv=0;iv<nVtx;iv++) {
      const StvHit *hit = mVertexFinder[0]->Result()[iv];
      if (nMaxTks > hit->getCount()) continue;
      nMaxTks = hit->getCount();
      vertex = hit; ivertex = iv+1;
  } }
  mPullEvent->mChi2 = 0;	
  memset(mPullEvent->mVtx,0,sizeof(mPullEvent->mVtx));
  memset(mPullEvent->mEtx,0,sizeof(mPullEvent->mEtx));
  if (vertex) {
    mPullEvent->mIVtx = ivertex;
    mPullEvent->mVtx[0] = vertex->x()[0];
    mPullEvent->mVtx[1] = vertex->x()[1];
    mPullEvent->mVtx[2] = vertex->x()[2];
    TCL::ucopy(vertex->errMtx(),mPullEvent->mEtx,6);
  }
  mPullEvent->Finish();
  mPullTTree->Fill();
  mPullEvent->Clear();
  return kStOK;  
}  
//_____________________________________________________________________________
int StvMaker::GeoTest()
{
 int ierr=0;
 if (!gGeoManager) return 1;
 for (int phi = 60,sect=1;phi>=-360+90;phi -=30,sect++)
 {
   double x = 100*cos(phi*3.1415/180);
   double y = 100*sin(phi*3.1415/180);
   double z = 100.;
   TGeoNode *node = gGeoManager->FindNode(x,y,z);if(node){};
   TGeoNode *parn = gGeoManager->GetMother(1);
   if (sect != parn->GetNumber()) ierr++;

   const char *path = gGeoManager->GetPath();
   printf("Sector=%d path=%s\n",sect,path);


 }
 for (int phi = 120,sect=13;sect<=24;phi +=30,sect++)
 {
   double x = 100*cos(phi*3.1415/180);
   double y = 100*sin(phi*3.1415/180);
   double z = -100.;
   TGeoNode *node = gGeoManager->FindNode(x,y,z);if(node){};
   TGeoNode *parn = gGeoManager->GetMother(1);
   if (sect-12 != parn->GetNumber()) ierr++;
   const char *path = gGeoManager->GetPath();
   printf("Sector=%d path=%s\n",sect,path);
 }
return ierr;
}
//________________________________________________________________________________
static bool TrackCompareStatus(const StvTrack *a, const StvTrack *b)
{
  int nA = a->GetNHits();
  int nB = b->GetNHits();
  if (nA!=nB) return (nA > nB);
  return (a->GetXi2Aux() <b->GetXi2Aux());
//  return (a->GetXi2W()<b->GetXi2W());
}
//_____________________________________________________________________________
int StvMaker::CleanGlobalTracks()
{
static StvToolkit *kit = StvToolkit::Inst();
  StvTracks &trackContainer = kit->GetTracks();

  for (auto it = trackContainer.begin(); it!=trackContainer.end(); ++it) 
  {
    auto* kTrack = *it;
    kTrack->SetUnused();
  }
  trackContainer.sort(TrackCompareStatus );

  for (auto it = trackContainer.begin(); it!=trackContainer.end();) 
  {
    auto* kTrack =*it;
    int nHits=0,nNits=0;
    for (auto nodeIt =kTrack->begin();nodeIt!=kTrack->end();nodeIt++) 
    {
      auto *node = *nodeIt;
      StvHit *hit = node->GetHit(); if (!hit) 		continue;
      if (hit->timesUsed()) { nNits++; node->SetHit(0)     ;}
      else                  { nHits++; hit->setTimesUsed(1);}
    }
    if (nHits<4) 			{ it = trackContainer.erase(it); continue; }
    StvNode *dcaNode = kTrack->GetNode(StvTrack::kDcaPoint);
    if (dcaNode) {
       kTrack->remove(dcaNode);
    }


    int ans = mCurTrackFitter->Refit(kTrack,1);
    if (ans || kTrack->GetNHits()<4)	{ it = trackContainer.erase(it); continue; }
    mCurTrackFinder->MakeDcaNode(kTrack);
    ++it;
  }

  for (auto it = trackContainer.begin(); it!=trackContainer.end(); ++it) 
  {
    auto* kTrack = *it;
    for (auto nodeIt =kTrack->begin();nodeIt!=kTrack->end();nodeIt++) 
    {
      auto *node = *nodeIt;
      auto *hit = node->GetHit(); if (!hit) continue;
      assert(hit->timesUsed()==1);
    }
  }
  return trackContainer.size();
}

//_____________________________________________________________________________
int StvMaker::TestGlobalTracks() const
{
static StvToolkit *kit = StvToolkit::Inst();
  StvTracks &trackContainer = kit->GetTracks();

  for (auto it = trackContainer.begin(); it!=trackContainer.end(); ++it) 
  {
    auto* kTrack = *it;
    int nHits = kTrack->GetNHits();
    StvDebug::Count("IdQua_vs_NHits",nHits,kTrack->GetQua()*100);
  }
  return 0;
}
