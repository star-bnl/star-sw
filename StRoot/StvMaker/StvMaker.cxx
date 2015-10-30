// $Id: StvMaker.cxx,v 1.50 2015/10/30 20:36:02 perev Exp $
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
#include "Stv/StvToolkit.h"
#include "Stv/StvSeedFinder.h"
#include "Stv/StvKalmanTrackFinder.h"
#include "StvUtil/StvHitErrCalculator.h"
#include "StvUtil/StvHitErrCalculator.h"
#include "StvUtil/StvFtsHitErrCalculator.h"
#include "Stv/StvFitter.h"
#include "Stv/StvKalmanTrackFitter.h"
#include "StvStEventFiller.h"
#include "StvStarVertexFinder.h"
#include "StvTpcActive.h"
#include "StvStEventMaker.h"
/// Definion of minimal primary vertex errors.
/// Typical case,vertex got from simulations with zero errors.
/// But zero errors could to unpredicted problems
/// Now minimal possible error is 1 micron
static const float MIN_VTX_ERR2 = 1e-4*1e-4;
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
  
  StvToolkit   *kit = StvToolkit::Inst();
  StTGeoProxy *tgh = StTGeoProxy::Inst();
  if (*SAttr("HitLoadOpt")) tgh->SetOpt(IAttr("HitLoadOpt"));

//	What is the geo version
  TString geoName(gGeoManager->GetName()); geoName.ToLower();
  int yGeo = 2014;
  if (geoName(0,2)=="y2") yGeo = TString(geoName(1,99)).Atoi();


//		Activate detectors
  if (IAttr("activeFts")) { assert(tgh->SetActive(kFtsId                   ));
                            SetAttr("activeTpc",0);}
  if (IAttr("activeTpc")) { assert(tgh->SetActive(kTpcId,1,new StvTpcActive));}
  if (IAttr("activeEtr")) { assert(tgh->SetActive(kEtrId                   ));}
  if (IAttr("activeFgt")) { assert(tgh->SetActive(kFgtId                   ));}
  if (IAttr("activeIst")) { assert(tgh->SetActive(kIstId                   ));}
  if (IAttr("activePxl")) { assert(tgh->SetActive(kPxlId                   ));}
  if (IAttr("activePixel")){assert(tgh->SetActive(kPxlId                   ));}

//		Now Initialize TGeo proxy
  tgh->Init(1+2+4);
  if (yGeo>=2009) { 	//no prompt hits for geo <y2009
    StvTpcPrompt promp;
    tgh->InitHitPlane(&promp);
  }

//	TPC has non standard TGeo. Edit it
  if (yGeo>=2009) { 
    StvTpcEdit tpce;
    int nEdit = tgh->Edit(kTpcId,&tpce);	//Disable fake padrows
    Info("InitDetectors","%d fake TPC padrows disabled",nEdit);
  }
  tgh->InitLayers();
  tgh->InitHitShape();

  tgh->Summary();



//	Check is Stv is running in fit hit error utility

  mFETracks = IAttr("fiterr");
  if (mFETracks>0) SetAttr(".privilege",1,"");
  
//		TGeo herlper is ready, add error calculators
  int actTpc = IAttr("activeTpc");
  if (actTpc) {	//TPC error calculators
  
    const char*  innOutNames[]  = {"StvTpcInnerHitErrs"    ,"StvTpcOuterHitErrs" 
                                  ,"StvTpcInnerPromptErrs" ,"StvTpcOuterPromptErrs",0};
    const char* innOut = 0;
    for (int io=0;(innOut=innOutNames[io]);io++) {
      if (yGeo<2009 && strstr(innOut,"Prompt")) 	break;
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
    if (yGeo>=2009) kit->HitLoader()->SetHitActor(new StvTpcHitActor);
  }

  if (IAttr("activeEtr")) {	//Etr error calculators
    TString myName("EtrHitErrs"); if (mFETracks) myName+="FE";
    StvHitErrCalculator *hec = new StvHitErrCalculator(myName,2);
    double etrPars[StvHitErrCalculator::kMaxPars]={9e-4,9e-4};
    hec->SetPars(etrPars);
    int nHP = tgh->SetHitErrCalc(kEtrId,hec,0);
    Info("Init","%s: %d HitPlanes","EtrHitErrs",nHP);
    assert(nHP);
  }

  if (IAttr("activeFgt")) {    // FGT error calculator
    TString myName("FgtHitErrs"); if (mFETracks) myName+="FE";
    StvHitErrCalculator *hec = new StvHitErrCalculator(myName, 2);
    Double_t fgtPars[ StvHitErrCalculator::kMaxPars]={
      9e-4,9e-4,
    };
    hec -> SetPars( fgtPars );
    Int_t nHP = tgh->SetHitErrCalc(kFgtId,hec,0);
    Info("Init","%s: %d Hitplanes", "FgtHitErrs", nHP);
  }
  if (IAttr("activeIst")) {    // IST error calculator
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

  if (IAttr("activeFTS")) {    // FTS error calculator
    mSeedFinder->SetMinHits(3);
    TString myName("FtsHitErrs"); 
    StvHitErrCalculator *hec = new StvFtsHitErrCalculator(myName);
    Int_t nHP = tgh->SetHitErrCalc(kFtsId,hec,0);
    Info("Init","%s: %d Hitplanes", "FtsHitErrs", nHP);
  }

  kit->SetHitLoader(new StvHitLoader);
  assert(kit->HitLoader()->Init());

//		In case of fithiterr utility working, selects special hits to speedup
  if (mFETracks) kit->HitLoader()->SetHitSelector();

  return kStOk;
} 

//_____________________________________________________________________________
Int_t StvMaker::InitRun(int run)
{
static int initialized = 0;
  if (initialized) return 0;
  new StvConst();
  TString geom;
// 		Geometry via DBMaker
  TDataSet *myGeo = GetDataBase("VmcGeometry"); if (myGeo){};
  if (gGeoManager) {
    geom = gGeoManager->GetName();
  } else {
    geom = GetChainOpt()->GetGeometry();
    assert(geom.Length());
    StTGeoProxy::Inst()->Load(geom);
  }
  assert(!GeoTest());

  StVMCApplication *app = new StVMCApplication(geom, "StVMC application");
  StvMCInitApp *ini = new StvMCInitApp();
  app->SetInit(ini);
  app->Init();


  StvToolkit *kit =StvToolkit::Inst();

//		Choose seed finders
  assert(gSystem->Load("StvSeed.so")>=0);
  TString seeds = SAttr("seedFinders");
  if (!seeds.Length()) seeds = "Default";
  TObjArray *tokens = seeds.Tokenize(" .,");
  mSeedFinder=0; int seedErr=0; 
  const char *seedNick[]={"CA"                 ,"Default"                 ,"KN"                 ,0};
  const char *seedNews[]={"new StvCASeedFinder","new StvDefaultSeedFinder","new StvKNSeedFinder",0};

  for (int idx=0;idx<=tokens->GetLast();idx++) {
  TString &chunk = ((TObjString*)tokens->At(idx))->String();
    for (int nick=0;seedNick[nick];nick++) {
      if (chunk.CompareTo(seedNick[nick],TString::kIgnoreCase)!=0) continue;
      if (nick==0) {
        assert(gSystem->Load("Vc.so")		>=0);
        assert(gSystem->Load("TPCCATracker.so")	>=0);}
      mSeedFinder = (StvSeedFinder*)gROOT->ProcessLineFast(seedNews[nick],&seedErr);
      assert(mSeedFinder && !seedErr);
      kit->SetSeedFinder (mSeedFinder); break;
      Info("InitRun","Added %s seed finder",mSeedFinder->GetName());
    }
  };
  delete tokens;
  InitDetectors();

  StvKalmanTrackFinder *tf = new StvKalmanTrackFinder;
  kit->SetTrackFinder(tf);
  int iRefit = IAttr("Refit");
  tf->SetRefit(iRefit);
  
  new StvFitter();
  new StvKalmanTrackFitter();
  if (IAttr("useEventFiller"))
    mEventFiller= new StvStEventFiller;
  InitPulls();
  if (IAttr("useVertexFinder")) 
    mVertexFinder = new StvStarVertexFinder("GenericVertex");
       
  return StMaker::InitRun(run);
}

//_____________________________________________________________________________
Int_t StvMaker::Make()
{
  cout <<"StvMaker::Make() -I- Starting on new event"<<endl;

  StEvent   *event = dynamic_cast<StEvent*>(GetInputDS("StEvent"));

  if (!event) return kStWarn;
  StvToolkit* kit = StvToolkit::Inst();
  kit->HitLoader()->LoadHits(event);
  kit->Reset();
  int nTks = kit->TrackFinder()->FindTracks();
  mToTracks += nTks;

  if (mEventFiller) {
    mEventFiller->Set(event,&kit->GetTracks());
    mEventFiller->fillEvent();
  }

  do {//pseudo loop
    if (!mVertexFinder) break;
    int nVtx = mVertexFinder->Fit(event);
    if (!nVtx) 		break;
    Info("Make","VertexFinder found %d vertices",nVtx);
    const StvHits &vertexes = mVertexFinder->Result();
    if (!vertexes.size()) break;       
//Set minimal errors
    for (size_t i=0;i<vertexes.size();i++) {
      StvHit *vtx=vertexes[i];
      float *vtxErr = vtx->errMtx();
      if (vtxErr[5]>MIN_VTX_ERR2) continue;
      memset(vtxErr,0,sizeof(vtxErr[0])*6);
      vtxErr[0]=MIN_VTX_ERR2;
      vtxErr[2]=MIN_VTX_ERR2;
      vtxErr[5]=MIN_VTX_ERR2;
    }
    //cout << "StvMaker::Make() -I- Got Vertex; extend Tracks"<<endl;
    kit->TrackFinder()->FindPrimaries(vertexes);
    if (mEventFiller) mEventFiller->fillEventPrimaries();
  } while(0);

  int iAns=kStOK,iAnz;
  if (mPullTTree) {iAns = FillPulls();}
  cout<< "StvMaker::Make() -I- Done"<<endl;
  iAnz = StMaker::Make();
  kit->Clear();
  StTGeoProxy::Inst()->Clear();
  if (iAns) return iAns;
  if (iAnz) return iAnz;
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
  mEventFiller->Set(mPullEvent);
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
  if (mVertexFinder && mVertexFinder->Result().size()) {
    int nVtx = mVertexFinder->Result().size();
    for (int iv=0;iv<nVtx;iv++) {
      const StvHit *hit = mVertexFinder->Result()[iv];
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

