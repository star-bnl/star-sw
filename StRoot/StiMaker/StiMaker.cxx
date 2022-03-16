// $Id: StiMaker.cxx,v 1.242 2019/06/20 16:04:55 genevb Exp $
/// \File StiMaker.cxx
/// \author M.L. Miller 5/00
/// \author C Pruneau 3/02
//
/*!

\class StiMaker

\author M.L. Miller 5/00
\author C Pruneau 3/02
\author V Perev 2005

A maker StiMaker is a steering maker for Sti package.
<br>
Main tasks:
<ul>
<li> Create StiHits;
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
     SetAttr("useTpc"  ,1) && SetAttr("activeTpc"  ,1) 	// default
     SetAttr("useSvt",  1) && SetAttr("activeSvt"  ,0) 	// default
     SetAttr("useSsd"  ,0) && SetAttr("activeSsd"  ,0)	// default Off

     SetAttr("usePixel",0) && SetAttr("activePixel",0)	// default Off
     SetAttr("useIst"  ,0) && SetAttr("activeIst"  ,0)	// default Off

     SetAttr("useHpd"  ,0) && SetAttr("activeHpd"  ,0)	// default Off

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
#include "TSystem.h"
#include "TTree.h"
#if ROOT_VERSION_CODE < 331013
#include "TCL.h"
#else
#include "TCernLib.h"
#endif
#include "TDataSet.h"
#include "TDataSetIter.h"
#include "StMessMgr.h"
#include "StChainOpt.h"
#include "SystemOfUnits.h"
#include "StMemStat.h"
#include "PhysicalConstants.h"
#include "StDetectorId.h"
#include "StEventTypes.h"
#include "Sti/Base/EditableFilter.h"
#include "Sti/StiKalmanTrackFinder.h"
#include "Sti/StiKalmanTrackFitter.h"
#include "Sti/StiTrackContainer.h"
#include "Sti/StiDefaultTrackFilter.h"
#include "Sti/StiMasterDetectorBuilder.h"
#include "Sti/Star/StiStarDetectorGroup.h"
#include "StiTpc/StiTpcDetectorGroup.h"
#include "StiTpc/StiTpcHitLoader.h"
#include "StiSvt/StiSvtDetectorGroup.h"
#include "StiSsd/StiSsdDetectorGroup.h"
#include "StiSsd/StiSstDetectorGroup.h"
#include "StiPxl/StiPxlDetectorGroup.h"
#include "Sti/StiKalmanTrackNode.h"
#include "Sti/StiKalmanTrack.h"
#include "Sti/StiVertexFinder.h"
#include "Sti/StiDetectorContainer.h"
#include "StiMaker/StiStEventFiller.h"
#include "Sti/StiDefaultToolkit.h"
#include "StiMaker.h"
#include "TFile.h"
#include "TCanvas.h"
#include "StDetectorDbMaker/StiKalmanTrackFinderParameters.h"
#include "StDetectorDbMaker/StiKalmanTrackFitterParameters.h"

#include "StDetectorDbMaker/StiHitErrorCalculator.h"
// #include "StiRnD/Ist/StiIstDetectorGroup.h"
// #include "StiRnD/Ist/StiIstDetectorGroup.h"
#include "StiIst/StiIstDetectorGroup.h"

#include "StiUtilities/StiDebug.h"
#include "StiUtilities/StiPullEvent.h"
#include "TDataSet.h"
#include "TGeometry.h"
#include "Sti/StiTimer.h"
#include "StiDetectorVolume.h"
#include "StarMagField.h"
#if 0
#include "StG2TrackVertexMap.h"
#endif
#include "StTpcDb/StTpcDb.h"
#include "StSsdDbMaker/StSsdDbMaker.h"
#include "StSsdDbMaker/StSstDbMaker.h"
#include "StSvtDbMaker/StSvtDbMaker.h"
/// Definion of minimal primary vertex errors.
/// Typical case,vertex got from simulations with zero errors.
/// But zero errors could to unpredicted problems
/// Now minimal possible error is 1 micron
static const float MIN_VTX_ERR2 = 1e-4*1e-4;
enum { kHitTimg,kGloTimg,kVtxTimg,kPriTimg,kFilTimg};

void CountHits();

//_____________________________________________________________________________
StiMaker::StiMaker(const Char_t *name) :
    StMaker(name),
    fVolume(0),
    _initialized(false),
    _toolkit(0),
    _hitLoader(0),
    _seedFinder(0),
    _tracker(0),
    _fitter(0),
    _eventFiller(0),
    _trackContainer(0),
    _vertexFinder(0),
    _loaderTrackFilter(0),
    _loaderHitFilter(0)

{
  mMaxTimes = 0;
  memset(mTimg,0,sizeof(mTimg));
  cout <<"StiMaker::StiMaker() -I- Starting"<<endl;
  mPullFile=0; mPullEvent=0;mPullTTree=0;
  memset(mPullHits,0,sizeof(mPullHits));
  mTotPrimTks[0]=mTotPrimTks[1]=0;
  if (!StiToolkit::instance()) new StiDefaultToolkit;
  _toolkit = StiToolkit::instance();
  SetAttr("useTpc"		,kTRUE);
  SetAttr("activeTpc"		,kTRUE);

  SetAttr("useSvt"		,kTRUE);
  SetAttr("useSsd"		,kTRUE);

  //SetAttr("usePixel"		,kTRUE);
  //SetAttr("useSst"		,kTRUE);
  //SetAttr("useIst"		,kTRUE);

//SetAttr("activeSsd"		,kTRUE);
//SetAttr("useAux"		,kTRUE); // Auxiliary info added to output for evaluation
  SetAttr("useEventFiller"      ,kTRUE);
  SetAttr("useTracker"          ,kTRUE);
  SetAttr("useVertexFinder"     ,kTRUE);
  SetAttr("Alignment"           ,kFALSE);
  if (strstr(gSystem->Getenv("STAR"),".DEV"))
     SetAttr("useAux",kTRUE); // Auxiliary info added to output for evaluation
}

//_____________________________________________________________________________
StiMaker::~StiMaker()
{
  cout <<"StiMaker::~StiMaker() -I- Started/Done"<<endl;
}

//_____________________________________________________________________________
void StiMaker::Clear(const char*)
{
  if (_tracker  ) _tracker->clear();
  if (mPullEvent) mPullEvent->Clear();
  StMaker::Clear();
}

//_____________________________________________________________________________
Int_t StiMaker::Finish()
{
  StiDebug::Finish();
//	Finish Pull
  if (mPullTTree) {
    if  (Debug()) mPullTTree->Print();
    if (mPullFile) {
      TFile *tfile = mPullTTree->GetCurrentFile(); //just in case we switched to a new file
      tfile->Write();
      tfile->Close();
      mPullFile  = 0;
      mPullTTree = 0;
    }
  }
  StiTimer::Print();
  StiTimer::Clear();

static const char *timg[] = {"HitLoa","GlobFnd","VtxFnd","PriFnd","FilFnd",0};
  if (mTimg[0]) {
    for (int i=0;timg[i];i++) {
      Info("Timing","%s(%d) \tCpuTime = %6.2f seconds,\tPerEvent = %g seconds"
      ,timg[i],mTimg[i]->Counter(),mTimg[i]->CpuTime()
      ,mTimg[i]->CpuTime()/mTimg[i]->Counter());
  } }
  if (_tracker) _tracker->finish();

  return StMaker::Finish();
}

//_____________________________________________________________________________
Int_t StiMaker::Init()
{

  StiDebug::Init();
  StiTimer::Init("StiTrackFinder::find() TIMING"
	        ,StiTimer::fgFindTimer,StiTimer::fgFindTally);
  if (IAttr("Alignment")) SetAttr(".Privilege",kTRUE);
  _loaderHitFilter = 0; // not using this yet.
  mTotPrimTks[1] = IAttr("maxTotPrims");
  if (*SAttr("maxRefiter")) StiKalmanTrack::setMaxRefiter(IAttr("maxRefiter"));


  if (*SAttr("maxRefiter")) StiKalmanTrack::setMaxRefiter(IAttr("maxRefiter"));
  if (IAttr("useTiming")) {
    for (int it=0;it<(int)(sizeof(mTimg)/sizeof(mTimg[0]));it++){
      mTimg[it]= new TStopwatch(); mTimg[it]->Stop();
    } }

  return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StiMaker::InitDetectors()
{
  StiDetectorGroup<StEvent> * group;
  cout<<"StiMaker::InitDetectors() -I- Adding detector group:Star"<<endl;
  _toolkit->add(new StiStarDetectorGroup(false));
  if (IAttr("useTpc") && gStTpcDb)
    {
      cout<<"StiMaker::InitDetectors() -I- Adding detector group:TPC"<<endl;
      _toolkit->add(group = new StiTpcDetectorGroup(IAttr("activeTpc"), IAttr("activeiTpc")));
      group->setGroupId(kTpcId);
      StiTpcHitLoader* hitLoader = (StiTpcHitLoader*) group->hitLoader();
      if (IAttr("activeSvt") || IAttr("activeSsd") || IAttr("skip1row")) {// skip 1 row
	hitLoader->setMinRow(2);
      }
      if (IAttr("EastOff")) {
	hitLoader->setMinSector(1);
	hitLoader->setMaxSector(12);
      }
      if (IAttr("WestOff")) {
	hitLoader->setMinSector(13);
	hitLoader->setMaxSector(24);
      }
      cout << "StiMaker::InitDetectors() -I- use hits in sectors["
	   << hitLoader->minSector() << "," << hitLoader->maxSector() << "] and rows["
	   << hitLoader->minRow() << ",*]" << endl;
      if (IAttr("laserIT")) {
	StiKalmanTrackNode::SetLaser(1);
	cout << "StiMaker::InitDetectors() -I- set laser time of flight correction" << endl;
      }
    }
  if (IAttr("useSvt") && gStSvtDbMaker)
    {
    cout<<"StiMaker::Init() -I- Adding detector group:SVT"<<endl;
    _toolkit->add(group = new StiSvtDetectorGroup(IAttr("activeSvt")));
    group->setGroupId(kSvtId);
    }

  // SSD or SST - they share the db and the kSsdId
  if (IAttr("useSst") && gStSstDbMaker){
    cout<<"StiMaker::Init() -I- Adding detector group:Sst (ssd)"<<endl;
    _toolkit->add(group = new StiSstDetectorGroup(IAttr("activeSst")));
    group->setGroupId(kSstId);

  } else if ( IAttr("useSsd") && gStSsdDbMaker){
    cout<<"StiMaker::Init() -I- Adding detector group:Ssd"<<endl;
    _toolkit->add(group = new StiSsdDetectorGroup(IAttr("activeSsd")));
    group->setGroupId(kSsdId);
  }


  if (IAttr("usePixel"))
    {
      cout<<"StiMaker::Init() -I- Adding detector group:PIXEL"<<endl;
      _toolkit->add(group = new StiPxlDetectorGroup(IAttr("activePixel")));
      group->setGroupId(kPxlId);
    }
 if (IAttr("useIst"))
    {
      cout<<"StiMaker::Init() -I- Adding detector group:Ist"<<endl;
      _toolkit->add(group = new StiIstDetectorGroup(IAttr("activeIst")));
      group->setGroupId(kIstId);
    }
  return kStOk;
}

//_____________________________________________________________________________
Int_t StiMaker::InitRun(int run)
{
  if (!_initialized) {
    cout <<"StiMaker::InitRun() -I- Initialization Segment Started"<<endl;
    InitDetectors();
    // Load Detector related parameters
    StiMasterDetectorBuilder * masterBuilder = _toolkit->getDetectorBuilder();
    masterBuilder->build(*this);
    StiDetectorContainer * detectorContainer = _toolkit->getDetectorContainer();
    detectorContainer->initialize();//build(masterBuilder);
    detectorContainer->reset();
    _seedFinder = _toolkit->getTrackSeedFinder();
    _seedFinder->initialize();
    _hitLoader  = _toolkit->getHitLoader();
    _tracker=0;
    mMaxTimes = IAttr("setMaxTimes");

    if (IAttr("useTracker")) {

      _tracker = (StiKalmanTrackFinder *)(_toolkit->getTrackFinder());

      do  {

        TString seedFinders = SAttr("seedFinders");

        // Return without changing anything if attribute's value is empty
        if (seedFinders.Length()) {	//found list of ssedfinders

          TObjArray *sub_strings = seedFinders.Tokenize(" .,");

          for (int i=0; i <= sub_strings->GetLast(); i++) {
            int n = 0;
            TString &sub_string = static_cast<TObjString*>( sub_strings->At(i))->String();
            if (sub_string[0]=='!') continue; 	//commented out
            if ( !sub_string.CompareTo("CA", TString::kIgnoreCase) ) 
              {n++;_tracker->addSeedFinder(_toolkit->getTrackSeedFinderCA());}
            if ( !sub_string.CompareTo("Def", TString::kIgnoreCase) ) 
              {n++;_tracker->addSeedFinder(_toolkit->getTrackSeedFinder());}
            if ( !sub_string.CompareTo("KNN", TString::kIgnoreCase) ) 
              {n++;_tracker->addSeedFinder(_toolkit->getTrackSeedFinderKNN());}
            assert(n);
          }
          delete sub_strings; break;
         }// end seedfinder list

       if (IAttr("StiCA")) {
          _tracker->addSeedFinder(_toolkit->getTrackSeedFinderCA());
          _tracker->addSeedFinder(_toolkit->getTrackSeedFinder());
          break;
       }
      // Default case, Sti seed finder only
      _tracker->addSeedFinder(_toolkit->getTrackSeedFinder());

      }while(0);

    }//end tracker

    // useTreeSearch flag means
    // useTreeSearch == tpcFlag *4 + hftFlag
    // flag == treeSearchOn + 2*treeSearchFull
    // treeSearchOn==1 means tree search is ON
    // treeSearchFull==1 means tree search includes case that existing hit
    //                                on this level could be omitted
    // Typical case chain->SetAttr("useTreeSearch",(1+2) +4*(1),"Sti");
    // means HFT full tree searh, TPC tree search only with existing hits, no hits is not considered

    if (*SAttr("useTreeSearch")) _tracker->setComb(IAttr("useTreeSearch"));
    if ( IAttr("useTiming"    )) _tracker->setTiming();
    _fitter  = dynamic_cast<StiKalmanTrackFitter *>(_toolkit->getTrackFitter());

    //if (*SAttr("useMCS")) StiKalmanTrackNode::setMCS(IAttr("useMCS"));

    _eventFiller=0;
    if (IAttr("useEventFiller")) {
      _eventFiller =  new StiStEventFiller();
      _eventFiller->setUseAux(IAttr("useAux"));
      InitPulls();
    }
    _trackContainer = _toolkit->getTrackContainer();
    _vertexFinder   = 0;
    if (GetTopChain()->GetMakerInheritsFrom("StGenericVertexMaker")) {
      _vertexFinder   = _toolkit->getVertexFinder();
    }
    if (_tracker) {
      _tracker->initialize();
      _tracker->clear();
    }
    _initialized=true;
    cout <<"StiMaker::InitRun() -I- Initialization Segment Completed"<<endl;
  }

  return StMaker::InitRun(run);
}

//_____________________________________________________________________________
Int_t StiMaker::Make()
{
  cout <<"StiMaker::Make() -I- Starting on new event"<<endl;
  Int_t iAns=kStOK,iAnz=0; if (iAns){};
  if (! _tracker) return kStWarn;
  StEvent   * event = dynamic_cast<StEvent*>( GetInputDS("StEvent") );
  if (!event) return kStWarn;
  eventIsFinished = false;

    _tracker->clear();
    if (mTimg[kHitTimg]) mTimg[kHitTimg]->Start(0);
    _hitLoader->loadEvent(event,_loaderTrackFilter,_loaderHitFilter);
    if (mMaxTimes) _hitLoader->setMaxTimes(mMaxTimes);
    if (mTimg[kHitTimg]) mTimg[kHitTimg]->Stop();

    iAnz = MakeGlobalTracks(event);
    if (iAnz) {MyClear(); return iAnz;}
    CountHits();

    if (_vertexFinder) {
      iAnz = MakePrimaryTracks(event);
      if (iAnz) {MyClear(); return iAnz;}
    } else {
      iAnz = StMaker::Make();
    }
    if (mPullTTree) {iAns = FillPulls();}
    cout<< "StiMaker::Make() -I- Done"<<endl;
    MyClear();
    if (iAnz) return iAnz;
    if (mTotPrimTks[1] && mTotPrimTks[0]>mTotPrimTks[1]) return kStStop;
    if (IAttr("Alignment") &&  ! _tracker->getNTracks()) return kStErr;
    return (!iAns)? kStOK:kStWarn;
}

//_____________________________________________________________________________
Int_t StiMaker::MakeGlobalTracks(StEvent   * event) {
  if (mTimg[kGloTimg]) mTimg[kGloTimg]->Start(0);
  _tracker->reset();    // get the rest
  _tracker->findTracks();    // get the rest
  if (mMaxTimes>1) CleanGlobalTracks();
  FinishTracks(0);
  if (mTimg[kGloTimg]) mTimg[kGloTimg]->Stop();
  if (mTimg[kFilTimg]) mTimg[kFilTimg]->Start(0);
  if (_eventFiller)
    _eventFiller->fillEvent(event, _trackContainer);
  if (mTimg[kFilTimg]) mTimg[kFilTimg]->Stop();
  return kStOK;
}
//_____________________________________________________________________________
Int_t StiMaker::MakePrimaryTracks(StEvent   * event) {
  if (! _vertexFinder) return kStWarn;
  if (mTimg[kVtxTimg]) mTimg[kVtxTimg]->Start(0);
  _vertexFinder->fit(event);
  const std::vector<StiHit*> *vertexes = _vertexFinder->result();
  if (mTimg[kVtxTimg]) mTimg[kVtxTimg]->Stop();
  if (vertexes && vertexes->size())	  {
    //Set minimal errors
    for (size_t i=0;i<vertexes->size();i++) {
      StiHit *vtx=(*vertexes)[i];
      float vtxErr[6];
      memcpy(vtxErr,vtx->errMtx(),sizeof(vtxErr));
      if (vtxErr[0]>MIN_VTX_ERR2
	  &&  vtxErr[2]>MIN_VTX_ERR2
	  &&  vtxErr[5]>MIN_VTX_ERR2) continue;
      memset(vtxErr,0,sizeof(vtxErr));
      vtxErr[0]=MIN_VTX_ERR2;
      vtxErr[2]=MIN_VTX_ERR2;
      vtxErr[5]=MIN_VTX_ERR2;
      vtx->setError(vtxErr);
    }
    if (mTimg[kPriTimg]) mTimg[kPriTimg]->Start(0);

    _tracker->extendTracksToVertices(*vertexes);
    mTotPrimTks[0]+=_tracker->getNPrims();
    FinishTracks(1);
    if (mTimg[kPriTimg]) mTimg[kPriTimg]->Stop();

    //cout << "StiMaker::Make() -I- Primary Filling"<<endl;
    if (mTimg[kFilTimg]) mTimg[kFilTimg]->Start(0);
    if (_eventFiller) {_eventFiller->fillEventPrimaries(); /* fillVxFlags(); */}
    if (mTimg[kFilTimg]) mTimg[kFilTimg]->Stop();
  }
  return kStOK;
}
//_____________________________________________________________________________
void StiMaker::MyClear()
{
//    cout << "StiMaker -I- Perform Yuri's clear... ;-)" << endl;
//      StMemStat::PrintMem("Before StiFactory clear()");
      _toolkit->getHitFactory()->clear();
      _toolkit->getTrackNodeFactory()->clear();
      _toolkit->getTrackNodeExtFactory()->clear();
      _toolkit->getTrackNodeInfFactory()->clear();
      _toolkit->getTrackFactory()->clear();
//      StMemStat::PrintMem("After  StiFactory clear()");
}




//_____________________________________________________________________________
Int_t StiMaker::InitPulls()
{
  if (!IAttr("makePulls")) 	return 0;

  const StChainOpt *bfc = GetChainOpt();
  assert(bfc);
  TFile *tfile  = GetTFile();
  if (!tfile) {
    TString ts  = bfc->GetFileIn();
    if ( ts == "" ) {
      ts = "pullfile";
    }
    ts= gSystem->BaseName(ts);
    int ext = ts.Index(".");
    if (ext>0) ts.Replace(ext,999,"");
    ts +=".stipull.root";
    tfile = mPullFile = new TFile(ts,"RECREATE","TTree Sti Pulls ROOT file");
  }
  tfile->cd();
  mPullTTree = new TTree("StiPulls","TTree Sti pulls");
  mPullTTree->SetAutoSave(100000000);  // autosave when 0.1 Gbyte written
  mPullEvent = new StiPullEvent;
  TBranch *branch = mPullTTree->Branch("event", mPullEvent->ClassName(),&mPullEvent, 16000,99);
  branch->SetAutoDelete(kFALSE);
  _eventFiller->setPullEvent(mPullEvent);
  return 0;
}
//_____________________________________________________________________________
Int_t StiMaker::FillPulls()
{
  StEvtHddr   *hddr = GetEvtHddr();
  mPullEvent->mRun  = hddr->GetRunNumber();
  mPullEvent->mEvt  = hddr->GetEventNumber();
  mPullEvent->mDate = hddr->GetDateTime();	//DAQ time (GMT)
  mPullEvent->mChi2 = 0;

  memset(mPullEvent->mVtx,0,sizeof(mPullEvent->mVtx));
  memset(mPullEvent->mEtx,0,sizeof(mPullEvent->mEtx));
  if (_vertexFinder) {
    StiHit *vertex   = _vertexFinder->getVertex(0);
    if (vertex) {
      mPullEvent->mVtx[0] = vertex->x_g();
      mPullEvent->mVtx[1] = vertex->y_g();
      mPullEvent->mVtx[2] = vertex->z_g();
      TCL::ucopy(vertex->errMtx(),mPullEvent->mEtx,6);
    }
  }
  mPullEvent->Finish();
  mPullTTree->Fill();
  for (int i=0; i<3; i++) {mPullHits[i]+=mPullEvent->mNHits[i];}
  if (! IAttr(".Privilege")) return kStOK;

  int k;for (k=2; k>=0; k--) {if (mPullHits[k]) break;}
  if (k<0) return kStOK;
  k = mPullHits[k]<<(k*3);
  if (k>1000000) return kStSTOP;
  return kStOK;
}
//_____________________________________________________________________________
TDataSet  *StiMaker::FindDataSet (const char* logInput,const StMaker *uppMk,
                                        const StMaker *dowMk) const
{
  TDataSet *ds = StMaker::FindDataSet(logInput,uppMk,dowMk);

  if (ds || strcmp(logInput,"STIGEOM")) return ds;

//  if (!fVolume && _toolkit) ((StiMaker *)this)->fVolume = new StiDetectorVolume(*_toolkit->getDetectorBuilder(), kActive);
  if (!fVolume && _toolkit) ((StiMaker *)this)->fVolume = new StiDetectorVolume(*_toolkit, TString(), 0);

  if (fVolume) {
     if (gGeometry) {
        TList *listOfVolume = gGeometry->GetListOfNodes();

        // Remove hall from the list of ROOT nodes to make it free of ROOT control
        listOfVolume->Remove(fVolume);
        listOfVolume->Remove(fVolume);
     }
     // Add "hall" into ".const" area of this maker
     ((StiMaker *)this)->AddConst(fVolume);
     if (Debug()) fVolume->ls(3);
  }
  return fVolume;
}
//________________________________________________________________________________
static Bool_t TrackCompareStatus(const StiTrack *a, const StiTrack *b)
{
  int nA = a->getFitPointCount();
  int nB = b->getFitPointCount();
  if (nA!=nB) return (nA > nB);
  return (a->getChi2()<b->getChi2());
//return (a->getChi2Max()<b->getChi2Max());
}
//_____________________________________________________________________________
int StiMaker::CleanGlobalTracks()
{
  for (int iTk=0; iTk< (int)_trackContainer->size(); iTk++) 
  {
    auto* kTrack = static_cast<StiKalmanTrack*>((*_trackContainer)[iTk]);
    kTrack->reserveHits(0);
  }
  sort(_trackContainer->begin(), _trackContainer->end(),TrackCompareStatus );

  for (int iTk=0; iTk<(int) _trackContainer->size(); iTk++) 
  {
    auto* kTrack = (StiKalmanTrack*)((*_trackContainer)[iTk]);
    StiKalmanTrackNode *node = 0;
    int nHits=0,nNits=0;
    do {//one time loop

      StiKalmanTrackNode *dcaNode=0;
      for (auto nodeIt =kTrack->begin();nodeIt!=kTrack->end();nodeIt++) 
      {
	node = &(*nodeIt);
        if (node->isDca()) 		{dcaNode = node;  continue;}
	StiHit *hit = node->getHit(); 	if (!hit)         continue;
	if (!node->isValid()) 		{node->setHit(0); continue;}
	if (node->getChi2()>1e2)	{node->setHit(0); continue;}
	if (hit->timesUsed()) 		{nNits++; node->setHit(0);}
	else                  		{ nHits++;}
      }

      if (!nNits) 				continue;
      if (nHits<5) { kTrack->setFlag(-1); 	continue; }
      if (dcaNode) kTrack->removeNode(dcaNode);

  //		Refit track with removed hits
      int ans = kTrack->refit();
      if (ans || kTrack->getFitPointCount()<5) 
	 { kTrack->setFlag(-1); 		continue; }

  //		Make new Dca node
      StiHit dcaHit; dcaHit.makeDca();
      StiTrackNode *extenDca = kTrack->extendToVertex(&dcaHit);
      if (extenDca) kTrack->add(extenDca,kOutsideIn);
    } while(0);

//	Now mark hits as used
    kTrack->reserveHits();
  }
//		Compact track container
  int jTk=0;
  for (int iTk=0; iTk< (int)_trackContainer->size(); iTk++) 
  {
    auto* kTrack = ((*_trackContainer)[iTk]);
    if (kTrack->getFlag()<0) continue;
    if (jTk!=iTk)(*_trackContainer)[jTk]=kTrack;
    jTk++;
  }
  _trackContainer->resize(jTk);
  
  for (int iTk=0; iTk<(int) _trackContainer->size(); iTk++) 
  {
    auto* kTrack = (StiKalmanTrack*)((*_trackContainer)[iTk]);
    StiKalmanTrackNode *node = 0;
    int nHit = 0,nNode = 0;
    for (auto nodeIt =kTrack->begin();nodeIt!=kTrack->end();nodeIt++) 
    {
      node = &(*nodeIt);
      nNode++;
      StiHit *hit = node->getHit(); if (!hit) 		continue;
      nHit++;
      assert(hit->timesUsed());
    }
  assert(nHit || !nNode);
  }
  return _trackContainer->size();
}
//_____________________________________________________________________________
void StiMaker::FinishTracks (int gloPri) 
{
// Added new method FonishTracks(int gloPri) 0=global 1=primary tracks
// In this method:
// 1. loop over nodes
// 2. Move node to the center volume along x or r  local

// static const char * tkNames[2] = {"globalTracks","primaryTracks"};
// static const char * noNames[2] = {"globalNodes" ,"primaryNodes" };
// static const char * inNames[2] = {"globalInside","primaryInside"};
// static const char * hiNames[2] = {"globaHits"   ,"primaryHits"  };
// static const char * elNames[2] = {"globaELoss"  ,"primaryELoss" };


 StiTrackContainer* tkV  = StiToolkit::instance()->getTrackContainer();
 if (!tkV) return;

 int nTk=0,nNodes=0,nInside=0,nHits=0;
 
   for (int itk=0; itk<(int)tkV->size(); itk++)
   {
     StiKalmanTrack *track = (StiKalmanTrack*)(*tkV)[itk];
     if (gloPri && !track->isPrimary()) continue;
     nTk++;
     StiKTNIterator tNode = track->begin();
     StiKTNIterator eNode = track->end();
     nNodes=0;nInside=0;nHits=0;
     for (;tNode!=eNode;++tNode) 
     {
	StiKalmanTrackNode *node = &(*tNode);
	if(!node->isValid()) 	continue;
	if (node->isDca()  ) 	continue;	
	StiHit *hit = node->getHit();
	if (hit && !hit->detector()) continue;	//primary vertex
	nNodes++;
	if ( hit && node->getChi2()<100) nHits++;
	node->nudge();
	if (1 /*node->inside()*/) {
	  nInside++;
//          StiDebug::Count(elNames[gloPri],node->getELoss()[0].mELoss);
        }
     }
     int qa,idt = track->idTruth(&qa);if(idt){};
//      StiDebug::Count(noNames[gloPri],nNodes );
//      StiDebug::Count(inNames[gloPri],nInside);
//      StiDebug::Count(hiNames[gloPri],nHits  );
  }
//  StiDebug::Count(tkNames[gloPri],nTk );
}


// $Id: StiMaker.cxx,v 1.242 2019/06/20 16:04:55 genevb Exp $
// $Log: StiMaker.cxx,v $
// Revision 1.242  2019/06/20 16:04:55  genevb
// Update from B4SL18h branch
//
// Revision 1.241.4.1  2019/02/27 21:32:53  genevb
// Avoid unnecessary re-initializations in InitRun()
//
// Revision 1.241  2018/07/06 22:13:05  smirnovd
// [Cosmetic] Changes in white space
//
// Revision 1.240  2018/06/29 21:46:33  smirnovd
// Revert iTPC-related changes committed on 2018-06-20 through 2018-06-28
//
// Revert "NoDead option added"
// Revert "Fill mag field more carefully"
// Revert "Assert commented out"
// Revert "Merging with TPC group code"
// Revert "Remove too strong assert"
// Revert "Restore removed by mistake line"
// Revert "Remove not used anymore file"
// Revert "iTPCheckIn"
//
// Revision 1.238  2018/04/19 15:48:28  smirnovd
// [Cosmetic] Remove unused include statements
//
// Revision 1.237  2018/04/12 18:43:59  smirnovd
// Add new option to deactivate iTpc hits
//
// Revision 1.236  2018/04/10 11:32:10  smirnovd
// Minor corrections across multiple files
//
// - Remove ClassImp macro
// - Change white space
// - Correct windows newlines to unix
// - Remove unused debugging
// - Correct StTpcRTSHitMaker header guard
// - Remove unused preprocessor directives in StiCA
// - Minor changes in status and debug print out
// - Remove using std namespace from StiKalmanTrackFinder
// - Remove includes for unused headers
//
// Revision 1.235  2018/01/03 21:24:21  smirnovd
// Remove unused std::string
//
// Revision 1.234  2017/12/19 20:14:12  jwebb
// If no input file, provide default name
//
// Revision 1.233  2017/01/26 21:17:48  perev
// In method CleanGlobalTracks
// 1. for track with reused hits old Dca node removed(not only marked)
// 2. After Refit, new Dca node created
//
// In method FinishTracks some histo created (if debug>2)
//
// Revision 1.232  2016/11/07 20:51:43  perev
// CleanGlobalTracks() added. This method provides cleanen reused hits in the style of CA.
// Thic call is triggered by nMaxTimes attribute, which allows reuse hits nMaxTimes times.
//
// Revision 1.231  2016/06/30 18:49:41  perev
// 1. include StiCADefaultToolkit.h removed. No such file anymore
// 2. local function CountHits() added for print only
// 3. added seed finders SeedFinderKNN,SeedFinderCA
// 4. Flag StiCA means CA seed finder and after standard Sti one
//
// Revision 1.227.4.6  2016/06/29 20:10:11  perev
// CleanGlobalTracks added
//
// Revision 1.227.4.5  2016/06/03 17:00:49  smirnovd
// Sti and StiCA refactoring
//
// Revision 1.227.4.4  2016/06/03 16:07:15  smirnovd
// Sync with MAIN branch as of 2016-05-31
//
// Revision 1.229  2016/03/28 00:15:53  perev
// Add max number of tracks assigned to one hit
//
// Revision 1.228  2016/02/25 23:05:31  genevb
// kSsdId => kSstId
//
// Revision 1.227  2015/07/07 23:31:52  perev
// Clean mess of reset and clear methods
//
// Revision 1.226  2015/02/27 03:51:06  perev
// remove not used BTof
//
// Revision 1.225  2015/01/29 19:04:43  perev
// Comments about treeSearch On/Off added
//
// Revision 1.224  2015/01/29 17:08:47  perev
// Remove redundant inputFile parameter
//
// Revision 1.223  2015/01/15 19:04:41  perev
// Debug--
//
// Revision 1.222  2014/12/16 17:21:14  perev
// temporary supress inside()
//
// Revision 1.221  2014/12/16 01:14:40  perev
// Added new method fonishTracks(int gloPri) 0=global 1=primary tracks
// In this method:
// 1. loop over nodes
// 2. Move node to the center volume along x or r  local
// 3. If StiDebug::mgGlobal >1 create a set of technical histogramms
//
// Revision 1.220  2014/10/30 15:03:55  jeromel
// Reverted to Oct 2nd
//
// Revision 1.217  2014/09/10 15:52:12  perev
// Fix typo, StiSsdDetectorGroup ==> StiSsdDetectorGroup
//
// Revision 1.216  2014/08/22 15:56:30  perev
// Remove never used input file in StiXXXDetectorGroup
//
// Revision 1.215  2014/08/08 17:07:50  jeromel
// oops - previous commits included Jonathan's changes (recalled the previous commit by mistake)
//
// Revision 1.214  2014/08/08 17:06:19  jeromel
// No change - moved history at the bootom as getting very long
//
// Revision 1.213  2014/08/08 16:41:23  jeromel
// No change - moved history at the bootom as getting very long
//
// Revision 1.212  2014/04/24 18:15:02  perev
// Remove RnD as old approach
//
// Revision 1.211  2014/04/22 21:43:13  jeromel
// Grompf! Inverted ssd <-> sst now corrected (thx DS)
//
// Revision 1.210  2014/04/10 23:39:23  jeromel
// Backstep: remove default useXX as clash with older data (we need to reshape the options)
//
// Revision 1.209  2014/04/10 15:48:28  jeromel
// Oops
//
// Revision 1.208  2014/04/10 15:28:55  jeromel
// Merged my previous differences commited by Victor with Dmitri's (Victor OK)
//
// Revision 1.207  2014/04/09 23:52:20  perev
// Ssd+Sst+Pxl
//
// Revision 1.206  2013/04/10 22:14:20  fisyak
// Roll back to version 04/04/2013
//
// Revision 1.204  2012/06/13 20:21:09  fisyak
// Check that vertex fitter exist before getting vertex postion
//
// Revision 1.203  2012/06/11 15:33:41  fisyak
// std namespace
//
// Revision 1.202  2012/05/07 14:56:14  fisyak
// Add StKFVertexMaker
//
// Revision 1.201  2012/02/25 01:48:55  perev
// Limit on permutation installed
//
// Revision 1.200  2011/10/17 12:16:10  fisyak
// Comment out request  DoAlignment for Track Finder
//
// Revision 1.199  2011/10/17 00:14:34  fisyak
// Move handles for IdTruth to StEvent
//
// Revision 1.198  2011/04/05 22:49:16  fisyak
// Add safety marging
//
// Revision 1.197  2011/04/05 22:26:30  fisyak
// Remove alloc/free
//
// Revision 1.196  2011/04/04 19:13:41  fisyak
// Move intialization of detectors in InitRun
//
// Revision 1.195  2011/04/04 15:18:52  fisyak
// Add check that the corresponding Db maker has been instantiated before adding the detector
//
// Revision 1.194  2011/03/31 22:11:24  fisyak
// Propagate IdTruth to StEvent
//
// Revision 1.193  2010/09/01 21:25:03  fisyak
// Add comment
//
// Revision 1.192  2010/01/27 21:43:49  perev
// Add _nPrimTracks for case of fiterr
//
// Revision 1.191  2009/10/18 22:47:29  perev
// assert instead of skip
//
// Revision 1.190  2009/03/16 13:50:14  fisyak
// Move out all Sti Chairs into StDetectorDb
//
// Revision 1.189  2009/01/26 22:00:47  fisyak
// rename TMemStat => StMemStat
//
// Revision 1.188  2008/06/11 22:04:38  fisyak
// Add dead material
//
// Revision 1.187  2008/04/08 14:22:15  fisyak
// remove redundant includes
//
// Revision 1.186  2008/04/03 20:04:05  fisyak
// Straighten out DB access via chairs
//
// Revision 1.185  2008/03/25 18:03:11  perev
// remove field field from everythere
//
// Revision 1.184  2008/03/20 02:01:36  perev
// setMinPrecHits(..) obsolete
//
// Revision 1.183  2008/02/07 18:26:17  perev
// Remove setMCS() call
//
// Revision 1.182  2008/02/07 02:27:40  perev
// Add minPrecHits
//
// Revision 1.181  2007/10/17 15:32:34  fisyak
// rename Hft => Pxl
//
// Revision 1.180  2007/09/22 03:29:16  perev
// Timer + Pulls without vertex
//
// Revision 1.179  2007/09/10 00:32:30  perev
// Attribute useTreeSearch added
//
// Revision 1.178  2007/04/30 19:53:47  fisyak
// add time of flight corrrection for Laser
//
// Revision 1.177  2007/04/28 17:56:19  perev
// Redundant StChain.h removed
//
// Revision 1.176  2007/04/26 04:23:54  perev
// Remove StBFChain dependency
//
// Revision 1.175  2007/04/17 05:11:45  perev
// GetTFile()==>StMaker. Jerome request
//
// Revision 1.174  2007/03/21 17:51:17  fisyak
// add option for EastOff and WestOff, FindDataSet for Sti Geometry
//
// Revision 1.173  2006/12/18 01:29:00  perev
// +noTreeSearch flag & pulls
//
// Revision 1.172  2006/10/16 20:30:42  fisyak
// Clean dependencies from Sti useless classes
//
// Revision 1.171  2006/10/15 05:10:10  fisyak
// Add Hpd
//
// Revision 1.170  2006/10/09 15:51:28  fisyak
// Remove Ftpc
//
// Revision 1.169  2006/08/01 03:51:00  perev
// Return from Make() for too many hits
//
// Revision 1.168  2006/06/16 21:27:52  perev
// Minimal errors of vertex 1 micron
//
// Revision 1.167  2006/05/31 03:59:04  fisyak
// Add Victor's dca track parameters, clean up
//
// Revision 1.166  2006/04/14 22:51:26  perev
// Option useFakeVertex added
//
// Revision 1.161  2006/02/14 18:53:58  perev
// Sub makerFunctionality added.
//
// Revision 1.160  2006/02/08 20:56:39  fisyak
// use kHftId and kIstId for StiDetector groups instead of hadr coded numbers 9999 and 9998
//
// Revision 1.159  2006/01/19 20:21:52  perev
// Ist added
//
// Revision 1.158  2005/12/31 01:34:02  perev
// Degug histos added
//
// Revision 1.157  2005/12/07 23:55:02  perev
// control is changed using StMaker::SetAttr
//
// Revision 1.156  2005/11/22 23:15:27  fisyak
// Clean up parameters setting
//
// Revision 1.155  2005/10/26 21:54:10  fisyak
// Remove dead classes, gid rid off dependencies from StMcEvent and StiGui
//
// Revision 1.154  2005/10/06 20:38:46  fisyak
// Clean up
//
// Revision 1.153  2005/09/28 21:46:36  fisyak
// Persistent StMcEvent
//
// Revision 1.152  2005/08/09 15:23:18  perev
// Add new factory for Node extention
//
// Revision 1.151  2005/08/04 04:03:19  perev
// Cleanup
//
// Revision 1.150  2005/07/21 01:20:12  perev
// clearmem is default now
//
// Revision 1.149  2005/07/20 17:33:25  perev
// MultiVertex
//
// Revision 1.148  2005/02/25 17:41:01  perev
// Time count added
//
// Revision 1.147  2005/01/25 17:23:48  pruneau
// removed references to html package
//
// Revision 1.146  2005/01/21 03:13:37  pruneau
// turned off StiHistograms
//
// Revision 1.145  2005/01/17 03:56:47  pruneau
// change track container to vector
//
// Revision 1.144  2005/01/17 01:32:13  perev
// parameters protected
//
// Revision 1.143  2004/08/04 21:06:17  pruneau
// Added an "if" statement predicated on m_Mode to clear the memory used by the
// factories at the end of StiMaker::Make().
//
// Revision 1.142  2004/04/15 00:43:22  pruneau
// Added Ssd to the list of possible detectors...
//
// Revision 1.141  2004/03/26 15:30:06  andrewar
// bug in field reset
//
// Revision 1.140  2004/03/26 14:52:43  calderon
// Print out the magnetic field read from StEvent::eventSummary()
//
// Revision 1.139  2004/03/25 22:42:44  andrewar
// temp mag field fix; cache filed value and reset if it goes to zero. This
// protects against corrupt event headers...
//
// Revision 1.138  2004/02/24 01:59:46  jeromel
// Commented out include of disappeared .h
//
// Revision 1.137  2004/02/21 18:28:31  pruneau
// Updates to comply to changes in interfaces
//
// Revision 1.136  2004/02/19 22:18:07  pruneau
// Modified call to StMcEventMaker structure
//
// Revision 1.135  2004/02/13 17:36:24  andrewar
// Changed name of StMcEventMaker to StMcEvent... this allows me to run
// simulation. It doesn't seem like this follows the Maker name scheme, though...
//
// Revision 1.134  2004/02/03 18:10:10  pruneau
// Changed name of StMcEventMaker to McEvent in GetMaker call
//
// Revision 1.133  2004/01/30 21:47:23  pruneau
// Changed organization so detector geometris are loaded and build in InitRun
// rather than Make.
// Added accesses to db
//
// Revision 1.132  2003/10/28 16:01:15  andrewar
// Passing tracking parameter file to detector Builders.
//
// Revision 1.131  2003/09/02 17:59:59  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.130  2003/08/05 18:20:33  andrewar
// Changed default parameters to apply eta filters.
//
// Revision 1.129  2003/07/30 20:12:31  pruneau
// Added new histo group
//
// Revision 1.128  2003/06/10 18:47:28  andrewar
// Changed StiResiduaCalc calls to conform to modified class.
//
// Revision 1.127  2003/05/07 03:06:34  pruneau
// *** empty log message ***
//
// Revision 1.126  2003/05/06 16:48:10  mmiller
// Incorporated StiPixel.  usePixel==false by default.
//
// Revision 1.125  2003/05/06 15:36:36  mmiller
// Committing changes to turn on multiple regions (StiPlacement::StiRegion -> kMidRapidity, kForwardRapidity, etc).
// Also added a point to StiToolkit for StiMaker.  This allows for the req. GetDataSet calls in the FTPC code.
// Not so elegant...
//
// Revision 1.124  2003/04/30 15:39:33  pruneau
// Integrating StiResidual in main stream Sti
//
// Revision 1.123  2003/04/29 18:48:50  pruneau
// *** empty log message ***
//
// Revision 1.122  2003/04/13 02:16:13  pruneau
// *** empty log message ***
//
// Revision 1.121  2003/04/11 18:56:14  pruneau
// Pulling the B field from StEventSummary
//
// Revision 1.120  2003/04/11 16:51:57  pruneau
// various fixes
//
// Revision 1.119  2003/04/10 14:53:06  pruneau
// removing obsolete files and classes
//
// Revision 1.118  2003/04/10 12:10:09  pruneau
// Changed StiMaker and Default Toolkit to accomodate the new Event Display
//
// Revision 1.117  2003/03/31 17:19:27  pruneau
// various
//
// Revision 1.116  2003/03/17 17:44:49  pruneau
// *** empty log message ***
//
// Revision 1.115  2003/03/13 18:59:42  pruneau
// various updates
//
// Revision 1.114  2003/03/13 16:30:59  andrewar
// Added plotting package
//
// Revision 1.113  2003/03/13 15:15:51  pruneau
// various
//
// Revision 1.112  2003/03/12 17:58:04  pruneau
// fixing stuff
//
// Revision 1.111  2003/02/25 14:21:06  pruneau
// *** empty log message ***
//
// Revision 1.110  2003/01/24 06:12:28  pruneau
// removing centralized io
//
// Revision 1.109  2003/01/22 20:06:26  andrewar
// Changed includes to point to new libraries (StiTpc, StiSvt, etc)
//
// Revision 1.108  2002/12/19 19:29:42  pruneau
// *** empty log message ***
//
// Revision 1.106  2002/10/04 01:54:48  pruneau
// DefaultToolkit now uses the StiHitLoader scheme rahter than the StiHitFiller.
//
// Revision 1.105  2002/09/27 19:19:01  mmiller
// Changed program flow to once again allow for track by track gui.
//
// Revision 1.104  2002/09/10 18:42:40  pruneau
// Fixed bug in the call sequence of the association maker
// introduced in the previous release.
//
// Revision 1.103  2002/09/05 21:27:10  pruneau
// Fixed problem with StiRootSimpleTrackFilter::makeNewObject
//
// Revision 1.102  2002/09/05 05:47:30  pruneau
// Adding Editable Parameters and dynamic StiOptionFrame
//
// Revision 1.101  2002/08/28 17:14:18  pruneau
// Simplified the interface of StiKalmanTrackFinder and the calls
// required in StiMaker.
//
// Revision 1.100  2002/08/23 18:16:50  pruneau
// Added StiSimpleTrackFilter to StiMaker to enable simple and
// fast track finding diagnostics.
//
// Revision 1.99  2002/08/19 19:32:59  pruneau
// eliminated cout when unnecessary, made helix member of the EventFiller
//
// Revision 1.98  2002/06/26 23:05:31  pruneau
// changed macro
//
// Revision 1.97  2002/06/18 18:08:34  pruneau
// some cout statements removed/added
//
// Revision 1.96  2002/06/04 19:45:31  pruneau
// including changes for inside out tracking
//

//_____________________________________________________________________________
void CountHits()
{
 int nTimesUsed=0;
 StiTrackContainer* tkV  = StiToolkit::instance()->getTrackContainer();
 if (!tkV) return;
   for (int itk=0; itk<(int)tkV->size(); itk++)
   {
     StiKalmanTrack *track = (StiKalmanTrack*)(*tkV)[itk];
     StiKalmanTrackNode *node;
     StiKTNIterator it = track->begin();
     for (;(node=it());it++){
       const StiHit *hit = node->getHit();
       if (!hit) 		continue;
       if (!hit->detector()) 	continue;
       assert(node->getChi2()<1000.);
       nTimesUsed++;
   } }
}
