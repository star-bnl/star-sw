// $Id: StiVMCMaker.cxx,v 2.6 2011/02/11 15:56:38 fisyak Exp $
/// \File StiVMCMaker.cxx
/// \author M.L. Miller 5/00
/// \author C Pruneau 3/02
/*!
  
\class StiVMCMaker 

\author M.L. Miller 5/00
\author C Pruneau 3/02
\author V Perev 2005

A maker StiVMCMaker is a steering maker for Sti package.
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
SetAttr("activeTpc"  ,1) 	// default
SetAttr("activeSvt"  ,0) 	// default
SetAttr("activeSsd"  ,0)	// default Off
SetAttr("activePixel",0)	// default Off
SetAttr("activeIst"  ,0)	// default Off
SetAttr("activeHpd"  ,0)	// default Off

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
#include "StiVMC/StiKalmanTrackContainer.h"
#include "StiVMC/StiKalmanTrackNode.h"
#include "StiVMC/StiKalmanTrack.h"
#include "StiVMC/StiVertexFinder.h"
#include "StiVMC/StiDetectorContainer.h"
#include "StiStEventFiller.h"
#include "StiDefaultToolkit.h"
#include "StiVMCMaker.h"
#include "StiVMC/StiHitTest.h"
#include "StiVMC/StiHitContainer.h"
#include "TFile.h"
#include "StDetectorDbMaker/StiKalmanTrackFinderParameters.h"
#include "StDetectorDbMaker/StiKalmanTrackFitterParameters.h"
#include "StDetectorDbMaker/St_tpcRDOMasksC.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
#include "StDetectorDbMaker/StiIst1HitErrorCalculator.h"
#include "StDetectorDbMaker/StiPixelHitErrorCalculator.h"
#include "StDetectorDbMaker/StiSsdHitErrorCalculator.h"
#include "StDetectorDbMaker/StiSvtHitErrorCalculator.h"
#include "StDetectorDbMaker/StiTpcInnerHitErrorCalculator.h"
#include "StDetectorDbMaker/StiTpcOuterHitErrorCalculator.h"
#include "StDetectorDbMaker/StiDefaultTrackingParameters.h"
#include "StDetectorDbMaker/StiPixelTrackingParameters.h"
#include "StDetectorDbMaker/StiSsdTrackingParameters.h"
#include "StDetectorDbMaker/StiSvtTrackingParameters.h"
#include "StDetectorDbMaker/StiTpcTrackingParameters.h"
#include "StTpcDb/StTpcDb.h"
#include "TDataSet.h"
#include "TDataSetIter.h"
#include "TGeoManager.h"
#include "StiVMC/StiTimer.h"
#include "StarMagField.h"
#include "StarVMCDetectorSet.h"
#include "StarVMCDetector.h"
#include "StarDetectorMap.h"
#include "StGeanePropagator.h"
/// Definion of minimal primary vertex errors.
/// Typical case,vertex got from simulations with zero errors.
/// But zero errors could to unpredicted problems
/// Now minimal possible error is 1 micron
static const float MIN_VTX_ERR2 = 1e-4*1e-4;
enum { kHitTimg,kGloTimg,kVtxTimg,kPriTimg,kFilTimg};
ClassImp(StiVMCMaker);
//_____________________________________________________________________________
StiVMCMaker::StiVMCMaker(const Char_t *name) : 
    StMaker(name),
    _Initialized(false),
    _toolkit(0),
    _eventFiller(0),
    fStiDetector(0),
    fVMCDetector(0),
    fDetectorContainer(0),
    fPath(),
    fId(kUnknownId)
{
  memset(mTimg,0,sizeof(mTimg));
  cout <<"StiVMCMaker::StiVMCMaker() -I- Starting"<<endl;
  memset (fIndx, 0, sizeof(fIndx));
  if (!StiToolkit::instance()) new StiDefaultToolkit;
  _toolkit = StiToolkit::instance();
  SetAttr("activeTpc"		,kTRUE);
  SetAttr("activeSvt"		,kTRUE);
  SetAttr("activeSsd"		,kTRUE);
  //SetAttr("useAux"		,kTRUE); // Auxiliary info added to output for evaluation
  SetAttr("useEventFiller"      ,kTRUE);
  SetAttr("useVertexFinder"     ,kTRUE);
  
  if (strstr(gSystem->Getenv("STAR"),".DEV"))
    SetAttr("useAux",kTRUE); // Auxiliary info added to output for evaluation
}
//_____________________________________________________________________________
void StiVMCMaker::Clear(const char*) {
  StMaker::Clear();
}

//_____________________________________________________________________________
Int_t StiVMCMaker::Finish() {
  StiTimer::Print();
  StiTimer::Clear();
  
  static const char *timg[] = {"HitLoa","GlobFnd","VtxFnd","PriFnd","FilFnd",0};
  if (mTimg[0]) {
    for (Int_t i=0;timg[i];i++) {
      Info("Timing","%s(%d) \tCpuTime = %6.2f seconds,\tPerEvent = %g seconds"
	   ,timg[i],mTimg[i]->Counter(),mTimg[i]->CpuTime()
	   ,mTimg[i]->CpuTime()/mTimg[i]->Counter());    
    } 
  }
  
  return StMaker::Finish();
}
//_____________________________________________________________________________
Int_t StiVMCMaker::Init() {
  StiTimer::Init("StiKalmanTrackFinder::find() TIMING"
		 ,StiTimer::fgFindTimer,StiTimer::fgFindTally);
  if (*SAttr("maxRefiter")) StiKalmanTrack::SetMaxRefiter(IAttr("maxRefiter"));
  if (IAttr("useTiming")) {
    for (Int_t it=0;it<(int)(sizeof(mTimg)/sizeof(mTimg[0]));it++){
      mTimg[it]= new TStopwatch(); mTimg[it]->Stop();
    } 
  }
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StiVMCMaker::InitDetectors() {
  if (! gGeoManager) GetDataBase("VmcGeometry");
  StGeanePropagator *propagator = StGeanePropagator::instance();
  assert(propagator);
  if (Debug()) propagator->SetDebug(Debug());
  Int_t ikine = 5;
  propagator->SetParticle(ikine);
  fDetectorContainer = _toolkit->DetectorContainer();
  fDetectorContainer->Initialize();
  THashList *dSet = StarVMCDetectorSet::instance()->GetDetectorHash();
  TIter next(dSet);
  StarVMCDetector *fVMCDetector = 0;
  // Make list of active detectors
  Int_t minSector = 1;
  Int_t maxSector = 24;
  Int_t minRow    = 1;
  while ((fVMCDetector = (StarVMCDetector *) next())) {
    TString detName(fVMCDetector->GetName());
    if (detName.BeginsWith(".")) continue;
    StDetectorId kId = kUnknownId;
    for (Int_t i = 0; i < NoDetectors; i++) {
      if (detName == Detectors[i].det) {
	kId = Detectors[i].Id;
	break;
      }
    }
    if (kId == kUnknownId) {
      cout << detName.Data() << " is not registered and ignored because of this" << endl;
      continue;
    }
    if (! (kId ==  kSvtId || kId == kSsdId || kId == kPxlId || kId == kIstId || kId == kTpcId)) {
      cout << detName.Data() << " is not coded yet and ignored because of this" << endl;
      continue;
    }
    StiTrackingParameters *trackingParameters = (StiTrackingParameters *) StiDefaultTrackingParameters::instance();
    // Selection of active detector elements
    if (kId == kSvtId && ! IAttr("activeSvt")   ) continue;
    if (kId == kSsdId && ! IAttr("activeSsd")   ) continue;
    if (kId == kPxlId && ! IAttr("activePixel") ) continue;
    if (kId == kIstId && ! IAttr("activeIst")   ) continue;
    if (kId == kTpcId) {
      if (               ! IAttr("activeTpc")   ) continue;
      if (IAttr("laserIT")) {
	StiKalmanTrackNode::SetLaser(1);
	cout << "StiVMCMaker::InitDetectors() -I- " << detName.Data() 
	     << " set laser time of flight correction" << endl;
      }
      if (IAttr("WestOff")) minSector = 13;
      if (IAttr("EastOff")) maxSector = 12;
      if ((IAttr("activeSvt")  || 
	   IAttr("activeSsd")) && IAttr("skip1row")) minRow = 2;
      cout << "StiVMCMaker::InitDetectors() -I- " << detName.Data() 
	   << "\tuse sectors in range[" << minSector << "," << maxSector
	   << "] with  rows >= " << minRow << endl;
    }
    cout<<"StiVMCMaker::InitDetectors() -I- Adding detector group:" << detName <<endl;
    propagator->SetSensVolume(detName);
    Int_t Ntot = 1; // Total no. of detectors in detector Set
    for (Int_t k = 0; k < fVMCDetector->GetNVL(); k++) Ntot *= fVMCDetector->GetNVmax()[k];
    for (Int_t j = 0; j < Ntot; j++) {// loop over detector elements
      Int_t ind = j;
      for (Int_t k =  fVMCDetector->GetNVL() - 1; k >= 0; k--) {
	fIndx[k] = ind%fVMCDetector->GetNVmax()[k]+1; ind /= fVMCDetector->GetNVmax()[k];
      }
      Int_t keyR   = 0;
      Int_t keyA   = 0;
      Int_t part   = 0;
      Int_t sector = 1;
      Int_t row    = 1;
      StiHitErrorCalculator *hitErrorCalculator = 0;
      Int_t volumeID = -1;
      switch (kId) {
      case kTpcId:
	part = fIndx[0];
	sector = 12*(part - 1) + fIndx[1];
	row    = fIndx[2];
	keyA = sector;
	if (sector < minSector || sector > maxSector) continue;
	if (row    < minRow) continue;
	if (! St_tpcRDOMasksC::instance()->isRowOn(sector,row)) continue;
	if (row<13) hitErrorCalculator = StiTpcInnerHitErrorCalculator::instance();
	else        hitErrorCalculator = StiTpcOuterHitErrorCalculator::instance();
	trackingParameters = (StiTrackingParameters *) StiTpcTrackingParameters::instance();
	volumeID = 100*sector + row;
	break;
      case kSvtId:
	sector = fIndx[2];
	row    = fIndx[1];
	part   = fIndx[3];
	keyA   = fIndx[3] + 7*fIndx[2]; // wafer + 7*ladder
	hitErrorCalculator = StiSvtHitErrorCalculator::instance();	break;
	trackingParameters = (StiTrackingParameters *) StiSvtTrackingParameters::instance();
      case kSsdId:
	sector = fIndx[1];
	part   = fIndx[2];
	keyA   = fIndx[2] + 16*fIndx[1]; // wafer + 16*ladder
	hitErrorCalculator = StiSsdHitErrorCalculator::instance();      break;
	trackingParameters = (StiTrackingParameters *) StiSsdTrackingParameters::instance();
      case kPxlId:
	hitErrorCalculator = StiPixelHitErrorCalculator::instance();    break;
	trackingParameters = (StiTrackingParameters *) StiPixelTrackingParameters::instance();
      case kIstId:
	hitErrorCalculator = StiIst1HitErrorCalculator::instance();     break;
      default:                                                   	break;
      }
      if (volumeID > 0) {
	fPath = fVMCDetector->FormPath(volumeID);
      } else {
	fPath = StarVMCDetector::FormPath(fVMCDetector->GetFMT().Data(),fVMCDetector->GetNVL(),fIndx);
      }
      if (! gGeoManager->CheckPath(fPath)) continue;
      fStiDetector = (StiDetector *) fDetectorContainer->FindDetector(fPath);
      if (! fStiDetector) {
	TGeoPhysicalNode *nodeP = (TGeoPhysicalNode *) gGeoManager->GetListOfPhysicalNodes()->FindObject(fPath);
	if (! nodeP) nodeP = gGeoManager->MakePhysicalNode(fPath);
	if (! nodeP) {
	  cout << "Can't get a Physical Node from path " << fPath << endl;
	  continue;
	}
	fStiDetector = _toolkit->DetectorFactory()->getInstance();
	fStiDetector->SetGroupId(kId);
	fStiDetector->SetName(nodeP->GetName());
	fStiDetector->SetPhysicalNode(nodeP);
	fStiDetector->SetIsActive(new StiIsActiveFunctor());
	fStiDetector->SetHitErrorCalculator(hitErrorCalculator);
	fStiDetector->SetKey(0,part);
	keyR  = 2*TMath::Nint(fStiDetector->NormalRadius()/2);
	fStiDetector->SetKey(1,keyR);
	fStiDetector->SetKey(2,keyA);
	fStiDetector->SetTrackingParameters(trackingParameters);
	fDetectorContainer->Add(fStiDetector); if (Debug() > 1) cout << *fStiDetector << endl;
      }
    } // detector element loop
  } // detector Set loop
  assert(fDetectorContainer->DetectorsHash()->GetSize());
  fDetectorContainer->Build();
  fDetectorContainer->Reset();
  return kStOk;
}

//_____________________________________________________________________________
Int_t StiVMCMaker::InitRun(Int_t run)
{
  if (!_Initialized)    {
    cout <<"StiVMCMaker::InitRun() -I- Initialization Segment Started"<<endl;
    InitDetectors();
    // Load Detector related parameters
    _toolkit->TrackSeedFinder()->Initialize();
    SafeDelete(_eventFiller);
    if (IAttr("useEventFiller")) {
      _eventFiller =  new StiStEventFiller();
    }
    _Initialized=kTRUE;
    cout <<"StiVMCMaker::InitRun() -I- Initialization Segment Completed"<<endl;
  }
  return StMaker::InitRun(run);
}
//_____________________________________________________________________________
void StiVMCMaker::loadTpcHits(StEvent *pEvent) {
  const StTpcHitCollection* tpcHits = pEvent->tpcHitCollection();
  if (!tpcHits) return;
  if (! tpcHits->numberOfHits()) return;
  StiHit* stiHit;
  UInt_t sec;
  UInt_t noHitsLoaded = 0;
  for (UInt_t sector = 0;  sector < tpcHits->numberOfSectors(); sector++) {
    sec = sector + 1;
    const StTpcSectorHitCollection* secHits = tpcHits->sector(sector);
    if (!secHits) {
      cout << "StiVMCMaker::loadTpcHits -W- no hits for sector:"<<sector<<endl;
      break;
    }
    Float_t driftvel = 1e-6*gStTpcDb->DriftVelocity(sec); // cm/mkmsec
    for (UInt_t padrow = 0; padrow < secHits->numberOfPadrows(); padrow++) {
      //cout << "StiTpcHitLoader:loadHits() -I- Loading row:"<<row<<" sector:"<<sector<<endl;
      const StTpcPadrowHitCollection* padrowHits = secHits->padrow(padrow);
      if (!padrowHits) break;
      const StSPtrVecTpcHit& hitvec = padrowHits->hits();
      if (! hitvec.size()) continue;
      const_StTpcHitIterator iter;
      StiHitTest hitTest;
      for (iter = hitvec.begin();iter != hitvec.end();++iter)        {
        const StTpcHit *hit= (StTpcHit*) *iter;
	Int_t volumeID = hit->volumeID();
	fPath = fVMCDetector->FormPath(volumeID);
	fStiDetector = (StiDetector *) fDetectorContainer->FindDetector(fPath);
	if (! fStiDetector) continue;
        stiHit = _toolkit->HitFactory()->getInstance();
        assert(stiHit);
        stiHit->Reset();
        stiHit->setGlobal(fStiDetector,hit,hit->position().x(),hit->position().y(), hit->position().z());
        hitTest.Add(hit->position().x(),hit->position().y(), hit->position().z());
#if 0	
	if (hit->sector() <= 12) stiHit->setVz( driftvel);
	else                     stiHit->setVz(-driftvel);
#else
	stiHit->setVz( driftvel);
#endif
        _toolkit->HitContainer()->Add( stiHit );
	noHitsLoaded++;
	if (Debug()) {
	  cout << "add hit S/R =" << sector << "/" << padrow << " to detector " << *fStiDetector << endl;
	}
      }
      if (hitTest.width()>0.1) {
	printf("**** TPC hits too wide (%g) sector=%d row%d\n"
	       ,hitTest.width(),sector,padrow);
      }
    }
  }
  cout << "StiVMCMaker::loadTpcHits -I- Done with " << noHitsLoaded << " hits" <<  endl;
}
//_____________________________________________________________________________
void StiVMCMaker::loadSsdHits(StEvent *pEvent) {
  const StSsdHitCollection* ssdhits = pEvent->ssdHitCollection();
  if (!ssdhits) return;
  StiHit* stiHit;
  cout <<"StiVMCMaker::loadSsdHits - Started"<<endl;
  Int_t compt = 0;
  StSsdHit* hit;
  UInt_t noHitsLoaded = 0;
  for (UInt_t ladder = 0; ladder< ssdhits->numberOfLadders(); ++ladder) {
    const StSsdLadderHitCollection* ladderhits = ssdhits->ladder(ladder);
    fIndx[0] = StSsdHit::sector(ladder);
    fIndx[1] = ladder + 1;
    if (!ladderhits) break;
    for (UInt_t wafer = 0; wafer< ladderhits->numberOfWafers(); ++wafer) {
      const StSsdWaferHitCollection* waferhits = ladderhits->wafer(wafer);
      if (!waferhits) break;
      fIndx[2] = wafer + 1;
      fPath = StarVMCDetector::FormPath(fVMCDetector->GetFMT().Data(),fVMCDetector->GetNVL(),fIndx);
      if (! gGeoManager->CheckPath(fPath)) continue;
      fStiDetector = (StiDetector *) fDetectorContainer->FindDetector(fPath);
      const StSPtrVecSsdHit& hits = waferhits->hits(); 
      for (const_StSsdHitIterator it=hits.begin(); it!=hits.end(); ++it) {
	if (!*it) continue;
	hit = static_cast<StSsdHit*>(*it);
	if (!hit) continue;
	if (hit && fStiDetector) {
	  compt++;
	  stiHit = _toolkit->HitFactory()->getInstance();
	  stiHit->setGlobal(fStiDetector,hit,
			    hit->position().x(),
			    hit->position().y(),
			    hit->position().z());
	  _toolkit->HitContainer()->Add( stiHit );
	  noHitsLoaded++;
	}
      }
    }
  }
  cout <<"StiVMCMaker::loadSsdHits - I - Done <====> Number of SSD Hits loaded = " << noHitsLoaded <<endl; 
}
//_____________________________________________________________________________
void StiVMCMaker::loadSvtHits(StEvent *pEvent) {
  const StSvtHitCollection* svthits = pEvent->svtHitCollection();
  if (!svthits) return;
  StiHit* stiHit;
  cout <<"StiVMCMaker::loadSvtHits - Started"<<endl;
  StSvtHit* hit;
  Int_t hitCounter = 0;
  for (UInt_t barrel=0; barrel<svthits->numberOfBarrels(); ++barrel) {
    const StSvtBarrelHitCollection* barrelhits = svthits->barrel(barrel);
    if (!barrelhits) continue;
    for (UInt_t ladder=0; ladder<barrelhits->numberOfLadders(); ++ladder) {
	const StSvtLadderHitCollection* ladderhits = barrelhits->ladder(ladder);
	if (!ladderhits) continue;
	fIndx[0] = StSvtHit::shell(barrel+1,ladder+1); 
	fIndx[1] = StSvtHit::layer(barrel+1,ladder+1); 
	fIndx[2] = ladder+1;
	fPath = StarVMCDetector::FormPath(fVMCDetector->GetFMT().Data(),fVMCDetector->GetNVL(),fIndx);
	if (! gGeoManager->CheckPath(fPath)) continue;
	fStiDetector = (StiDetector *) fDetectorContainer->FindDetector(fPath);
	assert(fStiDetector);
	for (UInt_t wafer=0; wafer<ladderhits->numberOfWafers(); ++wafer) {
	  const StSvtWaferHitCollection* waferhits = ladderhits->wafer(wafer);
	  if (!waferhits) continue;
	  fIndx[3] = wafer+1;
	  const StSPtrVecSvtHit& hits = waferhits->hits();
	  StiHitTest hitTest;
	  for (const_StSvtHitIterator it=hits.begin(); it!=hits.end(); ++it) {
	    if (!*it) continue;
	    hit = static_cast<StSvtHit*>(*it);
	    if (!hit) continue;
	    if (hit->flag()>=4) continue;
	    if (hit->flag()< 0) continue;
	    stiHit = _toolkit->HitFactory()->getInstance();
	    stiHit->setGlobal(fStiDetector,hit,hit->position().x(),hit->position().y(),hit->position().z());
	    hitTest.Add(stiHit->Detector()->NormalRadius(),stiHit->y(),stiHit->z());
	    _toolkit->HitContainer()->Add( stiHit );
	    hitCounter++;
	  }
	  if (hitTest.getN()< 10) continue;
	  double w=hitTest.width();
	  double dx = fStiDetector->NormalRadius()-hitTest.center()[0];;
	  double ay = hitTest.yAngle()*180/3.1415;
	  double az = hitTest.zAngle()*180/3.1415;
	  
	  if (w< 0.1 && fabs(dx)<1 && fabs(ay)<1 && fabs(az)<1) continue;
	  printf("**** SVT geom problem: barrel=%d ladder%d wafer%d\n",barrel,ladder,wafer);
	  printf("**** SVT dX=%g aY=%g aZ=%g\n\n",dx,ay,az);
	}
      }
  }
  cout <<"StiVMCMaker::loadSvtHits -I- SVT Hits added:"<<hitCounter<<endl;
  cout <<"StiVMCMaker::loadSvtHits -I- Done"<<endl;
}
//_____________________________________________________________________________
void StiVMCMaker::loadIstHits(StEvent *pEvent) {
  LOG_INFO << "StiVMCMaker::loadIstHits -I- Started" << endm;
  StRnDHitCollection *col = pEvent->rndHitCollection();
  if (!col) {
    LOG_INFO <<"StiVMCMaker::loadIstHits\tERROR:\tcol==0"<<endm;
    LOG_INFO <<"You must not have pixelFastSim in your chain"<<endm;
    LOG_INFO <<"will return with no action taken"<<endm;
    return;
  }
  StSPtrVecRnDHit& vec = col->hits();
  
  LOG_DEBUG <<"StiIstHitLoader: RnD Hits: "<<vec.size()<<endm;
  
  for(UInt_t j=0; j<vec.size(); j++) {
    StRnDHit* hit = vec[j];
    assert(hit);
    
    if (hit->detector()!=kIstId) continue;
    //if(hit->extraByte0()==1){
    //                             ladder    module           side    
    //        volume_id = numbv(1)*1000000 + numbv(2)*10000 + numbv(3)*100  + numbv(4)
    //MLM cout <<"retrieve detector"<<endl;
    //    Int_t layer=hit->layer();
    Int_t ladder=hit->ladder();
    Int_t wafer=hit->wafer();
    LOG_DEBUG<<"StiVMCMaker::loadIstHits: hit has ladder: "<<ladder<<"; wafer: "<<wafer<<endm;
    LOG_DEBUG<<"StiVMCMaker::loadIstHits: hit volume id: "<<hit->volumeID()<<endm;
    fIndx[0] = ladder;
    fIndx[1] = wafer;
    fPath = StarVMCDetector::FormPath(fVMCDetector->GetFMT().Data(),fVMCDetector->GetNVL(),fIndx);
    if (! gGeoManager->CheckPath(fPath)) continue;
    fStiDetector = (StiDetector *) fDetectorContainer->FindDetector(fPath);
    assert(fStiDetector);
    cout <<"StiVMCMaker::loadIstHits: add hit to detector:\t"<<fStiDetector->GetName()<<endl;
    
    StiHit * stiHit = _toolkit->HitFactory()->getInstance();
    if(!stiHit) continue;
    stiHit->Reset();
    LOG_DEBUG<<"StiVMCMaker::loadIstHits: hit has position ("
	     <<hit->position().x()<<","<<hit->position().y()<<","<<hit->position().z()<<")"<<endm;
    stiHit->setGlobal(fStiDetector, hit, hit->position().x(),hit->position().y(),hit->position().z());
    _toolkit->HitContainer()->Add( stiHit );
  }
  LOG_INFO << "StiVMCMaker::loadIstHits -I- Done" << endm;
}
//_____________________________________________________________________________
void StiVMCMaker::loadPxlHits(StEvent *pEvent) {
  StRnDHitCollection *col = pEvent->rndHitCollection();
  if (!col) return;
  StSPtrVecRnDHit& vec = col->hits();
  
  Int_t nHit=0;
  for(UInt_t j=0; j<vec.size(); j++)	{
    StRnDHit *pxlH = vec[j];
    if(!pxlH) continue;
    if (pxlH->detector()!=kPxlId) continue;
    
    // Because of a screw up with the layout in the pixlgeo3.g file, the detectors are not 
    // sequential in phi. List (geant,ittf): (1,2),(2,1),(3,0),(4,5),(5,4),(6,3),(7,8),(8,7),(9,6)
    // This works to: 
    //    ittfL = 3*n - geantL
    //    n = 1,3,5 (or 2k+1, k=0,1,2)
    //    k= int[( geantL -1 )/3]
    //Resolve the layer and ladder ids.
    
    //detector= _detector->Detector(pxlH->layer()-1, pxlH->ladder()-1);
    //assert(row<_detectors.size());
    //assert(sector<_detectors[row].size());
    //MLM 
    LOG_DEBUG <<Form("hit layer: %i ladder: %i\n",pxlH->layer(), pxlH->ladder()) << endm;
    /*
      if(pxlH->layer()==1)
      ittfLadder= ( 2* int( (pxlH->ladder()-1.) /3. ) +1)*3 - pxlH->ladder();
      else
      ittfLadder=( 2* int( (pxlH->ladder()-1.) /8. ) +1)*8 - pxlH->ladder();
    */
    //MLM 
    //printf("row<==> pxlH()-1:%i sector<==>ittfLadder: %i\n",pxlH->layer()-1,ittfLadder);
    //detector= _detector->Detector(pxlH->layer(), ittfLadder);
    fIndx[0] = pxlH->layer();
    fIndx[1] = pxlH->ladder();
    fPath = StarVMCDetector::FormPath(fVMCDetector->GetFMT().Data(),fVMCDetector->GetNVL(),fIndx);
    if (! gGeoManager->CheckPath(fPath)) continue;
    fStiDetector = (StiDetector *) fDetectorContainer->FindDetector(fPath);
    assert(fStiDetector);
    LOG_DEBUG <<"add hit to detector:\t"<<fStiDetector->GetName()<<endm;
    StiHit *stiHit=_toolkit->HitFactory()->getInstance();
    if(!stiHit) continue;
    stiHit->Reset();
    stiHit->setGlobal(fStiDetector, pxlH,
		      pxlH->position().x(), pxlH->position().y(),
		      pxlH->position().z());
    _toolkit->HitContainer()->Add(stiHit);
    LOG_DEBUG <<" nHit = "<<nHit<<" Layer = "<<pxlH->layer()<<" Ladder = "<<pxlH->ladder()
	      <<" x = "<<pxlH->position().x()<<" y = "<<pxlH->position().y()<<" z = "<<pxlH->position().z()<<endm;
    //done loop over hits
    nHit++;
  }
  LOG_INFO <<"StiPixelHitLoader:loadHits -I- Loaded "<<nHit<<" pixel hits."<<endm;
}
//_____________________________________________________________________________
void StiVMCMaker::loadTofHits(StEvent *pEvent) {cout << "StiVMCMaker::loadTofHits has not emplemended yet !" << endl;}
//_____________________________________________________________________________
void StiVMCMaker::loadCtbHits(StEvent *pEvent) {cout << "StiVMCMaker::loadCtbHits has not emplemended yet !" << endl;}
//_____________________________________________________________________________
void StiVMCMaker::loadFgtHits(StEvent *pEvent) {cout << "StiVMCMaker::loadFgtHits has not emplemended yet !" << endl;}
//_____________________________________________________________________________
void StiVMCMaker::loadPhmdHits(StEvent *pEvent) {cout << "StiVMCMaker::loadPhmdHits has not emplemended yet !" << endl;}
//_____________________________________________________________________________
void StiVMCMaker::loadZdcHits(StEvent *pEvent) {cout << "StiVMCMaker::loadZdcHits has not emplemended yet !" << endl;}
//_____________________________________________________________________________
void StiVMCMaker::loadFtpcHits(StEvent *pEvent) {cout << "StiVMCMaker::loadFtpcHits has not emplemended yet !" << endl;}
//_____________________________________________________________________________
void StiVMCMaker::loadBarrelEmcTowerHits(StEvent *pEvent) {cout << "StiVMCMaker::loadBarrelEMCTowerHits has not emplemended yet !" << endl;}
//_____________________________________________________________________________
void StiVMCMaker::loadEndcapEmcTowerHits(StEvent *pEvent) {cout << "StiVMCMaker::loadEndcapEmcTowerHits has not emplemended yet !" << endl;}
//_____________________________________________________________________________
void StiVMCMaker::loadBarrelEmcPreShowerHits(StEvent *pEvent) {cout << "StiVMCMaker::loadBarrelEmcPreShowerHits has not emplemended yet !" << endl;}
//_____________________________________________________________________________
void StiVMCMaker::loadEndcapEmcPreShowerHits(StEvent *pEvent) {cout << "StiVMCMaker::loadEndcapEmcPreShowerHits has not emplemended yet !" << endl;}
//_____________________________________________________________________________
void StiVMCMaker::loadHits(StEvent* pEvent) {
  THashList *dSet = StarVMCDetectorSet::instance()->GetDetectorHash();
  TIter next(dSet);
  fVMCDetector = 0;
  while ((fVMCDetector = (StarVMCDetector *) next())) {
    TString detName(fVMCDetector->GetName());
    if (detName.BeginsWith(".")) continue;
    fId = kUnknownId;
    Int_t i = 0;
    for (; i < NoDetectors; i++) {
      if (detName == Detectors[i].det) {
	fId = Detectors[i].Id;
	break;
      }
    }
    if (fId == kUnknownId) {
      cout << detName.Data() << " is not registered and ignored because of this" << endl;
      continue;
    }
    fStiDetector = 0;  
    /*
      StBTofCollection.h
      StTofCollection.h
      StEmcClusterCollection.h
      StEmcCollection.h
      StEtrHitCollection.h
      StFgtCollection.h
      StFgtHitCollection.h
      StFgtPointCollection.h
      StFmsCollection.h
      StFpdCollection.h
      StFtpcHitCollection.h
      StGmtCollection.h
      StGmtHitCollection.h
      StGmtPointCollection.h
      StIstHitCollection.h
      StMtdCollection.h
      StPhmdClusterCollection.h
      StPhmdCollection.h
      StPxlHitCollection.h
      StRichCollection.h
      StRnDHitCollection.h
      StRpsCollection.h
      StSsdHitCollection.h
      StSstHitCollection.h
      StSvtHitCollection.h
      StTpcHitCollection.h
     */
    switch (fId) {
    case kSsdId     	       : loadSsdHits(pEvent);                break;
    case kSvtId                : loadSvtHits(pEvent); 		     break;
    case kTpcId     	       : loadTpcHits(pEvent); 		     break;
    case kTofId     	       : loadTofHits(pEvent); 		     break;
    case kCtbId     	       : loadCtbHits(pEvent); 		     break;
    case kFgtId                : loadFgtHits(pEvent); 		     break;
    case kIstId                : loadIstHits(pEvent); 		     break;
    case kPxlId                : loadPxlHits(pEvent); 		     break;
    case kPhmdId               : loadPhmdHits(pEvent);		     break;
    case kZdcWestId            : loadZdcHits(pEvent); 		     break;
    case kFtpcWestId           : loadFtpcHits(pEvent);		     break;
    case kBarrelEmcTowerId     : loadBarrelEmcTowerHits(pEvent);     break;
    case kEndcapEmcTowerId     : loadEndcapEmcTowerHits(pEvent);     break;
    case kBarrelEmcPreShowerId : loadBarrelEmcPreShowerHits(pEvent); break;
    case kEndcapEmcPreShowerId : loadEndcapEmcPreShowerHits(pEvent); break;
    case kFpdEastId            :
    case kFmsIdentifier        :
    default: continue;
    }  
  }
  cout << "StiVMCMaker::loadHits -I- Hit Container size:\t" << _toolkit->HitContainer()->size() << endl;
}
//_____________________________________________________________________________
Int_t StiVMCMaker::Make() {
  cout <<"StiVMCMaker::Make() -I- Starting on new event"<<endl;
  eventIsFinished = kFALSE;
  StEvent   * event = dynamic_cast<StEvent*>( GetInputDS("StEvent") );
  
  if (!event) return kStWarn;
  
  if (mTimg[kHitTimg]) mTimg[kHitTimg]->Start(0);
  loadHits(event);
  if (mTimg[kHitTimg]) mTimg[kHitTimg]->Stop();
  // Track loop
  while (kTRUE) {
    // 		obtain track seed from seed finder
    StiKalmanTrack* track = (StiKalmanTrack*)_toolkit->TrackSeedFinder()->findTrack();
    if (! track) break; // no more seeds
    if (! track->Fit()) {
      track->SetFlag(1);
      StiToolkit::instance()->TrackContainer()->push_back(track);
      track->SetId(StiToolkit::instance()->TrackContainer()->size());
      track->ReserveHits();
      continue;
    }
    BFactory::Free(track);
  }
  Int_t nTracks = _toolkit->TrackContainer()->size();
  if (nTracks) {
    if (_eventFiller) _eventFiller->fillEvent(event, _toolkit->TrackContainer());
    const std::vector<StiHit*> *vertices=0;
    if (_toolkit->VertexFinder())    {
      //cout << "StiVMCMaker::Maker() -I- Will Find Vertex"<<endl;
      if (mTimg[kVtxTimg]) mTimg[kVtxTimg]->Start(0);
      
      _toolkit->VertexFinder()->fit(event);
      vertices = _toolkit->VertexFinder()->result();
      
      if (mTimg[kVtxTimg]) mTimg[kVtxTimg]->Stop();
      
      Int_t nVertex =  0;
      if (vertices) nVertex = vertices->size();
      if (nVertex)  {
	//Set minimal errors
	for (size_t i=0;i<vertices->size();i++) {
	  StiHit *vtx=(*vertices)[i];
	  Double_t vtxErr[6];
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
	//cout << "StiVMCMaker::Make() -I- Got Vertex; Extend Tracks"<<endl;
	if (mTimg[kPriTimg]) mTimg[kPriTimg]->Start(0);
	for (Int_t iTrack=0;iTrack<nTracks;iTrack++)		{
	  StiKalmanTrack * track = (StiKalmanTrack*)(*_toolkit->TrackContainer())[iTrack];  
	  track->ExtendTrackToVertices(*vertices);
	}
	if (mTimg[kPriTimg]) mTimg[kPriTimg]->Stop();
	
	//cout << "StiVMCMaker::Make() -I- Primary Filling"<<endl; 
	if (mTimg[kFilTimg]) mTimg[kFilTimg]->Start(0);
	if (_eventFiller) _eventFiller->fillEventPrimaries();
	if (mTimg[kFilTimg]) mTimg[kFilTimg]->Stop();
      }
    }
  }
  Int_t iAns=kStOK,iAnz;
  cout<< "StiVMCMaker::Make() -I- Done"<<endl;
  iAnz = StMaker::Make();
  
  MyClear();
  if (iAns) return iAns;
  if (iAnz) return iAnz;
  return kStOK;
}
//_____________________________________________________________________________
void StiVMCMaker::MyClear() {
  _toolkit->HitContainer()->Clear();
  _toolkit->TrackSeedFinder()->Reset();
  _toolkit->TrackContainer()->Clear();
  _toolkit->HitFactory()->Clear();
  _toolkit->TrackNodeFactory()->Clear();
  _toolkit->TrackFactory()->Clear();
}
// $Log: StiVMCMaker.cxx,v $
// Revision 2.6  2011/02/11 15:56:38  fisyak
// Add Svt and Ssd as active detectors
//
// Revision 2.5  2009/08/20 22:25:30  fisyak
// Freeze before moving to SD set os parameters
//
// Revision 2.4  2009/08/19 19:56:40  fisyak
// Clean up
//
// Revision 2.3  2009/08/19 18:08:01  fisyak
// Eliminate StiStarVertexFinder
//
// Revision 2.2  2009/08/04 18:55:14  fisyak
// Capitilize method names
//
// Revision 2.1  2009/08/02 19:05:36  fisyak
// Add reference track
//
// Revision 1.202  2009/07/24 18:28:21  fisyak
// Split parameters into : Predicted, Fitted and Smoothed
//
// Revision 1.201  2009/07/23 19:40:02  fisyak
// Remove StiKalmanTrackFinder
//
// Revision 1.200  2009/07/20 14:10:25  fisyak
// Remove *Parameter* and exceptions
//
// Revision 1.199  2009/07/20 13:15:46  fisyak
// Remove Filters
//
// Revision 1.198  2009/07/19 20:56:38  fisyak
// Eliminate StiKalmanTrackFitter
//
// Revision 1.197  2009/07/19 20:14:35  fisyak
// remove abstract classes
//
// Revision 1.196  2009/06/14 21:51:04  fisyak
// cleanning from exceptions
//
// Revision 1.195  2009/05/26 21:57:24  fisyak
// Comment out material and detector shape related methods
//
// Revision 1.194  2009/05/06 16:39:45  fisyak
// Move to TRArray
//
// Revision 1.193  2009/04/29 14:36:34  fisyak
// Freeze 0-th version of VMC base reconstruction
//
// Revision 1.192  2009/04/15 23:03:13  fisyak
// add new kFpdEastId and kFmsIdentifier
//
// Revision 1.191  2009/04/15 20:26:53  fisyak
// Clean ups, use VMC TGeo for detector description, load hits in the central place
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
// Clearmem is default now
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
// Added an "if" statement predicated on m_Mode to Clear the memory used by the
// factories at the end of StiVMCMaker::Make().
//
// Revision 1.142  2004/04/15 00:43:22  pruneau
// Added Ssd to the list of possible detectors...
//
// Revision 1.141  2004/03/26 15:30:06  andrewar
// bug in field Reset
//
// Revision 1.140  2004/03/26 14:52:43  calderon
// Print out the magnetic field read from StEvent::eventSummary()
//
// Revision 1.139  2004/03/25 22:42:44  andrewar
// temp mag field fix; cache filed value and Reset if it goes to zero. This
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
// Changed organization so detector geometris are loaded and Build in InitRun
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
// Also added a point to StiToolkit for StiVMCMaker.  This allows for the req. GetDataSet calls in the FTPC code.
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
// Changed StiVMCMaker and Default Toolkit to accomodate the new Event Display
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
// required in StiVMCMaker.
//
// Revision 1.100  2002/08/23 18:16:50  pruneau
// Added StiSimpleTrackFilter to StiVMCMaker to enable simple and
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
