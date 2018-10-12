// Author : Yuri Fisyak
// $Id: StxMaker.cxx,v 1.6 2013/09/16 19:54:04 fisyak Exp $
#include "StxMaker.h"
#include "StxSeedFinder.h"
#include "StxCAInterface.h"
#include "StEvent/StEvent.h"
#include "StEvent/StGlobalTrack.h"
#include "StEvent/StTrackMassFit.h"
#include "StEvent/StL3Trigger.h"
#include "StEvent/StTrack.h"
#include "StEvent/StTrackNode.h"
#include "StEvent/StTrackDefinitions.h"
#include "StEvent/StTrackMethod.h"
#include "StEvent/StTrackDetectorInfo.h"
#include "StEvent/StHelixModel.h"
#include "StTrackGeometry.h"
#include "StEvent/StTpcHit.h"
#include "StEventUtilities/StEventHelper.h"
#include "TRMatrix.h"
#include "TRVector.h"
#include "KFParticle/KFVertex.h"
#include "KFParticle/KFPTrack.h"
// GenFit
#include <iostream>
#include <execinfo.h>
#include <signal.h>
#include <stdlib.h>

#include "GenFit/AbsFinitePlane.h"
#include "GenFit/AbsFitterInfo.h"
#include "GenFit/AbsMeasurement.h"
#include "GenFit/AbsTrackRep.h"
#include "GenFit/ConstField.h"
#include "GenFit/DetPlane.h"
#include "GenFit/Exception.h"
#include "GenFit/FieldManager.h"
#include "GenFit/KalmanFittedStateOnPlane.h"
#include "GenFit/AbsKalmanFitter.h"
#include "GenFit/KalmanFitter.h"
#include "GenFit/KalmanFitterRefTrack.h"
#include "GenFit/KalmanFitterInfo.h"
#include "GenFit/KalmanFitStatus.h"
#include "GenFit/DAF.h"
#include "GenFit/GFGbl.h"
#include "GenFit/MeasuredStateOnPlane.h"
#include "GenFit/MeasurementOnPlane.h"
#include "GenFit/FullMeasurement.h"
#include "GenFit/PlanarMeasurement.h"
#include "GenFit/ProlateSpacepointMeasurement.h"
#include "GenFit/RectangularFinitePlane.h"
#include "GenFit/ReferenceStateOnPlane.h"
#include "GenFit/SharedPlanePtr.h"
#include "GenFit/SpacepointMeasurement.h"
#include "GenFit/StateOnPlane.h"
#include "GenFit/Tools.h"
#include "GenFit/TrackCand.h"
#include "GenFit/TrackCandHit.h"
#include "GenFit/Track.h"
#include "GenFit/TrackPoint.h"
#include "GenFit/WireMeasurement.h"
#include "GenFit/WirePointMeasurement.h"

#include "GenFit/MaterialEffects.h"
#include "GenFit/RKTools.h"
#include "GenFit/RKTrackRep.h"
#include "GenFit/StepLimits.h"
#include "GenFit/TGeoMaterialInterface.h"

#include "GenFit/EventDisplay.h"

#include "GenFit/HelixTrackModel.h"
#include "GenFit/MeasurementCreator.h"
#include "StTpcPlanarMeasurement.h"
#include "TApplication.h"
#include "TCanvas.h"
#include "TDatabasePDG.h"
#include "TEveManager.h"
#include "TGeoManager.h"
#include "TGeoMatrix.h"
#include "TH1D.h"
#include "TRandom.h"
#include "TStyle.h"
#include "TVector3.h"
#include "TStopwatch.h"
#include "TString.h"
#include <vector>

#include "TROOT.h"
#include "TFile.h"
#include "TTree.h"
#include "TDatabasePDG.h"
#include "TMath.h"
#include "TString.h"

#include <memory>
//#define VALGRIND

#ifdef VALGRIND
  #include <valgrind/callgrind.h>
#else
#define CALLGRIND_START_INSTRUMENTATION
#define CALLGRIND_STOP_INSTRUMENTATION
#define CALLGRIND_DUMP_STATS
#endif
#include "StarField.h"
//#define __DEBUG__
#ifdef __DEBUG__
#define DEBUG_LEVEL
#define PrPP(A,B)  DEBUG_LEVEL {LOG_INFO << "StxMaker::" << (#A) << "\t" << (#B) << " = \t" << (B) << endm;}
#else
#define PrPP(A,B)
#endif
using namespace  genfit;
using namespace  std;
ClassImp(StxMaker);
//_____________________________________________________________________________
Int_t StxMaker::Init(){
  // Create tables
  // Create Histograms    
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StxMaker::Make(){
  mEvent = dynamic_cast<StEvent*>( GetInputDS("StEvent") );
  if (! mEvent) {return kStWarn;};
  StEventHelper::Remove(mEvent,"StSPtrVecTrackDetectorInfo");
  StEventHelper::Remove(mEvent,"StSPtrVecTrackNode");
  StEventHelper::Remove(mEvent,"StSPtrVecPrimaryVertex");
  StEventHelper::Remove(mEvent,"StSPtrVecV0Vertex");
  StEventHelper::Remove(mEvent,"StSPtrVecXiVertex");
  StEventHelper::Remove(mEvent,"StSPtrVecKinkVertex");
  //  StiKalmanTrackNode::SetExternalZofPVX(0);

  StxCAInterface::Instance().SetNewEvent();
  // Run reconstruction by the CA Tracker
  StxCAInterface::Instance().Run();
  const int NRecoTracks = StxCAInterface::Instance().GetTracker()->NTracks();
  for ( int iTr = 0; iTr < NRecoTracks; iTr++ ) {
    const AliHLTTPCCAGBTrack &tr = StxCAInterface::Instance().GetTracker()->Track( iTr );
    if (! FitTrack(tr)) continue;
    // Create StTrack
  }
  return kStOK;
}
//________________________________________________________________________________
#ifdef __HANDLER__
void handler(int sig) {
  void *array[10];
  size_t size;

  // get void*'s for all entries on the stack
  size = backtrace(array, 10);

  // print out all the frames to stderr
  fprintf(stderr, "Error: signal %d:\n", sig);
  backtrace_symbols_fd(array, size, 2);
  exit(1);
}
#endif
//________________________________________________________________________________
Double_t StxMaker::ConvertCA2XYZ(const AliHLTTPCCAGBTrack &tr, TVector3 &pos, TVector3 &mom, TMatrixDSym &covM) {
  
  const AliHLTTPCCATrackParam& caPar = tr.InnerParam();
  // --------------------------------------------------------------------------------
  StxNodePars pars;
  StxNodeErrs errs;
  Double_t alpha = tr.Alpha();
  StxCAInterface::Instance().ConvertPars(caPar, alpha, pars, errs);
  Float_t _alpha = TMath::Pi()/2 - alpha;
  Double_t ca = cos(_alpha);
  Double_t sa = sin(_alpha);
  Double_t xyzp[6];
  xyzp[0] = ca*pars.x() - sa*pars.y(); 
  xyzp[1] = sa*pars.x() + ca*pars.y(); 
  xyzp[2] =  pars.z();
  Int_t charge = (pars.ptin() > 0.0) ? -1 : 1;
  Double_t pT = 1./TMath::Abs(pars.ptin());
  Double_t ce = TMath::Cos(pars.eta()+_alpha);
  Double_t se = TMath::Sin(pars.eta()+_alpha);
  Double_t px = pT*ce;
  Double_t py = pT*se;
  Double_t pz = pT*pars.tanl();
  xyzp[3] = px;
  xyzp[4] = py;
  xyzp[5] = pz;
  Double_t dpTdPti = -pT*pT*TMath::Sign(1.,pars.ptin());
  Double_t f[36] = {
    //          x,  y,     z,     eta,               ptin, tanl
    /*  x */  ca, -sa,     0,       0,                  0,    0, 
    /*  y */  sa,  ca,     0,       0,                  0,    0, 
    /*  z */   0,   0,     1,       0,                  0,    0, 
    /* px */   0,   0,     0,     -py,         dpTdPti*ce,    0, 
    /* py */   0,   0,     0,      px,         dpTdPti*se,    0,
    /* pz */   0,   0,     0,       0,dpTdPti*pars.tanl(),   pT};
  TRMatrix F(6,6,f);
  TRSymMatrix C(6,errs.G());
  TRSymMatrix Cov(F,TRArray::kAxSxAT,C);
  // --------------------------------------------------------------------------------
  pos = TVector3(xyzp);
  mom = TVector3(xyzp+3);
  for (Int_t i = 0; i < 6; i++) 
    for (Int_t j = 0; j < 6; j++) 
      covM(i,j) = Cov(i,j);
#ifdef __DEBUG__
  covM.Print("");
#endif
  return charge;
}
//________________________________________________________________________________
Int_t StxMaker::FitTrack(const AliHLTTPCCAGBTrack &tr) {
  static TStopwatch *watch = new  TStopwatch;
  watch->Start(kTRUE);
  //const genfit::eFitterType fitterId = genfit::SimpleKalman;
  const genfit::eFitterType fitterId = genfit::RefKalman;
  //const genfit::eFitterType fitterId = genfit::DafRef;
  //const genfit::eFitterType fitterId = genfit::DafSimple;
  //const genfit::eMultipleMeasurementHandling mmHandling = genfit::weightedAverage;
  //const genfit::eMultipleMeasurementHandling mmHandling = genfit::unweightedClosestToReference;
  //const genfit::eMultipleMeasurementHandling mmHandling = genfit::unweightedClosestToPrediction;
  //const genfit::eMultipleMeasurementHandling mmHandling = genfit::weightedClosestToReference;
  //const genfit::eMultipleMeasurementHandling mmHandling = genfit::weightedClosestToPrediction;
  //const genfit::eMultipleMeasurementHandling mmHandling = genfit::unweightedClosestToReferenceWire;
  const genfit::eMultipleMeasurementHandling mmHandling = genfit::unweightedClosestToPredictionWire;
  //const genfit::eMultipleMeasurementHandling mmHandling = genfit::weightedClosestToReferenceWire;
  //const genfit::eMultipleMeasurementHandling mmHandling = genfit::weightedClosestToPredictionWire;
  const int nIter = 20; // max number of iterations
  const double dPVal = 1.E-3; // convergence criterion

  //  const bool resort = false;
  //  const bool prefit = false; // make a simple Kalman iteration before the actual fit
  //  const bool refit  = false; // if fit did not converge, try to fit again

  //  const bool twoReps = false; // test if everything works with more than one rep in the tracks

  //  const bool checkPruning = true; // test pruning


  const bool matFX = true; // false;         // include material effects; can only be disabled for RKTrackRep!

  //  const bool onlyDisplayFailed = false; // only load non-converged tracks into the display

#ifdef __HANDLER__
  signal(SIGSEGV, handler);   // install our handler
#endif
  // init fitter
  genfit::AbsKalmanFitter* fitter = 0;
  switch (fitterId) {
    case genfit::SimpleKalman:
      fitter = new genfit::KalmanFitter(nIter, dPVal);
      fitter->setMultipleMeasurementHandling(mmHandling);
      break;

    case genfit::RefKalman:
      fitter = new genfit::KalmanFitterRefTrack(nIter, dPVal);
      fitter->setMultipleMeasurementHandling(mmHandling);
      break;

    case genfit::DafSimple:
      fitter = new genfit::DAF(false);
      break;
    case genfit::DafRef:
      fitter = new genfit::DAF();
      break;
  }
  fitter->setMaxIterations(nIter);
  if (Debug()) {
    fitter->setDebugLvl(10);
    gGeoManager->SetVerboseLevel(5);
  } else {
    fitter->setDebugLvl(0);
    gGeoManager->SetVerboseLevel(0);
  }
  /*if (dynamic_cast<genfit::DAF*>(fitter) != nullptr) {
    //static_cast<genfit::DAF*>(fitter)->setBetas(100, 50, 25, 12, 6, 3, 1, 0.5, 0.1);
    //static_cast<genfit::DAF*>(fitter)->setBetas(81, 8, 4, 0.5, 0.1);
    static_cast<genfit::DAF*>(fitter)->setAnnealingScheme(100, 0.1, 5);
    //static_cast<genfit::DAF*>(fitter)->setConvergenceDeltaWeight(0.0001);
    //fitter->setMaxIterations(nIter);
  }*/

  genfit::FieldManager::getInstance()->init(new genfit::StarField());
  //  genfit::FieldManager::getInstance()->useCache(true, 8);
  //  genfit::FieldManager::getInstance()->useCache(false, 0);
  genfit::MaterialEffects::getInstance()->init(new genfit::TGeoMaterialInterface());
  const int pdg = 211; // -13;               // particle pdg code mu+
  //  const double charge = TDatabasePDG::Instance()->GetParticle(pdg)->Charge()/(3.);
  //========== Reference  track ======================================================================
  TVector3 posSeed, momSeed;
  TMatrixDSym covSeed(6);
  const AliHLTTPCCATrackParam& caPar = tr.InnerParam();
  Double_t alpha = tr.Alpha();
  StxNodePars pars;
  StxNodeErrs errs;
  StxCAInterface::Instance().ConvertPars(caPar, alpha, pars, errs);
  Double_t sign = ConvertCA2XYZ(tr, posSeed, momSeed, covSeed);
  genfit::AbsTrackRep* rep = new genfit::RKTrackRep(sign*pdg);
  if (Debug()) rep->setDebugLvl();
  rep->setPropDir(1);
  //  genfit::AbsTrackRep* secondRep = 0;
  //  if (twoReps) secondRep = new genfit::RKTrackRep(sign*-211);
  genfit::MeasuredStateOnPlane stateSeed(rep);
  stateSeed.setPosMomCov(posSeed, momSeed, covSeed);
  // create track
  //  genfit::Track* secondTrack(nullptr);
  //  genfit::Track  fitTrack(rep, posSeed, momSeed);
  TVectorD  state7(6);
  TMatrixDSym origCov(6);
  stateSeed.get6DStateCov(state7, origCov);
  genfit::Track fitTrack(rep, state7, origCov);

  //  genfit::Track  fitTrack(rep, stateSeed, covSeed);
  
  // smeared start state
  //  genfit::MeasuredStateOnPlane stateSmeared(rep);
  //  rep->setPosMomCov(stateSeed, posSeed, momSeed, covSeed);
  // propagation direction. (-1, 0, 1) -> (backward, auto, forward).
  if (!matFX) genfit::MaterialEffects::getInstance()->setNoEffects();
  // remember original initial state
  const genfit::StateOnPlane stateRefOrig(stateSeed);
  //========== Mesurements ======================================================================
  const int NHits = tr.NHits();
  vector<SeedHit_t>        &fSeedHits = StxCAInterface::Instance().GetSeedHits();
  std::vector<genfit::eMeasurementType> measurementTypes;
  //  std::vector< std::vector<genfit::AbsMeasurement*> > measurements;
  //  genfit::AbsMeasurement* measurement = 0;
  if (Debug()) {
    cout << "momSeed\t"; momSeed.Print("");
    cout << "posSeed\t"; posSeed.Print("");
    cout << "NHits = " << NHits << endl;
  }
  for ( int iHit = 0; iHit < NHits; iHit++ ){ 
    const Int_t index = StxCAInterface::Instance().GetTracker()->TrackHit( tr.FirstHitRef() + iHit );
    const Int_t hId   = StxCAInterface::Instance().GetTracker()->Hit( index ).ID();
    const StTpcHit *tpcHit = fSeedHits[hId].hit;
    genfit::PlanarMeasurement* measurement = new StTpcPlanarMeasurement(tpcHit, nullptr);
    fitTrack.insertPoint(new genfit::TrackPoint(measurement, &fitTrack));
  }
  try{
    //check
    fitTrack.checkConsistency();
    
    // do the fit
    fitter->processTrack(&fitTrack);
    
    // print fit result
    fitTrack.getFittedState().Print();
    
    //check
    fitTrack.checkConsistency();
  }
  catch(genfit::Exception& e){
    std::cout<<"Exception, next track"<<std::endl;
    std::cout << e.what();
    return kStErr;
  }
  //_________ Fill StTrack _______________
  //  UInt_t npoints = fitTrack.getNumPoints();
  
  if (Debug()) {
    std::cout << "Inner Parameters" << std::endl << "====================" << endl;
    fitTrack.getFittedState().Print();
  }
  //  const AbsTrackRep* rep = fitTrack.getCardinalRep();
  genfit::TrackPoint* point = fitTrack.getPointWithMeasurementAndFitterInfo(0, rep);
  genfit::AbsFitterInfo* fitterInfo = point->getFitterInfo(rep);
  const genfit::MeasuredStateOnPlane& measuredPointStateI = fitterInfo->getFittedState(true);
  TVector3 posI, momI;
  TMatrixDSym covI(6,6);
  measuredPointStateI.getPosMomCov(posI, momI, covI);
  if (Debug()) {
    std::cout << "Outer Parameters" << std::endl << "====================" << endl;
    fitTrack.getFittedState(-1).Print();
  }
  point = fitTrack.getPointWithMeasurementAndFitterInfo(-1, rep);
  fitterInfo = point->getFitterInfo(rep);
  const genfit::MeasuredStateOnPlane& measuredPointStateO = fitterInfo->getFittedState(true);
  TVector3 posO, momO;
  TMatrixDSym covO(6,6);
  measuredPointStateO.getPosMomCov(posO, momO, covO);
  try{
    FillGlobalTrack(&fitTrack);
  }
  catch(genfit::Exception& e) {
    std::cout << "Exception, FillGlobalTrack" << std::endl;
    return kStErr;
  }  
  watch->Print("");
  return kStOK;
}
// $Log: StxMaker.cxx,v $
//_____________________________________________________________________________
Bool_t StxMaker::Accept(genfit::Track *kTrack) {
#if 0
  Int_t nFittedPoints   = track->FitPointCount(0);
  if (nFittedPoints  <  5 )   return kFALSE;
  if(track->TrackLength()<=0) return kFALSE; 
#endif
  return kTRUE;
}
//_____________________________________________________________________________
void StxMaker::FillGlobalTrack(genfit::Track *kTrack) {
  if (!Accept(kTrack)) return; // get rid of riff-raff
  StTrackDetectorInfo* detInfo = new StTrackDetectorInfo;
  // track node where the new StTrack will reside
  StTrackNode* trackNode = new StTrackNode;
  // actual filling of StTrack from genfit::Track 
  StGlobalTrack* gTrack = new StGlobalTrack;
  // filling successful, set up relationships between objects
  StSPtrVecTrackNode& trNodeVec = mEvent->trackNodes(); 
  UShort_t Id = trNodeVec.size() + 1;
  gTrack->setKey(Id);
  // reuse the utility to fill the topology map
  // this has to be done at the end as it relies on
  // having the proper track->detectorInfo() relationship
  // and a valid StDetectorInfo object.
  //cout<<"Tester: Event Track Node Entries: "<<trackNode->entries()<<endl;
  //  mTrkNodeMap.insert(map<StxKalmanTrack*,StTrackNode*>::value_type (kTrack,trNodeVec.back()) );
  FillTrack(gTrack,kTrack,detInfo);
  Int_t ibad = gTrack->bad();
  if (ibad) {
    delete detInfo;
    delete gTrack;
    delete trackNode;
    throw genfit::Exception("Consistency check failed ", __LINE__, __FILE__);
  }
  StSPtrVecTrackDetectorInfo& detInfoVec = mEvent->trackDetectorInfo(); 
  detInfoVec.push_back(detInfo);
  trackNode->addTrack(gTrack);
  trNodeVec.push_back(trackNode);
  return;
}
//_____________________________________________________________________________
void StxMaker::FillTrack(StTrack* gTrack, genfit::Track * kTrack,StTrackDetectorInfo* detInfo )
{
  //cout << "StxMaker::FillTrack()" << endl;
  // encoded method = 16 bits = 12 fitting and 4 finding, for the moment use:
  // kKalmanFitId
  // bit 15 for finding, (needs to be changed in StEvent).
  // change: make sure bits are ok, are the bits set up one in each position and nothing else?
  // this would mean that the encoded method is wasting space!
  // the problem is that in principle there might be combinations of finders for each tracking detector
  // but the integrated tracker will use only one for all detectors maybe
  // so need this bit pattern:
  // finding 100000000000     
  // fitting             0010 
  //            32768    +    2 = 32770;
  //
  // above is no longer used, instead use kITKalmanfitId as fitter and tpcOther as finding method
  //  gTrack->setEncodedMethod(mStxEncoded);
  Double_t tlen = kTrack->getTrackLen();
  assert(tlen >0.0 && tlen<1000.);
  gTrack->setLength(tlen);// someone removed this, grrrr!!!!
  FillDetectorInfo(gTrack,detInfo,kTrack,true); //3d argument used to increase/not increase the refCount. MCBS oct 04.
  FillGeometry(gTrack, kTrack, false); // inner geometry
  FillGeometry(gTrack, kTrack, true ); // outer geometry
  FillFitTraits(gTrack, kTrack);
  gTrack->setDetectorInfo(detInfo);
#if 0
  StuFixTopoMap(gTrack);
#endif
  FillFlags(gTrack);
#if 0
  if (!gTrack->IsPrimary()) 
#endif
    FillDca(gTrack,kTrack);
  return;
}
//_____________________________________________________________________________
void StxMaker::FillEventPrimaries()  {
#if 0
  //cout <<"StxMaker::FillEventPrimaries() -I- Started"<<endl;
  mGloPri=1;
  if (!mTrkNodeMap.size()) 
    {
      cout <<"StxMaker::FillEventPrimaries(). ERROR:\t"
	   << "Mapping between the StTrackNodes and the genfit::Track s is empty.  Exit." << endl;
      return;
    }
  //Added residual maker...aar
  StPrimaryVertex* vertex = 0;
  StSPtrVecTrackDetectorInfo& detInfoVec = mEvent->trackDetectorInfo();
  cout << "StxMaker::FillEventPrimaries() -I- Tracks in container:" << mTrackStore->size() << endl;
  Int_t mTrackN=0,mVertN=0;
  Int_t noPipe=0;
  Int_t ifcOK=0;
  Int_t fillTrackCount1=0;
  Int_t fillTrackCount2=0;
  Int_t fillTrackCountG=0;
  StErrorHelper errh;
  Int_t nTracks = mTrackStore->size();
  genfit::Track  *kTrack = 0;
  StPrimaryTrack *pTrack = 0;
  StGlobalTrack  *gTrack = 0;
  StTrackNode    *nTRack = 0;
  mTrackNumber=0;
  for (mTrackN=0; mTrackN<nTracks;++mTrackN) {
    kTrack = (genfit::Track *)(*mTrackStore)[mTrackN];
    if (!Accept(kTrack)) 			continue;
    map<StxKalmanTrack*, StTrackNode*>::iterator itKtrack = mTrkNodeMap.find(kTrack);
    if (itKtrack == mTrkNodeMap.end())  	continue;//Stx global was rejected
    mTrackNumber++;

    nTRack = (*itKtrack).second;
    assert(nTRack->entries()<=10);
    assert(nTRack->entries(global)); 

    //Double_t globalDca = nTRack->track(global)->impactParameter();
    //Even though this is filling of primary tracks, there are certain
    // quantities that need to be filled for global tracks that are only known
    // after the vertex is found, such as dca.  Here we can fill them.
    // 
    gTrack = static_cast<StGlobalTrack*>(nTRack->track(global));
    assert(gTrack->key()==kTrack->Id());
    Float_t minDca = 1e10; //We do not know which primary. Use the smallest one
    
    pTrack = 0;
    for (mVertN=0; (vertex = mEvent->primaryVertex(mVertN));mVertN++) {
      StThreeVectorD vertexPosition = vertex->position();
      Double_t zPrim = vertexPosition.z();
      // loop over genfit::Track s
      Float_t globalDca = impactParameter(gTrack,vertexPosition);
      if (fabs(minDca) > fabs(globalDca)) minDca = globalDca;
 
      if (!kTrack->IsPrimary())			continue;
      StxKalmanTrackNode *lastNode = kTrack->LastNode();
      StxHit *pHit = lastNode->Hit();
      if (fabs(pHit->z_g()-zPrim)>0.1)		continue;//not this primary

      fillTrackCount1++;
      // detector info
      StTrackDetectorInfo* detInfo = new StTrackDetectorInfo;
      FillDetectorInfo(detInfo,kTrack,false); //3d argument used to increase/not increase the refCount. MCBS oct 04.
      StPrimaryTrack* pTrack = new StPrimaryTrack;
      pTrack->setKey( gTrack->key());

      FillTrack(pTrack,kTrack, detInfo);
      // set up relationships between objects
      detInfoVec.push_back(detInfo);

      nTRack->addTrack(pTrack);  // StTrackNode::addTrack() calls track->setNode(this);
      vertex->addDaughter(pTrack);
      fillTrackCount2++;
      Int_t ibad = pTrack->bad();
      errh.Add(ibad);
      if (ibad) {
//VP	        printf("PTrack error: %s\n",errh.Say(ibad).Data());
//VP	        throw runtime_error("StxMaker::fillEventPrimaries() StTrack::bad() non zero");
      }
      if (pTrack->numberOfPossiblePoints()<10) 		break;
      if (pTrack->geometry()->momentum().mag()<0.1) 	break;
      fillTrackCountG++;
      break;
    } //end of verteces
      kTrack->setDca(minDca);
      gTrack->setImpactParameter(minDca);
      if (pTrack) pTrack->setImpactParameter(minDca);

  } // kalman track loop
  mTrkNodeMap.clear();  // need to Reset for the next event
  cout <<"StxMaker::FillEventPrimaries() -I- Primaries (1):"<< fillTrackCount1<< " (2):"<< fillTrackCount2<< " no pipe node:"<<noPipe<<" with IFC:"<< ifcOK<<endl;
  cout <<"StxMaker::FillEventPrimaries() -I- GOOD:"<< fillTrackCountG <<endl;
  errh.Print();
#endif
  return;
}
//_____________________________________________________________________________
/// use the vector of StHits to fill the detector info
/// change: currently point and fit points are the same for genfit::Track s,
/// if this gets modified later in ITTF, this must be changed here
/// but maybe use track->PointCount() later?
//_____________________________________________________________________________
void StxMaker::FillDetectorInfo(StTrack *gTrack, StTrackDetectorInfo* detInfo, genfit::Track * track, bool refCountIncr) {
  //  output array actually is count[maxDetId+1][3] 
  //  count[0] all detectors
  //  count[detId] for particular detector
  //  count[detId][0] == number of possible points
  //  count[detId][1] == number of measured points
  //  count[detId][2] == number of fitted   points
  enum {kPP=0,kMP=1,kFP=2};
  Int_t dets[kMaxDetectorId][3];

  memset(dets,0,sizeof(dets));
  genfit::TrackPoint *firstTP = 0, *lastTP = 0;
  const AbsTrackRep* rep = track->getCardinalRep();
  for (std::vector< genfit::TrackPoint* >::const_iterator it = track->getPointsWithMeasurement().begin(); 
       it != track->getPointsWithMeasurement().end(); ++it) {
    genfit::TrackPoint *tp = *it;
    for (std::vector< genfit::AbsMeasurement* >::const_iterator im = tp->getRawMeasurements().begin(); 
	 im !=  tp->getRawMeasurements().end(); ++im) {
      dets[0][kPP]++;
      const StTpcPlanarMeasurement *measurement =  dynamic_cast<StTpcPlanarMeasurement *>(*im);
      if (! measurement) continue;
      if (! firstTP) firstTP = tp;
      lastTP = tp;
      Int_t detId = measurement->getPlaneId()/10000 + 1;
      dets[0][kPP]++; dets[detId][kPP]++;
      const StHit *hit = measurement->Hit();
      if (! hit) continue;
      detId = hit->detector();
      dets[0][kMP]++; dets[detId][kMP]++;
      if (! tp->hasFitterInfo(rep)) continue;
      dets[0][kFP]++; dets[detId][kFP]++;
      detInfo->addHit((StHit *) hit,refCountIncr);
#if 0
      if (!refCountIncr) 	continue;
      hit->setFitFlag(stiHit->timesUsed());
#endif
    }
  }
  genfit::TrackPoint *flTP[2] = {firstTP, lastTP};
  for (Int_t i = 0; i < 2; i++) {
    if (! flTP[i]) continue;
    genfit::AbsFitterInfo* fitterInfo = flTP[i]->getFitterInfo();
    const genfit::MeasuredStateOnPlane& measuredPointState = fitterInfo->getFittedState(true);
    TVector3 pos = measuredPointState.getPos();
    StThreeVectorF posF(pos.X(), pos.Y(), pos.Z());
    if (! i) detInfo->setFirstPoint(posF);
    else     detInfo->setLastPoint (posF);
  }
  for (Int_t i=1;i<kMaxDetectorId;i++) {
    if(!dets[i][0]) continue;
    gTrack->setNumberOfPossiblePoints((unsigned char)dets[i][0],(StDetectorId)i);
    if (!dets[i][1]) continue;
    detInfo->setNumberOfPoints(dets[i][1],static_cast<StDetectorId>(i));
  }
}
//_____________________________________________________________________________
void StxMaker::FillGeometry(StTrack* gTrack, genfit::Track * track, bool outer) {
  assert(gTrack);
  assert(track) ;
  genfit::AbsTrackRep* rep = 0;
  genfit::TrackPoint* point = 0;
  if (! outer) point = track->getPointWithMeasurementAndFitterInfo(0, rep);
  else         point = track->getPointWithMeasurementAndFitterInfo(-1, rep);
  genfit::AbsFitterInfo* fitterInfo = point->getFitterInfo(rep);
  const genfit::MeasuredStateOnPlane& measuredPointState = fitterInfo->getFittedState(true);
  TVector3 pos, mom;
  TMatrixDSym cov(6,6);
  measuredPointState.getPosMomCov(pos, mom, cov);
  TVector3 field = FieldManager::getInstance()->getField()->get(pos);
  StThreeVectorF origin(pos.X(),pos.Y(),pos.Z());
  static const double EC = 2.99792458e-4;
  StThreeVectorF p(mom.X(), mom.Y(), mom.Z());
  Double_t hz = EC*field.Z();
  Double_t qovepT = measuredPointState.getCharge()/mom.Pt();
  Double_t curvature = - hz*qovepT;
  Double_t helicity = (curvature < 0) ? -1 : 1;
  StTrackGeometry* geometry = new StHelixModel(short(measuredPointState.getCharge()),
					       mom.Phi(),
					       fabs(curvature), 
					       TMath::PiOver2() - mom.Theta(),
					       origin, 
					       p,
					       helicity);
  if (outer)
    gTrack->setOuterGeometry(geometry);
  else
    gTrack->setGeometry(geometry);
#ifdef  __kfpAtFirstHit__
  Double_t xyzp[6] = {pos.X(), pos.Y(), pos.Z(), mom.X(), mom.Y(), mom.Z()};
  Double_t CovXyzp[21];
  Int_t ii = 0;
  for (Int_t i = 0; i < 6; i++)
    for (Int_t j = 0; j < = i; j++)
      CovXyzp[ii] = cov(i,j);
  KFPTrack *KFPTrackAtHit = new KFPTrack();
  KFPTrackAtHit->SetID(gTrack->key());
  KFPTrackAtHit->SetCharge(track->getCharge());
  KFPTrackAtHit->SetParameters(xyzp);
  KFPTrackAtHit->SetCovarianceMatrix(CovXyzp);
  if (outer) gTrack->setKFPTrackatLastHit(KFPTrackAtHit);
  else       gTrack->setKFPTrackatFirstHit(KFPTrackAtHit);
#endif
  return;
}
//_____________________________________________________________________________
void StxMaker::FillFitTraits(StTrack* gTrack, genfit::Track * track){
  // mass
  // this makes no sense right now... Double_t massHyp = track->getMass();  // change: perhaps this mass is not set right?
  UShort_t geantIdPidHyp = 9999;
  //if (.13< massHyp<.14) 
  geantIdPidHyp = 9;
#if 0
  // chi square and covariance matrix, plus other stuff from the
  // innermost track node
  genfit::Track Node* node = track->InnerMostHitNode(3);
  Float_t x[6],covMFloat[15];
  node->GlobalTpt(x,covMFloat);
#else
  Float_t covMFloat[15];
#endif
  KalmanFitStatus *fitStatus = track->getKalmanFitStatus();
  Float_t chi2[2];
  //get chi2/dof
  chi2[0] = fitStatus->getChi2()/fitStatus->getNdf();
  chi2[1] = -999; // change: here goes an actual probability, need to calculate?
#if 0
  // December 04: The second element of the array will now hold the incremental chi2 of adding
  // the vertex for primary tracks
  if (gTrack->type()==primary) {
    assert(node->Detector()==0);
    chi2[1]=node->Chi2();
  }
#endif    
  // setFitTraits uses assignment operator of StTrackFitTraits, which is the default one,
  // which does a memberwise copy.  Therefore, constructing a local instance of 
  // StTrackFitTraits is fine, as it will get properly copied.
  StTrackFitTraits fitTraits(geantIdPidHyp,0,chi2,covMFloat);
#if 0
  if (gTrack->type()==primary) {
     fitTraits.setPrimaryVertexUsedInFit(true);
  }
#endif
  gTrack->setFitTraits(fitTraits);
  return;
}

///_____________________________________________________________________________
/// data members from StEvent/StTrack.h
///  The track flag (mFlag accessed via flag() method) definitions with ITTF 
///(flag definition in EGR era can be found at  http://www.star.bnl.gov/STAR/html/all_l/html/dst_track_flags.html)
///
///  mFlag=zxyy, where  z = 1 for pile up track in TPC (otherwise 0) 
///                     x indicates the detectors included in the fit and 
///                    yy indicates the status of the fit. 
///  Positive mFlag values are good fits, negative values are bad fits. 
///
///  The first digit indicates which detectors were used in the refit: 
///
///      x=1 -> TPC only 
///      x=3 -> TPC       + primary vertex 
///      x=5 -> SVT + TPC 
///      x=6 -> SVT + TPC + primary vertex 
///      x=7 -> FTPC only 
///      x=8 -> FTPC      + primary 
///      x=9 -> TPC beam background tracks            
///
///  The last two digits indicate the status of the refit: 
///       = +x01 -> good track 
///
///       = -x01 -> Bad fit, outlier removal eliminated too many points 
///       = -x02 -> Bad fit, not enough points to fit 
///       = -x03 -> Bad fit, too many fit iterations 
///       = -x04 -> Bad Fit, too many outlier removal iterations 
///       = -x06 -> Bad fit, outlier could not be identified 
///       = -x10 -> Bad fit, not enough points to start 
///
///       = -x11 -> Short track pointing to EEMC

void StxMaker::FillFlags(StTrack* gTrack) {
  if (gTrack->type()==global) {
    gTrack->setFlag(101); //change: make sure flag is ok
  }
  else if (gTrack->type()==primary) {
    gTrack->setFlag(301);
  }
  StTrackFitTraits& fitTrait = gTrack->fitTraits();
  //int tpcFitPoints = fitTrait.numberOfFitPoints(kTpcId);
  Int_t svtFitPoints = fitTrait.numberOfFitPoints(kSvtId);
  Int_t ssdFitPoints = fitTrait.numberOfFitPoints(kSsdId);
  Int_t pxlFitPoints = fitTrait.numberOfFitPoints(kPxlId);
  Int_t istFitPoints = fitTrait.numberOfFitPoints(kIstId);
  //  Int_t totFitPoints = fitTrait.numberOfFitPoints();
  /// In the flagging scheme, I will put in the cases for
  /// TPC only, and TPC+SVT (plus their respective cases with vertex)
  /// Ftpc case has their own code and SSD doesn't have a flag...

  // first case is default above, tpc only = 101 and tpc+vertex = 301
  // next case is:
  // if the track has svt points, it will be an svt+tpc track
  // (we assume that the ittf tracks start from tpc, so we don't
  // use the "svt only" case.)
  if (svtFitPoints+ssdFitPoints+pxlFitPoints+istFitPoints>0) {
      if (gTrack->type()==global) {
	  gTrack->setFlag(501); //svt+tpc
      }
      else if (gTrack->type()==primary) {
	  gTrack->setFlag(601); //svt+tpc+primary
      }
  }
  const StTrackDetectorInfo *dinfo = gTrack->detectorInfo();
  if (dinfo) {
    Int_t NoTpcFitPoints = dinfo->numberOfPoints(kTpcId);
    Int_t NoFtpcWestId   = dinfo->numberOfPoints(kFtpcWestId);
    Int_t NoFtpcEastId   = dinfo->numberOfPoints(kFtpcEastId);
    // Check that it could be TPC pile-up track, i.e. in the same half TPC (West East) 
    // there are more than 2 hits with wrong Z -position
    Int_t flag = TMath::Abs(gTrack->flag());
    if (NoTpcFitPoints >= 11) {
      const StTrackDetectorInfo *dinfo = gTrack->detectorInfo();
      const StPtrVecHit& hits = dinfo->hits(kTpcId);
      Int_t Nhits = hits.size();
      Int_t NoWrongSignZ = 0;
      for (Int_t i = 0; i < Nhits; i++) {
	const StTpcHit *hit = (StTpcHit *) hits[i];
	if ((hit->position().z() < -1.0 && hit->sector() <= 12) ||
	    (hit->position().z() >  1.0 && hit->sector() >  12)) NoWrongSignZ++;
      }
      if (NoWrongSignZ >= 2) 
	gTrack->setFlag((flag%1000) + 1000); // +1000
    }
    if (NoTpcFitPoints < 11 && NoFtpcWestId < 5 && NoFtpcEastId < 5) { 
      // hadrcoded number correspondant to  __MIN_HITS_TPC__ 11 in StMuFilter.cxx
      //keep most sig. digit, set last digit to 2, and set negative sign
      gTrack->setFlag(-(((flag/100)*100)+2)); // -x02 
      if (gTrack->geometry()) {
	const StThreeVectorF &momentum = gTrack->geometry()->momentum();
	if (momentum.pseudoRapidity() > 0.5) {
	  const StTrackDetectorInfo *dinfo = gTrack->detectorInfo();
	  const StPtrVecHit& hits = dinfo->hits();
	  Int_t Nhits = hits.size();
	  for (Int_t i = 0; i < Nhits; i++) {
	    const StHit *hit = hits[i];
	    if (hit->position().z() > 150.0) {
	      gTrack->setFlag((((flag/100)*100)+11)); // +x11 
	      return;
	    }
	  }
	}
      }
    }
  }
}
//_____________________________________________________________________________
Double_t StxMaker::impactParameter(genfit::Track * track, StThreeVectorD &vertexPosition) {
#if 0
  return   track->InnerMostNode(2)->Helix()->helix().distance(vertexPosition);
#else
  return 0;
#endif
}
//_____________________________________________________________________________
Double_t StxMaker::impactParameter(StTrack* track, StThreeVectorD &vertex) {
  return track->geometry()->helix().distance(vertex);
}
//_____________________________________________________________________________
void StxMaker::FillDca(StTrack* stTrack, genfit::Track * track)
{
  StGlobalTrack *gTrack = dynamic_cast<StGlobalTrack*>(stTrack);
  assert(gTrack);
  static TVector3 linePoint(0,0,0);
  static TVector3 lineDirection(0,0,1);
  const AbsTrackRep* rep = track->getCardinalRep();
  genfit::TrackPoint* point = track->getPointWithMeasurementAndFitterInfo(0, rep);
  genfit::AbsFitterInfo* fitterInfo = point->getFitterInfo(rep);
  const genfit::MeasuredStateOnPlane& stateI = fitterInfo->getFittedState(true);
  genfit::MeasuredStateOnPlane  state = stateI;
  try{
    //  Double_t s = 
    rep->extrapolateToLine(state, linePoint, lineDirection);
    state.Print("");
  }
  catch(genfit::Exception& e) {
    std::cout << "Exception, fail to make DCA" << std::endl;
    std::cout << "Inner Parameters" << std::endl << "====================" << endl;
    track->getFittedState().Print();
    std::cout << "Outer Parameters" << std::endl << "====================" << endl;
    track->getFittedState(-1).Print();
    return;
  }
  state.Print();
  KalmanFitStatus *fitStatus = track->getKalmanFitStatus();
  Double_t chi2 = fitStatus->getChi2();
  Int_t Ndf = fitStatus->getNdf();
#if 0
  TVectorD &stateKF  = state.getState();
  Double_t charge = (stateKF[0] > 0) ? 1 : -1;
#endif
  Double_t charge = state.getCharge();
  TVector3 mom, pos;
  TMatrixDSym cov(6,6);
  state.getPosMomCov(pos, mom, cov);
  // ___  KFParticle at Dca to (0,0) ____
  Int_t kg = gTrack->key();
  static KFParticle fParticle;
  static KFPTrack kfTrack;
  Float_t xyzp[6], CovXyzp[21];
  pos.GetXYZ(xyzp);
  mom.GetXYZ(xyzp+3);
  Int_t ij = 0;
  for (Int_t i = 0; i < 6; i++) {
    for (Int_t j = 0; j <= i; j++) {
      CovXyzp[ij] = cov(i,j);
      ij++;
    }
  }
  kfTrack.SetParameters(xyzp);
  kfTrack.SetCovarianceMatrix(CovXyzp);
  kfTrack.SetNDF(Ndf);
  kfTrack.SetChi2(chi2);
  //  kfTrack.SetId(kg);
  Int_t pdg = rep->getPDG();
  kfTrack.SetCharge(charge);
  fParticle = KFParticle(kfTrack, pdg);
  fParticle.SetPDG(pdg);
  fParticle.SetId(kg);
  fParticle.AddDaughterId(kg);
#if 0
  StTrackMassFitAtDca *dcaTrack = new StTrackMassFitAtDca(kg, &fParticle);
  gTrack->node()->addTrack(dcaTrack);
#endif
  // 
  Double_t pT = mom.Pt();
  TVector3 field = FieldManager::getInstance()->getField()->get(pos);
  static const double EC = 2.99792458e-4;
  Double_t hz = EC*field.Z();
  Double_t qoverpT = charge/mom.Pt();
  Double_t qoverpT2 = qoverpT * qoverpT;
  Double_t curvature = - hz*qoverpT;
  Double_t sinPsi = mom.X()/pT;
  Double_t cosPsi = mom.Y()/pT;
  Double_t Psi = TMath::ATan2(sinPsi, cosPsi);
  Double_t tanL   = mom.Z()/pT;
  Double_t Imp    = -pos.X()*sinPsi + pos.Y()*cosPsi;
#if 0
  Double_t dpTdpx = - mom.X()/pT;
  Double_t dpTdpy = - mom.Y()/pT;
#endif
  Double_t V = - qoverpT;
  Double_t dImpdPsi = -(pos.X()*cosPsi + pos.Y()*sinPsi);
  Double_t dPsidpx  = -mom.Y()*qoverpT2;
  Double_t dPsidpy  =  mom.X()*qoverpT2;
  Double_t VpT2  = V/(pT*pT);
  Double_t f[30] = {
    //                 x,       y,    z,               px,               py,    pz	
    /* Imp   */  -sinPsi,  cosPsi,    0, dImpdPsi*dPsidpx, dImpdPsi*dPsidpy,     0,
    /* Z     */        0,       0,    1,                0,                0,     0,
    /* Psi   */        0,       0,    0,          dPsidpx,          dPsidpy,     0,
    /* -q/pT */        0,       0,    0,    -VpT2*mom.X(),    -VpT2*mom.Y(),     0,
    /* tanL  */        0,       0,    0,    -tanL*mom.X(),    -tanL*mom.Y(),  1./pT
  };
  TRMatrix F(5,6,f);
  TRSymMatrix C(6,CovXyzp);
  TRSymMatrix Cov(F,TRArray::kAxSxAT,C);
  Float_t setp[6];
  setp[0] = Imp; 
  setp[1] = pos.Z(); 
  setp[2] = Psi; 
  setp[3] = V; 
  setp[4] = tanL; 
  setp[5] = curvature;
  Float_t sete[15];
  for (Int_t i = 0; i < 15; i++) sete[i] = Cov[i];
  StDcaGeometry *dca = new StDcaGeometry;
  gTrack->setDcaGeometry(dca);
  dca->set(setp,sete);
}
#if 0
//_____________________________________________________________________________
void StxMaker::FillStHitErr(StHit *hh,const StxKalmanTrackNode *node) {
#if 0
  Double_t stiErr[6];
  TCL::ucopy(node->V().GetArray(),stiErr,6);
#else
  TRSymMatrix V(node->V());
  Double_t stiErr[6] = {
    0., 
    0., V(0,0),
    0., V(1,0), V(1,1)
  };
#endif
  TRMatrix  R(3,3,node->Detector()->GetMatrix()->GetRotationMatrix());
  TRSymMatrix StxErr(3,stiErr);
  TRSymMatrix StErr(R,TRArray::kAxSxAT,StxErr);
  StThreeVectorF f3(TMath::Sqrt(StErr(0,0)),TMath::Sqrt(StErr(1,1)),TMath::Sqrt(StErr(2,2)));
  hh->setPositionError(f3);
}
#endif
