// Author : Yuri Fisyak
// $Id: StxMaker.cxx,v 1.6 2013/09/16 19:54:04 fisyak Exp $
#include "StxMaker.h"
#include "StxSeedFinder.h"
#include "StxCAInterface.h"
#include "StEvent/StEvent.h"
#include "StEvent/StGlobalTrack.h"
#include "StEvent/StL3Trigger.h"
#include "StEvent/StTrackDetectorInfo.h"
#include "StEvent/StTrackNode.h"
#include "StEvent/StTrackDefinitions.h"
#include "StEvent/StTrackMethod.h"
#include "StEvent/StTpcHit.h"
#include "StEventUtilities/StEventHelper.h"
#include "TRMatrix.h"
#include "TRVector.h"
#include "StDetectorDbMaker/St_tpcPadConfigC.h"
#include "StDetectorDbMaker/StiTPCHitErrorCalculator.h"
#include "StDetectorDbMaker/StiTpcInnerHitErrorCalculator.h"
#include "StDetectorDbMaker/StiTpcOuterHitErrorCalculator.h"
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

#include "TApplication.h"
#include "TCanvas.h"
#include "TDatabasePDG.h"
#include "TEveManager.h"
#include "TGeoManager.h"
#include "TGeoMatrix.h"
#include "TGeoPhysicalNode.h"
#include "TGeoBBox.h"
#include "StarVMC/StarVMCApplication/StarVMCDetector.h"
#include "TH1D.h"
#include "TRandom.h"
#include "TStyle.h"
#include "TVector3.h"
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
ClassImp(StxMaker);
//_____________________________________________________________________________
Int_t StxMaker::Init(){
  // Create tables
  // Create Histograms    
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StxMaker::Make(){
  StEvent   *mEvent = dynamic_cast<StEvent*>( GetInputDS("StEvent") );
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
#if 0
  Double_t X = caPar.GetX();
  Double_t Y = caPar.GetY();        // 0
  Double_t Z = caPar.GetZ();        // 1
  Double_t S = caPar.GetSinPhi();   // 2
  Double_t T = caPar.GetDzDs();     // 3
  Double_t V = caPar.GetQPt();      // 4

  Double_t x =  X;
  Double_t y =  Y;
  Double_t z = -Z;
  Double_t charge = 1;
  if (V > 0) charge = -1;
  Double_t pT = - charge/V;
  Double_t t  = -T;
  Double_t Phi = TMath::Pi()/2 - TMath::ASin(S);
  Double_t s   = TMath::Sin(Phi); // sqrt(1 - S**2)
  Double_t c   = TMath::Cos(Phi); // S
  
  Double_t px = pT*c; //  pT*S
  Double_t py = pT*s; //  pT*sqrt(1 - S**2)
  Double_t pz = pT*t; // -pT*T
  Double_t xyzp[6] = { x, y, z, px, py, pz};
  Double_t dpTdV = -charge*pT*pT;
  Double_t f[36] = {
    //         Y    Z       S          T         V
    /*  x */   0,   0,      0,         0,        0,
    /*  y */   1,   0,      0,         0,        0,
    /*  z */   0,  -1,      0,         0,        0,
    /* px */   0,   0,     pT,         0,  dpTdV*s,
    /* py */   0,   0, pT*s/c,         0 , dpTdV*c,
    /* pz */   0,   0,      0,       -pT, -dpTdV*t}; 
  TRMatrix F(6,5,f);      PrPP(ConvertCA2XYZ,F);
  const Float_t *caCov = caPar.GetCov();
  TRSymMatrix C(5,caCov); PrPP(ConvertCA2XYZ,C);
  TRSymMatrix Cov(F,TRArray::kAxSxAT,C); PrPP(ConvertCA2XYZ,Cov);
#endif
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
#if 0
  const double outlierProb = -0.1;
  const double outlierRange = 2;

  const double hitSwitchProb = -0.1; // probability to give hits to fit in wrong order (flip two hits)

  const int splitTrack = -5; //nMeasurements/2; // for track merging testing.
  const bool fullMeasurement = false; // put fit result of first tracklet as FullMeasurement into second tracklet, don't merge
#endif
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
  }
  /*if (dynamic_cast<genfit::DAF*>(fitter) != nullptr) {
    //static_cast<genfit::DAF*>(fitter)->setBetas(100, 50, 25, 12, 6, 3, 1, 0.5, 0.1);
    //static_cast<genfit::DAF*>(fitter)->setBetas(81, 8, 4, 0.5, 0.1);
    static_cast<genfit::DAF*>(fitter)->setAnnealingScheme(100, 0.1, 5);
    //static_cast<genfit::DAF*>(fitter)->setConvergenceDeltaWeight(0.0001);
    //fitter->setMaxIterations(nIter);
  }*/

  genfit::FieldManager::getInstance()->init(new genfit::StarField());
  genfit::FieldManager::getInstance()->useCache(true, 8);
  //  genfit::FieldManager::getInstance()->useCache(false, 0);
  genfit::MaterialEffects::getInstance()->init(new genfit::TGeoMaterialInterface());
  const int pdg = 211; // -13;               // particle pdg code mu+
  //  const double charge = TDatabasePDG::Instance()->GetParticle(pdg)->Charge()/(3.);
#if 0
  double maxWeight(0);
  unsigned int nTotalIterConverged(0);
  unsigned int nTotalIterNotConverged(0);
  unsigned int nTotalIterSecondConverged(0);
  unsigned int nTotalIterSecondNotConverged(0);
  unsigned int nConvergedFits(0);
  unsigned int nUNConvergedFits(0);
  unsigned int nConvergedFitsSecond(0);
  unsigned int nUNConvergedFitsSecond(0);


  CALLGRIND_START_INSTRUMENTATION;
#endif
  //========== Reference  track ======================================================================
  TVector3 pos, mom;
  TMatrixDSym covM(6);
#if 1
  const AliHLTTPCCATrackParam& caPar = tr.InnerParam();
  Double_t alpha = tr.Alpha();
  StxNodePars pars;
  StxNodeErrs errs;
  StxCAInterface::Instance().ConvertPars(caPar, alpha, pars, errs);
#endif  
  Double_t sign = ConvertCA2XYZ(tr, pos, mom, covM);
  genfit::AbsTrackRep* rep = new genfit::RKTrackRep(sign*pdg);
  if (Debug()) rep->setDebugLvl();
  //  genfit::AbsTrackRep* secondRep = 0;
  //  if (twoReps) secondRep = new genfit::RKTrackRep(sign*-211);
  genfit::MeasuredStateOnPlane stateRef(rep);
  // create track
  //  genfit::Track* secondTrack(nullptr);
  genfit::Track  fitTrack(rep, pos, mom);
  
  // smeared start state
  //  genfit::MeasuredStateOnPlane stateSmeared(rep);
  rep->setPosMomCov(stateRef, pos, mom, covM);
  // propagation direction. (-1, 0, 1) -> (backward, auto, forward).
  rep->setPropDir(1);
  if (!matFX) genfit::MaterialEffects::getInstance()->setNoEffects();
  // remember original initial state
  const genfit::StateOnPlane stateRefOrig(stateRef);
  //========== Mesurements ======================================================================
  const int NHits = tr.NHits();
  vector<SeedHit_t>        &fSeedHits = StxCAInterface::Instance().GetSeedHits();
  std::vector<genfit::eMeasurementType> measurementTypes;
  //  std::vector< std::vector<genfit::AbsMeasurement*> > measurements;
  //  genfit::AbsMeasurement* measurement = 0;
  static TString path2TPC("/HALL_1/CAVE_1/TpcRefSys_1/TPCE_1/TPGV_%d/TPSS_%d/TPAD_%d");
  const int detId(0); // detector ID
  int planeId(0); // detector plane ID
  int hitId(0); // hit ID
  if (Debug()) {
    cout << "mom\t"; mom.Print("");
    cout << "ps\t"; pos.Print("");
    cout << "NHits = " << NHits << endl;
  }
  for ( int iHit = 0; iHit < NHits; iHit++ ){ 
    const Int_t index = StxCAInterface::Instance().GetTracker()->TrackHit( tr.FirstHitRef() + iHit );
    const Int_t hId   = StxCAInterface::Instance().GetTracker()->Hit( index ).ID();
    const StTpcHit *tpcHit = fSeedHits[hId].hit;
    Int_t sector = tpcHit->sector();
    Int_t half   = (sector  - 1)/12 + 1;
    Int_t sectorVMC = (sector - 1)%12 + 1;
    Int_t rowRC = tpcHit->padrow();
    Int_t rowVMC = 0;
    Int_t NoOfInnerRows = St_tpcPadConfigC::instance()->innerPadRows(sector);
    Int_t NoOfRows = St_tpcPadConfigC::instance()->padRows(sector);
    StiHitErrorCalculator *errCalc = 0;
    if (NoOfInnerRows == 13) {
      if (rowRC <= NoOfInnerRows) {rowVMC = 3*(rowRC -  1) +  2;  errCalc = StiTpcInnerHitErrorCalculator::instance(); }
      else                        {rowVMC =   (rowRC - 14  + 41); errCalc = StiTpcOuterHitErrorCalculator::instance(); }
      if (rowVMC > 72)   rowVMC = 72;
    } else {// iTPC
      if (rowRC <= NoOfInnerRows) {
	rowVMC = rowRC + 1; 
	if (rowVMC <  2) rowVMC =  2; 
	if (rowVMC > 41) rowVMC = 41;
	errCalc = StiTPCHitErrorCalculator::instance(); 
      } else {
	rowVMC = rowRC + 3;
	if (rowVMC < 44) rowVMC = 44;
	if (rowRC > NoOfRows) rowRC = NoOfRows;
	errCalc = StiTpcOuterHitErrorCalculator::instance(); 
      }
    }
    Int_t indx[3] = {half, sectorVMC, rowVMC};
    TString path(StarVMCDetector::FormPath(path2TPC,3,indx));
    if (! gGeoManager->CheckPath(path)) {
      cout << "Illegal path " << path.Data() << endl;
      continue;
    }
    TGeoPhysicalNode *nodeP = gGeoManager->MakePhysicalNode(path);
    if (! nodeP) {
      cout << "TGeoPhysicalNode with path " << path.Data() << " does not exists" << endl;
      continue;
    }
    const TGeoHMatrix &D = *nodeP->GetMatrix();
    genfit::eMeasurementType type = genfit::Spacepoint;
    measurementTypes.push_back(type);
    Double_t xyzG[3] = {tpcHit->position().x(),tpcHit->position().y(),tpcHit->position().z()};
    Double_t xyzL[3];
    D.MasterToLocal(xyzG, xyzL);
    TVectorD HitCoords(2);
    HitCoords[0] = xyzL[1];
    HitCoords[1] = xyzL[2];
    TMatrixDSym hitCov(2);
    Double_t zL = xyzL[2] + ((TGeoBBox *)nodeP->GetVolume(-1)->GetShape())->GetDZ();
    Double_t ecross, edip;
    errCalc->calculateError(zL, pars.eta(), pars.tanl(), ecross, edip);
    hitCov(0,0) = ecross;
    hitCov(1,1) = edip;
    if (Debug()) {
      cout << path.Data() << " local xyz " << xyzL[0] << "/" << xyzL[1] << "/" << xyzL[2] <<  endl;
      tpcHit->Print();
    }
    genfit::PlanarMeasurement* measurement = new genfit::PlanarMeasurement(HitCoords, hitCov, detId, ++hitId, nullptr);
    measurement->setPlane(genfit::SharedPlanePtr(new genfit::DetPlane(TVector3(D.GetTranslation()), TVector3(D.GetRotationMatrix()+3), TVector3(D.GetRotationMatrix()+6))), ++planeId);
    fitTrack.insertPoint(new genfit::TrackPoint(measurement, nullptr));
    
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
  genfit::MeasuredStateOnPlane state = measuredPointStateI;
  TVector3 linePoint(0,0,0);
  TVector3 lineDirection(0,0,1);
  Int_t ok = 0;
  try{
    //  Double_t s = 
    rep->extrapolateToLine(state, linePoint, lineDirection);
  }
  catch(genfit::Exception& e) {
    std::cout << "Exception, fail to make DCA" << std::endl;
    std::cout << "Inner Parameters" << std::endl << "====================" << endl;
    fitTrack.getFittedState().Print();
    std::cout << "Outer Parameters" << std::endl << "====================" << endl;
    fitTrack.getFittedState(-1).Print();
    ok = -1;
  }
  if (! ok) {
    //  TVector3 pos, mom;
    TMatrixDSym cov(6,6);
    state.getPosMomCov(pos, mom, cov);
    state.Print();
  }
#if 0
  StSPtrVecTrackNode& trNodeVec = mEvent->trackNodes(); 
  StSPtrVecTrackDetectorInfo& detInfoVec = mEvent->trackDetectorInfo(); 
  StTrackDetectorInfo* detInfo = new StTrackDetectorInfo;
  // fillDetectorInfo(detInfo,kTrack,true); 
  /* 
     detInfo->setNumberOfPoints(dets[i][1],static_cast<StDetectorId>(i));
 StThreeVectorF posL(lastNode->x_g(),lastNode->y_g(),lastNode->z_g());
  detInfo->setLastPoint (posL);
  StThreeVectorF posF(fistNode->x_g(),fistNode->y_g(),fistNode->z_g());
  detInfo->setFirstPoint(posF);
  Int_t dets[kMaxDetectorId][3];
  track->AllPointCount(dets,kMaxDetectorId-1);
  for (Int_t i=1;i<kMaxDetectorId;i++) {
    if(!dets[i][0]) continue;
    gTrack->setNumberOfPossiblePoints((unsigned char)dets[i][0],(StDetectorId)i);
  }

  fillGeometry(gTrack, track, false); // inner geometry
  fillGeometry(gTrack, track, true ); // outer geometry
  fillFitTraits(gTrack, track);
  gTrack->setDetectorInfo(detInfo);
  StuFixTopoMap(gTrack);
  fillFlags(gTrack);
  if (!track->IsPrimary()) fillDca(gTrack,track);

   */
  StTrackNode* trackNode = new StTrackNode;
  trNodeVec.push_back(trackNode);
  UInt_t TrackId = trNodeVec.size();
  StGlobalTrack* gTrack = new StGlobalTrack;
  // fillTrack(gTrack,kTrack,detInfo);
  /*
   gTrack->setEncodedMethod(mStxEncoded);
  Double_t tlen = track->TrackLength();
  assert(tlen >0.0 && tlen<1000.);
  gTrack->setLength(tlen);// someone removed this, grrrr!!!!
  */
  gTrack->setKey(TrackId);
  trackNode->addTrack(gTrack);
  //	  fillTrack(gTrack,kTrack,detInfo);
	  trNodeVec.push_back(trackNode);
  

#endif
  return kStOK;
}
#if 0
#endif
// $Log: StxMaker.cxx,v $
