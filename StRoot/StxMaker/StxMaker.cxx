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
#include "StTpcHit.h"
#include "TRMatrix.h"
#include "TRVector.h"
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
  Double_t alpha = tr.Alpha();
  StxNodePars pars;
  StxNodeErrs errs;
  StxCAInterface::Instance().ConvertPars(caPar, alpha, pars, errs);
  Double_t ca = cos(alpha);
  Double_t sa = sin(alpha);
  Double_t xyzp[6];
  xyzp[0] = ca*pars.x() - sa*pars.y(); 
  xyzp[1] = sa*pars.x() + ca*pars.y(); 
  xyzp[2] =    pars.z();
  Double_t ce = TMath::Cos(pars.eta()+alpha);
  Double_t se = TMath::Sin(pars.eta()+alpha);
  Double_t sign = 1;
  if (caPar.QPt() > 0) sign = -1;
  Double_t pT = - sign/caPar.QPt();
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
  pos = TVector3(xyzp);
  mom = TVector3(xyzp+3);
  covM = TMatrixDSym(6, Cov.GetArray());
  return sign;
}
//________________________________________________________________________________
Int_t StxMaker::FitTrack(const AliHLTTPCCAGBTrack &tr) {
#if 0
  std::cout<<"fitterTests"<<std::endl;
  gRandom->SetSeed(14);


  const unsigned int nEvents = 1000;
  const unsigned int nMeasurements = 10;
  const double BField = 20.;       // kGauss
  const double momentum = 0.1;     // GeV
  const double theta = 110;         // degree
  const double thetaDetPlane = 90;         // degree
  const double phiDetPlane = 0;         // degree
  const double pointDist = 3.;      // cm; approx. distance between measurements
  const double resolution = 0.05;   // cm; resolution of generated measurements

  const double resolutionWire = 5*resolution;   // cm; resolution of generated measurements
  const TVector3 wireDir(0,0,1);
  const double skewAngle(5);
  const bool useSkew = true;
  const int nSuperLayer = 10;
  const double minDrift = 0.;
  const double maxDrift = 2;
  const bool idealLRResolution = false; // resolve the l/r ambiguities of the wire measurements
#endif
  const double outlierProb = -0.1;
  const double outlierRange = 2;

  const double hitSwitchProb = -0.1; // probability to give hits to fit in wrong order (flip two hits)

  const int splitTrack = -5; //nMeasurements/2; // for track merging testing.
  const bool fullMeasurement = false; // put fit result of first tracklet as FullMeasurement into second tracklet, don't merge

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

  const bool resort = false;
  const bool prefit = false; // make a simple Kalman iteration before the actual fit
  const bool refit  = false; // if fit did not converge, try to fit again

  const bool twoReps = false; // test if everything works with more than one rep in the tracks

  const bool checkPruning = true; // test pruning

#if 0
  const bool smearPosMom = true;     // init the Reps with smeared pos and mom
  const double chargeSwitchProb = -0.1; // probability to seed with wrong charge sign
  const double posSmear = 10*resolution;     // cm
  const double momSmear = 5. /180.*TMath::Pi();     // rad
  const double momMagSmear = 0.2;   // relative
  const double zSmearFac = 2;
#endif

  const bool matFX = true; // false;         // include material effects; can only be disabled for RKTrackRep!

  const bool debug = false;
  const bool onlyDisplayFailed = false; // only load non-converged tracks into the display

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
  if (debug)
    fitter->setDebugLvl(10);

  /*if (dynamic_cast<genfit::DAF*>(fitter) != nullptr) {
    //static_cast<genfit::DAF*>(fitter)->setBetas(100, 50, 25, 12, 6, 3, 1, 0.5, 0.1);
    //static_cast<genfit::DAF*>(fitter)->setBetas(81, 8, 4, 0.5, 0.1);
    static_cast<genfit::DAF*>(fitter)->setAnnealingScheme(100, 0.1, 5);
    //static_cast<genfit::DAF*>(fitter)->setConvergenceDeltaWeight(0.0001);
    //fitter->setMaxIterations(nIter);
  }*/

#if 0
  // init MeasurementCreator
  genfit::MeasurementCreator measurementCreator;
  measurementCreator.setResolution(resolution);
  measurementCreator.setResolutionWire(resolutionWire);
  measurementCreator.setOutlierProb(outlierProb);
  measurementCreator.setOutlierRange(outlierRange);
  measurementCreator.setThetaDetPlane(thetaDetPlane);
  measurementCreator.setPhiDetPlane(phiDetPlane);
  measurementCreator.setWireDir(wireDir);
  measurementCreator.setMinDrift(minDrift);
  measurementCreator.setMaxDrift(maxDrift);
  measurementCreator.setIdealLRResolution(idealLRResolution);
  measurementCreator.setUseSkew(useSkew);
  measurementCreator.setSkewAngle(skewAngle);
  measurementCreator.setNSuperLayer(nSuperLayer);
  measurementCreator.setDebug(debug);

  // init geometry and mag. field
  new TGeoManager("Geometry", "Geane geometry");
  TGeoManager::Import("genfitGeom.root");
  genfit::FieldManager::getInstance()->init(new genfit::ConstField(0.,0.,BField));
#endif
  genfit::FieldManager::getInstance()->init(new genfit::StarField());
  genfit::FieldManager::getInstance()->useCache(true, 8);
  genfit::MaterialEffects::getInstance()->init(new genfit::TGeoMaterialInterface());
  const int pdg = 13;               // particle pdg code
#if 0
  const double charge = TDatabasePDG::Instance()->GetParticle(pdg)->Charge()/(3.);
#endif
  // init event display
#ifndef VALGRIND
#ifdef __DISPLAY__
  genfit::EventDisplay* display = genfit::EventDisplay::getInstance();
  display->reset();
#else
  genfit::EventDisplay* display = 0;
#endif 
#endif


#ifndef VALGRIND
#if 0
  // create histograms
  gROOT->SetStyle("Plain");
  gStyle->SetPalette(1);
  gStyle->SetOptFit(1111);
  TFile *fOut = new TFile("fitterTests.root","recreate");
  TH1D *hmomRes = new TH1D("hmomRes","mom res",500,-20*resolution*momentum/nMeasurements,20*resolution*momentum/nMeasurements);
  TH1D *hupRes = new TH1D("hupRes","u' res",500,-15*resolution/nMeasurements, 15*resolution/nMeasurements);
  TH1D *hvpRes = new TH1D("hvpRes","v' res",500,-15*resolution/nMeasurements, 15*resolution/nMeasurements);
  TH1D *huRes = new TH1D("huRes","u res",500,-15*resolution, 15*resolution);
  TH1D *hvRes = new TH1D("hvRes","v res",500,-15*resolution, 15*resolution);

  TH1D *hqopPu = new TH1D("hqopPu","q/p pull",200,-6.,6.);
  TH1D *pVal = new TH1D("pVal","p-value",100,0.,1.00000001);
  pVal->SetMinimum(0);
  TH1D *hupPu = new TH1D("hupPu","u' pull",200,-6.,6.);
  TH1D *hvpPu = new TH1D("hvpPu","v' pull",200,-6.,6.);
  TH1D *huPu = new TH1D("huPu","u pull",200,-6.,6.);
  TH1D *hvPu = new TH1D("hvPu","v pull",200,-6.,6.);

  TH1D *weights = new TH1D("weights","Daf vs true weights",500,-1.01,1.01);

  TH1D *trackLenRes = new TH1D("trackLenRes","(trueLen - FittedLen) / trueLen",500,-0.01,0.01);
#endif
#endif

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

  genfit::Track* fitTrack(nullptr);
  genfit::Track* secondTrack(nullptr);
  //========== Reference  track ======================================================================
  TVector3 pos, mom;
  TMatrixDSym covM(6);
  Double_t sign = ConvertCA2XYZ(tr, pos, mom, covM);
  genfit::AbsTrackRep* rep = new genfit::RKTrackRep(sign*pdg);
  genfit::AbsTrackRep* secondRep = 0;
  if (twoReps) secondRep = new genfit::RKTrackRep(sign*-211);
  genfit::MeasuredStateOnPlane stateRef(rep);
  // smeared start state
  //  genfit::MeasuredStateOnPlane stateSmeared(rep);
  rep->setPosMomCov(stateRef, pos, mom, covM);
  //rep->setPropDir(1);
  if (!matFX) genfit::MaterialEffects::getInstance()->setNoEffects();
  // remember original initial state
  const genfit::StateOnPlane stateRefOrig(stateRef);
  //========== Mesurements ======================================================================
  const int NHits = tr.NHits();
  vector<SeedHit_t>        &fSeedHits = StxCAInterface::Instance().GetSeedHits();
  std::vector<genfit::eMeasurementType> measurementTypes;
  std::vector< std::vector<genfit::AbsMeasurement*> > measurements;
  genfit::AbsMeasurement* measurement = 0;
  for ( int iHit = NHits-1; iHit >= 0; iHit-- ){ 
    const int index = StxCAInterface::Instance().GetTracker()->TrackHit( tr.FirstHitRef() + iHit );
    const int hId   = StxCAInterface::Instance().GetTracker()->Hit( index ).ID();
    const StTpcHit *tpcHit = fSeedHits[hId].hit;
    genfit::eMeasurementType type = genfit::Spacepoint;
    measurementTypes.push_back(type);
    TVector3 hitPos(tpcHit->position().xyz());
    TMatrixDSym cov(3);
    
  }
  return kStOK;
}
#ifdef  MYCOP
      // true start values
      TVector3 pos(0, 0, 0);
      TVector3 mom(1.,0,0);
      mom.SetPhi(gRandom->Uniform(0.,2*TMath::Pi()));
      //mom.SetTheta(gRandom->Uniform(0.5*TMath::Pi(),0.9*TMath::Pi()));
      mom.SetTheta(theta*TMath::Pi()/180);
      mom.SetMag(momentum);
      TMatrixDSym covM(6);
      for (int i = 0; i < 3; ++i)
        covM(i,i) = resolution*resolution;
      for (int i = 3; i < 6; ++i)
        covM(i,i) = pow(resolution / nMeasurements / sqrt(3), 2);

      if (debug) {
        std::cout << "start values \n";
        pos.Print();
        mom.Print();
      }

      // calc helix parameters
      genfit::HelixTrackModel* helix = new genfit::HelixTrackModel(pos, mom, charge);
      measurementCreator.setTrackModel(helix);

      // smeared start values
      TVector3 posM(pos);
      TVector3 momM(mom);
      if (smearPosMom) {
        posM.SetX(gRandom->Gaus(posM.X(),posSmear));
        posM.SetY(gRandom->Gaus(posM.Y(),posSmear));
        posM.SetZ(gRandom->Gaus(posM.Z(),zSmearFac*posSmear));

        momM.SetPhi(gRandom->Gaus(mom.Phi(),momSmear));
        momM.SetTheta(gRandom->Gaus(mom.Theta(),momSmear));
        momM.SetMag(gRandom->Gaus(mom.Mag(), momMagSmear*mom.Mag()));
      }

      // create smeared measurements
      std::vector< std::vector<genfit::AbsMeasurement*> > measurements;

      std::vector<bool> outlierTrue;
      bool outlier;
      // true values for left right. 0 for non wire measurements
      std::vector<int> leftRightTrue;
      int lr;

      double trueLen(-1);

      try{
        for (unsigned int i=0; i<measurementTypes.size(); ++i){
          trueLen = i*pointDist;

          measurements.push_back(measurementCreator.create(measurementTypes[i], trueLen, outlier, lr));
          outlierTrue.push_back(outlier);
          leftRightTrue.push_back(lr);
        }
        assert(measurementTypes.size() == leftRightTrue.size());
        assert(measurementTypes.size() == outlierTrue.size());
      }
      catch(genfit::Exception& e){
        std::cerr<<"Exception, next track"<<std::endl;
        std::cerr << e.what();
        continue; // here is a memleak!
      }

      if (debug) std::cout << "... done creating measurements \n";


      // create track
      TVectorD seedState(6);
      TMatrixDSym seedCov(6);
#if 0
      rep->get6DStateCov(stateSmeared, seedState, seedCov);
#endif
      fitTrack = new genfit::Track(rep, seedState, seedCov); //initialized with smeared rep
      if (twoReps) {
	secondTrack = new genfit::Track(rep->clone(), seedState, seedCov); //initialized with smeared rep
        fitTrack->addTrackRep(secondRep);
        secondTrack->addTrackRep(secondRep->clone());
      }
      if (debug) fitTrack->Print("C");

      fitTrack->checkConsistency();
      //fitTrack->addTrackRep(rep->clone()); // check if everything works fine with more than one rep
#if 0
      // add measurements
      for(unsigned int i=0; i<measurements.size(); ++i){
        if (splitTrack > 0 && (int)i >= splitTrack)
          break;
        if (i>0 && hitSwitchProb > gRandom->Uniform(1.))
          fitTrack->insertPoint(new genfit::TrackPoint(measurements[i], fitTrack), -2);
        else
          fitTrack->insertPoint(new genfit::TrackPoint(measurements[i], fitTrack));

        fitTrack->checkConsistency();
        if (debug) fitTrack->Print("C");
      }

      if (splitTrack > 0 && secondTrack) {
        for(unsigned int i=splitTrack; i<measurements.size(); ++i){
          if (i>0 && hitSwitchProb > gRandom->Uniform(1.))
            secondTrack->insertPoint(new genfit::TrackPoint(measurements[i], secondTrack), -2);
          else
            secondTrack->insertPoint(new genfit::TrackPoint(measurements[i], secondTrack));

          if (debug) fitTrack->Print("C");
        }
      }
#else
#endif
      fitTrack->checkConsistency();
      if (secondTrack) secondTrack->checkConsistency();

      if (debug) fitTrack->Print();

      // do the fit
      try{
        if (debug) std::cout<<"Starting the fitter"<<std::endl;

        if (prefit) {
          genfit::KalmanFitter prefitter(1, dPVal);
          prefitter.setMultipleMeasurementHandling(genfit::weightedClosestToPrediction);
          prefitter.processTrackWithRep(fitTrack, fitTrack->getCardinalRep());
        }

        fitter->processTrack(fitTrack, resort);
        if (splitTrack > 0 && secondTrack)
          fitter->processTrack(secondTrack, resort);

        if (debug) std::cout<<"fitter is finished!"<<std::endl;
      }
      catch(genfit::Exception& e){
        std::cerr << e.what();
        std::cerr << "Exception, next track" << std::endl;
        continue;
      }

      if (splitTrack > 0 && secondTrack) {
        if (debug) fitTrack->Print("C");
        if (debug) secondTrack->Print("C");

        if (fullMeasurement) {
          genfit::FullMeasurement* fullM = new genfit::FullMeasurement(secondTrack->getFittedState());
          fitTrack->insertPoint(new genfit::TrackPoint(fullM, fitTrack));
        }
        else
          fitTrack->mergeTrack(secondTrack);

        if (debug) fitTrack->Print("C");

        try{
          if (debug) std::cout<<"Starting the fitter"<<std::endl;
          fitter->processTrack(fitTrack, resort);
          if (debug) std::cout<<"fitter is finished!"<<std::endl;
        }
        catch(genfit::Exception& e){
          std::cerr << e.what();
          std::cerr << "Exception, next track" << std::endl;
          continue;
        }
      }


      if (refit && !fitTrack->getFitStatus(rep)->isFitConverged()) {
        std::cout<<"Trying to fit again "<<std::endl;
        fitter->processTrack(fitTrack, resort);
      }



      if (debug) {
        fitTrack->Print("C");
        fitTrack->getFitStatus(rep)->Print();
      }

      fitTrack->checkConsistency();
      if (secondTrack) secondTrack->checkConsistency();

#ifndef VALGRIND
      if (display && !onlyDisplayFailed && iEvent < 1000) {
        std::vector<genfit::Track*> event;
        event.push_back(fitTrack);
        if (splitTrack > 0 && secondTrack)
          event.push_back(secondTrack);
        display->addEvent(event);
      }
      else if (onlyDisplayFailed &&
               (!fitTrack->getFitStatus(rep)->isFitConverged() ||
                fitTrack->getFitStatus(rep)->getPVal() < 0.01)) {
        // add track to event display
	display->addEvent(fitTrack);
      }
#endif


      if (fitTrack->getFitStatus(rep)->isFitConverged()) {
        nTotalIterConverged += static_cast<genfit::KalmanFitStatus*>(fitTrack->getFitStatus(rep))->getNumIterations();
        nConvergedFits += 1;
      }
      else {
        nTotalIterNotConverged += static_cast<genfit::KalmanFitStatus*>(fitTrack->getFitStatus(rep))->getNumIterations();
        nUNConvergedFits += 1;
      }

      if (twoReps && secondRep) {
        if (fitTrack->getFitStatus(secondRep)->isFitConverged()) {
          nTotalIterSecondConverged += static_cast<genfit::KalmanFitStatus*>(fitTrack->getFitStatus(secondRep))->getNumIterations();
          nConvergedFitsSecond += 1;
        }
        else {
          nTotalIterSecondNotConverged += static_cast<genfit::KalmanFitStatus*>(fitTrack->getFitStatus(secondRep))->getNumIterations();
          nUNConvergedFitsSecond += 1;
        }
      }


      // check if fit was successful
      if (! fitTrack->getFitStatus(rep)->isFitConverged()) {
        std::cout << "Track could not be fitted successfully! Fit is not converged! \n";
        continue;
      }


      genfit::TrackPoint* tp = fitTrack->getPointWithMeasurementAndFitterInfo(0, rep);
      if (tp == nullptr) {
        std::cout << "Track has no TrackPoint with fitterInfo! \n";
        continue;
      }
      genfit::KalmanFittedStateOnPlane kfsop(*(static_cast<genfit::KalmanFitterInfo*>(tp->getFitterInfo(rep))->getBackwardUpdate()));
      if (debug) {
        std::cout << "state before extrapolating back to reference plane \n";
        kfsop.Print();
      }

      // extrapolate back to reference plane.
      try{
        rep->extrapolateToPlane(kfsop, stateRefOrig.getPlane());;
      }
      catch(genfit::Exception& e){
        std::cerr<<"Exception, next track"<<std::endl;
        std::cerr << e.what();
        continue;
      }

#ifndef VALGRIND
      // calculate pulls
      const TVectorD& referenceState = stateRefOrig.getState();

      const TVectorD& state = kfsop.getState();
      const TMatrixDSym& cov = kfsop.getCov();

      double pval = fitter->getPVal(fitTrack, rep);
      //assert( fabs(pval - static_cast<genfit::KalmanFitStatus*>(fitTrack->getFitStatus(rep))->getBackwardPVal()) < 1E-10 );
#if 0
      hmomRes->Fill( (charge/state[0]-momentum));
      hupRes->Fill(  (state[1]-referenceState[1]));
      hvpRes->Fill(  (state[2]-referenceState[2]));
      huRes->Fill(   (state[3]-referenceState[3]));
      hvRes->Fill(   (state[4]-referenceState[4]));

      hqopPu->Fill( (state[0]-referenceState[0]) / sqrt(cov[0][0]) );
      pVal->Fill(   pval);
      hupPu->Fill(  (state[1]-referenceState[1]) / sqrt(cov[1][1]) );
      hvpPu->Fill(  (state[2]-referenceState[2]) / sqrt(cov[2][2]) );
      huPu->Fill(   (state[3]-referenceState[3]) / sqrt(cov[3][3]) );
      hvPu->Fill(   (state[4]-referenceState[4]) / sqrt(cov[4][4]) );
#endif

      try {
        trackLenRes->Fill( (trueLen - fitTrack->getTrackLen(rep)) / trueLen );

        if (debug) {
          std::cout << "true track length = " << trueLen << "; fitted length = " << fitTrack->getTrackLen(rep) << "\n";
          std::cout << "fitted tof = " << fitTrack->getTOF(rep) << " ns\n";
        }
      }
      catch (genfit::Exception& e) {
        std::cerr << e.what();
        std::cout << "could not get TrackLen or TOF! \n";
      }



      // check l/r resolution and outlier rejection
      if (dynamic_cast<genfit::DAF*>(fitter) != nullptr) {
        for (unsigned int i=0; i<fitTrack->getNumPointsWithMeasurement(); ++i){

          if (! fitTrack->getPointWithMeasurement(i)->hasFitterInfo(rep))
            continue;

          if (debug) {
            std::vector<double> dafWeights = dynamic_cast<genfit::KalmanFitterInfo*>(fitTrack->getPointWithMeasurement(i)->getFitterInfo(rep))->getWeights();
            std::cout << "hit " << i;
            if (outlierTrue[i]) std::cout << " is an OUTLIER";
            std::cout << " weights: ";
            for (unsigned int j=0; j<dafWeights.size(); ++j){
              std::cout << dafWeights[j] << "  ";
            }
            std::cout << "   l/r: " << leftRightTrue[i];
            std::cout << "\n";
          }
          int trueSide = leftRightTrue[i];
          if (trueSide == 0) continue; // not a wire measurement
          if (outlierTrue[i]) continue; // an outlier
          std::vector<double> dafWeightLR = dynamic_cast<genfit::KalmanFitterInfo*>(fitTrack->getPointWithMeasurement(i)->getFitterInfo(rep))->getWeights();
          if(dafWeightLR.size() != 2)
            continue;

          double weightCorrectSide, weightWrongSide;

          if (trueSide < 0) {
            weightCorrectSide = dafWeightLR[0];
            weightWrongSide =  dafWeightLR[1];
          }
          else {
            weightCorrectSide = dafWeightLR[1];
            weightWrongSide =  dafWeightLR[0];
          }
          weightWrongSide -= 1.;

          weights->Fill(weightCorrectSide);
          weights->Fill(weightWrongSide);

          if (weightCorrectSide>maxWeight) maxWeight = weightCorrectSide;
        }

        for (unsigned int i=0; i<fitTrack->getNumPointsWithMeasurement(); ++i){
          if (! fitTrack->getPointWithMeasurement(i)->hasFitterInfo(rep))
            continue;

          std::vector<double> dafWeights = dynamic_cast<genfit::KalmanFitterInfo*>(fitTrack->getPointWithMeasurement(i)->getFitterInfo(rep))->getWeights();

          if (outlierTrue[i]) { // an outlier
            for (unsigned int j=0; j<dafWeights.size(); ++j){
              weights->Fill(dafWeights[j]-1);
            }
          }
          else if (leftRightTrue[i] == 0) { // only for non wire hits
            for (unsigned int j=0; j<dafWeights.size(); ++j){
              weights->Fill(dafWeights[j]);
            }
          }
        }

      }

      if (checkPruning) { //check pruning
        //std::cout<<"\n";
        //std::cout<<"get stFirst ";
        genfit::MeasuredStateOnPlane stFirst = fitTrack->getFittedState();
        //std::cout<<"get stLast ";
        genfit::MeasuredStateOnPlane stLast = fitTrack->getFittedState(-1);

        for (unsigned int i=0; i<1; ++i) {
          genfit::Track trClone(*fitTrack);
          trClone.checkConsistency();

          bool first(false), last(false);

          TString opt("");
          try {
            if (gRandom->Uniform() < 0.5) trClone.prune("C");
            if (gRandom->Uniform() < 0.5) {
              opt.Append("F");
              first = true;
            }
            if (gRandom->Uniform() < 0.5) {
              opt.Append("L");
              last = true;
            }
            if (gRandom->Uniform() < 0.5) opt.Append("W");
            if (gRandom->Uniform() < 0.5) opt.Append("R");
            if (gRandom->Uniform() < 0.5) opt.Append("M");
            if (gRandom->Uniform() < 0.5) opt.Append("I");
            if (gRandom->Uniform() < 0.5) opt.Append("U");

            trClone.prune(opt);

            try {
              trClone.checkConsistency();
            } catch (genfit::Exception& e) {
              trClone.getFitStatus()->getPruneFlags().Print();
            }

            //std::cout<<"get stCloneFirst ";
            genfit::MeasuredStateOnPlane stCloneFirst = trClone.getFittedState();
            //std::cout<<"get stCloneLast ";
            genfit::MeasuredStateOnPlane stCloneLast = trClone.getFittedState(-1);

            if (first and ! (stFirst.getState() == stCloneFirst.getState() and stFirst.getCov() == stCloneFirst.getCov() )) {
              //std::cout<<" failed first state ";
              //stFirst.Print();
              //stCloneFirst.Print();

              if (debug)
                trClone.getFitStatus()->getPruneFlags().Print();
            }

            if (last  and ! (stLast.getState()  == stCloneLast.getState()  and stLast.getCov()  == stCloneLast.getCov() )) {
              //std::cout<<" failed last state ";
              //stLast.Print();
              //stCloneLast.Print();

              if (debug)
                trClone.getFitStatus()->getPruneFlags().Print();
            }

            if (debug) {
              std::cout<<" pruned track: ";
              trClone.Print();
            }
          }
          catch (genfit::Exception &e) {
            std::cerr << e.what();
          }
        }

      } // end check pruning

#endif

#if 0
  }// end loop over events
#endif
  delete fitTrack;
  delete secondTrack;
  delete fitter;

  CALLGRIND_STOP_INSTRUMENTATION;
  CALLGRIND_DUMP_STATS;

  std::cout<<"maxWeight = " << maxWeight << std::endl;
  std::cout<<"avg nr iterations =                     " << (double)(nTotalIterConverged + nTotalIterNotConverged)/(double)(nConvergedFits + nUNConvergedFits) << std::endl;
  std::cout<<"avg nr iterations of converged fits =   " << (double)(nTotalIterConverged)/(double)(nConvergedFits) << std::endl;
  std::cout<<"avg nr iterations of UNconverged fits = " << (double)(nTotalIterNotConverged)/(double)(nUNConvergedFits) << std::endl;
  std::cout<<"fit efficiency =                        " << (double)nConvergedFits/nEvents << std::endl;

  if (twoReps) {
    std::cout<<"second rep: \navg nr iterations =                     " << (double)(nTotalIterSecondConverged + nTotalIterSecondNotConverged)/(double)(nConvergedFitsSecond + nUNConvergedFitsSecond) << std::endl;
    std::cout<<"avg nr iterations of converged fits =   " << (double)(nTotalIterSecondConverged)/(double)(nConvergedFitsSecond) << std::endl;
    std::cout<<"avg nr iterations of UNconverged fits = " << (double)(nTotalIterSecondNotConverged)/(double)(nUNConvergedFitsSecond) << std::endl;
    std::cout<<"fit efficiency =                        " << (double)nConvergedFitsSecond/nEvents << std::endl;
  }
			      
			      
			      //std::cout<<"avg nr iterations (2nd rep) = " << (double)nTotalIterSecond/nSuccessfullFitsSecond << std::endl;
			      //std::cout<<"fit efficiency (2nd rep) = " << (double)nConvergedFitsSecond/nEvents << std::endl;


#ifndef VALGRIND

			      if (debug) std::cout<<"Draw histograms ...";
  // fit and draw histograms
  TCanvas* c1 = new TCanvas();
  c1->Divide(2,3);

  c1->cd(1);
  hmomRes->Fit("gaus");
  hmomRes->Draw();

  c1->cd(2);
  weights->Draw();

  c1->cd(3);
  hupRes->Fit("gaus");
  hupRes->Draw();

  c1->cd(4);
  hvpRes->Fit("gaus");
  hvpRes->Draw();

  c1->cd(5);
  huRes->Fit("gaus");
  huRes->Draw();

  c1->cd(6);
  hvRes->Fit("gaus");
  hvRes->Draw();

  c1->Write();

  TCanvas* c2 = new TCanvas();
  c2->Divide(2,3);

  c2->cd(1);
  hqopPu->Fit("gaus");
  hqopPu->Draw();

  c2->cd(2);
  pVal->Fit("pol1");
  pVal->Draw();
  c2->cd(3);
  hupPu->Fit("gaus");
  hupPu->Draw();

  c2->cd(4);
  hvpPu->Fit("gaus");
  hvpPu->Draw();

  c2->cd(5);
  huPu->Fit("gaus");
  huPu->Draw();

  c2->cd(6);
  hvPu->Fit("gaus");
  hvPu->Draw();

  c2->Write();

  fOut->Write();

  TCanvas* c3 = new TCanvas();
  //c3->Divide(2,3);

  c3->cd(1);
  trackLenRes->Fit("gaus");
  trackLenRes->Draw();

  c3->Write();

  if (debug) std::cout<<"... done"<<std::endl;

  // open event display
  if (display) {
    display->setOptions("ABDEFHMPT"); // G show geometry
    if (matFX) display->setOptions("ABDEFGHMPT"); // G show geometry
    display->open();
  }

#endif
#endif
// $Log: StxMaker.cxx,v $
