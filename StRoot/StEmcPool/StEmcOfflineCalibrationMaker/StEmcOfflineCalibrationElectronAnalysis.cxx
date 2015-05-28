/*
 * StEmcOfflineCalibrationElectronAnalysis.cxx
 * J. Kevin Adkins, University of Kentucky
 * June 24, 2014
 *
 * This will combine old methods for creating
 * ring, crate, and crateslice E/p histograms
 * for the BEMC absolute calibration.
 */

// User defined includes
#include "StEmcOfflineCalibrationElectronAnalysis.h"
#include "StEmcOfflineCalibrationEvent.h"

// STD includes
#include <map>
#include <set>
#include <stdio.h>
#include <math.h>
#include <fstream>

// ROOT includes
#include "TH1D.h"
#include "TH2F.h"
#include "TF1.h"
#include "TFile.h"
#include "TChain.h"
#include "TString.h"

// StRoot includes
#include "StEmcRawMaker/defines.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/database/StEmcDecoder.h"
#include "StEmcUtil/database/StBemcTables.h"
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"
#include "StEmcADCtoEMaker/StBemcData.h"

ClassImp(StEmcOfflineCalibrationElectronAnalysis);

StEmcOfflineCalibrationElectronAnalysis::StEmcOfflineCalibrationElectronAnalysis(const char *name, const char* outfile, const char* mipFilename, const char* geantFilename, TChain *calibChain):StMaker(name){
  mCalibChain = calibChain;
  mOutfileName = (TString)outfile;
  mipGainFilename = (TString)mipFilename;
  mGeantFilename = (TString)geantFilename;
  mFile = 0; mGeantFile = 0; mEvent = 0; mVertex = 0; mTrack = 0; mCluster = 0;
  mBHT0 = mBHT1 = mBHT2 = 0;
  mEmcGeom = 0; mEmcAdcToE = 0; mBemcTables = 0; mEmcDecoder = 0;  
}

StEmcOfflineCalibrationElectronAnalysis::~StEmcOfflineCalibrationElectronAnalysis()
{
  if (mCalibChain) delete mCalibChain; if (mFile) delete mFile; if (mGeantFile) delete mGeantFile;
  if (mEvent) delete mEvent; if (mTrack) delete mTrack; if (mVertex) delete mVertex; if (mCluster) delete mCluster;
  if (mBHT0) delete mBHT0; if (mBHT1) delete mBHT1; if (mBHT2) delete mBHT2;
  if (mEmcGeom) delete mEmcGeom; if (mEmcAdcToE) delete mEmcAdcToE; 
  if (mBemcTables) delete mBemcTables; if (mEmcDecoder) delete mEmcDecoder;
}

Int_t StEmcOfflineCalibrationElectronAnalysis::Init()
{
  pi = TMath::Pi();
  nTowers = 4800;
  nRings = 40;
  nCrates = 30;
  nSlices = 20;
  nGoodElectrons = 0;
  mCalibChain->SetBranchAddress("event_branch",&mEvent);
  mCluster = new StEmcOfflineCalibrationCluster();
  mEmcGeom = StEmcGeom::instance("bemc");
  mEmcAdcToE = static_cast<StEmcADCtoEMaker*>(this->GetMakerInheritsFrom("StEmcADCtoEMaker"));
  mBemcTables = mEmcAdcToE->getBemcData()->getTables();  
  mEmcDecoder = mBemcTables->getDecoder();

  // Geant file and fits;
  mGeantFile = new TFile(mGeantFilename);
  Char_t fitName[100];
  for (Int_t iFit = 0; iFit < 20; ++iFit){
    sprintf(fitName,"fit_%i",iFit);
    mGeantFits[iFit] = (TF1*)mGeantFile->Get(fitName);
  }

  // Read in mip gains, errors, and statuses.
  // Gainfile should be packaged with sandbox
  ifstream mipGainFile(mipGainFilename,ifstream::in);
  if (!mipGainFile.is_open()){
    cout << "Cannot open the gainfile" << endl;
    exit(1);
  }
  
  while(1){
    Int_t softId, towerStat;
    Float_t mipAdc, mipAdcErr, mipGain, towEta, towTheta;
    mipGainFile >> softId >> mipAdc >> mipAdcErr >> towerStat;
    if(!mipGainFile.good())break;
    mEmcGeom->getEta(softId, towEta);
    mEmcGeom->getTheta(softId, towTheta);
    mipGain = 0.264*(1+0.056*towEta*towEta)/(sin(towTheta)*mipAdc);
    mipGains[softId-1] = mipGain;
    mipStatus[softId-1] = towerStat;
  }
  mipGainFile.close();

  // Create TFile and initialize histograms afterwards so we may use only one TFile::Write() command
  mFile = new TFile(mOutfileName, "RECREATE");

  Char_t ringName[100], cratesliceName[100];
  Char_t ringTitle[200], cratesliceTitle[200];

  for (Int_t iRing = 0; iRing < nRings; ++iRing){
    Int_t ringId = iRing+1;
    sprintf(ringName,"ringHisto_%i",ringId);
    sprintf(ringTitle,"Ring %i E/p",ringId);
    ringHisto[iRing] = new TH1D(ringName,ringTitle, 60, 0., 3.);
    ringHisto[iRing]->Sumw2();

    sprintf(ringName,"ringHisto_Unbiased_%i",ringId);
    sprintf(ringTitle,"Ring %i E/p",ringId);
    ringHisto_Unbiased[iRing] = new TH1D(ringName,ringTitle, 60, 0., 3.);
    ringHisto_Unbiased[iRing]->Sumw2();

    sprintf(ringName,"ringHisto_HT_%i",ringId);
    sprintf(ringTitle,"Ring %i E/p",ringId);
    ringHisto_HT[iRing] = new TH1D(ringName,ringTitle, 60, 0., 3.);
    ringHisto_HT[iRing]->Sumw2();
  }

  // Initialize crate-slice E/p histograms
  for (Int_t iCrate = 0; iCrate < nCrates; ++iCrate){
    Int_t crateId = iCrate+1;
    for (Int_t iSlice = 0; iSlice < nSlices; ++iSlice){
      Int_t sliceId = iSlice+1;
      sprintf(cratesliceName,"cratesliceHisto_%i_%i",crateId,sliceId);
      sprintf(cratesliceTitle,"Crate Slice %i_%i E/p",crateId,sliceId);
      cratesliceHisto[iCrate][iSlice] = new TH1D(cratesliceName, cratesliceTitle, 60, 0., 3.);
      cratesliceHisto[iCrate][iSlice]->Sumw2();
    }
  }

  return StMaker::Init();
}

Int_t StEmcOfflineCalibrationElectronAnalysis::Make()
{
  // Get triggers for this event
  mBHT0 = mEvent->trigger(370501);
  mBHT1 = mEvent->trigger(370511);
  if (mEvent->trigger(370531))
    mBHT2 = mEvent->trigger(370531);
  else if (mEvent->trigger(370521))
    mBHT2 = mEvent->trigger(370521);
  else
    mBHT2 = mEvent->trigger(370522);

  // Get towers above HT threshold depending on which trigger fired
  towersAboveTh0.clear(); towersAboveTh1.clear(); towersAboveTh2.clear();
  towersAboveTh0 = mEvent->towersAboveHighTowerTh(0);
  towersAboveTh1 = mEvent->towersAboveHighTowerTh(1);
  towersAboveTh2 = mEvent->towersAboveHighTowerTh(2);

  // Loop through tracks to get excluded towers
  includedTowers.clear();
  excludedTowers.clear();

  // Loop over tracks to get excluded towers
  for (Int_t iTrk = 0; iTrk < mEvent->nTracks(); ++iTrk){
    mTrack = mEvent->track(iTrk);
    Int_t softId = mTrack->towerId(0);
    
    if(includedTowers.find(softId) != includedTowers.end())
      excludedTowers.insert(softId);      
    else
      includedTowers.insert(softId);
  }

  //Begin loop through vertices
  for (Int_t iVertex = 0; iVertex < mEvent->nVertices(); ++iVertex){
    mVertex = mEvent->vertex(iVertex);
    
    if (mVertex->ranking() < 1e6) continue;
    if (fabs(mVertex->z()) > 60) continue;

    // Loop through tracks at vertex
    for (Int_t iTrack = 0; iTrack < mVertex->nTracks(); ++iTrack){
      mTrack = mVertex->track(iTrack);
      /****************************************** Set some track variables ******************************************/
      towerEta = towerPhi = towerTheta = 0.;
      trackEta = trackPhi = towerTrackDr = trackEnergy = trackP = 0.;
      geantScale = 0.;
      towerCrate = towerSequence = ringIndex = sliceEtaIndex = 0;
      softId = mTrack->towerId(0);
      mEmcGeom->getEta(softId,towerEta);
      mEmcGeom->getPhi(softId,towerPhi);
      if (fabs(towerEta) > 0.965) // Outer tower Eta is 0.967, bump this up to 0.975 for calculating ringIndex correctly
	towerEta += 0.008*fabs(towerEta)/towerEta;
      mEmcDecoder->GetCrateFromTowerId(softId,towerCrate,towerSequence);
      trackEta = mTrack->eta();
      trackPhi = mTrack->phi();
      trackP = mTrack->p();
      trackEnergy = (mTrack->towerAdc(0) - mTrack->towerPedestal(0))*mipGains[softId-1];
      towerTrackDr = sqrt(mTrack->deta()*mTrack->deta() + mTrack->dphi()*mTrack->dphi());
      ringIndex = (TMath::Nint(towerEta*1000.) + 25)/50 + 19; // Ring index for eta rings
      sliceEtaIndex = (TMath::Nint(fabs(towerEta)*1000.) + 25)/50 - 1; // For getting crateslice array index
      geantScale = mGeantFits[sliceEtaIndex]->Eval(towerTrackDr);
      trackEnergy /= geantScale;
      if (sliceEtaIndex == 19)
	towerTrackDr *= 0.025/0.017;
      /**************************************************************************************************************/

      // Track cuts
      if (mTrack->p() < 1.5 || mTrack->p() > 10.) continue;
      if (mTrack->towerStatus(0) != 1) continue;
      if (mipStatus[softId-1] != 1) continue;
      if (mTrack->nHits() < 10) continue;
      if (excludedTowers.find(softId) != excludedTowers.end()) continue;
      if (mTrack->towerId(0) != mTrack->towerExitId()) continue;
      if ((mTrack->towerAdc(0) - mTrack->towerPedestal(0)) < 2.5*mTrack->towerPedestalRms(0)) continue;
      if (mTrack->dEdx() < 3.5e-6 || mTrack->dEdx() > 5.0e-6) continue;
      if (mTrack->nSigmaElectron() < -1. || mTrack->nSigmaElectron() > 2.) continue;
      if (mTrack->nSigmaPion() > -1. && mTrack->nSigmaPion() < 2.5) continue;
      if (towerTrackDr > 0.02) continue;

      // The following line matches a track to the tower which fired the HT trigger. It's left in as an example, but not used for 2012 pp200 calibration
      //if ((triggerFire(mBHT0) && trackPointsToHT(towersAboveTh0,softId)) || (triggerFire(mBHT1) && trackPointsToHT(towersAboveTh1,softId)) || (triggerFire(mBHT2) && trackPointsToHT(towersAboveTh2,softId))) continue;

      // Clear and form cluster around this track
      mCluster->Clear();
      mCluster->setCentralTrack(mTrack);
      for (Int_t iNbr = 1; iNbr < 9; ++iNbr){
	if (includedTowers.find(mTrack->towerId(iNbr)) == includedTowers.end()) continue;
	for (Int_t aTrk = 0; aTrk < mEvent->nTracks(); ++aTrk){
	  if (aTrk == iTrack) continue;
	  StEmcOfflineCalibrationTrack *nbrTrack = mEvent->track(aTrk);
	  if (nbrTrack->towerId(0) == mTrack->towerId(iNbr))
	    mCluster->addNeighborTrack(nbrTrack);
	}
      }      
      if (mCluster->numberOfNeighborTracks() > 0) continue;

      // Check that maxEt
      maxClusterEt = 0.;
      maxClusterId = 0;
      for (Int_t clustId = 0; clustId < 9; ++clustId){
	if ((mTrack->towerAdc(clustId) - mTrack->towerPedestal(clustId)) < 0) continue;
	if (mTrack->towerId(clustId) == 0) continue;
	mEmcGeom->getTheta(mTrack->towerId(clustId),towerTheta);
	Float_t holdEt = (mTrack->towerAdc(clustId) - mTrack->towerPedestal(clustId))*mipGains[mTrack->towerId(clustId)-1]*sin(towerTheta);
	if (holdEt > maxClusterEt){
	  maxClusterEt = holdEt;
	  maxClusterId = clustId;
	}
      }
      if (maxClusterId != 0) continue;
      nGoodElectrons++;

      // Only use unbiased for inner eta rings (use sliceEtaIndex to define)
      if (sliceEtaIndex <= 12 && trackP < 5. && (((!mBHT0 && !mBHT1 && !mBHT2) || (mBHT0 && !mBHT0->didFire()) || (mBHT1 && !mBHT1->didFire()) || (mBHT2 && !mBHT2->didFire())))){
	ringHisto[ringIndex]->Fill(trackEnergy/trackP);
	cratesliceHisto[towerCrate-1][sliceEtaIndex]->Fill(trackEnergy/trackP);
      }
      // Use BHT2 triggers for ALL rings if track momentum greater than 5 GeV
      else if (trackP > 5. && !triggerFire(mBHT0) && !triggerFire(mBHT1) && triggerFire(mBHT2)){
	ringHisto[ringIndex]->Fill(trackEnergy/trackP);
	cratesliceHisto[towerCrate-1][sliceEtaIndex]->Fill(trackEnergy/trackP);
      }

      // QA histograms for ALL eta ring slices
      if (trackP < 5. && (((!mBHT0 && !mBHT1 && !mBHT2) || (mBHT0 && !mBHT0->didFire()) || (mBHT1 && !mBHT1->didFire()) || (mBHT2 && !mBHT2->didFire())))){
	ringHisto_Unbiased[ringIndex]->Fill(trackEnergy/trackP);
      }
      if (trackP > 5. && !triggerFire(mBHT0) && !triggerFire(mBHT1) && triggerFire(mBHT2)){
	ringHisto_HT[ringIndex]->Fill(trackEnergy/trackP);
      }
    }// Tracks Loop
  }// Vertex Loop
  
 return kStOK;
}

Int_t StEmcOfflineCalibrationElectronAnalysis::Finish()
{
  cout << "Added " << nGoodElectrons << " electrons to the calibration histograms" << endl;
  mFile->Write(); mFile->Close(); delete mFile; 
  mGeantFile->Close(); delete mGeantFile;
  return kStOk;
}

Bool_t StEmcOfflineCalibrationElectronAnalysis::triggerFire(StEmcOfflineCalibrationTrigger *trig)
{
  return trig && trig->didFire() && trig->shouldFire();
}

Bool_t StEmcOfflineCalibrationElectronAnalysis::trackPointsToHT(const map<Int_t,Int_t>& highTowers, Int_t towId)
{
  for (map<Int_t,Int_t>::const_iterator it = highTowers.begin(); it != highTowers.end(); ++it){
    if (towId == it->first)
      return true;
  }
  
  return false;
}
