
// Centrality headers
#include "StCentralityAnalyzer.h"
#include "Stypes.h"
#include "Param.h"

// ROOT headers
#include "TString.h"
#include "TMath.h"

// C++ headers
#include <iostream>

ClassImp(StCentralityAnalyzer)


//________________
StCentralityAnalyzer::StCentralityAnalyzer(StPicoDstReader *reader,
                                           const Char_t* oFileName):
mOutFileName(oFileName), mOutFile(nullptr), mReader(reader), mDst(nullptr),
  mTriggerIdCollection(), mEventsPassed(0), mDebug(kFALSE),
  hRefMult(nullptr), hGRefMult(nullptr), hPrimVertNum(nullptr), hPrimVertZ(nullptr),
  hZdcAdcEast(nullptr), hZdcAdcWest(nullptr), hZdcAdcSum(nullptr), hZdcCoincidenceRate(nullptr),
  hEpdAdcEast(nullptr), hEpdAdcWest(nullptr), hEpdAdcSum(nullptr),
  hPrimVertXvsY(nullptr), hPrimVertVpdVzDiff(nullptr), hRefMultVsTofTrayMult(nullptr),
  hGRefMultVsTofTrayMult(nullptr), hRefMultVsTofMatched(nullptr), hRefMultVsBemcMatch(nullptr),
  hRefMultVsRunNumber(nullptr), hGRefMultVsRunNumber(nullptr), hTofTrayMultVsRunNumber(nullptr),
  hRefMultVsZdcCoincidenceRate(nullptr),hZdcAdcSumVsRunNumber(nullptr), hPrimTrackPtVsRunNumber(nullptr),
  hPrimTrackNHitsVsRunNumber(nullptr), hPrimTrackDedxVsRunNumber(nullptr),
  hGlobTrackPtVsRunNumber(nullptr), hGlobTrackNHitsVsRunNumber(nullptr),
  hGlobTrackDedxVsRunNumber(nullptr), hAvgRefMultVsZdcCoincidenceRate(nullptr) {

  mVtxZ[0] = -70.; mVtxZ[1] = 70.;
  mVtxR[0] = 0.; mVtxR[1] = 2.;
  mVtxXShift = 0.; mVtxYShift = 0.;
  mVpdVzDiff[0] = -5.; mVpdVzDiff[1] = 5.;
  mRefMult[0] = 0.; mRefMult[1] = 1000.;
  // Next numbers are for isobar runs
  mRunIdBins = 57990;
  mRunIdRange[0] = 19071030; mRunIdRange[1] = 19129020;
  mMom[0] = 0.1; mMom[1] = 1e9;
  mEta[0] = -1.; mEta[1] = 1.;
  mNHits[0] = 15; mNHits[1] = 100;
  mNHitsRatio[0] = 0.; mNHitsRatio[1] = 1.1;
}

//________________
StCentralityAnalyzer::~StCentralityAnalyzer() {
  /* empty */
}

//________________
Int_t StCentralityAnalyzer::Init() {
  if ( !mReader ) {
    std::cout << "No StPicoDstReader has been provided" << std::endl;
    return kStErr;
  }

  mDst =  mReader->picoDst();
  if ( !mDst ) {
    std::cout << "No StPicoDst has been retrieved from reader" << std::endl;
    return kStErr;
  }

  mOutFile = new TFile(mOutFileName, "recreate");
  createHistograms();

  return StMaker::Init();
}

//________________
Int_t StCentralityAnalyzer::Finish() {
  if( mOutFile ) {
    std::cout << "StCentralityAnalyzer::Finish - Writing histograms to the output file...";
    mOutFile->Write();
    mOutFile->Close();
    std::cout << "\t [DONE]" << std::endl;
  }

  return kStOK;
}

//________________
Int_t StCentralityAnalyzer::Make() {

  mEventsPassed++;
  Bool_t readEvent = mReader->readPicoEvent(mEventsPassed);
  //std::cout << "Working on event# " << mEventsPassed << std::endl;

  StPicoEvent* event = mDst->event();
  TRandom3* rndm3 = new TRandom3(0);

  if ( !event ) {
    std::cout << "No StPicoEvent has been retrieved from DST" << std::endl;
    return kStSkip;
  }

  // Skip bad runs
  if ( isInBadRunList( event->runId() ) ) return kStOk;
     
  // Skip bad events
  if ( !isGoodEvent(event) ) return kStOk;

  //std::cout << "Event vtxZ: " << event->primaryVertex().Z() << std::endl;
  double VtxX = event->primaryVertex().X();
  double VtxY = event->primaryVertex().Y();
  double VtxZ = event->primaryVertex().Z();
  double VzVpd = event->vzVpd();

  // Fill event information
  Double_t refMult = event->refMult();
  
  // Luminosity correction
  if(mUseLumCorr){
    Double_t thislumcorr = calculateLumCorr(event->ZDCx());
    double zeroToOne = rndm3->Rndm();
    double nonIntegerRefMult = (Double_t)(refMult)-0.5+zeroToOne; 
    refMult = TMath::Nint(nonIntegerRefMult*thislumcorr);
  }

  //fill tree
  mTree_Vz      = VtxZ;
  mTree_ZDCx    = event->ZDCx();
  mTree_refMult = refMult;

  //correct refMult Vz dependence
  if(mUseVzCorr){
    refMult = getRefMultCorrVz(refMult, VtxZ);
    mTree_refMultCor = refMult; 
  }
  mTree_nBTOFMatched = event->nBTOFMatch();
  mTree->Fill();
  
  //RefMult shape weight
  //Double weight = getShapeWeight(VtxZ, refMult);
  //it should be used like hRefMult->Fill(refMult, weight);

  Int_t grefMult = event->grefMult();

  double VtxZBinDouble = VtxZ/2. + 36.;  //change this if Vz histogram labels change
  if(VtxZ==73.0) hRefMultVtxZ[72]->Fill(refMult); //edge case 
  else if(VtxZ==-73.0) hRefMultVtxZ[0]->Fill(refMult); //edge case
  else if(VtxZBinDouble - (int)VtxZBinDouble == 0.5){ //more likely edge case, filling both hists if Vz is on boundary
     hRefMultVtxZ[(int)VtxZBinDouble]->Fill(refMult);
     hRefMultVtxZ[(int)VtxZBinDouble+1]->Fill(refMult);
  }
  else hRefMultVtxZ[TMath::Nint(VtxZBinDouble)]->Fill(refMult); //most events by far

  hPrimVertXvsY->Fill(VtxX, VtxY);
  hPrimVertZ->Fill(VtxZ);
  hPrimVertVpdVzDiff->Fill(VtxZ-VzVpd);

  // Will be faster than direct call in case of simultanious usage
  UInt_t runId = event->runId();

  hRefMult->Fill( refMult );
  hGRefMult->Fill( grefMult );
  hRefMultVsRunNumber->Fill( runId, refMult );
  hRefMultVsTofTrayMult->Fill( refMult, event->btofTrayMultiplicity() );
  hRefMultVsTofMatched->Fill( event->nBTOFMatch() , refMult );
  hGRefMultVsTofTrayMult->Fill( grefMult, event->btofTrayMultiplicity() );
  hGRefMultVsTofMatched->Fill( grefMult, event->nBTOFMatch() );
  hRefMultVsRunNumber->Fill( runId, refMult );
  hGRefMultVsRunNumber->Fill( runId, grefMult );
  hRefMultVsZdcCoincidenceRate->Fill(event->ZDCx(),refMult);
  hAvgRefMultVsZdcCoincidenceRate->Fill(event->ZDCx(), refMult);
  hZdcCoincidenceRateVsRunNumber->Fill( runId, event->ZDCx() );
  hTofTrayMultVsRunNumber->Fill( runId, event->btofTrayMultiplicity() );
  hTofMatchedVsRunNumber->Fill( runId, event->nBTOFMatch() );
  
  std::vector<unsigned int> TheseTrigs = event->triggerIds();
  for(int jTrigs=0; jTrigs<TheseTrigs.size(); jTrigs++){
    int thisTrig = TheseTrigs[jTrigs];
    if(thisTrig==600001) hAvgRefMultVsZdcCoincidenceRateForTrig[0]->Fill(event->ZDCx(), refMult);
    if(thisTrig==600011) hAvgRefMultVsZdcCoincidenceRateForTrig[1]->Fill(event->ZDCx(), refMult);
    if(thisTrig==600021) hAvgRefMultVsZdcCoincidenceRateForTrig[2]->Fill(event->ZDCx(), refMult);
    if(thisTrig==600031) hAvgRefMultVsZdcCoincidenceRateForTrig[3]->Fill(event->ZDCx(), refMult);
  }

  for ( UInt_t iTrk=0; iTrk<mDst->numberOfTracks(); iTrk++ ) {
    StPicoTrack* track = mDst->track( iTrk );
    if ( !track ) continue;

    if ( !isGoodTrack( track ) ) continue;

    // Fill track information
    hGlobTrackNHitsVsRunNumber->Fill( runId, track->nHits() );
    hGlobTrackPtVsRunNumber->Fill( runId, track->gPt() );
  } // for ( Int_t iTrk=0; iTrk<mDst->numberOfTracks(); iTrk++ )

  return kStOK;
}

//________________
void StCentralityAnalyzer::Clear(Option_t *opt) {
  StMaker::Clear();
}

//________________
//Informed by https://drupal.star.bnl.gov/STAR/system/files/Centrality_for_Run18_27GeVAuAu_ZaochenYe_20190827.pdf
Double_t StCentralityAnalyzer::calculateLumCorr(Double_t ZDCx) {
  double f_ZDCx = m_LumCorr_a*ZDCx + m_LumCorr_b;
  double LumCorr = m_LumCorr_bprime/(f_ZDCx);
  return LumCorr;
}

//________________
Bool_t StCentralityAnalyzer::isGoodEvent(StPicoEvent* ev) {
  Bool_t goodEvent = ( isGoodVertex( ev->primaryVertex().X(),
                         ev->primaryVertex().Y(),
                         ev->primaryVertex().Z(),
                         ev->vzVpd() ) &&
		       isGoodTrigger( ev->triggerIds() ) );
  Bool_t pileUpRejected = true;
  if(mUsePileUp){
    pileUpRejected = isNotPileUp( ev->refMult(), ev->nBTOFMatch() );
  }
  return ( goodEvent && pileUpRejected );
}

//________________
void StCentralityAnalyzer::addTriggerId(const unsigned int& id) {
  Bool_t isInList = ( std::find(mTriggerIdCollection.begin(), mTriggerIdCollection.end(), id) != mTriggerIdCollection.end() );
  if ( !isInList ) {
    mTriggerIdCollection.push_back( id );
  }
}

//________________
Bool_t StCentralityAnalyzer::isNotPileUp(UShort_t refMult, UShort_t btofMatched) {
  
  //double refmultcutmode=m_a0+m_a1*(btofMatched)+m_a2*pow(btofMatched,2)+m_a3*pow(btofMatched,3)+m_a4*pow(btofMatched,4);
  double refmultcutmax = ( m_b0 + m_b1*(btofMatched) + m_b2*pow(btofMatched,2) +
			   m_b3*pow(btofMatched,3) + m_b4*pow(btofMatched,4) );
  double refmultcutmin = ( m_c0 + m_c1*(btofMatched) + m_c2*pow(btofMatched,2) +
			   m_c3*pow(btofMatched,3) + m_c4*pow(btofMatched,4) );

  return ( refMult<refmultcutmax && refMult>refmultcutmin );
}

//_________________
Bool_t StCentralityAnalyzer::isInBadRunList(unsigned int runId) {
  Bool_t isInRuRu_200GeV = ( std::find( bad_run_list_ruru_200gev.begin(), bad_run_list_ruru_200gev.end(), runId) != bad_run_list_ruru_200gev.end() );
  Bool_t isInZrZr_200GeV = ( std::find( bad_run_list_zrzr_200gev.begin(), bad_run_list_zrzr_200gev.end(), runId) != bad_run_list_zrzr_200gev.end() );

  return ( isInRuRu_200GeV || isInZrZr_200GeV );
}

//________________
Bool_t StCentralityAnalyzer::isGoodTrigger(std::vector<unsigned int> triggers) {
  Bool_t isInList = false;
  for ( unsigned int i=0; i<triggers.size(); i++ ) {
    if ( std::find(mTriggerIdCollection.begin(), mTriggerIdCollection.end(), triggers.at(i)) != mTriggerIdCollection.end() ) {
      isInList = true;
      break;
    }
  } //for ( unsigned int i=0; i<triggers.size(); i++ )
  return isInList;
}

//_________________
Bool_t StCentralityAnalyzer::isGoodVertex(Float_t x, Float_t y, Float_t z, Float_t vpdVz) {
  Bool_t mIsGoodPositionZ = false;
  Bool_t mIsGoodPositionR = false;
  Bool_t mIsGoodVpdVzDiff = false;

  Float_t vpdVzDiff = z - vpdVz;
  Float_t vtxPositionR = TMath::Sqrt( (x-mVtxXShift)*(x-mVtxXShift) +
				      (y-mVtxYShift)*(y-mVtxYShift) );

  mIsGoodPositionZ = ( (z > mVtxZ[0]) && (z < mVtxZ[1]) );
  mIsGoodPositionR = ( vtxPositionR >= mVtxR[0] && vtxPositionR < mVtxR[1] );
  mIsGoodVpdVzDiff = ( vpdVzDiff > mVpdVzDiff[0] && vpdVzDiff < mVpdVzDiff[1] );

  if(mDebug) {
    Bool_t isGoodVertex = ( mIsGoodPositionZ && mIsGoodPositionR && mIsGoodVpdVzDiff );
    std::cout << "IsGoodVertex: " << isGoodVertex << " : " << std::endl;
    std::cout << "position z: " << z << " IsGood: " << mIsGoodPositionZ << std::endl
	      << "position r: " << vtxPositionR << " IsGood: " << mIsGoodPositionR << std::endl
	      << "vpdvz diff: " << vpdVzDiff << " IsGood: " << mIsGoodVpdVzDiff << std::endl;
  } //if(mDebug)

  return ( mIsGoodPositionZ && mIsGoodPositionR && mIsGoodVpdVzDiff );
}

//________________
Bool_t StCentralityAnalyzer::isGoodTrack(StPicoTrack *trk) {
  Bool_t mGoodTrk = false;
  Float_t hitRatio = ( (float)trk->nHitsFit() / (float)trk->nHitsMax() );
  mGoodTrk = ( (trk->gPtot() >= mMom[0]) && (trk->gPtot() <= mMom[1]) &&
	       (trk->gPt() >= 0.1) &&
	       (trk->gMom().Eta() >= mEta[0]) &&
	       (trk->gMom().Eta() <= mEta[1]) &&
	       (trk->nHits() >= mNHits[0]) &&
	       (trk->nHits() <= mNHits[1]) &&
	       (hitRatio >= mNHitsRatio[0]) &&
	       (hitRatio <= mNHitsRatio[1]) );

  if(mDebug) {
    std::cout << "IsGoodTrack: " << mGoodTrk << " : " << std::endl
	      << "primary : " << trk->isPrimary() << std::endl
	      << "gPtot   : " << trk->gPtot() << std::endl
	      << "gPt     : " << trk->gPt() << std::endl
	      << "gEta    : " << trk->gMom().Eta() << std::endl;
    if( trk->isPrimary() ) {
      std::cout << "Ptot  : " << trk->pMom().Mag() << std::endl
		<< "Pt    : " << trk->pMom().Perp() << std::endl
		<< "Eta   : " << trk->pMom().Eta() << std::endl;
    } //if( trk->isPrimary() )

    std::cout << "nhits   : " << trk->nHits() << std::endl
	      << "hitRatio: " << hitRatio << std::endl;
  } //if(mDebug)
  return mGoodTrk;
}

//________________
void StCentralityAnalyzer::createHistograms() {
  createEventHistograms();
  createGlobalTrackHistograms();
  createPrimaryTrackHistograms();
}

//________________
void StCentralityAnalyzer::createEventHistograms() {
  std::cout << "Creating event histograms...";

  mTree = new TTree("mTree","backup tree for refMult correction");
  mTree->Branch("ZDCx",         &mTree_ZDCx,         "ZDCx/D");
  mTree->Branch("Vz",           &mTree_Vz,           "Vz/D");
  mTree->Branch("refMult",      &mTree_refMult,      "refMult/D");
  mTree->Branch("refMultCor",   &mTree_refMultCor,   "refMultCor/D");
  mTree->Branch("nBTOFMatched", &mTree_nBTOFMatched, "nBTOFMatched/D");

  Int_t refMultBins = 500;
  Float_t refMult[2] = { 0.0, 500.0 }; //refMult hist should have bin edges on integer values in order to make centrality cuts exact
  hRefMult = new TH1F( Form("hRefMult"),
		       Form("refMult;refMult;events"),
		       refMultBins, refMult[0], refMult[1]);
  for ( Int_t iBin=0; iBin<73; iBin++ ) {
    Float_t zmin = -73.;
    Float_t zstep = 2.;
    Float_t zrange[2] = { zmin + iBin*zstep, zmin + (iBin+1)*zstep};
    hRefMultVtxZ[iBin] = new TH1F( Form("hRefMultVtxZ_%d",iBin),
				   Form("Reference multiplicity for %3.1f #leq z #leq %3.1f",zrange[0],zrange[1]),
				   refMultBins, refMult[0], refMult[1] );
  } // for ( Int_t iBin=0; iBin<73; iBin++ )
  hGRefMult = new TH1F( Form("hGRefMult"),
			Form("gRefMult;gRefMult;events"),
			2500, 0.0, 2500.0);
  hPrimVertNum = new TH1F( Form("hPrimVertNum"),
			   Form("hPrimVertNum;Number of pVtx; events"),
			   15, -0.5, 14.5);
  hPrimVertZ = new TH1F( Form("hPrimVertZ"), Form("hPrimVertZ;z (cm);events/4 cm"), 110, -220., 220.);
  hPrimVertXvsY = new TH2F( Form("hPrimVertXvsY"),
			    Form("hPrimVertXvsY;x (cm);y (cm);events"),
			    40, -10., 10., 40, -10., 10.);
  hPrimVertVpdVzDiff = new TH1F( Form("hPrimVertVpdVzDiff"),
				 Form("hPrimVertVpdVzDiff;Vz_{TPC}-Vz_{VPD} (cm);events"),
				 40, -20., 20.);
  hRefMultVsTofTrayMult = new TH2F( Form("hRefMultVsTofTrayMult"),
				    Form("hRefMultVsTofTrayMult;refMult;btofTrayMultiplicity;events"),
				    650, -0.5, 649.5, 650, -0.5, 649.5 );
  hRefMultVsTofMatched = new TH2F( Form("hRefMultVsTofMatched"),
				   Form("hRefMultVsTofTrayMult;btofMatched;refMult;events"),
				   650, -0.5, 649.5, 650, -0.5, 649.5 );
  hGRefMultVsTofTrayMult = new TH2F( Form("hGRefMultVsTofTrayMult"),
				     Form("hGRefMultVsTofTrayMult;gRefMult;btofTrayMultiplicity;events"),
				     1950, -0.5, 1949.5,
				     650, -0.5, 649.5);
  hGRefMultVsTofMatched = new TH2F( Form("hGRefMultVsTofMatched"),
				    Form("hGRefMultVsTofTrayMult;gRefMult;BTofMatched;events"),
				    1950, -0.5, 1949.5,
				    650, -0.5, 649.5);
  hRefMultVsZdcCoincidenceRate = new TH2F( Form("hRefMultVsZdcCoincidenceRate"),
                                               Form("hRefMultVsZdcCoincidenceRate;ZdcCoincidenceRate (Hz);refMult"),
                                               700, 6000., 15000.,650,-0.5,649.5);
  hRefMultVsRunNumber = new TProfile( Form("hRefMultVsRunNumber"),
				      Form("hRefMultVsRunNumber;runId;<refMult>"),
				      mRunIdBins, mRunIdRange[0], mRunIdRange[1] );
  hAvgRefMultVsZdcCoincidenceRate = new TProfile( Form("hAvgRefMultVsZdcCoincidenceRate"),
                                               Form("hAvgRefMultVsZdcCoincidenceRate;ZdcCoincidenceRate (Hz);<refMult>"),
                                               700, 6000., 15000.);
  int Trigs[4]={600001,600011,600021,600031};
  for(int iTrig=0; iTrig<4; iTrig++){
      hAvgRefMultVsZdcCoincidenceRateForTrig[iTrig] = new TProfile( Form("hAvgRefMultVsZdcCoincidenceRateForTrig_%d",Trigs[iTrig]),
                                               Form("hAvgRefMultVsZdcCoincidenceRate_%d;ZdcCoincidenceRate (Hz);<refMult>",Trigs[iTrig]),
                                               700, 6000., 15000.);
  }

  hGRefMultVsRunNumber = new TProfile( Form("hGRefMultVsRunNumber"),
				       Form("hGRefMultVsRunNumber;runId;<gRefMult>"),
				       mRunIdBins, mRunIdRange[0], mRunIdRange[1] );
  hZdcCoincidenceRateVsRunNumber = new TProfile( Form("hZdcCoincidenceRateVsRunNumber"),
						 Form("hZdcCoincidenceRateVsRunNumber;runId;<ZDC coincidence rate> (kHz)"),
						 mRunIdBins, mRunIdRange[0], mRunIdRange[1] );
  hTofTrayMultVsRunNumber = new TProfile( Form("hTofTrayMultVsRunNumber"),
					  Form("hTofTrayMultVsRunNumber;runId;btofTrayMult"),
					  mRunIdBins, mRunIdRange[0], mRunIdRange[1] );
  hTofMatchedVsRunNumber = new TProfile( Form("hTofMatchedVsRunNumber"),
					 Form("hTofTrayMultVsRunNumber;runId;BTofMatched"),
					 mRunIdBins, mRunIdRange[0], mRunIdRange[1] );

  std::cout << "\t[DONE]" << std::endl;
}

//________________
void StCentralityAnalyzer::createGlobalTrackHistograms() {
  std::cout << "Creating global track histograms...";
  hGlobTrackNHitsVsRunNumber = new TProfile( Form("hGlobTrackNHitsVsRunNumber"),
					     Form("hGlobTrackNHitsVsRunNumber;runId;primary track <nHits>"),
					     mRunIdBins, mRunIdRange[0], mRunIdRange[1] );
  hGlobTrackDedxVsRunNumber = new TProfile( Form("hGlobTrackDedxVsRunNumber"),
					    Form("hGlobTrackDedxVsRunNumber;runId;primary track <dE/dx> (keV/cm)"),
					    mRunIdBins, mRunIdRange[0], mRunIdRange[1] );
  hGlobTrackPtVsRunNumber = new TProfile( Form("hGlobTrackPtVsRunNumber"),
					  Form("hGlobTrackPtVsRunNumber;runId;global track <p_T> (GeV/c)"),
					  mRunIdBins, mRunIdRange[0], mRunIdRange[1] );
  std::cout << "\t[DONE]" << std::endl;
}

//________________
void StCentralityAnalyzer::createPrimaryTrackHistograms() {
  std::cout << "Creating primary track histograms...";
  hPrimTrackPtVsRunNumber = new TProfile( Form("hPrimTrackPtVsRunNumber"),
					  Form("hPrimTrackPtVsRunNumber;runId;primary track <p_T> (GeV/c)"),
					  mRunIdBins, mRunIdRange[0], mRunIdRange[1] );
  hPrimTrackNHitsVsRunNumber = new TProfile( Form("hPrimTrackNHitsVsRunNumber"),
					     Form("hPrimTrackNHitsVsRunNumber;runId;primary track <nHits>"),
					     mRunIdBins, mRunIdRange[0], mRunIdRange[1] );
  hPrimTrackDedxVsRunNumber = new TProfile( Form("hPrimTrackDedxVsRunNumber"),
					    Form("hPrimTrackDedxVsRunNumber;runId;primary track <dE/dx> (keV/cm)"),
					    mRunIdBins, mRunIdRange[0], mRunIdRange[1] );
  std::cout << "\t[DONE]" << std::endl;
}

//_________________
const std::vector<unsigned int> StCentralityAnalyzer::bad_run_list_ruru_200gev = {
  19120009,19102023,19102054,19103022,
  19083049, 19083050, 19083051, 19083052, 19083053, 
  19083054, 19083055, 19083056, 19083057, 19083058, 
  19083059, 19083060, 19083061, 19083062, 19083063, 
  19083064, 19083065, 19083066, 19083067, 19084001, 
  19084002, 19084003, 19084004, 19084005, 19084006, 
  19084007, 19084008, 19084010, 19084011, 19084013, 
  19084022, 19084024, 19084025, 19084026, 19084027, 
  19084028, 19084029, 19084030, 19084031, 19084032, 
  19084033, 19084034, 19084035, 19084036, 19084037, 
  19084038, 19084039, 19084053, 19084055, 19084057, 
  19084059, 19084060, 19084061, 19084062, 19084063, 
  19084064, 19084065, 19084066, 19084067, 19084068, 
  19084070, 19084071, 19084072, 19085001, 19085002, 
  19085003, 19085004, 19085005, 19085006, 19085007, 
  19085008, 19085009, 19085010, 19085011, 19085012, 
  19085013, 19085014, 19085015, 19085016, 19085017, 
  19085018, 19085019, 19085020, 19085021, 19085023, 
  19085024, 19085025, 19085026, 19085058, 19086026, 
  19086060, 19086061, 19086062, 19086063, 19086064, 
  19086066, 19086067, 19086069, 19086070, 19086072, 
  19086073, 19086074, 19086076, 19086077, 19086080, 
  19087001, 19087012, 19087014, 19087015, 19087016, 
  19087017, 19087021, 19087022, 19087038, 19087042, 
  19088051, 19088052, 19088053, 19088055, 19090009, 
  19090010, 19090011, 19090012, 19090015, 19090016, 
  19090018, 19090019, 19090021, 19090022, 19090023, 
  19090024, 19090025, 19090032, 19092051, 19093042, 
  19093043, 19095061, 19096002, 19096005, 19096006, 
  19096057, 19097057, 19098017, 19098018, 19098020, 
  19100045, 19103007, 19103041, 19105024, 19105026, 
  19106023, 19106034, 19107045, 19110015, 19110039, 
  19112012, 19112029, 19115020, 19116035, 19120047, 
  19120048, 19122004, 19122005
  /* Here will be some runIds in the format:  11111111, 22222222, etc */
};

//_________________
const std::vector<unsigned int> StCentralityAnalyzer::bad_run_list_zrzr_200gev = {
  19120009,19102023,19102054,19103022,
  19083049, 19083050, 19083051, 19083052, 19083053, 
  19083054, 19083055, 19083056, 19083057, 19083058, 
  19083059, 19083060, 19083061, 19083062, 19083063, 
  19083064, 19083065, 19083066, 19083067, 19084001, 
  19084002, 19084003, 19084004, 19084005, 19084006, 
  19084007, 19084008, 19084010, 19084011, 19084013, 
  19084022, 19084024, 19084025, 19084026, 19084027, 
  19084028, 19084029, 19084030, 19084031, 19084032, 
  19084033, 19084034, 19084035, 19084036, 19084037, 
  19084038, 19084039, 19084053, 19084055, 19084057, 
  19084059, 19084060, 19084061, 19084062, 19084063, 
  19084064, 19084065, 19084066, 19084067, 19084068, 
  19084070, 19084071, 19084072, 19085001, 19085002, 
  19085003, 19085004, 19085005, 19085006, 19085007, 
  19085008, 19085009, 19085010, 19085011, 19085012, 
  19085013, 19085014, 19085015, 19085016, 19085017, 
  19085018, 19085019, 19085020, 19085021, 19085023, 
  19085024, 19085025, 19085026, 19085058, 19086026, 
  19086060, 19086061, 19086062, 19086063, 19086064, 
  19086066, 19086067, 19086069, 19086070, 19086072, 
  19086073, 19086074, 19086076, 19086077, 19086080, 
  19087001, 19087012, 19087014, 19087015, 19087016, 
  19087017, 19087021, 19087022, 19087038, 19087042, 
  19088051, 19088052, 19088053, 19088055, 19090009, 
  19090010, 19090011, 19090012, 19090015, 19090016, 
  19090018, 19090019, 19090021, 19090022, 19090023, 
  19090024, 19090025, 19090032, 19092051, 19093042, 
  19093043, 19095061, 19096002, 19096005, 19096006, 
  19096057, 19097057, 19098017, 19098018, 19098020, 
  19100045, 19103007, 19103041, 19105024, 19105026, 
  19106023, 19106034, 19107045, 19110015, 19110039, 
  19112012, 19112029, 19115020, 19116035, 19120047, 
  19120048, 19122004, 19122005
  /* Here will be some runIds in the format:  11111111, 22222222, etc */
};

//_________________
Double_t StCentralityAnalyzer::getRefMultCorrVz(Double_t RefMult, Double_t Vz){

  const Double_t RefMult_ref = m_vzCorr0; //RefMult at |z|<1.
  const Double_t RefMult_z   = m_vzCorr0 
    + m_vzCorr1*Vz         + m_vzCorr2*pow(Vz, 2) 
    + m_vzCorr3*pow(Vz, 3) + m_vzCorr4*pow(Vz, 4)
    + m_vzCorr5*pow(Vz, 5) + m_vzCorr6*pow(Vz, 6);

  Double_t ScaleFactor = 1.0;

  if(RefMult_z > 0.0){
    ScaleFactor = RefMult_ref / RefMult_z;
  }

  return RefMult * ScaleFactor;

}

//_________________
Double_t StCentralityAnalyzer::getShapeWeight(Double_t Vz, Double_t RefMult){

  // no shape correction for -9<=Vz<=9 
  if(Vz>=-9 && Vz<=9) return 1.;  
  //obtain index to load weight
  Double_t VtxZBinDouble = Vz/2. + 17.;
  Int_t VzIndex = 0;
  if(Vz == 25.) VzIndex = 29;
  else if(Vz == -35.) VzIndex = 0;
  else VzIndex = TMath::Nint(VtxZBinDouble);
  //handle VzIndex for Vz>9
  if(VzIndex >= 22) VzIndex = VzIndex - 9;
  //retrive shape weight 
  Double_t weight = ShapeWeightArray[mShapeIndex][VzIndex][TMath::Nint(RefMult)];
  //handle bad weight
  if(weight == 0 || TMath::IsNaN(weight)) weight = 1.;
  return weight;

}
