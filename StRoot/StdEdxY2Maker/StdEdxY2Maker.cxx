// $Id: StdEdxY2Maker.cxx,v 1.35 2004/07/29 22:30:19 fisyak Exp $
//#define dChargeCorrection
//#define SpaceCharge
//#define CompareWithToF
#include <Stiostream.h>		 
#include "StdEdxY2Maker.h"
// ROOT
#include "TMinuit.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH2.h"
#include "TH3.h"
#include "TStyle.h"
#include "TProfile.h"
#include "TProfile2D.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TRandom.h"
#include "TClonesArray.h"
// StUtilities
#include "StMagF.h"
#include "StDbUtilities/StMagUtilities.h"
#include "StMessMgr.h" 
#include "BetheBloch.h"
#include "StBichsel/Bichsel.h"
// St_base, StChain
#include "StBFChain.h"
// tables
#include "tables/St_dst_dedx_Table.h"
// #include "tables/St_TpcSecRowCor_Table.h"
// #include "tables/St_tpcCorrection_Table.h"
// global
#include "StDetectorId.h"
#include "StDedxMethod.h"
// StarClassLibrary
#include "StHelixD.hh"
#include "StTimer.hh"
#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif
#include "StarClassLibrary/StParticleTypes.hh"

// StDb
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StCoordinates.hh" 
#include "StTpcDb/StTpcDb.h"
// StEvent 
#include "StEventTypes.h"
#include "StProbPidTraits.h"
#ifdef CompareWithToF
#include "StTofPidTraits.h"
#endif
#include "StTpcDedxPidAlgorithm.h"
const static StPidParticle NHYPS = kPidTriton;
static Int_t tMin = 20010601;
static Int_t tMax = 20040601;
Int_t   StdEdxY2Maker::NdEdx = 0;
dEdx_t *StdEdxY2Maker::CdEdx = 0;
dEdx_t *StdEdxY2Maker::FdEdx = 0;
dEdx_t *StdEdxY2Maker::dEdxS = 0;

static  Int_t numberOfSectors;
static  Int_t numberOfTimeBins;
static  Int_t NumberOfRows;
static  Int_t NumberOfInnerRows;
static  Int_t NoPads;

const static Double_t pMomin = 0.4; // range for dE/dx calibration
const static Double_t pMomax = 0.5;
#include "dEdxTrack.h"
//______________________________________________________________________________

// QA histogramss
const static Int_t  fNZOfBadHits = 9;
static TH1D **fZOfBadHits = 0;
static TH1D *fZOfGoodHits = 0;
static TH1D *fPhiOfBadHits = 0;
static TH1D *fTracklengthInTpcTotal = 0;
static TH1D *fTracklengthInTpc = 0;

ClassImp(StdEdxY2Maker);
Bichsel *StdEdxY2Maker::m_Bichsel = 0;
//_____________________________________________________________________________
StdEdxY2Maker::StdEdxY2Maker(const char *name):
  StMaker(name), 
  m_Minuit(0), m_Mask(-1), 
  m_trigDetSums(0), m_trig(0),
  mHitsUsage(0)
{
#if 0
  SETBIT(m_Mask,StTpcdEdxCorrection::ktpcPressure); 
  SETBIT(m_Mask,StTpcdEdxCorrection::ktpcMethaneIn); 
  //  SETBIT(m_Mask,StTpcdEdxCorrection::ktpcGasTemperature); 
  //  SETBIT(m_Mask,StTpcdEdxCorrection::ktpcWaterOut); 
  SETBIT(m_Mask,StTpcdEdxCorrection::kAdcCorrection); 
  SETBIT(m_Mask,StTpcdEdxCorrection::kTpcSecRow); 
  SETBIT(m_Mask,StTpcdEdxCorrection::kDrift);
  SETBIT(m_Mask,StTpcdEdxCorrection::kzCorrection);
  //  SETBIT(m_Mask,StTpcdEdxCorrection::kdXCorrection);
  //  SETBIT(m_Mask,StTpcdEdxCorrection::kTpcdEdxCor);
  SETBIT(m_Mask,StTpcdEdxCorrection::kTpcLengthCorrection);
#endif  
  //  SETBIT(m_Mode,kOldClusterFinder); 
  SETBIT(m_Mode,kPadSelection); 
  SETBIT(m_Mode,kAlignment);
  if (!m_Minuit) m_Minuit = new TMinuit(2);
  for (int i = 0; i < 24; i++) 
    for (int j = 0; j < 45; j++) {
      mNormal[i][j] = 0;
      for (Int_t k = 0; k < 3; k++) 
	mRowPosition[i][j][k] = 0;
    }
  if (! CdEdx) CdEdx = new dEdx_t[60];
  if (! FdEdx) FdEdx = new dEdx_t[60];
  if (! dEdxS) dEdxS = new dEdx_t[60];
}
//_____________________________________________________________________________
Int_t StdEdxY2Maker::Init(){
  Int_t mode = m_Mode;
  if (m_Mode == -10 || m_Mode == 0) { // default
    //    SETBIT(m_Mode,kOldClusterFinder); 
    m_Mode = 0;
    SETBIT(m_Mode,kPadSelection); 
    if (mode == -10) SETBIT(m_Mode,kDoNotCorrectdEdx); 
  }
  if (Debug()) {
    if (! TESTBIT(m_Mode, kOldClusterFinder)) 
      gMessMgr->Warning() << "StdEdxY2Maker::Init use new Cluster Finder parameterization" << endm;
    else                  
      gMessMgr->Warning() << "StdEdxY2Maker::Init use old Cluster Finder parameterization" << endm;
    if (TESTBIT(m_Mode, kPadSelection))     
      gMessMgr->Warning() << "StdEdxY2Maker::Init Pad Selection is ON" << endm;
    if (TESTBIT(m_Mode, kDoNotCorrectdEdx))     
      gMessMgr->Warning() << "StdEdxY2Maker::Init Don't Correct dEdx" << endm;
    if (TESTBIT(m_Mode, kAlignment))     
      gMessMgr->Warning() << "StdEdxY2Maker::Init Use Sector Alignment" << endm;
  }
  if (! m_Bichsel) m_Bichsel = new Bichsel();
  
  gMessMgr->SetLimit("StdEdxY2Maker:: mismatched Sector",20);
  gMessMgr->SetLimit("StdEdxY2Maker:: pad/TimeBucket out of range:",20);
  gMessMgr->SetLimit("StdEdxY2Maker:: Helix Pediction",20);
  gMessMgr->SetLimit("StdEdxY2Maker:: Coordinates",20);
  gMessMgr->SetLimit("StdEdxY2Maker:: Prediction",20);
  gMessMgr->SetLimit("StdEdxY2Maker:: NdEdx",20);
  gMessMgr->SetLimit("StdEdxY2Maker:: Illegal time for scalers",20);
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StdEdxY2Maker::InitRun(Int_t RunNumber){
  static Int_t DoOnce = 0;
  if (!gStTpcDb) {
    cout << "Database Missing! Can't initialize StdEdxY2Maker" << endl;
    return kStFatal;
  }
  // 		TPG parameters
  numberOfSectors   = gStTpcDb->Dimensions()->numberOfSectors();
  numberOfTimeBins  = gStTpcDb->Electronics()->numberOfTimeBins();
  NumberOfRows      = gStTpcDb->PadPlaneGeometry()->numberOfRows();
  NumberOfInnerRows = gStTpcDb->PadPlaneGeometry()->numberOfInnerRows();
  NoPads = TMath::Max(gStTpcDb->PadPlaneGeometry()->numberOfPadsAtRow(NumberOfInnerRows),
		      gStTpcDb->PadPlaneGeometry()->numberOfPadsAtRow(NumberOfRows));	  
  if (! m_trigDetSums) {
    TDataSet *rich_calib  = GetDataBase("Calibrations/rich"); 
    if (! rich_calib ) gMessMgr->Error() << "StdEdxY2Maker:: Cannot find Calibrations/rich" << endm;
    else {
      m_trigDetSums = (St_trigDetSums *) rich_calib->Find("trigDetSums");
      if ( ! m_trigDetSums ) gMessMgr->Error() << "StdEdxY2Maker:: Cannot find trigDetSums in Calibrations/rich" << endm;
      else {
	if (!m_trigDetSums->GetNRows()) gMessMgr->Error() << "StdEdxY2Maker:: trigDetSums has not data" << endm;
	else {
	  m_trig = m_trigDetSums->GetTable();
	  UInt_t date = GetDateTime().Convert();
	  if (date < m_trig->timeOffset) {
	    gMessMgr->Error() << "StdEdxY2Maker:: Illegal time for scalers = " 
			      << m_trig->timeOffset << "/" << date
			      << " Run " << m_trig->runNumber << "/" << GetRunNumber() << endm;
	    m_trigDetSums = 0; m_trig = 0;
	  }
	}
      }
    }
  }
  if (! DoOnce) {
    DoOnce = 1;
    if (TESTBIT(m_Mode, kCalibration)) {// calibration mode
      if (Debug()) gMessMgr->Warning() << "StdEdxY2Maker::InitRun Calibration Mode is On (make calibration histograms)" << endm;
      StBFChain *chain = dynamic_cast<StBFChain*>(GetChain());
      TFile *f = 0;
      if (chain) {
	f = chain->GetTFile();
	if (f)     f->cd();
	if ((TESTBIT(m_Mode, kGASHISTOGRAMS))) {
	  if (Debug()) gMessMgr->Warning() << "StdEdxY2Maker::InitRun Gas Histograms is ON" << endm;
	  TrigHistos();
	}
	Histogramming();
	if ((TESTBIT(m_Mode, kSpaceChargeStudy))) SpaceCharge();
	if ((TESTBIT(m_Mode, kXYZcheck))) XyzCheck();
      }
    }
    QAPlots(0);
  }
//   if ((TESTBIT(m_Mode, kGASHISTOGRAMS))) {
//     m_tpcGainMonitor = (St_tpcGainMonitor *) GetDataBase("Conditions/tpc/tpcGainMonitor");
//   } // GASHISTOGRAMS
  if (! TESTBIT(m_Mode, kDoNotCorrectdEdx))
    m_TpcdEdxCorrection = new StTpcdEdxCorrection(m_Mask, Debug());
  StTpcCoordinateTransform transform(gStTpcDb);
  for (Int_t sector = 1; sector<= numberOfSectors; sector++) {
    for (Int_t row = 1; row <= NumberOfRows; row++) {
      if (Debug()>1) cout << "========= sector/row ========" << sector << "/" << row << endl;
      StTpcLocalSectorDirection  dirLS(0.,1.,0.,sector,row);  if (Debug()>1) cout << "dirLS\t" << dirLS << endl;
      StTpcLocalDirection        dirL;      
      if (TESTBIT(m_Mode, kAlignment)) {
	StTpcLocalSectorAlignedDirection  dirLSA;
	transform(dirLS,dirLSA);   if (Debug()>1) cout << "dirLSA\t" << dirLSA << endl;
	transform(dirLSA,dirL);                     if (Debug()>1) cout << "dirL\t" << dirL << endl;
      } else {
	transform(dirLS,dirL);                      if (Debug()>1) cout << "dirL\t" << dirL << endl;      
      }
      StGlobalDirection          dirG;
      transform(dirL,dirG);                       if (Debug()>1) cout << "dirG\t" << dirG << endl;
      SafeDelete(mNormal[sector-1][row-1]);
      mNormal[sector-1][row-1] = new StThreeVectorD(dirG.position().x(),dirG.position().y(),dirG.position().z());
      if (Debug()>1) cout << "Normal[" << sector-1 << "][" << row-1 << "] = " << *mNormal[sector-1][row-1] << endl;
      Double_t padlength;
      if (row<14) padlength = gStTpcDb->PadPlaneGeometry()->innerSectorPadLength();
      else 	  padlength = gStTpcDb->PadPlaneGeometry()->outerSectorPadLength();
      for (Int_t l = 0; l < 3; l++) {
	SafeDelete(mRowPosition[sector-1][row-1][l]); 
	Double_t y = transform.yFromRow(row);
	if (l == 1) y += padlength/2.;
	if (l == 2) y -= padlength/2.;
	StTpcLocalSectorCoordinate  lsCoord(0., y, 10.,sector,row); if (Debug()>1) cout << lsCoord << endl;
	StGlobalCoordinate  gCoord; 
	if (TESTBIT(m_Mode, kAlignment)) {
	  StTpcLocalSectorAlignedCoordinate lsCoordA;  
	  transform(lsCoord,lsCoordA);                       if (Debug()>1) cout << lsCoordA << endl;                   
	  transform(lsCoordA, gCoord);                       if (Debug()>1) cout << gCoord << endl;                   
	} else {
	  transform(lsCoord,  gCoord);                       if (Debug()>1) cout << gCoord << endl;                   
	}
	mRowPosition[sector-1][row-1][l] = 
	  new  StThreeVectorD(gCoord.position().x(),gCoord.position().y(),gCoord.position().z());
	if (Debug()>1) cout << "mRowPosition[" << sector-1 << "][" << row-1 << "][" << l << "] = " 
			    << *mRowPosition[sector-1][row-1][l] << endl;
      }
    }
  }
  return kStOK;
}
//_____________________________________________________________________________
Int_t StdEdxY2Maker::FinishRun(Int_t OldRunNumber) {
  // Move Clean up to InitRun
  
  for (int i = 0; i < 24; i++) 
    for (int j = 0; j < 45; j++) {
      SafeDelete(mNormal[i][j]);
      for (Int_t k = 0; k < 3; k++) 
	SafeDelete(mRowPosition[i][j][k]);
    }
  return StMaker::FinishRun(OldRunNumber);
}
//_____________________________________________________________________________
Int_t StdEdxY2Maker::Finish() {
  FinishRun(0);
  SafeDelete(m_TpcdEdxCorrection);
  SafeDelete(m_Minuit);
  SafeDelete(m_Bichsel);
  return StMaker::Finish();
}
//_____________________________________________________________________________
Int_t StdEdxY2Maker::Make(){ 
  static StTimer timer;
#ifdef dChargeCorrection
  static TH3D Charge("Charge","total charge for sector/row integrated  over drift length",
		     24,1.,25.,45,1.,46.,450,-15.,210.);
  static TAxis *Zax = Charge.GetZaxis();
#endif
  if (Debug() > 0) timer.start();
  St_tpcGas           *tpcGas = 0;
  if (m_TpcdEdxCorrection) {
    tpcGas = m_TpcdEdxCorrection->tpcGas();
  }
  if (TESTBIT(m_Mode, kCalibration) && tpcGas) TrigHistos(1);
  dst_dedx_st dedx;
  StTpcCoordinateTransform transform(gStTpcDb);
  Double_t bField = 0;
  StEvent* pEvent = dynamic_cast<StEvent*> (GetInputDS("StEvent"));
  if (!pEvent) {
    gMessMgr->Info() << "StdEdxY2Maker: no StEvent " << endm;
    return kStOK;        // if no event, we're done
  }
  if (pEvent->runInfo()) bField = pEvent->runInfo()->magneticField()*kilogauss;
  if ((TESTBIT(m_Mode, kSpaceChargeStudy))) SpaceCharge(1,pEvent);
  // no of tpc hits
  Int_t TotalNoOfTpcHits = 0;
  Int_t NoOfTpcHitsUsed  = 0;
  Double_t DriftDistance;
  StTpcHitCollection* TpcHitCollection = pEvent->tpcHitCollection();
#ifdef dChargeCorrection
  Charge.Reset();
#endif
  if (TpcHitCollection) {
    UInt_t numberOfSectors = TpcHitCollection->numberOfSectors();
    StTpcLocalSectorCoordinate local;
    for (UInt_t i = 0; i< numberOfSectors; i++) {
      StTpcSectorHitCollection* sectorCollection = TpcHitCollection->sector(i);
      if (sectorCollection) {
	Int_t numberOfPadrows = sectorCollection->numberOfPadrows();
	for (int j = 0; j< numberOfPadrows; j++) {
	  StTpcPadrowHitCollection *rowCollection = sectorCollection->padrow(j);
	  if (rowCollection) {
	    StSPtrVecTpcHit &hits = rowCollection->hits();
	    UInt_t NoHits = hits.size();
	    if (NoHits) {
	      TotalNoOfTpcHits += NoHits;
#if defined(dChargeCorrection) || defined(SpaceCharge)
	      if (NoHits) {
		Double_t *zL = new Double_t[NoHits];
		Int_t    *indx = new Int_t[NoHits];
		Int_t     n = 0;
		for (UInt_t k = 0; k < NoHits; k++) {
		  StTpcHit *tpcHit = static_cast<StTpcHit *> (hits[k]);
		  if (! tpcHit)  zL[k] = 9999;
		  else 		 {
		    StGlobalCoordinate global(tpcHit->position());
		    transform(global,local,i+1,j+1);
		    zL[n] = local.position().z();
#ifdef dChargeCorrection
		    Int_t sector = tpcHit->sector();
		    Int_t row    = tpcHit->padrow();
		    Charge.Fill(sector+.5,row+0.5,local.position().z(), tpcHit->charge());
#endif
		  }
		}
		TMath::Sort(NoHits, zL, indx, kFALSE);
		for (UInt_t k = 0; k < NoHits - 1; k++) {
		  StTpcHit *tpcHitk = static_cast<StTpcHit *> (hits[indx[k]]);
		  StTpcHit *tpcHitk1 = static_cast<StTpcHit *> (hits[indx[k+1]]);
		  if (tpcHitk) tpcHitk->SetNextHit(tpcHitk1);
		}
		delete [] zL;
		delete [] indx;
	      }
#endif /* dChargeCorrection || SpaceCharge */
	    }
	  }
	}
      }
    }
#ifdef dChargeCorrection
    if (Charge.GetEntries() > 0) {
      for (Int_t i = 1; i <= 24; i++) {
	for (Int_t j = 1; j <= 45; j++) {
	  Double_t Sum = 0;
	  for (Int_t k = 1; k <= 450; k++) {
	    Sum += Charge.GetBinContent(i,j,k);
	    if (Sum > 0.0) {
	      Charge.SetBinContent(i,j,k,Sum);
	    }
	  }
	}
      }
    }
#endif 
  }
  StSPtrVecTrackNode& trackNode = pEvent->trackNodes();
  UInt_t nTracks = trackNode.size();
  StTrackNode *node=0;
  for (unsigned int i=0; i < nTracks; i++) {
    node = trackNode[i]; if (!node) continue;
    StGlobalTrack  *gTrack = static_cast<StGlobalTrack *>(node->track(global));
    StPrimaryTrack *pTrack = static_cast<StPrimaryTrack*>(node->track(primary));
    StTptTrack     *tTrack = static_cast<StTptTrack    *>(node->track(tpt));
    if (Debug() > 1) {
      cout << "Track:" << i 
	   << "\ttype " << gTrack->type()
	   << "\tvertex " << gTrack->vertex()
	   << "\tkey " << gTrack->key()
	   << "\tflag " << gTrack->flag()
	   << "\tencodedMethod " << gTrack->encodedMethod()
	   << "\timpactParameter " << gTrack->impactParameter()
	   << "\tlength " << gTrack->length()
	   << "\tnumberOfPossiblePoints " << gTrack->numberOfPossiblePoints() << endl;
      StThreeVectorF pxyz = gTrack->geometry()->momentum();
      cout << "pxyz: " << pxyz << endl;
    }
    if (m_TpcdEdxCorrection) {
      // clean up old Tpc traits if any
      StTrack *track = 0;
      StTrack *tracks[3] = {gTrack, pTrack, tTrack};
      // clean old PiD traits
      for (int l = 0; l < 3; l++) {
	track = tracks[l]; 
	if (track) {
	  StSPtrVecTrackPidTraits &traits = track->pidTraits();
	  unsigned int size = traits.size();
	  if (size) {
	    for (unsigned int i = 0; i < size; i++) {
	      StDedxPidTraits *pid = dynamic_cast<StDedxPidTraits*>(traits[i]);
	      if (! pid) continue;
	      if (pid->detector() != kTpcId) continue;
	      traits[i]->makeZombie(1);
	    }
	  }
	}
      }
    }
    if (! gTrack ||  gTrack->flag() <= 0) continue;
    StPtrVecHit hvec = gTrack->detectorInfo()->hits(kTpcId);
    if (hvec.size()) {// if no hits than make only histograms. Works if kDoNotCorrectdEdx mode is set
      Int_t Id = gTrack->key();
      Int_t NoFitPoints = gTrack->fitTraits().numberOfFitPoints();
      NdEdx = 0;
      Double_t TrackLength70 = 0, TrackLength = 0;
      Double_t TrackLengthTotal = 0;
      for (unsigned int j=0; j<hvec.size(); j++) {// hit loop
	StTpcHit *tpcHit = static_cast<StTpcHit *> (hvec[j]);
	if (! tpcHit) continue;
	if (Debug() > 1) {
	  cout << "Hit: " << j << " charge: " << tpcHit->charge()
	       << " flag: " << tpcHit->flag() 
	       << " useInFit :" << tpcHit->usedInFit()
	       << " position: " << tpcHit->position() 
	       << " positionError: " << tpcHit->positionError() << endl;
	}
	Int_t sector = tpcHit->sector();
	Int_t row    = tpcHit->padrow();
	//________________________________________________________________________________      
	StThreeVectorD &normal = *mNormal[sector-1][row-1];
	const StThreeVectorD  &middle = *mRowPosition[sector-1][row-1][0];
	const StThreeVectorD  &upper  = *mRowPosition[sector-1][row-1][1];
	const StThreeVectorD  &lower  = *mRowPosition[sector-1][row-1][2];
	// check that helix prediction is consistent with measurement
	Double_t s = gTrack->geometry()->helix().pathLength(middle, normal);
	if (s > 1.e4) {BadHit(2,tpcHit->position()); continue;}
	StThreeVectorD xyzOnPlane = gTrack->geometry()->helix().at(s);
	StGlobalCoordinate globalOnPlane(xyzOnPlane.x(),xyzOnPlane.y(),xyzOnPlane.z());
	StGlobalCoordinate global(tpcHit->position());
	StTpcLocalSectorCoordinate local;
	StTpcPadCoordinate PadOnPlane; 
	StTpcPadCoordinate Pad;
	if (TESTBIT(m_Mode, kAlignment)) {
	  StTpcLocalSectorAlignedCoordinate localA;
	  transform(globalOnPlane,localA,sector,row);
	  transform(localA,local);
	  transform(local,PadOnPlane);
	  transform(global,localA,sector,row);
	  transform(localA,local);
#if 0
	  if (StMagUtilities::Instance()) {
	    Float_t pos[3] = {local.position().x(), local.position().y(), local.position().z()};
	    Float_t posMoved[3];
	    StMagUtilities::Instance()->DoDistortion(pos,posMoved);   // input pos[], returns posMoved[]
	    StThreeVector<double> postion(posMoved[0],posMoved[1],posMoved[2]);
	    local.setPosition(postion);
	  }
#endif
	  transform(local,Pad);
	} else {
	  transform(globalOnPlane,local,sector,row); 
	  transform(local,PadOnPlane);
	  transform(global,local,sector,row);
	  transform(local,Pad);
	}
	double s_out = gTrack->geometry()->helix().pathLength(upper, normal);
	if (s_out > 1.e4) {BadHit(2, tpcHit->position()); continue;}
	double s_in  = gTrack->geometry()->helix().pathLength(lower, normal);
	if (s_in > 1.e4) {BadHit(2, tpcHit->position()); continue;}
	Double_t dx = TMath::Abs(s_out-s_in);
	TrackLengthTotal += dx;
	if (! tpcHit->usedInFit()) {BadHit(0,tpcHit->position()); continue;}
	if (  tpcHit->flag()) {BadHit(1,tpcHit->position()); continue;}
	//________________________________________________________________________________      
	Int_t iokCheck = 0;
	if (sector != Pad.sector() || // ? && TMath::Abs(xyzOnPlane.x()) > 20.0 ||
	    row    != Pad.row()) {
	  gMessMgr->Warning() << "StdEdxY2Maker:: mismatched Sector " 
			      << Pad.sector() << " / " << sector
			      << " Row " << Pad.row() << " / " << row 
			      << "pad " << Pad.pad() << " TimeBucket :" << Pad.timeBucket() 
			      << " x/y/z: " << tpcHit->position()
			      << endm;
	  iokCheck++;
	}
	if (Pad.pad()    < 1             ||
	    Pad.pad()    >= NoPads       ||
	    Pad.timeBucket() < 0         ||
	    Pad.timeBucket() >= numberOfTimeBins) {
	  gMessMgr->Warning() << "StdEdxY2Maker:: pad/TimeBucket out of range: " 
			      <<  Pad.pad() << " / " << Pad.timeBucket() << endm;
	  iokCheck++;
	}
	if (sector != PadOnPlane.sector() || row != PadOnPlane.row() ||	TMath::Abs(Pad.pad()-PadOnPlane.pad()) > 5) {
	  if (Debug() > 1) {
	    gMessMgr->Warning() << "StdEdxY2Maker::	Helix Pediction " 
				<< "Sector = " 
				<< PadOnPlane.sector() << "/" 
				<< sector 
				<< " Row = " << PadOnPlane.row() << "/" 
				<< row 
				<< " Pad = " << PadOnPlane.pad() << "/" 
				<< Pad.pad() 
				<< " from Helix  is not matched with point/" << endm;;
	    gMessMgr->Warning() << "StdEdxY2Maker:: Coordinates " 
				<< xyzOnPlane << "/" << tpcHit->position()
				<< endm;
	  }
	  iokCheck++;
	}
	if ((TESTBIT(m_Mode, kXYZcheck)) && (TESTBIT(m_Mode, kCalibration))) XyzCheck(&global, iokCheck);
	if ((TESTBIT(m_Mode, kPadSelection)) && iokCheck) {BadHit(3, tpcHit->position()); continue;}
	if ((TESTBIT(m_Mode, kPadSelection)) && (dx < 0.5 || dx > 25.)) {BadHit(4, tpcHit->position()); continue;}
	StTpcdEdxCorrection::ESector kTpcOutIn = StTpcdEdxCorrection::kTpcOuter;
	if (row <= NumberOfInnerRows) kTpcOutIn = StTpcdEdxCorrection::kTpcInner;
#if 0
	StThreeVectorF pxyz = gTrack->geometry()->momentum();
	pxyz += gTrack->outerGeometry()->momentum();
	Double_t dn0 = pxyz*normal;
	dn0         /= pxyz.mag();
#endif
	Double_t padlength;
	if (row<14) padlength = gStTpcDb->PadPlaneGeometry()->innerSectorPadLength();
	else        padlength = gStTpcDb->PadPlaneGeometry()->outerSectorPadLength();
#if 0
	Double_t dx0 = padlength/dn0;
#endif
	if (CdEdx[NdEdx].row<14) DriftDistance = gStTpcDb->Dimensions()->innerEffectiveDriftDistance();
	else                     DriftDistance = gStTpcDb->Dimensions()->outerEffectiveDriftDistance();
	// Corrections
	memset (&CdEdx[NdEdx].sector, 0 , sizeof(dEdx_t));
#ifdef SpaceCharge
	// next hit
	StTpcHit *currentTpcHit = tpcHit;
	StTpcHit *NextTpcHit = 0;
	while (currentTpcHit) {
	  NextTpcHit = currentTpcHit->GetNextHit();
	  if (! NextTpcHit) break;
	}
#endif
	CdEdx[NdEdx].sector = sector; 
	CdEdx[NdEdx].row    = row;
	CdEdx[NdEdx].pad    = Pad.pad();
	CdEdx[NdEdx].Npads  = tpcHit->padsInHit();
	CdEdx[NdEdx].Ntbins = tpcHit->pixelsInHit();
	CdEdx[NdEdx].dE     = tpcHit->charge();
	CdEdx[NdEdx].dx     = dx;
#if 0
	CdEdx[NdEdx].dx0    = dx0;
#endif
	CdEdx[NdEdx].xyz[0] = global.position().x();
	CdEdx[NdEdx].xyz[1] = global.position().y();
	CdEdx[NdEdx].xyz[2] = global.position().z();
	CdEdx[NdEdx].ZdriftDistance = local.position().z();
#ifdef dChargeCorrection
	Int_t iz = Zax->FindBin(CdEdx[NdEdx].ZdriftDistance)-1;
	Double_t dCharge = 1.e6*Charge.GetBinContent(sector,row,iz);
	if (dCharge < 0.2) dCharge = 0.2;
	CdEdx[NdEdx].dCharge = TMath::Log10(dCharge);
#endif
	if (m_TpcdEdxCorrection) {
	  Int_t iok = m_TpcdEdxCorrection->dEdxCorrection(CdEdx[NdEdx]);
	  if (iok) {BadHit(5+iok, tpcHit->position()); continue;} 
	}
	TrackLength         += CdEdx[NdEdx].dx;
	if ((TESTBIT(m_Mode, kSpaceChargeStudy))) SpaceCharge(2,pEvent,&global,&CdEdx[NdEdx]);
	if (fZOfGoodHits) fZOfGoodHits->Fill(tpcHit->position().z());
	NdEdx++; NoOfTpcHitsUsed++;
	if (fTracklengthInTpcTotal) fTracklengthInTpcTotal->Fill(TrackLengthTotal);
	if (fTracklengthInTpc)      fTracklengthInTpc->Fill(TrackLength);
	if (NdEdx > NoFitPoints) 
	  gMessMgr->Error() << "StdEdxY2Maker:: NdEdx = " << NdEdx 
			    << ">  NoFitPoints ="<< NoFitPoints << endm; 
      }
      if (NdEdx <= 0 || ! m_TpcdEdxCorrection) goto HIST;
      SortdEdx(NdEdx,CdEdx,dEdxS);
#if 0
      PrintdEdx(2);
#endif
      Double_t I70 = 0, D70 = 0;
      Int_t N70 = NdEdx - (int) (0.3*NdEdx + 0.5); 
      if (N70 <= 1) continue;
      Int_t k;
      for (k = 0; k < N70; k++) {
	I70 += dEdxS[k].dEdx;
	D70 += dEdxS[k].dEdx*dEdxS[k].dEdx;
	TrackLength70 += dEdxS[k].dx;
      }
      I70 /= N70; D70 /= N70;
      D70  = TMath::Sqrt(D70 - I70*I70);
      D70 /= I70;
      dedx.id_track  =  Id;
      dedx.det_id    =  kTpcId;    // TPC track 
      dedx.method    =  kTruncatedMeanIdentifier;
      dedx.ndedx     =  N70 + 100*((int) TrackLength);
      dedx.dedx[0]   =  I70;
      dedx.dedx[1]   =  D70;
      m_TpcdEdxCorrection->dEdxTrackCorrection(StTpcdEdxCorrection::kTpcLengthCorrection,0,dedx); 
      m_TpcdEdxCorrection->dEdxTrackCorrection(StTpcdEdxCorrection::kTpcdEdxCor,0,dedx); 
      gTrack->addPidTraits(new StDedxPidTraits(dedx));
      if (pTrack) pTrack->addPidTraits(new StDedxPidTraits(dedx));
      if (tTrack) tTrack->addPidTraits(new StDedxPidTraits(dedx));
      // likelihood fit
      Double_t chisq, fitZ, fitdZ;
      DoFitZ(chisq, fitZ, fitdZ);
      if (chisq >0 && chisq < 10000.0) {
	dedx.id_track  =  Id;
	dedx.det_id    =  kTpcId;    // TPC track 
	dedx.method    =  kLikelihoodFitIdentifier;
	dedx.ndedx     =  NdEdx + 100*((int) TrackLength);
	dedx.dedx[0]   =  TMath::Exp(fitZ);
	dedx.dedx[1]   =  fitdZ; 
	m_TpcdEdxCorrection->dEdxTrackCorrection(StTpcdEdxCorrection::kTpcLengthCorrection,1,dedx); 
	m_TpcdEdxCorrection->dEdxTrackCorrection(StTpcdEdxCorrection::kTpcdEdxCor,1,dedx); 
	gTrack->addPidTraits(new StDedxPidTraits(dedx));
	if (pTrack) pTrack->addPidTraits(new StDedxPidTraits(dedx));
	if (tTrack) tTrack->addPidTraits(new StDedxPidTraits(dedx));
      }
      StThreeVectorD g3 = gTrack->geometry()->momentum(); // p of global track
      Double_t pMomentum = g3.mag();
      Float_t Chisq[NHYPS];
      for (int hyp = 0; hyp < NHYPS; hyp++) {
	Double_t bgL10 = TMath::Log10(pMomentum/StProbPidTraits::mPidParticleDefinitions[hyp]->mass());
	Chisq[hyp] = LikeliHood(bgL10,NdEdx,FdEdx);
      }
      gTrack->addPidTraits(new StProbPidTraits(NdEdx,kTpcId,NHYPS,Chisq));
      if (pTrack) pTrack->addPidTraits(new StProbPidTraits(NdEdx,kTpcId,NHYPS,Chisq));
      if (tTrack) tTrack->addPidTraits(new StProbPidTraits(NdEdx,kTpcId,NHYPS,Chisq));
      //    if (primtrkC && iprim >= 0&& (TESTBIT(m_Mode, kCalibration)) > 0) {
      //      if ((TESTBIT(m_Mode, kCalibration)) && bField && pTrack) Histogramming(gTrack);
    }
  HIST:
    if ((TESTBIT(m_Mode, kCalibration))) Histogramming(gTrack);
    QAPlots(gTrack);
  }
  if (Debug() > 1) {
    gMessMgr->QAInfo() << "StdEdxY2Maker:"
		       << "  Type: " << pEvent->type()
		       << "  Run: " << pEvent->runId() 
		       << "  Event: " << pEvent->id()
		       << "  # track nodes: "
		       << pEvent->trackNodes().size() << endm;
  }
  if (Debug()) {
    timer.stop();
    gMessMgr->QAInfo() << "CPU time for StdEdxY2Maker::Make(): "
		       << timer.elapsedTime() << " sec\n" << endm;
  }
  if (mHitsUsage) mHitsUsage->Fill(TMath::Log10(TotalNoOfTpcHits+1.), TMath::Log10(NoOfTpcHitsUsed+1.));
  return kStOK;
}
//________________________________________________________________________________
Int_t StdEdxY2Maker::SortdEdx(Int_t N, dEdx_t *dE, dEdx_t *dES) {
  int i;
  for (i = 0; i < N; i++) dES[i] = dE[i];
  for (i = 0; i < N-1; i++) {
    for (int j = i+1; j < N; j++) {
      if (dES[i].dEdx > dES[j].dEdx) {
	dEdx_t temp = dES[i];
	dES[i] = dES[j];
	dES[j] = temp;
      }
    }
  }
  return 0;
}
//________________________________________________________________________________
void StdEdxY2Maker::Histogramming(StGlobalTrack* gTrack) {
  const static Double_t GeV2keV = TMath::Log(1.e-6);
  // Histograms
  static TProfile2D *ETA = 0;
  static TH3D *ETA3 = 0;
  static TH2D *Time = 0, *TimeP = 0, *TimeC = 0;
  static TH3D *Pressure = 0, *PressureC = 0, *PressureA = 0;
  static TH3D *PressureT = 0, *PressureTC = 0, *PressureTA = 0;
  // CORRELATION
  static TH2D *corrI = 0, *corrO = 0, *corrI2 = 0, *corrO2 = 0, *corrI5 = 0, *corrO5 = 0;
  static TH2D *corrIw = 0, *corrOw = 0, *corrI2w = 0, *corrO2w = 0, *corrI5w = 0, *corrO5w = 0;
  static TH1D *corrI1w = 0, *corrO1w = 0;
  // ZBGX
  static TH3D **zbgx = 0;
  // end of ZBGX
  static TH3D *MulRow = 0;
  static TH3D *MulRowC = 0;
  //  static TH2D *GainMonitor = 0;
  static TH3D *SecRow3 = 0, *SecRow3C = 0, *SecRow3A = 0;
  static TH3D *Z3 = 0, *Z3A = 0, *Z3C = 0;
  static TH3D *Z3O = 0, *Z3OC = 0, *Z3OA = 0;
  static TH3D *Z3OW = 0, *Z3OWC = 0, *Z3OWA = 0;
#if 0
  static TH3D *Pads3 = 0, *Pads3C = 0; //  *Pads3A = 0,
  static TH3D *Tbins3 = 0,  *Tbins3C = 0;// *Tbins3A = 0,
  static TH3D *PadsTbins3 = 0, *PadsTbins3C = 0; //  *PadsTbins3A = 0,
#endif
  static TH3D *dCharge3 = 0, *dCharge3C = 0;
  // AdcHistos
  static TH2D *AdcI = 0, *AdcO = 0, *AdcIC = 0, *AdcOC = 0, *Adc3I = 0, *Adc3O = 0, *Adc3IC = 0, *Adc3OC = 0;
  static TH3D *AdcIZP = 0, *AdcOZP = 0, *AdcIZN = 0, *AdcOZN = 0;
  static TH2D **Adc3Ip = 0, **Adc3Op = 0;
  // end of AdcHistos
  static TH2D *ZdcCP = 0, *BBCP = 0, *L0P = 0, *MultiplicityPI = 0, *MultiplicityPO = 0;
  // Mip 
  static TH3D *SecRow3Mip = 0;
  // end of Mip
  static TH1D *hdEI = 0, *hdEUI = 0, *hdERI = 0, *hdEPI = 0, *hdETI = 0, *hdESI = 0, *hdEZI = 0, *hdEMI = 0;
  static TH1D *hdEO = 0, *hdEUO = 0, *hdERO = 0, *hdEPO = 0, *hdETO = 0, *hdESO = 0, *hdEZO = 0, *hdEMO = 0;
  static TH2D *Points =  0, *Points70 =  0;
  static TH2D *PointsB = 0, *Points70B = 0; 
  static TH2D *TPoints = 0, *TPoints70 = 0;
  static TH2D *TPointsB = 0, *TPoints70B = 0;
  static TH2D *hist70[NHYPS][2], *histz[NHYPS][2];
  static TH2D *hist70B[NHYPS][2], *histzB[NHYPS][2];
  static TProfile *histB[NHYPS][2], *histBB[NHYPS][2]; 
  static TH2D *FitPull = 0, *Pull70 = 0;
  static TTree *ftree = 0;
  static TH2D *ffitZ[NHYPS],  *ffitP[NHYPS], *ffitZU = 0, *ffitZU3 = 0, *ffitZA = 0;
  const static Int_t Nlog2dx = 140;
  const static Double_t log2dxLow = 0.0, log2dxHigh = 3.5;
  static TH2D *inputTPCGasPressureP = 0, *nitrogenPressureP = 0, *gasPressureDiffP = 0, *inputGasTemperatureP = 0;
  static TH2D *outputGasTemperatureP = 0, *flowRateArgon1P = 0, *flowRateArgon2P = 0;
  static TH2D *flowRateMethaneP = 0;
  static TH2D *percentMethaneInP = 0, *percentMethaneInPC = 0, *percentMethaneInPA = 0;
  static TH2D *ppmOxygenInP = 0, *flowRateExhaustP = 0;
  static TH2D *ppmOxygenOutP = 0, *flowRateRecirculationP = 0;
  static TH2D *ppmWaterOutP = 0, *ppmWaterOutPC = 0, *ppmWaterOutPA = 0;
  // ProbabilityPlot
  static TH3D *Prob = 0;
  // end of ProbabilityPlot
#if 0
  static TH2D *dx0dx = 0;
#endif
  static TH3D *dXdE  = 0, *dXdEA  = 0, *dXdEC  = 0;
  // end of CORRELATION
  static dEdxTrack *ftrack = 0;
  
  if (! gTrack) {
    // book histograms
    Int_t      nZBins = 200;
    Double_t ZdEdxMin = -5;
    Double_t ZdEdxMax =  5;
    Z3  = new TH3D("Z3",
		   "log(dEdx/Pion) versus row and Drift Distance",
		   NumberOfRows,1., NumberOfRows+1,105,0.,210.,nZBins,ZdEdxMin,ZdEdxMax);
    Z3A = new TH3D("Z3A",
		   "log(dEdx/Pion) just after corection versus row and Drift Distance",
		   NumberOfRows,1., NumberOfRows+1,105,0.,210.,nZBins,ZdEdxMin,ZdEdxMax);
    Z3C = new TH3D("Z3C",
		   "log(dEdx/Pion) corrected versus row and Drift Distance",
		   NumberOfRows,1., NumberOfRows+1,105,0.,210.,nZBins,ZdEdxMin,ZdEdxMax);
    Z3O = new TH3D("Z3O",
		   "log(dEdx/Pion) versus row and (Drift)*ppmO2In",
		   NumberOfRows,1., NumberOfRows+1,100,0.,1.e4,nZBins,ZdEdxMin,ZdEdxMax);
    Z3OA= new TH3D("Z3OA",
		   "log(dEdx/Pion) just after correction versus row and (Drift)*ppmO2In",
		   NumberOfRows,1., NumberOfRows+1,100,0.,1.e4,nZBins,ZdEdxMin,ZdEdxMax);
    Z3OC= new TH3D("Z3OC",
		   "log(dEdx/Pion) corrected versus row and (Drift)*ppmO2In",
		   NumberOfRows,1., NumberOfRows+1,100,0.,1.e4,nZBins,ZdEdxMin,ZdEdxMax);
    Z3OW = new TH3D("Z3OW",
		   "log(dEdx/Pion) versus row and (Drift)*ppmO2In*ppmWaterOut*",
		   NumberOfRows,1., NumberOfRows+1,100,0.,1.2e5,nZBins,ZdEdxMin,ZdEdxMax);
    Z3OWA= new TH3D("Z3OWA",
		   "log(dEdx/Pion) just after correction versus row and (Drift)*ppmO2In*ppmWaterOut",
		   NumberOfRows,1., NumberOfRows+1,100,0.,1.2e5,nZBins,ZdEdxMin,ZdEdxMax);
    Z3OWC= new TH3D("Z3OWC",
		   "log(dEdx/Pion) corrected versus row and (Drift)*ppmO2In*ppmWaterOut",
		   NumberOfRows,1., NumberOfRows+1,100,0.,1.2e5,nZBins,ZdEdxMin,ZdEdxMax);
#if 0
    // pads & time bins
    Pads3 = new TH3D("Pads3",
		   "log(dEdx/Pion) versus row and No. of pads",
		   NumberOfRows,1., NumberOfRows+1,30, 2., 32.,nZBins,ZdEdxMin,ZdEdxMax);
//     Pads3A= new TH3D("Pads3A",
// 		   "log(dEdx/Pion) just after correction versus row and No. of pads",
// 		   NumberOfRows,1., NumberOfRows+1,30, 2., 32.,nZBins,ZdEdxMin,ZdEdxMax);
    Pads3C= new TH3D("Pads3C",
		   "log(dEdx/Pion) corrected versus row and No. of pads",
		   NumberOfRows,1., NumberOfRows+1,30, 2., 32.,nZBins,ZdEdxMin,ZdEdxMax);
    Tbins3 = new TH3D("Tbins3",
		   "log(dEdx/Pion) versus row and No. of tbins",
		   NumberOfRows,1., NumberOfRows+1,30, 5., 35.,nZBins,ZdEdxMin,ZdEdxMax);
//     Tbins3A= new TH3D("Tbins3A",
// 		   "log(dEdx/Pion) just after correction versus row and No.of tbins",
// 		   NumberOfRows,1., NumberOfRows+1,30, 5., 35.,nZBins,ZdEdxMin,ZdEdxMax);
    Tbins3C= new TH3D("Tbins3C",
		   "log(dEdx/Pion) corrected versus row and No. of tbins",
		   NumberOfRows,1., NumberOfRows+1,30, 5., 35.,nZBins,ZdEdxMin,ZdEdxMax);
    PadsTbins3 = new TH3D("PadsTbins3",
		   "log(dEdx/Pion) versus row and No. of Pads*Tbins",
		   NumberOfRows,1., NumberOfRows+1,60, 10., 250.,nZBins,ZdEdxMin,ZdEdxMax);
//     PadsTbins3A= new TH3D("PadsTbins3A",
// 		   "log(dEdx/Pion) just after correction versus row and No.of Pads*Tbins",
// 		   NumberOfRows,1., NumberOfRows+1,60, 10., 250.,nZBins,ZdEdxMin,ZdEdxMax);
    PadsTbins3C= new TH3D("PadsTbins3C",
		   "log(dEdx/Pion) corrected versus row and No. of Pads*Tbins",
		   NumberOfRows,1., NumberOfRows+1,60, 10., 250.,nZBins,ZdEdxMin,ZdEdxMax);
#endif
    dCharge3 = new TH3D("dCharge3",
		   "log(dEdx/Pion) versus row and log10(dE (keV))",
		   NumberOfRows,1., NumberOfRows+1, 50,-1.,4.,nZBins,ZdEdxMin,ZdEdxMax);
    dCharge3C= new TH3D("dCharge3C",
		   "log(dEdx/Pion) corrected versus row and log10(dE (keV))",
		   NumberOfRows,1., NumberOfRows+1, 50,-1.,4.,nZBins,ZdEdxMin,ZdEdxMax);
    // eta
    ETA   = new TProfile2D("ETA",
			   "log(dEdx/Pion) versus Sector I/O and #{eta}",
			   NumberOfRows,1., NumberOfRows+1, 135,-2.25,2.25);
    ETA3  = new TH3D("ETA3",
		     "log(dEdx/Pion) versus Sector I/O and #{eta}",
		     NumberOfRows,1., NumberOfRows+1, 135,-2.25,2.25,nZBins,ZdEdxMin,ZdEdxMax);
    SecRow3= new TH3D("SecRow3","<log(dEdx/Pion)> (uncorrected) versus sector and row",
		      numberOfSectors,1., numberOfSectors+1, NumberOfRows,1., NumberOfRows+1,nZBins,ZdEdxMin,ZdEdxMax);
    SecRow3->SetXTitle("Sector number");
    SecRow3->SetYTitle("Row number");
    SecRow3C= new TH3D("SecRow3C","<log(dEdx/Pion)> (corrected) versus sector and row",
		       numberOfSectors,1., numberOfSectors+1, NumberOfRows,1., NumberOfRows+1,nZBins,ZdEdxMin,ZdEdxMax);
    SecRow3C->SetXTitle("Sector number");
    SecRow3C->SetYTitle("Row number");
    SecRow3A= new TH3D("SecRow3A","<log(dEdx/Pion)> (just after correction) versus sector and row",
		       numberOfSectors,1., numberOfSectors+1, NumberOfRows,1., NumberOfRows+1,nZBins,ZdEdxMin,ZdEdxMax);
    SecRow3A->SetXTitle("Sector number");
    SecRow3A->SetYTitle("Row number");
    MultiplicityPI = new TH2D("MultiplicityPI","Multiplicity (log10) Inner",100,0,10,nZBins,ZdEdxMin,ZdEdxMax);
    MultiplicityPO = new TH2D("MultiplicityPO","Multiplicity (log10) Outer",100,0,10,nZBins,ZdEdxMin,ZdEdxMax);
    if ((TESTBIT(m_Mode, kAdcHistos))) {
      gMessMgr->Warning() << "StdEdxY2Maker::Histogramming Adc Histograms" << endm;
      AdcI    = new TH2D("AdcI",
			 "log10dE (keV measured) versus log10dE(Predicted) for Inner rows",
			 120,-5.7,-3.3,185,-7.2,-3.5);
      AdcO    = new TH2D("AdcO",
			 "log10dE (keV measured) versus log10dE(Predicted) for Outer rows",
			 150,-5.5,-2.5,165,-6.5,-3.0);
      AdcIC   = new TH2D("AdcIC",
			 "log10dE (keV measured corrected) versus log10dE(Predicted) for Inner rows",
			 120,-5.7,-3.3,185,-7.2,-3.5);
      AdcOC   = new TH2D("AdcOC",
			 "log10dE (keV measured corrected) versus log10dE(Predicted) for Outer rows",
			 150,-5.5,-2.5,165,-6.5,-3.0);
      Adc3I    = new TH2D("Adc3I",
			  "Uniq 3*sigma log10dE (keV measured) versus log10dE(Predicted) for Inner rows",
			  120,-5.7,-3.3,185,-7.2,-3.5);
      Adc3O    = new TH2D("Adc3O",
			  "Uniq 3*sigma log10dE (keV measured) versus log10dE(Predicted) for Outer rows",
			  150,-5.5,-2.5,165,-6.5,-3.0);
      Adc3IC   = new TH2D("Adc3IC",
			  "Uniq 3*sigma log10dE (keV measured corrected) versus log10dE(Predicted) for Inner rows",
			  120,-5.7,-3.3,185,-7.2,-3.5);
      Adc3OC   = new TH2D("Adc3OC",
			  "Uniq 3*sigma log10dE (keV measured corrected) versus log10dE(Predicted) for Outer rows",
			  150,-5.5,-2.5,165,-6.5,-3.0);
      Adc3Ip    = new TH2D*[NHYPS];
      Adc3Op    = new TH2D*[NHYPS];
      for (int hyp = 0; hyp < NHYPS; hyp++) {
	TString nameP(StProbPidTraits::mPidParticleDefinitions[hyp]->name().data());
	nameP.ReplaceAll("-","");
	Adc3Ip[hyp] = new 
	  TH2D(Form("Adc3I%s",nameP.Data()), 
	       Form("%s Uniq 3*sigma log10dE (keV meas.cor.) versus log10dE(Predicted) for Inner rows",
		    nameP.Data()),
	       120,-5.7,-3.3,185,-7.2,-3.5);
	Adc3Op[hyp] = new 
	  TH2D(Form("Adc3O%s",nameP.Data()), 
	       Form("%s Uniq 3*sigma log10dE (keV meas.cor.) versus log10dE(Predicted) for Outer rows",
		    nameP.Data()),
	       120,-5.7,-3.3,185,-7.2,-3.5);
      }
      AdcIZP    = new TH3D("AdcIZP","z (Positive measured) versus dE(Predicted) and Z for Inner rows",
			   200,0,200,100,0.,1.e4,nZBins,ZdEdxMin,ZdEdxMax);
      AdcOZP    = new TH3D("AdcOZP","z (Positive measured) versus dE(Predicted) and Z for Outer rows",
			   500,0,500,100,0.,1.e4,nZBins,ZdEdxMin,ZdEdxMax);
      AdcIZN    = new TH3D("AdcIZN","z (Positive measured) versus dE(Predicted) and Z for Inner rows",
			   200,0,200,100,0.,1.e4,nZBins,ZdEdxMin,ZdEdxMax);
      AdcOZN    = new TH3D("AdcOZN","z (Positive measured) versus dE(Predicted) and Z for Outer rows",
			   500,0,500,100,0.,1.e4,nZBins,ZdEdxMin,ZdEdxMax);
    } // AdcHistos
    ZdcCP  = new TH2D("ZdcCP","ZdcCoincidenceRate (log10)",100,0,10,nZBins,ZdEdxMin,ZdEdxMax);
    BBCP   = new TH2D("BBCP","BbcCoincidenceRate (log10)",100,0,10,nZBins,ZdEdxMin,ZdEdxMax);
    L0P    = new TH2D("L0P","L0RateToRich (log10)",100,0,2,nZBins,ZdEdxMin,ZdEdxMax);
    if ((TESTBIT(m_Mode, kMip))) {
      gMessMgr->Warning() << "StdEdxY2Maker::Histogramming Mip Histograms" << endm;
      SecRow3Mip = new TH3D
	("SecRow3Mip",
	 "<log(dEdx/Pion)>/sigma (corrected) versus row and log2(dx) for MIP particle)",
	 NumberOfRows,1., NumberOfRows+1,Nlog2dx, log2dxLow, log2dxHigh, 200,-5,15);
    } // Mip
    MulRow = new TH3D("MulRow","log(dEdx/Pion) versus log10(Multplicity) and row",
		      100,0,10, NumberOfRows,1., NumberOfRows+1,nZBins,ZdEdxMin,ZdEdxMax);
    MulRowC = new TH3D("MulRowC","log(dEdx/Pion) versus log10(Multplicity) and row corrected",
		       100,0,10, NumberOfRows,1., NumberOfRows+1,nZBins,ZdEdxMin,ZdEdxMax);
    if ((TESTBIT(m_Mode, kCORRELATION))) {
      gMessMgr->Warning() << "StdEdxY2Maker::Histogramming make Correlation histograms" << endm;
      corrI   = new TH2D("corrI","Correlation for Inner Sector for pair of nearest rows",
			 100,-10.,10., 100,-10.,10.);
      corrO   = new TH2D("corrO","Correlation for Outer Sector for pair of nearest rows",
			 100,-10.,10., 100,-10.,10.);
      corrI2   = new TH2D("corrI2","Correlation for Inner Sector for pair rows & row + 2",
			  100,-10.,10., 100,-10.,10.);
      corrO2   = new TH2D("corrO2","Correlation for Outer Sector for pair rows & row + 2",
			  100,-10.,10., 100,-10.,10.);
      corrI5   = new TH2D("corrI5","Correlation for Inner Sector for pair rows & row + 5",
			  100,-10.,10., 100,-10.,10.);
      corrO5   = new TH2D("corrO5","Correlation for Outer Sector for pair rows & row + 5",
			  100,-10.,10., 100,-10.,10.);
      corrIw   = new TH2D("corrIw","Weighted correlation for Inner Sector for pair of nearest rows",
			  100,-10.,10., 100,-10.,10.);
      corrOw   = new TH2D("corrOw","Weighted correlation for Outer Sector for pair of nearest rows",
			  100,-10.,10., 100,-10.,10.);
      corrI1w   = new TH1D("corrI1w","Weighted distribution for Inner Sector",100,-10.,10.);
      corrO1w   = new TH1D("corrO1w","Weighted distribution for Outer Sector",100,-10.,10.);
      corrI2w   = new TH2D("corrI2w","Weighted correlation for Inner Sector for pair rows & row + 2",
			   100,-10.,10., 100,-10.,10.);
      corrO2w   = new TH2D("corrO2w","Weighted correlation for Outer Sector for pair rows & row + 2",
			   100,-10.,10., 100,-10.,10.);
      corrI5w   = new TH2D("corrI5w","Weighted correlation for Inner Sector for pair rows & row + 5",
			   100,-10.,10., 100,-10.,10.);
      corrO5w   = new TH2D("corrO5w","Weighted correlation for Outer Sector for pair rows & row + 5",
			   100,-10.,10., 100,-10.,10.);
    } // CORRELATION
    Points    = new TH2D("Points","dEdx(fit) versus no. of measured points",50,0,50., 500,-1.,4.);
    PointsB   = new TH2D("PointsB","dEdx(fit) versus no. of measured points Bichsel",50,0,50., 500,-1.,4.);
    TPoints  = new TH2D("TPoints","dEdx(fit) versus length", 
			150,10.,160., 500,-1.,4.);
    TPointsB  = new TH2D("TPointsB","dEdx(fit) versus length Bichsel", 
			 150,10.,160., 500,-1.,4.);
    Points70  = new TH2D("Points70","dEdx(I70) versus no. of measured points",50,0,50.,500,-1.,4.);
    Points70B = new TH2D("Points70B","dEdx(I70) versus no. of measured points Bichsel",50,0,50.,500,-1.,4.);
    TPoints70= new TH2D("TPoints70","dEdx(fit) versus length", 
			150,10.,160., 500,-1.,4.);
    TPoints70B= new TH2D("TPoints70B","dEdx(fit) versus length Bichsel", 
			 150,10.,160., 500,-1.,4.);
    if ((TESTBIT(m_Mode, kZBGX))) {
      gMessMgr->Warning() << "StdEdxY2Maker::Histogramming make zbgx histograms" << endm;
      zbgx = new TH3D*[2*NHYPS]; 
    }
    for (int hyp=0; hyp<NHYPS;hyp++) {
      TString nameP("fit");
      nameP += StProbPidTraits::mPidParticleDefinitions[hyp]->name().data();
      nameP.ReplaceAll("-","");
      TString title = "fitZ - Pred. for ";
      title += StProbPidTraits::mPidParticleDefinitions[hyp]->name().data();
      title.ReplaceAll("-","");
      title += " versus log10(beta*gamma) for pion";
      ffitZ[hyp]  = new TH2D(nameP.Data(),title.Data(),100,-1,4,100,-5,5);
      ffitZ[hyp]->SetMarkerColor(hyp+2);
      ffitP[hyp] = new TH2D(*ffitZ[hyp]);
      nameP.ReplaceAll("fit","fitP");
      ffitP[hyp]->SetName(nameP.Data());
      for (int sCharge = 0; sCharge < 2; sCharge++) {
	if ((TESTBIT(m_Mode, kZBGX))) {
	  nameP = "zbgx";
	  nameP += StProbPidTraits::mPidParticleDefinitions[hyp]->name().data();
	  nameP.ReplaceAll("-","");
	  if (sCharge == 0) nameP += "P";
	  else              nameP += "N";
	  if (zbgx) 
	    zbgx[2*hyp+sCharge] = new TH3D(nameP.Data(),"z = log(dE/dx) versus log10(beta*gamma) and log2(dx) for unique hyps",
				 100,-1,4,Nlog2dx,log2dxLow,log2dxHigh,320,-2,6);
	} // ZBGX
	nameP = StProbPidTraits::mPidParticleDefinitions[hyp]->name().data();
	nameP.ReplaceAll("-","");
	if (sCharge == 0) nameP += "P";
	else              nameP += "N";
	TString name = nameP;
	name += "70";
	title = "log(dE/dx70/I(";
	title += nameP;
	title += ")) versus log10(p/m)";
	hist70[hyp][sCharge] = new TH2D(name.Data(),title.Data(),100,-1.,4.,600,-2.,4.);
	name += "B";
	title += " Bichsel";
	hist70B[hyp][sCharge] = new TH2D(name.Data(),title.Data(),100,-1.,4.,600,-2.,4.);
	name = nameP;
	name += "z";
	title = "zFit - log(I(";
	title += nameP;
	title += ")) versus log10(p/m)";
	histz[hyp][sCharge] = new TH2D(name.Data(),title.Data(),100,-1.,4.,600,-2.,4.);
	name += "B";
	title += " Bichsel";
	histzB[hyp][sCharge] = new TH2D(name.Data(),title.Data(),100,-1.,4.,600,-2.,4.);
	name = nameP;
	name += "B";
	title = "log(I_{Sirrf}(";
	title += nameP;
	title += ")) versus log10(p/m)";
	histB[hyp][sCharge] = new TProfile(name.Data(),title.Data(),100,-1.,4.);
	name += "B";
	title = "log(I_{BB}(";
	title += nameP;
	title += ")) versus log10(p/m) Bichsel";
	histBB[hyp][sCharge] = new TProfile(name.Data(),title.Data(),100,-1.,4.);
      }
    }
    TDatime t1(tMin,0); /// min Time and
    TDatime t2(tMax,0); /// max 
    
    UInt_t i1 = t1.Convert();
    UInt_t i2 = t2.Convert();
    Int_t Nt = (i2 - i1)/(3600); // each hour 
    Pressure   = new TH3D("Pressure","log(dE/dx)_{uncorrected} - log(I(pi)) versus Row & Log(Pressure)", 
			  NumberOfRows,1., NumberOfRows+1,150, 6.84, 6.99,nZBins,ZdEdxMin,ZdEdxMax);
    PressureA  = new TH3D("PressureA","log(dE/dx)_{just after correction} log(I(pi)) versus Log(Pressure)", 
			  NumberOfRows,1., NumberOfRows+1,150, 6.84, 6.99,nZBins,ZdEdxMin,ZdEdxMax);
    PressureC  = new TH3D("PressureC","log(dE/dx)_{corrected} - row & log(I(pi)) versus Log(Pressure)", 
			  NumberOfRows,1., NumberOfRows+1,150, 6.84, 6.99,nZBins,ZdEdxMin,ZdEdxMax);
    PressureT   = new TH3D("PressureT","log(dE/dx)_{uncorrected} - log(I(pi)) versus Row & Log(Pressure*298.2/outputGasTemperature)", 
			  NumberOfRows,1., NumberOfRows+1,150, 6.84, 6.99,nZBins,ZdEdxMin,ZdEdxMax);
    PressureTA  = new TH3D("PressureTA","log(dE/dx)_{just after correction} log(I(pi)) versus Log(Pressure*298.2/outputGasTemperature)", 
			  NumberOfRows,1., NumberOfRows+1,150, 6.84, 6.99,nZBins,ZdEdxMin,ZdEdxMax);
    PressureTC  = new TH3D("PressureTC","log(dE/dx)_{corrected} - row & log(I(pi)) versus Log(Pressure*298.2/outputGasTemperature)", 
			  NumberOfRows,1., NumberOfRows+1,150, 6.84, 6.99,nZBins,ZdEdxMin,ZdEdxMax);
//     GainMonitor  = new TH2D("GainMonitor","log(dE/dx)_{corrected} - log(I(pi)) versus GainMonitor", 
// 			    100,70.,120.,nZBins,ZdEdxMin,ZdEdxMax);
    Time   = new TH2D("Time","log(dE/dx)_{uncorrected} - log(I(pi)) versus Date& Time", 
		      Nt,i1,i2,nZBins,ZdEdxMin,ZdEdxMax);
    TimeC  = new TH2D("TimeC","log(dE/dx)_{corrected} - log(I(pi)) versus Date& Time after correction", 
		      Nt,i1,i2,nZBins,ZdEdxMin,ZdEdxMax);
    TimeP  = new TH2D("TimeP","log(dE/dx)_{after pressure correction} - log(I(pi)) versus Date& Time", 
		      Nt,i1,i2,nZBins,ZdEdxMin,ZdEdxMax);
    FitPull= new TH2D("FitPull","(zFit - log(I(pi)))/dzFit  versus track length", 
		      150,10.,160,nZBins,ZdEdxMin,ZdEdxMax);
    Pull70 = new TH2D("Pull70","log(I70/I(pi)))/D70  versus track length", 
		      150,10.,160,nZBins,ZdEdxMin,ZdEdxMax);
    TString title("");
    title = "log(dE/dx/Pion) vs inputTPCGasPressure (mbar)";
    inputTPCGasPressureP = new TH2D("inputTPCGasPressureP","log(dE/dx/Pion) vs inputTPCGasPressure (mbar)",100,2.0,2.2,nZBins,ZdEdxMin,ZdEdxMax);
    nitrogenPressureP = new TH2D("nitrogenPressureP","log(dE/dx/Pion) vs nitrogenPressure (mbar)",100,0.9,1.1,nZBins,ZdEdxMin,ZdEdxMax);
    gasPressureDiffP = new TH2D("gasPressureDiffP","log(dE/dx/Pion) vs gasPressureDiff (mbar)",100,0.6,1.,nZBins,ZdEdxMin,ZdEdxMax);
    inputGasTemperatureP = new TH2D("inputGasTemperatureP","log(dE/dx/Pion) vs inputGasTemperature (degrees C)",100,295.,300.,nZBins,ZdEdxMin,ZdEdxMax);
    outputGasTemperatureP = new TH2D("outputGasTemperatureP","log(dE/dx/Pion) vs outputGasTemperature (degrees C)",100,295.,300.,nZBins,ZdEdxMin,ZdEdxMax);
    flowRateArgon1P = new TH2D("flowRateArgon1P","log(dE/dx/Pion) vs flowRateArgon1 (liters/min)",100,14.95,15.0,nZBins,ZdEdxMin,ZdEdxMax);
    flowRateArgon2P = new TH2D("flowRateArgon2P","log(dE/dx/Pion) vs flowRateArgon2 (liters/min)",100,0.,0.25,nZBins,ZdEdxMin,ZdEdxMax);
    flowRateMethaneP = new TH2D("flowRateMethaneP","log(dE/dx/Pion) vs flowRateMethane (liters/min)",100,1.34,1.37,nZBins,ZdEdxMin,ZdEdxMax);
    percentMethaneInP = new TH2D("percentMethaneInP","log(dE/dx/Pion) vs percentMethaneIn (percent)",100,9.6,10.6,nZBins,ZdEdxMin,ZdEdxMax);
    percentMethaneInPC = new TH2D("percentMethaneInPC","log(dE/dx/Pion)(corrected) vs percentMethaneIn (percent)",100,9.6,10.6,nZBins,ZdEdxMin,ZdEdxMax);
    percentMethaneInPA = new TH2D("percentMethaneInPA","log(dE/dx/Pion)(just after correction) vs percentMethaneIn (percent)",
				 100,9.6,10.6,nZBins,ZdEdxMin,ZdEdxMax);
    ppmOxygenInP = new TH2D("ppmOxygenInP","log(dE/dx/Pion) vs ppmOxygenIn (ppm)",100,20.,30.,nZBins,ZdEdxMin,ZdEdxMax);
    flowRateExhaustP = new TH2D("flowRateExhaustP","log(dE/dx/Pion) vs flowRateExhaust (liters/min)",100,5.,20.,nZBins,ZdEdxMin,ZdEdxMax);
    ppmWaterOutP = new TH2D("ppmWaterOutP","log(dE/dx/Pion) vs ppmWaterOut (ppm)",100,0.,20.,nZBins,ZdEdxMin,ZdEdxMax);
    ppmWaterOutPC = new TH2D("ppmWaterOutPC","log(dE/dx/Pion) corrected vs ppmWaterOut (ppm)",100,0.,20.,nZBins,ZdEdxMin,ZdEdxMax);
    ppmWaterOutPA = new TH2D("ppmWaterOutPA","log(dE/dx/Pion) just after correction vs ppmWaterOut (ppm)",100,0.,20.,nZBins,ZdEdxMin,ZdEdxMax);
    ppmOxygenOutP = new TH2D("ppmOxygenOutP","log(dE/dx/Pion) vs ppmOxygenOut (ppm)",100,0,20,nZBins,ZdEdxMin,ZdEdxMax);
    flowRateRecirculationP = new TH2D("flowRateRecirculationP","log(dE/dx/Pion) vs flowRateRecirculation (liters/min)",
				      100,515.,545.,nZBins,ZdEdxMin,ZdEdxMax);
    ffitZU = new TH2D("fitZU","fitZ - PredPi Unique versus log10(beta*gamma)",100,-1,4,100,-5,5);
    ffitZU->SetMarkerColor(7);
    ffitZU3 = new TH2D("fitZU3","fitZ - PredPi Unique and 3 sigma away versus log10(beta*gamma)",100,-1,4,100,-5,5);
    ffitZU3->SetMarkerColor(6);
    ffitZA = new TH2D("fitZA","fitZ - PredPi All versus log10(beta*gamma)",100,-1,4,100,-5,5);
    ffitZA->SetMarkerColor(1);
    hdEI  = new TH1D("hdEI","log10(dE) Inner after calibration",100,-8.,-3.);
    hdEUI = new TH1D("hdEUI","log10(dEU) Inner before correction",100,-8.,-3.);
    hdERI = new TH1D("hdERI","log10(dER) Inner after row correction  correction",100,-8.,-3.);
    hdEPI = new TH1D("hdEPI","log10(dEP) Inner after Pressure correction",100,-8.,-3.);
    hdETI = new TH1D("hdETI","log10(dET) Inner after TimeScale",100,-8.,-3.);
    hdESI = new TH1D("hdESI","log10(dES) Inner after after TimeScale + SecRow corrections",100,-8.,-3.);
    hdEZI = new TH1D("hdEZI","log10(dEZ) Inner after TimeScale + SecRow + Sec Z corrections ",100,-8.,-3.);
    hdEMI = new TH1D("hdEMI","log10(dEM) Inner after TimeScale + SecRow + Sec Z + Multiplicity corrections",100,-8.,-3.);
    hdEO  = new TH1D("hdEO","log10(dE) Outer after calibration",100,-8.,-3.);
    hdEUO = new TH1D("hdEUO","log10(dEU) Outer before correction",100,-8.,-3.);
    hdERO = new TH1D("hdERO","log10(dER) Outer after row correction  correction",100,-8.,-3.);
    hdEPO = new TH1D("hdEPO","log10(dEP) Outer after Pressure correction",100,-8.,-3.);
    hdETO = new TH1D("hdETO","log10(dET) Outer after TimeScale",100,-8.,-3.);
    hdESO = new TH1D("hdESO","log10(dES) Outer after after TimeScale + SecRow corrections",100,-8.,-3.);
    hdEZO = new TH1D("hdEZO","log10(dEZ) Outer after TimeScale + SecRow + Sec Z corrections ",100,-8.,-3.);
    hdEMO = new TH1D("hdEMO","log10(dEM) Outer after TimeScale + SecRow + Sec Z + Multiplicity corrections",100,-8.,-3.);
    if ((TESTBIT(m_Mode, kProbabilityPlot))) {
      gMessMgr->Warning() << "StdEdxY2Maker::Histogramming Probability Histograms" << endm;
      Prob = new TH3D("Prob","Z(=log(I70/Bichsel)) versun log10(bg) for pion and Probability",
		      100,-1.,4.,10*NHYPS+1,-.1,NHYPS,600,-2.,4.);
    } // ProbabilityPlot
#if 0
    dx0dx = new TH2D("dx0dx","segment length versus sright line approximation",50,0.,5.,80,0.,8.);
#endif
    dXdE  = new TH3D("dXdE","log(dEdx/Pion) versus dX and row",
		     100,0.,5., NumberOfRows,1., NumberOfRows+1,nZBins,ZdEdxMin,ZdEdxMax);
    dXdEA = new TH3D("dXdEA","log(dEdx/Pion) just after correction versus dX and row",
		     100,0.,5., NumberOfRows,1., NumberOfRows+1,nZBins,ZdEdxMin,ZdEdxMax);
    dXdEC = new TH3D("dXdEC","log(dEdx/Pion) corrected versus dX and row",
		     100,0.,5., NumberOfRows,1., NumberOfRows+1,nZBins,ZdEdxMin,ZdEdxMax);
    //    Z3->SetTitle(Form("%s p in [%4f.1,%4f.1]",Z3->GetTitle(),pMomin,pMomax);
		 //
    // Create a ROOT Tree and one superbranch
    if ((TESTBIT(m_Mode, kMakeTree))) { 
      gMessMgr->Warning() << "StdEdxY2Maker::Histogramming Make Tree" << endm;
      ftree = new TTree("dEdxT","dEdx tree");
      ftree->SetAutoSave(1000000000);  // autosave when 1 Gbyte written
      Int_t bufsize = 64000;
      Int_t split = 99;
      if (split)  bufsize /= 4;
      ftrack = new dEdxTrack();
      TTree::SetBranchStyle(1); //new style by default
      TBranch *branch = ftree->Branch("dEdxTrack", "dEdxTrack", &ftrack, bufsize,split);
      branch->SetAutoDelete(kFALSE);
    }
    return;
  }
  // fill histograms 
  St_tpcGas           *tpcGas = 0;
  if ( m_TpcdEdxCorrection) tpcGas = m_TpcdEdxCorrection->tpcGas();
  StThreeVectorD g3 = gTrack->geometry()->momentum(); // p of global track
  Double_t pMomentum = g3.mag();
  Double_t Eta = g3.pseudoRapidity();
  Int_t sCharge = 0;
  if (gTrack->geometry()->charge() < 0) sCharge = 1;
  Int_t NoFitPoints = gTrack->fitTraits().numberOfFitPoints();
  //  StTpcDedxPidAlgorithm tpcDedxAlgo;
  // dE/dx
  //  StPtrVecTrackPidTraits traits = gTrack->pidTraits(kTpcId);
  StSPtrVecTrackPidTraits &traits = gTrack->pidTraits();
  unsigned int size = traits.size();
  StDedxPidTraits *pid, *pid70 = 0, *pidF = 0;
  StProbPidTraits *pidprob = 0;
#ifdef CompareWithToF
  StTofPidTraits* pidTof  = 0;
#endif
  Double_t I70 = 0, D70 = 0;
  Double_t chisq = 1e10, fitZ = 0, fitdZ = 1e10;
  Int_t N70 = 0, NF = 0;
  Double_t TrackLength70 = 0, TrackLength = 0;
  if (size) {
    for (unsigned int i = 0; i < traits.size(); i++) {
      if (! traits[i]) continue;
      if ( traits[i]->IsZombie()) continue;
      pid = dynamic_cast<StDedxPidTraits*>(traits[i]);
      if (pid) {
	if (pid->method() == kTruncatedMeanIdentifier) {
	  pid70 = pid; I70 = pid70->mean(); N70 = pid70->numberOfPoints();
	  TrackLength70 = pid70->length(); D70 = pid70->errorOnMean();
	}
	if (pid->method() == kLikelihoodFitIdentifier) {
	  pidF = pid;
	  fitZ = TMath::Log(pidF->mean()); NF = pidF->numberOfPoints(); 
	  TrackLength = pidF->length(); fitdZ = pidF->errorOnMean(); 
	}
	continue;
      }
      StProbPidTraits *p = dynamic_cast<StProbPidTraits*>(traits[i]);
      if (p) {pidprob = p; continue;}
      //      pidTof = dynamic_cast<StTofPidTraits*>(traits[i]);
    }
  }
#ifdef CompareWithToF
  // Tof for primary track only
  StTrackNode *node = gTrack->node();
  if (node) {
    StPrimaryTrack *pTrack = static_cast<StPrimaryTrack*>(node->track(primary));
    if (pTrack) {
      StSPtrVecTrackPidTraits &traitsp = pTrack->pidTraits();
      for (unsigned int i = 0; i < traitsp.size(); i++) {
	StTofPidTraits* p = dynamic_cast<StTofPidTraits*>(traitsp[i]);
	if (p) {pidTof = p;}
      }
    }
  }
#endif  
  if (pid70 && ! pidF) TrackLength = TrackLength70;
  Double_t Pred[NHYPS],  Pred70[NHYPS];
  Double_t PredB[NHYPS], Pred70B[NHYPS];
  Double_t date = GetDateTime().Convert();
  Double_t devZ[NHYPS], devZs[NHYPS];
  Double_t bg = TMath::Log10(pMomentum/StProbPidTraits::mPidParticleDefinitions[kPidPion]->mass());
  Double_t bghyp[NHYPS];
  Int_t l;
  for (l = kPidElectron; l < NHYPS; l += 1) {
    bghyp[l] = TMath::Log10(pMomentum/StProbPidTraits::mPidParticleDefinitions[l]->mass());
    PredB[l]   = 1.e-6*TMath::Exp(m_Bichsel->GetMostProbableZ(bghyp[l],1.0)); 
    Pred70B[l] = 1.e-6*m_Bichsel->GetI70(bghyp[l],1.0); 
    Pred[l] = 1.e-6*BetheBloch::Sirrf(pMomentum/StProbPidTraits::mPidParticleDefinitions[l]->mass(),60.,l==3); 
    Pred70[l] = Pred[l];
    if (pid70 && TrackLength70 > 40.) {
      hist70[l][sCharge]->Fill(bghyp[l],TMath::Log(I70/Pred70[l]));
      histB[l][sCharge]->Fill(bghyp[l],TMath::Log(Pred[l]));
      hist70B[l][sCharge]->Fill(bghyp[l],TMath::Log(I70/Pred70B[l]));
      histBB[l][sCharge]->Fill(bghyp[l],TMath::Log(PredB[l]));
      devZ[l] = TMath::Log(I70/Pred70B[l]);
    }
    if (pidF && TrackLength > 40.) {
      histz[l][sCharge]->Fill(bghyp[l],fitZ - TMath::Log(Pred[l]));
      histzB[l][sCharge]->Fill(bghyp[l],fitZ - TMath::Log(PredB[l]));
      devZs[l] = TMath::Abs(devZ[l])/fitdZ; //D70;
    }
  }
  Double_t devZmin = 999;
  Int_t PiDStatus = 0;
  Int_t  PiDkey   = -1; // best
  Int_t  PiDkeyU  = -1; // only one with devZs<3, 
  Int_t  PiDkeyU3 = -1; // -"- and devZs > 5 for all others 
  Int_t lBest  = -1;
  if (pidF && TrackLength > 40.) {
#ifdef CompareWithToF
    // use ToF 
    Double_t devToF[NHYPS];
    if (pidTof) {
      for (l = kPidElectron; l < NHYPS; l++) {
	switch (l) {
	case kPidElectron:
	  devToF[l] = pidTof->sigmaElectron();
	  break;
	case kPidProton:
	  devToF[l] = pidTof->sigmaProton();
	  break;
	case kPidKaon:
	  devToF[l] = pidTof->sigmaKaon();
	  break;
	case kPidPion:
	  devToF[l] = pidTof->sigmaPion();
	  break;
	default:
	  devToF[l] = 999.;
	  break;
	}
	devToF[l] = TMath::Abs(devToF[l]);
      }
    for (l = kPidElectron; l < NHYPS; l++) {
      if (l == kPidMuon) continue;
      if (devToF[l] < 3.0) PiDStatus += 1<<l;
      if (devToF[l] < devZmin) {devZmin = devToF[l]; lBest = l;}
    }
    if (lBest >=0) {
      if (devToF[lBest] < 3.0) {
	PiDkey = lBest;
	Int_t lNext = -1;
	devZmin = 999;
	for (l = kPidElectron; l < NHYPS; l += 1) {
	  if (l == kPidMuon) continue;
	  if (l == lBest) continue;
	  if (devToF[l] < devZmin) {devZmin = devToF[l]; lNext = l;}
	}
	if (lNext >= 0) {
	  if (devToF[lNext] > 3.) {
	    PiDkeyU = PiDkey;
	    if (devToF[lNext] > 5.) PiDkeyU3 = PiDkeyU;
	    //	    else  {if (devZs[lBest] < devZs[lNext] + 5.) PiDkeyU3 = lBest;}
	  }
	}
      }
    }
    }
#else  
    for (l = kPidElectron; l < NHYPS; l++) {
      if (l == kPidMuon) continue;
      if (devZs[l] < 3.0) PiDStatus += 1<<l;
      if (devZs[l] < devZmin) {devZmin = devZs[l]; lBest = l;}
    }
    if (lBest >=0) {
      if (devZs[lBest] < 3.0) {
	PiDkey = lBest;
	Int_t lNext = -1;
	devZmin = 999;
	for (l = kPidElectron; l < NHYPS; l += 1) {
	  if (l == kPidMuon) continue;
	  if (l == lBest) continue;
	  if (devZs[l] < devZmin) {devZmin = devZs[l]; lNext = l;}
	}
	if (lNext >= 0) {
	  if (devZs[lNext] > 3.) {
	    PiDkeyU = PiDkey;
	    if (devZs[lNext] > 5.) PiDkeyU3 = PiDkeyU;
	    //	    else  {if (devZs[lBest] < devZs[lNext] + 5.) PiDkeyU3 = lBest;}
	  }
	}
      }
    }
#endif
  }
  if ((TESTBIT(m_Mode, kProbabilityPlot))) {
    if (TrackLength70 > 40) { 
      Double_t Z70 = TMath::Log(I70/Pred70[kPidPion]);
      Prob->Fill(bghyp[kPidPion],-0.05,Z70);
      if (pidprob) {
	Int_t N = pidprob->GetPidArray()->GetSize();
	for (int i = 0; i < N; i++) {
	  Double_t p = pidprob->GetProbability(i);
	  if (p > 0.99) p = 0.99;
	  Prob->Fill(bghyp[kPidPion],i+p,Z70);
	}
      }
    }
  } // ProbabilityPlot
  //  if (D70 > 0.05 && D70 < 0.15) {
  if (pidF && fitdZ > 0.05 && fitdZ < 0.15) {
    ffitZA->Fill(bg,devZ[kPidPion]);
    for (l = kPidElectron; l < NHYPS; l += 1) {
      ffitZ[l]->Fill(bg,devZ[kPidPion]);
      if (pidprob && pidprob->GetSum() > 0 && pidprob->GetProbability(l) > 0.9) ffitP[l]->Fill(bg,devZ[kPidPion]);
      if (PiDkeyU  >= 0) ffitZU->Fill(bghyp[kPidPion],devZ[kPidPion]);
      if (PiDkeyU3 >= 0) ffitZU3->Fill(bghyp[kPidPion],devZ[kPidPion]);
    }
  }
  if (Pred[kPidPion] <= 0) {
    gMessMgr->Warning() << "StdEdxY2Maker:: Prediction for p = " 
			<< pMomentum << " and TrackLength = " << TrackLength 
			<< " is wrong = " << Pred[kPidPion] << " <<<<<<<<<<<<<" << endl;
    return;
  };
  if (pidF) {
    Points->Fill(NdEdx,fitZ-TMath::Log(Pred[kPidPion]));
    PointsB->Fill(NdEdx,fitZ-TMath::Log(PredB[kPidPion]));
    TPoints->Fill(TrackLength,fitZ-TMath::Log(Pred[kPidPion]));
    TPointsB->Fill(TrackLength,fitZ-TMath::Log(PredB[kPidPion]));
    FitPull->Fill(TrackLength,(fitZ - TMath::Log(PredB[kPidPion]))/fitdZ);
  }
  if (pid70) {
    Points70->Fill(N70,TMath::Log(I70/Pred70[kPidPion]));
    Points70B->Fill(N70,TMath::Log(I70/PredB[kPidPion]));
    TPoints70->Fill(TrackLength,TMath::Log(I70/Pred70[kPidPion]));
    TPoints70B->Fill(TrackLength,TMath::Log(I70/Pred70B[kPidPion]));
    Pull70->Fill(TrackLength,TMath::Log(I70/Pred70B[kPidPion])/D70);
  }
  //  if (NoFitPoints >= 30 && TrackLength > 40) { 
  if (NoFitPoints >= 20) { 
    Int_t k;
    for (k = 0; k < NdEdx; k++) {
      FdEdx[k].zP = 
	m_Bichsel->GetMostProbableZ(TMath::Log10(pMomentum/StProbPidTraits::mPidParticleDefinitions[kPidPion]->mass()),
      				    TMath::Log2(FdEdx[k].dx));
      FdEdx[k].sigmaP = 
	m_Bichsel->GetRmsZ(TMath::Log10(pMomentum/StProbPidTraits::mPidParticleDefinitions[kPidPion]->mass()),
			   TMath::Log2(FdEdx[k].dx));
      Double_t predB  = 1.e-6*TMath::Exp(FdEdx[k].zP);
      FdEdx[k].dEdxN  = TMath::Log(FdEdx[k].dEdx /predB);
      for (Int_t l = 0; l <= StTpcdEdxCorrection::kTpcLast; l++) {
	FdEdx[k].C[l].dEdxN = TMath::Log(FdEdx[k].C[l].dEdx/predB);
      }
      if ((TESTBIT(m_Mode, kCORRELATION))) {
	if (chisq > 0 && chisq < 10000.0) {
	  Double_t zk  = FdEdx[k].zdev;
	  if (FdEdx[k].Prob > 1.e-12) {
	    if (FdEdx[k].row > 13) corrO1w->Fill(zk,1./FdEdx[k].Prob);
	    else                   corrI1w->Fill(zk,1./FdEdx[k].Prob);
	  }
	  for (int m = 0; m < NdEdx; m++){
	    if (k == m) continue;
	    Double_t zl  = FdEdx[m].zdev;
	    if (FdEdx[m].row%2 == 1 && FdEdx[m].row - FdEdx[k].row  == 1) {
	      if (FdEdx[k].row > 13) {
		corrO->Fill(zk,zl); 
		if (FdEdx[k].Prob*FdEdx[m].Prob > 1.e-12) 
		  corrOw->Fill(zk,zl,1./(FdEdx[k].Prob*FdEdx[m].Prob));
	      }
	      else {
		corrI->Fill(zk,zl); 
		if (FdEdx[k].Prob*FdEdx[m].Prob > 1.e-12) 
		  corrIw->Fill(zk,zl,1./(FdEdx[k].Prob*FdEdx[m].Prob));
	      }
	    }
	    if (FdEdx[m].row%2 == 1 && FdEdx[m].row - FdEdx[k].row  == 2) {
	      if (FdEdx[k].row > 13) {
		corrO2->Fill(zk,zl); 
		if (FdEdx[k].Prob*FdEdx[m].Prob > 1.e-12) 
		  corrO2w->Fill(zk,zl,1./(FdEdx[k].Prob*FdEdx[m].Prob));
	      }
	      else {
		corrI2->Fill(zk,zl); 
		if (FdEdx[k].Prob*FdEdx[m].Prob > 1.e-12) 
		  corrI2w->Fill(zk,zl,1./(FdEdx[k].Prob*FdEdx[m].Prob));
	      }
	    }
	    if (FdEdx[m].row%2 == 1 && FdEdx[m].row - FdEdx[k].row  == 5) {
	      if (FdEdx[k].row > 13) {
		corrO5->Fill(zk,zl); 
		if (FdEdx[k].Prob*FdEdx[m].Prob > 1.e-12) 
		  corrO5w->Fill(zk,zl,1./(FdEdx[k].Prob*FdEdx[m].Prob));
	      }
	      else {
		corrI5->Fill(zk,zl); 
		if (FdEdx[k].Prob*FdEdx[m].Prob > 1.e-12) 
		  corrI5w->Fill(zk,zl,1./(FdEdx[k].Prob*FdEdx[m].Prob));
	      }
	    }
	  } // end of l loop
	}
      } // CORRELATION
      if (FdEdx[k].row < 14) {
	hdEI->Fill(TMath::Log10(FdEdx[k].dE));
	hdEUI->Fill(TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::kUncorrected].dE));
	hdERI->Fill(TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::kAdcCorrection].dE));
	hdEPI->Fill(TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::ktpcPressure].dE));
	hdETI->Fill(TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::ktpcTime].dE));
	hdESI->Fill(TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::kTpcSecRow].dE));
	hdEZI->Fill(TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::kzCorrection].dE));
	hdEMI->Fill(TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::kMultiplicity].dE));
      }
      else {
	hdEO->Fill(TMath::Log10(FdEdx[k].dE));
	hdEUO->Fill(TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::kUncorrected].dE));
	hdERO->Fill(TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::kAdcCorrection].dE));
	hdEPO->Fill(TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::ktpcPressure].dE));
	hdETO->Fill(TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::ktpcTime].dE));
	hdESO->Fill(TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::kTpcSecRow].dE));
	hdEZO->Fill(TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::kzCorrection].dE));
	hdEMO->Fill(TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::kMultiplicity].dE));
      }
      if ((TESTBIT(m_Mode, kAdcHistos))) {
	if (PiDkeyU3 >= 0) {
	  Double_t betaXgamma = pMomentum/StProbPidTraits::mPidParticleDefinitions[PiDkeyU3]->mass();
	  Double_t zA = m_Bichsel->GetMostProbableZ(TMath::Log10(betaXgamma),TMath::Log2(FdEdx[k].dx));
	  Double_t PredA = 1.e-6*TMath::Exp(zA);
	  Double_t PredE =  PredA*FdEdx[k].dx;
	  Double_t PredEL = TMath::Log10(PredE);
	  if (FdEdx[k].row < 14) {
	    if (AdcI)        AdcI->Fill(PredEL,TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::kUncorrected].dE));
	    if (AdcIC)      AdcIC->Fill(PredEL,TMath::Log10(FdEdx[k].dE));
	  }
	  else {
	    if (AdcO)        AdcO->Fill(PredEL,TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::kUncorrected].dE));
	    if (AdcOC)      AdcOC->Fill(PredEL,TMath::Log10(FdEdx[k].dE));
	  }
	  Double_t PredEU =  PredE;
	  Double_t PredEUL = TMath::Log10(PredEU);
	  Double_t PredEULN = TMath::Log(PredEU);
	  Double_t PredEUkeV = 1.e6*PredE;
	  if (tpcGas) {
	    if (FdEdx[k].row < 14) {
	      if (Adc3I)   Adc3I->Fill(PredEUL,TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::kUncorrected].dE));
	      if (Adc3IC) Adc3IC->Fill(PredEUL,TMath::Log10(FdEdx[k].dE));
	      if (Adc3Ip && Adc3Ip[PiDkeyU3]) 
		Adc3Ip[PiDkeyU3]->Fill(PredEUL,TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::kUncorrected].dE));
	      if (sCharge == 0) 
		AdcIZP->Fill(PredEUkeV,FdEdx[k].ZdriftDistanceO2,TMath::Log(FdEdx[k].C[StTpcdEdxCorrection::kUncorrected].dE)-PredEULN);
	      else 
		AdcIZN->Fill(PredEUkeV,FdEdx[k].ZdriftDistanceO2,TMath::Log(FdEdx[k].C[StTpcdEdxCorrection::kUncorrected].dE)-PredEULN);
	    }
	    else {		   					                            
	      if (Adc3O)   Adc3O->Fill(PredEUL,TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::kUncorrected].dE));
	      if (Adc3OC) Adc3OC->Fill(PredEUL,TMath::Log10(FdEdx[k].dE));
	      if (Adc3Op && Adc3Op[PiDkeyU3]) 
		Adc3Op[PiDkeyU3]->Fill(PredEUL,TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::kUncorrected].dE));
	      if (sCharge == 0) 
		AdcOZP->Fill(PredEUkeV,FdEdx[k].ZdriftDistanceO2,TMath::Log(FdEdx[k].C[StTpcdEdxCorrection::kUncorrected].dE)-PredEULN);
	      else 
		AdcOZN->Fill(PredEUkeV,FdEdx[k].ZdriftDistanceO2,TMath::Log(FdEdx[k].C[StTpcdEdxCorrection::kUncorrected].dE)-PredEULN);
	    }
	  }
	}
      } // AdcHistos
      if (pMomentum > pMomin && pMomentum < pMomax) { // Momentum cut
	if (tpcGas) {
	  if (inputTPCGasPressureP) inputTPCGasPressureP->Fill((*tpcGas)[0].inputTPCGasPressure,FdEdx[k].dEdxN);
	  if (nitrogenPressureP) nitrogenPressureP->Fill((*tpcGas)[0].nitrogenPressure,FdEdx[k].dEdxN);
	  if (gasPressureDiffP) gasPressureDiffP->Fill((*tpcGas)[0].gasPressureDiff,FdEdx[k].dEdxN);
	  if (inputGasTemperatureP) inputGasTemperatureP->Fill((*tpcGas)[0].inputGasTemperature,FdEdx[k].dEdxN);
	  if (outputGasTemperatureP) outputGasTemperatureP->Fill((*tpcGas)[0].outputGasTemperature,FdEdx[k].dEdxN);
	  if (flowRateArgon1P) flowRateArgon1P->Fill((*tpcGas)[0].flowRateArgon1,FdEdx[k].dEdxN);
	  if (flowRateArgon2P) flowRateArgon2P->Fill((*tpcGas)[0].flowRateArgon2,FdEdx[k].dEdxN);
	  if (flowRateMethaneP)  flowRateMethaneP->Fill((*tpcGas)[0].flowRateMethane,FdEdx[k].dEdxN);
	  if (percentMethaneInP)  percentMethaneInP->Fill((*tpcGas)[0].percentMethaneIn,FdEdx[k].C[StTpcdEdxCorrection::ktpcPressure-1].dEdxN);
	  if (percentMethaneInPA) percentMethaneInPA->Fill((*tpcGas)[0].percentMethaneIn,FdEdx[k].C[StTpcdEdxCorrection::ktpcPressure].dEdxN);
	  if (percentMethaneInPC) percentMethaneInPC->Fill((*tpcGas)[0].percentMethaneIn,FdEdx[k].dEdxN);
	  if (ppmOxygenInP) ppmOxygenInP->Fill((*tpcGas)[0].ppmOxygenIn,FdEdx[k].dEdxN);
	  if (flowRateExhaustP) flowRateExhaustP->Fill((*tpcGas)[0].flowRateExhaust,FdEdx[k].dEdxN);
	  if (ppmWaterOutP)  ppmWaterOutP->Fill((*tpcGas)[0].ppmWaterOut,FdEdx[k].C[StTpcdEdxCorrection::ktpcPressure-1].dEdxN);
	  if (ppmWaterOutPA) ppmWaterOutPA->Fill((*tpcGas)[0].ppmWaterOut,FdEdx[k].C[StTpcdEdxCorrection::ktpcPressure].dEdxN);
	  if (ppmWaterOutPC) ppmWaterOutPC->Fill((*tpcGas)[0].ppmWaterOut,FdEdx[k].dEdxN);
	  if (ppmOxygenOutP) ppmOxygenOutP->Fill((*tpcGas)[0].ppmOxygenOut,FdEdx[k].dEdxN);
	  if (flowRateRecirculationP) flowRateRecirculationP->Fill((*tpcGas)[0].flowRateRecirculation,FdEdx[k].dEdxN);
	}
	if (m_trig) {
	  if (m_trig->zdcX > 0 && ZdcCP) ZdcCP->Fill(TMath::Log10(m_trig->zdcX), FdEdx[k].dEdxN);
	  if (m_trig->bbcX > 0 && BBCP) BBCP->Fill(TMath::Log10(m_trig->bbcX), FdEdx[k].dEdxN);
	  if (m_trig->L0   > 0 && L0P) L0P->Fill(TMath::Log10(m_trig->L0), FdEdx[k].dEdxN);
	  if (m_trig->mult > 0) {
	    if (MultiplicityPI && FdEdx[k].row < 14) MultiplicityPI->Fill(TMath::Log10(m_trig->mult), FdEdx[k].dEdxN);
	    if (MultiplicityPO && FdEdx[k].row > 13) MultiplicityPO->Fill(TMath::Log10(m_trig->mult), FdEdx[k].dEdxN);
	  }
	}
	//       if ((TESTBIT(m_Mode, kGASHISTOGRAMS))) {
	// 	if (m_tpcGainMonitor) {
	// 	  if (GainMonitor)  GainMonitor->Fill((*m_tpcGainMonitor)[0].center, FdEdx[k].dEdxN);
	// 	}
	//       } // GASHISTOGRAMS 
	// Correction 
	Double_t eta =  Eta;
#if 0
	Int_t SecN = FdEdx[k].sector;
	if (FdEdx[k].row > 13) SecN += 24;
#endif
	if (SecRow3 )  SecRow3->Fill(FdEdx[k].sector+0.5,FdEdx[k].row+0.5,FdEdx[k].C[StTpcdEdxCorrection::kTpcSecRow-1].dEdxN);
	if (SecRow3A) SecRow3A->Fill(FdEdx[k].sector+0.5,FdEdx[k].row+0.5,FdEdx[k].C[StTpcdEdxCorrection::kTpcSecRow].dEdxN);
	if (SecRow3C) SecRow3C->Fill(FdEdx[k].sector+0.5,FdEdx[k].row+0.5,FdEdx[k].dEdxN);
	if ((TESTBIT(m_Mode, kMip))) {
	  if (SecRow3Mip && TMath::Abs(devZs[kPidPion]) < 2) 
	    SecRow3Mip->Fill(FdEdx[k].row+0.5,
			     TMath::Log(FdEdx[k].dx)/TMath::Log(2.),
			     FdEdx[k].dEdxN);// /FdEdx[k].sigmaP);
	} // Mip
	if (tpcGas && Pressure) {
	  Double_t p     = (*tpcGas)[0].barometricPressure;
	  Double_t t     = (*tpcGas)[0].inputGasTemperature/298.2;
	  if (p > 0) {
	    Double_t press = TMath::Log(p);
	    if (Pressure)  Pressure ->Fill(FdEdx[k].row+0.5,press,FdEdx[k].C[StTpcdEdxCorrection::ktpcPressure-1].dEdxN);
	    if (PressureA) PressureA->Fill(FdEdx[k].row+0.5,press,FdEdx[k].C[StTpcdEdxCorrection::ktpcPressure].dEdxN);
	    if (PressureC) PressureC->Fill(FdEdx[k].row+0.5,press,FdEdx[k].dEdxN);
	  }
	  if (p*t > 0) {
	    Double_t temp = TMath::Log(p/t);
	    if (PressureT)  PressureT ->Fill(FdEdx[k].row+0.5,temp,FdEdx[k].C[StTpcdEdxCorrection::ktpcPressure-1].dEdxN);
	    if (PressureTA) PressureTA->Fill(FdEdx[k].row+0.5,temp,FdEdx[k].C[StTpcdEdxCorrection::ktpcPressure].dEdxN);
	    if (PressureTC) PressureTC->Fill(FdEdx[k].row+0.5,temp,FdEdx[k].dEdxN);
	  }
	}
	if (Time)    Time->Fill(date,FdEdx[k].C[StTpcdEdxCorrection::ktpcTime-1].dEdxN);
	if (TimeP)  TimeP->Fill(date,FdEdx[k].C[StTpcdEdxCorrection::ktpcTime].dEdxN);
	if (TimeC)  TimeC->Fill(date,FdEdx[k].dEdxN);
	if (Z3O)  Z3O->Fill(FdEdx[k].row+0.5,FdEdx[k].ZdriftDistanceO2,FdEdx[k].C[StTpcdEdxCorrection::kDrift-1].dEdxN);
	if (Z3OA)Z3OA->Fill(FdEdx[k].row+0.5,FdEdx[k].ZdriftDistanceO2,FdEdx[k].C[StTpcdEdxCorrection::kDrift].dEdxN);
	if (Z3OC)Z3OC->Fill(FdEdx[k].row+0.5,FdEdx[k].ZdriftDistanceO2,FdEdx[k].dEdxN);
	if (Z3OW)  Z3OW->Fill(FdEdx[k].row+0.5,FdEdx[k].ZdriftDistanceO2W,FdEdx[k].C[StTpcdEdxCorrection::kDrift-1].dEdxN);
	if (Z3OWA)Z3OWA->Fill(FdEdx[k].row+0.5,FdEdx[k].ZdriftDistanceO2W,FdEdx[k].C[StTpcdEdxCorrection::kDrift].dEdxN);
	if (Z3OWC)Z3OWC->Fill(FdEdx[k].row+0.5,FdEdx[k].ZdriftDistanceO2W,FdEdx[k].dEdxN);
	if (Z3)    Z3->Fill(FdEdx[k].row+0.5,FdEdx[k].ZdriftDistance,  FdEdx[k].C[StTpcdEdxCorrection::kzCorrection-1].dEdxN);
	if (Z3A)  Z3A->Fill(FdEdx[k].row+0.5,FdEdx[k].ZdriftDistance,  FdEdx[k].C[StTpcdEdxCorrection::kzCorrection].dEdxN);
	if (Z3C)  Z3C->Fill(FdEdx[k].row+0.5,FdEdx[k].ZdriftDistance,  FdEdx[k].dEdxN);
	if (ETA)  ETA->Fill(FdEdx[k].row+0.5,eta,FdEdx[k].dEdxN);
	if (ETA3)ETA3->Fill(FdEdx[k].row+0.5,eta,FdEdx[k].dEdxN);
#if 0
	if (Pads3)    Pads3->Fill(FdEdx[k].row+0.5,FdEdx[k].Npads,  FdEdx[k].C[StTpcdEdxCorrection::kTpcPadTBins-1].dEdxN);
	if (Pads3C)  Pads3C->Fill(FdEdx[k].row+0.5,FdEdx[k].Npads,  FdEdx[k].dEdxN);
	if (Tbins3)    Tbins3->Fill(FdEdx[k].row+0.5,FdEdx[k].Ntbins,  FdEdx[k].C[StTpcdEdxCorrection::kTpcPadTBins-1].dEdxN);
	if (Tbins3C)  Tbins3C->Fill(FdEdx[k].row+0.5,FdEdx[k].Ntbins,  FdEdx[k].dEdxN);
	if (PadsTbins3)    PadsTbins3->Fill(FdEdx[k].row+0.5,FdEdx[k].Npads*FdEdx[k].Ntbins,  FdEdx[k].C[StTpcdEdxCorrection::kTpcPadTBins-1].dEdxN);
	if (PadsTbins3C)  PadsTbins3C->Fill(FdEdx[k].row+0.5,FdEdx[k].Npads*FdEdx[k].Ntbins,  FdEdx[k].dEdxN);
#endif
	if (dCharge3)    dCharge3->Fill(FdEdx[k].row+0.5,FdEdx[k].dCharge,  FdEdx[k].C[StTpcdEdxCorrection::kTpcdCharge-1].dEdxN);
	if (dCharge3C)  dCharge3C->Fill(FdEdx[k].row+0.5,FdEdx[k].dCharge,  FdEdx[k].dEdxN);
	if (m_trig && m_trig->mult > 0) {
	  if (MulRow)   MulRow->Fill(TMath::Log10(m_trig->mult),FdEdx[k].row+0.5,FdEdx[k].C[StTpcdEdxCorrection::kMultiplicity-1].dEdxN);
	  if (MulRowC) MulRowC->Fill(TMath::Log10(m_trig->mult),FdEdx[k].row+0.5,FdEdx[k].dEdxN);
	}
#if 0
	if (dx0dx) dx0dx->Fill(FdEdx[k].dx, FdEdx[k].dx0);
#endif
	if (dXdE )  dXdE->Fill(FdEdx[k].dx,FdEdx[k].row+0.5,FdEdx[k].C[StTpcdEdxCorrection::kdXCorrection-1].dEdxN);
	if (dXdEA) dXdEA->Fill(FdEdx[k].dx,FdEdx[k].row+0.5,FdEdx[k].C[StTpcdEdxCorrection::kdXCorrection].dEdxN);
	if (dXdEC) dXdEC->Fill(FdEdx[k].dx,FdEdx[k].row+0.5,FdEdx[k].dEdxN);
      }
      if (TESTBIT(m_Mode, kZBGX) && PiDkeyU3 >= 0 && zbgx) 
	zbgx[2*PiDkeyU3+sCharge]->Fill(bghyp[PiDkeyU3],TMath::Log(FdEdx[k].dx)/TMath::Log(2.),FdEdx[k].dEdxL-GeV2keV);
    }
  }
  // dE/dx tree
  if (ftrack && ftree && TrackLength70 > 20.0) {
    ftrack->Clear();
    ftrack->sCharge = 1 - 2*sCharge;
    ftrack->p = pMomentum;
    ftrack->Eta = Eta;
    ftrack->R0 = gTrack->geometry()->origin().mag();
    ftrack->Z0 = gTrack->geometry()->origin().z();
    ftrack->Phi0 = gTrack->geometry()->origin().phi();
    ftrack->NoFitPoints = NoFitPoints;
    ftrack->N70 = N70;
    ftrack->I70 = I70;
    ftrack->TrackLength70 = TrackLength70;
    ftrack->NdEdx = NdEdx;
    ftrack->chisq = chisq;
    ftrack->fitZ = fitZ;
    ftrack->fitdZ = fitdZ;
    ftrack->TrackLength = TrackLength;
    ftrack->PredP = Pred70[kPidProton];
    ftrack->PredK = Pred70[kPidKaon];
    ftrack->PredPi = Pred70[kPidPion];
    ftrack->PredE = Pred70[kPidElectron];
    for (Int_t k = 0; k < NdEdx; k++) {
      ftrack->AddPoint(FdEdx[k]);
    }
    ftree->Fill();
  }
  return;
}
//_____________________________________________________________________________
void StdEdxY2Maker::PrintdEdx(Int_t iop) {
  const Char_t *Names[3] = {"CdEdx","FdEdx","dEdxS"};
  if (iop < 0 || iop > 2) return;
  dEdx_t *dEdx;
  Double_t I = 0, avrz = 0;
  Int_t N70 = NdEdx - (int) (0.3*NdEdx + 0.5); 
  Int_t N60 = NdEdx - (int) (0.4*NdEdx + 0.5);
  Double_t I70 = 0, I60 = 0;
  for (int i=0; i< NdEdx; i++) {
    if (iop == 0) dEdx = &CdEdx[i];
    else if (iop == 1) dEdx = &FdEdx[i];
    else if (iop == 2) dEdx = &dEdxS[i];
    I = (i*I +  dEdx->dEdx)/(i+1);
    cout << Names[iop] << "\t" << i << "\tsector\t" << dEdx->sector << "\trow\t" << dEdx->row
	 << "\tdEdx(keV/cm)\t" << 1.e6*dEdx->dEdx << "\tdx\t" << dEdx->dx << "\tSum\t" << 1.e6*I << "(keV)\tProb\t" 
	 << dEdx->Prob << endl;
    if (iop == 2) {
      if (i < N60) I60 += dEdx->dEdx;
      if (i < N70) I70 += dEdx->dEdx;
      if (i == N60 - 1) {
	I60 /= N60;
	cout << " ======================= I60 \t" << I60 << endl;
      }
      if (i == N70 - 1) {
	I70 /= N70;
	cout << " ======================= I70 \t" << I70 << endl;
      }
    }
    avrz += TMath::Log(dEdx->dEdx);
  }
  avrz /= NdEdx;
  cout << "mean dEdx \t" << I << "\tExp(avrz)\t" << TMath::Exp(avrz) << endl;
}
//________________________________________________________________________________
Double_t StdEdxY2Maker::LikeliHood(Double_t Xlog10bg, Int_t NdEdx, dEdx_t *dEdx) {
  //SecRowMipFitpHist298P02gh1.root  correction to most probable value vs log2(dx)
  //  static const Double_t probdx2[3] = {-3.58584e-02, 4.16084e-02,-1.45163e-02};// 
  const static Double_t ProbCut = 1.e-4;
  const static Double_t GeV2keV = TMath::Log(1.e-6);
  Double_t f = 0;
  St_tpcCorrectionC *dXCorrection = 0;
  if (m_TpcdEdxCorrection)  dXCorrection = m_TpcdEdxCorrection->dXCorrection();
  Int_t k = 0;
  if (dXCorrection) {
    tpcCorrection_st *dXC = ((St_tpcCorrection *)dXCorrection->Table())->GetTable();
    if (dXC) {
      Int_t N = dXC->nrows;
      if (N > 3) k = 3; 
    } 
  }
  for (int i=0;i<NdEdx; i++) {
    Double_t Ylog2dx = TMath::Log2(dEdx[i].dx);
    StTpcdEdxCorrection::ESector l = StTpcdEdxCorrection::kTpcInner;
    if (dEdx[i].row > NumberOfInnerRows) l = StTpcdEdxCorrection::kTpcOuter;
    Double_t sigmaC = 0;
    if (dXCorrection) sigmaC = dXCorrection->CalcCorrection(k,Ylog2dx); 
    Double_t zMostProb = m_Bichsel->GetMostProbableZ(Xlog10bg,Ylog2dx);
    Double_t sigma     = m_Bichsel->GetRmsZ(Xlog10bg,Ylog2dx) + sigmaC;
    Double_t xi = (dEdx[i].dEdxL - GeV2keV - zMostProb)/sigma;
    Double_t  Phi = m_Bichsel->GetProbability(Xlog10bg,Ylog2dx,xi);
    dEdx[i].Prob = Phi/sigma;
    if (dEdx[i].Prob < ProbCut) {
      dEdx[i].Prob = ProbCut; 
    }
    f -= 2*TMath::Log( dEdx[i].Prob );
  }
  return f;
}
//________________________________________________________________________________
void StdEdxY2Maker::Landau(Double_t x, Double_t *val){
#if 0
  // TMath::Log from Landau
  static Double_t params[6] = {
    -3.93739e+00,//    1  p0           5.96123e-03   2.40826e-06  -8.19249e-03
      1.98550e+00,//    2  p1           7.17058e-03   3.54798e-06   2.69672e-03
      1.56338e+00,//    3  p2           1.37436e-03   2.59636e-06   3.43991e-03
      1.44692e+00,//    4  p3           3.29935e-03   7.69892e-07  -3.93145e-02
      -4.93793e-01,//    5  p4           2.46951e-03   2.28224e-06  -8.62375e-03
      1.54585e+00 //    6  p5           2.63794e-03   2.24202e-06  -8.62431e-03
      };
  Double_t dev1 = (x-params[1])/params[2];
  Double_t dev2 = (x-params[4])/params[5];
  Double_t c    = TMath::Exp(params[3]-0.5*dev2*dev2);
  val[0] = params[0]-0.5*dev1*dev1+c;
  val[1] = - (dev1/params[2]+c*dev2/params[5]);
#else
  //  TF1 *LandauF = new TF1("LandauF","exp([0]-0.5*((x-[1])/[2])**2+exp([3]-0.5*((x-[4])/[5])**2+exp([6]-0.5*((x-[7])/[8])**2)))",-4,10);
  Double_t params[9] = {
    -7.74975e+00,
     6.53414e+00,
     1.21524e+00,
     3.31409e+00,
    -2.58291e+00,
     3.51463e+00,
    -3.47755e+00,
     3.77698e-02,
     6.67913e-01};
  Double_t dev1 = (x-params[1])/params[2];
  Double_t dev2 = (x-params[4])/params[5];
  Double_t dev3 = (x-params[7])/params[8];
  Double_t d    = TMath::Exp(params[6]-0.5*dev3*dev3);
  Double_t dp   = -dev3/params[8]*d;
  Double_t c    = TMath::Exp(params[3]-0.5*dev2*dev2 + d);
  Double_t cp   = (-dev2/params[5] + dp)*c;
  val[0]        = params[0]-0.5*dev1*dev1+c;
  val[1]        = - dev1/params[2]+cp;
#endif
}
//________________________________________________________________________________
void StdEdxY2Maker::fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag) {
  Double_t Val[2];
#if 0
  // Ar
  static Double_t sigma_p[3] = {// sigma versus ::log(dX)
       5.31393e-01,//    1  p0  1.33485e-03   7.13072e-07   7.08416e-08
      -1.43277e-01,//    2  p1  3.36846e-03   6.62434e-07  -1.13681e-05
       2.43800e-02); //    3  p2  1.81240e-03   4.02492e-07  -2.08423e-05
#else
  // P10
  // dEdxS->Draw("sigma_z:log(x)/log(2)","","prof")
  static Double_t sigma_p[3] = { 5.66734e-01,   -1.24725e-01,   1.96085e-02};
#endif
  f = 0.;
  gin[0] = 0.;
  gin[1] = 0.;
  for (int i=0;i<NdEdx; i++) {
#if 1
    //    Double_t sigma = StTpcdEdxCorrection::SumSeries(TMath::Log(FdEdx[i].dx),3,sigma_p);
    Double_t X = TMath::Log(FdEdx[i].dx);
    Double_t sigma = sigma_p[2];
    for (int n = 1; n>=0; n--) sigma = X*sigma + sigma_p[n];
    FdEdx[i].zdev    = (FdEdx[i].dEdxL-par[0])/sigma;
    Landau(FdEdx[i].zdev,Val);
    FdEdx[i].Prob = TMath::Exp(Val[0]);
    f      -= Val[0];
    gin[0] += Val[1]/sigma;
#else
    const static Double_t Xlog10bg = TMath::Log10(4.);
    const static Double_t ProbCut = 1.e-4;
    Double_t Ylog2dx = TMath::Log2(FdEdx[i].dx);
    //    Double_t zMostProb = m_Bichsel->GetMostProbableZ(Xlog10bg,Ylog2dx);
    Double_t sigma     = m_Bichsel->GetRmsZ(Xlog10bg,Ylog2dx);
    Double_t xi = (FdEdx[i].dEdxL - par[0])/sigma;
    Double_t Phi = m_Bichsel->GetProbability(Xlog10bg,Ylog2dx,xi);
    FdEdx[i].Prob = Phi/sigma;
    if (FdEdx[i].Prob < ProbCut) FdEdx[i].Prob = ProbCut; 
    f      -= TMath::Log( FdEdx[i].Prob );
    gin[0] += 2.*m_Bichsel->GetProbability(Xlog10bg,Ylog2dx,xi,3)/sigma/FdEdx[i].Prob;
#endif
  }
}
//________________________________________________________________________________
void StdEdxY2Maker::DoFitZ(Double_t &chisq, Double_t &fitZ, Double_t &fitdZ){
  Double_t avz = 0;
  for (int i=0;i<NdEdx;i++) {
    FdEdx[i] = CdEdx[i];
    avz += FdEdx[i].dEdxL;
    for (int j=0;j<i;j++) {// order by rows to account correlations
      if (FdEdx[i].sector == FdEdx[j].sector &&
	  FdEdx[i].row    <  FdEdx[j].row) {
	dEdx_t temp = FdEdx[j];
	FdEdx[j] = FdEdx[i];
	FdEdx[i] = temp;
      }
    }
  }
  if (NdEdx>5) {
    avz /= NdEdx;
    Double_t arglist[10];
    Int_t ierflg = 0;
    m_Minuit->SetFCN(fcn);
    //    m_Minuit->SetPrintLevel(-1);
    if (Debug() < 2) {
      arglist[0] = -1;
      m_Minuit->mnexcm("set print",arglist, 1, ierflg);
    }
    m_Minuit->mnexcm("set NOW",arglist, 0, ierflg);
    m_Minuit->mnexcm("CLEAR",arglist, 0, ierflg);
    arglist[0] = 0.5;
    m_Minuit->mnexcm("SET ERR", arglist ,1,ierflg);
    m_Minuit->mnparm(0, "mean", avz, 0.01,0.,0.,ierflg); //First Guess
    if (Debug() < 2)       arglist[0] = 1.;   // 1.
    else                   arglist[0] = 0.;   // Check gradient 
    m_Minuit->mnexcm("SET GRAD",arglist,1,ierflg);
    arglist[0] = 500;
    arglist[1] = 1.;
    m_Minuit->mnexcm("MIGRAD", arglist ,2,ierflg);
    m_Minuit->mnexcm("HESSE  ",arglist,0,ierflg);
    Double_t edm,errdef;
    Int_t nvpar,nparx,icstat;
    m_Minuit->mnstat(chisq,edm,errdef,nvpar,nparx,icstat);
    m_Minuit->GetParameter(0, fitZ, fitdZ);
  }
  else {
    fitZ = fitdZ = chisq =  -999.;
  }
}
//________________________________________________________________________________
void StdEdxY2Maker::TrigHistos(Int_t iok) {
  static TProfile *BarPressure = 0, *inputTPCGasPressure = 0;
  static TProfile *nitrogenPressure = 0, *gasPressureDiff = 0, *inputGasTemperature = 0;
  static TProfile *outputGasTemperature = 0, *flowRateArgon1 = 0, *flowRateArgon2 = 0, *flowRateMethane = 0;
  static TProfile *percentMethaneIn = 0, *ppmOxygenIn = 0, *flowRateExhaust = 0;
  static TProfile *ppmWaterOut = 0, *ppmOxygenOut = 0, *flowRateRecirculation = 0;
  static TProfile *Center = 0, *Height = 0, *Width = 0,  *CenterPressure = 0;
  static TH1D *Multiplicity; // mult rate 
  static TH2D *Zdc  = 0; // ZdcEastRate versus ZdcWestRate
  static TH1D *ZdcC = 0; // ZdcCoincidenceRate
  static TH1D *BBC   = 0; // BbcCoincidenceRate
  static TH1D *L0   = 0; // L0RateToRich
  if (! iok) {
    TDatime t1(tMin,0); /// min Time and
    TDatime t2(tMax,0); /// max 
    UInt_t i1 = t1.Convert();
    UInt_t i2 = t2.Convert();
    Int_t Nt = (i2 - i1)/(3600); // each hour 
    BarPressure           = new TProfile("BarPressure","barometricPressure (mbar) versus time",Nt,i1,i2);                    
    inputTPCGasPressure   = new TProfile("inputTPCGasPressure","inputTPCGasPressure (mbar) versus time",Nt,i1,i2);           
    nitrogenPressure      = new TProfile("nitrogenPressure","nitrogenPressure (mbar) versus time",Nt,i1,i2);                 
    gasPressureDiff       = new TProfile("gasPressureDiff","gasPressureDiff (mbar) versus time",Nt,i1,i2);                   
    inputGasTemperature   = new TProfile("inputGasTemperature","inputGasTemperature (degrees C) versus time",Nt,i1,i2);      
    outputGasTemperature  = new TProfile("outputGasTemperature","outputGasTemperature (degrees C) versus time",Nt,i1,i2);    
    flowRateArgon1        = new TProfile("flowRateArgon1","flowRateArgon1 (liters/min) versus time",Nt,i1,i2);               
    flowRateArgon2        = new TProfile("flowRateArgon2","flowRateArgon2 (liters/min) versus time",Nt,i1,i2);               
    flowRateMethane       = new TProfile("flowRateMethane","flowRateMethane (liters/min) versus time",Nt,i1,i2);             
    percentMethaneIn      = new TProfile("percentMethaneIn","percentMethaneIn (percent) versus time",Nt,i1,i2);              
    ppmOxygenIn           = new TProfile("ppmOxygenIn","ppmOxygenIn (ppm) versus time",Nt,i1,i2);                            
    flowRateExhaust       = new TProfile("flowRateExhaust","flowRateExhaust (liters/min) versus time",Nt,i1,i2);             
    ppmWaterOut           = new TProfile("ppmWaterOut","ppmWaterOut (ppm) versus time",Nt,i1,i2);                             
    ppmOxygenOut          = new TProfile("ppmOxygenOut","ppmOxygenOut (ppm) versus time",Nt,i1,i2);
    flowRateRecirculation = new TProfile("flowRateRecirculation","flowRateRecirculation (liters/min) versus time",Nt,i1,i2);
    CenterPressure        = new TProfile("CenterPressureP","log(center) vs log(Pressure)",150, 6.84, 6.99);
    Center                = new TProfile("Center","Tpc Gain Monitor center versus Time",Nt,i1,i2);
    Height 		  = new TProfile("Height","Tpc Gain Monitor height versus Time",Nt,i1,i2);
    Width  		  = new TProfile("Width","Tpc Gain Monitor width versus Time",Nt,i1,i2);  
    // trigDetSums histograms
    Zdc                   = new TH2D("Zdc","ZdcEastRate versus ZdcWestRate (log10)",100,0,10,100,0,10);
    ZdcC                  = new TH1D("ZdcC","ZdcCoincidenceRate (log10)",100,0,10);
    Multiplicity          = new TH1D("Multiplicity","Multiplicity (log10)",100,0,10);
    BBC                   = new TH1D("BBC","BbcCoincidenceRate (log10)",100,0,10);
    L0                    = new TH1D("L0","L0RateToRich (log10)",100,0,2);
  }
  else {
    UInt_t date = GetDateTime().Convert();
    St_tpcGas             *tpcGas = 0;
    if (m_TpcdEdxCorrection)  tpcGas = m_TpcdEdxCorrection->tpcGas();
    if (tpcGas) {
      if (BarPressure)           BarPressure->Fill(date,(*tpcGas)[0].barometricPressure);             
      if (inputTPCGasPressure)   inputTPCGasPressure->Fill(date,(*tpcGas)[0].inputTPCGasPressure);    
      if (nitrogenPressure)      nitrogenPressure->Fill(date,(*tpcGas)[0].nitrogenPressure);          
      if (gasPressureDiff)       gasPressureDiff->Fill(date,(*tpcGas)[0].gasPressureDiff);            
      if (inputGasTemperature)   inputGasTemperature->Fill(date,(*tpcGas)[0].inputGasTemperature);    
      if (outputGasTemperature)  outputGasTemperature->Fill(date,(*tpcGas)[0].outputGasTemperature);  
      if (flowRateArgon1)        flowRateArgon1->Fill(date,(*tpcGas)[0].flowRateArgon1);              
      if (flowRateArgon2)        flowRateArgon2->Fill(date,(*tpcGas)[0].flowRateArgon2);              
      if (flowRateMethane)       flowRateMethane->Fill(date,(*tpcGas)[0].flowRateMethane);            
      if (percentMethaneIn)      percentMethaneIn->Fill(date,(*tpcGas)[0].percentMethaneIn);          
      if (ppmOxygenIn)           ppmOxygenIn->Fill(date,(*tpcGas)[0].ppmOxygenIn);                    
      if (flowRateExhaust)       flowRateExhaust->Fill(date,(*tpcGas)[0].flowRateExhaust);            
      if (ppmWaterOut)           ppmWaterOut->Fill(date,(*tpcGas)[0].ppmWaterOut);                    
      if (ppmOxygenOut)          ppmOxygenOut->Fill(date,(*tpcGas)[0].ppmOxygenOut);                
      if (flowRateRecirculation) flowRateRecirculation->Fill(date,(*tpcGas)[0].flowRateRecirculation);
    }
//     if (m_tpcGainMonitor) {
//       if (Center) {
// 	Center->Fill(date,(*m_tpcGainMonitor)[0].center);
// 	if (CenterPressure) CenterPressure->Fill(TMath::Log((*tpcGas)[0].barometricPressure),
// 						 TMath::Log((*m_tpcGainMonitor)[0].center));
//       }
//       if (Height) Height->Fill(date,(*m_tpcGainMonitor)[0].height);
//       if (Width)  Width->Fill(date,(*m_tpcGainMonitor)[0].width);
//     }
    if (m_trig) {
      if (m_trig->zdcWest > 0 &&
	  m_trig->zdcEast > 0 && Zdc) Zdc->Fill(TMath::Log10(m_trig->zdcWest),
						TMath::Log10(m_trig->zdcEast));
      if (m_trig->zdcX > 0 && ZdcC) ZdcC->Fill(TMath::Log10(m_trig->zdcX));
      if (m_trig->bbcX > 0 && BBC) BBC->Fill(TMath::Log10(m_trig->bbcX));
      if (m_trig->L0   > 0 && L0) L0->Fill(TMath::Log10(m_trig->L0));
      if (m_trig->mult > 0 && Multiplicity) Multiplicity->Fill(TMath::Log10(m_trig->mult));
    }
  } // (TESTBIT(m_Mode, kGASHISTOGRAMS))n
}
//________________________________________________________________________________
void StdEdxY2Maker::SpaceCharge(Int_t iok, StEvent* pEvent, StGlobalCoordinate *global, dEdx_t *CdEdx) {
  // SpaceChargeStudy
  static TProfile2D *SpaceCharge = 0, *SpaceChargeU = 0, *SpaceChargeT = 0;
  static TH2D *Space2Charge = 0, *Space2ChargeU = 0, *Space2ChargeT = 0;
  static TH1D *TimeShift = 0;
  static TH3D *Space3Charge = 0, *Space3ChargePRZ = 0,*Space3ChargeShifted = 0;
  if (iok == 0) {
    if (Debug()) gMessMgr->Warning() << "StdEdxY2Maker::SpaceCharge Space Charge Histograms" << endm;
    SpaceCharge   = new TProfile2D("SpaceCharge","dE versus R and Z",85,40.,210.,92,-230.,230.);
    SpaceChargeU  = new TProfile2D("SpaceChargeU","dEU versus R and Z",85,40.,210.,92,-230.,230.);
    SpaceChargeT  = new TProfile2D("SpaceChargeT","dEU (all) versus R and Z",85,40.,210.,92,-230.,230.);
    Space2Charge  = new TH2D("Space2Charge","dE versus R and Z",85,40.,210.,92,-230.,230.);
    Space2ChargeU = new TH2D("Space2ChargeU","dEU versus R and Z",85,40.,210.,92,-230.,230.);
    Space2ChargeT = new TH2D("Space2ChargeT","dEU (all) versus R and Z",85,40.,210.,92,-230.,230.);
    TimeShift     = new TH1D("TimeShift","Shift in time wrt collsion",200,0,2000.);
    Space3Charge  = new TH3D("Space3Charge","Space charged versus Sector, Row and Z",
			     numberOfSectors,1., numberOfSectors+1, NumberOfRows,1., NumberOfRows+1,105,0.,210.);
    Space3Charge->Sumw2();
    Space3ChargePRZ  = new TH3D("Space3ChargePRZ","Space charged versus Phi(rads), Rho and Z",
				36,-TMath::Pi(), TMath::Pi(), 85, 40., 210.,105,-210.,210.);
    Space3ChargePRZ->Sumw2();
    Space3ChargeShifted = new TH3D("Space3ChargeShifted","Space charged shifted in time versus Sector, Row and Z",
				   numberOfSectors,1., numberOfSectors+1, NumberOfRows,1., NumberOfRows+1,105,0.,210.);
    Space3ChargeShifted->Sumw2();
    return;
  }
  if (iok == 1 && pEvent) {
    StTpcHitCollection* TpcHitCollection = pEvent->tpcHitCollection();
    if (TpcHitCollection) {
      UInt_t numberOfSectors = TpcHitCollection->numberOfSectors();
      const Int_t EventId  = GetEventNumber();
      Int_t Ids[24];
      Int_t secs[24];
      Double_t zshift[24];
      const Int_t NMixEvts = 1152;
      for (UInt_t ev = 0; ev< numberOfSectors; ev++) {
	Ids[ev] = (EventId + gRandom->Integer(NMixEvts))%NMixEvts;
	if (TimeShift) TimeShift->Fill(Ids[ev]);
	secs[ev] = gRandom->Integer(24);
	zshift[ev] = 0.18*Ids[ev];
      }
      for (UInt_t i = 0; i< numberOfSectors; i++) {
	StTpcSectorHitCollection* sectorCollection = TpcHitCollection->sector(i);
	if (sectorCollection) {
	  Int_t numberOfPadrows = sectorCollection->numberOfPadrows();
	  for (int j = 0; j< numberOfPadrows; j++) {
	    StTpcPadrowHitCollection *rowCollection = TpcHitCollection->sector(i)->padrow(j);
	    if (rowCollection) {
	      UInt_t NoHits = rowCollection->hits().size();
	      for (UInt_t k = 0; k < NoHits; k++) {
		StTpcHit* tpcHit = TpcHitCollection->sector(i)->padrow(j)->hits().at(k);
		SpaceChargeT->Fill(tpcHit->position().perp(),tpcHit->position().z(),tpcHit->charge());
		if (Space2ChargeT) 
		  Space2ChargeT->Fill(tpcHit->position().perp(),tpcHit->position().z(),tpcHit->charge());
		if (Space3Charge) {
		  Double_t z = TMath::Abs(tpcHit->position().z());
		  Space3Charge->Fill(i+1,j+1,z,tpcHit->charge());
		  if (Space3ChargeShifted) {
		    for (UInt_t ev = 0; ev<numberOfSectors; ev++) {
		      Double_t zz = z - zshift[ev];
		      if (zz > 0) {
			int sec = (i + secs[ev])%24;
			Space3ChargeShifted->Fill(sec+1,j+1,zz,tpcHit->charge());
		      }
		    }
		  }
		}
		if (Space3ChargePRZ) {
		  Space3ChargePRZ->Fill(tpcHit->position().phi(),
					tpcHit->position().perp(),
					tpcHit->position().z(),tpcHit->charge());
		} 
	      }
	    }
	  }
	}
      }
    }
    return;
  }
  if (iok == 2 && global && CdEdx) {
    if (SpaceCharge) SpaceCharge->Fill(global->position().perp(),global->position().z(),CdEdx->dE);
    if (SpaceChargeU) SpaceChargeU->Fill(global->position().perp(),global->position().z(),CdEdx->C[StTpcdEdxCorrection::kUncorrected].dE);
    if (Space2Charge) Space2Charge->Fill(global->position().perp(),global->position().z(),CdEdx->dE);
    if (Space2ChargeU) Space2ChargeU->Fill(global->position().perp(),global->position().z(),CdEdx->C[StTpcdEdxCorrection::kUncorrected].dE);
  }
  return;
}
//________________________________________________________________________________
void StdEdxY2Maker::XyzCheck(StGlobalCoordinate *global, Int_t iokCheck) {
  static TH3D *XYZ = 0, *XYZbad = 0;
  if (! global) {
    if (Debug()) gMessMgr->Warning() << "StdEdxY2Maker::XyzCheck XYZ check Histograms" << endm;
    XYZ    = new TH3D("XYZ","xyz for clusters",80,-200,200,80,-200,200,84,-210,210);
    XYZbad = new TH3D("XYZbad","xyz for clusters with mismatched sectors",
		      80,-200,200,80,-200,200,84,-210,210);
  }
  else 
    if (XYZ) XYZ->Fill( global->position().x(), global->position().y(), global->position().z());
  if (iokCheck && XYZbad) XYZbad->Fill( global->position().x(), global->position().y(), global->position().z());
}
//________________________________________________________________________________
void StdEdxY2Maker::QAPlots(StGlobalTrack* gTrack) {
  static TH2D *fTdEdxP70 = 0, *fTdEdxP70pi = 0, *fTdEdxP70e = 0, *fTdEdxP70K = 0, *fTdEdxP70P = 0;
  static TH2D *fTdEdxPF = 0, *fTdEdxPFpi = 0, *fTdEdxPFe = 0, *fTdEdxPFK = 0, *fTdEdxPFP = 0;
  static StTpcDedxPidAlgorithm PidAlgorithm;
  static StElectron* Electron = StElectron::instance();
  static StPionPlus* Pion = StPionPlus::instance();
  static StKaonPlus* Kaon = StKaonPlus::instance();
  static StProton* Proton = StProton::instance();
  static const Double_t Log10E = TMath::Log10(TMath::Exp(1.));
  if (! gTrack) {
    TFile *f = 0;
    if (TESTBIT(m_Mode, kCalibration)) {
      StBFChain *chain = dynamic_cast<StBFChain*>(GetChain());
      if (chain) f = chain->GetTFile();
      if (f) f->cd();
    }
    fZOfBadHits = new TH1D*[fNZOfBadHits];
    static Char_t *BadCaseses[fNZOfBadHits] = 
    {"it is not used in track fit",     // 0
       "it is flagged ",                // 1
       "track length is inf ",          // 2
       "it does not pass check ",       // 3
       "dx is out interval [0.5,25]",   // 4
       "Sector/Row gain < 0",           // 5 iok + 4
       "drift distance < min || drift distance > max", // 6
       "dE < 0 or dx < 0",              // 7
       "Total no.of rejected clusters"  // 8
       };
    for (Int_t i = 0; i < fNZOfBadHits; i++) 
      fZOfBadHits[i] = new TH1D(Form("ZOfBadHits%i",i),
				Form("Z of rejected clusters  because %s",BadCaseses[i]),
				100,-210,210);                        
    fZOfGoodHits = new TH1D("ZOfGoodHits","Z of accepted clusters",100,-210,210);                        
    fPhiOfBadHits = new TH1D("PhiOfBadHits","Phi of rejected clusters",100, -TMath::Pi(), TMath::Pi());
    fTracklengthInTpcTotal = new TH1D("TracklengthInTpcTotal","Total track in TPC",100,0,200);         
    fTracklengthInTpc = new TH1D("TracklengthInTpc","Track length in TPC used for dE/dx",100,0,200);   
    fTdEdxPF    = new TH2D("TdEdxPF","log10(dE/dx(fit)(keV/cm)) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm", 
			 150,-1.,2., 500,0.,2.5);
    fTdEdxP70    = new TH2D("TdEdxP70","log10(dE/dx(I70)(keV/cm)) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm", 
			 150,-1.,2., 500,0.,2.5);
    fTdEdxP70pi  = new TH2D("TdEdxP70pi","log10(dE/dx(I70)(keV/cm)) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm |nSigmaPion| < 1", 
			 150,-1.,2., 500,0.,2.5);
    fTdEdxP70pi->SetMarkerColor(2);
    fTdEdxP70e   = new TH2D("TdEdxP70e","log10(dE/dx(I70)(keV/cm)) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm |nSigmaElectron| < 1", 
			 150,-1.,2., 500,0.,2.5);
    fTdEdxP70e->SetMarkerColor(3);
    fTdEdxP70K   = new TH2D("TdEdxP70K","log10(dE/dx(I70)(keV/cm)) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm |nSigmaKaon| < 1", 
			 150,-1.,2., 500,0.,2.5);
    fTdEdxP70K->SetMarkerColor(4);
    fTdEdxP70P   = new TH2D("TdEdxP70P","log10(dE/dx(I70)(keV/cm)) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm |nSigmaProton| < 1", 
			 150,-1.,2., 500,0.,2.5);
    fTdEdxP70P->SetMarkerColor(6);

    fTdEdxPFpi  = new TH2D("TdEdxPFpi","log10(dE/dx(Ifit)(keV/cm)) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm and Prob > 68.3", 
			 150,-1.,2., 500,0.,2.5);
    fTdEdxPFpi->SetMarkerColor(2);
    fTdEdxPFe   = new TH2D("TdEdxPFe","log10(dE/dx(Ifit)(keV/cm)) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm and Prob > 68.3", 
			 150,-1.,2., 500,0.,2.5);
    fTdEdxPFe->SetMarkerColor(3);
    fTdEdxPFK   = new TH2D("TdEdxPFK","log10(dE/dx(Ifit)(keV/cm)) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm and Prob > 68.3", 
			 150,-1.,2., 500,0.,2.5);
    fTdEdxPFK->SetMarkerColor(4);
    fTdEdxPFP   = new TH2D("TdEdxPFP","log10(dE/dx(Ifit)(keV/cm)) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm and Prob > 68.3", 
			 150,-1.,2., 500,0.,2.5);
    fTdEdxPFP->SetMarkerColor(6);
    mHitsUsage  = new TH2D("HitsUsage","log10(No.of Used in dE/dx hits) versus log10(Total no. of Tpc Hits",
			   80,0,8,60,0,6);
    if (! f) {
      for (Int_t i = 0; i < fNZOfBadHits; i++) AddHist(fZOfBadHits[i]);           
      AddHist(fZOfGoodHits);
      AddHist(fPhiOfBadHits);         
      AddHist(fTracklengthInTpcTotal);
      AddHist(fTracklengthInTpc);     
      AddHist(fTdEdxP70);
      AddHist(fTdEdxP70pi);
      AddHist(fTdEdxP70e);
      AddHist(fTdEdxP70K);
      AddHist(fTdEdxP70P);
      AddHist(fTdEdxPF);
      AddHist(fTdEdxPFpi);
      AddHist(fTdEdxPFe);
      AddHist(fTdEdxPFK);
      AddHist(fTdEdxPFP);
      AddHist(mHitsUsage);
    }
  }
  else {
    StSPtrVecTrackPidTraits &traits = gTrack->pidTraits();
    static StDedxPidTraits *pid, *pid70 = 0, *pidF = 0;
    static Double_t TrackLength70, TrackLength, I70, D70, fitZ, fitdZ;
    static StProbPidTraits *pidprob = 0;
    static Int_t N70, NF;
    pid = pid70 = pidF = 0;
    TrackLength70 =  TrackLength =  I70 =  D70 = fitZ = fitdZ = 0; 
    N70 = NF = 0;
    for (unsigned int i = 0; i < traits.size(); i++) {
      if (! traits[i]) continue;
      if ( traits[i]->IsZombie()) continue;
      pid = dynamic_cast<StDedxPidTraits*>(traits[i]);
      if (pid) {
	if (pid->method() == kTruncatedMeanIdentifier) {
	  pid70 = pid; I70 = pid70->mean(); N70 = pid70->numberOfPoints();
	  TrackLength70 = pid70->length(); D70 = pid70->errorOnMean();
	}
	if (pid->method() == kLikelihoodFitIdentifier) {
	  pidF = pid;
	  fitZ = TMath::Log(pidF->mean()); NF = pidF->numberOfPoints(); 
	  TrackLength = pidF->length(); fitdZ = pidF->errorOnMean(); 
	}
      }
      else pidprob = dynamic_cast<StProbPidTraits*>(traits[i]);
    }
    StThreeVectorD g3 = gTrack->geometry()->momentum(); // p of global track
    Double_t pMomentum = g3.mag();
    if (pid70 && ! pidF) TrackLength = TrackLength70;
    if (pid70 && TrackLength70 > 40.) { 
      fTdEdxP70->Fill(TMath::Log10(pMomentum), TMath::Log10(I70)+6.);
      const StParticleDefinition* pd = gTrack->pidTraits(PidAlgorithm);
      if (pd) {
	if (TMath::Abs(PidAlgorithm.numberOfSigma(Electron)) < 1) fTdEdxP70e->Fill(TMath::Log10(pMomentum), TMath::Log10(I70)+6.);
	if (TMath::Abs(PidAlgorithm.numberOfSigma(Pion)) < 1) fTdEdxP70pi->Fill(TMath::Log10(pMomentum), TMath::Log10(I70)+6.);
	if (TMath::Abs(PidAlgorithm.numberOfSigma(Kaon)) < 1) fTdEdxP70K->Fill(TMath::Log10(pMomentum), TMath::Log10(I70)+6.);
	if (TMath::Abs(PidAlgorithm.numberOfSigma(Proton)) < 1) fTdEdxP70P->Fill(TMath::Log10(pMomentum), TMath::Log10(I70)+6.);
      }
    }
    if (pidF &&pidprob && TrackLength70 > 40.) { 
      fTdEdxPF->Fill(TMath::Log10(pMomentum), Log10E*fitZ + 6.);
      const StParticleDefinition* pd = gTrack->pidTraits(PidAlgorithm);
      if (pd) {
	if (pidprob->GetChi2Prob(kPidElectron) > 0.683) fTdEdxPFe->Fill(TMath::Log10(pMomentum), Log10E*fitZ + 6.);
	if (pidprob->GetChi2Prob(kPidPion)     > 0.683) fTdEdxPFpi->Fill(TMath::Log10(pMomentum), Log10E*fitZ + 6.);
	if (pidprob->GetChi2Prob(kPidKaon)     > 0.683) fTdEdxPFK->Fill(TMath::Log10(pMomentum), Log10E*fitZ + 6.);
	if (pidprob->GetChi2Prob(kPidProton)   > 0.683) fTdEdxPFP->Fill(TMath::Log10(pMomentum), Log10E*fitZ + 6.);
      }
    }
  }
}
//________________________________________________________________________________
void StdEdxY2Maker::BadHit(Int_t iFlag, const StThreeVectorF &xyz) {
  if (iFlag >= 0 && iFlag < fNZOfBadHits && fZOfBadHits[iFlag]) fZOfBadHits[iFlag]->Fill(xyz.z());
  if (fZOfBadHits[fNZOfBadHits-1]) fZOfBadHits[fNZOfBadHits-1]->Fill(xyz.z());
  if (fPhiOfBadHits!= 0) fPhiOfBadHits->Fill(TMath::ATan2(xyz.y(),xyz.x()));
}
