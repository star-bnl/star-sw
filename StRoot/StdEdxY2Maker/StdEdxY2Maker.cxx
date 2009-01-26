// $Id: StdEdxY2Maker.cxx,v 1.62 2009/01/26 15:30:56 fisyak Exp $
//#define dChargeCorrection
//#define SpaceChargeQdZ
//#define SeparateSums
//#define CompareWithToF
//#define AnodeSum
//#define UseInnerOuterGeometry
//#define UseOuterGeometry
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
#include "TArrayI.h"
#include "TArrayD.h"
// StUtilities
#include "StMagF.h"
#include "StDbUtilities/StMagUtilities.h"
#include "StMessMgr.h" 
#include "BetheBloch.h"
#include "StBichsel/Bichsel.h"
// St_base, StChain
// tables
#include "tables/St_dst_dedx_Table.h"
#ifdef AnodeSum
#include "tables/St_TpcSCAnodeCurrent_Table.h"
#endif
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
//#define __THELIX__
//#define __DoDistortion__
#ifdef __THELIX__
#include "THelixTrack.h"
#endif /* __THELIX__ */
const static StPidParticle NHYPS = kPidHe3;//kPidTriton;
static Int_t tZero= 19950101;
static Int_t tMin = 20060201;
static Int_t tMax = 20060801;
static TDatime t0(tZero,0);
const static Int_t timeOffSet = t0.Convert();
const static Int_t NdEdxMax  = 60;
Int_t   StdEdxY2Maker::NdEdx = 0;
dEdxY2_t *StdEdxY2Maker::CdEdx = 0;
dEdxY2_t *StdEdxY2Maker::FdEdx = 0;
dEdxY2_t *StdEdxY2Maker::dEdxS = 0;
static  Int_t numberOfSectors;
static  Int_t numberOfTimeBins;
static  Int_t NumberOfRows;
static  Int_t NumberOfInnerRows;
static  Int_t NoPads;

const static Double_t pMomin = 0.4; // range for dE/dx calibration
const static Double_t pMomax = 0.5;
#include "dEdxTrackY2.h"
#include "StMemStat.h"
void pmem() { StMemStat::PM();}


//______________________________________________________________________________

// QA histogramss
const static Int_t  fNZOfBadHits = 10;
static TH1F **fZOfBadHits = 0;
static TH1F *fZOfGoodHits = 0;
static TH1F *fPhiOfBadHits = 0;
static TH1F *fTracklengthInTpcTotal = 0;
static TH1F *fTracklengthInTpc = 0;

ClassImp(StdEdxY2Maker);
Bichsel *StdEdxY2Maker::m_Bichsel = 0;
//_____________________________________________________________________________
StdEdxY2Maker::StdEdxY2Maker(const char *name):
  StMaker(name), 
  m_Minuit(0), m_TpcdEdxCorrection(0),  m_Mask(-1), 
  m_trigDetSums(0), m_trig(0),
  mHitsUsage(0)
{
  //  SETBIT(m_Mode,kOldClusterFinder); 
  SETBIT(m_Mode,kPadSelection); 
  m_Minuit = new TMinuit(2);
  memset(mNormal[0]        ,0,sizeof(mNormal     ));
  memset(mRowPosition[0][0],0,sizeof(mRowPosition));
}
//_____________________________________________________________________________
Int_t StdEdxY2Maker::Init(){
  Int_t mode = m_Mode;
  if (m_Mode == -10 || m_Mode == -11 || m_Mode == 0) { // default
    //    SETBIT(m_Mode,kOldClusterFinder); 
    m_Mode = 0;
    if (mode == -11) {SETBIT(m_Mode,kEmbedding);}
    SETBIT(m_Mode,kPadSelection); 
    if (mode == -10) {m_Mask = 0; SETBIT(m_Mask,StTpcdEdxCorrection::kTpcLast);}
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
    if (TESTBIT(m_Mode, kEmbedding))     
      gMessMgr->Warning() << "StdEdxY2Maker::Init This is embedding run" << endm;
  }
  if (! m_Bichsel) m_Bichsel = new Bichsel();
  
  gMessMgr->SetLimit("StdEdxY2Maker:: mismatched Sector",20);
  gMessMgr->SetLimit("StdEdxY2Maker:: pad/TimeBucket out of range:",20);
  gMessMgr->SetLimit("StdEdxY2Maker:: Helix Prediction",20);
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
      TFile *f = GetTFile();
      if (f) {
	f->cd();
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
  SafeDelete(m_TpcdEdxCorrection);
  m_TpcdEdxCorrection = new StTpcdEdxCorrection(m_Mask, Debug());
  
  StTpcCoordinateTransform transform(gStTpcDb);
  for (Int_t sector = 1; sector<= numberOfSectors; sector++) {
    for (Int_t row = 1; row <= NumberOfRows; row++) {
      if (Debug()>1) cout << "========= sector/row ========" << sector << "/" << row << endl;
      StTpcLocalSectorDirection  dirLS(0.,1.,0.,sector,row);  if (Debug()>1) cout << "dirLS\t" << dirLS << endl;
      StTpcLocalDirection        dirL;      
      StTpcLocalSectorAlignedDirection  dirLSA;
      transform(dirLS,dirLSA);   if (Debug()>1) cout << "dirLSA\t" << dirLSA << endl;
      transform(dirLSA,dirL);                     if (Debug()>1) cout << "dirL\t" << dirL << endl;
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
	StTpcLocalSectorAlignedCoordinate lsCoordA;  
	transform(lsCoord,lsCoordA);                       if (Debug()>1) cout << lsCoordA << endl;                   
	transform(lsCoordA, gCoord);                       if (Debug()>1) cout << gCoord << endl;                   
	SafeDelete(mRowPosition[sector-1][row-1][l]);
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
  SafeDelete(m_TpcdEdxCorrection);
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
  StTpcLocalSectorCoordinate        localSect[4], lNext;
#ifdef dChargeCorrection
  static TH3D &Charge= *(new TH3D("Charge","total charge for sector/row integrated  over drift length",
		     24,1.,25.,45,1.,46.,450,-15.,210.));
  static TAxis *Zax = Charge.GetZaxis();
#endif
#ifdef __DoDistortion__
  StTpcLocalCoordinate              local[4];
#endif /* __DoDistortion__  */
  StTpcPadCoordinate                PadOfTrack, Pad, PadNext;
  StTpcLocalSectorDirection         localDirectionOfTrack;
  StTpcLocalSectorAlignedDirection  localADirectionOfTrack;
  StTpcLocalSectorAlignedCoordinate localA, lANext;
  StThreeVectorD xyz[4];
  if (Debug() > 0) timer.start();
  dEdxY2_t CdEdxT[NdEdxMax],FdEdxT[NdEdxMax],dEdxST[NdEdxMax];
  CdEdx = CdEdxT; 
  FdEdx = FdEdxT; 
  dEdxS = dEdxST; 
  St_tpcGas  *tpcGas = m_TpcdEdxCorrection->tpcGas();
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

  if (fabs(bField) < 1.e-5*kilogauss) return kStOK;

  if ((TESTBIT(m_Mode, kSpaceChargeStudy))) SpaceCharge(1,pEvent);
  // no of tpc hits
  Int_t TotalNoOfTpcHits = 0;
  Int_t NoOfTpcHitsUsed  = 0;
  StTpcHitCollection* TpcHitCollection = pEvent->tpcHitCollection();
#ifdef dChargeCorrection
  Charge.Reset();
#endif
  if (TpcHitCollection && ! TESTBIT(m_Mode, kDoNotCorrectdEdx)) {
    UInt_t numberOfSectors = TpcHitCollection->numberOfSectors();
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
#if defined(dChargeCorrection) || defined(SpaceChargeQdZ)
	      if (NoHits > 1) {
		TArrayD zLT(NoHits);TArrayI indxT(NoHits);
		Double_t *zL = zLT.GetArray();
		Int_t    *indx = indxT.GetArray();
		for (UInt_t k = 0; k < NoHits; k++) {
		  StTpcHit *tpcHit = static_cast<StTpcHit *> (hits[k]);
		  if (! tpcHit)  zL[k] = 9999;
		  else 		 {
		    StGlobalCoordinate global(tpcHit->position());
		    transform(global,lNext,i+1,j+1);
		    zL[k] = lNext.position().z();
#ifdef dChargeCorrection
		    Int_t sector = tpcHit->sector();
		    Int_t row    = tpcHit->padrow();
		    Charge.Fill(sector+.5,row+0.5,zL[k], tpcHit->charge());
#endif
		  }
		}
		TMath::Sort(NoHits, zL, indx, kFALSE);
		for (UInt_t k = 0; k < NoHits - 1; k++) {
		  StTpcHit *tpcHitk = static_cast<StTpcHit *> (hits[indx[k]]);
		  StTpcHit *tpcHitk1 = static_cast<StTpcHit *> (hits[indx[k+1]]);
		  if (tpcHitk) tpcHitk->SetNextHit(tpcHitk1);
		}
	      }
#endif /* dChargeCorrection || SpaceChargeQdZ */
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
    node = trackNode[i]; 
    if (!node) continue;
    StGlobalTrack  *gTrack = static_cast<StGlobalTrack *>(node->track(global));
    StPrimaryTrack *pTrack = static_cast<StPrimaryTrack*>(node->track(primary));
    StTptTrack     *tTrack = static_cast<StTptTrack    *>(node->track(tpt));
    StTrack *track = 0;
    StTrack *tracks[3] = {gTrack, pTrack, tTrack};
    if (! TESTBIT(m_Mode, kDoNotCorrectdEdx)) {
      // clean up old Tpc traits if any
      // clean old PiD traits
      for (int l = 0; l < 3; l++) {
	track = tracks[l]; 
	if (track) {
	  StSPtrVecTrackPidTraits &traits = track->pidTraits();
	  unsigned int size = traits.size();
	  if (size) {
#if 1
	    for (unsigned int i = 0; i < size; i++) {
	      StDedxPidTraits *pid = dynamic_cast<StDedxPidTraits*>(traits[i]);
	      if (! pid) continue;
	      if (pid->detector() != kTpcId) continue;
	      traits[i]->makeZombie(1);
	    }
#else
	    traits.erase(traits.begin(),traits.end());
#endif
	  }
	}
      }
    }
    if (! gTrack ||  gTrack->flag() <= 0) continue;
#ifdef UseInnerOuterGeometry
    StThreeVectorD xI, xO;
    StPhysicalHelixD helixO = gTrack->outerGeometry()->helix();
    StPhysicalHelixD helixI = gTrack->geometry()->helix();
#else
#ifdef UseOuterGeometry
    StPhysicalHelixD helix = gTrack->outerGeometry()->helix();
#else
    StPhysicalHelixD helix = gTrack->geometry()->helix();
#endif
#endif
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
#ifdef UseInnerOuterGeometry
      cout << "pxyzI:        " << gTrack->geometry()->momentum() << "\tmag " << gTrack->geometry()->momentum().mag() << endl;
      cout << "pxyzO:        " << gTrack->outerGeometry()->momentum() << "\tmag " << gTrack->outerGeometry()->momentum().mag() << endl;
      cout << "start Point I/O:  " << helixI.at(0) << "/"  << helixO.at(0) << endl;    
#else
#ifdef UseOuterGeometry
      cout << "pxyz:        " << gTrack->outerGeometry()->momentum() << "\tmag " << gTrack->outerGeometry()->momentum().mag() << endl;
#else
      cout << "pxyz:        " << gTrack->geometry()->momentum() << "\tmag " << gTrack->geometry()->momentum().mag() << endl;
#endif
      cout << "start Point: " << helix.at(0) << endl;
#endif
    }
    //    StPtrVecHit hvec = gTrack->detectorInfo()->hits(kTpcId);
    StPtrVecHit hvec = gTrack->detectorInfo()->hits();
    if (hvec.size() && ! TESTBIT(m_Mode, kDoNotCorrectdEdx)) {// if no hits than make only histograms. Works if kDoNotCorrectdEdx mode is set
      Int_t Id = gTrack->key();
      Int_t NoFitPoints = gTrack->fitTraits().numberOfFitPoints(kTpcId);
      NdEdx = 0;
      Double_t TrackLength70 = 0, TrackLength = 0;
      Double_t TrackLengthTotal = 0;
      for (unsigned int j=0; j<hvec.size(); j++) {// hit loop
	if (hvec[j]->detector() != kTpcId) continue;
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
	const StThreeVectorD &normal = *mNormal[sector-1][row-1];
	const StThreeVectorD &middle = *mRowPosition[sector-1][row-1][0];
	const StThreeVectorD &upper  = *mRowPosition[sector-1][row-1][1];
	const StThreeVectorD &lower  = *mRowPosition[sector-1][row-1][2];
	// check that helix prediction is consistent with measurement
#ifdef UseInnerOuterGeometry
	Double_t s[2] = {helixI.pathLength(middle, normal), 
			 helixO.pathLength(middle, normal)};
	Double_t sA[2] = {TMath::Abs(s[0]), TMath::Abs(s[1])};
	if (sA[0] > 1.e3 || sA[1] > 1.e3) {BadHit(2,tpcHit->position()); continue;}
	xI = helixI.at(s[0]);
	xO = helixO.at(s[1]);
	xyz[0] = (sA[0]*xO + sA[1]*xI)/(sA[0] + sA[1]);
	if (Debug() > 1) {
	  cout << " Prediction:\t" << xyz[0] << "\tat sInner=\t" << s[0] << "\tat sOuter=\t" << s[1]<< endl; 
	}
	Double_t s_out[2] = {TMath::Abs(helixI.pathLength(upper, normal)), 
			     TMath::Abs(helixO.pathLength(upper, normal))};
	if (s_out[0] > 1.e4 || s_out[1] > 1.e4)  {BadHit(2, tpcHit->position()); continue;}
	Double_t s_in[2] = {TMath::Abs(helixI.pathLength(lower, normal)), 
			    TMath::Abs(helixO.pathLength(lower, normal))};
	if (s_in[0] > 1.e4 || s_in[1] > 1.e4)  {BadHit(2, tpcHit->position()); continue;}
	xyz[1] = (s_out[0]*helixO.at(s_out[1]) + s_out[1]*helixI.at(s_out[0]))/(s_out[0] + s_out[1]);
	xyz[2] = (s_in[0]*helixO.at(s_in[1]) + s_in[1]*helixI.at(s_in[0]))/(s_in[0] + s_in[1]);
	xyz[3] = StThreeVectorD(tpcHit->position().x(),tpcHit->position().y(),tpcHit->position().z());
	StThreeVectorD dirG = (sA[0]*helixO.momentumAt(s[1],bField) + sA[1]*helixI.momentumAt(s[0],bField))/(sA[0] + sA[1]);
	StGlobalDirection  globalDirectionOfTrack(dirG);
#else
	Double_t s = helix.pathLength(middle, normal);
	if (TMath::Abs(s) > 1.e3) {
	  // reverse helix
	  StPhysicalHelixD helixI(helix.curvature(),
				  -helix.dipAngle(),
				  -helix.phase(),
				  helix.origin(),
				  -helix.h());
	  Double_t s2 = helixI.pathLength(middle, normal);
	  if (TMath::Abs(s2) < TMath::Abs(s)) {
	    helix = helixI;
	    s = s2;
	  }
	}
	if (TMath::Abs(s) > 1.e3) {BadHit(2,tpcHit->position()); continue;}
	xyz[0] = helix.at(s);
	if (Debug() > 1) {
	  cout << " Prediction:\t" << xyz[0] << "\tat s=\t" << s << endl; 
	}
	double s_out = helix.pathLength(upper, normal);
	if (s_out > 1.e4) {BadHit(2, tpcHit->position()); continue;}
	double s_in  = helix.pathLength(lower, normal);
	if (s_in > 1.e4) {BadHit(2, tpcHit->position()); continue;}
	StGlobalCoordinate global(tpcHit->position());
	xyz[1] = helix.at(s_out);
	xyz[2] = helix.at(s_in);
	xyz[3] = StThreeVectorD(tpcHit->position().x(),tpcHit->position().y(),tpcHit->position().z());
	StGlobalDirection  globalDirectionOfTrack(helix.momentumAt(s,bField));
#endif
	for (Int_t l = 0; l < 4; l++) {
	  StGlobalCoordinate globalOfTrack(xyz[l].x(),xyz[l].y(),xyz[l].z());
	  if (Debug() > 2) {
	    Int_t k = l;
	    if (l == 3) k = 0;
	    Double_t D = - (*mRowPosition[sector-1][row-1][k])*normal;
	    Double_t A = normal*normal;
	    Double_t delta = (xyz[l]*normal + D)/TMath::Sqrt(A);
	    if (TMath::Abs(delta) > 1.e-2) {
	      cout << "Out of Plane by " << delta << "\tPlane " 
		   << (*mRowPosition[sector-1][row-1][k]) << "\tNormal " << normal << endl;
	      cout << "Track/hit : " << endl; 
	      cout << "\txyz[0] " << xyz[0] << endl;
	      cout << "\txyz[1] " << xyz[1] << endl; 
	      cout << "\txyz[2] " << xyz[2] << endl;
	      cout << "\txyz[3] " << xyz[3] << endl;
	    }
	  }
	  transform(globalOfTrack,localA,sector,row);
	  transform(localA,localSect[l]);
#ifdef __DoDistortion__
	  transform(localSect[l],local[l]);
	  if (gStTpcDb->ExB()) {
	    Float_t pos[3] = {local[l].position().x(), local[l].position().y(), local[l].position().z()};
	    Float_t posMoved[3];
	    gStTpcDb->ExB()->DoDistortion(pos,posMoved);   // input pos[], returns posMoved[]
	    StThreeVector<double> position(posMoved[0],posMoved[1],posMoved[2]);
	    local[l].setPosition(position);
	    transform(local[l],localSect[l]);
	  }
#endif /* __DoDistortion__ */
	}
	transform(localSect[0],PadOfTrack);
	transform(globalDirectionOfTrack,localADirectionOfTrack,sector,row);
	transform(localADirectionOfTrack,localDirectionOfTrack);
	transform(localSect[3],Pad);
	CdEdx[NdEdx].Reset();
	CdEdx[NdEdx].resXYZ[0] = localSect[3].position().x() - localSect[0].position().x();
	CdEdx[NdEdx].resXYZ[1] = localSect[3].position().y() - localSect[0].position().y();
	CdEdx[NdEdx].resXYZ[2] = localSect[3].position().z() - localSect[0].position().z();
#ifdef UseInnerOuterGeometry
	Double_t dx = (sA[0]*TMath::Abs(s_out[1]-s_in[1]) + 
		       sA[1]*TMath::Abs(s_out[0]-s_in[0]))/(sA[0] + sA[1]);
#else
#ifdef  __THELIX__
	// try THelixTRack model in local Sector coordinate system with taking out distortion corrections
	const Double_t pnts[9] = {
	  localSect[0].position().x(), localSect[0].position().y(), localSect[0].position().z(),
	  localSect[1].position().x(), localSect[1].position().y(), localSect[1].position().z(),
	  localSect[2].position().x(), localSect[2].position().y(), localSect[2].position().z()};
	THelixTrack vHelix(pnts, 3); 
	Double_t padlength;
	if (row<14) padlength = gStTpcDb->PadPlaneGeometry()->innerSectorPadLength();
	else 	  padlength = gStTpcDb->PadPlaneGeometry()->outerSectorPadLength();
	Double_t XYZ[3][3];
	Double_t step[3];
	static Double_t stepMX = 25.;
	for (Int_t l = 0; l < 3; l++) {
	  Double_t y = transform.yFromRow(row);
	  if (l == 2) y += padlength/2.;
	  if (l == 0) y -= padlength/2.;
	  Double_t surf[4] = {-y, 0, 1, 0};
	  step[l] = vHelix.Step(stepMX, surf, 4, XYZ[l]);
	  if (step[l] >= stepMX) {
	    vHelix.Backward();
	    step[l] = - vHelix.Step(stepMX, surf, 4, XYZ[l]);
	    vHelix.Backward();
	  }
	  if (Debug() > 2) {
	    cout << "local Sect.[" << l << "] = " << localSect[l] << endl;
	    const THelixTrack &p = *&vHelix;
	    p.Print("");
	  }
	}
	
	Double_t dx = TMath::Abs(step[2] - step[0]);
	Double_t dxH = TMath::Abs(s_out-s_in);
#else /* ! __THELIX__ */
	Double_t dx = TMath::Abs(s_out-s_in);
#endif /* __THELIX__ */
#endif
	transform(localSect[3],Pad);
	CdEdx[NdEdx].Reset();
	CdEdx[NdEdx].resXYZ[0] = localSect[3].position().x() - localSect[0].position().x();
	CdEdx[NdEdx].resXYZ[1] = localSect[3].position().y() - localSect[0].position().y();
	CdEdx[NdEdx].resXYZ[2] = localSect[3].position().z() - localSect[0].position().z();
	TrackLengthTotal += dx;
	if (! tpcHit->usedInFit()) {
	  BadHit(0,tpcHit->position());
	  continue;}
	if (  tpcHit->flag()) {
	  BadHit(1,tpcHit->position());
	  continue;}
	//________________________________________________________________________________      
	Int_t iokCheck = 0;
	if (sector != Pad.sector() || // ? && TMath::Abs(xyz[0].x()) > 20.0 ||
	    row    != Pad.row()) {
	  gMessMgr->Warning() << "StdEdxY2Maker:: mismatched Sector " 
			      << Pad.sector() << " / " << sector
			      << " Row " << Pad.row() << " / " << row 
			      << "pad " << Pad.pad() << " TimeBucket :" << Pad.timeBucket() 
			      << " x/y/z: " << tpcHit->position()
			      << endm;
	  iokCheck++;
	}
#ifdef Edge
	Int_t EdgePad = 6;
#else
	Int_t EdgePad = 0;
#endif
	if (row > 13) EdgePad = 4;
	//	if (Pad.pad()    <= 1            || // reject cluster in first and last pad
	//	    Pad.pad()    >= gStTpcDb->PadPlaneGeometry()->numberOfPadsAtRow(row) ||
	if (Pad.pad()    <= EdgePad ||
	    Pad.pad()    >= gStTpcDb->PadPlaneGeometry()->numberOfPadsAtRow(row) - EdgePad ||
	    Pad.timeBucket() < 0         ||
	    Pad.timeBucket() >= numberOfTimeBins) {
	  gMessMgr->Warning() << "StdEdxY2Maker:: pad/TimeBucket out of range: " 
			      <<  Pad.pad() << " / " << Pad.timeBucket() << endm;
	  iokCheck++;
	}
	if (sector != PadOfTrack.sector() || 
	    row != PadOfTrack.row() ||	
	    TMath::Abs(Pad.pad()-PadOfTrack.pad()) > 5) {
	  if (Debug() > 1) {
	    gMessMgr->Warning() << "StdEdxY2Maker::	Helix Prediction " 
				<< "Sector = " 
				<< PadOfTrack.sector() << "/" 
				<< sector 
				<< " Row = " << PadOfTrack.row() << "/" 
				<< row 
				<< " Pad = " << PadOfTrack.pad() << "/" 
				<< Pad.pad() 
				<< " from Helix  is not matched with point/" << endm;;
	    gMessMgr->Warning() << "StdEdxY2Maker:: Coordinates Preiction: " 
#ifdef UseInnerOuterGeometry
	                        << "In: " << xI << "\nOut : " << xO << "\n"
#endif
				<< xyz[0] << "/Hit " << tpcHit->position()
				<< endm;
	  }
	  iokCheck++;
	}
	//	if ((TESTBIT(m_Mode, kXYZcheck)) && (TESTBIT(m_Mode, kCalibration))) XyzCheck(&global, iokCheck);
	if ((TESTBIT(m_Mode, kPadSelection)) && iokCheck) {BadHit(3, tpcHit->position()); continue;}
	if ((TESTBIT(m_Mode, kPadSelection)) && (dx < 0.5 || dx > 25.)) {BadHit(4, tpcHit->position()); continue;}
	StTpcdEdxCorrection::ESector kTpcOutIn = StTpcdEdxCorrection::kTpcOuter;
	if (row <= NumberOfInnerRows) kTpcOutIn = StTpcdEdxCorrection::kTpcInner;
	// Corrections
	CdEdx[NdEdx].Reset();
	Double_t PreviousCharge = 0;
	StTpcHit *NextTpcHit = 0;
#ifdef SpaceChargeQdZ
	// next hit
	StTpcHit *currentTpcHit = tpcHit;
	//                            I    O
	//	Int_t PadDif = row <= 13 ? 10 : 6;
	while (currentTpcHit) {
	  const StTpcHit *next = static_cast<const StTpcHit*>(currentTpcHit->nextHit());
	  if (! next) break;
	  StGlobalCoordinate gNext(next->position());
	  transform(gNext,lANext,sector,row);
	  transform(lANext,lNext);
	  transform(lNext,PadNext);
	  //	  if (TMath::Abs(Pad.pad() - PadNext.pad()) <=   PadDif) {
	  Int_t PadDif = (next->padsInHit() + currentTpcHit->padsInHit() + 1)/2;
	  if (TMath::Abs(Pad.pad() - PadNext.pad()) <=  PadDif) {
	    if (! NextTpcHit) NextTpcHit = (StTpcHit *) next;
	    if (next->padsInHit())
	      PreviousCharge += next->charge()/next->padsInHit();
	    break;
	  }
	  currentTpcHit = (StTpcHit *) next;
	}
#endif /* SpaceChargeQdZ */
	CdEdx[NdEdx].DeltaZ = 5.2; 
	CdEdx[NdEdx].QRatio = -2;
	CdEdx[NdEdx].QRatioA = -2.;
	CdEdx[NdEdx].QSumA = 0;
	if (PreviousCharge > 0.0) {
	  CdEdx[NdEdx].QRatioA = TMath::Log(PreviousCharge/(tpcHit->charge()+1e-10));
	  CdEdx[NdEdx].QSumA = TMath::Log10(1. + 1.e6*PreviousCharge);
	}
	if (NextTpcHit) {
	  Double_t dZ = lNext.position().z() - localSect[3].position().z();
	  if (dZ > 0) 	    CdEdx[NdEdx].DeltaZ = TMath::Log(dZ);
	  if (NextTpcHit->charge() > 0.0 && tpcHit->charge() > 0.0) {
	    CdEdx[NdEdx].QRatio = TMath::Log(NextTpcHit->charge()/(tpcHit->charge()+1e-10));
	    if (CdEdx[NdEdx].QRatio < -2.) CdEdx[NdEdx].QRatio = -2.;
	    if (CdEdx[NdEdx].QRatio >  8.) CdEdx[NdEdx].QRatio =  8.;
	  }
	}
	CdEdx[NdEdx].sector = sector; 
	CdEdx[NdEdx].row    = row;
	CdEdx[NdEdx].pad    = (Int_t) Pad.pad();
	CdEdx[NdEdx].edge   = CdEdx[NdEdx].pad;
	if (CdEdx[NdEdx].edge > 0.5*gStTpcDb->PadPlaneGeometry()->numberOfPadsAtRow(row)) 
	  CdEdx[NdEdx].edge += 1 - gStTpcDb->PadPlaneGeometry()->numberOfPadsAtRow(row);
	CdEdx[NdEdx].Npads  = tpcHit->padsInHit();
	CdEdx[NdEdx].Ntbins = tpcHit->pixelsInHit();
	CdEdx[NdEdx].dE     = tpcHit->charge();
	if (tpcHit->idTruth() && tpcHit->qaTruth() > 95) CdEdx[NdEdx].lSimulated = tpcHit->idTruth();
	CdEdx[NdEdx].dx     = dx;
#ifdef __THELIX__
	CdEdx[NdEdx].dxH    = dxH;
#else
	CdEdx[NdEdx].dxH    = 0;
#endif /* __THELIX__ */
	CdEdx[NdEdx].xyz[0] = localSect[3].position().x();
	CdEdx[NdEdx].xyz[1] = localSect[3].position().y();
	CdEdx[NdEdx].xyz[2] = localSect[3].position().z();
	Double_t probablePad = gStTpcDb->PadPlaneGeometry()->numberOfPadsAtRow(row)/2;
	Double_t pitch = (row<14) ?
	  gStTpcDb->PadPlaneGeometry()->innerSectorPadPitch() :
	  gStTpcDb->PadPlaneGeometry()->outerSectorPadPitch();
	Double_t PhiMax = TMath::ATan2(probablePad*pitch, gStTpcDb->PadPlaneGeometry()->radialDistanceAtRow(row));
	CdEdx[NdEdx].PhiR   = TMath::ATan2(CdEdx[NdEdx].xyz[0],CdEdx[NdEdx].xyz[1])/PhiMax;
	CdEdx[NdEdx].xyzD[0] = localDirectionOfTrack.position().x();
	CdEdx[NdEdx].xyzD[1] = localDirectionOfTrack.position().y();
	CdEdx[NdEdx].xyzD[2] = localDirectionOfTrack.position().z();
	CdEdx[NdEdx].ZdriftDistance = localSect[3].position().z();
#ifdef dChargeCorrection
	Int_t iz = Zax->FindBin(CdEdx[NdEdx].ZdriftDistance)-1;
	Double_t dCharge = 1.e6*Charge.GetBinContent(sector,row,iz);
	if (dCharge < 0.2) dCharge = 0.2;
	CdEdx[NdEdx].dCharge = TMath::Log10(dCharge);
#endif
	Bool_t doIT = kTRUE;
	if (TESTBIT(m_Mode,kEmbedding)) doIT = kFALSE;
	Int_t iok = m_TpcdEdxCorrection->dEdxCorrection(CdEdx[NdEdx],doIT);
	if (iok) {BadHit(4+iok, tpcHit->position()); continue;} 
	TrackLength         += CdEdx[NdEdx].dx;
	//	if ((TESTBIT(m_Mode, kSpaceChargeStudy))) SpaceCharge(2,pEvent,&global,&CdEdx[NdEdx]);
	if (fZOfGoodHits) fZOfGoodHits->Fill(tpcHit->position().z());
	if (NdEdx < NdEdxMax) {NdEdx++; NoOfTpcHitsUsed++;}
	if (fTracklengthInTpcTotal) fTracklengthInTpcTotal->Fill(TrackLengthTotal);
	if (fTracklengthInTpc)      fTracklengthInTpc->Fill(TrackLength);
	if (NdEdx > NoFitPoints) 
	  gMessMgr->Error() << "StdEdxY2Maker:: NdEdx = " << NdEdx 
			    << ">  NoFitPoints ="<< NoFitPoints << endm;
      }
      SortdEdx();
#if 0
      PrintdEdx(2);
#endif
      Double_t I70 = 0, D70 = 0;
      Int_t N70 = NdEdx - (int) (0.3*NdEdx + 0.5); 
      if (N70 > 1) {
#ifndef SeparateSums
	Int_t k;
	for (k = 0; k < N70; k++) {
	  I70 += dEdxS[k].dEdx;
	  D70 += dEdxS[k].dEdx*dEdxS[k].dEdx;
	  TrackLength70 += dEdxS[k].dx;
	}
	I70 /= N70; D70 /= N70;
	D70  = TMath::Sqrt(D70 - I70*I70);
	D70 /= I70;
#else
	Int_t k, l;
	Int_t Nio[2] = {0, 0}; 
	for (k = 0; k < NdEdx; k++) {
	  if (dEdxS[k].row <= 13) Nio[StTpcdEdxCorrection::kTpcInner]++;
	  else                    Nio[StTpcdEdxCorrection::kTpcOuter]++;
	}
	Int_t Nio70[2] = {0, 0};
	for (l = 0; l < 2; l++) 	Nio70[l] = Nio[l] - (int) (0.3*Nio[l] + 0.5);
	Double_t Iio70[2] = {0, 0};
	Double_t Dio70[2] = {0, 0};
	Int_t    nio70[2] = {0, 0};
	for (k = 0; k < NdEdx; k++) {
	  l = StTpcdEdxCorrection::kTpcOuter;
	  if (dEdxS[k].row <= NumberOfInnerRows) l = StTpcdEdxCorrection::kTpcInner;
	  if (nio70[l] < Nio70[l]) {
	    Iio70[l] += dEdxS[k].dEdx;
	    Dio70[l] += dEdxS[k].dEdx*dEdxS[k].dEdx;;
	    TrackLength70 += dEdxS[k].dx;
	    nio70[l]++;
	  } 
	}
	Double_t W = 0;
	for (l = 0; l < 2; l++) {
	  if (nio70[l] > 1) {
	    Iio70[l] /= nio70[l]; 
	    Dio70[l] /= nio70[l]; 
	    Dio70[l] = TMath::Sqrt(Dio70[l] - Iio70[l]*Iio70[l]); 
	    if (Dio70[l] > 1e-7) {
	      N70 += nio70[l];
	      W   += 1./(Dio70[l]*Dio70[l]);
	      I70 += Iio70[l]/(Dio70[l]*Dio70[l]);
	    }
	  }	 
	}
	if (W > 0) {
	  I70 /= W;
	  D70  = 1./TMath::Sqrt(W);
	} else {
	  N70 = 0;
	  I70 = D70 = 0;
	}
#endif
	dedx.id_track  =  Id;
	dedx.det_id    =  kTpcId;    // TPC track 
	dedx.method    =  kEnsembleTruncatedMeanId; // == kTruncatedMeanId+1;
	dedx.ndedx     =  N70 + 100*((int) TrackLength);
	dedx.dedx[0]   =  I70;
	dedx.dedx[1]   =  D70;
	if ((TESTBIT(m_Mode, kCalibration)))  // uncorrected dEdx
	  for (int l = 0; l < 3; l++) {
	    if (tracks[l]) tracks[l]->addPidTraits(new StDedxPidTraits(dedx));
	  }
	if (! TESTBIT(m_Mode, kDoNotCorrectdEdx)) { 
	  m_TpcdEdxCorrection->dEdxTrackCorrection(StTpcdEdxCorrection::kTpcLengthCorrection,0,dedx); 
	  m_TpcdEdxCorrection->dEdxTrackCorrection(StTpcdEdxCorrection::kTpcdEdxCor,0,dedx); 
	  dedx.method    =  kTruncatedMeanId;
	  for (int l = 0; l < 3; l++) {if (tracks[l]) tracks[l]->addPidTraits(new StDedxPidTraits(dedx));}
	}
#ifdef I05_75 
	if ((TESTBIT(m_Mode, kCalibration))) { 
	  Double_t I70A = 0, D70A = 0;
	  Int_t N05 = (int) (0.05*NdEdx + 0.5); 
	  Int_t N75 = NdEdx - (int) (0.25*NdEdx + 0.5); 
	  if (N75 > 1) {
	    Int_t k;
	    Int_t N = 0;
	    for (k = N05; k < N75; k++, N++) {
	      I70A += dEdxS[k].dEdx;
	      D70A += dEdxS[k].dEdx*dEdxS[k].dEdx;
	    }
	    I70A /= N; D70A /= N;
	    D70A  = TMath::Sqrt(D70A - I70A*I70A);
	    D70A /= I70A;
	    dedx.id_track  =  Id;
	    dedx.det_id    =  kTpcId;    // TPC track 
	    dedx.method    =  kOtherMethodIdentifier; // == kTruncatedMeanId+1;
	    dedx.ndedx     =  N + 100*((int) TrackLength);
	    dedx.dedx[0]   =  I70A;
	    dedx.dedx[1]   =  D70A;
	    m_TpcdEdxCorrection->dEdxTrackCorrection(StTpcdEdxCorrection::kTpcLengthCorrection,0,dedx); 
	    m_TpcdEdxCorrection->dEdxTrackCorrection(StTpcdEdxCorrection::kTpcdEdxCor,0,dedx); 
	    for (int l = 0; l < 3; l++) {if (tracks[l]) tracks[l]->addPidTraits(new StDedxPidTraits(dedx));}
	  }
	}
#else  /* not I05_75 */
	// 30% truncated for Outer only
	Double_t I70A = 0, D70A = 0;
	Int_t Nouter = 0;
	for (k = 0; k < NdEdx; k++) if (dEdxS[k].row > 13) Nouter++;
	Int_t N70outer = Nouter - (int) (0.3*Nouter + 0.5);
	if (N70outer > 1) {
	  Int_t N = 0;
	  for (k = 0; k < N70outer; k++) {
	    if (dEdxS[k].row <= 13 || N > N70outer) continue;
	    N++;
	    I70A += dEdxS[k].dEdx;
	    D70A += dEdxS[k].dEdx*dEdxS[k].dEdx;
	  }
	  if (N > 0) {
	    I70A /= N; D70A /= N;

	    // Arguably, negative D70A is more than odd considering the
	    // definition but a Sqrt( -0.0000 ) is detected by Insure so
	    // the addition of this check. 
	    D70A  = D70A - I70A*I70A;
	    if (D70A > 0.0){
	      D70A = TMath::Sqrt(D70A)/I70A;
	    } else {
	      D70A = 0;
	    }
	    dedx.id_track  =  Id;
	    dedx.det_id    =  kTpcId;    // TPC track 
	    dedx.method    =  kOtherMethodIdentifier; // == kTruncatedMeanId+1;
	    dedx.ndedx     =  N + 100*((int) TrackLength);
	    dedx.dedx[0]   =  I70A;
	    dedx.dedx[1]   =  D70A;
	    for (int l = 0; l < 3; l++) {if (tracks[l]) tracks[l]->addPidTraits(new StDedxPidTraits(dedx));}
	  }
	}
#endif /* I05_75 */
	// likelihood fit
	Double_t chisq, fitZ, fitdZ;
	DoFitZ(chisq, fitZ, fitdZ);
	if (chisq >0 && chisq < 10000.0) {
	  dedx.id_track  =  Id;
	  dedx.det_id    =  kTpcId;    // TPC track 
	  dedx.method    =  kWeightedTruncatedMeanId;// == kLikelihoodFitId+1;
	  dedx.ndedx     =  NdEdx + 100*((int) TrackLength);
	  dedx.dedx[0]   =  TMath::Exp(fitZ);
	  dedx.dedx[1]   =  fitdZ; 
	  if ((TESTBIT(m_Mode, kCalibration)))  // uncorrected dEdx
	    for (int l = 0; l < 3; l++) {if (tracks[l]) tracks[l]->addPidTraits(new StDedxPidTraits(dedx));}
	  if (! TESTBIT(m_Mode, kDoNotCorrectdEdx)) { 
 	    m_TpcdEdxCorrection->dEdxTrackCorrection(StTpcdEdxCorrection::kTpcLengthCorrection,1,dedx); 
 	    m_TpcdEdxCorrection->dEdxTrackCorrection(StTpcdEdxCorrection::kTpcdEdxCor,1,dedx); 
	    dedx.method    =  kLikelihoodFitId;
	    for (int l = 0; l < 3; l++) {if (tracks[l]) tracks[l]->addPidTraits(new StDedxPidTraits(dedx));}
	  }
	}
	if (! TESTBIT(m_Mode, kDoNotCorrectdEdx)) { 
#ifdef UseInnerOuterGeometry
	  StThreeVectorD g3 = (gTrack->outerGeometry()->momentum() + gTrack->geometry()->momentum())/2.;
#else
#ifdef UseOuterGeometry
	  StThreeVectorD g3 = gTrack->outerGeometry()->momentum(); // p of global track
#else
	  StThreeVectorD g3 = gTrack->geometry()->momentum(); // p of global track
#endif
#endif
	  Double_t pMomentum = g3.mag();
	  Float_t Chisq[NHYPS];
	  for (int hyp = 0; hyp < NHYPS; hyp++) {
	    Double_t bgL10 = TMath::Log10(pMomentum/StProbPidTraits::mPidParticleDefinitions[hyp]->mass());
	    Chisq[hyp] = LikeliHood(bgL10,NdEdx,FdEdx);
	  }
	  for (int l = 0; l < 3; l++) {if (tracks[l]) tracks[l]->addPidTraits(new StProbPidTraits(NdEdx,kTpcId,NHYPS,Chisq));}
	}
      }
    }
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
void StdEdxY2Maker::SortdEdx() {
  Int_t i;
  TArrayI idxT(NdEdx); Int_t *idx = idxT.GetArray();
  TArrayD dT(NdEdx);   Double_t *d = dT.GetArray();
  for (i = 0; i < NdEdx; i++) d[i] = CdEdx[i].dEdx;
  TMath::Sort(NdEdx,d,idx,0);
  for (i=0;i<NdEdx;i++) dEdxS[i] = CdEdx[idx[i]];
  TArrayI rowT(NdEdx); Int_t *r = rowT.GetArray();
  for (i = 0; i < NdEdx; i++) r[i] = CdEdx[i].row;
  TMath::Sort(NdEdx,r,idx,0);
  for (i=0;i<NdEdx;i++) FdEdx[i] = CdEdx[idx[i]];
}
//________________________________________________________________________________
void StdEdxY2Maker::Histogramming(StGlobalTrack* gTrack) {
  const static Double_t GeV2keV = TMath::Log(1.e-6);
  // Histograms
  static TProfile2D *ETA = 0;
  static TH3S *ETA3 = 0;
  static TH2S *Time = 0, *TimeP = 0, *TimeC = 0;
  static TH3S *Pressure = 0, *PressureC = 0, *PressureA = 0;
  static TH3S *PressureT = 0, *PressureTC = 0, *PressureTA = 0;
  // ZBGX
  static TH3S **zbgx = 0;
  // end of ZBGX
  static TH3S *MulRow = 0;
  static TH3S *MulRowC = 0;
  static TH3S *Phi3 = 0;
  static TH3S *Phi3D = 0, *Theta3D = 0;
  //  static TH2S *GainMonitor = 0;
  static TH3S *SecRow3 = 0, *SecRow3C = 0, *SecRow3A = 0;
  static TH3S *Z3 = 0, *Z3A = 0, *Z3C = 0;
  static TH3S *Z3O = 0, *Z3OC = 0, *Z3OA = 0;
  static TH3S *Z3OW = 0, *Z3OWC = 0, *Z3OWA = 0;
#ifdef dChargeCorrection
static TH3D *dCharge3 = 0, *dCharge3C = 0;
#endif
#ifdef SpaceChargeQdZ
  static TH3S *Charge3I = 0, *Charge3O = 0, *Charge3IB = 0, *Charge3OB = 0;
  static TH2S *ChargeA2I = 0, *ChargeA2O = 0, *ChargeA2IB = 0, *ChargeA2OB = 0;
  static TH3S *ChargeSum3 = 0, *ChargeSum3B = 0;
  static TH2S *ChargeQ2I = 0, *ChargeQ2O = 0;
#endif
  // AdcHistos
  static TH2S *AdcI = 0, *AdcO = 0, *AdcIC = 0, *AdcOC = 0, *Adc3I = 0, *Adc3O = 0, *Adc3IC = 0, *Adc3OC = 0;
  static TH3S *AdcIZP = 0, *AdcOZP = 0, *AdcIZN = 0, *AdcOZN = 0;
  static TH2S **Adc3Ip = 0, **Adc3Op = 0;
  // end of AdcHistos
  static TH2S *ZdcCP = 0, *BBCP = 0, *L0P = 0, *MultiplicityPI = 0, *MultiplicityPO = 0;
  //  static TH2S *ctbWest = 0, *ctbEast = 0, *ctbTOFp = 0, *zdcWest = 0, *zdcEast = 0;
  static TH2S *bbcYellowBkg = 0, *bbcBlueBkg = 0;
  // Mip 
  static TH3S *SecRow3Mip = 0;
  // Anode Currents
#ifdef AnodeSum
  static TH3S *AnodeI = 0, *AnodeO = 0;
  static St_TpcSCAnodeCurrent *SCAnodeCurrent = 0;
#endif
  // end of Mip
  static TH1F *hdEI = 0, *hdEUI = 0, *hdERI = 0, *hdEPI = 0, *hdETI = 0, *hdESI = 0, *hdEZI = 0, *hdEMI = 0;
  static TH1F *hdEO = 0, *hdEUO = 0, *hdERO = 0, *hdEPO = 0, *hdETO = 0, *hdESO = 0, *hdEZO = 0, *hdEMO = 0;
  //  static TH2S *PointsB = 0, *Points70B = 0, *PointsBU = 0, *Points70BU = 0, *Points70BA = 0; 
  //  static TH2S *TPointsB = 0, *TPoints70B = 0, *TPointsBU = 0, *TPoints70BU = 0, *TPoints70BA = 0;
  //  static TH2S *MPointsB = 0, *MPoints70B = 0, *MPointsBU = 0, *MPoints70BU = 0, *MPoints70BA = 0;
  static TH2S *Points[25][5];
  static TH2S *TPoints[25][5];
  static TH2S *MPoints[25][5];
  static TH2S *hist70[NHYPS][2], *histz[NHYPS][2];
  static TH2S *hist70B[NHYPS][2], *histzB[NHYPS][2];
  static TH2S *hist70BT[NHYPS][2], *histzBT[NHYPS][2];
  static TProfile *histB[NHYPS][2], *histBB[NHYPS][2]; 
  static TH2S *FitPull = 0, *Pull70 = 0;
  static TTree *ftree = 0;
  static TH2S *ffitZ[NHYPS],  *ffitP[NHYPS], *ffitZU = 0, *ffitZU3 = 0, *ffitZA = 0;
  const static Int_t Nlog2dx = 140;
  const static Double_t log2dxLow = 0.0, log2dxHigh = 3.5;
  static TH2S *inputTPCGasPressureP = 0, *nitrogenPressureP = 0, *gasPressureDiffP = 0, *inputGasTemperatureP = 0;
  static TH2S *outputGasTemperatureP = 0, *flowRateArgon1P = 0, *flowRateArgon2P = 0;
  static TH2S *flowRateMethaneP = 0;
  static TH2S *percentMethaneInP = 0, *percentMethaneInPC = 0, *percentMethaneInPA = 0;
  static TH2S *ppmOxygenInP = 0, *flowRateExhaustP = 0;
  static TH2S *ppmOxygenOutP = 0, *flowRateRecirculationP = 0;
  static TH2S *ppmWaterOutP = 0, *ppmWaterOutPC = 0, *ppmWaterOutPA = 0;
  // ProbabilityPlot
  static TH3S *Prob = 0;
  // end of ProbabilityPlot
  static TH3S *dXdE  = 0, *dXdEA  = 0, *dXdEC  = 0;
  // end of CORRELATION
#ifdef __THELIX__
  static TProfile2D *dxHdx = 0;
  const static Int_t    nZR   =  20;
  const static Double_t ZMinR =   0;
  const static Double_t ZMaxR = 200;
  const static Int_t    nPhiDR   =  40;
  const static Double_t PhiDMinR = -20;
  const static Double_t PhiDMaxR =  20;
#if 0
  const static Int_t    nPhiR    =  40;
  const static Double_t PhiMinR  = -40;
  const static Double_t PhiMaxR  =  40;
#endif
  static TH3S *ResIX = 0, *ResIY = 0, *ResIZ = 0;
  static TH3S *ResOX = 0, *ResOY = 0, *ResOZ = 0;
#endif /* __THELIX__ */
  static TH2S *BaddEdxZPhi70[2], *BaddEdxZPhiZ[2];
  static TH1F *BaddEdxMult70[2], *BaddEdxMultZ[2];
  static dEdxTrackY2 *ftrack = 0;
  static int hMade = 0;
  
  if (! gTrack && !hMade) {
    hMade=2004;
    // book histograms
    Int_t      nZBins = 200;
    Double_t ZdEdxMin = -5;
    Double_t ZdEdxMax =  5;
    Z3  = new TH3S("Z3",
		   "log(dEdx/Pion) versus row and Drift Distance",
		   NumberOfRows,1., NumberOfRows+1,105,0.,210.,nZBins,ZdEdxMin,ZdEdxMax);
    Z3A = new TH3S("Z3A",
		   "log(dEdx/Pion) just after corection versus row and Drift Distance",
		   NumberOfRows,1., NumberOfRows+1,105,0.,210.,nZBins,ZdEdxMin,ZdEdxMax);
    Z3C = new TH3S("Z3C",
		   "log(dEdx/Pion) corrected versus row and Drift Distance",
		   NumberOfRows,1., NumberOfRows+1,105,0.,210.,nZBins,ZdEdxMin,ZdEdxMax);
    Z3O = new TH3S("Z3O",
		   "log(dEdx/Pion) versus row and (Drift)*ppmO2In",
		   NumberOfRows,1., NumberOfRows+1,100,0.,1.e4,nZBins,ZdEdxMin,ZdEdxMax);
    Z3OA= new TH3S("Z3OA",
		   "log(dEdx/Pion) just after correction versus row and (Drift)*ppmO2In",
		   NumberOfRows,1., NumberOfRows+1,100,0.,1.e4,nZBins,ZdEdxMin,ZdEdxMax);
    Z3OC= new TH3S("Z3OC",
		   "log(dEdx/Pion) corrected versus row and (Drift)*ppmO2In",
		   NumberOfRows,1., NumberOfRows+1,100,0.,1.e4,nZBins,ZdEdxMin,ZdEdxMax);
    Z3OW = new TH3S("Z3OW",
		    "log(dEdx/Pion) versus row and (Drift)*ppmO2In*ppmWaterOut*",
		    NumberOfRows,1., NumberOfRows+1,100,0.,1.2e5,nZBins,ZdEdxMin,ZdEdxMax);
    Z3OWA= new TH3S("Z3OWA",
		    "log(dEdx/Pion) just after correction versus row and (Drift)*ppmO2In*ppmWaterOut",
		    NumberOfRows,1., NumberOfRows+1,100,0.,1.2e5,nZBins,ZdEdxMin,ZdEdxMax);
    Z3OWC= new TH3S("Z3OWC",
		    "log(dEdx/Pion) corrected versus row and (Drift)*ppmO2In*ppmWaterOut",
		    NumberOfRows,1., NumberOfRows+1,100,0.,1.2e5,nZBins,ZdEdxMin,ZdEdxMax);
#ifdef dChargeCorrection
    dCharge3 = new TH3D("dCharge3",
			"log(dEdx/Pion) versus row and log10(dE (keV))",
			NumberOfRows,1., NumberOfRows+1, 50,-1.,4.,nZBins,ZdEdxMin,ZdEdxMax);
    dCharge3C= new TH3D("dCharge3C",
			"log(dEdx/Pion) corrected versus row and log10(dE (keV))",
			NumberOfRows,1., NumberOfRows+1, 50,-1.,4.,nZBins,ZdEdxMin,ZdEdxMax);
#endif
#ifdef SpaceChargeQdZ
    Charge3I = new TH3S("Charge3I",
			"log(dEdx/Pion) versus QRatio and DeltaZ for Inner",
			55,-2.5,8.5,40,-2.0,6.0,nZBins,ZdEdxMin,ZdEdxMax);
    Charge3O = new TH3S("Charge3O",
			"log(dEdx/Pion) versus QRatio and DeltaZ for Outer",
			55,-2.5,8.5,40,-2.0,6.0,nZBins,ZdEdxMin,ZdEdxMax);
    Charge3IB = new TH3S("Charge3IB",
			 "log(dEdx/Pion) versus QRatio and DeltaZ for Inner before correction",
			 55,-2.5,8.5,40,-2.0,6.0,nZBins,ZdEdxMin,ZdEdxMax);
    Charge3OB = new TH3S("Charge3OB",
			 "log(dEdx/Pion) versus QRatio and DeltaZ for Outer before correction",
			 55,-2.5,8.5,40,-2.0,6.0,nZBins,ZdEdxMin,ZdEdxMax);
    ChargeA2I = new TH2S("ChargeA2I",
			 "log(dEdx/Pion) versus QRatioA Inner",
			 55,-2.5,8.5,nZBins,ZdEdxMin,ZdEdxMax);
    ChargeA2O = new TH2S("ChargeA2O",
			 "log(dEdx/Pion) versus QRatioA Outer",
			 55,-2.5,8.5,nZBins,ZdEdxMin,ZdEdxMax);
    ChargeQ2I = new TH2S("ChargeQ2I",
			 "log(dEdx/Pion) versus QRatioA Inner",
			 86,-13.8,-5.2,nZBins,ZdEdxMin,ZdEdxMax);
    ChargeQ2O = new TH2S("ChargeQ2O",
			 "log(dEdx/Pion) versus QRatioA Outer",
			 86,-13.8,-5.2,nZBins,ZdEdxMin,ZdEdxMax);
    ChargeA2IB = new TH2S("ChargeA2IB",
			  "log(dEdx/Pion) versus QRatioA Inner before correction",
			  55,-2.5,8.5,nZBins,ZdEdxMin,ZdEdxMax);
    ChargeA2OB = new TH2S("ChargeA2OB",
			  "log(dEdx/Pion) versus QRatioA Outer before correction",
			  55,-2.5,8.5,nZBins,ZdEdxMin,ZdEdxMax);
    ChargeSum3 = new TH3S("ChargeSum3",
			  "log(dEdx/Pion) versus row and log10(1 + dE)",
			  NumberOfRows,1., NumberOfRows+1, 50,-1.,4.,nZBins,ZdEdxMin,ZdEdxMax);
    ChargeSum3B= new TH3S("ChargeSum3B",
			  "log(dEdx/Pion) corrected versus row and log10(1 + dE (keV))",
			  NumberOfRows,1., NumberOfRows+1, 50,-1.,4.,nZBins,ZdEdxMin,ZdEdxMax);
#endif
    // eta
    ETA   = new TProfile2D("ETA",
			   "log(dEdx/Pion) versus Sector I/O and #{eta}",
			   NumberOfRows,1., NumberOfRows+1, 135,-2.25,2.25);
    ETA3  = new TH3S("ETA3",
		     "log(dEdx/Pion) versus Sector I/O and #{eta}",
		     NumberOfRows,1., NumberOfRows+1, 135,-2.25,2.25,nZBins,ZdEdxMin,ZdEdxMax);
    SecRow3= new TH3S("SecRow3","<log(dEdx/Pion)> (uncorrected) versus sector and row",
		      numberOfSectors,1., numberOfSectors+1, NumberOfRows,1., NumberOfRows+1,nZBins,ZdEdxMin,ZdEdxMax);
    SecRow3->SetXTitle("Sector number");
    SecRow3->SetYTitle("Row number");
    SecRow3C= new TH3S("SecRow3C","<log(dEdx/Pion)> (corrected) versus sector and row",
		       numberOfSectors,1., numberOfSectors+1, NumberOfRows,1., NumberOfRows+1,nZBins,ZdEdxMin,ZdEdxMax);
    SecRow3C->SetXTitle("Sector number");
    SecRow3C->SetYTitle("Row number");
    SecRow3A= new TH3S("SecRow3A","<log(dEdx/Pion)> (just after correction) versus sector and row",
		       numberOfSectors,1., numberOfSectors+1, NumberOfRows,1., NumberOfRows+1,nZBins,ZdEdxMin,ZdEdxMax);
    SecRow3A->SetXTitle("Sector number");
    SecRow3A->SetYTitle("Row number");
#ifdef AnodeSum
    AnodeI= new TH3S("AnodeI","<log(dEdx/Pion)> versus sum_curr_1 and row",
		       80 ,0., 8., NumberOfRows,1., NumberOfRows+1,nZBins,ZdEdxMin,ZdEdxMax);
    AnodeI->SetXTitle("sum_curr_1");
    AnodeI->SetYTitle("Row number");

    AnodeO= new TH3S("AnodeO","<log(dEdx/Pion)> versus sum_curr_0 and row",
		       80 ,0., 8., NumberOfRows,1., NumberOfRows+1,nZBins,ZdEdxMin,ZdEdxMax);
    AnodeO->SetXTitle("sum_curr_0");
    AnodeO->SetYTitle("Row number");
   
#endif

    MultiplicityPI = new TH2S("MultiplicityPI","Multiplicity (log10) Inner",100,0,10,nZBins,ZdEdxMin,ZdEdxMax);
    MultiplicityPO = new TH2S("MultiplicityPO","Multiplicity (log10) Outer",100,0,10,nZBins,ZdEdxMin,ZdEdxMax);
    if ((TESTBIT(m_Mode, kAdcHistos))) {
      gMessMgr->Warning() << "StdEdxY2Maker::Histogramming Adc Histograms" << endm;
      AdcI    = new TH2S("AdcI",
			 "log10dE (keV measured) versus log10dE(Predicted) for Inner rows",
			 120,-5.7,-3.3,185,-7.2,-3.5);
      AdcO    = new TH2S("AdcO",
			 "log10dE (keV measured) versus log10dE(Predicted) for Outer rows",
			 150,-5.5,-2.5,165,-6.5,-3.0);
      AdcIC   = new TH2S("AdcIC",
			 "log10dE (keV measured corrected) versus log10dE(Predicted) for Inner rows",
			 120,-5.7,-3.3,185,-7.2,-3.5);
      AdcOC   = new TH2S("AdcOC",
			 "log10dE (keV measured corrected) versus log10dE(Predicted) for Outer rows",
			 150,-5.5,-2.5,165,-6.5,-3.0);
      Adc3I    = new TH2S("Adc3I",
			  "Uniq 3*sigma log10dE (keV measured) versus log10dE(Predicted) for Inner rows",
			  120,-5.7,-3.3,185,-7.2,-3.5);
      Adc3O    = new TH2S("Adc3O",
			  "Uniq 3*sigma log10dE (keV measured) versus log10dE(Predicted) for Outer rows",
			  150,-5.5,-2.5,165,-6.5,-3.0);
      Adc3IC   = new TH2S("Adc3IC",
			  "Uniq 3*sigma log10dE (keV measured corrected) versus log10dE(Predicted) for Inner rows",
			  120,-5.7,-3.3,185,-7.2,-3.5);
      Adc3OC   = new TH2S("Adc3OC",
			  "Uniq 3*sigma log10dE (keV measured corrected) versus log10dE(Predicted) for Outer rows",
			  150,-5.5,-2.5,165,-6.5,-3.0);
      Adc3Ip    = new TH2S*[NHYPS];
      Adc3Op    = new TH2S*[NHYPS];
      for (int hyp = 0; hyp < NHYPS; hyp++) {
	TString nameP(StProbPidTraits::mPidParticleDefinitions[hyp]->name().data());
	nameP.ReplaceAll("-","");
	Adc3Ip[hyp] = new 
	  TH2S(Form("Adc3I%s",nameP.Data()), 
	       Form("%s Uniq 3*sigma log10dE (keV meas.cor.) versus log10dE(Predicted) for Inner rows",
		    nameP.Data()),
	       120,-5.7,-3.3,185,-7.2,-3.5);
	Adc3Op[hyp] = new 
	  TH2S(Form("Adc3O%s",nameP.Data()), 
	       Form("%s Uniq 3*sigma log10dE (keV meas.cor.) versus log10dE(Predicted) for Outer rows",
		    nameP.Data()),
	       120,-5.7,-3.3,185,-7.2,-3.5);
      }
      AdcIZP    = new TH3S("AdcIZP","z (Positive measured) versus dE(Predicted) and Z for Inner rows",
			   200,0,200,100,0.,1.e4,nZBins,ZdEdxMin,ZdEdxMax);
      AdcOZP    = new TH3S("AdcOZP","z (Positive measured) versus dE(Predicted) and Z for Outer rows",
			   500,0,500,100,0.,1.e4,nZBins,ZdEdxMin,ZdEdxMax);
      AdcIZN    = new TH3S("AdcIZN","z (Positive measured) versus dE(Predicted) and Z for Inner rows",
			   200,0,200,100,0.,1.e4,nZBins,ZdEdxMin,ZdEdxMax);
      AdcOZN    = new TH3S("AdcOZN","z (Positive measured) versus dE(Predicted) and Z for Outer rows",
			   500,0,500,100,0.,1.e4,nZBins,ZdEdxMin,ZdEdxMax);
    } // AdcHistos
    ZdcCP  = new TH2S("ZdcCP","ZdcCoincidenceRate (log10)",100,0,10,nZBins,ZdEdxMin,ZdEdxMax);
    BBCP   = new TH2S("BBCP","BbcCoincidenceRate (log10)",100,0,10,nZBins,ZdEdxMin,ZdEdxMax);
    L0P    = new TH2S("L0P","L0RateToRich (log10)",100,0,2,nZBins,ZdEdxMin,ZdEdxMax);
    bbcYellowBkg = new TH2S("bbcYellowBkg","(BBC Eastdelayed) and (BBC West) (log10)",
			    100,0,10,nZBins,ZdEdxMin,ZdEdxMax);
    bbcBlueBkg   = new TH2S("bbcBlueBkg","(BBC Westdelayed) and (BBC East) (log10)",
			    100,0,10,nZBins,ZdEdxMin,ZdEdxMax);
    if ((TESTBIT(m_Mode, kMip))) {
      gMessMgr->Warning() << "StdEdxY2Maker::Histogramming Mip Histograms" << endm;
      SecRow3Mip = new TH3S
	("SecRow3Mip",
	 "<log(dEdx/Pion)>/sigma (corrected) versus row and log2(dx) for MIP particle)",
	 NumberOfRows,1., NumberOfRows+1,Nlog2dx, log2dxLow, log2dxHigh, 200,-5,15);
    } // Mip
    MulRow = new TH3S("MulRow","log(dEdx/Pion) versus log10(Multplicity) and row",
		      100,0,10, NumberOfRows,1., NumberOfRows+1,nZBins,ZdEdxMin,ZdEdxMax);
    MulRowC = new TH3S("MulRowC","log(dEdx/Pion) versus log10(Multplicity) and row corrected",
		       100,0,10, NumberOfRows,1., NumberOfRows+1,nZBins,ZdEdxMin,ZdEdxMax);
    Phi3    = new TH3S("Phi3","log(dEdx/Pion) versus Phi (coordinates, relative) (degrees) and row",
		       210,-1.05,1.05, NumberOfRows,1., NumberOfRows+1,nZBins,ZdEdxMin,ZdEdxMax);
    Phi3D   = new TH3S("Phi3D","log(dEdx/Pion) versus Phi (direction) and row",
		       480,-60,60, NumberOfRows,1., NumberOfRows+1,nZBins,ZdEdxMin,ZdEdxMax);
    Theta3D   = new TH3S("Theta3D","log(dEdx/Pion) versus Theta (direction) (degrees) and row",
		       560,-60,80, NumberOfRows,1., NumberOfRows+1,nZBins,ZdEdxMin,ZdEdxMax);
    const Char_t *N[5] = {"B","70B","BU","70BU","70BA"};
    const Char_t *T[5] = {"dEdx(fit)/Pion",
			  "dEdx(fit_uncorrected)/Pion ",
			  "dEdx(I70)/Pion",
			  "dEdx(I70_uncorrected)/Pion",
			  "dEdx(I70A_uncorrected)/Pion"};
    for (Int_t t = 0; t < 5; t++) {
      for (Int_t z = 0; z < 25; z++) {
	TString ZN("");
	TString ZT("all");
	if (z > 0) {
	  ZN = Form("%02i",z);
	  ZT = Form("Sector %02i",z);
	}
	Points[z][t]    = new TH2S(Form("Points%s%s",N[t],ZN.Data()),
				   Form("%s/sigma versus no. of measured points for %s",T[t],ZT.Data()),
				   50,0,50., 500,-5.,20.);
	TPoints[z][t]   = new TH2S(Form("TPoints%s%s",N[t],ZN.Data()),
				   Form("%s versus no. of measured points for %s",T[t],ZT.Data()),
				   150,10,160., 500,-1.,4.);
	MPoints[z][t]   = new TH2S(Form("MPoints%s%s",N[t],ZN.Data()),
				   Form("%s versus no. of measured points for %s",T[t],ZT.Data()),
				   150,10,160., 500,-1.,4.);
      }
    }
    if ((TESTBIT(m_Mode, kZBGX))) {
      gMessMgr->Warning() << "StdEdxY2Maker::Histogramming make zbgx histograms" << endm;
      zbgx = new TH3S*[2*NHYPS]; 
    }
    for (int hyp=0; hyp<NHYPS;hyp++) {
      TString nameP("fit");
      nameP += StProbPidTraits::mPidParticleDefinitions[hyp]->name().data();
      nameP.ReplaceAll("-","");
      TString title = "fitZ - Pred. for ";
      title += StProbPidTraits::mPidParticleDefinitions[hyp]->name().data();
      title.ReplaceAll("-","");
      title += " versus log10(beta*gamma) for pion";
      ffitZ[hyp]  = new TH2S(nameP.Data(),title.Data(),100,-1,4,100,-5,5);
      ffitZ[hyp]->SetMarkerColor(hyp+2);
      ffitP[hyp] = new TH2S(*ffitZ[hyp]);
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
	    zbgx[2*hyp+sCharge] = new TH3S(nameP.Data(),"z = log(dE/dx) versus log10(beta*gamma) and log2(dx) for unique hyps",
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
	hist70[hyp][sCharge] = new TH2S(name.Data(),title.Data(),100,-1.,4.,600,-2.,4.);
	name += "B";
	title += " Bichsel";
	hist70B[hyp][sCharge] = new TH2S(name.Data(),title.Data(),100,-1.,4.,600,-2.,4.);
	name += "T";
	title += " Unique";
	hist70BT[hyp][sCharge] = new TH2S(name.Data(),title.Data(),100,-1.,4.,600,-2.,4.);
	name = nameP;
	name += "z";
	title = "zFit - log(I(";
	title += nameP;
	title += ")) versus log10(p/m)";
	histz[hyp][sCharge] = new TH2S(name.Data(),title.Data(),100,-1.,4.,600,-2.,4.);
	name += "B";
	title += " Bichsel";
	histzB[hyp][sCharge] = new TH2S(name.Data(),title.Data(),100,-1.,4.,600,-2.,4.);
	name += "T";
	title += " Unique";
	histzBT[hyp][sCharge] = new TH2S(name.Data(),title.Data(),100,-1.,4.,600,-2.,4.);
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
    TDatime t1(tMin,0); // min Time and
    TDatime t2(tMax,0); // max 
    
    UInt_t i1 = t1.Convert() - timeOffSet;
    UInt_t i2 = t2.Convert() - timeOffSet;
    Int_t Nt = (i2 - i1)/(3600); // each hour 
    Pressure   = new TH3S("Pressure","log(dE/dx)_{uncorrected} - log(I(pi)) versus Row & Log(Pressure)", 
			  NumberOfRows,1., NumberOfRows+1,150, 6.84, 6.99,nZBins,ZdEdxMin,ZdEdxMax);
    PressureA  = new TH3S("PressureA","log(dE/dx)_{just after correction} log(I(pi)) versus Log(Pressure)", 
			  NumberOfRows,1., NumberOfRows+1,150, 6.84, 6.99,nZBins,ZdEdxMin,ZdEdxMax);
    PressureC  = new TH3S("PressureC","log(dE/dx)_{corrected} - row & log(I(pi)) versus Log(Pressure)", 
			  NumberOfRows,1., NumberOfRows+1,150, 6.84, 6.99,nZBins,ZdEdxMin,ZdEdxMax);
    PressureT   = new TH3S("PressureT","log(dE/dx)_{uncorrected} - log(I(pi)) versus Row & Log(Pressure*298.2/outputGasTemperature)", 
			   NumberOfRows,1., NumberOfRows+1,150, 6.84, 6.99,nZBins,ZdEdxMin,ZdEdxMax);
    PressureTA  = new TH3S("PressureTA","log(dE/dx)_{just after correction} log(I(pi)) versus Log(Pressure*298.2/outputGasTemperature)", 
			   NumberOfRows,1., NumberOfRows+1,150, 6.84, 6.99,nZBins,ZdEdxMin,ZdEdxMax);
    PressureTC  = new TH3S("PressureTC","log(dE/dx)_{corrected} - row & log(I(pi)) versus Log(Pressure*298.2/outputGasTemperature)", 
			   NumberOfRows,1., NumberOfRows+1,150, 6.84, 6.99,nZBins,ZdEdxMin,ZdEdxMax);
    //     GainMonitor  = new TH2S("GainMonitor","log(dE/dx)_{corrected} - log(I(pi)) versus GainMonitor", 
    // 			    100,70.,120.,nZBins,ZdEdxMin,ZdEdxMax);
    Time   = new TH2S("Time","log(dE/dx)_{uncorrected} - log(I(pi)) versus Date& Time", 
		      Nt,i1,i2,nZBins,ZdEdxMin,ZdEdxMax);
    TimeC  = new TH2S("TimeC","log(dE/dx)_{corrected} - log(I(pi)) versus Date& Time after correction", 
		      Nt,i1,i2,nZBins,ZdEdxMin,ZdEdxMax);
    TimeP  = new TH2S("TimeP","log(dE/dx)_{after pressure correction} - log(I(pi)) versus Date& Time", 
		      Nt,i1,i2,nZBins,ZdEdxMin,ZdEdxMax);
    FitPull= new TH2S("FitPull","(zFit - log(I(pi)))/dzFit  versus track length", 
		      150,10.,160,nZBins,ZdEdxMin,ZdEdxMax);
    Pull70 = new TH2S("Pull70","log(I70/I(pi)))/D70  versus track length", 
		      150,10.,160,nZBins,ZdEdxMin,ZdEdxMax);
    TString title("");
    title = "log(dE/dx/Pion) vs inputTPCGasPressure (mbar)";
    inputTPCGasPressureP = new TH2S("inputTPCGasPressureP","log(dE/dx/Pion) vs inputTPCGasPressure (mbar)",100,2.0,2.2,nZBins,ZdEdxMin,ZdEdxMax);
    nitrogenPressureP = new TH2S("nitrogenPressureP","log(dE/dx/Pion) vs nitrogenPressure (mbar)",100,0.9,1.1,nZBins,ZdEdxMin,ZdEdxMax);
    gasPressureDiffP = new TH2S("gasPressureDiffP","log(dE/dx/Pion) vs gasPressureDiff (mbar)",100,0.6,1.,nZBins,ZdEdxMin,ZdEdxMax);
    inputGasTemperatureP = new TH2S("inputGasTemperatureP","log(dE/dx/Pion) vs inputGasTemperature (degrees C)",100,295.,300.,nZBins,ZdEdxMin,ZdEdxMax);
    outputGasTemperatureP = new TH2S("outputGasTemperatureP","log(dE/dx/Pion) vs outputGasTemperature (degrees C)",100,295.,300.,nZBins,ZdEdxMin,ZdEdxMax);
    flowRateArgon1P = new TH2S("flowRateArgon1P","log(dE/dx/Pion) vs flowRateArgon1 (liters/min)",100,14.95,15.0,nZBins,ZdEdxMin,ZdEdxMax);
    flowRateArgon2P = new TH2S("flowRateArgon2P","log(dE/dx/Pion) vs flowRateArgon2 (liters/min)",100,0.,0.25,nZBins,ZdEdxMin,ZdEdxMax);
    flowRateMethaneP = new TH2S("flowRateMethaneP","log(dE/dx/Pion) vs flowRateMethane (liters/min)",100,1.34,1.37,nZBins,ZdEdxMin,ZdEdxMax);
    percentMethaneInP = new TH2S("percentMethaneInP","log(dE/dx/Pion) vs percentMethaneIn (percent)",100,9.6,10.6,nZBins,ZdEdxMin,ZdEdxMax);
    percentMethaneInPC = new TH2S("percentMethaneInPC","log(dE/dx/Pion)(corrected) vs percentMethaneIn (percent)",100,9.6,10.6,nZBins,ZdEdxMin,ZdEdxMax);
    percentMethaneInPA = new TH2S("percentMethaneInPA","log(dE/dx/Pion)(just after correction) vs percentMethaneIn (percent)",
				  100,9.6,10.6,nZBins,ZdEdxMin,ZdEdxMax);
    ppmOxygenInP = new TH2S("ppmOxygenInP","log(dE/dx/Pion) vs ppmOxygenIn (ppm)",100,20.,30.,nZBins,ZdEdxMin,ZdEdxMax);
    flowRateExhaustP = new TH2S("flowRateExhaustP","log(dE/dx/Pion) vs flowRateExhaust (liters/min)",100,5.,20.,nZBins,ZdEdxMin,ZdEdxMax);
    ppmWaterOutP = new TH2S("ppmWaterOutP","log(dE/dx/Pion) vs ppmWaterOut (ppm)",100,0.,20.,nZBins,ZdEdxMin,ZdEdxMax);
    ppmWaterOutPC = new TH2S("ppmWaterOutPC","log(dE/dx/Pion) corrected vs ppmWaterOut (ppm)",100,0.,20.,nZBins,ZdEdxMin,ZdEdxMax);
    ppmWaterOutPA = new TH2S("ppmWaterOutPA","log(dE/dx/Pion) just after correction vs ppmWaterOut (ppm)",100,0.,20.,nZBins,ZdEdxMin,ZdEdxMax);
    ppmOxygenOutP = new TH2S("ppmOxygenOutP","log(dE/dx/Pion) vs ppmOxygenOut (ppm)",100,0,20,nZBins,ZdEdxMin,ZdEdxMax);
    flowRateRecirculationP = new TH2S("flowRateRecirculationP","log(dE/dx/Pion) vs flowRateRecirculation (liters/min)",
				      100,515.,545.,nZBins,ZdEdxMin,ZdEdxMax);
    ffitZU = new TH2S("fitZU","fitZ - PredPi Unique versus log10(beta*gamma)",100,-1,4,100,-5,5);
    ffitZU->SetMarkerColor(7);
    ffitZU3 = new TH2S("fitZU3","fitZ - PredPi Unique and 3 sigma away versus log10(beta*gamma)",100,-1,4,100,-5,5);
    ffitZU3->SetMarkerColor(6);
    ffitZA = new TH2S("fitZA","fitZ - PredPi All versus log10(beta*gamma)",100,-1,4,100,-5,5);
    ffitZA->SetMarkerColor(1);
    hdEI  = new TH1F("hdEI","log10(dE) Inner after calibration",100,-8.,-3.);
    hdEUI = new TH1F("hdEUI","log10(dEU) Inner before correction",100,-8.,-3.);
    hdERI = new TH1F("hdERI","log10(dER) Inner after row correction  correction",100,-8.,-3.);
    hdEPI = new TH1F("hdEPI","log10(dEP) Inner after Pressure correction",100,-8.,-3.);
    hdETI = new TH1F("hdETI","log10(dET) Inner after TimeScale",100,-8.,-3.);
    hdESI = new TH1F("hdESI","log10(dES) Inner after after TimeScale + SecRow corrections",100,-8.,-3.);
    hdEZI = new TH1F("hdEZI","log10(dEZ) Inner after TimeScale + SecRow + Sec Z corrections ",100,-8.,-3.);
    hdEMI = new TH1F("hdEMI","log10(dEM) Inner after TimeScale + SecRow + Sec Z + Multiplicity corrections",100,-8.,-3.);
    hdEO  = new TH1F("hdEO","log10(dE) Outer after calibration",100,-8.,-3.);
    hdEUO = new TH1F("hdEUO","log10(dEU) Outer before correction",100,-8.,-3.);
    hdERO = new TH1F("hdERO","log10(dER) Outer after row correction  correction",100,-8.,-3.);
    hdEPO = new TH1F("hdEPO","log10(dEP) Outer after Pressure correction",100,-8.,-3.);
    hdETO = new TH1F("hdETO","log10(dET) Outer after TimeScale",100,-8.,-3.);
    hdESO = new TH1F("hdESO","log10(dES) Outer after after TimeScale + SecRow corrections",100,-8.,-3.);
    hdEZO = new TH1F("hdEZO","log10(dEZ) Outer after TimeScale + SecRow + Sec Z corrections ",100,-8.,-3.);
    hdEMO = new TH1F("hdEMO","log10(dEM) Outer after TimeScale + SecRow + Sec Z + Multiplicity corrections",100,-8.,-3.);
    if ((TESTBIT(m_Mode, kProbabilityPlot))) {
      gMessMgr->Warning() << "StdEdxY2Maker::Histogramming Probability Histograms" << endm;
      Prob = new TH3S("Prob","Z(=log(I70/Bichsel)) versun log10(bg) for pion and Probability",
		      100,-1.,4.,10*NHYPS+1,-.1,NHYPS,600,-2.,4.);
    } // ProbabilityPlot
    dXdE  = new TH3S("dXdE","log(dEdx/Pion) versus dX and row",
		     100,0.,5., NumberOfRows,1., NumberOfRows+1,nZBins,ZdEdxMin,ZdEdxMax);
    dXdEA = new TH3S("dXdEA","log(dEdx/Pion) just after correction versus dX and row",
		     100,0.,5., NumberOfRows,1., NumberOfRows+1,nZBins,ZdEdxMin,ZdEdxMax);
    dXdEC = new TH3S("dXdEC","log(dEdx/Pion) corrected versus dX and row",
		     100,0.,5., NumberOfRows,1., NumberOfRows+1,nZBins,ZdEdxMin,ZdEdxMax);
    //    Z3->SetTitle(Form("%s p in [%4f.1,%4f.1]",Z3->GetTitle(),pMomin,pMomax);
    // Create a ROOT Tree and one superbranch
#ifdef __THELIX__
#if 1
    dxHdx   = new TProfile2D("dxHdx",
			     "dX from THelixTrack/dX from StPhysicalHelix versus sector/row",
			     numberOfSectors,1., numberOfSectors+1, NumberOfRows,1., NumberOfRows+1);
#endif
    ResIX = new TH3S("ResIX","x Residual for Inner versus Z and PhiD",nZR,ZMinR,ZMaxR,nPhiDR,PhiDMinR,PhiDMaxR,100,-5.,5.);
    ResIY = new TH3S("ResIY","y Residual for Inner versus Z and PhiD",nZR,ZMinR,ZMaxR,nPhiDR,PhiDMinR,PhiDMaxR,100,-5.,5.);
    ResIZ = new TH3S("ResIZ","z Residual for Inner versus Z and PhiD",nZR,ZMinR,ZMaxR,nPhiDR,PhiDMinR,PhiDMaxR,100,-5.,5.);
    ResOX = new TH3S("ResOX","x Residual for Outer versus Z and PhiD",nZR,ZMinR,ZMaxR,nPhiDR,PhiDMinR,PhiDMaxR,100,-5.,5.);
    ResOY = new TH3S("ResOY","y Residual for Outer versus Z and PhiD",nZR,ZMinR,ZMaxR,nPhiDR,PhiDMinR,PhiDMaxR,100,-5.,5.);
    ResOZ = new TH3S("ResOZ","z Residual for Outer versus Z and PhiD",nZR,ZMinR,ZMaxR,nPhiDR,PhiDMinR,PhiDMaxR,100,-5.,5.);
#endif /* __THELIX__ */
    BaddEdxZPhi70[0] = new TH2S("BaddEdxZPhi700","Z and Phi for I70 below any limits by 5 s.d.",210,-210,210,360,-180.,180.);
    BaddEdxZPhi70[1] = new TH2S("BaddEdxZPhi701","Z and Phi for I70 above any limits by 5 s.d.",210,-210,210,360,-180.,180.);
    BaddEdxMult70[0] = new TH1F("BaddEdxMult700","Multiplicity (log10) for I70 below any limits by 5 s.d.",100,0.,10.);
    BaddEdxMult70[1] = new TH1F("BaddEdxMult701","Multiplicity (log10) for I70 above any limits by 5 s.d.",100,0.,10.);
    BaddEdxZPhiZ[0] = new TH2S("BaddEdxZPhiZ0","Z and Phi for Ifit below any limits by 5 s.d.",210,-210,210,360,-180.,180.);
    BaddEdxZPhiZ[1] = new TH2S("BaddEdxZPhiZ1","Z and Phi for Ifit above any limits by 5 s.d.",210,-210,210,360,-180.,180.);
    BaddEdxMultZ[0] = new TH1F("BaddEdxMultZ0","Multiplicity (log10) for Ifit below any limits by 5 s.d.",100,0.,10.);
    BaddEdxMultZ[1] = new TH1F("BaddEdxMultZ1","Multiplicity (log10) for Ifit above any limits by 5 s.d.",100,0.,10.);
    if ((TESTBIT(m_Mode, kMakeTree))&& !ftree) { 
      gMessMgr->Warning() << "StdEdxY2Maker::Histogramming Make Tree" << endm;
      ftree = new TTree("dEdxT","dEdx tree");
      ftree->SetAutoSave(1000000000);  // autosave when 1 Gbyte written
      Int_t bufsize = 64000;
      Int_t split = 99;
      if (split)  bufsize /= 4;
      ftrack = new dEdxTrackY2();
      TTree::SetBranchStyle(1); //new style by default
      TBranch *branch = ftree->Branch("dEdxTrackY2", "dEdxTrackY2", &ftrack, bufsize,split);
      branch->SetAutoDelete(kFALSE);
    }
    return;
  }
#ifdef AnodeSum
  if (! SCAnodeCurrent ) {
    TDataSet *cond_tpc  = GetDataBase("Conditions/tpc"); 
    if (cond_tpc) SCAnodeCurrent = (St_TpcSCAnodeCurrent *) cond_tpc->Find("TpcSCAnodeCurrent");
  }
#endif 
  // fill histograms 
  St_tpcGas           *tpcGas = 0;
  if ( m_TpcdEdxCorrection) tpcGas = m_TpcdEdxCorrection->tpcGas();
  StThreeVectorD g3 = gTrack->geometry()->momentum(); // p of global track
  Double_t pMomentum = g3.mag();
  Double_t Eta = g3.pseudoRapidity();
  Int_t sCharge = 0;
  if (gTrack->geometry()->charge() < 0) sCharge = 1;
  //  StTpcDedxPidAlgorithm tpcDedxAlgo;
  // dE/dx
  //  StPtrVecTrackPidTraits traits = gTrack->pidTraits(kTpcId);
  StSPtrVecTrackPidTraits &traits = gTrack->pidTraits();
  unsigned int size = traits.size();
  StDedxPidTraits *pid, *pid70 = 0, *pidF = 0, *pid70U = 0, *pidFU = 0, *pid70A = 0;
  StProbPidTraits *pidprob = 0, *p = 0;
#ifdef CompareWithToF
  Int_t NoFitPoints = gTrack->fitTraits().numberOfFitPoints(kTpcId);
  StTofPidTraits* pidTof  = 0;
  static const Int_t IdxH[4] = {kPidProton,kPidKaon,kPidPion,kPidElectron};
#endif
  Double_t I70 = 0, D70 = 0, I70U = 0, D70U = 0, I70A = 0, D70A = 0;
  Double_t fitZ = 0, fitdZ = 1e10, fitZU = 0, fitdZU = 1e10;
  Int_t N70 = 0, NF = 0, N70A = 0;
  Double_t TrackLength70 = 0, TrackLength = 0;
  if (size) {
    for (unsigned int i = 0; i < traits.size(); i++) {
      if (! traits[i]) continue;
      if ( traits[i]->IsZombie()) continue;
      pid = dynamic_cast<StDedxPidTraits*>(traits[i]);
      if (pid && pid->detector() == kTpcId) {
	switch (pid->method()) {
	case kTruncatedMeanId:
	  pid70 = pid; I70 = pid70->mean(); N70 = pid70->numberOfPoints();
	  TrackLength70 = pid70->length(); D70 = pid70->errorOnMean();
	  break;
	case kEnsembleTruncatedMeanId: // == kTruncatedMeanId+1 uncorrected
	  pid70U = pid; I70U = pid70U->mean(); D70U = pid70U->errorOnMean();
	  break;
	case kLikelihoodFitId:
	  pidF = pid;
	  fitZ = TMath::Log(pidF->mean()); NF = pidF->numberOfPoints(); 
	  TrackLength = pidF->length(); fitdZ = pidF->errorOnMean(); 
	  break;
	case kWeightedTruncatedMeanId: // == kLikelihoodFitId+1; uncorrected
	  pidFU = pid; fitZU = TMath::Log(pidFU->mean()); fitdZU = pidFU->errorOnMean(); 
	  break;
	case kOtherMethodIdentifier:
	  pid70A = pid; I70A = pid70A->mean(); D70A = pid70A->errorOnMean(); N70A = pid70A->numberOfPoints();
	  break;
	default:
	  break;
	}
      } else {
	p = dynamic_cast<StProbPidTraits*>(traits[i]);
	if (p) {pidprob = p; continue;}
      }
    }
  }
#ifdef CompareWithToF
  // Tof for primary track only
  StTrackNode *node = gTrack->node();
  if (node) {
    StPrimaryTrack *pTrack = static_cast<StPrimaryTrack*>(node->track(primary));
    if (pTrack) {
      StSPtrVecTrackPidTraits &traitsp = pTrack->pidTraits();
      UInt_t size = traitsp.size();
      for (unsigned int i = 0; i < size; i++) {
	if (traitsp[i]->detector() != kTofId) continue;
	StTofPidTraits* p = dynamic_cast<StTofPidTraits*>(traitsp[i]);
	if (p) {pidTof = p;}
      }
    }
  }
#endif  
  if (pid70 && ! pidF) TrackLength = TrackLength70;
  Double_t Pred[NHYPS],  Pred70[NHYPS];
  Double_t PredB[NHYPS], Pred70B[NHYPS];
  Double_t PredBMN[2], Pred70BMN[2]; 
  Double_t date = GetDateTime().Convert() - timeOffSet;
  Double_t devZ[NHYPS], devZs[NHYPS], devToF[NHYPS];
  memset (devZ, 0, NHYPS*sizeof(Double_t));
  memset (devZs, 0, NHYPS*sizeof(Double_t));
  memset (devToF, 0, NHYPS*sizeof(Double_t));
  Double_t bg = TMath::Log10(pMomentum/StProbPidTraits::mPidParticleDefinitions[kPidPion]->mass());
  Double_t bghyp[NHYPS];
  Int_t l;
  PredBMN[0] = Pred70BMN[0] =  1;
  PredBMN[1] = Pred70BMN[1] = -1;
  for (l = kPidElectron; l < NHYPS; l += 1) {
    bghyp[l] = TMath::Log10(pMomentum/StProbPidTraits::mPidParticleDefinitions[l]->mass());
    PredB[l]   = 1.e-6*TMath::Exp(m_Bichsel->GetMostProbableZ(bghyp[l],1.0)); 
    if (PredB[l] < PredBMN[0]) PredBMN[0] = PredB[l];
    if (PredB[l] > PredBMN[1]) PredBMN[1] = PredB[l];
    Pred70B[l] = 1.e-6*m_Bichsel->GetI70(bghyp[l],1.0); 
    if (Pred70B[l] < Pred70BMN[0]) Pred70BMN[0] = Pred70B[l];
    if (Pred70B[l] > Pred70BMN[1]) Pred70BMN[1] = Pred70B[l];
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
    // Bad dE/dx
    Double_t L10Mult = -1;
    if (m_trig) L10Mult = m_trig->mult;
    //    StThreeVectorD pxyz = gTrack->geometry()->momentum();
    StThreeVectorD  xyz = gTrack->geometry()->helix().at(0);
    Double_t ZG  = xyz.z();
    Double_t PhiDG = 180*xyz.phi();
    if (Pred70BMN[1] > 0 && D70 > 0) {
      if (TMath::Log(I70/Pred70BMN[0]) < -5*D70) {
	BaddEdxZPhi70[0]->Fill(ZG,PhiDG);
	BaddEdxMult70[0]->Fill(L10Mult);
      }
      if (TMath::Log(I70/Pred70BMN[1]) > 5*D70) {
	BaddEdxZPhi70[1]->Fill(ZG,PhiDG);
	BaddEdxMult70[1]->Fill(L10Mult);
      }
    }
    if (PredBMN[1] > 0 && fitdZ > 0) {
      if (fitZ - TMath::Log(PredBMN[0]) < -5*fitdZ) {
	BaddEdxZPhiZ[0]->Fill(ZG,PhiDG);
	BaddEdxMultZ[0]->Fill(L10Mult);
      }
      if (fitZ - TMath::Log(PredBMN[1]) > 5*fitdZ) {
	BaddEdxZPhiZ[1]->Fill(ZG,PhiDG);
	BaddEdxMultZ[1]->Fill(L10Mult);
      }
    }
#ifdef CompareWithToF
    // use ToF 
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
  if (PiDkeyU3 >= 0) {
    l = PiDkeyU3;
    if (pid70 && TrackLength70 > 40.) {
      hist70BT[l][sCharge]->Fill(bghyp[l],TMath::Log(I70/Pred70B[l]));
    }
    if (pidF && TrackLength > 40.) {
      histzBT[l][sCharge]->Fill(bghyp[l],fitZ - TMath::Log(PredB[l]));
    }
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
  Int_t zCase = -1;
  for (Int_t k = 0; k < NdEdx; k++) {
    if (zCase > 0) {
      if (zCase == FdEdx[k].sector) continue;
      zCase = -1; break;
    } else {
      zCase = FdEdx[k].sector;
    }
  }
  if (pidF) {
    Points[0][0]->Fill(NdEdx,(fitZ-TMath::Log(PredB[kPidPion]))/fitdZ);
    TPoints[0][0]->Fill(TrackLength,fitZ-TMath::Log(PredB[kPidPion]));
    if (pMomentum > pMomin && pMomentum < pMomax) MPoints[0][0]->Fill(TrackLength,fitZ-TMath::Log(PredB[kPidPion]));
    if (zCase > 0) {
      Points[zCase][0]->Fill(NdEdx,(fitZ-TMath::Log(PredB[kPidPion]))/fitdZ);
      TPoints[zCase][0]->Fill(TrackLength,fitZ-TMath::Log(PredB[kPidPion]));
      if (pMomentum > pMomin && pMomentum < pMomax) MPoints[zCase][0]->Fill(TrackLength,fitZ-TMath::Log(PredB[kPidPion]));
    }
    FitPull->Fill(TrackLength,(fitZ - TMath::Log(PredB[kPidPion]))/fitdZ);
    if (pidFU) {
      Points[0][2]->Fill(NdEdx,(fitZU-TMath::Log(PredB[kPidPion]))/fitdZ);
      TPoints[0][2]->Fill(TrackLength,fitZU-TMath::Log(PredB[kPidPion]));
      if (pMomentum > pMomin && pMomentum < pMomax) MPoints[0][2]->Fill(TrackLength,fitZU-TMath::Log(PredB[kPidPion]));
      if (zCase > 0) {
	Points[zCase][2]->Fill(NdEdx,(fitZU-TMath::Log(PredB[kPidPion]))/fitdZ);
	TPoints[zCase][2]->Fill(TrackLength,fitZU-TMath::Log(PredB[kPidPion]));
	if (pMomentum > pMomin && pMomentum < pMomax) MPoints[zCase][2]->Fill(TrackLength,fitZU-TMath::Log(PredB[kPidPion]));
      }
    }
  }
  if (pid70) {
    Points[0][1]->Fill(N70,TMath::Log(I70/PredB[kPidPion])/D70);
    TPoints[0][1]->Fill(TrackLength,TMath::Log(I70/Pred70B[kPidPion]));
    if (pMomentum > pMomin && pMomentum < pMomax) MPoints[0][1]->Fill(TrackLength,TMath::Log(I70/Pred70B[kPidPion]));
    if (zCase > 0) {
      Points[zCase][1]->Fill(N70,TMath::Log(I70/PredB[kPidPion])/D70);
      TPoints[zCase][1]->Fill(TrackLength,TMath::Log(I70/Pred70B[kPidPion]));
      if (pMomentum > pMomin && pMomentum < pMomax) MPoints[zCase][1]->Fill(TrackLength,TMath::Log(I70/Pred70B[kPidPion]));
    }
    Pull70->Fill(TrackLength,TMath::Log(I70/Pred70B[kPidPion])/D70);
    if (pid70U) {
      Points[0][3]->Fill(N70,TMath::Log(I70U/PredB[kPidPion])/D70);
      TPoints[0][3]->Fill(TrackLength,TMath::Log(I70U/Pred70B[kPidPion]));
      if (pMomentum > pMomin && pMomentum < pMomax) MPoints[0][3]->Fill(TrackLength,TMath::Log(I70U/Pred70B[kPidPion]));
      if (zCase > 0) {
	Points[zCase][3]->Fill(N70,TMath::Log(I70U/PredB[kPidPion])/D70);
	TPoints[zCase][3]->Fill(TrackLength,TMath::Log(I70U/Pred70B[kPidPion]));
	if (pMomentum > pMomin && pMomentum < pMomax) MPoints[zCase][3]->Fill(TrackLength,TMath::Log(I70U/Pred70B[kPidPion]));
      }
    }
    if (pid70A) {
      Points[0][4]->Fill(N70A,TMath::Log(I70A/PredB[kPidPion])/D70);
      TPoints[0][4]->Fill(TrackLength,TMath::Log(I70A/Pred70B[kPidPion]));
      if (pMomentum > pMomin && pMomentum < pMomax) MPoints[0][4]->Fill(TrackLength,TMath::Log(I70A/Pred70B[kPidPion]));
      if (zCase > 0) {
	Points[zCase][4]->Fill(N70A,TMath::Log(I70A/PredB[kPidPion])/D70);
	TPoints[zCase][4]->Fill(TrackLength,TMath::Log(I70A/Pred70B[kPidPion]));
	if (pMomentum > pMomin && pMomentum < pMomax) MPoints[zCase][4]->Fill(TrackLength,TMath::Log(I70A/Pred70B[kPidPion]));
      }
    }
  }
  if (TrackLength > 20) { 
    //  if (NoFitPoints >= 20) { 
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
      if (pMomentum > pMomin && pMomentum < pMomax &&TrackLength > 40 ) { // Momentum cut
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
	  if (m_trig->bbcYellowBkg > 0 && bbcYellowBkg) 
	    bbcYellowBkg->Fill(TMath::Log10(m_trig->bbcYellowBkg), FdEdx[k].dEdxN);
	  if (m_trig->bbcBlueBkg > 0 && bbcBlueBkg) 
	    bbcBlueBkg->Fill(TMath::Log10(m_trig->bbcBlueBkg), FdEdx[k].dEdxN);
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
	if (SecRow3 )  SecRow3->Fill(FdEdx[k].sector+0.5,FdEdx[k].row+0.5,FdEdx[k].C[StTpcdEdxCorrection::kTpcSecRow-1].dEdxN);
	if (SecRow3A) SecRow3A->Fill(FdEdx[k].sector+0.5,FdEdx[k].row+0.5,FdEdx[k].C[StTpcdEdxCorrection::kTpcSecRow].dEdxN);
	if (SecRow3C) SecRow3C->Fill(FdEdx[k].sector+0.5,FdEdx[k].row+0.5,FdEdx[k].dEdxN);
#ifdef AnodeSum
	if (SCAnodeCurrent && AnodeI && AnodeO) {
	  TpcSCAnodeCurrent_st *row = SCAnodeCurrent->GetTable();
	  AnodeI->Fill(row->SUM_CURR_1,FdEdx[k].row+0.5,FdEdx[k].dEdxN);
	  AnodeO->Fill(row->SUM_CURR_0,FdEdx[k].row+0.5,FdEdx[k].dEdxN);
	}
#endif	
	//Double_t xyz[3]  = {FdEdx[k].xyz[0],FdEdx[k].xyz[1],FdEdx[k].xyz[2]};
	Double_t xyzD[3] = {FdEdx[k].xyzD[0],FdEdx[k].xyzD[1],FdEdx[k].xyzD[2]};
	//Double_t Phi  = 180./TMath::Pi()*TMath::ATan2(xyz[0],xyz[1]);
	Double_t PhiD = 180./TMath::Pi()*TMath::ATan2(xyzD[0],xyzD[1]); 
	Double_t ThetaD = 180./TMath::Pi()*TMath::ATan2(-xyzD[2],TMath::Sqrt(xyzD[0]*xyzD[0]+xyzD[1]*xyzD[1]));
#ifdef __THELIX__
	if (FdEdx[k].row <= 13) {
	  if (ResIX) ResIX->Fill(FdEdx[k].xyz[2],PhiD,FdEdx[k].resXYZ[0]);
	  if (ResIY) ResIY->Fill(FdEdx[k].xyz[2],PhiD,FdEdx[k].resXYZ[1]);
	  if (ResIZ) ResIZ->Fill(FdEdx[k].xyz[2],PhiD,FdEdx[k].resXYZ[2]);
	}
	else {
	  if (ResOX) ResOX->Fill(FdEdx[k].xyz[2],PhiD,FdEdx[k].resXYZ[0]);
	  if (ResOY) ResOY->Fill(FdEdx[k].xyz[2],PhiD,FdEdx[k].resXYZ[1]);
	  if (ResOZ) ResOZ->Fill(FdEdx[k].xyz[2],PhiD,FdEdx[k].resXYZ[2]);
	}
#endif
	if (Phi3)	  Phi3->Fill(FdEdx[k].PhiR,FdEdx[k].row+0.5,FdEdx[k].dEdxN);
	if (Phi3D) 	  Phi3D->Fill(PhiD,FdEdx[k].row+0.5,FdEdx[k].dEdxN);
	if (Theta3D) 	  Theta3D->Fill(ThetaD,FdEdx[k].row+0.5,FdEdx[k].dEdxN);
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
#ifdef dChargeCorrection
	if (dCharge3)    dCharge3->Fill(FdEdx[k].row+0.5,FdEdx[k].dCharge,  FdEdx[k].C[StTpcdEdxCorrection::kTpcdCharge-1].dEdxN);
	if (dCharge3C)  dCharge3C->Fill(FdEdx[k].row+0.5,FdEdx[k].dCharge,  FdEdx[k].dEdxN);
#endif
#ifdef SpaceChargeQdZ
	if (FdEdx[k].row <=13) {
	  if (Charge3I) Charge3I->Fill(FdEdx[k].QRatio,FdEdx[k].DeltaZ, FdEdx[k].dEdxN);
	  if (ChargeA2I) ChargeA2I->Fill(FdEdx[k].QRatioA, FdEdx[k].dEdxN);
	  if (ChargeQ2I) ChargeQ2I->Fill(FdEdx[k].QRatioA + TMath::Log(FdEdx[k].dE), FdEdx[k].dEdxN);
	  if (Charge3IB) Charge3IB->Fill(FdEdx[k].QRatio,FdEdx[k].DeltaZ, FdEdx[k].C[StTpcdEdxCorrection::kTpcdCharge-1].dEdxN);
	  if (ChargeA2IB) ChargeA2IB->Fill(FdEdx[k].QRatioA, FdEdx[k].C[StTpcdEdxCorrection::kTpcdCharge-1].dEdxN);
	}
	else {
	  if (Charge3O) Charge3O->Fill(FdEdx[k].QRatio,FdEdx[k].DeltaZ, FdEdx[k].dEdxN);
	  if (ChargeA2O) ChargeA2O->Fill(FdEdx[k].QRatioA, FdEdx[k].dEdxN);
	  if (ChargeQ2O) ChargeQ2O->Fill(FdEdx[k].QRatioA + TMath::Log(FdEdx[k].dE), FdEdx[k].dEdxN);
	  if (Charge3OB) Charge3OB->Fill(FdEdx[k].QRatio,FdEdx[k].DeltaZ, FdEdx[k].C[StTpcdEdxCorrection::kTpcdCharge-1].dEdxN);
	  if (ChargeA2OB) ChargeA2OB->Fill(FdEdx[k].QRatioA, FdEdx[k].C[StTpcdEdxCorrection::kTpcdCharge-1].dEdxN);
	}
	if (ChargeSum3)  ChargeSum3->Fill(FdEdx[k].row+0.5,FdEdx[k].QSumA,  FdEdx[k].dEdxN);
	if (ChargeSum3B) ChargeSum3B->Fill(FdEdx[k].row+0.5,FdEdx[k].QSumA,  FdEdx[k].C[StTpcdEdxCorrection::kTpcdCharge-1].dEdxN);
#endif
	if (m_trig && m_trig->mult > 0) {
	  if (MulRow)   MulRow->Fill(TMath::Log10(m_trig->mult),FdEdx[k].row+0.5,FdEdx[k].C[StTpcdEdxCorrection::kMultiplicity-1].dEdxN);
	  if (MulRowC) MulRowC->Fill(TMath::Log10(m_trig->mult),FdEdx[k].row+0.5,FdEdx[k].dEdxN);
	}
	if (dXdE )  dXdE->Fill(FdEdx[k].dx,FdEdx[k].row+0.5,FdEdx[k].C[StTpcdEdxCorrection::kdXCorrection-1].dEdxN);
	if (dXdEA) dXdEA->Fill(FdEdx[k].dx,FdEdx[k].row+0.5,FdEdx[k].C[StTpcdEdxCorrection::kdXCorrection].dEdxN);
	if (dXdEC) dXdEC->Fill(FdEdx[k].dx,FdEdx[k].row+0.5,FdEdx[k].dEdxN);
      }
      if (TESTBIT(m_Mode, kZBGX) && PiDkeyU3 >= 0 && zbgx) 
	zbgx[2*PiDkeyU3+sCharge]->Fill(bghyp[PiDkeyU3],TMath::Log(FdEdx[k].dx)/TMath::Log(2.),FdEdx[k].dEdxL-GeV2keV);
    }
    if ((TESTBIT(m_Mode, kCORRELATION))) Correlations();
  }
#ifdef CompareWithToF
  // dE/dx tree
  if (NdEdx > 0  && ftrack && ftree && TrackLength70 > 20.0 && pidTof) {
    ftrack->Clear();
    ftrack->sCharge = 1 - 2*sCharge;
    ftrack->p = pMomentum;
    ftrack->pX = g3.x();
    ftrack->pY = g3.y();
    ftrack->pZ = g3.z();
    ftrack->R0 = gTrack->geometry()->origin().mag();
    ftrack->Z0 = gTrack->geometry()->origin().z();
    ftrack->Phi0 = gTrack->geometry()->origin().phi();
    ftrack->NoFitPoints = NoFitPoints;
    ftrack->NdEdx = NdEdx;
    ftrack->N70 = N70;
    ftrack->I70 = I70;
    ftrack->D70 = D70;
    ftrack->TrackLength70 = TrackLength70;
    ftrack->fitZ = fitZ;
    ftrack->fitdZ = fitdZ;
    ftrack->TrackLength = TrackLength;
    Double_t *h = 0; 
    for (Int_t l = 0; l < 4; l++) {
      Int_t k = IdxH[l];
      if      (k == kPidProton  ) h = &ftrack->PredP;
      else if (k == kPidKaon    ) h = &ftrack->PredK;
      else if (k == kPidPion    ) h = &ftrack->Predpi;
      else if (k == kPidElectron) h = &ftrack->PredE;
      else continue;
      h[0] = PredB[k];
      h[1] = Pred70B[k];
      h[2] = devZs[k];
      h[3] = devZ[k];
      h[4] = devToF[k];
      if (pidprob) h[5] = pidprob->GetProbability(k);
      else         h[5] = -1;
    }
    for (Int_t k = 0; k < NdEdx; k++) {
      ftrack->AddPoint(FdEdx[k]);
    }
    ftree->Fill();
  }
#endif
  return;
}
//_____________________________________________________________________________
void StdEdxY2Maker::PrintdEdx(Int_t iop) {
  const Int_t NOpts = 20;
  const Char_t *Names[NOpts] = {"CdEdx","FdEdx","dEdxS","dEdxU","dEdxR",
				"dEdxS","dEdxS","dEdxP","dEdxt","dEdxO",
				"dEdxM","dEdxZ","dEdxm","dEdxT","dEdxW",
				"dEdxC","dEdxE","dEdxp","dEdxX","dEdxd"};
  if (iop < 0 || iop >= NOpts) return;
  dEdxY2_t *pdEdx = 0; 
  Double_t dEdx;
  Double_t I = 0;
  Int_t N70 = NdEdx - (int) (0.3*NdEdx + 0.5); 
  Int_t N60 = NdEdx - (int) (0.4*NdEdx + 0.5);
  Double_t I70 = 0, I60 = 0;
  Double_t avrz = 0;
  for (int i=0; i< NdEdx; i++) {
    dEdx = 0;
    if (iop == 0)      {pdEdx = &CdEdx[i]; dEdx = CdEdx[i].dEdx;}
    else if (iop == 1) {pdEdx = &FdEdx[i]; dEdx = FdEdx[i].dEdx;}
    else if (iop == 2) {pdEdx = &dEdxS[i]; dEdx = dEdxS[i].dEdx;}
    else if (iop >= 3) {pdEdx = &FdEdx[i]; dEdx = FdEdx[i].C[StTpcdEdxCorrection::kUncorrected+iop-3].dEdx;}
    I = (i*I +  pdEdx->dEdx)/(i+1);
// #ifndef __THELIX__
//     cout << Names[iop] << "\t" << i << "\tsector\t" << dEdx->sector << "\trow\t" << dEdx->row
// 	 << "\tdEdx(keV/cm)\t" << 1.e6*dEdx->dEdx << "\tdx\t" << dEdx->dx << "\tSum\t" << 1.e6*I << "(keV)\tProb\t" 
// 	 << dEdx->Prob << endl;
// #else /* __THELIX__ */
//     cout << Names[iop] << " " << i << " S/R " << dEdx->sector << "/" << dEdx->row
// 	 << " dEdx(keV/cm) " << 1.e6*dEdx->dEdx << " dx " << dEdx->dx 
//       //	 << " dx " << dEdx->dxH 
// 	 << " x[" << dEdx->xyz[0] << "," << dEdx->xyz[1] << "," << dEdx->xyz[2] << "]" 
// 	 << " d[" << dEdx->xyzD[0] << "," << dEdx->xyzD[1] << "," << dEdx->xyzD[2] << "]" 
// 	 << " R[" << dEdx->resXYZ[0] << "," << dEdx->resXYZ[1] << "," << dEdx->resXYZ[2] << "]" 
// 	 << " Sum " << 1.e6*I << "(keV)"
// 	 << " Prob " << dEdx->Prob << endl;
// #endif /* __THELIX__ */
    cout << Form("%s %2i  S/R %2i/%2i dEdx(keV/cm) %8.2f dx %5.2f x[%8.2f,%8.2f,%8.2f]", 
		 Names[iop],i,pdEdx->sector,pdEdx->row,1.e6*dEdx, pdEdx->dx, pdEdx->xyz[0], pdEdx->xyz[1], pdEdx->xyz[2]);
    cout << Form(" d[%8.2f,%8.2f,%8.2f] Sum %8.2f Prob %8.5f", pdEdx->xyzD[0], pdEdx->xyzD[1], pdEdx->xyzD[2],1.e6*I,pdEdx->Prob) << endl;
    if (iop == 2) {
      if (i < N60) I60 += dEdx;
      if (i < N70) I70 += dEdx;
      if (i == N60 - 1) {
	I60 /= N60;
	cout << " ======================= I60 \t" << I60 << endl;
      }
      if (i == N70 - 1) {
	I70 /= N70;
	cout << " ======================= I70 \t" << I70 << endl;
      }
    }
    avrz += TMath::Log(dEdx);
  }
  if (NdEdx) avrz /= NdEdx;
  cout << "mean dEdx \t" << I << "\tExp(avrz)\t" << TMath::Exp(avrz) << endl;
}
//________________________________________________________________________________
Double_t StdEdxY2Maker::LikeliHood(Double_t Xlog10bg, Int_t NdEdx, dEdxY2_t *dEdx) {
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
}
//________________________________________________________________________________
void StdEdxY2Maker::fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag) {
  Double_t Val[2];
  // P10
  // dEdxS->Draw("sigma_z:log(x)/log(2)","","prof")
  static Double_t sigma_p[3] = { 5.66734e-01,   -1.24725e-01,   1.96085e-02};
  f = 0.;
  gin[0] = 0.;
  gin[1] = 0.;
  for (int i=0;i<NdEdx; i++) {
    //    Double_t sigma = StTpcdEdxCorrection::SumSeries(TMath::Log(FdEdx[i].dx),3,sigma_p);
    Double_t X = TMath::Log(FdEdx[i].dx);
    Double_t sigma = sigma_p[2];
    for (int n = 1; n>=0; n--) sigma = X*sigma + sigma_p[n];
    FdEdx[i].zdev    = (FdEdx[i].dEdxL-par[0])/sigma;
    Landau(FdEdx[i].zdev,Val);
    FdEdx[i].Prob = TMath::Exp(Val[0]);
    f      -= Val[0];
    gin[0] += Val[1]/sigma;
  }
}
//________________________________________________________________________________
void StdEdxY2Maker::DoFitZ(Double_t &chisq, Double_t &fitZ, Double_t &fitdZ){
  Double_t avz = 0;
  for (int i=0;i<NdEdx;i++) avz += FdEdx[i].dEdxL;
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
  //  static TProfile *Center = 0, *Height = 0, *Width = 0,  *CenterPressure = 0;
  static TH1F *Multiplicity; // mult rate 
  static TH2S *Zdc  = 0; // ZdcEastRate versus ZdcWestRate
  static TH1F *ZdcC = 0; // ZdcCoincidenceRate
  static TH1F *BBC   = 0; // BbcCoincidenceRate
  static TH1F *L0   = 0; // L0RateToRich
  if (! iok && !BarPressure) {
    TDatime t1(tMin,0); /// min Time and
    TDatime t2(tMax,0); /// max 
    UInt_t i1 = t1.Convert() - timeOffSet;
    UInt_t i2 = t2.Convert() - timeOffSet;
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
//     CenterPressure        = new TProfile("CenterPressureP","log(center) vs log(Pressure)",150, 6.84, 6.99);
//     Center                = new TProfile("Center","Tpc Gain Monitor center versus Time",Nt,i1,i2);
//     Height 		  = new TProfile("Height","Tpc Gain Monitor height versus Time",Nt,i1,i2);
//     Width  		  = new TProfile("Width","Tpc Gain Monitor width versus Time",Nt,i1,i2);  
    // trigDetSums histograms
    Zdc                   = new TH2S("Zdc","ZdcEastRate versus ZdcWestRate (log10)",100,0,10,100,0,10);
    ZdcC                  = new TH1F("ZdcC","ZdcCoincidenceRate (log10)",100,0,10);
    Multiplicity          = new TH1F("Multiplicity","Multiplicity (log10)",100,0,10);
    BBC                   = new TH1F("BBC","BbcCoincidenceRate (log10)",100,0,10);
    L0                    = new TH1F("L0","L0RateToRich (log10)",100,0,2);
  }
  else {
    UInt_t date = GetDateTime().Convert() - timeOffSet;
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
void StdEdxY2Maker::SpaceCharge(Int_t iok, StEvent* pEvent, StGlobalCoordinate *global, dEdxY2_t *CdEdx) {
  // SpaceChargeStudy
  static TProfile2D *SpaceCharge = 0, *SpaceChargeU = 0, *SpaceChargeT = 0;
  static TH2S *Space2Charge = 0, *Space2ChargeU = 0, *Space2ChargeT = 0;
  static TH1F *TimeShift = 0;
  static TH3S *Space3Charge = 0, *Space3ChargePRZ = 0,*Space3ChargeShifted = 0;
  if (iok == 0 && !SpaceCharge) {
    if (Debug()) gMessMgr->Warning() << "StdEdxY2Maker::SpaceCharge Space Charge Histograms" << endm;
    SpaceCharge   = new TProfile2D("SpaceCharge","dE versus R and Z",85,40.,210.,92,-230.,230.);
    SpaceChargeU  = new TProfile2D("SpaceChargeU","dEU versus R and Z",85,40.,210.,92,-230.,230.);
    SpaceChargeT  = new TProfile2D("SpaceChargeT","dEU (all) versus R and Z",85,40.,210.,92,-230.,230.);
    Space2Charge  = new TH2S("Space2Charge","dE versus R and Z",85,40.,210.,92,-230.,230.);
    Space2ChargeU = new TH2S("Space2ChargeU","dEU versus R and Z",85,40.,210.,92,-230.,230.);
    Space2ChargeT = new TH2S("Space2ChargeT","dEU (all) versus R and Z",85,40.,210.,92,-230.,230.);
    TimeShift     = new TH1F("TimeShift","Shift in time wrt collsion",200,0,2000.);
    Space3Charge  = new TH3S("Space3Charge","Space charged versus Sector, Row and Z",
			     numberOfSectors,1., numberOfSectors+1, NumberOfRows,1., NumberOfRows+1,105,0.,210.);
    Space3Charge->Sumw2();
    Space3ChargePRZ  = new TH3S("Space3ChargePRZ","Space charged versus Phi(rads), Rho and Z",
				36,-TMath::Pi(), TMath::Pi(), 85, 40., 210.,105,-210.,210.);
    Space3ChargePRZ->Sumw2();
    Space3ChargeShifted = new TH3S("Space3ChargeShifted","Space charged shifted in time versus Sector, Row and Z",
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
  static TH3S *XYZ = 0, *XYZbad = 0;
  if (! global && !XYZ) {
    if (Debug()) gMessMgr->Warning() << "StdEdxY2Maker::XyzCheck XYZ check Histograms" << endm;
    XYZ    = new TH3S("XYZ","xyz for clusters",80,-200,200,80,-200,200,84,-210,210);
    XYZbad = new TH3S("XYZbad","xyz for clusters with mismatched sectors",
		      80,-200,200,80,-200,200,84,-210,210);
  }
  else 
    if (XYZ) XYZ->Fill( global->position().x(), global->position().y(), global->position().z());
  if (iokCheck && XYZbad) XYZbad->Fill( global->position().x(), global->position().y(), global->position().z());
}
//________________________________________________________________________________
void StdEdxY2Maker::QAPlots(StGlobalTrack* gTrack) {
  static TH2S *fTdEdxP70 = 0, *fTdEdxP70pi = 0, *fTdEdxP70e = 0, *fTdEdxP70K = 0, *fTdEdxP70P = 0;
  static TH2S *fTdEdxPF = 0, *fTdEdxPFpi = 0, *fTdEdxPFe = 0, *fTdEdxPFK = 0, *fTdEdxPFP = 0;
  static StTpcDedxPidAlgorithm PidAlgorithm;
  static StElectron* Electron = StElectron::instance();
  static StPionPlus* Pion = StPionPlus::instance();
  static StKaonPlus* Kaon = StKaonPlus::instance();
  static StProton* Proton = StProton::instance();
  static const Double_t Log10E = TMath::Log10(TMath::Exp(1.));
  static int first=0;
  if (! gTrack) {
    TFile *f = 0;
    if (TESTBIT(m_Mode, kCalibration)) {
      f = GetTFile();
      if (f) f->cd();
    }
    if (!first) {
      fZOfBadHits = new TH1F*[fNZOfBadHits];
      static Char_t *BadCaseses[fNZOfBadHits] = 
      {"it is not used in track fit",     // 0
       "it is flagged ",                  // 1
       "track length is inf ",            // 2
       "it does not pass check ",         // 3
       "dx is out interval [0.5,25]",     // 4
       "Sector/Row gain < 0",             // 5 iok + 4
       "drift distance < min || drift distance > max", // 6
       "dE < 0 or dx < 0",                // 7
       "Edge effect",                     // 8
       "Total no.of rejected clusters"    // 9
      };
      for (Int_t i = 0; i < fNZOfBadHits; i++) 
	fZOfBadHits[i] = new TH1F(Form("ZOfBadHits%i",i),
				  Form("Z of rejected clusters  because %s",BadCaseses[i]),
				  100,-210,210);                        
      fZOfGoodHits = new TH1F("ZOfGoodHits","Z of accepted clusters",100,-210,210);                        
      fPhiOfBadHits = new TH1F("PhiOfBadHits","Phi of rejected clusters",100, -TMath::Pi(), TMath::Pi());
      fTracklengthInTpcTotal = new TH1F("TracklengthInTpcTotal","Total track in TPC",100,0,200);         
      fTracklengthInTpc = new TH1F("TracklengthInTpc","Track length in TPC used for dE/dx",100,0,200);   
      fTdEdxPF    = new TH2S("TdEdxPF","log10(dE/dx(fit)(keV/cm)) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm", 
			     150,-1.,2., 500,0.,2.5);
      fTdEdxP70    = new TH2S("TdEdxP70","log10(dE/dx(I70)(keV/cm)) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm", 
			      150,-1.,2., 500,0.,2.5);
      fTdEdxP70pi  = new TH2S("TdEdxP70pi","log10(dE/dx(I70)(keV/cm)) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm |nSigmaPion| < 1", 
			      150,-1.,2., 500,0.,2.5);
      fTdEdxP70pi->SetMarkerColor(2);
      fTdEdxP70e   = new TH2S("TdEdxP70e","log10(dE/dx(I70)(keV/cm)) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm |nSigmaElectron| < 1", 
			      150,-1.,2., 500,0.,2.5);
      fTdEdxP70e->SetMarkerColor(3);
      fTdEdxP70K   = new TH2S("TdEdxP70K","log10(dE/dx(I70)(keV/cm)) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm |nSigmaKaon| < 1", 
			      150,-1.,2., 500,0.,2.5);
      fTdEdxP70K->SetMarkerColor(4);
      fTdEdxP70P   = new TH2S("TdEdxP70P","log10(dE/dx(I70)(keV/cm)) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm |nSigmaProton| < 1", 
			      150,-1.,2., 500,0.,2.5);
      fTdEdxP70P->SetMarkerColor(6);
      
      fTdEdxPFpi  = new TH2S("TdEdxPFpi","log10(dE/dx(Ifit)(keV/cm)) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm and Prob > 68.3", 
			     150,-1.,2., 500,0.,2.5);
      fTdEdxPFpi->SetMarkerColor(2);
      fTdEdxPFe   = new TH2S("TdEdxPFe","log10(dE/dx(Ifit)(keV/cm)) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm and Prob > 68.3", 
			     150,-1.,2., 500,0.,2.5);
      fTdEdxPFe->SetMarkerColor(3);
      fTdEdxPFK   = new TH2S("TdEdxPFK","log10(dE/dx(Ifit)(keV/cm)) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm and Prob > 68.3", 
			     150,-1.,2., 500,0.,2.5);
      fTdEdxPFK->SetMarkerColor(4);
      fTdEdxPFP   = new TH2S("TdEdxPFP","log10(dE/dx(Ifit)(keV/cm)) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm and Prob > 68.3", 
			     150,-1.,2., 500,0.,2.5);
      fTdEdxPFP->SetMarkerColor(6);
      mHitsUsage  = new TH2S("HitsUsage","log10(No.of Used in dE/dx hits) versus log10(Total no. of Tpc Hits",
			     80,0,8,60,0,6);
    }
    if (! f && !first) {
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
    static Double_t TrackLength70, TrackLength, I70, D70,  fitZ, fitdZ;
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
	if (pid->method() == kTruncatedMeanId) {
	  pid70 = pid; I70 = pid70->mean(); N70 = pid70->numberOfPoints();
	  TrackLength70 = pid70->length(); D70 = pid70->errorOnMean();
	}
	if (pid->method() == kLikelihoodFitId) {
	  pidF = pid;
	  fitZ = TMath::Log(pidF->mean()+3e-33); NF = pidF->numberOfPoints(); 
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
  first = 2004;
}
//________________________________________________________________________________
void StdEdxY2Maker::BadHit(Int_t iFlag, const StThreeVectorF &xyz) {
  if (iFlag >= 0 && iFlag < fNZOfBadHits && fZOfBadHits[iFlag]) fZOfBadHits[iFlag]->Fill(xyz.z());
  if (fZOfBadHits[fNZOfBadHits-1]) fZOfBadHits[fNZOfBadHits-1]->Fill(xyz.z());
  if (fPhiOfBadHits!= 0) fPhiOfBadHits->Fill(TMath::ATan2(xyz.y(),xyz.x()));
}
//________________________________________________________________________________
void StdEdxY2Maker::Correlations() {
  // CORRELATION
  static TH2S *corrI = 0, *corrO = 0, *corrI2 = 0, *corrO2 = 0, *corrI5 = 0, *corrO5 = 0;
  static TH2S *corrIw = 0, *corrOw = 0, *corrI2w = 0, *corrO2w = 0, *corrI5w = 0, *corrO5w = 0;
  static TH1F *corrI1w = 0, *corrO1w = 0;
  if (! corrI) {
    gMessMgr->Warning() << "StdEdxY2Maker::Histogramming make Correlation histograms" << endm;
    corrI   = new TH2S("corrI","Correlation for Inner Sector for pair of nearest rows",
		       100,-10.,10., 100,-10.,10.);
    corrO   = new TH2S("corrO","Correlation for Outer Sector for pair of nearest rows",
		       100,-10.,10., 100,-10.,10.);
    corrI2   = new TH2S("corrI2","Correlation for Inner Sector for pair rows & row + 2",
			100,-10.,10., 100,-10.,10.);
    corrO2   = new TH2S("corrO2","Correlation for Outer Sector for pair rows & row + 2",
			100,-10.,10., 100,-10.,10.);
    corrI5   = new TH2S("corrI5","Correlation for Inner Sector for pair rows & row + 5",
			100,-10.,10., 100,-10.,10.);
    corrO5   = new TH2S("corrO5","Correlation for Outer Sector for pair rows & row + 5",
			100,-10.,10., 100,-10.,10.);
    corrIw   = new TH2S("corrIw","Weighted correlation for Inner Sector for pair of nearest rows",
			100,-10.,10., 100,-10.,10.);
    corrOw   = new TH2S("corrOw","Weighted correlation for Outer Sector for pair of nearest rows",
			100,-10.,10., 100,-10.,10.);
    corrI1w   = new TH1F("corrI1w","Weighted distribution for Inner Sector",100,-10.,10.);
    corrO1w   = new TH1F("corrO1w","Weighted distribution for Outer Sector",100,-10.,10.);
    corrI2w   = new TH2S("corrI2w","Weighted correlation for Inner Sector for pair rows & row + 2",
			 100,-10.,10., 100,-10.,10.);
    corrO2w   = new TH2S("corrO2w","Weighted correlation for Outer Sector for pair rows & row + 2",
			 100,-10.,10., 100,-10.,10.);
    corrI5w   = new TH2S("corrI5w","Weighted correlation for Inner Sector for pair rows & row + 5",
			 100,-10.,10., 100,-10.,10.);
    corrO5w   = new TH2S("corrO5w","Weighted correlation for Outer Sector for pair rows & row + 5",
			 100,-10.,10., 100,-10.,10.);
  } // CORRELATION
  for (Int_t k = 0; k < NdEdx; k++) {
    Double_t zk  = FdEdx[k].zdev;
    if (FdEdx[k].Prob > 1.e-12) {
      if (FdEdx[k].row > 13) corrO1w->Fill(zk,1./FdEdx[k].Prob);
      else                   corrI1w->Fill(zk,1./FdEdx[k].Prob);
    }
    for (Int_t m = 0; m < NdEdx; m++){
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
}
