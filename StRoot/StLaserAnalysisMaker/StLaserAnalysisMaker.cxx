// 
// $Id: StLaserAnalysisMaker.cxx,v 1.23 2015/02/10 20:27:16 fisyak Exp $
// $Log: StLaserAnalysisMaker.cxx,v $
// Revision 1.23  2015/02/10 20:27:16  fisyak
// Adjust split style for ROOT_VERSION_CODE
//
// Revision 1.22  2015/02/06 00:31:46  perev
// defence agains zero dcaGeometry pointer added
//
// Revision 1.21  2014/09/17 19:11:49  fisyak
// Fix bug in Fit logic, bug #2901
//
// Revision 1.20  2014/08/19 17:23:31  fisyak
// Activate CORRECT_RAFT_DIRECTION
//
// Revision 1.19  2014/03/14 13:11:20  fisyak
// comment out gStTpcDb->ScaleY()
//
// Revision 1.18  2014/03/13 21:59:45  fisyak
// add cluster position in Local Sector Coordinate System
//
// Revision 1.16  2009/03/11 15:49:40  fisyak
// Remove hits not beloging to primary tracks
//
// Revision 1.15  2008/06/02 13:48:03  fisyak
// Add  t0 handlers for Tpx/Tpc time offsets
//
// Revision 1.14  2007/10/16 15:26:03  fisyak
// Freeze version for Run VII
//
// Revision 1.13  2007/07/05 14:37:04  fisyak
// Freeze version for run VII
//
// Revision 1.12  2007/05/09 13:36:44  fisyak
// Freeze before intruducing Bundle coordinate system
//
// Revision 1.11  2007/04/17 05:12:06  perev
// GetTFile()==>StMaker. Jerome request
//
// Revision 1.10  2007/03/06 16:31:55  fisyak
// Before Selection of good runs
//
//#define CORRECT_LASER_POSITIONS
#define CORRECT_RAFT_DIRECTION
#define __TRACKHITS__
#ifndef __TRACKHITS__
#define ADDPRIMTRACKHITS
#endif
#include <assert.h>
#include "TFile.h"
#include "StEventTypes.h"
#include "StLaserAnalysisMaker.h"
#include "laserino.h"
#include "LaserEvent.h"
#include "StTpcDb/StTpcDb.h"
#include "TGeoMatrix.h"
#include "TRVector.h"
#include "TRMatrix.h"
#include "TRSymMatrix.h"
#include "StDetectorDbMaker/StDetectorDbClock.h"
#define PrPP(A,B) if (Debug()) {LOG_INFO << "::StLaserAnalysisMaker" << (#A) << "\t" << (#B) << " = \t" << (B) << endm;}
ClassImp(StLaserAnalysisMaker);
static  LaserEvent             *event = 0;  //! Laser Event object
static  const Int_t NS = 12;
static  const Int_t NB =  6;
static  const Int_t NM =  7;
static LaserB *Lasers[NS][NB][NM];
#if 1
static Int_t NoBeams = 0;
static LaserRaft *LaserBeams[NS*NB*NM];
#endif
static TGeoHMatrix *Traft[14];
static TGeoHMatrix *Raft2Tpc[14];
static TGeoHMatrix *Bundles2Tpc[14][6];
static TGeoHMatrix *Mirrors2Tpc[14][6][7];
//________________________________________________________________________________
Int_t StLaserAnalysisMaker::Init(){
  if (! IsActive()) return kStOk;
  event = new LaserEvent();
  TFile *f = GetTFile();
#if 0
  if (! f) {
    f = new TFile("StLaserAnalysisMaker.root","recreate");
  }
#endif
  assert(f);
  if (f) {
    f->cd();
    m_laser = new TTree("laser","Tpc laser track tree");
    m_laser->SetAutoSave(100000000); //Save every 100 MB
    Int_t bufsize= 64000;
#if ROOT_VERSION_CODE <= ROOT_VERSION(5,34,10)
    Int_t split = 99;
#else
    Int_t split  = -2;       // by default, split Event in sub branches << old style
#endif
    if (split)  bufsize /= 4;
    Int_t branchStyle = 1; //new style by default
    if (split < 0) {branchStyle = 0; split = -1-split;}
    TTree::SetBranchStyle(branchStyle);
    m_laser->Branch("event", "LaserEvent",&event, bufsize, split);
  }
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StLaserAnalysisMaker::InitRun(Int_t run){
  const Int_t numSectors = 24;
  Double_t gamma = 0;
  Double_t dGamma = 720./numSectors;
  Int_t sector = 0;
  TGeoHMatrix TpcHalf[2];
  Double_t rotHalfs[18] = {
    0,  1, 0, -1, 0, 0, 0, 0,  1, // sector 13-24
    0, -1, 0, -1, 0, 0, 0, 0, -1  // sector  1-12
  };
  for (Int_t half = east; half <= west; half++) TpcHalf[half].SetRotation(&rotHalfs[9*half]);
  TGeoHMatrix RotSec[24];
  for (sector = 1; sector <= numSectors; sector++) {
    if (sector > 12) gamma = (numSectors-sector)*dGamma;
    else             gamma = sector*dGamma;
    RotSec[sector-1].RotateZ(-gamma);
  }
#ifdef CORRECT_RAFT_DIRECTION
  Double_t zWheel     = (229.71+1.7780);
  //                      East      West
  Double_t ZWheel[2]  = {-zWheel, zWheel};
  Double_t RDWheel[2] = { 190.5802, 190.5705};
  StThreeVectorD xyzDSE(RDWheel[0], 0, ZWheel[0]);
  StThreeVectorD xyzDSW(RDWheel[1], 0, ZWheel[1]);
  TGeoHMatrix R;
  StThreeVectorD xyzTE, xyzTW;
  if (Debug()) {
    cout << " TpcHalf(east) "; StTpcDb::instance()->TpcHalf(east).Print();
    cout << " TpcHalf(west) "; StTpcDb::instance()->TpcHalf(west).Print();
  }
  for (sector = 1; sector <= 12; sector++) {
    if (Debug()) {
      cout << "Sector " <<  sector << " ===========" << endl;
    }
    // S2R = Rot^-1 * Half * Rot
    TGeoHMatrix ET = RotSec[sector+12-1].Inverse()*StTpcDb::instance()->TpcHalf(east)*RotSec[sector+12-1]; if (Debug()) ET.Print();
    ET.LocalToMaster(xyzDSE.xyz(), xyzTE.xyz());                                                           PrPP(InitRun,xyzTE);
    TGeoHMatrix EW = RotSec[sector   -1].Inverse()*StTpcDb::instance()->TpcHalf(west)*RotSec[sector   -1]; if (Debug()) EW.Print();
    EW.LocalToMaster(xyzDSW.xyz(), xyzTW.xyz());                                                           PrPP(InitRun,xyzTW);
    // Survey line direction
    StThreeVectorD dif = (xyzTW - xyzTE)/2.; PrPP(InitRun,dif);
    // Survey line center
    StThreeVectorD sum = (xyzTW + xyzTE)/2.; PrPP(InitRun,sum);
    StThreeVectorD unit = dif.unit();        PrPP(InitRun,unit);
    Double_t alpha = - unit.y();
    Double_t beta  =   unit.x();
    StThreeVectorD tra = sum; tra.xyz()[0] -= 0.5*(RDWheel[0]+RDWheel[1]); PrPP(InitRun,tra);
    // Transformation from survey line to raft
    TGeoHMatrix S2R(Form("S2R_%i",sector));
    S2R.RotateX(alpha*180/TMath::Pi()); 
    S2R.RotateY(beta*180/TMath::Pi());
    S2R.SetTranslation(tra.xyz());                 if (Debug()) {cout << "S2R "; S2R.Print();}
    StThreeVectorD unitR;
    S2R.MasterToLocalVect(unit.xyz(),unitR.xyz()); PrPP(InitRun,unitR);
    if (Debug()) {cout << "RotSecO[" << sector-1 << "]:"; RotSec[sector-1].Print();}
    R = RotSec[sector-1]*S2R;                      if (Debug()) {cout << "R:"; R.Print();}
    RotSec[sector-1] = R;                          if (Debug()) {cout << "RotSecM[" << sector-1 << "]:"; RotSec[sector-1].Print();}
    if (Debug()) {cout << "RotSecO[" << sector+12-1 << "]:"; RotSec[sector+12-1].Print();}
    //    TGeoHMatrix R = RotSec[sector+12-1]*S2R.Inverse();     if (Debug()) {cout << "R:"; R.Print();}
    TGeoHMatrix R = RotSec[sector+12-1]*S2R;     if (Debug()) {cout << "R:"; R.Print();}
    RotSec[sector+12-1] = R;                       if (Debug()) {cout << "RotSecM[" << sector+12-1 << "]:"; RotSec[sector+12-1].Print();}
  }    
#endif /* CORRECT_RAFT_DIRECTION */
  memset (LaserBeams, 0, NS*NB*NM*sizeof(LaserRaft*));
  NoBeams = 0;
  memset(Traft, 0, 14*sizeof(TGeoHMatrix *));
  memset(Raft2Tpc, 0, 14*sizeof(TGeoHMatrix *));
  memset(Bundles2Tpc, 0,  14*6*sizeof(TGeoHMatrix *));
  memset(Mirrors2Tpc, 0,  14*6*7*sizeof(TGeoHMatrix *));
  Double_t y0[12] = { 0.0373, -0.0104, -0.0081, -0.0092,  0.0000,  0.0492,  0.0008, -0.0123,  0.0281,  0.0210, -0.0102, -0.0627};
  Double_t y1[12] = { 0.0088, -0.0033,  0.0000, -0.0045,  0.0000,  0.0079,  0.0006, -0.0013,  0.0068,  0.0052, -0.0033, -0.0168};
  Double_t z0[12] = {-0.0414,  0.0363, -0.1394,  0.0508,  0.0000,  0.0241, -0.0331,  0.0689, -0.1474, -0.0469,  0.1104,  0.0203};
  Double_t z1[12] = {-0.0002, -0.0001, -0.0089, -0.0002,  0.0000, -0.0000, -0.0002, -0.0000,  0.0003,  0.0031,  0.0007,  0.0002};
  for (Int_t i = 0; i < NoRaftPositions; i++) {
    if (! RaftPositions[i].Sector) continue;
    Int_t raft = RaftPositions[i].Raft;
    Traft[raft-1] = new TGeoHMatrix(Form("Raft%0i",raft));
    Traft[raft-1]->RotateX(RaftPositions[i].alpha*180/TMath::Pi());
    Traft[raft-1]->RotateY(RaftPositions[i].beta*180/TMath::Pi());
    Traft[raft-1]->RotateZ(RaftPositions[i].gamma*180/TMath::Pi());
    StThreeVectorD xyzRaft(RaftPositions[i].X,RaftPositions[i].Y,RaftPositions[i].Z - 0.05465 + 0.1022-0.0530);
    if (RaftPositions[i].Sector <= 12) xyzRaft.setY(xyzRaft.y() - 0.0480 - 0.0095 - 0.0028);
    else                               xyzRaft.setY(xyzRaft.y() + 0.0328 + 0.0118 + 0.0032);
    Int_t s = (RaftPositions[i].Sector-1)/2;
    xyzRaft.setY(xyzRaft.y() + y0[s] + y1[s]);
    xyzRaft.setZ(xyzRaft.z() + z0[s] + z1[s]);
    Traft[raft-1]->SetTranslation(xyzRaft.xyz());
    if (Debug()) {
      RaftPositions[i].Print();
      Traft[raft-1]->Print();
    }
  }
  for (Int_t r = 0; r < 14; r++) {
    Int_t sector = Bundles[r][0].Sector;
    if (! sector) continue;
    Int_t raft = Bundles[r][0].Raft;
    Int_t half = west;
    if (sector > 12) half = east;
    Raft2Tpc[raft-1] = new TGeoHMatrix(Form("Raft%iToTpc",raft));
    *Raft2Tpc[raft-1] = RotSec[sector-1] * TpcHalf[half] * (*Traft[raft-1]);
    for (Int_t b = 0; b < 6; b++) {
      Int_t bundle = Bundles[r][b].Bundle;
      TGeoHMatrix rotB;
      rotB.SetTranslation(&Bundles[r][b].X);
      Bundles2Tpc[raft-1][bundle-1] = new TGeoHMatrix(Form("BundleR%iB%i",raft,bundle));
      *Bundles2Tpc[raft-1][bundle-1] = *Raft2Tpc[raft-1] * rotB;
      for (Int_t m = 0; m < 7; m++) {
	if (Mirrors[r][b][m].Sector < 2) continue;
	LASERINO_t *local = &Mirrors[r][b][m];
	if (! local) continue;
	Int_t mirror = Mirrors[r][b][m].Mirror;
	LaserBeams[NoBeams] = new LaserRaft();
	LaserBeams[NoBeams]->Sector = local->Sector;
	LaserBeams[NoBeams]->Raft   = local->Raft;
	LaserBeams[NoBeams]->Bundle = local->Bundle;
	LaserBeams[NoBeams]->Mirror = local->Mirror;
#ifdef CORRECT_LASER_POSITIONS
       LaserBeams[NoBeams]->XyzB = StThreeVectorD(Mirrors[r][b][m].X+Mirrors[r][b][m].dX,
						  Mirrors[r][b][m].Y+Mirrors[r][b][m].dY,
						  Mirrors[r][b][m].Z+Mirrors[r][b][m].dZ);
#else
       LaserBeams[NoBeams]->XyzB = StThreeVectorD(Mirrors[r][b][m].X,
						  Mirrors[r][b][m].Y,
						  Mirrors[r][b][m].Z);
#endif
	Double_t theta = Bundles[r][b].ThetaZ + Mirrors[r][b][m].ThetaZ;
	Double_t phi   = Bundles[r][b].Phi    + Mirrors[r][b][m].Phi;
	TGeoHMatrix rotM;
	rotM.SetTranslation(LaserBeams[NoBeams]->XyzB.xyz());
	Mirrors2Tpc[raft-1][bundle-1][mirror-1] = new TGeoHMatrix(Form("MirrorR%iB%iM%i",raft,bundle,mirror));
	*Mirrors2Tpc[raft-1][bundle-1][mirror-1] = *Bundles2Tpc[raft-1][bundle-1] * rotM;
	
	LaserBeams[NoBeams]->dirB = StThreeVectorD(-TMath::Cos(phi)*TMath::Cos(theta), 
						   -TMath::Sin(phi)*TMath::Cos(theta), 
						   -TMath::Sin(theta));
	rotB.LocalToMaster(LaserBeams[NoBeams]->XyzB.xyz(), LaserBeams[NoBeams]->XyzU.xyz());
	rotB.LocalToMasterVect(LaserBeams[NoBeams]->dirB.xyz(), LaserBeams[NoBeams]->dirU.xyz());
	Raft2Tpc[raft-1]->LocalToMaster(LaserBeams[NoBeams]->XyzU.xyz(),LaserBeams[NoBeams]->XyzL.xyz());
	Raft2Tpc[raft-1]->LocalToMasterVect(LaserBeams[NoBeams]->dirU.xyz(),LaserBeams[NoBeams]->dirL.xyz());
	LaserBeams[NoBeams]->Theta = LaserBeams[NoBeams]->dirL.theta();
	LaserBeams[NoBeams]->Phi   = LaserBeams[NoBeams]->dirL.phi();
	if (Debug()) {
	  local->Print();
	  Raft2Tpc[raft-1]->Print();
	  LaserBeams[NoBeams]->Print();
	}
	NoBeams++;
      }
    }
  }
  // average Z for membrane = -3.6 cm
  memset(Lasers, 0, NS*NB*NM*sizeof(LaserB *));
  const TGeoHMatrix &Tpc2Global = gStTpcDb->Tpc2GlobalMatrix();
  LaserB *theLaser = 0;
  for (Int_t i = 0; i < NoBeams; i++) {
    if (! LaserBeams[i]) continue;
    Int_t s = LaserBeams[i]->Sector/2 - 1;
    if (s < 0 || s >= NS) continue;
    Int_t b = LaserBeams[i]->Bundle - 1;
    if (b < 0 || b >= NB) continue;
    Int_t m = LaserBeams[i]->Mirror - 1;
    if (m < 0 || m >= NM) continue;
    theLaser = new LaserB(*LaserBeams[i]);
    Lasers[s][b][m] = theLaser;
    Tpc2Global.LocalToMaster(theLaser->XyzL.xyz(),theLaser->XyzG.xyz());
    Tpc2Global.LocalToMasterVect(theLaser->dirL.xyz(),theLaser->dirG.xyz());
    gStTpcDb->SupS2Tpc(theLaser->Sector).MasterToLocal(theLaser->XyzL.xyz(),theLaser->XyzS.xyz());
    gStTpcDb->SupS2Tpc(theLaser->Sector).MasterToLocalVect(theLaser->dirL.xyz(),theLaser->dirS.xyz());
    theLaser->PhiG = theLaser->dirG.phi();
    theLaser->ThetaG = theLaser->dirG.theta();
    theLaser->IsValid = 0;
#if 0
    // From Laser Z
    if (theLaser->Sector ==  2 && theLaser->Bundle == 3 &&  theLaser->Mirror == 5) continue; // misplaced
    if (theLaser->Sector ==  4 && theLaser->Bundle == 4) continue; //dead
    if (theLaser->Sector ==  8) continue; // dead
    if (theLaser->Sector == 10) continue; // dead
    if (theLaser->Sector == 12 && theLaser->Bundle == 4 &&  theLaser->Mirror == 4) continue; // dead
    if (theLaser->Sector == 12 && theLaser->Bundle == 4 &&  theLaser->Mirror == 5) continue; // dead
    if (theLaser->Sector == 14 && theLaser->Bundle == 4 &&  theLaser->Mirror == 4) continue; // misplaced swap ?
    if (theLaser->Sector == 16 && theLaser->Bundle == 4 &&  theLaser->Mirror == 5) continue; // misplaced
    if (theLaser->Sector == 16 && theLaser->Bundle == 4 &&  theLaser->Mirror == 5) continue; // misplaced
    if (theLaser->Sector == 18 && theLaser->Bundle == 2) continue; // very strange pattern
    if (theLaser->Sector == 22 && theLaser->Bundle == 3) continue; // strange pattern
    if (theLaser->Sector == 22 && theLaser->Bundle == 4) continue; // missing
    if (theLaser->Sector == 24 && theLaser->Bundle == 2 && theLaser->Mirror == 6) continue; // missing
#endif
    theLaser->IsValid = 1;
    if (Debug()) {
      LaserBeams[i]->Print();
      theLaser->Print();
    }
  }
  return kStOk;
}
//_____________________________________________________________________________
void StLaserAnalysisMaker::Clear(const Option_t *option){
  if (event) event->Clear("C");
  StMaker::Clear(option);
}
//_____________________________________________________________________________
Int_t StLaserAnalysisMaker::Make(){
  StEvent* pEvent = dynamic_cast<StEvent*> (GetInputDS("StEvent"));
    // Fill the event header.
  
  StEvtHddr *EvtHddr = (StEvtHddr*)GetDataSet("EvtHddr");
  if (! EvtHddr) return kStWarn;
  event->SetHeader(EvtHddr->GetEventNumber(), EvtHddr->GetRunNumber(), EvtHddr->GetDate(), EvtHddr->GetTime(),
		   gStTpcDb->Electronics()->tZero(), gStTpcDb->DriftVelocity(), gStTpcDb->Electronics()->samplingFrequency(), 
		   EvtHddr->GetInputTriggerMask());
  event->SetDVWest(gStTpcDb->DriftVelocity(1));
  event->SetDVEast(gStTpcDb->DriftVelocity(13));
#if 0
  event->SetScaleY(gStTpcDb->ScaleY());
#endif
  event->GetHeader()->SetDriftDistance(gStTpcDb->Dimensions()->gatingGridZ());
  event->GetHeader()->SetInnerSectorzOffset(gStTpcDb->Dimensions()->zInnerOffset());
  event->GetHeader()->SetOuterSectorzOffset(gStTpcDb->Dimensions()->zOuterOffset());
  event->GetHeader()->SettriggerTimeOffset(gStTpcDb->triggerTimeOffset());
  event->GetHeader()->SetBField(pEvent->runInfo()->magneticField());
  StDetectorDbClock* dbclock = StDetectorDbClock::instance();
  double freq = dbclock->getCurrentFrequency()/1000000.0;
  event->GetHeader()->SetOnlClock(freq);
  if (! pEvent) return kStWarn;
  UInt_t nvtx = pEvent->numberOfPrimaryVertices();
  for (UInt_t i = 0; i < nvtx; i++) {
    event->AddVertex(pEvent->primaryVertex(i));
  }
  StSPtrVecTrackNode& trackNode = pEvent->trackNodes();
  UInt_t nTracks = trackNode.size();
  StTrackNode *node=0;
  for (unsigned int i=0; i < nTracks; i++) {
    node = trackNode[i]; 
    if (!node) continue;
    StGlobalTrack  *gTrack = static_cast<StGlobalTrack *>(node->track(global));
    if (! gTrack) continue;
    if (!gTrack->dcaGeometry()) continue;
    Int_t key = gTrack->key();
    //    if (gTrack->numberOfPossiblePoints(kTpcId) < 25) continue;
    StPrimaryTrack *pTrack = 	static_cast<StPrimaryTrack*>(node->track(primary));
    StThreeVectorD g3 = gTrack->outerGeometry()->momentum();
    if (g3.mag() < 10) continue;
    StThreeVectorD unit = g3.unit();
    StPhysicalHelixD helixO = gTrack->outerGeometry()->helix();
    // find a match using last hit
    StPtrVecHit hvec = gTrack->detectorInfo()->hits();
    Double_t rMax = 0;
    Int_t jmax = -1;
    //    StThreeVectorD *pred = 0;
    StTpcHit *tpcHit;
    Int_t sector, s = -1;
    Int_t bundle = -1;
    Double_t dZ, dZmin  = 9999;
    Int_t b, m;
    Double_t dPhi, dPhimin  = 999;
    Int_t mmax = -1;
    StThreeVectorD Pred, Diff;
    Track *t = 0;
    for (unsigned int j=0; j<hvec.size(); j++) {// hit loop
      if (hvec[j]->detector() != kTpcId) continue;
      tpcHit = static_cast<StTpcHit *> (hvec[j]);
#ifndef __TRACKHITS__
#ifdef ADDPRIMTRACKHITS
      if (pTrack) 
	event->AddHit(tpcHit, key);
#endif
#else
      event->AddHit(tpcHit,key);
#endif
      if (tpcHit->position().perp() > rMax) {
	rMax = tpcHit->position().perp();
	jmax = j;
      }
    }
    LaserB *theLaser = 0;
    if (jmax < 0 || rMax < 120) goto ADDTRACK;
    tpcHit = static_cast<StTpcHit *> (hvec[jmax]);
    // sector
    sector = tpcHit->sector();
    if (pTrack) goto ADDTRACK;
    if (2*(sector/2) != sector) goto ADDTRACK;
    s = sector/2 - 1;
    if (s < 0 || s >= NS) goto ADDTRACK;
    // bundle
    dZmin  = 9999;
    for (b = 0; b < NB; b++) {
      if (! Lasers[s][b][0]) continue;
      dZ = TMath::Abs(tpcHit->position().z() - Lasers[s][b][0]->XyzG.z());
      if (dZ < dZmin) {bundle = b; dZmin = dZ;}
    }
    if (bundle < 0 || dZmin > 15) goto ADDTRACK;
    // mirror
    // minimum distance in XY plane from laser spot
    dPhimin  = 999;
    for (m = 0; m < NM; m++) {
      if (! Lasers[s][bundle][m]) continue;
      dPhi = TMath::Abs(Lasers[s][bundle][m]->PhiG - g3.phi());
      if (dPhi < dPhimin) {
	dPhimin = dPhi;
	mmax = m;
      }
    }
    if (mmax < 0 || dPhimin > 0.1) goto ADDTRACK;
    theLaser = Lasers[s][bundle][mmax];
  ADDTRACK:
    t = event->AddTrack(sector,gTrack,theLaser,tpcHit->position().z());
    if (theLaser) {
      Int_t raft = theLaser->Raft;
      Int_t bundle = theLaser->Bundle;
      Int_t mirror = theLaser->Mirror;
      if (raft > 0 && raft <= 14 &&
	  bundle > 0 && bundle <= 6 &&
	  mirror > 0 && mirror <= 7) {
	t->SetPredictions(Raft2Tpc[raft-1], Bundles2Tpc[raft-1][bundle-1], Mirrors2Tpc[raft-1][bundle-1][mirror-1]);
	if (theLaser->IsValid)  event->AddTrackFit(t);
      }
    }
    if (Debug()) {
      t->Print();
    }
  }  
  static const Double_t sigma = 0.0385; // run 8090018 Full Field.
  for (Int_t k = 0; k < 11; k++) {
    FitDV *fit = (FitDV *) (*event->Fit())[k];
    Bool_t ok = kTRUE;
    Int_t N = fit->N;
    if (N > 3) {
      for (;;) {
	TRVector Y;
	TRMatrix A(0,2);
	for (Int_t i = 0; i < N; i++) {
	  if (! fit->Flag[i]) {
	    Double_t dev = fit->Y[i]/sigma;
	    Y.AddRow(&dev);
	    Double_t a[2] = {1./sigma, fit->X[i]/sigma};
	    A.AddRow(a);
	  }
	}
	Int_t ndf = A.GetNrows() - 2;
	if (ndf < 2) break;
	TRSymMatrix S(A,TRArray::kATxA);        if (Debug()) cout << "S: " << S << endl;
	TRVector    B(A,TRArray::kATxB,Y);      if (Debug()) cout << "B: " << B << endl;
	TRSymMatrix SInv(S,TRArray::kInvertedA);if (Debug()) cout << "SInv: " << SInv << endl;
	if (! SInv.IsValid()) {break;}
	TRVector    X(SInv,TRArray::kSxA,B);    if (Debug()) cout << "X: " << X << endl;
	TRVector    R(Y);               
	R -= TRVector(A,TRArray::kAxB,X);       if (Debug()) cout << "R: " << R << endl;
	Double_t chisq = R*R;
	Double_t prob = TMath::Prob(chisq,ndf);
	if (prob > 0.01) {
	  fit->offset  = X[0];
	  fit->slope   = X[1];
	  fit->chisq   = chisq;
	  fit->dslope  = SInv[2];
 	  fit->doffset = SInv[0];
	  fit->Prob = prob;
	  fit->ndf = ndf;
	  if (Debug()) fit->Print();
#if 0
	  TClonesArray &tracks = *event->Tracks();
	  Int_t Ntrack = event->GetNtrack();
	  for (Int_t i = 0; i < Ntrack; i++) {
	    Track *t = (Track *) tracks[i];
	    if (t->Flag == 2 && t->Laser.Sector == 2*(k+1)) {
	      t->fit = *fit;
	    }
	  }
#endif
	  break;
	} else {
	  Int_t j = -1;
	  Int_t imax = -1;
	  Double_t Rmax = 0;
	  for (Int_t i = 0; i < N; i++) {
	    if (! fit->Flag[i]) {
	      j++;
	      Double_t RR = TMath::Abs(R[j]);
	      if (RR > Rmax) {
		imax = i;
		Rmax = RR;
	      }
	    }
	  }
	  if (imax < 0) break;
	  fit->Flag[imax] = 1;
	}
      }
    }
    if (! ok) break;
  }
  m_laser->Fill(); //Fill the Tree
  return kStOK;
}
//________________________________________________________________________________
Int_t StLaserAnalysisMaker::Finish() {
  for (Int_t s = 0; s < NS; s++) 
    for (Int_t b = 0; b < NB; b++) 
      for (Int_t m = 0; m < NM; m++) SafeDelete(Lasers[s][b][m]);
#if 0
  if (! GetTFile()) {
    TSeqCollection *files = gROOT->GetListOfFiles();
    if (files) {
      TFile *f = 0;  
      TIter  next(files);
      while ((f = (TFile *) next())) {
	TString name(gSystem->BaseName(f->GetName()));
	if (name == "StLaserAnalysis.root") {
	  delete f;
	  break;
	}
      }
    }
  }
#endif
  return StMaker::Finish();
}
