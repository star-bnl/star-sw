// $Id: StarMCTTreePrimaryGenerator.cxx,v 1.1.1.1 2008/12/10 20:45:52 fisyak Exp $
#include <assert.h>
#include "StarMCTTreePrimaryGenerator.h"
#include "TFile.h"
#include "TGeant3.h"
#include "TEnv.h"
#include "TRandom.h"
#include "TSystem.h"
#include "TDatabasePDG.h"
#include "TParticlePDG.h"
#include "TMCParticle.h"
#include "StDetectorDbMaker/St_vertexSeedC.h"
#include "StDetectorDbMaker/St_beamInfoC.h"
#include "StMessMgr.h" 
#include "StMaker.h"
#include "StChainOpt.h"
ClassImp(StarMCTTreePrimaryGenerator);
//_____________________________________________________________________________
StarMCTTreePrimaryGenerator::StarMCTTreePrimaryGenerator(TString mode, Int_t tune) : StarMCPrimaryGenerator() {
  PreSet(); 
  SetGenerator(mode,tune);
}
//_____________________________________________________________________________
void StarMCTTreePrimaryGenerator::PreSet() {
  fNofPrimaries = 0; fId = 0;
  fOption = "";
  fOrigin = TVector3(0,0,0);
  fPVX = fPVY = fPVZ = fPVxyError = 0;
  fTree = 0;
  SetSpread(0.15, 0.15, 42.0);
  const StChainOpt *opt = StMaker::GetTopChain()->GetChainOpt();
  assert(opt);
  const TString inputfile = opt->GetFileIn();
  TFile *f = new TFile(inputfile);
  assert(f);
  fTree = (TTree *) f->Get("EICTree");
  assert(fTree);
  fTreeIter = new TTreeIter("EICTree");
  fTreeIter->AddFile(inputfile);
}
//_____________________________________________________________________________
void StarMCTTreePrimaryGenerator::SetGenerator(TString mode, Int_t tune) {
  TString path(".");
  TString File("PVxyz.root");
  Char_t *file = gSystem->Which(path,File,kReadPermission);
  if (file) {
    TFile *PVfile = TFile::Open(file);
    if (PVfile) {
      fPVX = (TH1 *) PVfile->Get("x"); assert(fPVX); fPVX->SetDirectory(0);
      fPVY = (TH1 *) PVfile->Get("y"); assert(fPVY); fPVY->SetDirectory(0);
      fPVZ = (TH1 *) PVfile->Get("z"); assert(fPVZ); fPVZ->SetDirectory(0);
      fPVxyError = (TH1 *) PVfile->Get("hPVError"); if (fPVxyError) fPVxyError->SetDirectory(0);
      delete PVfile;
      LOG_WARN << "PVxyz.root with x, y and z histograms has been found. These histogram will be use to generate primary vertex x, y, z." << endm;
      if (fPVxyError) LOG_WARN << " hPVError histogram will be used for transverse PV error." << endm;
    }
    delete [] file;
  }
  fgInstance = this;
}
//_____________________________________________________________________________
void StarMCTTreePrimaryGenerator::GeneratePrimary() {     
  // Add one primary particle to the user stack (derived from TVirtualMCStack).
  // Track ID (filled by stack)
  // Option: to be tracked
  TTreeIter &iter = *fTreeIter;
  static const Int_t&       nTracks                                  = iter("nTracks");
  static const Int_t*&      particles_id                             = iter("particles.id");
  static const Double_t*&   particles_px                             = iter("particles.px");
  static const Double_t*&   particles_py                             = iter("particles.py");
  static const Double_t*&   particles_pz                             = iter("particles.pz");
  static const Double32_t*& particles_E                              = iter("particles.E");
  static const Double_t*&   particles_xv                             = iter("particles.xv");
  static const Double_t*&   particles_yv                             = iter("particles.yv");
  static const Double_t*&   particles_zv                             = iter("particles.zv");
  if (! iter.Next()) {fStatus =  kStEOF; return;}
  Int_t toBeDone = 1; 
  Double_t polx = 0.; 
  Double_t poly = 0.; 
  Double_t polz = 0.; 
  Int_t ntr = 0;
  Int_t N = nTracks;
  for (Int_t i = 0; i < N; i++) {
    if (TMath::Abs(particles_id[i]) < 10) continue;
    if (! TDatabasePDG::Instance()->GetParticle(particles_id[i])) continue;
    // Add particle to stack 
    fStarStack->PushTrack(toBeDone, -1, 
			  particles_id[i], 
			  particles_px[i], 
			  particles_py[i], 
			  particles_pz[i], 
			  particles_E[i], 
			  particles_xv[i] + fOrigin.X(), 
			  particles_yv[i] + fOrigin.Y(), 
			  particles_zv[i] + fOrigin.Z(), 
			  0,
			  polx, poly, polz, kPPrimary, ntr, 1., 2);
  }
}
//_____________________________________________________________________________
void StarMCTTreePrimaryGenerator::GeneratePrimaries(const TVector3& origin) {    
  // Fill the user stack (derived from TVirtualMCStack) with primary particles.
  // ---
  Double_t sigmaX = gEnv->GetValue("FixedSigmaX", 0.00176);
  Double_t sigmaY = gEnv->GetValue("FixedSigmaY", 0.00176);
  Double_t sigmaZ = gEnv->GetValue("FixedSigmaZ", 0.00176);
  TVector3 dR(gRandom->Gaus(0, sigmaX), gRandom->Gaus(0, sigmaY), gRandom->Gaus(0, sigmaZ));
  fOrigin = origin + dR;
  GeneratePrimary();  
  fStarStack->SetNprimaries(fNofPrimaries);
}
//_____________________________________________________________________________
void StarMCTTreePrimaryGenerator::GeneratePrimaries() {
  if (! fSetVertex) {
    if (fPVX && fPVY && fPVZ) {
      fOrigin.SetX(fPVX->GetRandom());
      fOrigin.SetY(fPVY->GetRandom());
      fOrigin.SetZ(fPVZ->GetRandom());
      if (fPVxyError) {
	Double_t dxy = fPVxyError->GetRandom()/TMath::Sqrt(2.);
	gEnv->SetValue("FixedSigmaX", dxy);
	gEnv->SetValue("FixedSigmaY", dxy);
      }
    } else {
      fOrigin.SetX(gRandom->Gaus(0,gSpreadX));
      fOrigin.SetY(gRandom->Gaus(0,gSpreadY));
      fOrigin.SetZ(gRandom->Gaus(0,gSpreadZ));
    }
  }
  GeneratePrimaries(fOrigin);
}
