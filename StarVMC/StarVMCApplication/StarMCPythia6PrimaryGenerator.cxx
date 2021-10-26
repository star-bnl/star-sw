// $Id: StarMCPythia6PrimaryGenerator.cxx,v 1.1.1.1 2008/12/10 20:45:52 fisyak Exp $
#include "StarMCPythia6PrimaryGenerator.h"
#include "TFile.h"
#include "TGeant3.h"
#include "TEnv.h"
#include "TRandom.h"
#include "TSystem.h"
#include "TDatabasePDG.h"
#include "TParticlePDG.h"
#include "TPythia6Calls.h"
#include "TMCParticle.h"
#include "StDetectorDbMaker/St_vertexSeedC.h"
#include "StDetectorDbMaker/St_beamInfoC.h"
#include "StMessMgr.h" 
ClassImp(StarMCPythia6PrimaryGenerator);
//_____________________________________________________________________________
StarMCPythia6PrimaryGenerator::StarMCPythia6PrimaryGenerator(TString mode, Int_t tune) {
  PreSet(); 
  SetGenerator(mode,tune);
}
//_____________________________________________________________________________
void StarMCPythia6PrimaryGenerator::PreSet() {
  fStarStack = 0;
  fNofPrimaries = 0; fId = 0;
  fOption = "";
  fOrigin = TVector3(0,0,0);
  fPVX = fPVY = fPVZ = fPVxyError = 0;
  fPythia6 = 0;
  SetSpread(0.15, 0.15, 42.0);
}
//_____________________________________________________________________________
void StarMCPythia6PrimaryGenerator::SetGenerator(TString mode, Int_t tune) {
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
  fPythia6 = new TPythia6;
  TString blue("p");
  TString yellow("p");
  Double_t energy = 510;
  if (mode.Contains("pp",TString::kIgnoreCase)) {
#if 0
    if (mode.Contains("W",TString::kIgnoreCase)) {
      // Setup pythia process
      PySubs_t &pysubs = fPythia6->pysubs();
      pysubs.msel = 12;
      pysubs.ckin(3)=4.0;
    }
#endif
    if (mode.Contains("510")) energy = 510;
    if (mode.Contains("200")) energy = 200;
#if 0
  } else if ( mode.Contains("ep",TString::kIgnoreCase)) {
    blue = "e-";
    yellow = "p";
    Double_t pblue[]={0.,0.,30.0};
    Double_t pyell[]={0.,0.,-320.0};
    fPythia6->SetFrame("3MOM", pblue, pyell );
    fPythia6->SetBlue("e-");
    fPythia6->SetYell("proton");
#endif
  }
  fPythia6->Initialize("cms", blue, yellow, energy);
#if 0
  if ( tune ) fPythia6->PyTune( tune );
#endif
  fgInstance = this;
}
//_____________________________________________________________________________
void StarMCPythia6PrimaryGenerator::GeneratePrimary() {     
  // Add one primary particle to the user stack (derived from TVirtualMCStack).
  // Track ID (filled by stack)
  fPythia6->GenerateEvent();
  static TClonesArray particles("TParticle");
  fPythia6->ImportParticles(&particles);
  Int_t N = particles.GetLast();
  // Option: to be tracked
  Int_t toBeDone = 1; 
  Double_t polx = 0.; 
  Double_t poly = 0.; 
  Double_t polz = 0.; 
  Int_t ntr = 0;
  fNofPrimaries = 0;
  for (Int_t i = 0; i <= N; i++) {
    TParticle *p = (TParticle *) particles.UncheckedAt(i);
    if (! p) continue;
    // Add particle to stack 
    fStarStack->PushTrack(toBeDone, -1, p->GetPdgCode(), p->Px(), p->Py(), p->Pz(), p->Energy(), 
			  p->Vx() + fOrigin.X(), p->Vy() + fOrigin.Y(), p->Vz() + fOrigin.Z(), p->T(),  
			  polx, poly, polz, kPPrimary, ntr, 1., 2);
    fNofPrimaries++;
  }
}
//_____________________________________________________________________________
void StarMCPythia6PrimaryGenerator::GeneratePrimaries(const TVector3& origin) {    
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
void StarMCPythia6PrimaryGenerator::GeneratePrimaries() {
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
#if 0
    } else if (fUseBeamLine) {
      St_vertexSeedC* vSeed = St_vertexSeedC::instance();
      if (vSeed) {
	Double_t x0   = vSeed->x0()  ;// Double_t err_x0   = vSeed->err_x0();
	Double_t y0   = vSeed->y0()  ;// Double_t err_y0   = vSeed->err_y0();
	//	Double_t z0   = 0            ;// Double_t err_z0   = 60; 
	Double_t dxdz = vSeed->dxdz();
	Double_t dydz = vSeed->dydz(); 
	Double_t z    = fZ_min + (fZ_max-fZ_min)*gRandom->Rndm();
	SetOrigin(x0 + dxdz*z, y0 + dydz*z, z);
      } else {
	LOG_WARN << "Warning : Requested Beam Line, but there is no beam line" << endm;
      }
#endif
    } else {
      fOrigin.SetX(gRandom->Gaus(0,gSpreadX));
      fOrigin.SetY(gRandom->Gaus(0,gSpreadY));
      fOrigin.SetZ(gRandom->Gaus(0,gSpreadZ));
    }
  }
  GeneratePrimaries(fOrigin);
}
