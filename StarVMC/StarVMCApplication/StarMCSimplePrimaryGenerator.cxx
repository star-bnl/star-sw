// $Id: StarMCSimplePrimaryGenerator.cxx,v 1.1.1.1 2008/12/10 20:45:52 fisyak Exp $
#include "StarMCSimplePrimaryGenerator.h"
#include "TFile.h"
#include "TGeant3.h"
#include "TF1.h"
#include "TEnv.h"
#include "TRandom.h"
#include "TSystem.h"
#include "TDatabasePDG.h"
#include "TParticlePDG.h"
#include "StDetectorDbMaker/St_vertexSeedC.h"
#include "StDetectorDbMaker/St_beamInfoC.h"
#include "StMessMgr.h" 
ClassImp(StarMCSimplePrimaryGenerator);
Double_t StarMCSimplePrimaryGenerator::fTemperature = 0.457;; // GeV/c
//_____________________________________________________________________________
StarMCSimplePrimaryGenerator::StarMCSimplePrimaryGenerator(Int_t    nprim,   Int_t    Id, 
							   Double_t pT_min , Double_t pT_max,
							   Double_t Eta_min, Double_t Eta_max, 
							   Double_t Phi_min, Double_t Phi_max, 
							   Double_t Z_min,   Double_t Z_max, 
							   const Char_t *option): StarMCPrimaryGenerator() {
  PreSet(); 
  SetGenerator(nprim, Id, pT_min, pT_max, Eta_min, Eta_max, 
	       Phi_min, Phi_max,  Z_min, Z_max, option);
}
//_____________________________________________________________________________
void StarMCSimplePrimaryGenerator::PreSet() {
  fStarStack = 0;
  fIsRandom = false;
  fNofPrimaries = 0; fId = 0;
  fpT_min = 0; fpT_max = 0; fEta_min = 0; fEta_max = 0; fPhi_min = 0; fPhi_max = 0; fZ_min = 0; fZ_max = 0;
  fOption = "";
  fOrigin = TVector3(0,0,0);
  fGun = kFALSE;
  fGunpX = fGunpY = fGunpZ = fGunX = fGunY = fGunZ = 0;
  fGunId = 0;
  fPVX = fPVY = fPVZ = fPVxyError = 0;
}
//_____________________________________________________________________________
void StarMCSimplePrimaryGenerator::SetGenerator(Int_t nprim, Int_t Id, 
						Double_t pT_min,Double_t pT_max,
						Double_t Eta_min, Double_t Eta_max, 
						Double_t Phi_min, Double_t Phi_max, 
						Double_t Z_min, Double_t Z_max, const Char_t *option) {
  fGun = kFALSE;
  fNofPrimaries = nprim; 
  fpT_min = pT_min; 
  fpT_max = pT_max; 
  fEta_min = Eta_min; 
  fEta_max = Eta_max; 
  fPhi_min = Phi_min; 
  fPhi_max = Phi_max; 
  fZ_min = Z_min; 
  fZ_max = Z_max; 
  fOption = option; 
  if (fOption.Contains("G",TString::kIgnoreCase)) {
    fId = ((TGeant3* ) TVirtualMC::GetMC())->PDGFromId(Id);
  } else {
    fId = Id;
  }
  LOG_INFO << "Generate " << fNofPrimaries << " primary tracks of type " << fId << " in " << endm;
  if (! fOption.Contains("BL",TString::kIgnoreCase)) {
    LOG_INFO << fpT_min << " <  pT < " << fpT_max << endm;
  } else {
    LOG_INFO << fpT_min << " <  log10(beta*gamma) < " << fpT_max << endm;
  }
  if (fOption.Contains("mtsq",TString::kIgnoreCase)) {
    LOG_INFO << "Use dN/dmT^2 = exp(-mT/T) pT generation with T = " << Temperature() << " GeV/c" << endm;
  } else if (fOption.Contains("mt",TString::kIgnoreCase)) {
    LOG_INFO << "Use dN/dmT = exp(-mT/T) pT generation with T = " << Temperature() << " GeV/c" << endm;
  }
  if ( fOption.Contains("y",TString::kIgnoreCase)) {
    if ( St_beamInfoC::instance()->IsFixedTarget()) {
      Double_t KinE = St_beamInfoC::instance()->getYellowEnergy(); // per nucleon
      const double kProtonMass = 0.938272321;    // Proton mass in GeV
      //      Double_t mass =TDatabasePDG::Instance()->GetParticle(fId)->Mass();
      Double_t E = KinE + kProtonMass;
      Double_t plab = -TMath::Sqrt(E*E - kProtonMass*kProtonMass);
      Double_t Ecm = TMath::Sqrt(2*kProtonMass*kProtonMass + 2*E*kProtonMass);
      Double_t Etot = E +  kProtonMass;
      Double_t gammaCM = Etot/Ecm;
      Double_t betaCM  = plab/Etot;
      Double_t yCM  = TMath::Log(gammaCM*(1 + betaCM));
      fEta_min = yCM;
      fEta_max = 0.5;
    }
    LOG_INFO << "Rapidity is Gaussian with mean =  " << fEta_min  << " and sigma = " << fEta_max << endm;
  } else {
    LOG_INFO << fEta_min  << " < eta < " << fEta_max  << endm;
  }
  LOG_INFO << fPhi_min<< " < phi < " << fPhi_max<< endm;
  LOG_INFO << fZ_min  << " < zVer< " << fZ_max  << endm;
  
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
      LOG_INFO << "PVxyz.root with x, y and z histograms has been found. These histogram will be use to generate primary vertex x, y, z.";
      if (fPVxyError) LOG_INFO << " hPVError histogram will be used for transverse PV error.";
      LOG_INFO << endm;
    }
    delete [] file;
  }
  fgInstance = this;
}
//_____________________________________________________________________________
void StarMCSimplePrimaryGenerator::GeneratePrimary() {     
  // Add one primary particle to the user stack (derived from TVirtualMCStack).
  // Track ID (filled by stack)
  static TF1 *dNdpT = 0; 
  Int_t ntr;
  // Option: to be tracked
  Int_t toBeDone = 1; 
  // Particle type
  Int_t pdg  = fId;
  if (fGun) pdg = fGunId;
  Double_t mass      = TDatabasePDG::Instance()->GetParticle(pdg)->Mass();
  // Polarization
  Double_t polx = 0.; 
  Double_t poly = 0.; 
  Double_t polz = 0.; 
  Double_t px = 0, py = 0, pz = 0;
  Double_t vx = 0, vy = 0, vz = 0;
  Double_t tof = 0.;
  if (fGun) {
    px = fGunpX; py = fGunpY; pz = fGunpZ; vx = fGunX; vy = fGunY; vz = fGunZ; 
  } else {
    // Position
    vx  = fOrigin.X(); 
    vy  = fOrigin.Y(); 
    vz =  fOrigin.Z(); 
    // Energy (in GeV)
    Double_t eta       = fEta_min + (fEta_max - fEta_min)*gRandom->Rndm();
    Double_t phi       = fPhi_min + (fPhi_max - fPhi_min)*gRandom->Rndm();
    Double_t pT        = 0;
    if (fOption.Contains("BL",TString::kIgnoreCase)) {
      Double_t p = -1;
      Double_t bgL10   = fpT_min + (fpT_max - fpT_min)*gRandom->Rndm();
      Double_t bg      = TMath::Power(10.,bgL10);
      p       = mass*bg;
      pT               = p/TMath::CosH(eta);
    } else if (fOption.Contains("mtsq",TString::kIgnoreCase)) {
      if (! dNdpT) {
	dNdpT = new TF1("dNdpT","x*TMath::Exp(-TMath::Sqrt(x*x+[0]*[0])/[1])", fpT_min,fpT_max);
	dNdpT->SetParameters(mass,Temperature());
      }
      pT = dNdpT->GetRandom();
    } else if (fOption.Contains("mt",TString::kIgnoreCase)) {
      while (pT < fpT_min || pT > fpT_max) {
	Double_t mT = mass -Temperature()*TMath::Log(gRandom->Rndm());
	Double_t pT2 = mT*mT - mass*mass;
	pT  = TMath::Sqrt(pT2);
      }
    } else {
      pT               = fpT_min + (fpT_max - fpT_min)*gRandom->Rndm();
    }
    // Particle momentum
    px = pT*TMath::Cos(phi); 
    py = pT*TMath::Sin(phi);
    if (fOption.Contains("y",TString::kIgnoreCase)) {
      Double_t mT = TMath::Sqrt(pT*pT + mass*mass);
      eta = gRandom->Gaus( fEta_min, fEta_max);
      pz = mT*TMath::SinH(eta);
    } else {
      pz = pT*TMath::SinH(eta);
    }
  }
  // Double_t kinEnergy = 0.050;  
  Double_t e  = TMath::Sqrt(mass*mass + px*px +py*py + pz*pz);
  // Add particle to stack 
  fStarStack->PushTrack(toBeDone, -1, pdg, px, py, pz, e, vx, vy, vz, tof, polx, poly, polz, 
			kPPrimary, ntr, 1., 2);
}
//_____________________________________________________________________________
void StarMCSimplePrimaryGenerator::GeneratePrimaries(const TVector3& origin) {    
  // Fill the user stack (derived from TVirtualMCStack) with primary particles.
  // ---
  Double_t sigmaX = gEnv->GetValue("FixedSigmaX", 0.00176);
  Double_t sigmaY = gEnv->GetValue("FixedSigmaY", 0.00176);
  Double_t sigmaZ = gEnv->GetValue("FixedSigmaZ", 0.00176);
  TVector3 dR(gRandom->Gaus(0, sigmaX), gRandom->Gaus(0, sigmaY), gRandom->Gaus(0, sigmaZ));
  fOrigin = origin + dR;
  for (Int_t i=0; i<fNofPrimaries; i++) GeneratePrimary();  
  fStarStack->SetNprimaries(fNofPrimaries);
}
//_____________________________________________________________________________
void StarMCSimplePrimaryGenerator::GeneratePrimaries() {
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
      if (fUseBeamLine) {
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
	  LOG_INFO << "Warning : Requested Beam Line, but there is no beam line" << endm;
	}
      } else {
	fOrigin.SetZ(fZ_min + (fZ_max-fZ_min)*gRandom->Rndm());
      }
    }
  }
  GeneratePrimaries(fOrigin);
}
//________________________________________________________________________________
void StarMCSimplePrimaryGenerator::SetGun(Int_t Id, 
					 Double_t px, Double_t py, Double_t pz,
					 Double_t x, Double_t y, Double_t z, const Char_t *option) {
  fGun = kTRUE;
  fGunpX = px; fGunpY = py; fGunpZ = pz; fGunX = x; fGunY = y; fGunZ = z; 
  fOption = option;
  if (fOption.Contains("G",TString::kIgnoreCase)) {
    fGunId = ((TGeant3* ) TVirtualMC::GetMC())->PDGFromId(Id);
  } else {
    fGunId = Id;
  }
  LOG_INFO << "StarMCSimplePrimaryGenerator::SetGun\tid = " << fGunId 
       << "\tpxyz = (" << fGunpX << "," << fGunpY << "," << fGunpZ
       << ")\txyz = (" << fGunX << "," << fGunY << "," << fGunZ << ")" << endm;
}
