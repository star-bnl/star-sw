// $Id: StarMCSimplePrimaryGenerator.cxx,v 1.1.1.1 2008/12/10 20:45:52 fisyak Exp $
#include "StarMCSimplePrimaryGenerator.h"
#include "TGeant3.h"
ClassImp(StarMCSimplePrimaryGenerator);
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
}
//_____________________________________________________________________________
void StarMCSimplePrimaryGenerator::SetGenerator(Int_t nprim, Int_t Id, 
						Double_t pT_min,Double_t pT_max,
						Double_t Eta_min, Double_t Eta_max, 
						Double_t Phi_min, Double_t Phi_max, 
						Double_t Z_min, Double_t Z_max, const Char_t *option) {
  fNofPrimaries = nprim; fId = Id;  
  fpT_min = pT_min; 
  fpT_max = pT_max; 
  fEta_min = Eta_min; 
  fEta_max = Eta_max; 
  fPhi_min = Phi_min; 
  fPhi_max = Phi_max; 
  fZ_min = Z_min; 
  fZ_max = Z_max; 
  fOption = option; 
  if (! fOption.CompareTo("G",TString::kIgnoreCase)) {
    fId = ((TGeant3* ) TVirtualMC::GetMC())->PDGFromId(Id);
  }
  cout << "Generate " << fNofPrimaries << " primary tracks of type " << fId << endl;
  cout << fpT_min << " <  pT < " << fpT_max << endl;
  cout << fEta_min  << " < eta < " << fEta_max  << endl;
  cout << fPhi_min<< " < phi < " << fPhi_max<< endl;
  cout << fZ_min  << " < zVer< " << fZ_max  << endl;
  fgInstance = this;
}
//_____________________________________________________________________________
void StarMCSimplePrimaryGenerator::GeneratePrimary() {     
  // Add one primary particle to the user stack (derived from TVirtualMCStack).
 // Track ID (filled by stack)
 Int_t ntr;
 
 // Option: to be tracked
 Int_t toBeDone = 1; 
 
 // Particle type
 Int_t pdg  = fId;
 
 // Polarization
 Double_t polx = 0.; 
 Double_t poly = 0.; 
 Double_t polz = 0.; 

 // Position
 Double_t vx  = fOrigin.X(); 
 Double_t vy  = fOrigin.Y(); 
 Double_t vz =  fOrigin.Z(); 
 Double_t tof = 0.;

 // Energy (in GeV)
 Double_t pT        = fpT_min + (fpT_max - fpT_min)*gRandom->Rndm();
 Double_t eta       = fEta_min + (fEta_max - fEta_min)*gRandom->Rndm();
 Double_t phi       = fPhi_min + (fPhi_max - fPhi_min)*gRandom->Rndm();
 // Particle momentum
 Double_t px, py, pz;
 px = pT*TMath::Cos(phi); 
 py = pT*TMath::Sin(phi);
 pz = pT*TMath::SinH(eta);
 // Double_t kinEnergy = 0.050;  
 Double_t mass = TDatabasePDG::Instance()->GetParticle(pdg)->Mass();
 Double_t e  = TMath::Sqrt(mass*mass + pz*pz + pT*pT);
 // Add particle to stack 
 fStarStack->PushTrack(toBeDone, -1, pdg, px, py, pz, e, vx, vy, vz, tof, polx, poly, polz, 
                  kPPrimary, ntr, 1., 0);
}

//
// public methods
//

//_____________________________________________________________________________
void StarMCSimplePrimaryGenerator::GeneratePrimaries(const TVector3& origin)
{    
// Fill the user stack (derived from TVirtualMCStack) with primary particles.
// ---
  fOrigin = origin;
  for (Int_t i=0; i<fNofPrimaries; i++) GeneratePrimary();  
  //  fStarStack->SetNprimary(fNofPrimaries);
}

//_____________________________________________________________________________
void StarMCSimplePrimaryGenerator::GeneratePrimaries() {
  
  fOrigin.SetZ(fZ_min + (fZ_max-fZ_min)*gRandom->Rndm());
  GeneratePrimaries(fOrigin);
}
