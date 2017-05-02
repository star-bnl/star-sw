// $Id: StarCosmicRaysGenerator.cxx,v 1.1.1.1 2008/12/10 20:45:52 fisyak Exp $
#include "StarCosmicRaysGenerator.h"
#include "TGeant3.h"
#include "TRandom.h"
ClassImp(StarCosmicRaysGenerator);
//_____________________________________________________________________________
void StarCosmicRaysGenerator::GeneratePrimary() {     
  // Add one primary particle to the user stack (derived from TVirtualMCStack).
  // Track ID (filled by stack)
 // Particle type
  Int_t pdg = 13;
  if (gRandom->Rndm() > 0.5) pdg = -pdg;
  // dN/dp ~ 1/p**3
  static Double_t p_min = 0.5;
  static Double_t p_max = 100;
  static Double_t x_max = 1./(p_min*p_min);
  static Double_t x_min = 1./(p_max*p_max);
  Double_t x = x_min + (x_max - x_min) * gRandom->Rndm();
  Double_t p = 1./TMath::Sqrt(x);
  
 // Option: to be tracked
 Int_t toBeDone = 1; 
 // Polarization
 Double_t polx = 0.; 
 Double_t poly = 0.; 
 Double_t polz = 0.; 

 // Position
 static Double_t R = 200;
 Double_t Phi = TMath::TwoPi()*gRandom->Rndm();
 static Double_t Zmax = 200;
 TVector3 Vtx(R*TMath::Cos(Phi), R*TMath::Sin(Phi), 2*Zmax*(gRandom->Rndm() - 0.5));
 Double_t tof = 0.;
 
 static Double_t PxlSize = 1;
 TVector3 Pxl(2*PxlSize*(gRandom->Rndm() - 0.5), 2*PxlSize*(gRandom->Rndm() - 0.5), 2*PxlSize*(gRandom->Rndm() - 0.5));
 TVector3 P = Pxl - Vtx;
 P.SetMag(p);
 // Double_t kinEnergy = 0.050;  
 Double_t mass = TDatabasePDG::Instance()->GetParticle(pdg)->Mass();
 Double_t e  = TMath::Sqrt(mass*mass + p*p);
 // Add particle to stack 
 Int_t ntr = 1;
 fStarStack->PushTrack(toBeDone, -1, pdg, P.Px(), P.Py(), P.Pz(), e, Vtx.x(), Vtx.y(), Vtx.z(), tof, polx, poly, polz, 
                  kPPrimary, ntr, 1., 2);
 fNofPrimaries = 1;
}
//_____________________________________________________________________________
void StarCosmicRaysGenerator::GeneratePrimaries() {
  GeneratePrimary();  
  fStarStack->SetNprimaries(fNofPrimaries);
}
