// $Id: StMCSimplePrimaryGenerator.cxx,v 1.4 2012/06/11 16:17:42 fisyak Exp $
#include <stdio.h>
#include <assert.h>
#include "Riostream.h"
#include "StMCSimplePrimaryGenerator.h"
#include "TString.h"
#include "TDatabasePDG.h"
#include "TRandom.h"
#include "TMath.h"
using namespace std;
ClassImp(StMCSimplePrimaryGenerator);
//_____________________________________________________________________________
  StMCSimplePrimaryGenerator::StMCSimplePrimaryGenerator(
  int nprim, int Id, 
  double pT_min,  double pT_max,
  double Eta_min, double Eta_max, 
  double Phi_min, double Phi_max, 
  double Z_min,   double Z_max, const char *option)
  :StMCPrimaryGenerator() 
{
  SetGenerator(	nprim	,Id, 
  		pT_min	,pT_max,
  		Eta_min	,Eta_max, 
  		Phi_min	,Phi_max, 
  		Z_min	,Z_max  , option);
}

//_____________________________________________________________________________
void StMCSimplePrimaryGenerator::SetGenerator(int nprim, int Id, 
						double pT_min,double pT_max,
						double Eta_min, double Eta_max, 
						double Phi_min, double Phi_max, 
						double Z_min, double Z_max, const Char_t *option) {
  fNTrk = nprim; fPDG = Id;  
  fpT_min = pT_min; 
  fpT_max = pT_max; 
  fEta_min = Eta_min; 
  fEta_max = Eta_max; 
  fPhi_min = Phi_min; 
  fPhi_max = Phi_max; 
  fZ_min = Z_min; 
  fZ_max = Z_max; 
  TString opt(option); 
  if (! opt.CompareTo("G",TString::kIgnoreCase)) fPDG = TDatabasePDG::Instance()->ConvertGeant3ToPdg(fPDG);
  cout << "Generate " << fNTrk << " primary tracks of type " << fPDG << endl;
  cout << fpT_min << " <  pT < " << fpT_max << endl;
  cout << fEta_min  << " < eta < " << fEta_max  << endl;
  cout << fPhi_min<< " < phi < " << fPhi_max<< endl;
  cout << fZ_min  << " < zVer< " << fZ_max  << endl;
}
//_____________________________________________________________________________
void StMCSimplePrimaryGenerator::GenerateOnePrimary() {     
  // Add one primary particle to the user stack (derived from TVirtualMCStack).
 // Track ID (filled by stack)
 int ntr;
 
 // Option: to be tracked
 int toBeDone = 1; 
 
 // Particle type
 int pdg  = fPDG;
 
 // Polarization
 double polx = 0.; 
 double poly = 0.; 
 double polz = 0.; 

 // Position
 double tof = 0.;

 // Energy (in GeV)
 double pT        = fpT_min  + (fpT_max  - fpT_min )*gRandom->Rndm();
 double eta       = fEta_min + (fEta_max - fEta_min)*gRandom->Rndm();
 double phi       = fPhi_min + (fPhi_max - fPhi_min)*gRandom->Rndm();
 phi *= M_PI/180;
 
 // Particle momentum
 double px, py, pz;
 px = pT*TMath::Cos(phi); 
 py = pT*TMath::Sin(phi);
 pz = pT*TMath::SinH(eta);
 // double kinEnergy = 0.050;  
 double mass = TDatabasePDG::Instance()->GetParticle(pdg)->Mass();
 if (mass<=0.) mass = 1e-8;
 double e  = TMath::Sqrt(mass*mass + pz*pz + pT*pT);
 // Add particle to stack 
 assert(e>1e-6);
 PushTrack(toBeDone, -1, pdg, px, py, pz, e
          ,fVtx[0], fVtx[1],fVtx[2]
	  ,tof    , polx   ,poly, polz, 
           kPPrimary, ntr, 1., 0);
}
//_____________________________________________________________________________
int StMCSimplePrimaryGenerator::Fun() {
  
  fVtx[2]=(fZ_min + (fZ_max-fZ_min)*gRandom->Rndm());
  for (int i=0; i<fNTrk; i++) {GenerateOnePrimary();} 
  return 0;
}
