// $Id: StMCGeneratePrimariesTest.cxx,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $
//
//
// Class StMCGeneratePrimariesTest
// ------------------
// Base class for Magnetic field calculation

#include <stdio.h>
#include <string.h>
#include "StMCGeneratePrimariesTest.h"
#include "TVirtualMC.h"
#include "TPDGCode.h"
#include "TRandom.h"



ClassImp(StMCGeneratePrimariesTest)

//_____________________________________________________________________________
StMCGeneratePrimariesTest::StMCGeneratePrimariesTest(int ntrk,int pdg)
  : GCall("StMCGeneratePrimariesTest","")
{
   SetNTrk(ntrk); SetPDG(pdg);
}   
//_____________________________________________________________________________
int StMCGeneratePrimariesTest::Fun() 
{ 
  // Fill the user stack (derived from TVirtualMCStack) with primary particles.
  // ---
  double SUM=0;  
  // Track ID (filled by stack)
  Int_t ntr;
 
  // Option: to be tracked
  Int_t toBeDone = 1; 
 
  // Particle type
//Int_t pdg   = kMuonMinus;    	//Muon-     PDG   convention 
//Int_t pdg   = 0;    		//GEANTINO  PDG   convention 
//Int_t Ipart = 6;   		//Muon-     Geant convention
//Int_t Ipart = 48;  		//GEANTINO  Geant convention
//Int_t pdg  = kProton;    // 
  // Polarization
  Double_t polx = 0.; 
  Double_t poly = 0.; 
  Double_t polz = 0.; 

  // Position
  Double_t vx  = 0.01; 
  Double_t vy  = 0.02; 
  // --max--
  Double_t vz = (gRandom->Rndm()-0.5)*4;
  Double_t tof = 0.;

  // Energy
  Double_t kinEnergy = 3.0;
  Double_t mass = fMC->ParticleMass(fPDG);
  Double_t e  = mass + kinEnergy;
  // Momentum
  double px,py,pz,pp,pxy;
  pp = sqrt(e*e - mass*mass);
  
  // --max--
  //   px = 0.; 
  //   py = 0.; 
  //   pz = sqrt(e*e - mass*mass); 
  SUM += vx + vy + vz;
  for (int i=0;i<fNTrk;i++) {

    double phi =gRandom->Rndm()*2*3.1415926;
    double kos =gRandom->Rndm()*2-1.;
    pz  = pp*kos;
    pxy = (1.-kos*kos); if (pxy<0.) pxy=0;
    pxy = pp *sqrt(pxy);
    px  = pxy*cos (phi);
    py  = pxy*sin (phi); 
    SUM += px+py+pz;
    // Add particle to stack 
    fStack->PushTrack(toBeDone, -1, fPDG, px, py, pz, e, vx, vy, vz, tof, polx, poly, polz, 
		      kPPrimary, ntr, 1., 0);
  }
//  printf("Control SUM = %g\n",SUM);
  return 0;
}
//_____________________________________________________________________________
void StMCGeneratePrimariesTest::Print(const Option_t*) const
{
}		
		
		
		
