// $Id: StarKineGenerator.cxx,v 1.3 2004/09/02 23:27:55 potekhin Exp $
// $Log: StarKineGenerator.cxx,v $
// Revision 1.3  2004/09/02 23:27:55  potekhin
// Cosmetic
//

#include <iostream.h>
#include <TGenerator.h>
#include <TMCProcess.h>

#include "StarKineGenerator.h"


ClassImp(StarKineGenerator)

StarKineGenerator::StarKineGenerator():StarGenerator() {}
StarKineGenerator::StarKineGenerator(Int_t npart):StarGenerator(npart) {}
StarKineGenerator::StarKineGenerator(const StarGenerator &gen): StarGenerator(gen) {}

StarKineGenerator::~StarKineGenerator() {}
//_______________________________________________________________________
void StarKineGenerator::Init(void) {
  cout<<"Initializing with seed: "<<_seed<<endl;
  srand48(_seed);
}
//_______________________________________________________________________
void StarKineGenerator::Generate(void) { // Fill the user stack (derived from TVirtualMCStack) with primary particles.

  Int_t ntr;   // Track ID (filled by stack)
  Int_t toBeDone = 1; // Option: to be tracked
 
  // Particle type
  Int_t pdg  = kElectron;   //Int_t pdg  = 0;    // geantino
 
  // Polarization
  Double_t polx = 0.; 
  Double_t poly = 0.; 
  Double_t polz = 0.; 

  // Position
  Double_t vx  = 0.; 
  Double_t vy  = 0.; 
  Double_t vz  = 0.0;  // --max--  Double_t vz = -0.5*(fDetConstruction.GetWorldFullLength());

  Double_t tof = 0.;

  // Energy
  Double_t kinEnergy = 2.0;
  Double_t mass = 0.9382723;
  Double_t e  = mass + kinEnergy;
 
  Double_t pt = 2.0;

  // Momentum
  Double_t px, py, pz;
  // --max--
  //   px = 0.; 
  //   py = 0.; 
  //   pz = sqrt(e*e - mass*mass); 

  for (int i=0;i<1;i++) {

    float phi   = drand48()*2*3.1415926;
    float eta   = drand48()*2*1.0;
    float theta = atan(exp(-eta))*2.0;
    px = pt*cos(phi);
    py = pt*sin(phi); 
    pz = pt/tan(theta);

    cout<<"Generating particle "<<i<<" "<<px<<" "<<py<<" "<<pz<<endl;

    // Add particle to stack 
    _stack->PushTrack(toBeDone, -1, pdg, px, py, pz, e, vx, vy, vz, tof, polx, poly, polz, 
		      kPPrimary, ntr, 1., 0);
  }

}


