/* $Id: StarKineGenerator.cxx,v 1.1 2004/07/12 20:36:39 potekhin Exp $ */

#include <iostream.h>
#include <TGenerator.h>
#include <TMCProcess.h>

#include "StarKineGenerator.h"


ClassImp(StarKineGenerator)

StarKineGenerator::StarKineGenerator():StarGenerator() {}
StarKineGenerator::StarKineGenerator(Int_t npart):StarGenerator(npart) {}
StarKineGenerator::StarKineGenerator(const StarGenerator &gen): StarGenerator(gen) {}

StarKineGenerator::~StarKineGenerator() {}
//********************************************************************
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

  for (int i=0;i<5;i++) {

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


