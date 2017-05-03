// $Id: StarMCPrimaryGenerator.cxx,v 1.2 2011/02/11 16:12:52 fisyak Exp $
#include <map>
#include "TVirtualMC.h"
#include "TVirtualMCStack.h"
#include "TVirtualMCApplication.h"
#include "TRandom.h"
#include "TPDGCode.h"
#include "TDatabasePDG.h"
#include "TVector3.h"
#include "TMath.h"
#include "TRandom.h"
#include "Stiostream.h"
#include "StarMCPrimaryGenerator.h"
StarMCPrimaryGenerator *StarMCPrimaryGenerator::fgInstance = 0;
ClassImp(StarMCPrimaryGenerator);
//________________________________________________________________________________
void StarMCPrimaryGenerator::Print(Option_t *option) const {
  if (fStarStack) fStarStack->Print();
}
//________________________________________________________________________________
void StarMCPrimaryGenerator::SetStack(StarParticleStack *stack) {
  if (! stack) return;
  // Option: to be tracked
  Int_t toBeDone = 1; 
  // Particle type
  //  Int_t pdg  = 0;
  // Polarization
  Double_t polx = 0.; 
  Double_t poly = 0.; 
  Double_t polz = 0.; 
  // Position
  Double_t weight = 1;
  // Particle momentum
  TParticle *part = 0;
  //  Int_t itrk = -1;
  Int_t ntr  =  0;
  Int_t nop  = stack->Particles()->GetEntriesFast();
  map<Int_t,Int_t> IdOld2New;
  for (Int_t i = 0; i < nop; i++) {
    //  while ((part = stack->PopNextTrack(itrk))) {
    IdOld2New[i] = -1;
    part = stack->GetParticle(i);
    if ( part->GetStatusCode() != 1 ) continue;
    IdOld2New[i] = ntr;
    Int_t parent = IdOld2New[part->GetFirstMother()];
    //    cout << "IdOld2New[" << i << "] = " << IdOld2New[i] << " old " << part->GetFirstMother() << " new " << parent << endl;
  // Load a track on the stack
  //
  // done     0 if the track has to be transported
  //          1 if not
  // parent   identifier of the parent track. -1 for a primary
  // pdg    particle code
  // pmom     momentum GeV/c
  // vpos     position
  // polar    polarization
  // tof      time of flight in seconds
  // mecha    production mechanism
  // ntr      on output the number of the track stored
    fStarStack->PushTrack(toBeDone, parent, part->GetPdgCode(), part->Px(), part->Py(), part->Pz(), part->Energy(), 
			  part->Vx(), part->Vy(), part->Vz(), part->T(), polx, poly, polz, 
			  kPPrimary, ntr, weight,  part->GetStatusCode());
  }
}
