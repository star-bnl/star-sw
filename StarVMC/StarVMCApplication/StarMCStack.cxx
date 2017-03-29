// $Id: StarMCStack.cxx,v 1.1.1.1 2008/12/10 20:45:53 fisyak Exp $
#include <iostream>
#include "StarMCStack.h"

using namespace std;

ClassImp(StarMCStack)

//_____________________________________________________________________________
void  StarMCStack::PushTrack(Int_t toBeDone, Int_t parent, Int_t pdg,
			     Double_t px, Double_t py, Double_t pz, Double_t e,
			     Double_t vx, Double_t vy, Double_t vz, Double_t tof,
			     Double_t polx, Double_t poly, Double_t polz,
			     TMCProcess mech, Int_t& ntr, Double_t weight,
			     Int_t is) {
// Creates a new particle with specified properties,
// adds it to the particles array (fParticles) and if not done to the 
// stack (fStack).
// ---
  static const Double_t yMax = 6.3;
  static const Double_t pEMax = TMath::TanH(yMax);

  const Int_t kFirstDaughter=-1;
  const Int_t kLastDaughter=-1;
  
  TParticle* particleDef
    = new TParticle(pdg, is, parent, -1, kFirstDaughter, kLastDaughter,
		     px, py, pz, e, vx, vy, vz, tof);
   
  particleDef->SetPolarisation(polx, poly, polz);
  particleDef->SetWeight(weight);
  particleDef->SetUniqueID(mech);

  StarMCParticle* mother = 0;
  StarMCParticle* particle = 0;
  Int_t ID = GetNtrack();// + 1; //
  if (parent>=0 && toBeDone && mech != kPPrimary) {
    mother = GetParticle(parent);
    ID = mother->GetID();
    particle = new StarMCParticle(ID, particleDef, mother);
    if (mother) mother->AddDaughter(particle);
  }
  else {
    // rapidity cut
    if (TMath::Abs(pz/e) > pEMax) toBeDone = 0;
    particle = new StarMCParticle(ID, particleDef, 0);
    fParticles->Add(particle);
  }
    
  if (toBeDone) fStack.push(particle);  
}			 
//_____________________________________________________________________________
TParticle* StarMCStack::PopNextTrack(Int_t& itrack) {
// Gets next particle for tracking from the stack.
// ---

  fCurrentTrack  = -1;
  fCurrentParticle = 0;
  itrack = fCurrentTrack;
  if  (fStack.empty()) return 0;
		      
  fCurrentParticle = fStack.top();
  fStack.pop();

  if (!fCurrentParticle) return 0;  
  
  itrack = fCurrentParticle->GetID();
  fCurrentTrack = itrack;

  return fCurrentParticle->GetParticle();
}    
//_____________________________________________________________________________
void StarMCStack::Print(const Option_t *opt) const {
// Print(const Option_t *opt=0)s info for all particles.
// ---
  
  cout << "StarMCStack Info  " << endl;
  cout << "Total number of particles:   " <<  GetNtrack() << endl;
  cout << "Number of primary particles: " <<  GetNprimary() << endl;

  for (Int_t i=0; i<GetNtrack(); i++) {
    GetParticle(i)->Print();
    //GetParticle(i)->PrintDaughters();
  }  
}
//_____________________________________________________________________________
void StarMCStack::Reset() {
  // Deletes contained particles, resets particles array and stack.
  // ---
  
  // reset fStack should be empty by this time
  assert(fStack.empty());
  fCurrentTrack = -1;
  fNPrimary = 0;
  fParticles->Delete();
}       
//_____________________________________________________________________________
TParticle* StarMCStack::GetCurrentTrack() const
{
// Gets the current track particle.
// ---

  StarMCParticle* current = GetParticle(fCurrentTrack);
  
  if (current) 
    return  current->GetParticle();
  else 
    return 0;
}

//_____________________________________________________________________________
Int_t  StarMCStack::GetCurrentParentTrackNumber() const 
{
// Returns the current track parent ID.
// ---

  StarMCParticle* current = GetParticle(fCurrentTrack);
  
  if (!current) return -1; 
  
  StarMCParticle* mother = current->GetMother();
  
  if (!mother) return -1;
    
  return  mother->GetID();
}  

//_____________________________________________________________________________
StarMCParticle*  StarMCStack::GetParticle(Int_t id) const
{
// Returns id-th particle in fParticles.
// ---

//  if (! fParticles->BoundsOk("StarMCStack::GetParticle",id)) Fatal("GetParticle", "Index out of range"); 
   
  return (StarMCParticle*) fParticles->At(id);
}


