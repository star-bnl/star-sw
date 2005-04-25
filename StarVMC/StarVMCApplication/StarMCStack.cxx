// $Id: StarMCStack.cxx,v 1.1 2005/04/25 20:44:28 fisyak Exp $
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
		         Int_t is) 
{
// Creates a new particle with specified properties,
// adds it to the particles array (fParticles) and if not done to the 
// stack (fStack).
// ---

  const Int_t kFirstDaughter=-1;
  const Int_t kLastDaughter=-1;
  
  TParticle* particleDef
    = new TParticle(pdg, is, parent, -1, kFirstDaughter, kLastDaughter,
		     px, py, pz, e, vx, vy, vz, tof);
   
  particleDef->SetPolarisation(polx, poly, polz);
  particleDef->SetWeight(weight);
  particleDef->SetUniqueID(mech);

  StarMCParticle* mother = 0;
  if (parent>=0) 
    mother = GetParticle(parent);
  else
    fNPrimary++;  

  StarMCParticle* particle = new StarMCParticle(GetNtrack(), particleDef, mother);
  if (mother) mother->AddDaughter(particle);

  fParticles->Add(particle);
    
  if (toBeDone) fStack.push(particle);  
}			 

//_____________________________________________________________________________
TParticle* StarMCStack::PopNextTrack(Int_t& itrack)
{
// Gets next particle for tracking from the stack.
// ---

  itrack = -1;
  if  (fStack.empty()) return 0;
		      
  StarMCParticle* particle = fStack.top();
  fStack.pop();

  if (!particle) return 0;  
  
  itrack = particle->GetID();
  fCurrentTrack = itrack;

  return particle->GetParticle();
}    

//_____________________________________________________________________________
TParticle* StarMCStack::PopPrimaryForTracking(Int_t i)
{
// Returns i-th particle in fParticles.
// ---

  if (i < 0 || i >= fNPrimary)
    Fatal("GetPrimaryForTracking", "Index out of range"); 
  
  return ((StarMCParticle*)fParticles->At(i))->GetParticle();
}     

//_____________________________________________________________________________
void StarMCStack::Print(const Option_t *opt) const 
{
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
void StarMCStack::Reset()
{
  // Deletes contained particles, resets particles array and stack.
  // ---
  
  // reset fStack
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

  if (id < 0 || id >= fParticles->GetEntriesFast())
    Fatal("GetParticle", "Index out of range"); 
   
  return (StarMCParticle*)fParticles->At(id);
}


