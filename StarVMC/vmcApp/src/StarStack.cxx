// $Id: StarStack.cxx,v 1.1 2004/07/12 20:36:39 potekhin Exp $
//

// Warning this material comes from Ex02 and is not adapted from Ali

#include "StarStack.h"

#include <TParticle.h>
#include <TObjArray.h>
#include <TError.h>

#include <iostream>

using namespace std;

ClassImp(StarStack)

//_____________________________________________________________________________
StarStack::StarStack(Int_t size)
  : fParticles(0),
    fCurrentTrack(-1),
    fNPrimary(0)
{
//
  fParticles = new TObjArray(size);
}

//_____________________________________________________________________________
StarStack::StarStack()
  : fParticles(0),
    fCurrentTrack(-1),
    fNPrimary(0)
{
//
}

//_____________________________________________________________________________
StarStack::~StarStack() 
{
//
  if (fParticles) fParticles->Delete();
  delete fParticles;
}

// private methods

// public methods

//_____________________________________________________________________________
void  StarStack::PushTrack(Int_t toBeDone, Int_t parent, Int_t pdg,
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

  StarParticle* mother = 0;
  if (parent>=0) 
    mother = GetParticle(parent);
  else
    fNPrimary++;  

  StarParticle* particle = new StarParticle(GetNtrack(), particleDef, mother);
  if (mother) mother->AddDaughter(particle);

  fParticles->Add(particle);
    
  if (toBeDone) fStack.push(particle);  
}			 

//_____________________________________________________________________________
TParticle* StarStack::PopNextTrack(Int_t& itrack)
{
// Gets next particle for tracking from the stack.
// ---

  itrack = -1;
  if  (fStack.empty()) return 0;
		      
  StarParticle* particle = fStack.top();
  fStack.pop();

  if (!particle) return 0;  
  
  itrack = particle->GetID();
  fCurrentTrack = itrack;

  return particle->GetParticle();
}    

//_____________________________________________________________________________
TParticle* StarStack::PopPrimaryForTracking(Int_t i)
{
// Returns i-th particle in fParticles.
// ---

  if (i < 0 || i >= fNPrimary)
    Fatal("GetPrimaryForTracking", "Index out of range"); 
  
  return ((StarParticle*)fParticles->At(i))->GetParticle();
}     

//_____________________________________________________________________________
void StarStack::Print() const 
{
// Prints info for all particles.
// ---

/*
  cout << "StarStack Info  " << endl;
  cout << "Total number of particles:   " <<  GetNtrack() << endl;
  cout << "Number of primary particles: " <<  GetNprimary() << endl;

  for (Int_t i=0; i<GetNtrack(); i++) {
    GetParticle(i)->Print();
    //GetParticle(i)->PrintDaughters();
  }
 */
}

//_____________________________________________________________________________
void StarStack::Reset()
{
// Deletes contained particles, resets particles array and stack.
// ---

  // reset fStack
  fCurrentTrack = -1;
  fNPrimary = 0;
  fParticles->Delete();
}       

//_____________________________________________________________________________
void  StarStack::SetCurrentTrack(Int_t track) 
{
// Sets the current track to a given value.
// ---

  fCurrentTrack = track;
}     

//_____________________________________________________________________________
Int_t  StarStack::GetNtrack() const 
{
// Returns the number of all tracks.
// ---

  return fParticles->GetEntriesFast();
}  

//_____________________________________________________________________________
Int_t  StarStack::GetNprimary() const 
{
// Returns the number of primary tracks.
// ---

  return fNPrimary;
}  

//_____________________________________________________________________________
TParticle* StarStack::GetCurrentTrack() const
{
// Gets the current track particle.
// ---

  StarParticle* current = GetParticle(fCurrentTrack);
  
  if (current) 
    return  current->GetParticle();
  else 
    return 0;
}

//_____________________________________________________________________________
Int_t  StarStack::GetCurrentTrackNumber() const 
{
// Returns the current track ID.
// ---

  return fCurrentTrack;
}  

//_____________________________________________________________________________
Int_t  StarStack::GetCurrentParentTrackNumber() const 
{
// Returns the current track parent ID.
// ---

  StarParticle* current = GetParticle(fCurrentTrack);
  
  if (!current) return -1; 
  
  StarParticle* mother = current->GetMother();
  
  if (!mother) return -1;
    
  return  mother->GetID();
}  

//_____________________________________________________________________________
StarParticle*  StarStack::GetParticle(Int_t id) const
{
// Returns id-th particle in fParticles.
// ---

  if (id < 0 || id >= fParticles->GetEntriesFast())
    Fatal("GetParticle", "Index out of range"); 
   
  return (StarParticle*)fParticles->At(id);
}


