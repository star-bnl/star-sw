// $Id: StarStack.cxx,v 1.2 2004/07/16 22:51:42 potekhin Exp $
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
StarStack::StarStack(Int_t size) :
  _particles(0), _currentTrack(-1), _Nprimary(0),  _Ntrack(0) {

  _particles = new TObjArray(size);
}

//_____________________________________________________________________________
StarStack::StarStack() :
  _particles(0),  _currentTrack(-1), _Nprimary(0),  _Ntrack(0)
{}

//_____________________________________________________________________________
StarStack::~StarStack() 
{
//
  if (_particles) _particles->Delete();
  delete _particles;
}

//_____________________________________________________________________________
void  StarStack::PushTrack(Int_t toBeDone,  Int_t parent,  Int_t pdg,
			   Double_t px,     Double_t py,   Double_t pz, Double_t e,
			   Double_t vx,     Double_t vy,   Double_t vz, Double_t tof,
			   Double_t polx,   Double_t poly, Double_t polz,
			   TMCProcess mech, Int_t& ntr,    Double_t weight, Int_t is) {

// Creates a new particle with specified properties,
// adds it to the particles array (_particles) and if not done to the  stack 
// ---

  const Int_t kFirstDaughter = -1;
  const Int_t kLastDaughter  = -1;
  
  TParticle* particleDef = new TParticle(pdg, is, parent, -1,
					 kFirstDaughter, kLastDaughter,
					 px, py, pz, e, vx, vy, vz, tof);
   
  particleDef->SetPolarisation(polx, poly, polz);
  particleDef->SetWeight      (weight);
  particleDef->SetUniqueID    (mech);

  StarParticle* mother = 0;
  if (parent>=0) 
    mother = GetParticle(parent);
  else
    _Nprimary++;  

  StarParticle* particle = new StarParticle(GetNtrack(), particleDef, mother);

  if (mother) mother->AddDaughter(particle);

  _particles->Add(particle);
    
  ntr=_Ntrack++;

  if (toBeDone) {
    _stack.push(particle);
    //    TString name = ((TNamed*) particleDef)->GetName();
    //    cout<<"particle "<<name<<" parent "<<parent<<" to be done "<<toBeDone<<" total tracks "<<(ntr-1)<<endl;
  }


}

//_____________________________________________________________________________
TParticle* StarStack::PopNextTrack(Int_t& itrack)
{
// Gets next particle for tracking from the stack.
// ---

  itrack = -1;
  if  (_stack.empty()) return 0;
		      
  StarParticle* particle = _stack.top();
  _stack.pop();

  if (!particle) return 0;  
  
  itrack = particle->GetID();
  _currentTrack = itrack;
  //  cout<<"popping "<<particle->GetParticle()->GetName()<<endl;

  return particle->GetParticle();
}    

//_____________________________________________________________________________
TParticle* StarStack::PopPrimaryForTracking(Int_t i)
{
// Returns i-th particle in _particles.
// ---

  if (i < 0 || i >= _Nprimary)
    Fatal("GetPrimaryForTracking", "Index out of range"); 
  
  return ((StarParticle*)_particles->At(i))->GetParticle();
}     


//_____________________________________________________________________________
void  StarStack::PurifyKine(void) {
  // Compress kinematic tree, keeping only flagged particles
  // and renaming the particle id's in all the hits
}

//_____________________________________________________________________________
void StarStack::Print(void) const 
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

  _currentTrack = -1;
  _Nprimary = 0;
  _particles->Delete();
}       

//_____________________________________________________________________________
void  StarStack::SetCurrentTrack(Int_t track) 
{
// Sets the current track to a given value.
// ---

  _currentTrack = track;
}     

//_____________________________________________________________________________
Int_t  StarStack::GetNtrack() const 
{
// Returns the number of all tracks.
// ---

  return _particles->GetEntriesFast();
}  

//_____________________________________________________________________________
Int_t  StarStack::GetNprimary() const 
{
// Returns the number of primary tracks.
// ---

  return _Nprimary;
}  

//_____________________________________________________________________________
TParticle* StarStack::GetCurrentTrack() const
{
// Gets the current track particle.
// ---

  StarParticle* c = GetParticle(_currentTrack);
  return c ? c->GetParticle() : 0;
}

//_____________________________________________________________________________
Int_t  StarStack::GetCurrentTrackNumber() const 
{
// Returns the current track ID.
// ---

  return _currentTrack;
}  

//_____________________________________________________________________________
Int_t  StarStack::GetCurrentParentTrackNumber() const 
{
// Returns the current track parent ID.
// ---

  StarParticle* current = GetParticle(_currentTrack);
  
  if (!current) return -1; 
  
  StarParticle* mother = current->GetMother();
  
  if (!mother) return -1;
    
  return  mother->GetID();
}  

//_____________________________________________________________________________
StarParticle*  StarStack::GetParticle(Int_t id) const
{
// Returns id-th particle in _particles.
// ---

  if (id < 0 || id >= _particles->GetEntriesFast())  Fatal("GetParticle", "Index out of range"); 
  return (StarParticle*) _particles->At(id);
}


