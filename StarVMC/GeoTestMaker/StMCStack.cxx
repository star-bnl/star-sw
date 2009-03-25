// $Id: StMCStack.cxx,v 1.1 2009/03/25 23:15:10 perev Exp $
#include <iostream>
#include "StMCStack.h"
#include <TParticle.h>
#include <TObjArray.h>
#include <TError.h>
#include "StarMCParticle.h"

using namespace std;

ClassImp(StMCStack)
//_____________________________________________________________________________
StMCStack::StMCStack(int size) : 
fParticles(0), fCurrentTrack(-1), fNPrimary(0), fCurrentParticle(0) 
{
 if (size > 0) fParticles = new TObjArray(size);
}
//_____________________________________________________________________________
 StMCStack::~StMCStack() 
{
 if (fParticles) fParticles->Delete(); delete fParticles;
}
//_____________________________________________________________________________
TParticle* StMCStack::PopPrimaryForTracking(int i)	
{
return ((StarMCParticle *)fParticles->At(i))->GetParticle();
}

//_____________________________________________________________________________
void  StMCStack::PushTrack(int toBeDone, int parent, int pdg,
			     double px, double py, double pz, double e,
			     double vx, double vy, double vz, double tof,
			     double polx, double poly, double polz,
			     TMCProcess mech, int& ntr, double weight,
			     int is) {
// Creates a new particle with specified properties,
// adds it to the particles array (fParticles) and if not done to the 
// stack (fStack).
// ---
  static const double yMax = 6.3;
  static const double pEMax = TMath::TanH(yMax);

  const int kFirstDaughter=-1;
  const int kLastDaughter=-1;
  
  TParticle* particleDef
    = new TParticle(pdg, is, parent, -1, kFirstDaughter, kLastDaughter,
		     px, py, pz, e, vx, vy, vz, tof);
   
  particleDef->SetPolarisation(polx, poly, polz);
  particleDef->SetWeight(weight);
  particleDef->SetUniqueID(mech);

  StarMCParticle* mother = 0;
  StarMCParticle* particle = 0;
  int ID = GetNtrack();
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
  if (mech == kPPrimary) fNPrimary++;    
  if (toBeDone) fStack.push(particle);  
}			 
//_____________________________________________________________________________
TParticle* StMCStack::PopNextTrack(int& itrack) {
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
void StMCStack::Print(const Option_t *opt) const {
// Print(const Option_t *opt=0)s info for all particles.
// ---
  
  cout << "StMCStack Info  " << endl;
  cout << "Total number of particles:   " <<  GetNtrack() << endl;
  cout << "Number of primary particles: " <<  GetNprimary() << endl;

  for (int i=0; i<GetNtrack(); i++) {
    GetParticle(i)->Print();
    //GetParticle(i)->PrintDaughters();
  }  
}
//_____________________________________________________________________________
void StMCStack::Clear(const char *) 
{
  // Deletes contained particles, resets particles array and stack.
  // ---
  
  // reset fStack should be empty by this time
  assert(fStack.empty());
  fCurrentTrack = -1;
  fNPrimary = 0;
  fParticles->Delete();
}       
//_____________________________________________________________________________
TParticle* StMCStack::GetCurrentTrack() const
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
int  StMCStack::GetCurrentParentTrackNumber() const 
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
StarMCParticle*  StMCStack::GetParticle(int id) const
{
// Returns id-th particle in fParticles.
// ---

  if (id < 0 || id >= fParticles->GetEntriesFast())
    Fatal("GetParticle", "Index out of range"); 
   
  return (StarMCParticle*)fParticles->At(id);
}


