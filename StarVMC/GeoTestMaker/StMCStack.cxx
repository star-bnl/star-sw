// $Id: StMCStack.cxx,v 1.4 2010/07/29 03:14:40 perev Exp $
#include <iostream>
#include <assert.h>
#include "StMCStack.h"
#include <TParticle.h>
#include <TObjArray.h>
#include <TError.h>

class myTParticle : public TParticle {
 public:
myTParticle(Int_t pdg, Int_t  status, Int_t  mother1, Int_t  mother2
           ,Int_t  daughter1, Int_t daughter2
	   ,Double_t  px, Double_t  py, Double_t  pz
	   ,Double_t  etot, Double_t  vx, Double_t  vy, Double_t  vz
	   ,Double_t  time) 
 :TParticle(pdg,status,mother1,mother2,daughter1, daughter2
           ,px,py,pz,etot,vx,vy,vz,time)
  { fId=0;}

~myTParticle();

  int  GetId() const {return fId;}
  void SetId(int id) {fId = id  ;}
  void SetMother(myTParticle* particle)     { fMother=particle;}
  void AddDaughter(myTParticle* particle)   { fDaughters.push_back(particle);}
//void Print(const Option_t *opt=0) const;
  myTParticle*   GetMother() const          { return fMother;}
  Int_t          GetNDaughters() const      { return fDaughters.size();}
  myTParticle*   GetDaughter(Int_t i) const;
  
  private:
  // data members
  int          fId;
  myTParticle *fMother;
  std::vector<myTParticle*> fDaughters;
};

//_____________________________________________________________________________
myTParticle* myTParticle::GetDaughter(Int_t i) const 
{
  if (i < 0 || i >= (int)fDaughters.size())
    Fatal("GetDaughter", "Index out of range"); 
  return fDaughters[i];
}  

//_____________________________________________________________________________
myTParticle::~myTParticle()  
{
  int n = fDaughters.size();
  for (int i=0;i<n;i++) { delete fDaughters[i];}
  fDaughters.clear();
}  

ClassImp(StMCStack)
//_____________________________________________________________________________
StMCStack::StMCStack(int size) : 
  fParticles(size),
  fCurrentTrack(-1),
  fNPrimary(0),
  fCurrentParticle(0)
{
}
//_____________________________________________________________________________
 StMCStack::~StMCStack() 
{
 for( int i=0; i<(int)fParticles.size();i++) {delete fParticles[i];}
}
//_____________________________________________________________________________
TParticle* StMCStack::PopPrimaryForTracking(int i)	
{
return fParticles[i];
}

//_____________________________________________________________________________
void  StMCStack::PushTrack(int toBeDone, int parent, int pdg,
			     double px, double py, double pz, double e,
			     double vx, double vy, double vz, double tof,
			     double polx, double poly, double polz,
			     TMCProcess mech, int& ntr, double weight,
			     int is)
{
// Creates a new particle with specified properties,
// adds it to the particles array (fParticles) and if not done to the 
// stack (fStack).
// ---

  const int kFirstDaughter=-1;
  const int kLastDaughter=-1;
  
  myTParticle* particle
    = new myTParticle(pdg, is, parent, -1, kFirstDaughter, kLastDaughter,
		     px, py, pz, e, vx, vy, vz, tof);
   
  particle->SetPolarisation(polx, poly, polz);
  particle->SetWeight(weight);
  particle->SetUniqueID(mech);

  myTParticle* mother = 0;
  int ID = GetNtrack();
  if (parent>=0 && toBeDone && mech != kPPrimary) {
    mother = (myTParticle*)GetParticle(parent);
    ID = mother->GetId();
    particle->SetId(ID);  particle->SetMother(mother);
    mother->AddDaughter(particle);
  }
  else {
    particle->SetId(ID);
    fParticles.push_back(particle);
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
  
  itrack = fCurrentParticle->GetId();
  fCurrentTrack = itrack;

  return fCurrentParticle;
}    
//_____________________________________________________________________________
void StMCStack::Print(const Option_t *opt) const {
// Print(const Option_t *opt=0)s info for all particles.
// ---
  
  printf("*** StMCStack Info ***\n");
  printf("Total number of particles: %d\n",GetNtrack());
  printf("Number of primary particles: %d\n",GetNprimary());
  for (int i=0; i<GetNtrack(); i++) {
    GetParticle(i)->Print();
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
  for( int i=0; i<(int)fParticles.size();i++) {delete fParticles[i];}
  fParticles.clear();
}       
//_____________________________________________________________________________
TParticle* StMCStack::GetCurrentTrack() const
{
// Gets the current track particle.
// ---
  return GetParticle(fCurrentTrack);
}

//_____________________________________________________________________________
int  StMCStack::GetCurrentParentTrackNumber() const 
{
// Returns the current track parent ID.
// ---

  myTParticle* current = GetParticle(fCurrentTrack);
  
  if (!current) return -1; 
  
  myTParticle* mother = current->GetMother();
  
  if (!mother) return -1;
    
  return  mother->GetId();
}  

//_____________________________________________________________________________
myTParticle*  StMCStack::GetParticle(int id) const
{
// 		Returns id-th particle in fParticles.
  if (id < 0 || id >= (int)fParticles.size())
    Fatal("GetParticle", "Index out of range"); 
  return fParticles[id];
}


