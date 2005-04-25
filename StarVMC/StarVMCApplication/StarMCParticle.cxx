// $Id: StarMCParticle.cxx,v 1.1 2005/04/25 20:44:28 fisyak Exp $

#include "StarMCParticle.h"

#include <TParticle.h>
#include <TObjArray.h>

#include <iostream>

using namespace std;

ClassImp(StarMCParticle)

//_____________________________________________________________________________
StarMCParticle::StarMCParticle(Int_t id, TParticle* particle)
  : fID(id),
    fParticle(particle),
    fMother(),
    fDaughters()    
{
//
}

//_____________________________________________________________________________
StarMCParticle::StarMCParticle(Int_t id, TParticle* particle, StarMCParticle* mother)
  : fID(id),
    fParticle(particle),
    fMother(mother),
    fDaughters()    
{
//
}

//_____________________________________________________________________________
StarMCParticle::StarMCParticle()
  : fID(0),
    fParticle(0),
    fMother(),
    fDaughters()    
{
//   
}

//_____________________________________________________________________________
StarMCParticle::~StarMCParticle() 
{
//
  delete fParticle;
}

//
// public methods
//

//_____________________________________________________________________________
void StarMCParticle::SetMother(StarMCParticle* particle)
{
// Adds particles daughter
// ---

  fMother.SetObject(particle);
}  

//_____________________________________________________________________________
void StarMCParticle::AddDaughter(StarMCParticle* particle)
{
// Adds particles daughter
// ---

  fDaughters.Add(particle);
}  

//_____________________________________________________________________________
void StarMCParticle::Print(const Option_t *opt) const
{
// Print(const Option_t *opt=0)s particle properties.
// ---

  cout << "Track ID:  " << fID << endl;

  fParticle->Print();  
  
  if (GetMother()) {
    cout << "Mother:    " << GetMother()->GetParticle()->GetName() 
                          << "  with ID: " << GetMother()->GetID() << endl;
  }
  else			    
    cout << "Primary    " << endl; 
    
  cout << "Number of daughters: " << GetNofDaughters() << endl;
  cout << endl;			  
}  

//_____________________________________________________________________________
void StarMCParticle::PrintDaughters() const
{
// Print(const Option_t *opt=0)s particles daughters.
// ---

  for (Int_t i=0; i<GetNofDaughters(); i++)  {
    cout << i << "th daughter: " << endl;
    GetDaughter(i)->Print();
  }  
}  

//_____________________________________________________________________________
Int_t  StarMCParticle:: GetID() const
{
// Returs particle ID.
// ---

  return fID;
}  


//_____________________________________________________________________________
TParticle*  StarMCParticle::GetParticle() const
{
// Returns particle definition (TParticle).
// ---

  return fParticle;
}  

//_____________________________________________________________________________
StarMCParticle* StarMCParticle::GetMother() const
{
// Returns particle definition (TParticle).
// ---

  return (StarMCParticle*) fMother.GetObject();
}  

//_____________________________________________________________________________
Int_t StarMCParticle::GetNofDaughters() const
{
// Returns number of daughters.
// ---

  return fDaughters.GetEntriesFast();
}  

//_____________________________________________________________________________
StarMCParticle* StarMCParticle::GetDaughter(Int_t i) const
{
// Returns i-th daughter.
// ---

  if (i < 0 || i >= GetNofDaughters())
    Fatal("GetDaughter", "Index out of range"); 

  return (StarMCParticle*) fDaughters.At(i);
}  

