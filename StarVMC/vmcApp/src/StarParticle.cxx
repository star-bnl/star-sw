// $Id: StarParticle.cxx,v 1.1 2004/07/12 20:36:39 potekhin Exp $
//

#include "StarParticle.h"

#include <TParticle.h>
#include <TObjArray.h>

#include <iostream>

using namespace std;

ClassImp(StarParticle)

//_____________________________________________________________________________
StarParticle::StarParticle(Int_t id, TParticle* particle)
  : fID(id),
    fParticle(particle),
    fMother(),
    fDaughters()    
{
//
}

//_____________________________________________________________________________
StarParticle::StarParticle(Int_t id, TParticle* particle, StarParticle* mother)
  : fID(id),
    fParticle(particle),
    fMother(mother),
    fDaughters()    
{
//
}

//_____________________________________________________________________________
StarParticle::StarParticle()
  : fID(0),
    fParticle(0),
    fMother(),
    fDaughters()    
{
//   
}

//_____________________________________________________________________________
StarParticle::~StarParticle() 
{
//
  delete fParticle;
}

//
// public methods
//

//_____________________________________________________________________________
void StarParticle::SetMother(StarParticle* particle)
{
// Adds particles daughter
// ---

  fMother.SetObject(particle);
}  

//_____________________________________________________________________________
void StarParticle::AddDaughter(StarParticle* particle)
{
// Adds particles daughter
// ---

  fDaughters.Add(particle);
}  

//_____________________________________________________________________________
void StarParticle::Print() const
{
// Prints particle properties.
// ---

/*  cout << "Track ID:  " << fID << endl;

  fParticle->Print();  
  
  if (GetMother()) {
    cout << "Mother:    " << GetMother()->GetParticle()->GetName() 
                          << "  with ID: " << GetMother()->GetID() << endl;
  }
  else			    
    cout << "Primary    " << endl; 
    
  cout << "Number of daughters: " << GetNofDaughters() << endl;
  cout << endl;
 */
}  

//_____________________________________________________________________________
void StarParticle::PrintDaughters() const
{
// Prints particles daughters.
// ---

  for (Int_t i=0; i<GetNofDaughters(); i++)  {
    cout << i << "th daughter: " << endl;
    GetDaughter(i)->Print();
  }  
}  

//_____________________________________________________________________________
Int_t  StarParticle:: GetID() const
{
// Returs particle ID.
// ---

  return fID;
}  


//_____________________________________________________________________________
TParticle*  StarParticle::GetParticle() const
{
// Returns particle definition (TParticle).
// ---

  return fParticle;
}  

//_____________________________________________________________________________
StarParticle* StarParticle::GetMother() const
{
// Returns particle definition (TParticle).
// ---

  return (StarParticle*) fMother.GetObject();
}  

//_____________________________________________________________________________
Int_t StarParticle::GetNofDaughters() const
{
// Returns number of daughters.
// ---

  return fDaughters.GetEntriesFast();
}  

//_____________________________________________________________________________
StarParticle* StarParticle::GetDaughter(Int_t i) const
{
// Returns i-th daughter.
// ---

  if (i < 0 || i >= GetNofDaughters())
    Fatal("GetDaughter", "Index out of range"); 

  return (StarParticle*) fDaughters.At(i);
}  

