// $Id: StMCParticle.cxx,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $
//
// Geant4 novice ExampleN01 adapted to Virtual Monte Carlo 
//
// Class StMCParticle
// ------------------
// Extended TParticle with pointers to mother and daughters
// particles
//
// by Ivana Hrivnacova, 5.4.2002

#include "StMCParticle.h"

#include <TParticle.h>
#include <TObjArray.h>

#include "Riostream.h"

using namespace std;

ClassImp(StMCParticle)

//_____________________________________________________________________________
StMCParticle::StMCParticle(Int_t id, TParticle* particle)
  : fID(id),
    fParticle(particle),
    fMother(),
    fDaughters()    
{
//
}

//_____________________________________________________________________________
StMCParticle::StMCParticle(Int_t id, TParticle* particle, StMCParticle* mother)
  : fID(id),
    fParticle(particle),
    fMother(mother),
    fDaughters()    
{
//
}

//_____________________________________________________________________________
StMCParticle::StMCParticle()
  : fID(0),
    fParticle(0),
    fMother(),
    fDaughters()    
{
//   
}

//_____________________________________________________________________________
StMCParticle::~StMCParticle() 
{
//
  delete fParticle;
}

//
// public methods
//

//_____________________________________________________________________________
void StMCParticle::SetMother(StMCParticle* particle)
{
// Adds particles daughter
// ---

  fMother.SetObject(particle);
}  

//_____________________________________________________________________________
void StMCParticle::AddDaughter(StMCParticle* particle)
{
// Adds particles daughter
// ---

  fDaughters.Add(particle);
}  

//_____________________________________________________________________________
void StMCParticle::Print(const Option_t*) const
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
void StMCParticle::PrintDaughters() const
{
// Prints particles daughters.
// ---

  for (Int_t i=0; i<GetNofDaughters(); i++)  {
    cout << i << "th daughter: " << endl;
    GetDaughter(i)->Print();
  }  
}  

//_____________________________________________________________________________
Int_t  StMCParticle:: GetID() const
{
// Returs particle ID.
// ---

  return fID;
}  


//_____________________________________________________________________________
TParticle*  StMCParticle::GetParticle() const
{
// Returns particle definition (TParticle).
// ---

  return fParticle;
}  

//_____________________________________________________________________________
StMCParticle* StMCParticle::GetMother() const
{
// Returns particle definition (TParticle).
// ---

  return (StMCParticle*) fMother.GetObject();
}  

//_____________________________________________________________________________
Int_t StMCParticle::GetNofDaughters() const
{
// Returns number of daughters.
// ---

  return fDaughters.GetEntriesFast();
}  

//_____________________________________________________________________________
StMCParticle* StMCParticle::GetDaughter(Int_t i) const
{
// Returns i-th daughter.
// ---

  if (i < 0 || i >= GetNofDaughters())
    Fatal("GetDaughter", "Index out of range"); 

  return (StMCParticle*) fDaughters.At(i);
}  

