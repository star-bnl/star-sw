// $Id: StarParticle.cxx,v 1.2 2004/07/16 22:52:35 potekhin Exp $
//

#include "StarParticle.h"

#include <TParticle.h>
#include <TObjArray.h>

#include <iostream>

using namespace std;

ClassImp(StarParticle)

//_____________________________________________________________________________
StarParticle::StarParticle(Int_t id, TParticle* particle)
  : _ID(id),
    _particle(particle),
    _mother(),
    _daughters(),
    _keep(0)
{
//
}

//_____________________________________________________________________________
StarParticle::StarParticle(Int_t id, TParticle* particle, StarParticle* mother)
  : _ID(id),
    _particle(particle),
    _mother(mother),
    _daughters(),
    _keep(0)
{
//
}

//_____________________________________________________________________________
StarParticle::StarParticle()
  : _ID(0),
    _particle(0),
    _mother(),
    _daughters(),
    _keep(0)    
{
//   
}

//_____________________________________________________________________________
StarParticle::~StarParticle() 
{
//
  delete _particle;
}

//
// public methods
//

//_____________________________________________________________________________
void StarParticle::SetMother(StarParticle* particle)
{
// Adds particles daughter
// ---

  _mother.SetObject(particle);
}  

//_____________________________________________________________________________
void StarParticle::AddDaughter(StarParticle* particle)
{
// Adds particles daughter
// ---

  _daughters.Add(particle);
}  

//_____________________________________________________________________________
void StarParticle::Print() const
{
// Prints particle properties.
// ---

/*  cout << "Track ID:  " << _ID << endl;

  _particle->Print();  
  
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

  return _ID;
}  


//_____________________________________________________________________________
TParticle*  StarParticle::GetParticle() const
{
// Returns particle definition (TParticle).
// ---

  return _particle;
}  

//_____________________________________________________________________________
StarParticle* StarParticle::GetMother() const
{
// Returns particle definition (TParticle).
// ---

  return (StarParticle*) _mother.GetObject();
}  

//_____________________________________________________________________________
Int_t StarParticle::GetNofDaughters() const
{
// Returns number of daughters.
// ---

  return _daughters.GetEntriesFast();
}  

//_____________________________________________________________________________
StarParticle* StarParticle::GetDaughter(Int_t i) const
{
// Returns i-th daughter.
// ---

  if (i < 0 || i >= GetNofDaughters())
    Fatal("GetDaughter", "Index out of range"); 

  return (StarParticle*) _daughters.At(i);
}  

