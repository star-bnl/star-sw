// $Id: StarParticle.cxx,v 1.3 2004/09/02 23:26:43 potekhin Exp $
// $Log: StarParticle.cxx,v $
// Revision 1.3  2004/09/02 23:26:43  potekhin
// evolution
//


#include "StarParticle.h"
#include "StarVertex.h"

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
    _vertex(0),
    _daughters(),
    _vertices (0),
    _keep(0)
{
//
}

//_____________________________________________________________________________
StarParticle::StarParticle(Int_t id, TParticle* particle_, StarParticle* mother_, StarVertex* vertex_)
  : _ID(id),
    _particle (particle_),
    _mother   (mother_),
    _vertex   (vertex_),
    _daughters(),
    _vertices (0),
    _keep     (0)
{
//
}

//_____________________________________________________________________________
StarParticle::StarParticle()
  : _ID(0),
    _particle(0),
    _mother(),
    _vertex(0),
    _daughters(),
    _vertices(0),
    _keep(0)    
{
  cout<<"default particle ctor called"<<endl;
//   
}

//_____________________________________________________________________________
StarParticle::~StarParticle() 
{
//
  cout<<"particle dtor"<<endl;
  cout<<"deleting underlying particle"<<endl;
  delete _particle;

  if(_vertices) {
    _vertices->Clear();
    cout<<"cleared vertices"<<endl;
  }

}

//_____________________________________________________________________________
void StarParticle::SetMother(StarParticle* particle)     { // Adds particles daughter
  _mother.SetObject(particle);
}  

void StarParticle::SetVertex(StarVertex*   vertex_)      {
  _vertex=vertex_;
}
//_____________________________________________________________________________
void StarParticle::AddDaughter(StarParticle* particle)   { // Adds particles daughter
  _daughters.Add(particle);
}  
//_____________________________________________________________________________
void StarParticle::Print()                               { // Prints particle properties.

  StarVertex* vertOrigin = GetVertex();

  int nv=-1;
  if(!vertOrigin) {
    //    cout<<"No vertex found for this particle"<<endl;   //    return;
  }
  else {
    nv=vertOrigin->GetNumber();
  }

  cout << _ID << "  "<<nv;

  TObjArray* vertices=GetVertices();

  if(vertices) {
    TIterator*      it =vertices->MakeIterator();
    StarVertex*      v = (StarVertex*) it->Next();

    while(v) {
      Int_t id=v->GetNumber();
      cout<<" "<<id;
      v=(StarVertex*) it->Next();
    }
  }
  cout<<endl;


  /*
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
void StarParticle::PrintDaughters() const       {   // Prints particles daughters.
  for (Int_t i=0; i<GetNofDaughters(); i++)  {
    cout << i << "th daughter: " << endl;
    GetDaughter(i)->Print();
  }  
}  

//_____________________________________________________________________________
Int_t  StarParticle:: GetID() const             {   // Returs particle ID.
  return _ID;
}  


//_____________________________________________________________________________
TParticle*  StarParticle::GetParticle() const   {   // Returns particle definition (TParticle).
  return _particle;
}  

//_____________________________________________________________________________
StarParticle* StarParticle::GetMother() const   {   // Returns particle definition (TParticle).
  return (StarParticle*) _mother.GetObject();
}  

//_____________________________________________________________________________
Int_t StarParticle::GetNofDaughters() const     {   // Returns number of daughters.
  return _daughters.GetEntriesFast();
}  

//_____________________________________________________________________________
StarParticle* StarParticle::GetDaughter(Int_t i) const { // Returns i-th daughter.
  if (i < 0 || i >= GetNofDaughters())
    Fatal("GetDaughter", "Index out of range"); 

  return (StarParticle*) _daughters.At(i);
}  

