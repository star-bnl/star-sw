// $Id: StarStack.cxx,v 1.3 2004/09/02 23:26:18 potekhin Exp $
// $Log: StarStack.cxx,v $
// Revision 1.3  2004/09/02 23:26:18  potekhin
// evolution
//


// Warning this material comes from Ex02 and is not adapted from Ali

#include "StarStack.h"
#include "StarVertex.h"

#include <TVirtualMC.h>
#include <TParticle.h>
#include <TObjArray.h>
#include <TError.h>

#include <iostream>

using namespace std;

Bool_t close(Float_t x1, Float_t x2, Float_t y1, Float_t y2, Float_t z1,Float_t z2);


ClassImp(StarStack)


static StarParticle* origin = new StarParticle(-1,0);

//_____________________________________________________________________________
StarStack::StarStack(Int_t size) :
  _stack(0), _kine(0), _currentTrack(-1),
 _Nprimary(0),  _Ntrack(0), _debug(0) {

    _stack = new TObjArray(size);
    _kine  = new TObjArray(size);
    Init();
}

//_____________________________________________________________________________
StarStack::StarStack() :
  _stack(0),  _kine(0), _currentTrack(-1),
 _Nprimary(0),  _Ntrack(0), _debug(0) {

    Init();
}

//_____________________________________________________________________________
StarStack::~StarStack() {

  cout<<"Stack dtor"<<endl;
  if (_stack) _stack->Delete();
  delete _stack;
}
//_____________________________________________________________________________
void StarStack::Init(void) {
  for(int i=0;i<kMaxMCProcess;i++) {
    _interest[i]=0;
  }
}
//_____________________________________________________________________________
void StarStack::SetInterest(const char* processName_) {
  for(int i=0;i<kMaxMCProcess;i++) {
    if(strcmp(processName_,TMCProcessName[i])==0) {
      cout<<processName_<<" flagged as process of interest"<<endl;
      _interest[i]=1;
    }
  }
}
//_____________________________________________________________________________
void  StarStack::PushTrack(Int_t toBeDone,  Int_t parent,  Int_t pdg,
			   Double_t px,     Double_t py,   Double_t pz, Double_t e,
			   Double_t vx,     Double_t vy,   Double_t vz, Double_t tof,
			   Double_t polx,   Double_t poly, Double_t polz,
			   TMCProcess mech, Int_t& ntr,    Double_t weight, Int_t is) {


  // This is to be investigated:
  const Int_t kFirstDaughter = -1;
  const Int_t kLastDaughter  = -1;
  

  TParticle* particleDef = new TParticle(pdg, is, parent, -1,
					 kFirstDaughter, kLastDaughter,
					 px, py, pz, e, vx, vy, vz, tof);
   
                                         particleDef->SetPolarisation(polx, poly, polz);
                                         particleDef->SetWeight      (weight);
                                         particleDef->SetUniqueID    (mech);


  Int_t number = GetNkine(); // we number particles as per kine

  // The new particle that goes on stack:
  StarParticle* particle = new StarParticle(number, particleDef, 0);

  if(_interest[mech] || parent<0) {
    particle->SetKeep();
    particle->SetMotherID(parent);
    AddToKine(particle);

    TLorentzVector pos;                        // Position of the new particle being created
    gMC->TrackPosition(pos);
    Float_t x=pos.X(), y=pos.Y(), z=pos.Z();   //    cout<<"x "<<x<<" y "<<y<<" z "<<z<<endl;

    StarParticle* mother = (parent<0)?origin:GetKine(parent);
    TObjArray* vertices = mother->GetVertices();

    if(!vertices) { // mother needs to have the vertices initialized
      vertices=mother->InitVertices();
      StarVertex* newVert = new StarVertex(mother, x, y, z);
      StarVertex::AddNewVertex(vertices,newVert);
    }

    // Now try and find the vertex
    TIterator*   iv  = vertices->MakeIterator();
    StarVertex*   v  = (StarVertex*) iv->Next();

    Bool_t found=0;

    while(v) {
      if(close(v->x(), x, v->y(), y, v->z(), z)) {//cout<<"Found vertex "<<v->GetNumber()<<endl;
	found=1;
	break;
      }
      v  = (StarVertex*) iv->Next();
    }


    if(found) { // simply add the current particle to the vertex
      v->AddParticle(particle);
    }
    else {      //      cout<<"New vertex"<<endl;
      StarVertex* newVert = new StarVertex(mother, x, y, z);
      StarVertex::AddNewVertex(vertices,newVert);
      newVert->AddParticle(particle);
    }


  } // mother








  if (toBeDone) {
    push(particle);
    if(_debug>=3) {
      TString name = ((TNamed*) particleDef)->GetName();
      cout<<"particle "<<name<<" toBeDone "<<toBeDone<<" number "<<number
	  <<" parent " <<parent
	  <<" mech "   <<TMCProcessName[mech]
	  <<" tracks " <<_Ntrack<<endl;
    } // debug
  }   // toBeDone

  cout<<"N: "<<_stack->GetEntries()<<endl;
}

//_____________________________________________________________________________
TParticle* StarStack::PopNextTrack(Int_t& itrack) {
// Gets next particle for tracking from the stack.

  itrack = -1;
  if  (empty()) return 0;
		      
  StarParticle* particle =   pop();

  if (!particle) return 0;  
  
  itrack = particle->GetID();  // returns the number we previously assigned in Push
  _currentTrack = itrack;

  //  cout<<"popping "<<particle->GetParticle()->GetName()<<endl;

  return particle->GetParticle();
}    

//_____________________________________________________________________________
TParticle* StarStack::PopPrimaryForTracking(Int_t i) {
// Returns i-th particle in _stack.

  cout<<"Pop primary"<<endl;
  if (i < 0 || i >= _Nprimary)
    Fatal("GetPrimaryForTracking", "Index out of range"); 
  
  return ((StarParticle*)_stack->At(i))->GetParticle();
}     


//_____________________________________________________________________________
void  StarStack::PurifyKine(void)     {
  // Compress kinematic tree, keeping only flagged particles
  // and renaming the particle id's in all the hits


}

//_____________________________________________________________________________
void StarStack::Print(void) const     {
// Prints info for all particles.

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
void StarStack::PrintKine(void) const {
// Prints info for all particles.
}
//_____________________________________________________________________________
void StarStack::Reset()               { // Deletes contained particles, resets particles array and stack.

  //  cout<<"Stack Reset"<<endl;

  delete origin;
  origin = new StarParticle(-1,0);

  //  cout<<"origin deleted"<<endl;
  _Ntrack=0;
  _currentTrack = -1;
  _Nprimary = 0;
  _stack->Delete();

  //  cout<<"stack deleted"<<endl;
  _kine->Delete();
  //  cout<<"kine deleted"<<endl;

  StarVertex::Reset();
  //  cout<<"vertes reset"<<endl;


  //  cout<<"Number left in stack: "<<GetNtrack()<<endl;
}       

//_____________________________________________________________________________
void  StarStack::SetCurrentTrack(Int_t track)         {  // Sets the current track to a given value.

  cout<<"Set TRACK!"<<endl;
  _currentTrack = track;
}     
//_____________________________________________________________________________
Int_t  StarStack::GetNtrack() const                   {  // Returns the number of all tracks.
  return _stack->GetEntriesFast();
}  
//_____________________________________________________________________________
Int_t  StarStack::GetNkine(void) const                {  // Returns the number of kine tracks.
  return _kine->GetEntriesFast();
}  
//_____________________________________________________________________________
Int_t  StarStack::GetNprimary() const                 {  // Returns the number of primary tracks.
  return _Nprimary;
}  
//_____________________________________________________________________________
TParticle* StarStack::GetCurrentTrack() const         {  // Gets the current track particle.
  cout<<"GetCurrentTrack"<<endl;
  StarParticle* c = GetParticle(_currentTrack);
  return c ? c->GetParticle() : 0;
}
//_____________________________________________________________________________
Int_t  StarStack::GetCurrentTrackNumber() const       {  // Returns the current track ID.
  //  cout<<"GetCurrentTrackNumber"<<endl;
  return _currentTrack;
}  
//_____________________________________________________________________________
Int_t  StarStack::GetCurrentParentTrackNumber() const {  // Returns the current track parent ID.

  StarParticle* current = GetParticle(_currentTrack);
  if (!current) return -1; 
  StarParticle* mother = current->GetMother();
  if (!mother) return -1;
  return  mother->GetID();
}  

//_____________________________________________________________________________
StarParticle*  StarStack::GetParticle(Int_t id) const {
// Returns id-th particle in _stack.
// ---

  cout<<"GetParticle "<<id<<endl;
  if (id < 0 || id > _stack->GetEntriesFast())  {
    //    cout<<"id: "<<id<<endl;
    Fatal("GetParticle", "Index out of range"); 
  }
  return (StarParticle*) _stack->At(id);
}
//_____________________________________________________________________________
StarParticle*  StarStack::GetKine(Int_t id) const {
// Returns id-th particle in _stack.
// ---

  if (id < 0 || id > _kine->GetEntriesFast())  {
    //    cout<<"id: "<<id<<endl;
    Fatal("GetKine", "Index out of range"); 
  }
  return (StarParticle*) _kine->At(id);
}
//____________________________________________________________
void StarStack::push(StarParticle* p_)                {
  _stack->Add(p_);

  //  cout<<"Npart is now "<<_stack->GetEntries()<<endl;
}

//_____________________________________________________________________________
StarParticle* StarStack::pop(void)                    {
  return (StarParticle*) _stack->RemoveAt(_stack->GetLast());
}
//_____________________________________________________________________________
Bool_t StarStack::empty(void)                         {
  return _stack->IsEmpty();
}

//_____________________________________________________________________________
Bool_t close(Float_t x1, Float_t x2,
	     Float_t y1, Float_t y2,
	     Float_t z1, Float_t z2)                  {

  return (fabsf(x1-x2)<1e-3&&fabsf(y1-y2)<1e-3&&fabsf(z1-z2)<1e-3);
}
