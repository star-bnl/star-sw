// $Id: StarMCParticle.h,v 1.1.1.1 2008/12/10 20:45:53 fisyak Exp $
#ifndef Star_PARTICLE_H
#define Star_PARTICLE_H
#include "TObject.h"
#include "TRef.h"
#include "TRefArray.h"
#include "TParticle.h"

class StarMCParticle : public TObject {
 public:
  StarMCParticle(Int_t id=0, TParticle* particle=0) : 
  fID(id), fIdGen(0), fParticle(particle), fMother(), fDaughters() {}
  StarMCParticle(Int_t id, TParticle* particle, StarMCParticle* mother, Int_t IdGen = 0) : 
  fID(id), fIdGen(IdGen), fParticle(particle), fMother(mother), fDaughters() {}
  virtual ~StarMCParticle() {delete fParticle; fParticle = 0;}

  void SetMother(StarMCParticle* particle)   { fMother.SetObject(particle);}
  void SetIdGen(Int_t m)                     { fIdGen = m;}
  void AddDaughter(StarMCParticle* particle) { fDaughters.Add(particle);}
  void Print(const Option_t *opt=0) const;
  void PrintDaughters() const;
  Int_t           GetID() const              { return fID;}
  Int_t           GetIdGen() const           { return fIdGen;}
  TParticle*      GetParticle() const        { return fParticle;}
  StarMCParticle* GetMother() const          { return (StarMCParticle*) fMother.GetObject();}
  Int_t           GetNofDaughters() const    { return fDaughters.GetEntriesFast();}
  StarMCParticle* GetDaughter(Int_t i) const;
  
  private:
  // data members
  Int_t       fID; // index in TObjArray of fParticles
  Int_t       fIdGen; // index in original generator particle list 
  TParticle*  fParticle;
  TRef        fMother;
  TRefArray   fDaughters;
  
  ClassDef(StarMCParticle,1) // Extended TParticle
};
#endif //Star_PARTICLE_H   
   

