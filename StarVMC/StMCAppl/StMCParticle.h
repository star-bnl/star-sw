// $Id: StMCParticle.h,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $
//
// Geant4 novice ExampleN02 adapted to Virtual Monte Carlo 
//
// Class StMCParticle
// ------------------
// Extended TParticle with persistent pointers to mother and daughters
// particles
//
// by Ivana Hrivnacova, 5.4.2002


#ifndef STMC_PARTICLE_H
#define STMC_PARTICLE_H

#include <TObject.h>
#include <TRef.h>
#include <TRefArray.h>

class TParticle;

class StMCParticle : public TObject
{
  public:
    StMCParticle(Int_t id, TParticle* particle);
    StMCParticle(Int_t id, TParticle* particle, StMCParticle* mother);
    StMCParticle();
    virtual ~StMCParticle();     

    // methods
    void SetMother(StMCParticle* particle);
    void AddDaughter(StMCParticle* particle);
    void Print(const Option_t* opt=0) const;
    void PrintDaughters() const;

    // get methods  
    Int_t         GetID() const;
    TParticle*    GetParticle() const;
    StMCParticle* GetMother() const;
    Int_t         GetNofDaughters() const;
    StMCParticle* GetDaughter(Int_t i) const;
    
  private:
    // data members
    Int_t       fID;
    TParticle*  fParticle;
    TRef        fMother;
    TRefArray   fDaughters;
    
    ClassDef(StMCParticle,1) // Extended TParticle
};

#endif //STMC_PARTICLE_H   
   

