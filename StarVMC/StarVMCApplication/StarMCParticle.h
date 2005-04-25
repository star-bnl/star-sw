// $Id: StarMCParticle.h,v 1.1 2005/04/25 20:44:28 fisyak Exp $

#ifndef Star_PARTICLE_H
#define Star_PARTICLE_H

#include <TObject.h>
#include <TRef.h>
#include <TRefArray.h>

class TParticle;

class StarMCParticle : public TObject
{
  public:
    StarMCParticle(Int_t id, TParticle* particle);
    StarMCParticle(Int_t id, TParticle* particle, StarMCParticle* mother);
    StarMCParticle();
    virtual ~StarMCParticle();     

    // methods
    void SetMother(StarMCParticle* particle);
    void AddDaughter(StarMCParticle* particle);
    void Print(const Option_t *opt=0) const;
    void PrintDaughters() const;

    // get methods  
    Int_t         GetID() const;
    TParticle*    GetParticle() const;
    StarMCParticle* GetMother() const;
    Int_t         GetNofDaughters() const;
    StarMCParticle* GetDaughter(Int_t i) const;
    
  private:
    // data members
    Int_t       fID;
    TParticle*  fParticle;
    TRef        fMother;
    TRefArray   fDaughters;
    
    ClassDef(StarMCParticle,1) // Extended TParticle
};

#endif //Star_PARTICLE_H   
   

