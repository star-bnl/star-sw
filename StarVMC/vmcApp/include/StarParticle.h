// $Id: StarParticle.h,v 1.1 2004/07/12 20:35:59 potekhin Exp $

#ifndef STARPARTICLE_H
#define STARPARTICLE_H

#include <TObject.h>
#include <TRef.h>
#include <TRefArray.h>
#include <TParticle.h>

class StarParticle : public TObject
{
  public:
    StarParticle(Int_t id, TParticle* particle);
    StarParticle(Int_t id, TParticle* particle, StarParticle* mother);
    StarParticle();
    virtual ~StarParticle();     

    // methods
    void SetMother(StarParticle* particle);
    void AddDaughter(StarParticle* particle);
    void Print() const;
    void PrintDaughters() const;

    // get methods  
    Int_t         GetID() const;
    TParticle*    GetParticle() const;
    StarParticle* GetMother() const;
    Int_t         GetNofDaughters() const;
    StarParticle* GetDaughter(Int_t i) const;
    
  private:
    // data members
    Int_t       fID;
    TParticle*  fParticle;
    TRef        fMother;
    TRefArray   fDaughters;
    
    ClassDef(StarParticle,1) // Extended TParticle
};

#endif //STARPARTICLE_H   
   

