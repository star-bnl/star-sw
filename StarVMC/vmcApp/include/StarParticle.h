// $Id: StarParticle.h,v 1.2 2004/07/16 22:52:53 potekhin Exp $

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
    Int_t         GetID()              const;
    TParticle*    GetParticle()        const;
    StarParticle* GetMother()          const;
    Int_t         GetNofDaughters()    const;
    StarParticle* GetDaughter(Int_t i) const;
    
  private:

    Int_t       _ID;
    TParticle*  _particle;
    TRef        _mother;
    TRefArray   _daughters;
    Bool_t      _keep;

    
    ClassDef(StarParticle,1) // Extended TParticle
};

#endif //STARPARTICLE_H   
   

