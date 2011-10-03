// $Id: StMCStack.h,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $
//
// Geant4 ExampleN02 adapted to Virtual Monte Carlo 
//
// Class StMCStack
// -----------------
// Implementation of the TVirtualMCStack interface
//
// by Ivana Hrivnacova, 5.4.2002


#ifndef STMC_STACK_H
#define STMC_STACK_H

#include "StMCParticle.h"

#include <TVirtualMCStack.h>

#include <stack>

class StMCStack : public TVirtualMCStack
{
  public:
    StMCStack(Int_t size);
    StMCStack();
    virtual ~StMCStack();     

    // methods
    virtual void  PushTrack(Int_t toBeDone, Int_t parent, Int_t pdg,
  	              Double_t px, Double_t py, Double_t pz, Double_t e,
  		      Double_t vx, Double_t vy, Double_t vz, Double_t tof,
		      Double_t polx, Double_t poly, Double_t polz,
		      TMCProcess mech, Int_t& ntr, Double_t weight,
		      Int_t is) ;
    virtual TParticle* PopNextTrack(Int_t& track);
    virtual TParticle* PopPrimaryForTracking(Int_t i); 
    void Print(const Option_t* opt=0) const;   
    void Clear(const Option_t* opt=0);   
   
    // set methods
    virtual void  SetCurrentTrack(Int_t track);                           

    // get methods
    virtual Int_t  GetNtrack() const;
    virtual Int_t  GetNprimary() const;
    virtual TParticle* GetCurrentTrack() const;    
    virtual Int_t  GetCurrentTrackNumber() const;
    virtual Int_t  GetCurrentParentTrackNumber() const;
    StMCParticle*  GetParticle(Int_t id) const;
    
  private:
    // data members
    std::stack<StMCParticle*>  fStack;    //!
    TObjArray*                 fParticles;
    Int_t                      fCurrentTrack;
    Int_t                      fNPrimary;
    
    ClassDef(StMCStack,1) // StMCStack
};

#endif //STMC_STACK_H   
   

