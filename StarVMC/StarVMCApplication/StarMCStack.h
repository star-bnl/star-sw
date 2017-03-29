// $Id: StarMCStack.h,v 1.2 2011/02/11 16:12:52 fisyak Exp $
// $Log: StarMCStack.h,v $
// Revision 1.2  2011/02/11 16:12:52  fisyak
// Fixes for gcc451
//
// Revision 1.1.1.1  2008/12/10 20:45:53  fisyak
// Merge with macos version
//
// Revision 1.3  2005/06/09 20:13:47  fisyak
// It looks like that all hits in place (calorimeters have to be check for volumeid)
//
// Revision 1.2  2005/05/03 15:42:14  fisyak
// Adjust for bfc
//
// Revision 1.1  2005/04/25 20:44:28  fisyak
// StarVMCApplication with example in macros/starVMC.C
//

#ifndef StarMC_STACK_H
#define StarMC_STACK_H
#include <assert.h>
#include "TParticle.h"
#include "TObjArray.h"
#include "TError.h"
#include "TVirtualMCStack.h"
#include "StarMCParticle.h"
#include <stack>

class StarMCStack : public TVirtualMCStack {
 public:
  StarMCStack(Int_t size = 0) : fParticles(0), fCurrentTrack(-1), fNPrimary(0), fCurrentParticle(0) {
    if (size > 0) fParticles = new TObjArray(size, 0 /*1*/);}
  virtual ~StarMCStack() {if (fParticles) fParticles->Delete(); delete fParticles;}

  // methods
  virtual void  PushTrack(Int_t toBeDone, Int_t parent, Int_t pdg,
			  Double_t px, Double_t py, Double_t pz, Double_t e,
			  Double_t vx, Double_t vy, Double_t vz, Double_t tof,
			  Double_t polx, Double_t poly, Double_t polz,
			  TMCProcess mech, Int_t& ntr, Double_t weight,
			  Int_t is) ;
  virtual TParticle* PopNextTrack(Int_t& track);
  virtual TParticle* PopPrimaryForTracking(Int_t i) {return ((StarMCParticle *) fParticles->At(i))->GetParticle();}
  void Print(const Option_t *opt=0) const;   
  void Reset();   
   
  virtual void       SetCurrentTrack(Int_t track)  {fCurrentTrack = track;}
  virtual void       SetNprimary(Int_t nprim)      {fNPrimary = nprim;}
  virtual Int_t      GetNtrack() const             {return fParticles->GetEntriesFast();}
  virtual Int_t      GetNprimary() const           {return fNPrimary;}
  virtual TParticle* GetCurrentTrack()   const;
  virtual Int_t      GetCurrentTrackNumber() const {return fCurrentTrack;}
  virtual Int_t      GetCurrentTrackId() const     {return GetCurrentTrackNumber()+1;}
  virtual Int_t      GetCurrentParentTrackNumber() const;
  StarMCParticle*    GetParticle(Int_t id) const;
  TObjArray*         GetParticles() const          {return fParticles;}
  StarMCParticle*    GetCurrentParticle()          {return fCurrentParticle;}
  private:
    // data members
  std::stack<StarMCParticle*>  fStack;    //!
  TObjArray*                 fParticles;
  Int_t                      fCurrentTrack;
  Int_t                      fNPrimary;
  StarMCParticle*            fCurrentParticle;
  ClassDef(StarMCStack,1) // StarMCStack
};

#endif
   

