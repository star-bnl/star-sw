#ifndef STAR_STACK_H
#define STAR_STACK_H
/* Copyright(c) 1998-1999, ALICE Experiment at CERN, All rights reserved. *
 * See cxx source for full Copyright notice                               */

/* $Id$ */

//  Particles stack class
//  Implements the TMCVirtualStack of the Virtual Monte Carlo
//  Author A.Morsch

#include "TClonesArray.h"
#include "TArrayI.h"
#include "TVirtualMCStack.h"
#include "TString.h"
#include "TParticle.h"
#include <stack>
enum {kKeepBit=1, kDaughtersBit=2, kDoneBit=4, kTransportBit=BIT(14)};

class StarStack : public TVirtualMCStack
{
  public:
    // creators, destructors
    StarStack(Int_t size, const char* name = "");
    StarStack();
    StarStack(const StarStack& st);
    virtual ~StarStack();
    StarStack& operator=(const StarStack& st)
      {st.Copy(*this); return(*this);}

    // methods

    virtual void  PushTrack(Int_t done, Int_t parent, Int_t pdg, 
                           const Float_t *pmom, const Float_t *vpos, const Float_t *polar, 
                           Float_t tof, TMCProcess mech, Int_t &ntr,
                           Float_t weight, Int_t is);

    virtual void  PushTrack(Int_t done, Int_t parent, Int_t pdg,
                           Double_t px, Double_t py, Double_t pz, Double_t e,
                           Double_t vx, Double_t vy, Double_t vz, Double_t tof,
                           Double_t polx, Double_t poly, Double_t polz,
                           TMCProcess mech, Int_t &ntr, Double_t weight,
                           Int_t is);

    virtual TParticle* PopNextTrack(Int_t& track);
    virtual TParticle* PopPrimaryForTracking(Int_t i);    
    virtual TParticle* GetCurrentTrack() const {return fCurrentTrack;}
    virtual void  SetNprimaries(Int_t n) { fNprimary = n;}
    virtual void  SetCurrentTrack(Int_t Id) {fCurrentTrack = Particle(Id); fCurrentID = Id;}
    static  void  SetDebug(Int_t m) {fgDebug = m;}
    virtual Int_t GetNtrack() const {return fParticles.GetEntriesFast();}
    virtual Int_t GetCurrentTrackNumber() const {return fCurrentID; }
    virtual Int_t GetNprimary() const {return fNprimary;}
    virtual Int_t GetCurrentParentTrackNumber() const {return 0;}
    TParticle*    GetNextParticle();
    TParticle*    Particle(Int_t id) {return (TParticle* ) fParticles[id];}
    virtual void  Print(Option_t *option="") const;
    Bool_t        IsStable(Int_t pdg) const;
    Bool_t        IsPhysicalPrimary(Int_t i);
    Bool_t        IsSecondaryFromWeakDecay(Int_t index);
    Bool_t        IsSecondaryFromMaterial (Int_t index);
    const TClonesArray*  Particles() const {return &fParticles;}
    void  Clean(Int_t size = 0);    
    void  Reset(Int_t size = 0);
    Int_t Debug() {return fgDebug;}
  protected:
  private:
    void Copy(TObject &st) const;
    // data members
    TClonesArray   fParticles;          //! Pointer to list of particles
    TParticle     *fCurrentTrack;       //! Pointer to particle currently transported
    Int_t          fNtrack;             //  Number of tracks
    Int_t          fNprimary;           //  Number of primaries
    Int_t          fTrackNo;            //! Last track returned from the stack
    Int_t          fCurrentID;          //! Index of track in fParticles
    static Int_t   fgDebug;             //!
    std::stack<TParticle>   fStack;//!
    ClassDef(StarStack,1) //Particles stack
};
#endif //STAR_STACK_H
