#ifndef STAR_STACK_H
#define STAR_STACK_H
/* Copyright(c) 1998-1999, ALICE Experiment at CERN, All rights reserved. *
 * See cxx source for full Copyright notice                               */

/* $Id$ */

//  Particles stack class
//  Implements the TMCVirtualStack of the Virtual Monte Carlo
//  Author A.Morsch

class TClonesArray;
class TFile;
class TObjArray;
class TString;
class TTree;
#include "TClonesArray.h"
#include "TArrayI.h"
#include "TVirtualMCStack.h"
#include "TParticle.h"
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
    virtual TParticle* GetCurrentTrack() const {return fCurrentTrack;}
    virtual TParticle* PopPrimaryForTracking(Int_t i);    

    void   ConnectTree(TTree* tree);
    Bool_t GetEvent();
    Bool_t PurifyKine();
    Bool_t ReorderKine();
    void   FinishEvent();
    void   FlagTrack(Int_t track);
    void   KeepTrack(Int_t itrack); 
    void   Clean(Int_t size = 0);
    void   Reset(Int_t size = 0);
    void   DumpPart(Int_t i) const;
    void   DumpPStack ();
    void   DumpLoadedStack () const;

    // set methods
    void  SetNtrack(Int_t ntrack);
    virtual void  SetCurrentTrack(Int_t track);                           
    void  SetHighWaterMark(Int_t hgwmk);    
    // get methods
    virtual Int_t GetNtrack() const;
    Int_t       GetNprimary() const;
    Int_t       GetNtransported() const;
    virtual Int_t GetCurrentTrackNumber() const;
    virtual Int_t GetCurrentParentTrackNumber() const;
    TParticle*  Particle(Int_t id);
    Int_t       GetPrimary(Int_t id);
    TTree*      TreeK() const {return fTreeK;}
    TParticle*  ParticleFromTreeK(Int_t id) const;
    Int_t       TreeKEntry(Int_t id) const;
    Bool_t      IsPhysicalPrimary(Int_t i);
    Bool_t      IsSecondaryFromWeakDecay(Int_t index);
    Bool_t      IsSecondaryFromMaterial (Int_t index);
    Int_t       TrackLabel(Int_t label) const {return fTrackLabelMap[label];}
    Int_t*      TrackLabelMap() {return fTrackLabelMap.GetArray();}
    const TObjArray*  Particles() const;
    
  protected:
    // methods
    void  CleanParents();
    void  ResetArrays(Int_t size);
    TParticle* GetParticleMapEntry(Int_t id) const;
    TParticle* GetNextParticle();
    Bool_t KeepPhysics(const TParticle* part);
    Bool_t IsStable(Int_t pdg) const;
  private:
    void Copy(TObject &st) const;

    // data members
    TClonesArray   fParticles;         //! Pointer to list of particles
    TObjArray      fParticleMap;       //! Map of particles in the supporting TClonesArray
    TArrayI        fParticleFileMap;   //  Map for particle ids 
    TParticle     *fParticleBuffer;    //! Pointer to current particle for writing
    TParticle     *fCurrentTrack;      //! Pointer to particle currently transported
    TTree         *fTreeK;             //! Particle stack  
    Int_t          fNtrack;            //  Number of tracks
    Int_t          fNprimary;          //  Number of primaries
    Int_t	   fNtransported;      //  Number of particles to be transported
    Int_t          fCurrent;           //! Last track returned from the stack
    Int_t          fCurrentPrimary;    //! Last primary track returned from the stack
    Int_t          fHgwmk;             //! Last track purified
    Int_t          fLoadPoint;         //! Next free position in the particle buffer
    TArrayI        fTrackLabelMap;     //! Map of track labels
    ClassDef(StarStack,6) //Particles stack
};

// inline

inline void  StarStack::SetNtrack(Int_t ntrack)
{ fNtrack = ntrack; }

inline Int_t StarStack::GetNtrack() const
{ return fNtrack; }

inline Int_t StarStack::GetNprimary() const
{ return fNprimary; }

inline Int_t StarStack::GetNtransported() const
{ return fNtransported; }

inline Int_t StarStack::GetCurrentTrackNumber() const 
{ return fCurrent; }

inline const TObjArray* StarStack::Particles() const
{ return &fParticleMap; }

// inline protected

inline TParticle* StarStack::GetParticleMapEntry(Int_t id) const
{ return (TParticle*) fParticleMap.At(id); }

#endif //STAR_STACK_H
