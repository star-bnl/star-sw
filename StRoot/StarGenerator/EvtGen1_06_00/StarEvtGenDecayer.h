/**
 \class StarEvtGenDecayer
 
 \brief STAR wrapper for EvtGen Decayer
 
 Authors: Xiaozhi Bai (xiaozhi@uic.edu),
 Mustafa Mustafa (mmustafa@lbl.gov)
 Zhenyu Ye (yezhenyu@uic.edu)
 */

#ifndef StarEvtGenDecayer__h
#define StarEvtGenDecayer__h

#include <cstddef>

#include "TVirtualMCDecayer.h"
#include "TClonesArray.h"
#include "TString.h"
#include "EvtGen/EvtGen.hh"
#include "HepMC/GenEvent.h"

class TLorentzVector;
class EvtRandomEngine;
class EvtParticle;

class StarEvtGenDecayer : public TVirtualMCDecayer
{
public:
    void Init();
    /// Decays the particle specified by PDG id and lorentz vector
    void Decay(Int_t pdgId, TLorentzVector* p);
    /// Returns the decay products in a TClonesArray of TParticle
    Int_t ImportParticles(TClonesArray* particles=0);
    ///
    void SetForceDecay(Int_t type);
    ///
    void ForceDecay();
    /// Return the branching ratio for the spdcified PDG ID
    Float_t GetPartialBranchingRatio(Int_t ipart);
    /// Return teh lifetime in seconds for the specified particle
    Float_t GetLifetime(Int_t pdgid);
    ///
    void ReadDecayTable();
    /// Add a particle with specified PDG ID to the stack
    void AppendParticle( Int_t pdgid, TLorentzVector *p=0 );
    /// Clear the event
    void ClearEvent();
    /// Set the debug level
    void SetDebug( Int_t dbg=1 ){ mDebug = dbg; }
    /// Modify EvtGen behavior
    void SetDecayTable(TString decayTable);
    /// Modify initial vertex position
    void SetVertex(TLorentzVector* r);

    /// Class constructor
    StarEvtGenDecayer(EvtGen* evtGen = NULL);
    /// Class destructor
    ~StarEvtGenDecayer();
    /// Initializes the decayer

private:
    EvtGen*          mEvtGen;
    EvtRandomEngine* mEvtGenRandomEngine;
    EvtParticle*     mParticle;
    EvtVector4R*     mVertex;
    bool             mOwner;
    Int_t            mDebug;

    ClassDef(StarEvtGenDecayer,1);
};
#endif
