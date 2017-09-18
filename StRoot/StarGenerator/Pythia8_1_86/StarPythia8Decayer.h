#ifndef __StarPythia8Decayer_h__
#define __StarPythia8Decayer_h__

#include "TVirtualMCDecayer.h"
#include "TClonesArray.h"
#include "Pythia.h"

class TLorentzVector;

class StarPythia8Decayer : public TVirtualMCDecayer
{
public:

  /// Initializes the decayer
  void Init();
  /// Decays the particle specified by PDG id and lorentz vector 
  void Decay( Int_t pdg, TLorentzVector *p=0 );
  /// Returns the decay products in a TClonesArray of TParticle
  Int_t ImportParticles( TClonesArray *array = 0 );
  /// 
  void SetForceDecay( Int_t type );
  /// 
  void ForceDecay();
  /// Return the branching ratio for the spdcified PDG ID
  Float_t GetPartialBranchingRatio( Int_t pdgid );
  /// Return teh lifetime in seconds for the specified particle
  Float_t GetLifetime( Int_t pdgid );
  ///
  void ReadDecayTable();
  /// Add a particle with specified PDG ID to the stack
  void AppendParticle( Int_t pdgid, TLorentzVector *p=0 );
  /// Clear the event
  void ClearEvent();
  
  /// Set the debug level
  void SetDebug( Int_t dbg=1 ){ mDebug = dbg; }

  /// Modify pythia8 behavior
  void Set( const Char_t *cmd ){ mPythia->readString(cmd); }

  /// Set root s
  void SetRootS( double rs ){ mRootS = rs; }

  /// Class constructor
  StarPythia8Decayer( Pythia8::Pythia *pythia = 0 );

  /// Class destructor
  ~StarPythia8Decayer();

private:
protected:

  Pythia8::Pythia *mPythia;
  Bool_t           mOwner;
  Int_t            mDebug;
  double           mRootS;

  ClassDef(StarPythia8Decayer,1);
};

#endif
