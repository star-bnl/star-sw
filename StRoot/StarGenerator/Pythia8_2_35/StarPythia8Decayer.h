#ifndef __StarPythia8Decayer_h__
#define __StarPythia8Decayer_h__

#include "TVirtualMCDecayer.h"
#include "TClonesArray.h"
#ifndef __CINT__
#include "Pythia.h"
#endif

class TLorentzVector;

class StarPythia8Decayer : public TVirtualMCDecayer
{
public:

  /// Initializes the decayer
  void Init();
  /// Decays the particle specified by PDG id and lorentz vector 
  void Decay( int pdg, TLorentzVector *p=0 );
  /// Returns the decay products in a TClonesArray of TParticle
  int ImportParticles( TClonesArray *array = 0 );
  /// 
  void SetForceDecay( int type );
  /// 
  void ForceDecay();
  /// Return the branching ratio for the spdcified PDG ID
  float GetPartialBranchingRatio( int pdgid );
  /// Return teh lifetime in seconds for the specified particle
  float GetLifetime( int pdgid );
  ///
  void ReadDecayTable();
  /// Add a particle with specified PDG ID to the stack
  void AppendParticle( int pdgid, TLorentzVector *p=0 );
  /// Clear the event
  void ClearEvent();
  
  /// Set the debug level
  void SetDebug( int dbg=1 ){ mDebug = dbg; }

  /// Modify pythia8 behavior
  void Set( const char *cmd ){ mPythia->readString(cmd); }

  /// Set root s
  void SetRootS( double rs ){ mRootS = rs; }

  /// Class constructor
  StarPythia8Decayer();  


  /// Class destructor
  ~StarPythia8Decayer();

private:
protected:
#ifndef __CINT__
  Pythia8::Pythia *mPythia;
#endif
  Bool_t           mOwner;
  int            mDebug;
  double           mRootS;

  ClassDef(StarPythia8Decayer,1);
};

#endif
