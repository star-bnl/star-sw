#ifndef __StarDecayManager_h__
#define __StarDecayManager_h__

#include "TVirtualMCDecayer.h"
#include "StMessMgr.h"

#include <map>
#include <vector>
using namespace std;

/**!
   \class StarDecayManager
   \brief Connects VMC to class(es) which handle particle decays


   The STAR Decay Manager is the interface between the virtual Monte Carlo and
   classes used to handle particle decays (Decayers).  Particles which are not
   recognized by the Monte Carlo application will be passed to the decay manager.
   The decay manager decides which (if any) decayer is responsible for decaying
   the specified particle, and initiates the decay, and returns the list of 
   particles to the calling code.

 */

class StarDecayManager : public TVirtualMCDecayer
{
public:
  StarDecayManager( const Char_t *name="DecayManager" );
 ~StarDecayManager();

  /// Initializes the decayer
  void Init(); 

  /// Decays the particle specified by PDG id and lorentz vector 
  void Decay( Int_t pdg, TLorentzVector *p=0 );

  /// Returns the decay products in a TClonesArray of TParticle
  Int_t ImportParticles( TClonesArray *array = 0 );

  /// 
  void SetForceDecay( Int_t pdgid );
  /// 
  void ForceDecay();
  /// Return the branching ratio for the spdcified PDG ID
  Float_t GetPartialBranchingRatio( Int_t pdgid );
  /// Return teh lifetime in seconds for the specified particle
  Float_t GetLifetime( Int_t pdgid );
  ///
  void ReadDecayTable();
  /// Add a particle with specified PDG ID to the stack

  void ClearEvent();
  
  /// Set the debug level
  void SetDebug( Int_t dbg=1 ){ mDebug = dbg; }

  /// Register a particle deayer to the specified PDG id
  /// @param pdgid Is the Particle Data Group ID.  If pdgid=0, specifies the default decayer for all IDs not specifically mapped to a decayer.
  /// @param decayer Specifies an instance of TVirtualMCDecayer to handle the particle decay.
  void AddDecayer( Int_t pdgid, TVirtualMCDecayer *decayer ){ mDecayer[pdgid] = decayer; }
  

private:
protected:


  // Maps decayers to particle ID
  map<Int_t, TVirtualMCDecayer *> mDecayer;
  TVirtualMCDecayer *mCurrentDecayer;

  Int_t mDebug;




  ClassDef(StarDecayManager,1);
};

#endif
