#ifndef __AgUDecay_h__
#define __AgUDecay_h__

#include "TObject.h"
#include "TLorentzVector.h"
#include "StarDecayManager.h"
class TClonesArray;

class TVirtualMCDecayer;
class TParticle;

/**!
   \class AgUDecay
   \brief Interface between starsim and virtual decayer (VMC implementation)

 */

class AgUDecay : public TObject
{
public:

  static AgUDecay &instance(){ return sInstance; }
  void   SetDecayer( StarDecayManager *dcay = 0 ){ mDecayer = dcay; }

  static StarDecayManager *Manager(){ 
    StarDecayManager *d = new StarDecayManager();
    instance().SetDecayer( d );
    return d;
  };

  int operator()();

  enum DiscoveryPolicy_t 
    {
      kDecay = 0, // Decayer handles the decay (may miss materials)
      kSpawn      // Add particle to G3 and DB      
    };

  void SetDiscovery( DiscoveryPolicy_t p ){ mDiscovery = p; }

  /// Debug method.  Will throw an exception when pdgid is decayed.
  static void setParticleStop( const int pdgid ){ mParticleStop[pdgid]=1; }    
 
private:
protected:

  AgUDecay();
  virtual ~AgUDecay(); 

  static AgUDecay sInstance;

  StarDecayManager *mDecayer;
  TClonesArray *mArray;
  TLorentzVector mP;

  DiscoveryPolicy_t mDiscovery;
  int mNextG3id;

  static std::map<int, int> mParticleStop;

  double StackParticleForTransport( const TParticle* particle );
  bool MayTransport( const TParticle* particle );

  int mNonConservation;

  ClassDef(AgUDecay,1);
};

#endif
