#ifndef __AgStarParticle_h__
#define __AgStarParticle_h__

#include "TObject.h"
#include "StarCallf77.h"

class AgStarParticle 
{
public:
  ClassDef(AgStarParticle,1);

  enum Type {
    kGtGama = 1, // photons handled by GTGAMA
    kGtElec,     // e+/e- handled by GTELEC
    kGtNeut,     // neutral hadrons handled by GTNEUT
    kGtHadr,     // charged hadrons handled by GTHADR
    kGtMuon,     // mu+/mu- handled by GTMUON
    kGtNino,     // geantinos
    kGtCkov,     // cherenkov photons
    kGtHion,     // heavy ions
    kGtMonp,     // magnetic monopole handled by GTMONP (for historical purpose only...)
    kUnknown
  };

  /// Add a particle to the G3 particle table
  /// @param name of the particle
  /// @param g3id G3 id
  /// @param type track type given by enum
  /// @param mass mass of the particle
  /// @param charge charge of the particle
  /// @param lifetime in seconds
  /// @param bratio branching ratios
  /// @param decay modes
  /// @param pdgid (optional) pdgid
  static void Add( const char* name, const int g3id, const int type, const float mass, const float charge, const float lifetime=0., const float* bratio=0, const int* mode=0, const int pdgid=0 );

  virtual ~AgStarParticle(){ /* nada */ };

private:

  AgStarParticle(){ /* nada */ };
 
};
#endif
