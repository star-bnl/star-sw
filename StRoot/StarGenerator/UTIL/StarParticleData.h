#ifndef __StarParticleData_h__
#define __StarParticleData_h__

#include "TParticlePDG.h"
#include "TObjectSet.h"

#include <map>

/**
   \class StarParticleData
   \author Jason C. Webb
   \brief Interface to PDG information

   StarParticleData provides an interface to the data provided
   in the TDatabasePDG class from ROOT.  We extend the available
   list of particles, including some standard heavy ions which are
   lacking in the ROOT table, and provide the capability to alias names to
   individual particles.

 */

class StarParticleData : public TObjectSet
{
 public:

  StarParticleData( const Char_t *name="PDG_Database",       TDataSet *parent = 0 );
  ~StarParticleData();

  /// Get a particle by name
  TParticlePDG *GetParticle( const Char_t *name ) ;
  /// Get a particle by PDG ID
  TParticlePDG *GetParticle( const Int_t   id   );
  /// Get a particle by G3 ID
  TParticlePDG *GetParticleG3( const Int_t id );

  /// Add a particle to the database
  void          AddParticle( const Char_t *name,  TParticlePDG *particle ); 
  /// Add an alias to a particle in the database
  void          AddAlias   ( const Char_t *alias, const Char_t *realname );

  /*!
    Particle definition normal constructor. If the particle is set to be
    stable, the decay width parameter does have no meaning and can be set to
    any value. The parameters granularity, LowerCutOff and HighCutOff are
    used for the construction of the mean free path look up tables. The
    granularity will be the number of logwise energy points for which the
    mean free path will be calculated.

    @param name Name of the particle
    @param title Title 
    @param mass  Mass of the particle [GeV]
    @param stable True if the particle is stable
    @param width Total width of the particle (meaningless if stable) [GeV]
    @param charge3 Charge of the particle in units of |e|/3... i.e. proton would be charge3 = 3, down quark charge3 = -1.
    @param particleClass is the class of the particle, e.g. lepton, hadron, ...
    @param pdgCode is the Particle Data Group code
    @param geantCode is the geant code of the particle

  */
  TParticlePDG *AddParticle( const Char_t *name, const Char_t *title, Double_t mass, Bool_t Stable, Double_t Width, Double_t Charge3, const char* ParticleClass, Int_t PdgCode, Int_t Anti, Int_t geantCode );

  /*!
    Particle definition to DB and G3 
  */
  TParticlePDG *AddParticleToG3( const char*   name, const double mass, const double lifetime, const double charge, const int tracktype, const int pdgcode, const int g3code, const double* bratio=0, const int* mode=0 );
  TParticlePDG *AddParticleToG3( TParticlePDG *part, const int g3code );

  /// Maps the particle with the given PDG id to G3 tracking ID and returns pointer to the particle data.
  TParticlePDG *SetTrackingCode( const int pdgid, const int g3id );


  /*!
    Add a decay channel to the named particle
    @param name Name of the particle
 
    AddDecay( const Char_t *name, Int_t num, Int_t matrixCode, Double_t branchingRatio, TArrayI kids );
   
  */

  /// Returns a reference to the single instance of this class
  static StarParticleData &instance(){ return sInstance; }

  TParticlePDG *operator()( const Char_t *name ){ return sInstance.GetParticle(name); }
  TParticlePDG *operator()( const Int_t  id    ){ return sInstance.GetParticle(id)  ; }

  /// Returns a reference to the list of particles.
  const TObjArray &GetParticles() const { return mParticleList; }

 private:
 protected:

  // Singleton instance of this class
  static StarParticleData sInstance; 

  TObjArray                           mParticleList;      // my particles
  std::map< TString, TParticlePDG* >  mParticleNameMap;   // particle aliases
  std::map< Int_t,   TParticlePDG* >  mParticleIdMap;     // map by PDG id
  std::map< Int_t,   TParticlePDG* >  mParticleG3IdMap;   // map by G3 id

  ClassDef( StarParticleData, 1 );

};

#endif
 
 
