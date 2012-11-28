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

  /// Add a particle to the database
  void          AddParticle( const Char_t *name,  TParticlePDG *particle );
  /// Add an alias to a particle in the database
  void          AddAlias   ( const Char_t *alias, const Char_t *realname );

  /// Returns a reference to the single instance of this class
  static StarParticleData &instance(){ return sInstance; }

  TParticlePDG *operator()( const Char_t *name ){ return sInstance.GetParticle(name); }
  TParticlePDG *operator()( const Int_t  id    ){ return sInstance.GetParticle(id)  ; }
  
 private:
 protected:

  // Singleton instance of this class
  static StarParticleData sInstance;

  TObjArray                           mParticleList;    // my particles
  std::map< TString, TParticlePDG* >  mParticleNameMap; // particle aliases
  std::map< Int_t,   TParticlePDG* >  mParticleIdMap;   // map by PDG id

  ClassDef( StarParticleData, 1 );

};

#endif
 
 
