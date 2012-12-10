#include "StarParticleData.h"
ClassImp(StarParticleData);

#include "TDatabasePDG.h"
#include "TCollection.h"
#include "THashList.h"
#include "assert.h"

using namespace std;

/// Helper function to define PDG ids for heavy ions
/// @param z Charge of the heavy ion
/// @param a Atomic number of the heavy ion
/// @param l Number of lambdas in a hypernucleus
Int_t hid( Int_t z, Int_t a, Int_t l=0 )
{
  //         10LZZZAAAI
  return (   1000000000
	 +     10000000*l					       
	 +        10000*z					
         +           10*a );
}

StarParticleData StarParticleData::sInstance;
// ---------------------------------------------------------------------------------------------
StarParticleData::~StarParticleData()
{

}
// ---------------------------------------------------------------------------------------------
StarParticleData::StarParticleData( const Char_t *name, TDataSet *parent ) :
  TObjectSet(name)
{

  if ( parent )                Shunt(parent); 

  // Particle list will own the particles added to it, and is responsible for
  // cleaning up their memory.
  mParticleList.SetOwner();

  //
  // Intiailze the particle data from TDatabasePDG
  //
  TDatabasePDG *pdg = TDatabasePDG::Instance();
  pdg -> ReadPDGTable(); // because it's too much to expect from a singleton

  TIter Next( pdg->ParticleList() );

  TParticlePDG *particle = 0;
  while(  (particle=(TParticlePDG *)Next())  )
    {

      TString name = particle->GetName();
      Int_t   code = particle->PdgCode();

      mParticleList. Add( (TParticlePDG *)particle->Clone(name) );   
      mParticleNameMap[ name ] = particle;
      mParticleIdMap[ code ]   = particle; 

    }
 
  //
  // Register the object array with the dataset
  //
  AddObject( &mParticleList, false );

  //
  // Create aliases for typical beam particles to handle differences
  // between event generator names and TDatabasePDG names... shouldn't
  // need e+ or pbar, but who knows...
  //
  AddAlias("electron",   "e-");
  AddAlias("positron",   "e+");
  AddAlias("p",    "proton");
  AddAlias("pbar", "antiproton");

  //
  // Next create typical heavy ions used in beams at RHIC... "hid" is defined
  // to help define PDG ids for the heavy ions
  //

  TParticlePDG *D     = new TParticlePDG( "D",     "Deuteron", /* mass */ 0.0,  true,  0., 1.0, "heavyion", hid(1,2,0),   0, 45 );
  TParticlePDG *He3   = new TParticlePDG( "He3",   "Helium-3", /* mass */ 0.0,  true,  0., 2.0, "heavyion", hid(2,1,0),   0, 49 );
  TParticlePDG *Cu    = new TParticlePDG( "Cu",    "Copper",   /* mass */ 0.0,  true,  0., 29,  "heavyion", hid(29,64,0),  0, 0 );
  TParticlePDG *Au    = new TParticlePDG( "Au",    "Gold",     /* mass */ 0.0,  true,  0., 79,  "heavyion", hid(79,197,0), 0, 0 );
  TParticlePDG *U     = new TParticlePDG( "U",     "Uranium",  /* mass */ 0.0,  true,  0., 92,  "heavyion", hid(92,238,0), 0, 0 );

  AddParticle("D",   D);
  AddParticle("He3", He3);
  AddParticle("Cu",  Cu);
  AddParticle("Au",  Au);
  AddParticle("U",   U);

  //
  // TODO: Add in the hypertriton and its antiparticles
  //

#undef hid

  

}
// ---------------------------------------------------------------------------------------------
//
// ---------------------------------------------------------------------------------------------
TParticlePDG *StarParticleData::GetParticle( const Char_t *name )
{
  return mParticleNameMap[ name ];
}
// ---------------------------------------------------------------------------------------------
//
// ---------------------------------------------------------------------------------------------
TParticlePDG *StarParticleData::GetParticle( const Int_t id )
{
  return mParticleIdMap[id];
}
// ---------------------------------------------------------------------------------------------
//
// ---------------------------------------------------------------------------------------------
void StarParticleData::AddParticle( const Char_t *name, TParticlePDG *particle )
{
  Int_t   code = particle->PdgCode();
  mParticleList.Add( particle );
  if ( mParticleNameMap[ name ] ) {    Warning( "AddParticle()", Form("Overwriting entry %s",name) );  }
  mParticleNameMap[ name ] = particle;
  if ( mParticleIdMap[ code ] ) {    Warning( "AddParticle()", Form("Overwriting entry %i",code) );  }
  mParticleIdMap[code]     = particle;
  return;
}
// ---------------------------------------------------------------------------------------------
//
// ---------------------------------------------------------------------------------------------
void StarParticleData::AddAlias( const Char_t *alias, const Char_t *name )
{
  TParticlePDG *particle = GetParticle(name);
  AddParticle( alias, particle );
}
// ---------------------------------------------------------------------------------------------
//
// ---------------------------------------------------------------------------------------------
