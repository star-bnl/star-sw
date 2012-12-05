#ifndef __StarKinematics_h__
#define __StarKinematics_h__

/**
   \class StarKinematics
   \author Jason Webb
   \brief Star Simple Kinematics Generator

   StarKinematics provides a simple event generator capable of injecting single particles, 
   or distributions of particles, into a simulation.

   Usage:

   // Create primary generator (main steering code)
   StarPrimaryMaker *primary = new StarPrimaryMaker();

   // Create kinematics generator and add it to the main maker
   StarKinematics *kine = new StarKinematics("kine");
   primary->AddGenerator( kine );
   
   // ... add other makers to chain

   // Initialize the chain
   chain -> Init();

   for ( Int_t i=0;i<nevents;i++ )
   {

      if ( option1 ) // Option 1: throw Npart muons flat in pt, eta, phi
      {
          kine->Kine(Npart, "muon+", 0., 10., -2.0, +2.0, -3.1415, +3.1415 );
      }
      if ( option2 ) // Option 2: add a single photon to the event
      {
          StarGenParticle *gamma = kine->AddParticle("gamma");
	  // Set particle properties explicitly ...
	  gamma->SetPx( 2.0 );
	  gamma->SetPy( 1.0 );
	  gamma->SetPz( 10.0 ); 
      }
      if ( option3 ) // Option 3: throw particles according to a pT distribution
      {
         // ptDist, etaDist can be a TF1 or TH1
	 kine->Dist( Npart, "pi0", ptDist, etaDist );
      }


      // Call Make() on all makers in the chain
      chain->Make();

   }
      
   
*/

#include "StarGenerator/BASE/StarGenerator.h"
#include "StarGenerator/EVENT/StarGenEvent.h"

#include "TMath.h"

#include "TH1.h"
#include "TF1.h"

class StarKinematics : public StarGenerator
{
 public:
  StarKinematics( const Char_t *name="StarKine" );
  ~StarKinematics(){ /* nada */ };

  /// Create a new particle on the event stack and return a pointer.  User is responsible
  /// for specifying the kinematics of the particle.  The status will be marked as kFinal.
  StarGenParticle *AddParticle();

  /// Create a new particle of the specified type and return a pointer.  User is responsible
  /// for specifying the kinematics of the particle.  The status will be marked as kFinal.
  /// @param type The type of the particle (will throw an assert if not recognized)
  StarGenParticle *AddParticle( const Char_t *type );

  /// Emulates STARSIM aguser/gkine.  Throws ntrack particles of specified type flat in
  /// pT, over the specified range ptlow to pthigh.  Rapidity and phi ranges may also be
  /// specified
  /// @param ntrack  Number of particles (tracks) to generate.
  /// @param type    Type of the particle to generate.  Defaults to mu+
  /// @param ptlow   Minimum pT
  /// @param pthigh  Maximum pT
  /// @param ylow    Minimum rapidity
  /// @param yhigh   Maximum rapidity
  /// @param philow  Minimum azimuthal angle
  /// @param phihigh Maximum azimuthal angle
  void Kine( Int_t ntrack, const Char_t *type="muon+", Double_t ptlow=0.0, Double_t pthigh=500.0,
	     Double_t ylow=-10.0, Double_t yhigh=+10.0,
	     Double_t philow=0.0, Double_t phihigh=TMath::TwoPi() );

  /// Generates ntracks of the specified type according to 1D distributions
  /// in pT, eta and phi.  If not provided, phi will be flat.
  /// @param ntrack Number of particles (tracks) to generate
  /// @param type   type of the particle
  /// @param pt     Distribution (TF1) to sample pT from.
  /// @param y      Distribution (TF1) to sample rapidity from.
  /// @param phi    Distribution (TF1) to sample phi from.  Optional.
  void Dist( Int_t ntrack, const Char_t *type, TF1 *pt, TF1 *y, TF1 *phi=0 );
  /// Generates ntracks of the specified type according to 1D distributions
  /// in pT, eta and phi.  If not provided, phi will be flat.
  /// @param ntrack Number of particles (tracks) to generate
  /// @param type   type of the particle
  /// @param pt     Distribution (TH1) to sample pT from.
  /// @param y      Distribution (TH1) to sample rapidity from.
  /// @param phi    Distribution (TH1) to sample phi from.  Optional.
  void Dist( Int_t ntrack, const Char_t *type, TH1 *pt, TH1 *y, TH1 *phi=0 );

  /// Generate event
  Int_t Generate();
  Int_t PreGenerate();
  Int_t PostGenerate();

  void Clear( const Option_t *opts="" ){ /* nada */ };

  /// Initialize generator
  Int_t Init(){ return kStOK; }

 private:
 protected:

  // User's event
  StarGenEvent *mUser;

  ClassDef(StarKinematics,1);

};

#endif
