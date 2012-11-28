#ifndef __StarKinematics_h__
#define __StarKinematics_h__

/**
   \class StarKinematics
   \author Jason Webb
   \brief Star Simple Kinematics Generator
   
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
  StarGenParticle *AddParticle( const Char_t *type );

  /// Emulates STARSIM aguser/gkine.  Throws ntrack particles of specified type flat in
  /// pT, over the specified range ptlow to pthigh.  Rapidity and phi ranges may also be
  /// specified
  void Kine( Int_t ntrack, const Char_t *type="mu+", Double_t ptlow=0.0, Double_t pthigh=500.0,
	     Double_t ylow=-10.0, Double_t yhigh=+10.0,
	     Double_t philow=0.0, Double_t phihigh=TMath::TwoPi() );

  /// Generates ntracks of the specified type according to 1D distributions
  /// in pT, eta and phi.  If not provided, phi will be flat.
  void Dist( Int_t ntrack, const Char_t *type, TF1 *pt, TF1 *y, TF1 *phi=0 );
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
