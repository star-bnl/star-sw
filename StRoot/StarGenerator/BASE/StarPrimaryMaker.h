#ifndef __StarPrimaryMaker_h__
#define __StarPrimaryMaker_h__

#include "StMaker.h"
#include "TLorentzVector.h"
#include "TVector3.h"
#include "TClonesArray.h"
#include "TDatabasePDG.h"
#include "StarParticleStack.h"

#include "StarGenerator/UTIL/StarParticleData.h"

class StarGenerator;
class StarGenEvent;

class TTree;
class TFile;

/**
   \class StarPrimaryMaker
   \author Jason C. Webb
   \brief Main steering class for event generation

   StarPrimaryMaker is the main steering maker for event generators in the (new) STAR
   event generator framework.  Users add one or more event generators (derived from
   StarGenerator) to the maker.  The primary maker is responsible for calling the 
   event generators in a well-defined calling sequence:

   1) PreGenerate Phase

   During the PreGenerate phase, the PreGenerate() method will be called on all generators
   in the order in which they were added to the primary maker.  This (optional) method on
   event generators is provided for the developer to perform any event-by-event configuration
   which may be required by the event generator.

   2) Generate Phase

   During the Generate phase, the event generation machinery of the concrete event generator
   is called.  Generate will be called on all event generators, in the order in which they
   were added to the primary maker.  Generate will be called on all generatos before the 
   PostGenerate phase is entered.  At the end of Generate, it is expected that event generators
   have filled their event records.

   3) PostGenerate Phase

   After Generate has been called on all generators, PostGenerate will be called on each 
   generator in the order in which they were added to the primary maker.  Developers may
   at this point access the results from event generators which were found earlier in the
   chain.  This is the last point at which a developer may interact with the event record.

   After PostGenerate, the event is finalized.  The primary maker will loop over all event
   generators in its list, set the vertex, and accumulate the particles from each generator
   at an appropriate vertex.  Two modes are available on generators:  standard and pileup 
   mode.  Standard mode (which is the default) places all events at the same vertex.  
   Pileup mode places pileup events, with some probability, at an independent vertex.

   Event Record

   The event record, which records the particle-wise and event-wise inforamtion from each
   event generator, is saved in a TTree format.  The results from each generator are saved
   in a separate branch, whose name corresponds to the assigned name of the generator.

   Particle-wise information is stored as an array of StarGenParticle s.  The main event
   record lists all particles which were generated, with inices refering to their position
   in their parent event generator, their position in the event record of the main event 
   generator, and their position on the GEANT stack (aka ID truth).

   
*/

class StarPrimaryMaker : public StMaker
{
 public:
  StarPrimaryMaker();
  ~StarPrimaryMaker();
  
  Int_t InitRun( Int_t runnumber );
  Int_t Init();
  Int_t Make();
  void  Clear( const Option_t *opts="" );
  Int_t Finish();

  /// Set the filename of the output TTree
  void  SetFileName( const Char_t *name ){ mFileName = name; }

  /// Returns a pointer to a particle class containing
  /// PDG information about the particle
  static TParticlePDG *pdg( Int_t pdgid );

  /// Add an event generator to the list of event generators.
  /// @param gener Is a pointer to the user's event generator
  void AddGenerator( StarGenerator *gener );

  /// Set the x, y and z vertex position
  void SetVertex( Double_t x, Double_t y, Double_t z ){ mVx=x; mVy=y; mVz=z; }

  /// Set the smearing in the x, y and z vertex positions.
  /// @sx is the smearing in x
  /// @sy is the smearing in y
  /// @sz is the smearing in z
  /// @rho is the correlation between x and y
  void SetSigma( Double_t sx, Double_t sy, Double_t sz, Double_t rho=0 ){ mSx=sx; mSy=sy; mSz=sz; mRho=rho; }

  /// Set particle cuts
  void SetCuts( Double_t ptmin,    Double_t ptmax=-1, 
		Double_t ymin=0,   Double_t ymax=-1,
		Double_t phimin=0, Double_t phimax=-1,
		Double_t zmin=0,   Double_t zmax=-1 );

  /// Set PT range.  Particles falling outside this range will be dropped from simulation.
  void SetPtRange( Double_t ptmin, Double_t ptmax=-1 ){ mPtMin = ptmin; mPtMax = ptmax; }
  /// Set rapidity range.  Particles falling outside this range will be dropped from simulation.
  void SetEtaRange( Double_t etamin, Double_t etamax ){ mRapidityMin = etamin; mRapidityMax = etamax; }
  /// Set phi range.  Particles falling outside this range will be dropped from simulation.
  void SetPhiRange( Double_t phimin, Double_t phimax ){ mPhiMin = phimin; mPhiMax = phimax; }
  /// Set z-vertex range.  Particles falling outside this range will be dropped from simulation.
  void SetZvertexRange( Double_t zmin, Double_t zmax ){ mZMin = zmin; mZMax = zmax; }

  /// Return a pointer to the event
  const StarGenEvent *event() const { return mPrimaryEvent; }


  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StarPrimaryMaker.h,v 1.2 2012/12/05 22:59:15 jwebb Exp $ built "__DATE__" "__TIME__ ; return cvs;}

 private:
 protected:

  TLorentzVector Vertex();

  Int_t     PreGenerate();
  Int_t        Generate();
  Int_t    PostGenerate();
  Int_t        Finalize();
  void      BuildTables();
  
  /// Total number of particles
  Int_t mNumParticles;

  /// The output tree
  TTree   *mTree;
  TFile   *mFile;
  TString  mTreeName;
  TString  mFileName;
  StarParticleStack *mStack;
  StarGenEvent      *mPrimaryEvent;

  Double_t mVx, mVy, mVz, mSx, mSy, mSz, mRho;

  Double_t mPtMin, mPtMax, mRapidityMin, mRapidityMax, mPhiMin, mPhiMax, mZMin, mZMax;

  /// Tests to see whether the particle passes all appropriate cuts to be passed to the simulator
  Bool_t Simulate( StarGenParticle *p );

  TLorentzVector mPrimaryVertex;

  ClassDef(StarPrimaryMaker,1);

};

#endif
