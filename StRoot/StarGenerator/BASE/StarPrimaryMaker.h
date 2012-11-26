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

   StarPrimaryMaker is the class which is responsible for running the event generators
   used in a simulation, pushing particles out to the geant stack for simulation, aggregating
   particles from multiple event generators into a common event record, and performing I/O 
   so that event records are persistent.

   The StarGenerator class is the interface class between the concrete event generator
   and the STAR software chain.  
   

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

  /// Set PT range
  void SetPtRange( Double_t ptmin, Double_t ptmax=-1 ){ mPtMin = ptmin; mPtMax = ptmax; }
  /// Set rapidity range
  void SetEtaRange( Double_t etamin, Double_t etamax ){ mRapidityMin = etamin; mRapidityMax = etamax; }
  /// Set phi range
  void SetPhiRange( Double_t phimin, Double_t phimax ){ mPhiMin = phimin; mPhiMax = phimax; }
  /// Set z-vertex range
  void SetZvertexRange( Double_t zmin, Double_t zmax ){ mZMin = zmin; mZMax = zmax; }

  /// Return a pointer to the event
  const StarGenEvent *event() const { return mPrimaryEvent; }


  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StarPrimaryMaker.h,v 1.1 2012/11/26 17:11:28 jeromel Exp $ built "__DATE__" "__TIME__ ; return cvs;}

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
