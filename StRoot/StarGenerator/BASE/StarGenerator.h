#ifndef __StarGenerator_h__
#define __StarGenerator_h__

#include "StMaker.h"
#include "TString.h"
#include "TDatabasePDG.h"
#include "StarCallf77.h"
#include "TLorentzVector.h"

#include "StarGenerator/EVENT/StarGenEvent.h"
#include "StarGenerator/EVENT/StarGenStats.h"
#include "StarGenerator/UTIL/StarParticleData.h"


/**
   \class StarGenerator
   \author Jason C. Webb
   \brief ABC for defining event generator interfaces
   
   StarGenerator is the abstract base class for building interfaces between event
   generators and the STAR simulation package.  The concrete implementation of 
   a StarGenerator class minimally implements three methods:  Init() to initialize
   the event generator, Generate() to exercise the event generation machinery of
   an event generator, and FillXX() to fill an event record.  In addition, hooks
   are provided which execute before any event generator runs -- PreGenerate() -- and
   after all event generators have run -- PostGenerate().  

   Event generators will typically specify distance in either cm or mm.  In order to
   match the simulation dimensions, multiply all distances by the appropriate unit
   when filling the event record.

 */

class StarGenerator : public StMaker
{
 public:

  StarGenerator( const Char_t *name="" );   
  ~StarGenerator(){ /* nada */ };

 protected:
  /// If an event generator measures distances in mm, then multiply by mm.
  const Double_t mm;
  /// If an event generator measures distances in cm, then multiply by cm.
  const Double_t cm;

 public:
  /// Developer must provide initialization routine.  This method should call
  /// any functions, subroutines and/or methods on the concrete event generator
  /// necessary for its initialization.
  virtual Int_t Init() = 0;

  /// Developer may not provide run initialization.
  Int_t InitRun( Int_t runnumber ) { return kStOK; };

  /// Developers may provide a pre-generate method which will execute before Generate().
  virtual Int_t PreGenerate(){ return kStOK; }
  /// Developers may provide a post-generate method which will execute after Generate().
  virtual Int_t PostGenerate(){ return kStOK;  };

  /// Developers must provide a generate method.  In this method, the event generation 
  /// machinery should be called.   It is expected that the event record is filled in
  /// the Generate method, so that particles and event characteristics are available to
  /// subsequent generators and the primary generator maker.  The primary maker is 
  /// responsible for pushing particles out to the concrete simulation package.
  virtual Int_t Generate() = 0;

  /// (Optional) Method to fill a PP event
  virtual void FillPP( StarGenEvent *event ){ };
  /// (Optional) Method to fill a AA event
  virtual void FillAA( StarGenEvent *event ){ };
  /// (Optional) Method to fill a DIS event
  virtual void FillEP( StarGenEvent *event ){ };
  /// (Optional) Method to fill a eA event
  virtual void FillEA( StarGenEvent *event ){ };
  /// (Optional) Method to fill a user-defined event
  virtual void FillUser( StarGenEvent *event ){ };

  /// 
  Int_t PreGenerateHook();
  ///
  Int_t PostGenerateHook();
  /// 

  /// If called, make simply aliases to the Generate() function.
  Int_t Make() { return Generate(); }

  /// Clear the event
  void Clear( const Option_t *opts="" );
 
  /// Finalize the event is called after PreGenerate, Generate and PostGenerate
  /// have been called.  It increments the event counter.
  Int_t Finalize();


  /********************************************************************************
   *
   * Configuration Options
   *
   *******************************************************************************/


  /// Sets the particle species for the blue beam
  void SetBlue( const Char_t *b );// { mBlue = b; }
  /// Sets the particle species for the yellow beam
  void SetYell( const Char_t *y ); //{ mYell = y; }
  /// Sets the frame of the interaction and the corresponding energy
  /// @param frame May be one of "CMS" and "FIXT", for center-of-mass and fixed-target respectively.
  /// @param val   Is the CMS energy \f$\sqrt{s}\f$ in the "CMS" frame, or the beam momentum in the fixed target frame.  val > 0 for blue beam incident, val < 0 for yellow beam incident.
  void SetFrame( const Char_t *frame, const Double_t val );
  /// Sets the frame of the interaction and the corresponding momenta of the beams
  /// @param frame Is one of "3MOM", "4MOM", "5MOM"
  /// @param pblue Is an array of dimension 3, 4 or 5 depending on the selected frame.  The elements of the array should contain px, py, pz, energy and mass.  (i.e., "4MOM" p[]={px,py,pz,energy}.)
  /// @param pyell Is an array of dimension 3, 4 or 5 depending on the selected frame.  The elements of the array should contain px, py, pz, energy and mass.  (i.e., "4MOM" p[]={px,py,pz,energy,mass}.)
  void SetFrame( const Char_t *frame, const Double_t *pblue, const Double_t *pyell );

  /// Set the minimum and maximum impact parameters for the collision (HI generators)
  void SetImpact( Double_t bmin, Double_t bmax )
  {
    mImpactMin = bmin;
    mImpactMax = bmax;
  }

  /// Returns the number of particles in the event
  Int_t GetNumberOfParticles(){ return mNumberOfParticles; }

  /// Sets the pileup flag.  If true, StarPrimaryMaker will generate events
  /// with probability "prob" from this generator and add them to a new
  /// event vertex.
  void   SetPileup( Bool_t p, Double_t prob=0.10 ){ mIsPileup = p; if ( p ) mPileup=prob; else mPileup=1.0; }


  /// Returns true if this is a pileup generator
  Bool_t IsPileup(){ return mIsPileup; }

  /// Returns the pileup probability
  Double_t GetPileup(){ return mPileup; }


  /// Sets the output tree.  This method is called by the StarPrimaryMaker and
  /// should not be used by the user.
  void SetOutputTree ( TTree *tree );

  /// Sets the input tree. 
  /// @param tree Pointer to the TTree used for input
  /// @param bname Name of the branch to be used.  If not provided, will default to the name of the class.
  void SetInputTree ( TTree *tree, const Char_t *bname=0 );

  void SetInputFile ( const Char_t *name, const Char_t *treename="genevents", const Char_t *bname=0 );

  /// Returns the ID of this event generator


  Int_t IOmode(){ return mIOMode; }

  /// Retrieves the event record
  StarGenEvent *Event(){ return mEvent; }

  /// Create and retrieve end-of-run statistics
  virtual StarGenStats Stats() { return StarGenStats(); }


  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StarGenerator.h,v 1.6 2015/04/24 18:21:11 jwebb Exp $ built " __DATE__ " " __TIME__ ; return cvs;}


 private:

  // Generator ID
  Int_t mId;
  // Set generator ID
  void SetId( Int_t id ) { mId = id; }

 protected:
  // Number of particles in this event
  Int_t mNumberOfParticles;

  /// Generated event
  StarGenEvent *mEvent;

  /// Name of the blue beam particle (+z)
  TString  mBlue;
  /// Name of the yellow beam particle (-z)
  TString  mYell;
  /// Frame of the collision, i.e. CMS, FIXT, 3MOM, 4MOM, 5MOM
  TString  mFrame;
  /// CMS energy or incident beam momentum for fixed target collisions
  Double_t mRootS;      
  /// Direction (+1 = W, -1 = E) of the beam in fixted target mode
  Double_t mDirect;

  /// Minimum impact parameter in a HI collision
  Double_t mImpactMin;
  /// Maximum impact parameter in a HI collision
  Double_t mImpactMax;

  /// 4-Momentum of the blue beam
  TLorentzVector mBlueMomentum;
  /// 4-Momentum of the yellow beam
  TLorentzVector mYellMomentum;
  /// Mass of the blue beam
  Double_t mBlueMass;
  /// Mass of the yellow beam
  Double_t mYellMass;

  /// Position of the interaction vertex
  Double_t mVertex[3]; 
  /// Width of the interaction vertex
  Double_t mSpread[3]; 

  /// IO flag.  0=no IO, 1=write, 2=read
  Int_t  mIOMode;         //!
  /// Pointer to the tree for reading in events
  TTree *mInputTree;      //! 
  /// Pointer to the tree for writing out events
  TTree *mOutputTree;     //!

  /// Flag which indicates whether this is a pileup simulation
  Bool_t   mIsPileup;
  /// Indicates the probability that this generator should be run in pileup mode
  Double_t mPileup;

  /// Database for particle information (based on TDatabasePDG)
  StarParticleData *mParticleDb;

  /// Concrete event generator library
  TString mLibrary;

  friend class StarPrimaryMaker;

  ClassDef(StarGenerator,1);

};
#endif
