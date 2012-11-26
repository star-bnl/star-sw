#ifndef __StarGenEvent_h__
#define __StarGenEvent_h__
#include "TString.h"
#include <vector>
#include "TObjArray.h"
#include "TClonesArray.h"
#include "TCollection.h"
#include "TObject.h"

class TClonesArray;
class StarGenParticle;

/**
   \class StarGenEvent
   \author Jason C Webb
   \brief Base class for event records
   
   StarGenEvent is the base class for event records in the STAR event generator
   framework.  It provides the base functionality which is common accross all
   event generators.  It holds a record of all particles (the particle record)
   and event-wised information (header) releveant for all event generators.  The
   StarGenEvent class should generally not be used for an event record, as it
   is far too general.  Developers should typically choose one of the derived
   classes -- StarGenPPEvent, StarGenEPEvent, StarGenAAEvent or StarGenEAEvent --
   for their event record, inheriting from it and extending its functionality
   if needed.

   The particle record is based on the HEPEVT and HepMC standards.  Generated 
   particles are created as instances of the StarGenParticle class and added
   to a list of particles.  The mother-daughter relationships of these particles
   is encoded as integers which refer to the index of the mother (daughter) 
   particle within the list.  Developers should take care in implementing their
   event generators to make sure that the status-code conventions defined in
   StarGenParticle are understood and respected.  The decision whether a particle
   is stable and should be stacked out for geant simulationn is based on that
   convention.

   The particle record maintains two lists of particles.  The complete event 
   record, listing all particles which have been generated, and
   the list of particles which have been stacked out to the geant simulation.
   It is the responsability of the developer to fill in the full list
   of particles.  The framework will take care of the decision of whether
   to stack the particle out and fill in the reference list of particles
   whcih have been fully simulated.

   mParticles -- Maintains the list of all particles generated in the event.  An
   extra entry providing summary inforamtion is inserted at the front of the
   record by StarGenerator.
   
   Header inforamtion is kept simple, as different event generators will have 
   a different set of variables which a developer will want to be filled.  So
   here we provide only the event number, run number, generator and process ID.
   Additional variables to store kinematic information are implemented in the 
   derived classes.

 */

class StarGenEvent : public TObject
{ 
 public:

  /// Constructor
  StarGenEvent( const Char_t *name="event", const Char_t *title="" );
  /// Destructor
  ~StarGenEvent();
  
  /// Set the generator ID
  void SetGeneratorId( Int_t id ) { mGeneratorId = id; }
  /// Returns the generator ID
  Int_t GetGeneratorId(){ return mGeneratorId; }
  /// Set the process ID for this event 
  void SetProcessId( Int_t id ){ mProcessId = id; }
  /// Get the process ID for this event
  Int_t GetProcessId(){ return mProcessId; }
  /// Set the particle index offset for this event
  void SetOffset( Int_t o ){ mOffset = o; }
  /// Get the particle index offset for this event 
  Int_t GetOffset(){ return mOffset; }  

  /// Prefix increment operator increases event number
  Int_t operator++(){ return ++mEventNumber; }
  /// Postfix increment operator increases event number
  Int_t operator++(Int_t){ return mEventNumber++; }
  /// Get the event number 
  Int_t GetEventNumber(){ return mEventNumber; }

  /// Set the run number for this event
  void SetRunNumber( Int_t run ){ mRunNumber = run; }
  /// Get the run number for this event
  Int_t GetRunNumber(){ return mRunNumber; }
  /// Set the run number for the DAQ file in an embedding job
  void SetDaqRunNumber( Int_t run ){ mDaqRunNumber= run; }
  /// Get the run number for the DAQ file in this event
  Int_t GetDaqRunNumber(){ return mDaqRunNumber; }
  /// Set the file number in an embedding job
  void SetDaqFileNumber( Int_t fnum ){ mDaqFileNumber= fnum; }
  /// Get the file number in an embedding job
  Int_t GetDaqFileNumber(){ return mDaqFileNumber; }

  /// Set the blue beam ID
  void SetBlue( Int_t id ) { mBlueId = id; }
  /// Get the blue beam ID
  Int_t GetBlue(){ return mBlueId; }
  /// Set the yellow beam ID
  void SetYell( Int_t id ){ mYellId = id; }
  /// Get the yellow beam ID
  Int_t GetYell(){ return mYellId; }
  /// Set the sqrt(s) of the collision
  void SetRootS( Double_t rs ){ mCmsEnergy = rs; }
  /// Get the sqrt(s) of the collision
  Double_t GetRootS(){ return mCmsEnergy; }
    


  /// Add a particle to the list of particles.  StarGenEvent is responsible for
  /// cleaning up the memory.
  StarGenParticle *AddParticle();

  /// Add a particle to the list of particles.  StarGenEvent is responsible for
  /// cleaning up the memory.
  /// @param status the status code of the particle using the HepMC standard
  /// @param pdg    the particle datagroup ID of the particle
  /// @param m1     the first mother in the event record
  /// @param m2     the last mother in the event record.  Note:  m1 to m2 must be continuous.
  /// @param d1     the first daughter in the event record
  /// @param d2     the last daughter in the event record.  Note:  d1 to d2 must be continuous.
  /// @param px     the x-component of the momentum
  /// @param py     the y-component of the momentum
  /// @param pz     the z-component of the momentum
  /// @param E      the energy
  /// @param M      the mass
  /// @param vx     the x-component of the production vertex
  /// @param vy     the y-component of the production vertex
  /// @param vz     the z-component of the production vertex
  /// @param tof    the time of flight to the production vertex
  StarGenParticle *AddParticle( Int_t status, Int_t pdg, Int_t m1, Int_t m2, Int_t d1, Int_t d2, 
				Double_t px, Double_t py, Double_t pz, Double_t E, Double_t M,
				Double_t vx, Double_t vy, Double_t vz, Double_t vt );


  /// Add a particle to the list of particles.  The particle will be copied, and a pointer to the
  /// copy returned.  StarGenEvent is responsible for cleaning up the memory.
  /// @param particle Pointer to a StarGenParticle which is to be added.  The 
  StarGenParticle *AddParticle( StarGenParticle *particle );

  /// Print the event
  void Print( const Option_t *opts="" ) const;

  /// Obtain an iterator over all generated particles
  TIter IterAll( Bool_t dir=kIterForward ){ 
    return TIter( mParticles, dir ); 
  }

  /// Clear the event
  virtual void Clear( const Option_t *opts="part,data" );

  /// Obtain a pointer to the particle at index idx
  StarGenParticle *operator[]( Int_t idx ){ 
    return (StarGenParticle *)(*mParticles)[idx]; 
  }

  /// Obtain the number of particles in the event record
  Int_t GetNumberOfParticles(){ return mNumParticles; }


 private:
  /// Copy ctor is private.  
  StarGenEvent( const StarGenEvent &other );
  /// Assigment operator is private.
  StarGenEvent &operator=( const StarGenEvent &other );

 protected:

  TString       mName;
  TString       mTitle;

  TClonesArray *mParticles;        // Array of particles

  Int_t    mGeneratorId;           // Generator Id
  Int_t    mProcessId;             // Event generator process ID
  Int_t    mOffset;                // Event generator offset

  Int_t    mEventNumber;           // Event number
  Int_t    mRunNumber;             // Monte Carlo run number
  Int_t    mDaqRunNumber;          // DAQ run number (for embedding)
  Int_t    mDaqFileNumber;         // File number (for embedding)

  Int_t    mBlueId;                // PDG for blue beam 
  Int_t    mYellId;                // PDG for yellow beam 
  
  Double_t mCmsEnergy;             // aka sqrt(s) 

  Int_t    mNumRejected[3];        // 0=total, 1=EG, 2=filter
  Int_t    mFilterResult;          // Result of filter

  std::vector<Double_t> mWeights;  // User weights

  Int_t mNumParticles;

  /// Initialize clowns arrays
  void InitArrays();
  
  ClassDef(StarGenEvent,1);
};



#endif
