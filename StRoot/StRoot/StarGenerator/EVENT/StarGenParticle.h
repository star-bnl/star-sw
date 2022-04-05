#ifndef __StarGenParticle_h__
#define __StarGenParticle_h__

#include "TObject.h"
#include "TRef.h"
#include "TMath.h"
#include "TLorentzVector.h"

/**

   \class StarGenParticle

   \brief Yet another particle class
 
   The StarGenParticle class is the class which represents event-generator particles
   within the event record.  It is based on the HEPEVT standard, developed many years
   ago when FORtran ruled the earth.  It stores particle information in a "table" format,
   specifying

   mId -- the Particle Data Group ID of the particle
   mStatus -- the status code of the particle, according to the HepMC standard noted below
   mMother[0] -- the index of the first mother of this particle in the array of particles
   mMother[1] -- the index of the last mother of this particle in the array of particles
   mDaughter[0] -- the index of the first daughter of this particle in the array of particles
   mDaughter[1] -- the index of the last  daughter of this particle in the array of particles

   mPx, mPy, mPz, mEnergy -- Specifies the four-momentum of this particle
   mMass -- the mass of the particle as specified by the event generator
   mVx, mVy, mVz, mTof -- the four-position of this particle
   
   It is the developer's responsability to set these variables, usually when calling
   StarGenEvent::AddParticle().

   Additinal variables are provided for bookkeeping purposes.

   mIndex -- the position of the particle within the array of particles
   mPrimaryKey -- the position of the particle within the primary event record's array of particles
   mStack -- the position of the particle on the geant stack, which corresponds to the ID truth in the MuDst or the "primary key" in the g2t_track record

   HEPEVT status codes (as defined in the HepMC manual)
     
   After some discussion, the authors in MCnet [18] have agreed to a 
   clarification of the HEPEVT  status codes. The Fortran Monte Carlo 
   generators will not change their behaviour, but Sherpa, Pythia8, and 
   Herwig++ will go to the newer usage.   These are the accepted status 
   code definitions:
   o 0 : an empty entry with no meaningful information and therefore to 
         be skipped unconditionally
   o 1 : a final-state particle, i.e. a particle that is not decayed further 
         by the generator (may also include unstable particles that are to be 
         decayed later, as part of the detector simulation). Such particles must
         always be labelled '1'
   o 2 : decayed Standard Model hadron or tau or mu lepton, excepting virtual 
         intermediate states thereof (i.e. the particle must undergo a normal decay, 
         not e.g. a shower branching). Such particles must always be labelled 2. No 
         other particles can be labelled 2.
   o 3 : a documentation entry
   o 4 : an incoming beam particle
     
   o 5-10 : undefined, reserved for future standards
   o 11-200: an intermediate (decayed/branched/...) particle that does not fullfil the criteria of status code
             2, with a generator-dependent classification of its nature.
   o 201- : at the disposal of the user, in particular for event tracking in the detector

  \author Jason C. Webb

*/

class StarGenParticle : public TObject
{
 public:
  StarGenParticle();
  ~StarGenParticle(){ };


  enum State { kNull,          /**< Null entry */
	       kFinal,         /**< Final state particle which will not be decayed further by the event generator */
	       kDecayed,       /**< Standard model hadron, muon or tau which has decayed.  Code does not include intermediate-state hadrons. */
	       kDocumentation, /**< Documentation line in the record */ 
	       kIncident,      /**< Incident beam particle */
	       kUnknown        /**< Negative numbers will signify STAR event header information */
  };

	          
  /// Set the status code of the particle according to the HEPEVT standard
  void  SetStatus( Int_t status ){ mStatus = status; }

  /// Get the status code of the particle according to the HEPEVT standard
  Int_t GetStatus() { return mStatus; }
  /// Set the id code of the particle according to the PDG standard.  The
  /// mass of the particle will be set according to the data tabulated in
  /// the PDG. 
  void  SetId( Int_t id );
  /// Get the id code of the particle according to the PDG standard
  Int_t GetId() { return mId; }
  /// Set the first mother particle in the array of particles
  void SetFirstMother( Int_t first ) { mMother[0]=first; }
  void SetFirstMother( StarGenParticle *p ){ SetFirstMother(p->GetIndex()); }
  /// Get the first mother particle
  Int_t GetFirstMother() { return mMother[0]; }
  /// Set the last mother particle in the array of particles
  void SetLastMother( Int_t last ) { mMother[1]=last; }
  void SetLastMother( StarGenParticle *p ){ SetLastMother(p->GetIndex()); }
  /// Get the last mother particle
  Int_t GetLastMother(){ return mMother[1]; }
  /// Set the first daughter particle in the array of particles
  void SetFirstDaughter( Int_t first ) { mDaughter[0]=first; }
  void SetFirstDaughter( StarGenParticle *p ) {SetFirstDaughter(p->GetIndex()); }
  /// Get the first daughter particle
  Int_t GetFirstDaughter() { return mDaughter[0]; }
  /// Set the last daughter particle in the array of particles
  void SetLastDaughter( Int_t last ) { mDaughter[1]=last; }
  void SetLastDaughter( StarGenParticle *p ) {SetLastDaughter(p->GetIndex()); }
  /// Get the last daughter particle
  Int_t GetLastDaughter(){ return mDaughter[1]; }

  /// Set the x-component of the momentum
  void SetPx( Float_t px ){ mPx = px; }
  /// Get the x-component of the momentum
  Float_t GetPx(){ return mPx; }
  /// Set the y-component of the momentum
  void SetPy( Float_t py ){ mPy = py; }
  /// Get the y-component of the momentum
  Float_t GetPy(){ return mPy; }
  /// Set the z-component of the momentum
  void SetPz( Float_t pz ){ mPz = pz; }
  /// Get the z-component of the momentum
  Float_t GetPz(){ return mPz; }

  /// Returns the transverse momentum of the particle
  Float_t pt(){ return TMath::Sqrt(pt2()); }
  /// Returns the transverse momentum squared
  Float_t pt2(){ return mPx*mPx+mPy*mPy; }
  /// Returns the transverse mass of the particle
  Float_t mt(){ return TMath::Sqrt(mt2()); }
  /// Returns the transverse mass squared
  Float_t mt2(){ return mMass*mMass+mPx*mPx+mPy*mPy; }

  /// Return the 4-momentum of the particle
  TLorentzVector momentum(){ return TLorentzVector(mPx,mPy,mPz,mEnergy); }



  /// Set the energy
  void SetEnergy( Float_t energy ){ mEnergy = energy; }
  /// Get the energy
  Float_t GetEnergy(){ return mEnergy; }
  /// Set the mass
  void SetMass( Float_t mass ){ mMass = mass; }
  /// Get the mass
  Float_t GetMass(){ return mMass; }

  /// Set the x-component of the start vertex
  void SetVx( Float_t vx ){ mVx = vx; }
  /// Get the x-component of the start vertex
  Float_t GetVx(){ return mVx; }
  /// Set the y-component of the start vertex
  void SetVy( Float_t vy ){ mVy = vy; }
  /// Get the y-component of the start vertex
  Float_t GetVy(){ return mVy; }
  /// Set the z-component of the start vertex
  void SetVz( Float_t vz ){ mVz = vz; }
  /// Get the z-component of the start vertex
  Float_t GetVz(){ return mVz; }
  /// Set the tof
  void SetTof( Float_t tof ){ mTof = tof; }
  /// Get the tof
  Float_t GetTof(){ return mTof; }

  /// Set the line number in the event record
  void SetIndex( Int_t i ) { mIndex = i; }
  /// Get the line number in the event record
  Int_t GetIndex(){ return mIndex; }

  /// Set the line number in the particle stack
  void SetStack( Int_t i ){ mStack = i; }
  /// Get the line number in the particle stack
  Int_t GetStack(){ return mStack; }

  void SetPrimaryKey( Int_t k ){ mPrimaryKey = k; }
  Int_t GetPrimaryKey(){ return mPrimaryKey; }

  void SetGeneratorId( Int_t id ){ mGeneratorId = id; }
  Int_t GetGeneratorId(){ return mGeneratorId; }

  /// Print the particle
  void Print( const Option_t *opts="" ) const;

  /// Returns true if the particle should be simulated (i.e. pushed out to
  /// the geant stack.
  Bool_t Simulate();

 private:

  // ------------------------------------------------------
  // Start of hepevt entry.  These variables must remain
  // packed together in this order in order to emulate the
  // HEPEVT record
  Int_t   mStatus;      
  Int_t   mId;
  Int_t   mMother[2];
  Int_t   mDaughter[2];
  Float_t mPx, mPy, mPz, mEnergy, mMass;
  Float_t mVx, mVy, mVz, mTof;
  // End of hepevt entry.  Address is returned using, and 
  // used in StarPrimaryMaker to fill the g2t particle 
  // table for backwards compatability with the starsim era
  // framework
  void *get_hepevt_address(){ return &mStatus; }
  // ------------------------------------------------------  


  ///\todo Add user-defined weight (or optionally a parallel array of particle attributes users can populate)
  ///\todo Add polarization


 protected:
  Int_t mIndex;
  Int_t mStack;
  Int_t mPrimaryKey;

  Int_t mGeneratorId;

  friend class StarPrimaryMaker;

  ClassDef(StarGenParticle,1);
};

#endif
/*
struct particle_st {
        int     isthep; //status code of the entry 
        int     idhep;  //particle identity, accordingly to the PDG standard 
        int     jmohep[2];      //pointer(s) to position where the mother(s) stored 
        int     jdahep[2];      //pointers to position of the first/last daughter 
        float   phep[5];        //p4 and mass (GeV) 
        float   vhep[4];        //production vertex (mm) and time (mm/c) 
}
 */
