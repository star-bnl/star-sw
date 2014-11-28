/*!
 *
 * \class StEEmcHit_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * Additional information to describe a hit, more than that in StSimpleHit.
 *
 * Note: most functions are inlined and default copy constructors,
 * equal operators, and deconstructors are used.
 *
 * WARNING: TODO: if allow hits with u and v not in the same sector,
 * will need to update this!!!
*/

#ifndef _ST_EEMC_HIT_H_
#define _ST_EEMC_HIT_H_

#include <iostream>
#include <TArrayS.h>
#include <TArrayF.h>

#include "StSimpleHit.h"

/// Forward declaration
class StEEmcHit_t;

/// Containers
typedef std::vector< StEEmcHit_t > StEEmcHitVec_t;
typedef std::vector< StEEmcHit_t* > StEEmcHitPtrVec_t;

//#define ClassDefVec( CLASS );

// force RootCint.pl to make vector dictionary
//ClassDefVec( StEEmcHit_t );

/// The class
class StEEmcHit_t : public StSimpleHit_t {
 public:
   // constructor
   StEEmcHit_t( Short_t ID_in = -1 ) : StSimpleHit_t( ID_in ), mSector(-1), mClusIDu(-1), mClusIDv(-1), mTowerIdx(-1), mEnergyU(-1), mEnergyV(-1),
      mTtest2(1e100), mWeightU(1), mWeightV(1), mIsValid(1) { /* */ };
   ~StEEmcHit_t(){ /* */ };

   // enum
   // enum PidType_t { PHOTON = 0, LEPTON = 1, HADRON = 2 };

   // accessors
   Short_t getSector() const { return mSector; };
   Short_t getClusIDu() const { return mClusIDu; };
   Short_t getClusIDv() const { return mClusIDv; };
   Int_t getTowerIdx() const { return mTowerIdx; };
   Float_t getEnergyU() const { return mEnergyU; };
   Float_t getEnergyV() const { return mEnergyV; };
   Float_t getTtest2() const { return mTtest2; };    // this is really the energy asymmetry squared
   Float_t getWeightU() const { return mWeightU; };
   Float_t getWeightV() const { return mWeightV; };
   Bool_t isValid() const { return mIsValid; };
   Int_t getNumUsedTowers() const { return mUsedTowerIndices.GetSize(); };
   //PidType_t getPid() const { return mPid; };

   // not yet programmed, but allow interface
   Short_t getSectorU() const { return mSector; };
   Short_t getSectorV() const { return mSector; };

   // note: TArray::At does bounds checking
   Short_t getUseTowerIndex( Int_t localIndex ) const { return mUsedTowerIndices.At( localIndex ); };
   Float_t getUseTowerWeight( Int_t localIndex ) const { return mUsedTowerWeights.At( localIndex ); };

   // modifiers
   void setSector( Short_t sec ){ mSector = sec; };
   void setClusIDu( Short_t idx ){ mClusIDu = idx; };
   void setClusIDv( Short_t idx ){ mClusIDv = idx; };
   void setTowerIdx( Int_t idx ){ mTowerIdx = idx; };
   void setEnergyU( Float_t E ) { mEnergyU = E; };
   void setEnergyV( Float_t E ) { mEnergyV = E; };
   void setWeightU( Float_t E ) { mWeightU = E; };
   void setWeightV( Float_t E ) { mWeightV = E; };
   void setIsValid( Bool_t isValid ){ mIsValid = isValid; };
   void setNumUsedTowers( UInt_t n );
   void addUsedTower( Int_t localIndex, Short_t towIndex, Float_t weight );
   void setUsedTowers( std::vector< Short_t >& usedIndices, std::vector< Float_t >& weights );
   //void setPid( PidType_t pid ){ mPid = pid; };

   // compute t-test
   void computeTtest2() {
      mTtest2 = ( mEnergyU - mEnergyV ) / ( mEnergyU + mEnergyV );
      mTtest2 *= mTtest2;
   };

   // to fill a EEmcHit_t from an StEEmcHit_t
   friend class StEEmcTreeMaker_t;

 protected:
   Short_t mSector;
   Short_t mClusIDu, mClusIDv;
   Int_t mTowerIdx;                                             // the main tower it is under
   Float_t mEnergyU, mEnergyV, mTtest2, mWeightU, mWeightV;
   Bool_t mIsValid;                                             //! just a flag whether to keep in vector
   //PidType_t mPid;

   TArrayS mUsedTowerIndices;                                   // The indices of the towers used to compute the energy
   TArrayF mUsedTowerWeights;                                   // The weights for the towers used to compute the energy

 private:
   /// Make class available to root
   ClassDef(StEEmcHit_t,1);   // Class to store EEmc Hits

};

std::ostream &operator<<( std::ostream &out, const StEEmcHit_t &hit );

#endif

/*
 * $Id: StEEmcHit.h,v 1.2 2013/02/21 22:00:44 sgliske Exp $ 
 * $Log: StEEmcHit.h,v $
 * Revision 1.2  2013/02/21 22:00:44  sgliske
 * general update
 *
 * Revision 1.1  2012/11/26 19:05:54  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
