/*!
 * \class StSimpleCluster_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * The basic information needed to define a cluster:
 *  - an ID for the cluster
 *  - an array of indices of elements
 *  - an array of weights for the elements
 *
 * This class is designed to be light weight and is optimized for
 * using minimal storage space, i.e. so it can be easily and compactly
 * saved as part of a TTree in a TFile.  Note: all functions are
 * inlined and default copy constructors, equal operators, and
 * deconstructors are used.
 *
 * Note: Only information that cannot be recomputed "quickly" is
 * stored.
 * 
 *
*/

#ifndef _ST_SIMPLE_CLUSTER_H_
#define _ST_SIMPLE_CLUSTER_H_

#include <TObject.h>
#include <Rtypes.h>
#include <TArrayS.h>
#include <TArrayF.h>
#include <iostream>
#include <vector>
#include <list>

//#include "StRoot/St_base/StMessMgr.h"

//#define ClassDefVec( CLASS );

/// Forward declaration
class StSimpleCluster_t;

/// Containers
typedef std::vector< StSimpleCluster_t > StSimpleClusterVec_t;
typedef std::list< StSimpleCluster_t > StSimpleClusterList_t;

// force RootCint.pl to make vector dictionary
// ClassDefVec( StSimpleCluster_t );

/// The class
class StSimpleCluster_t : public TObject {
 public:
   // constructor
   StSimpleCluster_t( Short_t mID_in = -1 ) : mID( mID_in ), mSeedIdx(0) { /* */ };
   ~StSimpleCluster_t() { /* */ };

   // accessors
   Short_t getID() const { return mID; };
   Float_t getEnergy() const { return mE; };

   TArrayS& getMemberArray() { return mMember; };
   const TArrayS& getMemberArray() const { return mMember; };
   Short_t getMember( Int_t i ) const { return mMember[i]; };

   TArrayF& getWeightArray() { return mWeight; };
   const TArrayF& getWeightArray() const { return mWeight; };
   Float_t getWeight( Int_t i ) const { return mWeight[i]; };
      
   Int_t getSeedIdx() const { return mSeedIdx; };  // the index of these seed in the internal Member and Weight arrays

   Int_t getSeedMember() const { return mSeedIdx < mMember.GetSize() ? mMember[mSeedIdx] : -1; };
   Int_t getSeedWeight() const { return mSeedIdx < mWeight.GetSize() ? mWeight[mSeedIdx] : -1; };

   Float_t getMeanX() const { return mMeanX; };   // used for both tower and strip clusters
   Float_t getMeanY() const { return mMeanY; };   // only used for tower clusters


   // modifiers
   void setID( const Short_t ID ) { mID =  ID; };
   void setEnergy( const Float_t E ) { mE =  E; };
   void setMemberArray( const TArrayS array ){ mMember = array; };
   void setWeightArray( const TArrayF array ){ mWeight = array; };
   void setSeedIdx( Int_t seedIdx ) { mSeedIdx = seedIdx; };
   void setMeanX( Float_t mean ){ mMeanX = mean; };   // used for both tower and strip clusters
   void setMeanY( Float_t mean ){ mMeanY = mean; };   // only used for tower clusters

   // operators
   StSimpleCluster_t& operator+=(  const StSimpleCluster_t& other );
   //   Bool_t operator <(const StSimpleCluster_t& b) const {
   //      return mID < b.mID;
   //   };

   // for display
   friend std::ostream &operator<<( std::ostream &out, const StSimpleCluster_t &clus );

 protected:
   Short_t mID;
   Float_t mE, mMeanX, mMeanY;
   TArrayS mMember;
   TArrayF mWeight;

   // keep track of which (if any) of the members is the seed.
   // defaults to 0.
   Int_t mSeedIdx;

 private:
   /// Make class available to root
   ClassDef(StSimpleCluster_t,1);   // Simplest class to describe a cluster


};

std::ostream &operator<<( std::ostream &out, const StSimpleCluster_t &clus );

#endif

/*
 * $Id: StSimpleCluster.h,v 1.1 2012/11/26 19:05:56 sgliske Exp $ 
 * $Log: StSimpleCluster.h,v $
 * Revision 1.1  2012/11/26 19:05:56  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
