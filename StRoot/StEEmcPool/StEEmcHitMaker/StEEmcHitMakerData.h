/*!
 * \class StEEmcHitMaker_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * Includes the basic data from the hit maker that one would want to
 * save to disk.  Thus, one needs not to save (serialize) the entire
 * Hit Maker class, but only this class.
 *
 * Note, this class only has accessors, and no modifiers, as the
 * modification (creation) is done by child classes.
 *
*/

#ifndef _ST_EEMC_HIT_MAKER_DATA_H_
#define _ST_EEMC_HIT_MAKER_DATA_H_

// closely related inclusions
#include "StEEmcHit.h"
#include "StSimpleCluster.h"
#include "StESMDClustersPerSector.h"

class StEEmcHitMakerData_t {
 public:
   /// To keep track of layers of towers
   /// definition is consistant with EEMC A2EMaker
   enum TowerLayer_t { TOWER = 0, PRESHOWER1 = 1, PRESHOWER2 = 2, POSTSHOWER = 3, NUM_TOW_LAYERS = 4 };

   /// Construtor
   StEEmcHitMakerData_t(){ /* */ };

   /// Deconstructor
   virtual ~StEEmcHitMakerData_t(){ /* */ };

   /// no copy constructor nor equals operator needed, since includes no dynamically allocated memory;

   /// Clear for next event
   virtual void clearData();

   /// const accessors
   const StEEmcHitVec_t& getHitVec() const { return mHitVec; };
   const StSimpleClusterVec_t& getTowerClusterVec( TowerLayer_t layer ) const { return getTowerClusterVec(layer); };
   const StSimpleClusterVec_t& getTowerClusterVec() const { return mTowerClusterVec[ TOWER ]; };
   const StESMDClustersVec_t& getESMDClustersVec() const { return mESMDClusterVec; };

   /// non-const accessors
   StEEmcHitVec_t& getHitVec() { return mHitVec; };
   StSimpleClusterVec_t& getTowerClusterVec( TowerLayer_t layer );
   StSimpleClusterVec_t& getTowerClusterVec() { return mTowerClusterVec[ TOWER ]; };
   StESMDClustersVec_t& getSMDClusterVec() { return mESMDClusterVec; };

   Int_t getHitVecSize() const { return mHitVec.size(); };

   StEEmcHitMakerData_t& getHitMakerDataRef() { return *this; };

 protected:
   /// Containers for cluster from each tower layer
   StSimpleClusterVec_t mTowerClusterVec[ NUM_TOW_LAYERS ];

   /// Containers for clusters from each SMD layer (U or V) and for
   /// each sector
   StESMDClustersVec_t mESMDClusterVec;

   /// Container for hits
   StEEmcHitVec_t mHitVec;

 private:
   ClassDef( StEEmcHitMakerData_t, 1 );
};

#endif

/*
 * $Id: StEEmcHitMakerData.h,v 1.1 2012/11/26 19:05:54 sgliske Exp $
 * $Log: StEEmcHitMakerData.h,v $
 * Revision 1.1  2012/11/26 19:05:54  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 *
*/
