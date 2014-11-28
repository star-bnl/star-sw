/*!
 * \class StEEmcHitMaker_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * Generic basic class for hit finding.  Note, data intended for
 * serialization (i.e. saved to a TFile) is stored in the parent
 * StEEmcHitMakerData_t class.
 *
*/

#ifndef _ST_EEMC_HIT_MAKER_H_
#define _ST_EEMC_HIT_MAKER_H_

// StRoot inclusions
#include "StRoot/StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"
#include "StMaker.h"

// containers
class EEmcEnergy_t;

// closely related inclusions
#include "StEEmcHit.h"
#include "StSimpleCluster.h"
#include "StESMDClustersPerSector.h"
#include "StEEmcHitMakerData.h"

class StEEmcHitMaker_t : public StMaker, public StEEmcHitMakerData_t {
 public:
   /// Construtor
   StEEmcHitMaker_t( const Char_t *myName, const Char_t *inputMkrName );  // inputMaker is for the EEmcEnergy_t pointer

   /// Deconstructor
   virtual ~StEEmcHitMaker_t();

   /// Initialize
   Int_t Init();

   /// Make (loads data, for children classes to use)
   Int_t Make();

   /// Clear for next event
   void  Clear(Option_t *opts="");

   /// modifiers

   /// Set which layers of tower to cluster.
   /// Slightly confusing since one "layer" of the tower is the sum of
   /// all layers.
   void doClusterTowers(     Bool_t flag = 1 ){ mDoClusterTower[ TOWER ] = flag; };
   void doClusterPreShower1( Bool_t flag = 1 ){ mDoClusterTower[ PRESHOWER1 ] = flag; };
   void doClusterPreShower2( Bool_t flag = 1 ){ mDoClusterTower[ PRESHOWER2 ] = flag; };
   void doClusterPostShower( Bool_t flag = 1 ){ mDoClusterTower[ POSTSHOWER ] = flag; };

   /// Set whether to cluster SMD strips
   void doClusterSMDStrips( Bool_t flag = 1 ){ mDoClusterSMDStrips = flag; };

   /// accessors
   Bool_t getIfClusteredTower( TowerLayer_t layer ) const;
   Bool_t getIfClusteredSMD() const { return mDoClusterSMDStrips; };

   Int_t getNumTowerClusters(){ return mTowerClusterVec[ TOWER ].size(); };
   Int_t getNumSMDClusters();

   // To access data storage
   static UInt_t convertToIndex( Int_t sec, Int_t sub, Int_t eta ){ return kEEmcNumEtas*( sec*kEEmcNumSubSectors + sub ) + eta; };
   static UInt_t convertToPhiBin( Int_t sec, Int_t sub ){ return sec*kEEmcNumSubSectors + sub; };

 protected:
   // regarding finding the response input
   Bool_t mInputIsTree;
   std::string mInputMakerName;
   EEmcEnergy_t *mEEmcEnergy;

   /// boolean flags
   Bool_t mDoClusterTower[ NUM_TOW_LAYERS ];
   Bool_t mDoClusterSMDStrips;
   Bool_t mConstructedOK;

   // Number of phi bins
   static const Int_t kEEmcNumPhiBins;

 private:
   ClassDef( StEEmcHitMaker_t, 1 );
};

#endif

/*
 * $Id: StEEmcHitMaker.h,v 1.1 2012/11/26 19:05:54 sgliske Exp $ 
 * $Log: StEEmcHitMaker.h,v $
 * Revision 1.1  2012/11/26 19:05:54  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
