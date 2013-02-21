/*!
 * \class StEEmcPointFinderIU_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * Implementation of the point finder algrothim from the PhD
 * dissertation of Weihong He.
 * 
 * Some implementation improvements have been made versus that found in 
 * $CVSROOT/offline/users/aliceb/StRoot/StEEmcPool/StEEmcIUPi0/StEEmcIUPointMaker.cxx
*/

#ifndef _ST_POINT_FINDER_IU_H_
#define _ST_POINT_FINDER_IU_H_

#include <set>

#include "StRoot/St_base/Stypes.h"
#include "StRoot/StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"
#include "StRoot/StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StRoot/StEEmcUtil/EEmcSmdMap/EEmcSmdMap.h"
#include "StRoot/StEEmcUtil/StEEmcSmd/EEmcSmdGeom.h"

#include "StEEmcPointFinder.h"
#include "StSimpleCluster.h"
#include "StEEmcHit.h"
#include "StEEmcHitMaker.h"
#include "StESMDClustersPerSector.h"

class StEEmcPointFinderIU_t : public StEEmcPointFinder_t {

 public:
   StEEmcPointFinderIU_t();
   virtual ~StEEmcPointFinderIU_t() { /* */ };

   /// clear things before doing the finding for the next event
   virtual void clear(){ /* */ };

   /// find some some points
   virtual Int_t find( const EEmcEnergy_t& eemcEnergy,
                       const StSimpleClusterVec_t &towerClusterVec,
                       const StESMDClustersVec_t &smdClusterVec,
                       const Double_t* smdEuEvRatio,
                       StEEmcHitVec_t& hitVec );

   // modifiers
   void setMinUVRatio( Float_t ratio ){ mMinUVratio = ratio; };
   void setTowerThreshold( Float_t thres ){ mTowerThreshold = thres; };
   void setZratioThres( Float_t thres ){ mZratioThres = thres; };

 protected:
   // function used to sort strips
   // defined here so is inlined
   struct clusterLessThan {
      const Bool_t operator()( const StSimpleCluster_t *a, const StSimpleCluster_t *b) const {
         // check for 0
         if (a == 0) {
            return b != 0; // if b is also 0, then they are equal, hence a is not < than b
         } else if (b == 0) {
            return false;
         } else {
            return a->getSeedMember() < b->getSeedMember();
         };
      };
   };
   struct clusterMoreThan {
      const Bool_t operator()( const StSimpleCluster_t *a, const StSimpleCluster_t *b) const {
         // check for 0
         if (b == 0) {
            return a != 0; // if a is also 0, then they are equal, hence a is not > than b
         } else if (a == 0) {
            return false;
         } else {
            return a->getSeedMember() > b->getSeedMember();
         };
      };
   };

   // Function used to sort candidate hits.
   static Bool_t hitTtest2LessThan( const StEEmcHit_t &h1, const StEEmcHit_t &h2 ) {
      return h1.getTtest2() < h2.getTtest2();
   };

   // typedef for the cluster pools
   typedef std::set< const StSimpleCluster_t*, clusterLessThan > StClusterPool_t;
   //typedef std::set< const StSimpleCluster_t* > StClusterPool_t;


   // geometry type classes
   EEmcSmdGeom *mEEmcSmdGeom;
   EEmcSmdMap *mEEmcSmdMap;
   EEmcGeomSimple mEEmcGeomSimple;

   // the ratio of u/v energy and v/u energy must both be greater than this minimum
   Float_t mMinUVratio;
   Float_t mZratioThres;
   Float_t mTtestThres;
   Bool_t mUseBugFixes;

   // Minimun energy for a tower to allow a point underneath
   Float_t mTowerThreshold;

   // class variable to keep track of things for recurseive algo
   Int_t mSector;
   Int_t mLastSMDhitID;

   // recursive point finder
   Int_t findPoints( const EEmcEnergy_t& eemcEnergy,
                     StClusterPool_t &uClusterVec,
                     StClusterPool_t &vClusterVec,
                     StEEmcHitVec_t& hitVec );

 private:
   ClassDef( StEEmcPointFinderIU_t, 1 );

};

#endif

/*
 * $Id: StEEmcPointFinderIU.h,v 1.2 2013/02/21 22:00:44 sgliske Exp $ 
 * $Log: StEEmcPointFinderIU.h,v $
 * Revision 1.2  2013/02/21 22:00:44  sgliske
 * general update
 *
 * Revision 1.1  2012/11/26 19:05:55  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
