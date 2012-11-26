/*
 * Created by S. Gliske, May 2012
 *
 * Description: Container used in the EEmcAnalysisTree.  Note: this
 * class does not explicitly depend on the STAR framework, and so the
 * trees can be read outside of the STAR framework.  Note: data
 * members are all public, to allow a lighter weight implementation.
 *
 */

#ifndef EEmcSmdCluster_H_
#define EEmcSmdCluster_H_

#include <Rtypes.h>
#include <TObject.h>
#include <TArrayS.h>
#include <TArrayF.h>

class EEmcSmdCluster_t : public TObject {
 public:
   EEmcSmdCluster_t();
   virtual ~EEmcSmdCluster_t(){ /* */ };
   void Clear( const Option_t* );

   Float_t   meanPos;
   Float_t   width;
   Float_t   energy;

   Short_t   sector;
   Bool_t    inLayerV;

   enum constant_t { kMaxClusterSize = 25 };

   // to save the weights of the strips associated with the clusters.
   Short_t seedStripIdx;                     // in [0-287]
   Short_t numUsedStrips;                    // in [0-kMaxClusterSize]
   Short_t usedStripIdx[kMaxClusterSize];    // each element in [0-287]
   Float_t usedStripWeight[kMaxClusterSize]; // each element in [0-1]

 private:
   ClassDef( EEmcSmdCluster_t, 3 );
};

#endif

/*
 * $Id: EEmcSmdCluster.h,v 1.1 2012/11/26 19:04:30 sgliske Exp $
 * $Log: EEmcSmdCluster.h,v $
 * Revision 1.1  2012/11/26 19:04:30  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/EEmcTreeContainers to StRoot/StEEmcPool/EEmcTreeContainers
 *
 *
 */
