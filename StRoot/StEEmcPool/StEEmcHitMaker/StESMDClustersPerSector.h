/*!
 * \class StESMDClustersPerSector_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * Storage class for all the SMD clusters per sector.
 *
*/

#ifndef _ST_ESMD_CLUSTER_PER_SECTOR_H_
#define _ST_ESMD_CLUSTER_PER_SECTOR_H_

#include <Rtypes.h>

#include "StSimpleCluster.h"

/// Forward declaration
class StESMDClustersPerSector_t;

/// Containers
typedef std::vector< StESMDClustersPerSector_t > StESMDClustersVec_t;

//#define ClassDefVec( CLASS );

// force RootCint.pl to make vector dictionary
//ClassDefVec( StESMDClustersPerSector_t );

/// The class
class StESMDClustersPerSector_t {
 public:
   // constructor
   StESMDClustersPerSector_t() : mSector(0) { /* */ };
   StESMDClustersPerSector_t( Short_t sector ) : mSector(sector) { /* */ };
   ~StESMDClustersPerSector_t(){ /* */ };

   // accessors
   StSimpleClusterVec_t& getClusterVecU() { return mClusterVecU; };
   StSimpleClusterVec_t& getClusterVecV() { return mClusterVecV; };
   const StSimpleClusterVec_t& getClusterVecU() const { return mClusterVecU; };
   const StSimpleClusterVec_t& getClusterVecV() const { return mClusterVecV; };
   Int_t getSector() const { return static_cast< Int_t >( mSector); };

   // modifiers
   void setSector( Short_t sector ){ mSector = sector; };
   void clear(){ mClusterVecU.clear(); mClusterVecV.clear(); };

 protected:
   Short_t mSector;                    //! the sector
   StSimpleClusterVec_t mClusterVecU;  //! the U clusters
   StSimpleClusterVec_t mClusterVecV;  //! the V clusters

 private:
   /// Make class available to root
   ClassDef(StESMDClustersPerSector_t,1);   // Simplest class to describe a cluster

};

#endif

/*
 * $Id: StESMDClustersPerSector.h,v 1.1 2012/11/26 19:05:56 sgliske Exp $ 
 * $Log: StESMDClustersPerSector.h,v $
 * Revision 1.1  2012/11/26 19:05:56  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
