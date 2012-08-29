/*!
 * \class StEEmcPointMap_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * Class to look up sets of strips and towers given nearness to a
 * specified (x,y) point (assumed to be on the endcap).  Also provides
 * endpoints for strip based on geoId, and distance of closest
 * approach (dca) between a point and a strip or tower.
 *
*/

#ifndef _ST_EEMC_POINT_MAP_
#define _ST_EEMC_POINT_MAP_

#include <set>
#include <Rtypes.h>
#include "StEEmcStripEndPointData.h"
#include "StRoot/StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StRoot/StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"

// The class
class StEEmcPointMap_t {
 public:
   // constructor
   StEEmcPointMap_t( Bool_t crossSectorBoundaries = 1 );

   // deconstructor
   ~StEEmcPointMap_t() { /* */ };

   static void getStripEndPoints( Float_t stripGeoId, Float_t& x1, Float_t& y1, Float_t& x2, Float_t& y2 );
   static void getStripLineParam( Float_t stripGeoId, Float_t& a, Float_t& b );
   static void convertStripUVtoXY( Short_t sector, Float_t u, Float_t v, Float_t& x, Float_t& y );

   Float_t getStripDCA( Float_t x, Float_t y, Short_t stripGeoId ) const;
   Float_t getTowerDCA( Float_t x, Float_t y, Short_t towerGeoId ) const;

   Short_t getStripsNearestPoint( Float_t x, Float_t y, Float_t r = 10 ) const;
   void getStripsNearPoint( Float_t x, Float_t y, Float_t r, std::set< Short_t >& stripSet ) const;

   static Short_t getTowerNearestPoint( Float_t x, Float_t y );
   static void getTowersNearPoint( Float_t x, Float_t y, Float_t r, std::set< Short_t >& towerSet );

   Short_t getSectorOfPoint( Float_t x, Float_t y );

   static StEEmcPointMap_t& instance();

 protected:
   Bool_t mCrossSectorBoundaries;
   static const StEEmcStripEndPointDataVec_t mStripDataVec;

   void addSmdContribution( Float_t x, Float_t y, Float_t r, Short_t sector, Char_t layer, Short_t sectorSide, std::set< Short_t >& stripSet ) const;
   static void loadData();

 private:
   ClassDef(StEEmcPointMap_t,1);
};

#endif

/*
 * $Id: StEEmcPointMap.h,v 1.1 2012/08/29 15:44:17 sgliske Exp $
 * $Log: StEEmcPointMap.h,v $
 * Revision 1.1  2012/08/29 15:44:17  sgliske
 * Moved from offline/users/sgliske/StRoot/StEEmcPool
 *
 *
 */

