/*!
 * \class StEEmcPointMap_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * See description in the header file.
 *
 * TODO: remove highest strips from "shorter" (edge) sectors
*/

#include "StEEmcPointMap.h"

#include <set>
#include <Rtypes.h>
#include "StRoot/St_base/StMessMgr.h"
#include "StRoot/St_base/Stypes.h"
#include "StEEmcStripEndPointData.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StRoot/StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"
#include "StRoot/StEEmcPool/StEEmcGeoId/StEEmcGeoId.h"

// 15 degrees
#ifndef PI_OVER_TWELVE
#define PI_OVER_TWELVE 0.2617993877991494263
#endif

// 30 degrees
#ifndef TWO_PI_OVER_TWELVE
#define TWO_PI_OVER_TWELVE 0.5235987755982988527
#endif

// Cut-off for the edge
#ifndef EDGE_FACTOR
#define EDGE_FACTOR 1.025
#endif


void StEEmcPointMap_t::getStripLineParam( Float_t stripGeoId, Float_t& a, Float_t& b ) {
   Float_t x1, x2, y1, y2;
   getStripEndPoints( stripGeoId, x1, y1, x2, y2 );

   Float_t theta = atan2( y2 - y1, x2 - x1 );

   // correct for rounding error: quantize theta to 5 degree marks
   // 0.0872664 [rad] = 5 [degrees]
   theta = 0.0872664*static_cast< Int_t >( theta/0.0872664 + 0.5 );

   a = tan( theta );
   b = y1 - a * x1;
};


void StEEmcPointMap_t::getStripEndPoints( Float_t stripGeoId, Float_t& x1, Float_t& y1, Float_t& x2, Float_t& y2 ) {

   // make sure the data is loaded
   if( mStripDataVec.empty() )
      loadData();

   // geo qualifers for the strip
   Short_t stripSector, stripIndex;
   Bool_t layerIsV;

   // decode geo id
   StEEmcGeoId_t::decodeSmd( stripGeoId, stripSector, layerIsV, stripIndex );

   if( (UInt_t)stripIndex > mStripDataVec.size() ){
      LOG_ERROR << "Asked for invalid strip: " << endm;
      LOG_ERROR << "\tGeoId " << stripGeoId << ", sector " << stripSector << " layer " << ( layerIsV ? 'V' : 'U' ) << ", index " << stripIndex << endm;
      LOG_ERROR << "\tStrip end point data currently loaded for " << mStripDataVec.size() << " strips." << endm;
      x1 = y1 = x2 = y2 = 0;
      return;
   };

   // find the correct strip
   StEEmcStripEndPointDataVec_t::const_iterator stripDataSetIter = mStripDataVec.begin();
   std::advance( stripDataSetIter, stripIndex );

   // rotate the point into the correct coordinate system
   // note: negative compared to other rotations in this class
   Float_t dphiU = -(stripSector * TWO_PI_OVER_TWELVE - PI_OVER_TWELVE);

   Float_t x1_ = stripDataSetIter->getX();
   Float_t y1_ = stripDataSetIter->getY1();
   Float_t x2_ = stripDataSetIter->getX();
   Float_t y2_ = stripDataSetIter->getY2();

   Float_t frac = stripGeoId - (Int_t)(stripGeoId);
   if( frac ){
      ++stripDataSetIter;
      if( stripDataSetIter != mStripDataVec.end() ){
         Float_t x1b_ = stripDataSetIter->getX();
         Float_t y1b_ = stripDataSetIter->getY1();
         Float_t x2b_ = stripDataSetIter->getX();
         Float_t y2b_ = stripDataSetIter->getY2();

         x1_ += frac*(x1b_ - x1_);
         y1_ += frac*(y1b_ - y1_);
         x2_ += frac*(x2b_ - x2_);
         y2_ += frac*(y2b_ - y2_);
      };
   };

   if( layerIsV ){
      Float_t temp = x1_;
      x1_ = y1_;
      y1_ = temp;

      temp = x2_;
      x2_ = y2_;
      y2_ = temp;
   };

   x1 = x1_*cos( dphiU ) - y1_*sin( dphiU );
   y1 = y1_*cos( dphiU ) + x1_*sin( dphiU );

   x2 = x2_*cos( dphiU ) - y2_*sin( dphiU );
   y2 = y2_*cos( dphiU ) + x2_*sin( dphiU );

};

Float_t StEEmcPointMap_t::getStripDCA( Float_t pointX, Float_t pointY, Short_t stripGeoId ) const {
   //
   // THE STRIP
   //

   // geo qualifers for the strip
   Short_t stripSector, stripIndex;
   Bool_t layerIsV;

   // decode geo id
   StEEmcGeoId_t::decodeSmd( stripGeoId, stripSector, layerIsV, stripIndex );

   // find the correct strip
   StEEmcStripEndPointDataVec_t::const_iterator stripDataSetIter = mStripDataVec.begin();
   std::advance( stripDataSetIter, stripIndex );

   //
   // THE POINT
   //

   // rotate the point into the correct coordinate system
   Float_t dphiU = stripSector * TWO_PI_OVER_TWELVE - PI_OVER_TWELVE;
   Float_t pointXtrans = pointX*cos( dphiU ) - pointY*sin( dphiU );
   Float_t pointYtrans = pointY*cos( dphiU ) + pointX*sin( dphiU );

   if( layerIsV ){
      Float_t temp = pointXtrans;
      pointXtrans = pointYtrans;
      pointYtrans = temp;
   };

   //
   // THE DCA
   //
   Float_t dX = 0;
   Float_t dY = 0;

   if( pointYtrans > stripDataSetIter->getY2() ){
      // is above, so get DCA to top endpoint
      dX = pointXtrans - stripDataSetIter->getX();
      dY = pointYtrans - stripDataSetIter->getY2();

   } else if( pointYtrans < stripDataSetIter->getY1() ){
      // is below, so DCA to bottom endpoint
      dX = pointXtrans - stripDataSetIter->getX();
      dY = pointYtrans - stripDataSetIter->getY1();

   } else {
      // In between, so compute dca to the line.
      // But line is verticle, so distance is just the x distance
      dX = pointXtrans - stripDataSetIter->getX();
      dY = 0;
   };

   // prepare to check for special edge case
   Bool_t isEdge = ( !layerIsV && (stripSector == 2 || stripSector == 8 )) || (layerIsV && (stripSector == 3 || stripSector == 9 ));
   Float_t phi = atan2( pointYtrans, pointXtrans );

   // check
   if( isEdge && phi < TWO_PI_OVER_TWELVE*EDGE_FACTOR ){
      Float_t altY1 = stripDataSetIter->getX()*tan( TWO_PI_OVER_TWELVE*EDGE_FACTOR );

      // check whether point is below the between-sector edge, or
      // if is inside the center edge.  If is below the
      // between-sector edge, use alternate cut-off.
      if( altY1 > stripDataSetIter->getY1() )
         dY = pointYtrans-altY1;
   };

   return sqrt( dX*dX + dY*dY ); 
};


Float_t StEEmcPointMap_t::getTowerDCA( Float_t x, Float_t y, Short_t towerGeoId ) const {
   Short_t sector;
   Short_t subsector;
   Short_t etaBin;

   StEEmcGeoId_t::decodeTow( towerGeoId, sector, subsector, etaBin );

   TVector3 towerCenter = EEmcGeomSimple::Instance().getTowerCenter( sector, subsector, etaBin );

   Float_t dX = x - towerCenter.X();
   Float_t dY = y - towerCenter.Y();

   return sqrt( dX*dX + dY*dY );
};

// r is a max distance to search
Short_t StEEmcPointMap_t::getStripsNearestPoint( Float_t x, Float_t y, Float_t r ) const{
   Short_t geoId = -1;
   Float_t dist = 0.5;
   std::set< Short_t > stripSet;

   for( ; dist < r && geoId < 0; dist += 0.5 ){
      getStripsNearPoint( x, y, dist, stripSet );

      if( stripSet.size() == 1 ){
         geoId = *(stripSet.begin());
      } else if (!stripSet.empty() ){
         Float_t smallestDist = 2*r;
         Short_t geoIdOfSmallest = -1;

         std::set< Short_t >::iterator iter;
         for( iter = stripSet.begin(); iter != stripSet.end(); ++iter ){
            Float_t thisDist = getStripDCA( x, y, *iter );
            if( thisDist < smallestDist ){
               smallestDist = thisDist;
               geoIdOfSmallest = *iter;
            };
         };
         geoId = geoIdOfSmallest;
      };
   };

   return geoId;
};


void StEEmcPointMap_t::getStripsNearPoint( Float_t x, Float_t y, Float_t r, std::set< Short_t >& stripSet ) const {

   Float_t phi = atan2( y, x );

   // note: 2.5 is the number of sectors between the beginning of sector 0 and phi = 0
   Float_t sectorFrac = ( 2.5 - phi/TWO_PI_OVER_TWELVE );
   Short_t sector = (Short_t)sectorFrac;

   // need to round the other way if negative
   if( sectorFrac <= 0 )
      --sector;

   sector %= kEEmcNumSectors;
   if( sector < 0 )
      sector += kEEmcNumSectors;

   // rotate x,y into correct area to compare with map
   Float_t dphiU = sector * TWO_PI_OVER_TWELVE - PI_OVER_TWELVE;
   Float_t x2 = x*cos( dphiU ) - y*sin( dphiU );
   Float_t y2 = y*cos( dphiU ) + x*sin( dphiU );

   // add contributions from regular sector
   addSmdContribution( x2, y2, r, sector, 'U', 0, stripSet );
   addSmdContribution( x2, y2, r, sector, 'V', 0, stripSet );

   if( mCrossSectorBoundaries ){
      // convention is as follows, e.g.: a point in sector 5 is to the
      // positive side of sector 6 and the negative side of sector 4.

      // place point just to the left (increasing phi) of the sector
      Float_t x3 = x2*cos( TWO_PI_OVER_TWELVE ) - y2*sin( TWO_PI_OVER_TWELVE );
      Float_t y3 = y2*cos( TWO_PI_OVER_TWELVE ) + x2*sin( TWO_PI_OVER_TWELVE );

      // add contributions
      Short_t newSector = (sector + 1) % kEEmcNumSectors;
      addSmdContribution( x3, y3, r, newSector, 'U', 1, stripSet );
      addSmdContribution( x3, y3, r, newSector, 'V', 1, stripSet );

      // place point just to the right (decreasing phi) of the sector
      Float_t x4 = x2*cos( -TWO_PI_OVER_TWELVE ) - y2*sin( -TWO_PI_OVER_TWELVE );
      Float_t y4 = y2*cos( -TWO_PI_OVER_TWELVE ) + x2*sin( -TWO_PI_OVER_TWELVE );

      // add contributions.  Note: modulo doesn't fix the sign, so add
      // in an extra factor of kEEmcNumSectors.
      newSector = (sector - 1 + kEEmcNumSectors) % kEEmcNumSectors;
      addSmdContribution( x4, y4, r, newSector, 'U', -1, stripSet );
      addSmdContribution( x4, y4, r, newSector, 'V', -1, stripSet );
   };
};


// Sector corresponse to the sector of the specific strips in
// question, not the sector of the x, y position.  Sector side:
// defines if point is to the left (larger phi, sectorSide == 1 ), to
// the right ( smaller phi, sectorSide == -1 ), or within the sector
// (sectorSide == 0).
void StEEmcPointMap_t::addSmdContribution( Float_t x, Float_t y, Float_t r, Short_t sector, Char_t layer, Short_t sectorSide, std::set< Short_t >& stripSet ) const {
   std::set< Short_t > stripIdxSet;
   std::set< Short_t >::iterator stripIdxSetIter;


   // flip if this is a V layer
   if( layer == 'V' ){
      Double_t temp = x;
      x = y;
      y = temp;
   };

   StEEmcStripEndPointDataVec_t::const_iterator low_end = std::lower_bound( mStripDataVec.begin(), mStripDataVec.end(), StEEmcStripEndPointData_t( 0, x-r, 0, 0 ) );
   StEEmcStripEndPointDataVec_t::const_iterator up_end = std::upper_bound( mStripDataVec.begin(), mStripDataVec.end(), StEEmcStripEndPointData_t( 0, x+r, 0, 0 ) );
   StEEmcStripEndPointDataVec_t::const_iterator stripDataSetIter;

   Bool_t isEdge = (layer == 'U' && (sector == 2 || sector == 8 )) || (layer == 'V' && (sector == 3 || sector == 9 ));

   for( ; low_end != up_end; ++low_end )
      stripIdxSet.insert( low_end->getStripIndex() );

   // erase last strip from, as no fiber
   stripIdxSet.erase( kEEmcNumStrips-1 );

   // remove more strips if one of the shorter sectors 
   if( isEdge ) {
      stripIdxSetIter = stripIdxSet.lower_bound( kEEmcNumEdgeStrips-3 );

      if( stripIdxSetIter != stripIdxSet.end() )
         stripIdxSet.erase( stripIdxSetIter, stripIdxSet.end() );
   };

   // compute once here, rather than many times later
   Float_t thresRsquared = r*r;
   //Float_t pointRsquared = x*x + y*y;
   Float_t phi = atan2( y, x );

   // now add strips to overall set
   for( stripIdxSetIter = stripIdxSet.begin(); stripIdxSetIter != stripIdxSet.end(); ++stripIdxSetIter ){

      // always check actual dca: if inside, but near the edge, could
      // otherwise let through dca > r

      // get pointer to the strip
      stripDataSetIter = mStripDataVec.begin();
      std::advance( stripDataSetIter, *stripIdxSetIter );

      // compute distance
      Float_t xdist = x - stripDataSetIter->getX();
      Float_t ydist = 0;
      if( y < stripDataSetIter->getY1() )
         ydist = y - stripDataSetIter->getY1();
      if( y > stripDataSetIter->getY2() )
         ydist = y - stripDataSetIter->getY2();

      // special case: sectors would normally overlap boundary, but do not

      if( isEdge && phi < TWO_PI_OVER_TWELVE*EDGE_FACTOR ){
         Float_t altY1 = stripDataSetIter->getX()*tan( TWO_PI_OVER_TWELVE*EDGE_FACTOR );

         // check whether point is below the between-sector edge, or
         // if is inside the center edge.  If is below the
         // between-sector edge, use alternate cut-off.  For alternate
         // cut-off, do not actually have the endpoint in this case,
         // but 30.75 degrees ( = 1.025 * pi/6 rad ) is a pretty good
         // guess
         if( altY1 > stripDataSetIter->getY1() )
            ydist = y-altY1;
      };

      Float_t dist_sq = xdist*xdist + ydist*ydist;

      if( dist_sq < thresRsquared ){
         stripSet.insert( StEEmcGeoId_t::encodeSmd( sector, (layer == 'V'), *stripIdxSetIter ) );

         stripDataSetIter = mStripDataVec.begin();
         std::advance( stripDataSetIter, *stripIdxSetIter );

      };
   };
};

void StEEmcPointMap_t::getTowersNearPoint( Float_t x, Float_t y, Float_t r, std::set< Short_t >& towerSet ){
   Int_t ierr = kStOK;

   Float_t phi = atan2( y, x );

   Float_t rOfXY = sqrt( x*x + y*y );

   if( r > rOfXY ){
      LOG_ERROR << "Invalid point, distance combination: (" << x << ", " << y << "), " << r << endm;
      ierr = kStErr;
   };

   if( !ierr ){
      Float_t dphi = asin( r/rOfXY );

      Short_t kNumPhiBins = kEEmcNumSectors*kEEmcNumSubSectors;

      // note: 12.5 is the number of phi bins between the beginning of sector 0 and phi = 0
      Float_t phiFrac1 = ( 12.5 - (phi+dphi)/TWO_PI_OVER_TWELVE*kEEmcNumSubSectors );
      Short_t phiBin1 = (Short_t)phiFrac1;

      // need to round the other way if negative
      if( phiFrac1 <= 0 )
         --phiBin1;

      // take care of negative as well as too large
      phiBin1 += kNumPhiBins;
      phiBin1 %= kNumPhiBins;

      Float_t phiFrac2 = ( 12.5 - (phi-dphi)/TWO_PI_OVER_TWELVE*kEEmcNumSubSectors );
      Short_t phiBin2 = (Short_t)phiFrac2;

      // need to round the other way if negative
      if( phiFrac2 <= 0 )
         --phiBin2;

      // take care of negative as well as too large
      phiBin2 += kNumPhiBins;
      phiBin2 %= kNumPhiBins;

      if( phiBin2 < phiBin1 )
         phiBin2 += kNumPhiBins;

      // assume point is at the z position of the SMD
      Float_t minTheta = atan2( rOfXY - r, kEEmcZSMD );
      Float_t maxTheta = atan2( rOfXY + r, kEEmcZSMD );

      Float_t minEta = - log( tan( maxTheta / 2 ));
      Float_t maxEta = - log( tan( minTheta / 2 ));

      const Float_t *etaBinRange = EEmcGeomSimple::Instance().getEtaBinRangeArray();
      Short_t minEtaBin = 0;
      Short_t maxEtaBin = kEEmcNumEtas-1;

      if( minEta > etaBinRange[ kEEmcNumEtas ] && maxEta < etaBinRange[ 0 ] ){

         // note: eta bins are in decreasing eta, so need to check maxEta
         // to find min eta bin
         for( ; minEtaBin <= kEEmcNumEtas+1 && etaBinRange[ minEtaBin ] > maxEta; ++minEtaBin );

         // minEtaBin set to first one that failed, so decrease if possible
         if( minEtaBin )
            --minEtaBin;

         for( maxEtaBin = minEtaBin; maxEtaBin <= kEEmcNumEtas+1 && etaBinRange[ maxEtaBin ] > minEta; ++maxEtaBin );

         // maxEtaBin set to first one that failed, so decrease if possible
         if( maxEtaBin )
            --maxEtaBin;


         // Now have the valid range of eta bins and phi bins, loop over these bins and convert to tower Ids
         for( Short_t etaBin = minEtaBin; etaBin <= maxEtaBin; ++etaBin )
            for( Short_t phiBin = phiBin1; phiBin <= phiBin2; ++phiBin )
               towerSet.insert( StEEmcGeoId_t::encodeTow( phiBin%kNumPhiBins, etaBin ) );
      };
   };
};


Short_t StEEmcPointMap_t::getTowerNearestPoint( Float_t x, Float_t y ){
   Short_t towIdx = -1;

   Float_t phi = atan2( y, x );
   Float_t rOfXY = sqrt( x*x + y*y );

   Short_t kNumPhiBins = kEEmcNumSectors*kEEmcNumSubSectors;

   // note: 12.5 is the number of phi bins between the beginning of sector 0 and phi = 0
   Float_t phiFrac = ( 12.5 - phi/TWO_PI_OVER_TWELVE*kEEmcNumSubSectors );
   Short_t phiBin = (Short_t)phiFrac;

   // need to round the other way if negative
   if( phiFrac <= 0 )
      --phiBin;

   // take care of negative as well as too large
   phiBin += kNumPhiBins;
   phiBin %= kNumPhiBins;

   // assume point is at the z position of the SMD
   Float_t theta = atan2( rOfXY, kEEmcZSMD );
   Float_t eta = - log( tan( theta / 2 ));

   const Float_t *etaBinRange = EEmcGeomSimple::Instance().getEtaBinRangeArray();
   Short_t etaBin = 0;

   if( eta > etaBinRange[ kEEmcNumEtas ] && eta < etaBinRange[ 0 ] ){

      // note: eta bins are in decreasing eta
      for( ; etaBin < kEEmcNumEtas && eta < etaBinRange[ etaBin+1 ]; ++etaBin );

      towIdx = StEEmcGeoId_t::encodeTow( phiBin%kNumPhiBins, etaBin );
   };

   return towIdx;
};

Short_t StEEmcPointMap_t::getSectorOfPoint( Float_t x, Float_t y ){
   Float_t phi = atan2( y, x );

   Float_t sectorFrac = ( 2.5 - phi/TWO_PI_OVER_TWELVE );
   Short_t sector = (Short_t)sectorFrac;

   // need to round the other way if negative
   if( sectorFrac <= 0 )
      --sector;

   sector %= kEEmcNumSectors;
   if( sector < 0 )
      sector += kEEmcNumSectors;

   return sector;
};

void StEEmcPointMap_t::convertStripUVtoXY( Short_t sector, Float_t u, Float_t v, Float_t& x, Float_t& y ) {

   // 'float' geoId's have a fractional strip position
   // just add the fraction on to the end
   Float_t uGeoId = StEEmcGeoId_t::encodeSmd( sector, 0, (Int_t)(u) ) + u - (Int_t)(u);
   Float_t vGeoId = StEEmcGeoId_t::encodeSmd( sector, 1, (Int_t)(v) ) + v - (Int_t)(v);

   // get equations for the lines
   Float_t uA, uB, vA, vB;
   getStripLineParam( uGeoId, uA, uB );
   getStripLineParam( vGeoId, vA, vB );

   // get intersection
   x = (uB - vB)/(vA - uA);
   y = uA*x + uB;
};

StEEmcPointMap_t& StEEmcPointMap_t::instance(){
   static StEEmcPointMap_t pointmap;
   return pointmap;
};

// the static data member
const StEEmcStripEndPointDataVec_t StEEmcPointMap_t::mStripDataVec;

// constructor
StEEmcPointMap_t::StEEmcPointMap_t( Bool_t crossSectorBoundaries ) : mCrossSectorBoundaries( crossSectorBoundaries ) {
   loadData();
};

// load the static data
void StEEmcPointMap_t::loadData(){ 
   if( mStripDataVec.empty() ){
      // remove constness so it can be filled
      StEEmcStripEndPointDataVec_t *stripDataSetPtr = const_cast< StEEmcStripEndPointDataVec_t* >( &mStripDataVec );

      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 0,   41.3655, 65.4309, 67.7721 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 1,   41.8705, 65.1089, 68.6468 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 2,   42.3755, 64.7814, 69.5215 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 3,   42.8805, 64.4482, 70.3962 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 4,   43.3855, 64.1093, 71.2709 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 5,   43.8905, 63.7647, 72.1456 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 6,   44.3955, 63.4141, 73.0203 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 7,   44.9005, 63.0575, 73.8950 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 8,   45.4055, 62.6949, 74.7696 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 9,   45.9105, 62.3260, 75.6443 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 10,  46.4155, 61.9509, 76.5190 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 11,  46.9205, 61.5693, 77.3937 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 12,  47.4255, 61.1811, 78.2684 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 13,  47.9305, 60.7863, 79.1431 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 14,  48.4355, 60.3847, 80.0177 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 15,  48.9405, 59.9761, 80.8924 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 16,  49.4455, 59.5605, 81.7671 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 17,  49.9505, 59.1376, 82.6418 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 18,  50.4555, 58.7073, 83.5165 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 19,  50.9605, 58.2695, 84.3912 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 20,  51.4655, 57.8240, 85.2659 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 21,  51.9705, 57.3705, 86.1405 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 22,  52.4755, 56.9090, 87.0152 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 23,  52.9805, 56.4391, 87.8899 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 24,  53.4855, 55.9608, 88.7646 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 25,  53.9905, 55.4737, 89.6393 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 26,  54.4955, 54.9777, 90.5140 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 27,  55.0005, 54.4725, 91.3887 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 28,  55.5055, 53.9578, 92.2633 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 29,  56.0105, 53.4334, 93.1380 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 30,  56.5155, 52.8990, 94.0127 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 31,  57.0205, 52.3543, 94.8874 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 32,  57.5255, 51.7989, 95.7621 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 33,  58.0305, 51.2325, 96.6368 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 34,  58.5355, 50.6548, 97.5115 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 35,  59.0405, 50.0652, 98.3861 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 36,  59.5455, 49.4635, 99.2608 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 37,  60.0505, 48.8492, 100.1355 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 38,  60.5555, 48.2218, 101.0102 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 39,  61.0605, 47.5807, 101.8849 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 40,  61.5655, 46.9254, 102.7596 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 41,  62.0705, 46.2554, 103.6343 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 42,  62.5755, 45.5699, 104.5089 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 43,  63.0805, 44.8682, 105.3836 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 44,  63.5855, 44.1497, 106.2583 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 45,  64.0905, 43.4133, 107.1330 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 46,  64.5955, 42.6583, 108.0077 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 47,  65.1005, 41.8836, 108.8824 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 48,  65.6055, 41.0880, 109.7571 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 49,  66.1105, 40.2705, 110.6317 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 50,  66.6155, 39.4295, 111.5064 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 51,  67.1205, 38.5635, 112.3811 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 52,  67.6255, 37.6709, 113.2558 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 53,  68.1305, 36.7497, 114.1305 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 54,  68.6355, 35.7977, 115.0052 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 55,  69.1405, 35.3711, 115.8799 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 56,  69.6455, 35.6626, 116.7545 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 57,  70.1505, 35.9542, 117.6292 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 58,  70.6555, 36.2458, 118.5039 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 59,  71.1605, 36.5373, 119.3786 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 60,  71.6655, 36.8289, 120.2533 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 61,  72.1705, 37.1204, 121.1280 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 62,  72.6755, 37.4120, 122.0026 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 63,  73.1805, 37.7036, 122.8773 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 64,  73.6855, 37.9951, 123.7520 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 65,  74.1905, 38.2867, 124.6267 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 66,  74.6955, 38.5783, 125.5014 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 67,  75.2005, 38.8698, 126.3761 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 68,  75.7055, 39.1614, 127.2508 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 69,  76.2105, 39.4529, 128.1255 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 70,  76.7155, 39.7445, 129.0001 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 71,  77.2205, 40.0361, 129.8748 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 72,  77.7255, 40.3276, 130.7495 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 73,  78.2305, 40.6192, 131.6242 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 74,  78.7355, 40.9108, 132.4989 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 75,  79.2405, 41.2023, 133.3736 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 76,  79.7455, 41.4939, 134.2482 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 77,  80.2505, 41.7854, 135.1229 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 78,  80.7555, 42.0770, 135.9976 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 79,  81.2605, 42.3686, 136.8723 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 80,  81.7655, 42.6601, 137.7470 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 81,  82.2705, 42.9517, 138.6217 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 82,  82.7755, 43.2432, 139.4964 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 83,  83.2805, 43.5348, 140.3711 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 84,  83.7855, 43.8264, 141.2457 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 85,  84.2905, 44.1179, 142.1204 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 86,  84.7955, 44.4095, 142.9951 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 87,  85.3005, 44.7011, 143.8698 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 88,  85.8055, 44.9926, 144.7445 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 89,  86.3105, 45.2842, 145.6192 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 90,  86.8155, 45.5757, 146.4939 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 91,  87.3205, 45.8673, 147.3685 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 92,  87.8255, 46.1589, 148.2432 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 93,  88.3305, 46.4504, 149.1179 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 94,  88.8355, 46.7420, 149.9926 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 95,  89.3405, 47.0336, 150.8673 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 96,  89.8455, 47.3251, 151.7420 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 97,  90.3505, 47.6167, 152.6167 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 98,  90.8555, 47.9082, 153.4913 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 99,  91.3605, 48.1998, 154.3660 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 100, 91.8655, 48.4914, 155.2407 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 101, 92.3705, 48.7829, 156.1154 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 102, 92.8755, 49.0745, 156.9901 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 103, 93.3805, 49.3660, 157.8648 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 104, 93.8855, 49.6576, 158.7394 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 105, 94.3905, 49.9492, 159.6141 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 106, 94.8955, 50.2407, 160.4888 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 107, 95.4005, 50.5323, 161.3635 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 108, 95.9055, 50.8239, 162.2382 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 109, 96.4105, 51.1154, 163.1129 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 110, 96.9155, 51.4070, 163.9876 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 111, 97.4205, 51.6985, 164.8623 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 112, 97.9255, 51.9901, 165.7369 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 113, 98.4305, 52.2817, 166.6116 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 114, 98.9355, 52.5732, 167.4863 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 115, 99.4405, 52.8648, 168.3610 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 116, 99.9455, 53.1564, 169.2357 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 117, 100.4505, 53.4479, 170.1104 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 118, 100.9555, 53.7395, 170.9850 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 119, 101.4605, 54.0310, 171.8597 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 120, 101.9655, 54.3226, 172.7344 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 121, 102.4705, 54.6142, 173.6091 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 122, 102.9755, 54.9057, 174.4838 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 123, 103.4805, 55.1973, 175.3585 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 124, 103.9855, 55.4888, 176.2332 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 125, 104.4905, 55.7804, 177.1078 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 126, 104.9955, 56.0720, 177.9825 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 127, 105.5005, 56.3635, 178.8572 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 128, 106.0055, 56.6551, 179.7319 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 129, 106.5105, 56.9467, 180.6066 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 130, 107.0155, 57.2382, 181.4813 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 131, 107.5205, 57.5298, 182.3560 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 132, 108.0255, 57.8213, 183.2307 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 133, 108.5305, 58.1129, 184.3468 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 134, 109.0355, 58.4045, 184.0486 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 135, 109.5405, 58.6960, 183.7485 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 136, 110.0455, 58.9876, 183.4465 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 137, 110.5505, 59.2791, 183.1426 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 138, 111.0555, 59.5707, 182.8368 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 139, 111.5605, 59.8623, 182.5291 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 140, 112.0655, 60.1538, 182.2195 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 141, 112.5705, 60.4454, 181.9079 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 142, 113.0755, 60.7370, 181.5945 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 143, 113.5805, 61.0285, 181.2790 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 144, 114.0855, 61.3201, 180.9616 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 145, 114.5905, 61.6116, 180.6423 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 146, 115.0955, 61.9032, 180.3210 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 147, 115.6005, 62.1948, 179.9976 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 148, 116.1055, 62.4863, 179.6723 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 149, 116.6105, 62.7779, 179.3450 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 150, 117.1155, 63.0695, 179.0156 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 151, 117.6205, 63.3610, 178.6842 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 152, 118.1255, 63.6526, 178.3507 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 153, 118.6305, 63.9441, 178.0152 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 154, 119.1355, 64.2357, 177.6777 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 155, 119.6405, 64.5273, 177.3380 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 156, 120.1455, 64.8188, 176.9963 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 157, 120.6505, 65.1104, 176.6524 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 158, 121.1555, 65.4020, 176.3065 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 159, 121.6605, 65.6935, 175.9583 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 160, 122.1655, 65.9851, 175.6081 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 161, 122.6705, 66.2766, 175.2557 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 162, 123.1755, 66.5682, 174.9012 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 163, 123.6805, 66.8598, 174.5444 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 164, 124.1855, 67.1513, 174.1855 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 165, 124.6905, 67.4429, 173.8243 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 166, 125.1955, 67.7344, 173.4610 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 167, 125.7005, 68.0260, 173.0954 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 168, 126.2055, 68.3176, 172.7275 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 169, 126.7105, 68.6091, 172.3574 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 170, 127.2155, 68.9007, 171.9850 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 171, 127.7205, 69.1923, 171.6103 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 172, 128.2255, 69.4838, 171.2333 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 173, 128.7305, 69.7754, 170.8540 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 174, 129.2355, 70.0669, 170.4723 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 175, 129.7405, 70.3585, 170.0883 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 176, 130.2455, 70.6501, 169.7019 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 177, 130.7505, 70.9416, 169.3131 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 178, 131.2555, 71.2332, 168.9219 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 179, 131.7605, 71.6419, 168.5283 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 180, 132.2655, 72.5166, 168.1323 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 181, 132.7705, 73.3913, 167.7337 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 182, 133.2755, 74.2659, 167.3328 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 183, 133.7805, 75.1406, 166.9293 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 184, 134.2855, 76.0153, 166.5233 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 185, 134.7905, 76.8900, 166.1149 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 186, 135.2955, 77.7647, 165.7038 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 187, 135.8005, 78.6394, 165.2902 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 188, 136.3055, 79.5140, 164.8740 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 189, 136.8105, 80.3887, 164.4552 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 190, 137.3155, 81.2634, 164.0338 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 191, 137.8205, 82.1381, 163.6097 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 192, 138.3255, 82.7200, 163.1830 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 193, 138.8305, 83.3347, 162.7535 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 194, 139.3355, 83.7504, 162.3214 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 195, 139.8405, 84.0118, 161.8866 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 196, 140.3455, 84.1096, 161.4489 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 197, 140.8505, 83.9558, 161.0086 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 198, 141.3555, 77.0644, 160.5654 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 199, 141.8605, 77.3560, 160.1194 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 200, 142.3655, 77.6475, 159.6706 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 201, 142.8705, 77.9391, 159.2188 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 202, 143.3755, 78.2307, 158.7643 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 203, 143.8805, 78.5222, 158.3067 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 204, 144.3855, 78.8138, 157.8463 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 205, 144.8905, 79.1054, 157.3829 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 206, 145.3955, 79.3969, 156.9164 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 207, 145.9005, 79.6885, 156.4470 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 208, 146.4055, 79.9800, 155.9745 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 209, 146.9105, 80.2716, 155.4989 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 210, 147.4155, 80.5632, 155.0203 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 211, 147.9205, 80.8547, 154.5385 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 212, 148.4255, 81.1463, 154.0535 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 213, 148.9305, 81.4379, 153.5654 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 214, 149.4355, 81.7294, 153.0740 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 215, 149.9405, 82.0210, 152.5794 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 216, 150.4455, 82.3125, 152.0815 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 217, 150.9505, 82.6041, 151.5802 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 218, 151.4555, 82.8957, 151.0757 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 219, 151.9605, 83.1872, 150.5677 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 220, 152.4655, 83.4788, 150.0563 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 221, 152.9705, 83.7703, 149.5415 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 222, 153.4755, 84.0619, 149.0231 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 223, 153.9805, 84.3535, 148.5013 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 224, 154.4855, 84.6450, 147.9758 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 225, 154.9905, 84.9366, 147.4468 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 226, 155.4955, 85.2282, 146.9141 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 227, 156.0005, 85.5197, 146.3778 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 228, 156.5055, 85.8113, 145.8377 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 229, 157.0105, 86.1028, 145.2939 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 230, 157.5155, 86.3944, 144.7463 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 231, 158.0205, 86.6860, 144.1948 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 232, 158.5255, 86.9775, 143.6394 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 233, 159.0305, 87.2691, 143.0801 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 234, 159.5355, 87.5607, 142.5168 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 235, 160.0405, 87.8522, 141.9495 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 236, 160.5455, 88.1438, 141.3781 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 237, 161.0505, 88.4353, 140.8025 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 238, 161.5555, 88.7269, 140.2228 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 239, 162.0605, 89.1230, 139.6389 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 240, 162.5655, 89.9977, 139.0506 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 241, 163.0705, 90.8724, 138.4581 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 242, 163.5755, 91.7471, 137.8611 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 243, 164.0805, 92.6218, 137.2596 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 244, 164.5855, 93.4964, 136.6537 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 245, 165.0905, 94.3711, 136.0432 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 246, 165.5955, 95.2458, 135.4280 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 247, 166.1005, 96.1205, 134.8082 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 248, 166.6055, 96.9952, 134.1836 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 249, 167.1105, 97.8699, 133.5541 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 250, 167.6155, 98.7446, 132.9198 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 251, 168.1205, 99.6192, 132.2804 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 252, 168.6255, 100.2036, 131.6361 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 253, 169.1305, 100.8238, 130.9866 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 254, 169.6355, 101.2431, 130.3319 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 255, 170.1405, 101.5078, 129.6720 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 256, 170.6455, 101.6095, 129.0067 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 257, 171.1505, 101.4636, 128.3360 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 258, 171.6555, 94.5581, 127.6597 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 259, 172.1605, 94.8497, 126.9779 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 260, 172.6655, 95.1413, 126.2903 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 261, 173.1705, 95.4328, 125.5970 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 262, 173.6755, 95.7244, 124.8977 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 263, 174.1805, 96.0159, 124.1925 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 264, 174.6855, 96.3075, 123.4812 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 265, 175.1905, 96.5991, 122.7636 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 266, 175.6955, 96.8906, 122.0398 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 267, 176.2005, 97.1822, 121.3095 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 268, 176.7055, 97.4738, 120.5727 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 269, 177.2105, 97.7653, 119.8293 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 270, 177.7155, 98.0569, 119.0790 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 271, 178.2205, 98.3484, 118.3219 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 272, 178.7255, 98.6400, 117.5577 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 273, 179.2305, 98.9316, 116.7863 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 274, 179.7355, 99.2231, 116.0076 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 275, 180.2405, 99.5147, 115.2215 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 276, 180.7455, 99.8062, 114.4276 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 277, 181.2505, 100.0978, 113.6260 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 278, 181.7555, 100.3894, 112.8165 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 279, 182.2605, 100.6809, 111.9988 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 280, 182.7655, 100.9725, 111.1728 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 281, 183.2705, 101.2641, 110.3383 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 282, 183.7755, 101.5556, 109.4951 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 283, 184.2805, 101.8472, 108.6431 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 284, 184.7855, 102.1388, 107.7819 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 285, 185.2905, 102.4303, 106.9114 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 286, 185.7955, 102.7219, 106.0314 ) ); 
      stripDataSetPtr->push_back( StEEmcStripEndPointData_t( 287, 186.3005, 103.0134, 105.1415 ) ); 

      std::sort( stripDataSetPtr->begin(), stripDataSetPtr->end() );
   };
};

ClassImp( StEEmcPointMap_t );

/*
 * $Id: StEEmcPointMap.cxx,v 1.1 2012/08/29 15:44:17 sgliske Exp $
 * $Log: StEEmcPointMap.cxx,v $
 * Revision 1.1  2012/08/29 15:44:17  sgliske
 * Moved from offline/users/sgliske/StRoot/StEEmcPool
 *
 *
 */
