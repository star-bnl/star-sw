/*!
 * \class StEEmcGeoId_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * Class to convert physical coordinates into a universal identifier
 * of the type "Short_t".  The tower geoId is identical to the tower
 * number, thus for the towers this class is just repeating known
 * conversions already strewn through existing code.  The a global
 * geoId for all the ESMD strips is needed when considering the
 * portions of the SMD strips which cross the sector boundaries.
 *
 * Note: all (as of date) member functions are static, so one should
 * just use the static member functions without instanciating the
 * class.
*/

#ifndef _ST_EEMC_GEO_ID_
#define _ST_EEMC_GEO_ID_

#include <set>
#include <Rtypes.h>

#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"

// The class
class StEEmcGeoId_t {
 public:
   // deconstructor
   ~StEEmcGeoId_t() { /* */ };

   // static member functions
   static Short_t encodeSmd( Short_t sector, Bool_t layerIsV, Short_t strip );
   static void decodeSmd( Short_t geoId, Short_t& sector, Bool_t& layerIsV, Short_t& strip );
   static Int_t getMaxSmdGeoId();

   static Short_t encodeTow( Short_t sector, Short_t subsector, Short_t etaBin );
   static void decodeTow( Short_t geoId, Short_t& sector, Short_t& subsector, Short_t& etaBin );

   static Short_t encodeTow( Short_t phiBin, Short_t etaBin );
   static void decodeTow( Short_t geoId, Short_t& phiBin, Short_t& etaBin );

 protected:
   enum { kMaxSmdGeoId = kEEmcNumStrips*kEEmcNumSmdUVs*kEEmcNumSectors };

 private:
   // constructor is private, so that it cannot be constructed
   StEEmcGeoId_t(){ /* */ };

};

// inline functions

inline Short_t StEEmcGeoId_t::encodeSmd( Short_t sector, Bool_t layerIsV, Short_t strip ){
   return kEEmcNumStrips*( 2*sector + layerIsV  ) + strip;
};

inline void StEEmcGeoId_t::decodeSmd( Short_t geoId, Short_t& sector, Bool_t& layerIsV, Short_t& strip ){
   strip = geoId % kEEmcNumStrips;
   geoId /= kEEmcNumStrips;
   layerIsV = geoId % 2;
   sector = geoId / 2;
};

inline Int_t StEEmcGeoId_t::getMaxSmdGeoId(){
   return kMaxSmdGeoId;
};

inline Short_t StEEmcGeoId_t::encodeTow( Short_t sector, Short_t subsector, Short_t etaBin ){
   return kEEmcNumEtas*( kEEmcNumSubSectors*sector + subsector   ) + etaBin;
};

inline void StEEmcGeoId_t::decodeTow( Short_t geoId, Short_t& sector, Short_t& subsector, Short_t& etaBin ){
   etaBin = geoId % kEEmcNumEtas;
   geoId /= kEEmcNumEtas;
   subsector = geoId % kEEmcNumSubSectors;
   sector    = geoId / kEEmcNumSubSectors;
};

inline Short_t StEEmcGeoId_t::encodeTow( Short_t phiBin, Short_t etaBin ){
   return kEEmcNumEtas*phiBin + etaBin;
};

inline void StEEmcGeoId_t::decodeTow( Short_t geoId, Short_t& phiBin, Short_t& etaBin ){
   etaBin = geoId % kEEmcNumEtas;
   phiBin = geoId / kEEmcNumEtas;
};

#endif

/*
 * $Id: StEEmcGeoId.h,v 1.1 2012/08/29 15:44:17 sgliske Exp $
 * $Log: StEEmcGeoId.h,v $
 * Revision 1.1  2012/08/29 15:44:17  sgliske
 * Moved from offline/users/sgliske/StRoot/StEEmcPool
 *
 *
 */
