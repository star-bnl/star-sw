#ifndef __EEmcSmdMap__
#define __EEmcSmdMap__

/*
 *
 * \class  EEmcSmdMap
 * \date   12/02/03
 * \author jwebb
 *
 * This class provides the range of SMD strips which fall within the
 * fiducial volume of a specified EEMC tower.  It is designed to access
 * a lookup table.  The entries in the table were determined by 
 * calculating the crossing point of all SMD U and V strip pairs.  
 * Crossing points which fell within the fiducical limits of a given
 * tower at a plane centered on the mean-z of the U and V planes
 * were used to determine the allowable range of U and V strips.
 *
 * This class is implemented as a singleton... i.e., get a pointer to
 * the class by EEmcSmdMap *map = EEmcSmdMap::instance(), instead of
 * calling the constructor.
 *
 * Usage:
 *
 * EEmcSmdMap *map = EEmcSmdMap::instance();
 * Int_t uMin,uMax,vMin,vMax;
 * map -> getRangeU( sector, subsector, etabin, uMin, uMax); // all arguements
 * map -> getRangeV( sector, subsector, etabin, vMin, vMax); // indexed from 0
 *
 * Int_t iuv;
 * map -> getRange( sector, subsector, etabin, iuv, vMin, vMax); // indexed from 0
 *
 * Limitations:
 *
 * 1. Strip ranges are approximate and should not be trusted to more
 *    than two or three strips.  This should be sufficient for most 
 *    purposes.
 *
 * 2. Only U-V planes which are nominally within a given sector are
 *    considered.  In other words, this class does not currently 
 *    provide any information on strips which overlap from adjacent
 *    sectors near the tie-rods.
 *
 */

#include <TObject.h>
#include <TString.h>

#include <vector>

#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"

//-- Structure to map strip ranges to towers --
struct EEmcStripMapItem {
  Char_t *tower;
  Int_t   uMin;   // minimum u strip, all indexed from 0
  Int_t   uMax;   // maximum u strip
  Int_t   vMin;   // minimum v strip
  Int_t   vMax;   // maximum v strip
};

//-- Maps towers to individual strips --
struct EEmcTowerMapItem {
  Int_t                nTower;    // Number of towers which match strip
  std::vector<TString> towers;    // [nTower] array of tower names, eg 01TA01
  std::vector<Int_t>   sector;    // [nTower] array of sectors
  std::vector<Int_t>   subsector; // [nTower] array of subsectors
  std::vector<Int_t>   etabin;    // [nTower] array of etabins
};



class EEmcSmdMap 
    : public TObject 
{

 public:

  EEmcSmdMap();
  ~EEmcSmdMap(){ /* nada */ };
  
  // return the single instance of this class
  static EEmcSmdMap *instance();

  ///////////////////////////////////////////////////
  //
  // Get min and max strips in each orientation which
  //   match the given tower.  All arguements are c++
  //   indices (i.e. sector runs from 0-11 not 1-12).
  //
  // Min and max returned via reference.
  //
  void getRangeU( Int_t sector,  
		  Int_t subsector,  
		  Int_t etabin,  
		  Int_t &uMin,
		  Int_t &uMax ) {
    
    uMin = mSmdMap[sector][subsector][etabin].uMin;
    uMax = mSmdMap[sector][subsector][etabin].uMax;
    
  }
  void getRangeV( Int_t sector,  
		  Int_t subsector,  
		  Int_t etabin,  
		  Int_t &vMin,
		  Int_t &vMax ) {

    vMin = mSmdMap[sector][subsector][etabin].vMin;
    vMax = mSmdMap[sector][subsector][etabin].vMax;

  }
  // Min and max returned via reference.
  //
  void getRange ( Int_t sector,  
		  Int_t subsector,  
		  Int_t etabin,  
		  Int_t iuv,
		  Int_t &Min,
		  Int_t &Max ) {
    switch(iuv) {
    case 0: return getRangeU( sector, subsector, etabin, Min, Max);
    case 1: return getRangeV( sector, subsector, etabin, Min, Max);
    default: {assert(2==3);} 
    }
  }
  //
  //////////////////////////////////////////////////

  //////////////////////////////////////////////////
  //
  // Get the number of towers which "belong" to this
  // strip.  "sec", "plane" and "strip" are counted
  // from 0.
  //
  Int_t getNTowers( Int_t sec, 
		    Int_t plane, 
		    Int_t strip ) { 
    return mTowerMap[sec][plane][strip].nTower; 
  }

  // For the specified sector, plane and strip (numbered
  // from 0), return the "nth" tower's subsector and
  // eta bin.
  void getTower( Int_t sec, Int_t plane, Int_t strip, 
		  Int_t ntow, Int_t &sub, Int_t &eta ) {
    sub = mTowerMap[sec][plane][strip].subsector[ntow];
    eta = mTowerMap[sec][plane][strip].etabin[ntow];
    return;
  }
  void getTower( Int_t sec, Int_t plane, Int_t strip, 
		  Int_t ntow, Int_t &sub, Int_t &eta, TString &name ) {
    sub  = mTowerMap[sec][plane][strip].subsector[ntow];
    eta  = mTowerMap[sec][plane][strip].etabin[ntow];
    name = mTowerMap[sec][plane][strip].towers[ntow];
    return;
  }

 private:

  static EEmcSmdMap *sInstance;
 
  EEmcStripMapItem mSmdMap[ kEEmcNumSectors ][ kEEmcNumSubSectors ][ kEEmcNumEtas ];
  EEmcTowerMapItem mTowerMap[ kEEmcNumSectors ][ kEEmcNumSmdUVs ][ kEEmcNumStrips ];

  void Init();

 protected:

  ClassDef(EEmcSmdMap,1);
 
};

#endif
