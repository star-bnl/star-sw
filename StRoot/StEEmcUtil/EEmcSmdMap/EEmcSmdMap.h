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

struct EEmcStripMapItem {
  char *tower;
  Int_t uMin;   // minimum u strip, all indexed from 0
  Int_t uMax;   // maximum u strip
  Int_t vMin;   // minimum v strip
  Int_t vMax;   // maximum v strip
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
  //
  //////////////////////////////////////////////////

 private:

  static EEmcSmdMap *sInstance;
 
  EEmcStripMapItem mSmdMap[12][5][12];

  void Init();

 protected:

  ClassDef(EEmcSmdMap,1);

};

#endif
