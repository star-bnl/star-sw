/***************************************************************************
 *
 * $Id: StMuFgtStrip.cxx,v 1.3 2018/01/04 17:36:47 smirnovd Exp $
 * Author: S. Gliske, Jan. 2012
 *
 ***************************************************************************
 *
 * Description: See header file.
 *
 ***************************************************************************
 *
 * $Log: StMuFgtStrip.cxx,v $
 * Revision 1.3  2018/01/04 17:36:47  smirnovd
 * [Cosmetic] Remove StRoot/ from include path
 *
 * $STAR/StRoot is already in the default path search
 *
 * Revision 1.2  2013/01/08 22:57:33  sangalin
 * Merged in FGT changes allowing for a variable number of timebins to be read out for each strip.
 *
 * Revision 1.1  2012/11/15 22:27:24  sangalin
 * Copied over from StFgtDevel.
 *
 * Revision 1.6  2012/04/13 18:56:31  sgliske
 * More adjustments based on the review:
 * - Lastest StEvents from Thomas U.
 * - StFgtA2CMaker can no longer remove strips other than bad status or bad ped
 * - other related updates
 *
 * Revision 1.5  2012/03/07 19:21:01  sgliske
 * updated based on changes to StEvent
 *
 * Revision 1.4  2012/03/07 15:23:53  sgliske
 * StFgtStrip no longer has a type field
 *
 * Revision 1.3  2012/01/30 11:40:05  sgliske
 * a2cMaker now fits the pulse shape,
 * strip containers updated
 *
 * Revision 1.2  2012/01/30 10:42:23  sgliske
 * strip containers now contain adc values for
 * all time bins.  Also fixed bug where setType modified the timebin
 * rather than the type.
 *
 * Revision 1.1  2012/01/04 19:15:34  sgliske
 * Reintroduced support for the FGT in MuDst
 *
 *
 **************************************************************************/

#include "StMuFgtStrip.h"
#include "StEvent/StFgtStrip.h"
#include "StFgtUtil/StFgtConsts.h"
#include <assert.h>

StMuFgtStrip::StMuFgtStrip() : mGeoId(-1), mAdcStartIdx(-1), mNumSavedTimeBins(-1), mClusterSeedType(kFgtSeedTypeNo), mCharge(-1), mChargeUncert(10000) {
  ///   assert( kFgtNumTimeBins == kMuFgtNumTimeBins ); // fix the StFgtConsts.h file if this fails
  //above assert not necessary anymore since we take number of time bins from the StFgtCollection, should be the same

};

StMuFgtStrip::StMuFgtStrip(const StFgtStrip& other ) :
   mGeoId( other.getGeoId() ),
   mAdcStartIdx(-1),
   mNumSavedTimeBins(-1), 
   mClusterSeedType( other.getClusterSeedType() ),
   mCharge( other.getCharge() ),
   mChargeUncert( other.getChargeUncert() ){

   assert( kFgtNumTimeBins == kMuFgtNumTimeBins ); // fix the StFgtConsts.h file if this fails
};

StMuFgtStrip& StMuFgtStrip::operator=(const StFgtStrip& other ){
   mGeoId = other.getGeoId();
   mAdcStartIdx = -1;
   mNumSavedTimeBins = -1;
   mClusterSeedType = other.getClusterSeedType();
   mCharge = other.getCharge();
   mChargeUncert = other.getChargeUncert();

   assert( kFgtNumTimeBins == kMuFgtNumTimeBins ); // fix the StFgtConsts.h file if this fails
 
   return *this;
};

ClassImp(StMuFgtStrip);
