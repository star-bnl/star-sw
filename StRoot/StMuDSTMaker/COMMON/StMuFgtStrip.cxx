/***************************************************************************
 *
 * $Id: StMuFgtStrip.cxx,v 1.1 2012/11/15 22:27:24 sangalin Exp $
 * Author: S. Gliske, Jan. 2012
 *
 ***************************************************************************
 *
 * Description: See header file.
 *
 ***************************************************************************
 *
 * $Log: StMuFgtStrip.cxx,v $
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
#include "StRoot/StEvent/StFgtStrip.h"
#include "StFgtUtil/StFgtConsts.h"
#include <assert.h>

StMuFgtStrip::StMuFgtStrip() : mGeoId(-1), mClusterSeedType(kFgtSeedTypeNo), mCharge(-1), mChargeUncert(10000) {
   assert( kFgtNumTimeBins == kMuFgtNumTimeBins ); // fix the StFgtConsts.h file if this fails

   for( Int_t i = 0; i < kFgtNumTimeBins; ++i )
      mAdc[i] = -1;
};

StMuFgtStrip::StMuFgtStrip(const StFgtStrip& other ) :
   mGeoId( other.getGeoId() ),
   mClusterSeedType( other.getClusterSeedType() ),
   mCharge( other.getCharge() ),
   mChargeUncert( other.getChargeUncert() ){

   assert( kFgtNumTimeBins == kMuFgtNumTimeBins ); // fix the StFgtConsts.h file if this fails
   for( Int_t i = 0; i < kFgtNumTimeBins; ++i )
      mAdc[i] = other.getAdc(i);
};

StMuFgtStrip& StMuFgtStrip::operator=(const StFgtStrip& other ){
   mGeoId = other.getGeoId();
   mClusterSeedType = other.getClusterSeedType();
   mCharge = other.getCharge();
   mChargeUncert = other.getChargeUncert();

   assert( kFgtNumTimeBins == kMuFgtNumTimeBins ); // fix the StFgtConsts.h file if this fails
   for( Int_t i = 0; i < kFgtNumTimeBins; ++i )
      mAdc[i] = other.getAdc(i);
 
   return *this;
};

ClassImp(StMuFgtStrip);
