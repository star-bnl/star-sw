/*!
 * \class StPxlRawHitCollection
 * \author Qiu Hao, March 2013
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlRawHitCollection.h,v 1.5 2017/08/28 17:05:16 dongx Exp $
 *
 * Author: Qiu Hao, March 2013
 ***************************************************************************
 *
 * Description:
 * pxl raw hit collection
 * More information at
 * https://www.star.bnl.gov/protected/heavy/qiuh/HFT/software/PXL_software.pdf
 *
 ***************************************************************************
 *
 * $Log: StPxlRawHitCollection.h,v $
 * Revision 1.5  2017/08/28 17:05:16  dongx
 * Append rawHits to StPxlRawHitCollection only if exists - for simu/embedding
 *
 * Revision 1.4  2014/08/06 11:43:35  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.3  2014/01/28 19:29:44  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/
#ifndef StPxlRawHitCollection_hh
#define StPxlRawHitCollection_hh

#include "StObject.h"
#include "StPxlRawHit.h"
#include "StPxlUtil/StPxlConstants.h"

class StPxlRawHitCollection : public StObject
{
public:
   StPxlRawHitCollection();
   void addRawHit(const StPxlRawHit &rawHit); ///< add a raw hit to the collection
   Int_t numberOfRawHits(Int_t sector, Int_t ladder, Int_t sensor); ///< number of raw hits in a sensor
   Int_t numberOfRawHits();
   const StPxlRawHit *rawHit(Int_t sector, Int_t ladder, Int_t sensor, Int_t rawHitIndex) const; ///< pionter to a rawHit in the collection
   virtual const char *GetCVS() const {
      static const char cvs[] = "Tag $Name:  $ $Id: StPxlRawHitCollection.h,v 1.5 2017/08/28 17:05:16 dongx Exp $ built " __DATE__ " " __TIME__ ;
      return cvs;
   }

protected:
   vector<StPxlRawHit> mPxlRawHitVec[kNumberOfPxlSectors][kNumberOfPxlLaddersPerSector][kNumberOfPxlSensorsPerLadder]; ///< vectors to store raw hits

   ClassDef(StPxlRawHitCollection, 1)
};

#endif
