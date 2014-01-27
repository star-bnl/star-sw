/*!
 * \class StPxlRawHitCollection
 * \author Qiu Hao, March 2013
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlRawHitCollection.cxx,v 1.2 2014/01/27 02:37:25 qiuh Exp $
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
 * $Log: StPxlRawHitCollection.cxx,v $
 * Revision 1.2  2014/01/27 02:37:25  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/
#include "StPxlRawHitCollection.h"

ClassImp(StPxlRawHitCollection)

StPxlRawHitCollection::StPxlRawHitCollection() : StObject()
{
}

void StPxlRawHitCollection::addRawHit(const StPxlRawHit &rawHit)
{
   mPxlRawHitVec[rawHit.sector() - 1][rawHit.ladder() - 1][rawHit.sensor() - 1].push_back(rawHit);
}

Int_t StPxlRawHitCollection::numberOfRawHits(Int_t sector, Int_t ladder, Int_t sensor)
{
   return mPxlRawHitVec[sector - 1][ladder - 1][sensor - 1].size();
}

const StPxlRawHit *StPxlRawHitCollection::rawHit(Int_t sector, Int_t ladder, Int_t sensor, Int_t rawHitIndex) const
{
   return &mPxlRawHitVec[sector - 1][ladder - 1][sensor - 1][rawHitIndex];
}

