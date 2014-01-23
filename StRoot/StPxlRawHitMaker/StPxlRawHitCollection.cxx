/*!
 * \class StPxlRawHitCollection
 * \author Qiu Hao, March 2013
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlRawHitCollection.cxx,v 1.1 2014/01/23 01:05:02 qiuh Exp $
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
 * Revision 1.1  2014/01/23 01:05:02  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/
#include "StPxlRawHitCollection.h"

ClassImp(StPxlRawHitCollection)

StPxlRawHitCollection::StPxlRawHitCollection()
{

}

StPxlRawHitCollection::~StPxlRawHitCollection()
{
    for(int i=0; i<nPxlSectors; i++)
        for(int j=0; j<nPxlLaddersPerSector; j++)
            for(int k=0; k<nPxlSensorsPerLadder; k++)
                {
                    int vectorSize = mPxlRawHitVec[i][j][k].size();
                    for(int l=0; l<vectorSize; l++)
                        {
                            delete mPxlRawHitVec[i][j][k][l];
                        }
                    mPxlRawHitVec[i][j][k].clear();
                }

}

void StPxlRawHitCollection::addRawHit(StPxlRawHit* rawHit)
{
    mPxlRawHitVec[rawHit->sector()-1][rawHit->ladder()-1][rawHit->sensor()-1].push_back(rawHit);
}

Int_t StPxlRawHitCollection::numberOfRawHits(Int_t sector, Int_t ladder, Int_t sensor)
{
    return mPxlRawHitVec[sector-1][ladder-1][sensor-1].size();
}

StPxlRawHit* StPxlRawHitCollection::rawHit(Int_t sector, Int_t ladder, Int_t sensor, Int_t rawHitIndex)
{
    return mPxlRawHitVec[sector-1][ladder-1][sensor-1][rawHitIndex];
}

