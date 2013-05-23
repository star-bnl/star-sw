/***************************************************************************
 *
 * $Id: StPxlRawHitCollection.cxx,v 1.2 2013/05/23 21:14:35 qiuh Exp $
 *
 * Author: Qiu Hao, March 2013
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPxlRawHitCollection.cxx,v $
 * Revision 1.2  2013/05/23 21:14:35  qiuh
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
                    int vectorSize = pxlRawHitVec[i][j][k].size();
                    for(int l=0; l<vectorSize; l++)
                        delete pxlRawHitVec[i][j][k][l];
                    pxlRawHitVec[i][j][k].clear();
                }

}

