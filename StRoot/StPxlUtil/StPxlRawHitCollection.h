/*!
 * \class StPxlRawHitCollection 
 * \author Qiu Hao, March 2013
 */
/***************************************************************************
 *
 * $Id: StPxlRawHitCollection.h,v 1.5 2013/05/23 21:28:44 qiuh Exp $
 *
 * Author: Qiu Hao, March 2013
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPxlRawHitCollection.h,v $
 * Revision 1.5  2013/05/23 21:28:44  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/
#ifndef StPxlRawHitCollection_hh
#define StPxlRawHitCollection_hh

#include "StObject.h"
#include "StPxlRawHit.h"
#include "StPxlConstants.h"

class StPxlRawHitCollection : public StObject {
public:
    StPxlRawHitCollection();
    ~StPxlRawHitCollection();

    vector<StPxlRawHit*> pxlRawHitVec[nPxlSectors][nPxlLaddersPerSector][nPxlSensorsPerLadder];

    ClassDef(StPxlRawHitCollection,1)
};

#endif
