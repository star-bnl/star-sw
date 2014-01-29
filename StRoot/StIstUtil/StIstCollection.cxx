/***************************************************************************
*
* $Id: StIstCollection.cxx,v 1.2 2014/01/29 18:25:03 ypwang Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description: 
* See header file.
****************************************************************************
*
* $Log: StIstCollection.cxx,v $
* Revision 1.2  2014/01/29 18:25:03  ypwang
* updating scripts
*
*
****************************************************************************
* StIstCollection.cxx,v 1.0
* Revision 1.0 2013/11/04 15:15:30 Yaping
* Initial version
****************************************************************************/

#include "StIstConsts.h"
#include "StIstCollection.h"

// constructor
StIstCollection::StIstCollection() : StObject() {
    // set the ladder field for some of the collections
    for( unsigned char i=0; i<kIstNumLadders; ++i ){
        mRawHitCollection[i].setLadder( i );
        mClusterCollection[i].setLadder( i );
    }
    mNumTimeBins=kIstNumTimeBins; //reasonable default
}

// deconstructor
StIstCollection::~StIstCollection(){
    // nothing to do
}

void StIstCollection::Clear( Option_t *opt ){
    for( unsigned char i=0; i<kIstNumLadders; ++i ){
        mRawHitCollection[i].Clear( opt );
        mClusterCollection[i].Clear( opt );
    }
}

ClassImp(StIstCollection);
