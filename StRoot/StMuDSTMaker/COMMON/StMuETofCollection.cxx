/***************************************************************************
 *
 * $Id: StMuETofCollection.cxx,v 1.1 2019/02/21 13:32:54 jdb Exp $
 *
 * Author: Florian Seck, October 2018
 ***************************************************************************
 *
 * Description: Data collection for storing eTOF information (header, digis,
 * hits) in the muDsts
 *
 ***************************************************************************
 *
 * $Log: StMuETofCollection.cxx,v $
 * Revision 1.1  2019/02/21 13:32:54  jdb
 * Inclusion of ETOF MuDst code. This code adds support for the full set of ETOF data which includes EtofDigi, EtofHit, EtofHeader. The code essentially copies similar structures from StEvent and additionally rebuilds the maps between Digis and Hits. Accessor methods are added based on the pattern from BTOF to provide access to data at various levels. The code for accessing the PID traits provided by ETOF is also provided
 *
 *
 ***************************************************************************/ 
#include <map>

#include "StMuETofCollection.h"
#include "StMuETofHeader.h"
#include "StMuETofDigi.h"
#include "StMuETofHit.h"

#include "StETofCollection.h"
#include "StETofHeader.h"
#include "StETofDigi.h"
#include "StETofHit.h"

#include "StMessMgr.h"


StMuETofCollection::StMuETofCollection()
{

}


StMuETofCollection::~StMuETofCollection()
{
    /* no op */
}

StMuETofCollection::StMuETofCollection( const StETofCollection* etofColl )
{
    // fill header 
    if( etofColl->etofHeader() ) { 
        mETofHeader.push_back( etofColl->etofHeader() );
    }
    else { 
        mETofHeader.push_back( StMuETofHeader() );
    }

    // store hits for better searchability
    //--> key: detector index, value: vector of hits indices in mETofHit vector
    std::map< int, vector< unsigned int > > mapStoreHit;

    LOG_DEBUG << "StMuETofCollection ctor -- filling hits into storage" << endm;

    //fill hits --> needed for assigning associatedHitId to digis
    if( etofColl->hitsPresent() ) {
        const StSPtrVecETofHit& vecHit = etofColl->etofHits();

        for( size_t i=0; i<vecHit.size(); i++ ){
            StETofHit* pHit = ( StETofHit* ) vecHit.at( i );

            if( !pHit ) continue;

            mETofHits.push_back( StMuETofHit( pHit ) );

            int detIndex = pHit->sector() * 100 + pHit->zPlane() * 10 + pHit->counter();

            mapStoreHit[ detIndex ].push_back( i );
        }
    }

    /*
    LOG_DEBUG << "StMuETofCollection ctor -- hit map created" << endm;
    for( auto kv: mapStoreHit ) {
        LOG_DEBUG << "detIndex :" << kv.first << "  -- hit index: ";
        for( auto v : kv.second ) {
            LOG_DEBUG << v << "  ";
        }
        LOG_DEBUG << endm;
    }
    */

    //fill digis 
    if( etofColl->digisPresent() ) {
        const StSPtrVecETofDigi& vecDigi = etofColl->etofDigis();

        for( size_t i=0; i<vecDigi.size(); i++ ) {
            StETofDigi* pDigi = ( StETofDigi* ) vecDigi.at( i );

            if( !pDigi ) continue;

            int assocHitId = -1;

            if( pDigi->associatedHit() != nullptr ) {
                int detIndex = pDigi->sector() * 100 + pDigi->zPlane() * 10 + pDigi->counter();

                double assocLocalX = pDigi->associatedHit()->localX();
                double assocLocalY = pDigi->associatedHit()->localY();
                double assocTime   = pDigi->associatedHit()->time();

                if( mapStoreHit.count( detIndex ) ) { 
                    for( auto v : mapStoreHit.at( detIndex ) ) {
                        if( fabs( mETofHits.at( v ).localX() - assocLocalX ) > 0.0001 ) continue;
                        if( fabs( mETofHits.at( v ).localY() - assocLocalY ) > 0.0001 ) continue;
                        if( fabs( mETofHits.at( v ).time()   - assocTime   ) > 0.0001 ) continue;

                        assocHitId = v;
                        break;
                    }
                }
                mETofDigis.push_back( StMuETofDigi( pDigi, assocHitId ) );
                LOG_DEBUG << "StMuETofCollection ctor -- added digi with associatedHitId = " << assocHitId << endm; 
            }
            else {
                mETofDigis.push_back( StMuETofDigi( pDigi ) );
                LOG_DEBUG << "StMuETofCollection ctor -- added digi with associatedHitId = -1" << endm;   
            }

        }
    }

}


const StMuETofHeader*
StMuETofCollection::etofHeader() const {
    return &mETofHeader.at( 0 );
}

StMuETofHeader*
StMuETofCollection::etofHeader() {
    return &mETofHeader.at( 0 );
}


StMuETofDigi*
StMuETofCollection::etofDigi( int i ) {
    return &mETofDigis.at( i );
}


StMuETofHit*
StMuETofCollection::etofHit( int i ) {
    return &mETofHits.at( i );
}


int
StMuETofCollection::digisPresent() {
    return mETofDigis.size();
}

int
StMuETofCollection::hitsPresent() {
    return mETofHits.size();
}