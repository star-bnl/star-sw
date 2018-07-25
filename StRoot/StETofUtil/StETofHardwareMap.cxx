/***************************************************************************
 *
 * $Id: StETofHardwareMap.cxx,v 1.1 2018/07/25 14:34:40 jeromel Exp $
 *
 * Author: Pengfei Lyu, April 2018
 ***************************************************************************
 *
 * Description: This class provides a mapping from the hardware address of
 * the electroinc channels to the eTOF geometry, e.g. sector, z-plane,
 * counter, strip, side 
 *
 ***************************************************************************
 *
 * $Log: StETofHardwareMap.cxx,v $
 * Revision 1.1  2018/07/25 14:34:40  jeromel
 * First version, reviewed Raghav+Jerome
 *
 *
 **************************************************************************/
#include "StETofHardwareMap.h"


StETofHardwareMap::StETofHardwareMap()
{
    init();
}



StETofHardwareMap::~StETofHardwareMap()
{
    /* no op */
}


void 
StETofHardwareMap::init() 
{
    // from BuildEtof_v18b.par
    // -----------------------
    // TODO: move electronic addresses to database
    mRocMap[ 0x18f6 ] = 0;
    mRocMap[ 0x0b59 ] = 1;
    mRocMap[ 0x1898 ] = 2;
    mRocMap[ 0x5f64 ] = 3;
    mRocMap[ 0x18e6 ] = 4;



    // initialize Get4 to PADI conversion
    // -----------------------
    // TODO: move conversion table to database
    int channelMap[32] = {
         3,  2,  1,  0, 
        23, 22, 21, 20,
         7,  6,  5,  4,
        27, 26, 25, 24,
        11, 10,  9,  8,
        31, 30, 29, 28,
        15, 14, 13, 12,
        19, 18, 17, 16 };

    for(int i=0; i<32; i++) {
        mGet4ToPadi.push_back( channelMap[i] );
    }


    // initialize ordered planes
    // -----------------------
    mOrderedPlanes.push_back( 2 );
    mOrderedPlanes.push_back( 1 );
    mOrderedPlanes.push_back( 3 );


    // initialize electronic numbers
    // -----------------------
    // TODO: move electrionic contants to database
    mNrOfChannelsPerGet4 = 4;
    mNrOfGet4PerFeb      = 8;
    mNrOfGet4ChipsPerRoc = 32;
    mNrOfChannelsPerCard = 32;
}


void
StETofHardwareMap::mapToGeom( unsigned int rocId, unsigned int chipId, unsigned int channelId, std::vector<unsigned int>& geomVec )
{
    if( mRocMap.count( rocId ) == 0 || chipId >= mNrOfGet4ChipsPerRoc || channelId >= mNrOfChannelsPerGet4 ) {
        LOG_WARN << "StETofHardwareMap::Map  - some id is out of range: rocId " << rocId;
        LOG_WARN << "  chipId " << chipId << "  channelId " << channelId << endm;
        return;
    }

    unsigned int channelNrInCard   = (chipId % mNrOfGet4PerFeb) * mNrOfChannelsPerGet4 + channelId;
    unsigned int cardNr            = (chipId / mNrOfGet4PerFeb);

    // TODO: in 2018 data only sector 18 exists. -> for next year prepare mapping of electronic address to sector as well 
    unsigned int sector = 18;

    unsigned int plane;
    unsigned int counter;

    if( cardNr / 2 == 0) {
        switch( mRocMap[ rocId ] ) {
            case 0:
                plane   = 1;
                counter = 3;
                break;
            case 1:
                plane   = 1;
                counter = 1;
                break;
            case 2:
                plane   = 2;
                counter = 3;
                break;
            case 3:
                plane   = 3;
                counter = 3;
                break;
            case 4:
                plane   = 3;
                counter = 1;
                break;
        }
    }
    else {
        switch( mRocMap[ rocId ] ) {
            case 0:
                plane   = 1;
                counter = 2;
                break;
            case 1:
                plane   = 2;
                counter = 1;
                break;
            case 2:
                plane   = 2;
                counter = 2;
                break;
            case 3:
                plane   = 3;
                counter = 2;
                break;
        }
    }

    unsigned int strip;
    unsigned int side;

    if( cardNr % 2 == 0) {
        strip = 1 + mGet4ToPadi[ channelNrInCard ];
        side  = 1;
    }
    else {
        strip = 32 - mGet4ToPadi[ channelNrInCard ];
        side  = 2;
    }

    // clear geomVec and fill it with the geometric identifiers
    geomVec.clear();

    geomVec.push_back( sector  );
    geomVec.push_back( plane   );
    geomVec.push_back( counter );
    geomVec.push_back( strip   );
    geomVec.push_back( side    );

    return;
}


unsigned int
StETofHardwareMap::module( unsigned int sector, unsigned int plane )
{
    return ( sector - 13 ) * 3 + mOrderedPlanes[ plane - 1 ];
}
