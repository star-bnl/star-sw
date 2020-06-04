/***************************************************************************
 *
 * $Id: StETofHardwareMap.cxx,v 1.3 2019/02/19 20:15:21 fseck Exp $
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
 * Revision 1.3  2019/02/19 20:15:21  fseck
 * update to allow initialization from database
 *
 * Revision 1.2  2018/07/27 14:01:57  fseck
 * small change to fix compiler warning
 *
 * Revision 1.1  2018/07/25 14:34:40  jeromel
 * First version, reviewed Raghav+Jerome
 *
 *
 **************************************************************************/
#include <fstream>

#include "StETofHardwareMap.h"
#include "StMessMgr.h"

#include "tables/St_etofElectronicsMap_Table.h"


StETofHardwareMap::StETofHardwareMap( St_etofElectronicsMap* etofElectronicsMap, unsigned int year )
{
    mYear = year;

    mOrderedPlanes.push_back( 2 );
    mOrderedPlanes.push_back( 1 );
    mOrderedPlanes.push_back( 3 );

    init( etofElectronicsMap );
}

StETofHardwareMap::StETofHardwareMap( string fileName, unsigned int year )
{
    mYear = year;

    mOrderedPlanes.push_back( 2 );
    mOrderedPlanes.push_back( 1 );
    mOrderedPlanes.push_back( 3 );

    init( fileName );
}


StETofHardwareMap::~StETofHardwareMap()
{
    /* no op */
}

void 
StETofHardwareMap::init( St_etofElectronicsMap* etofElectronicsMap ) 
{
    LOG_INFO << "StETofHardwareMap -- initializing eTOF electronics to hardware mapping via database" << endm;

    // get database table
    if( !etofElectronicsMap ) {
        LOG_ERROR << "StETofHardwareMap -- unable to get the electronics map from the database" << endm;
        return;
    }

    etofElectronicsMap_st* table = etofElectronicsMap->GetTable();

    // log for debugging
    LOG_DEBUG << "nAfcks: "    << ( int ) table->nAfcks << endm;
    LOG_DEBUG << "nChannels: " << table->nChannels      << endm;

    for( size_t i=0; i<12; i++ ) {
        LOG_DEBUG << "AFCK address: 0x" << std::hex << table->afckAddress[ i ] << std::dec << " --> sector: " << ( int ) table->sector[ i ] << endm;
    }

    for( size_t i=0; i<576; i++ ) {
        LOG_DEBUG << "channelNumber: " << table->channelNumber[ i ] << " --> geometryId: " << table->geometryId[ i ] << endm;
    }


    //fill the member variables with data
    for( size_t i=0; i<12; i++ ) {
        if( table->sector[ i ] != 0 ) {
            mAfckToSector.push_back( table->sector[ i ] );
            mAfckAddressMap[ table->afckAddress[ i ] ] = i;
        }
    }

    for( size_t i=0; i<576; i++ ) {
        mChannelNumberMap[ table->channelNumber[ i ] ] = table->geometryId[ i ];
    }

    LOG_INFO << "StETofHardwareMap -- initialization finished" << endm;    
}


void 
StETofHardwareMap::init( std::string fileName ) 
{
    LOG_INFO << "StETofHardwareMap -- initializing eTOF electronics to hardware mapping via local parameter file" << endm;

    // get local parameter file
    std::ifstream paramFile;

    if( fileName.empty() ) {
        LOG_ERROR << "StETofHardwareMap -- local file name is empty" << endm;
        return;
    }
    LOG_INFO << "StETofHardwareMap -- local file for electronics to hardware mapping: " << fileName << endm;

    paramFile.open( fileName.c_str() );

    if( !paramFile.is_open() ) {
        LOG_ERROR << "StETofHardwareMap -- unable to get the parameters from local file --> file does not exist" << endm;
        return;
    }

    unsigned int nAfcks    = 0;
    unsigned int nChannels = 0;
    vector< unsigned int > afckAddress;
    vector< unsigned int > sector;
    vector< unsigned int > channelNumber;
    vector< unsigned int > geometryId;



    if( !paramFile.eof() ) {
        paramFile >> nAfcks;
    }
    if( !paramFile.eof() ) {
        paramFile >> nChannels;
    }

    unsigned int temp;

    for( int i=0; i<12; i++ ) {
        if( !paramFile.eof() ) {
            paramFile >> std::hex >> temp;
            afckAddress.push_back( temp );
        }
    }

    for( int i=0; i<12; i++ ) {
        if( !paramFile.eof() ) {
            paramFile >> std::dec >> temp;
            sector.push_back( temp );
        }
    }
    for( int i=0; i<576; i++ ) {
        if( !paramFile.eof() ) {
            paramFile >> std::dec >> temp;
            channelNumber.push_back( temp );
        }
    }

    for( int i=0; i<576; i++ ) {
        if( !paramFile.eof() ) {
            paramFile >> std::dec >> temp;
            geometryId.push_back( temp );
        }
    }

    paramFile.close();

    if( !paramFile.eof() || nAfcks == 0 || nChannels == 0 ||
        afckAddress.size() != 12 || sector.size() != 12 ||
        channelNumber.size() != 576 || geometryId.size() != 576 )
    {
        LOG_ERROR << "StETofHardwareMap -- parameter file has not the right amount of entries !!!!" << endm;
        return;  
    }


    // logging for debug
    LOG_DEBUG << "nAfcks: "    << nAfcks    << endm;
    LOG_DEBUG << "nChannels: " << nChannels << endm;

    for( size_t i=0; i<12; i++ ) {
        LOG_DEBUG << "AFCK address: 0x" << std::hex << afckAddress[ i ] << std::dec << " --> sector: " << sector.at( i ) << endm;
    }

    for( size_t i=0; i<576; i++ ) {
        LOG_DEBUG << "channelNumber: " << channelNumber.at( i ) << " --> geometryId: " << geometryId.at( i ) << endm;
    }


    //fill the member variables with data
    for( size_t i=0; i<12; i++ ) {
        if( sector.at( i ) != 0 ) {
            mAfckToSector.push_back( sector.at( i ) );

            mAfckAddressMap[ afckAddress.at( i ) ] = i;
        }
    }

    for( size_t i=0; i<576; i++ ) {
        mChannelNumberMap[ channelNumber.at( i ) ] = geometryId.at( i );
    }

    LOG_INFO << "StETofHardwareMap -- initialization finished" << endm; 
}


void
StETofHardwareMap::mapToGeom( unsigned int rocId, unsigned int chipId, unsigned int chanId, std::vector< unsigned int >& geomVec )
{
    // clear geomVec
    geomVec.clear();

    if( mAfckAddressMap.count( rocId ) == 0 ) {
        LOG_INFO << "StETofHardwareMap::Map  - rocId is out of range: 0x" << std::hex << rocId << std::dec << endm;
        return;
    }

    unsigned int rocIndex = mAfckAddressMap.at( rocId );
    unsigned int channel  = chipId * 10 + chanId;

    if( mYear == 2018 ) {
        channel += rocIndex * 1000;
    }


    if( mChannelNumberMap.count( channel ) == 0 ) {
        LOG_INFO << "StETofHardwareMap::Map  - channel is out of range: chipId " << chipId << "  chanId " << chanId << endm;
        return;
    }


    unsigned int sector     = mAfckToSector.at( rocIndex );
    unsigned int geometryId = mChannelNumberMap.at( channel );

    if( geometryId == 0 ) {
        LOG_INFO << "chipId: " << chipId << " chanId: " << chanId << "  -> channel: " << channel << "  --> geometryId: " << geometryId << endm; 
    }

    // fill geomVec with the geometric identifiers
    geomVec.push_back( sector  );
    geomVec.push_back(   geometryId / 10000          );
    geomVec.push_back( ( geometryId % 10000 ) / 1000 );
    geomVec.push_back( ( geometryId % 1000  ) / 10   );
    geomVec.push_back(   geometryId % 10             );

    return;
}


unsigned int
StETofHardwareMap::module( unsigned int sector, unsigned int plane )
{
    return ( sector - 13 ) * 3 + mOrderedPlanes[ plane - 1 ];
}
