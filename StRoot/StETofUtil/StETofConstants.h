/***************************************************************************
 *
 * $Id: StETofConstants.h,v 1.1 2019/02/19 20:12:47 fseck Exp $
 *
 * Author: Florian Seck, October 2018
 ***************************************************************************
 *
 * Description: StETofConstants - place to collect eTOF related constants
 *
 ***************************************************************************
 *
 * $Log: StETofConstants.h,v $
 * Revision 1.1  2019/02/19 20:12:47  fseck
 * initial commit
 *
 *
 ***************************************************************************/

#ifndef STETOFCONSTANTS_H
#define STETOFCONSTANTS_H


namespace eTofConst{

    // geometry related constants
    const int nSectors  = 12;
    const int nPlanes   =  3;
    const int nModules  = nSectors * nPlanes;
    const int nCounters =  3;
    const int nStrips   = 32;
    const int nSides    =  2;

    const int sectorStart  = 13;
    const int sectorStop   = 24;
    const int zPlaneStart  =  1;
    const int zPlaneStop   =  3; 
    const int counterStart =  1;
    const int counterStop  =  3; 
    const int stripStart   =  1;
    const int stripStop    = 32;
    const int sideStart    =  1;
    const int sideStop     =  2;

    const float firstStripCenter = -15.5; // [cm]
    const float stripPitch       =   1.0; // [cm]
    const float stripLength      =  27.0; // [cm]

    const float zplanes[ 3 ] = { 209.0 - 489.01, 209.0 - 501.01, 209.0 - 513.01 }; // [cm]

    // number of counters
    const int nCountersPerSector  = nCounters * nPlanes;
    const int nCountersInSystem   = nCountersPerSector * nSectors;

    // number of channels
    const int nChannelsPerCounter = nStrips * nSides;
    const int nChannelsPerModule  = nChannelsPerCounter * nCounters;
    const int nChannelsPerSector  = nChannelsPerModule  * nPlanes;
    const int nChannelsInSystem   = nChannelsPerSector  * nSectors;


    // clock related constants
    const double coarseClockCycle  = 6.25;   // [ns]  -- needs to be double otherwise strange things happen ...
    const double bTofClockCycle    = 51200.; // [ns]


    // raw daq message size
    const int daqMessageSize = 8;  // 64 bits = 8 bytes
}

#endif
