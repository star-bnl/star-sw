/***************************************************************************
 *
 * $Id: StETofHardwareMap.h,v 1.2 2018/07/27 14:01:53 fseck Exp $
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
 * $Log: StETofHardwareMap.h,v $
 * Revision 1.2  2018/07/27 14:01:53  fseck
 * small change to fix compiler warning
 *
 * Revision 1.1  2018/07/25 14:34:40  jeromel
 * First version, reviewed Raghav+Jerome
 *
 *
 **************************************************************************/
#ifndef STETOFHARDWAREMAP_H
#define STETOFHARDWAREMAP_H

#include <vector>
#include <map>

#include "TObject.h"

class StETofHardwareMap : public TObject {

public:
    StETofHardwareMap();
    
    ~StETofHardwareMap();

    void          init(); 
    void          mapToGeom( unsigned int rocId, unsigned int chipId, unsigned int channelId, std::vector< unsigned int >& geomVec );
    unsigned int  module( unsigned int sector, unsigned int plane );

protected:

    std::map< unsigned int, unsigned int > mRocMap;

    std::vector< int > mGet4ToPadi;
    std::vector< int > mOrderedPlanes;

    unsigned int mNrOfChannelsPerGet4;
    unsigned int mNrOfGet4PerFeb;
    unsigned int mNrOfGet4ChipsPerRoc;
    unsigned int mNrOfChannelsPerCard;

    ClassDef( StETofHardwareMap, 1 )
};

#endif
