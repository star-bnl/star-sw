/***************************************************************************
 *
 * $Id: StETofHardwareMap.h,v 1.3 2019/02/19 20:15:14 fseck Exp $
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
 * Revision 1.3  2019/02/19 20:15:14  fseck
 * update to allow initialization from database
 *
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

#include <string>
#include <vector>
#include <map>

#include "TObject.h"

class St_etofElectronicsMap;


class StETofHardwareMap : public TObject {

public:
    StETofHardwareMap( St_etofElectronicsMap* etofElectronicsMap, unsigned int year );
    StETofHardwareMap( std::string fileName, unsigned int year );

    ~StETofHardwareMap();

    void          init( St_etofElectronicsMap* etofElectronicsMap );
    void          init( std::string fileName );

    void          mapToGeom( unsigned int rocId, unsigned int chipId, unsigned int chanId, std::vector< unsigned int >& geomVec );
    unsigned int  module( unsigned int sector, unsigned int plane );

protected:
    UInt_t mYear;

    std::vector< UInt_t > mOrderedPlanes;
    std::vector< UInt_t > mAfckToSector;

    std::map< UInt_t, UInt_t > mAfckAddressMap;
    std::map< UInt_t, UInt_t > mChannelNumberMap;


    ClassDef( StETofHardwareMap, 1 )
};

#endif