/***************************************************************************
 *
 * $Id: StTrsRawDataEvent.hh,v 1.2 1999/02/10 04:27:36 lasiuk Exp $
 *
 * Author: bl prelim
 ***************************************************************************
 *
 * Description: Contains a complete event of raw data
 *
 ***************************************************************************
 *
 * $Log: StTrsRawDataEvent.hh,v $
 * Revision 1.2  1999/02/10 04:27:36  lasiuk
 * TObject
 *
 * Revision 1.1  1999/02/04 18:36:32  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_TRS_RAW_DATA_EVENT_HH
#define ST_TRS_RAW_DATA_EVENT_HH

#include <vector>

// FROM SCL
#include "StTpcRawDataEvent.hh"

// FROM TRS
#include "StTrsDigitalSector.hh"

class StTrsRawDataEvent : public StTpcRawDataEvent {
public:
    StTrsRawDataEvent();
    ~StTrsRawDataEvent();
    
    //StTrsRawDataEvent(const StTrsRawDataEvent&);
    //StTrsRawDataEvent& operator=(const StTrsRawDataEvent&);
    
    unsigned long size();

public:
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<StTrsDigitalSector*> mSectors;
#else
    vector<StTrsDigitalSector*, allocator<StTrsDigitalSector*> > mSectors;
#endif
};

#endif
