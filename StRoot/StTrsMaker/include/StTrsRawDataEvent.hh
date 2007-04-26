/***************************************************************************
 *
 * $Id: StTrsRawDataEvent.hh,v 1.4 1999/10/11 23:55:13 calderon Exp $
 *
 * Author: bl prelim
 ***************************************************************************
 *
 * Description: Contains a complete event of raw data
 *
 ***************************************************************************
 *
 * $Log: StTrsRawDataEvent.hh,v $
 * Revision 1.4  1999/10/11 23:55:13  calderon
 * Version with Database Access and persistent file.
 * Not fully tested due to problems with cons, it
 * doesn't find the local files at compile time.
 * Yuri suggests forcing commit to work directly with
 * files in repository.
 *
 * Revision 1.3  1999/04/29 00:16:24  lasiuk
 * add the member function clear() to take care of
 * allocated memory within the event loop.
 *
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
    StTrsRawDataEvent(int);
    virtual ~StTrsRawDataEvent();
    
    //StTrsRawDataEvent(const StTrsRawDataEvent&);
    //StTrsRawDataEvent& operator=(const StTrsRawDataEvent&);
    
    unsigned long size();
    
    void          clear();
    
public:
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<StTrsDigitalSector*> mSectors;
#else
    vector<StTrsDigitalSector*, allocator<StTrsDigitalSector*> > mSectors;
#endif
};

#endif
