/***************************************************************************
 *
 * $Id: StBrowsableEvent.h,v 2.3 1999/11/04 13:29:52 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StBrowsableEvent.h,v $
 * Revision 2.3  1999/11/04 13:29:52  ullrich
 * Added constructor without summary table
 *
 * Revision 2.3  1999/11/04 13:29:52  ullrich
 * Added constructor without summary table
 *
 * Revision 2.2  1999/10/28 22:24:53  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:42:51  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StBrowsableEvent_hh
#define StBrowsableEvent_hh

#include "StEvent.h"
#include "TBrowser.h"

class event_header_st;
class dst_event_summary_st;
class dst_summary_param_st;

class StBrowsableEvent : public StEvent {
public:
    StBrowsableEvent();
    StBrowsableEvent(const event_header_st&,
                     const dst_event_summary_st&,
                     const dst_summary_param_st&);
    StBrowsableEvent(const event_header_st&);
    virtual ~StBrowsableEvent();
    
    void browse(TBrowser*);
    
private:
    StBrowsableEvent& operator=(const StBrowsableEvent&);
    StBrowsableEvent(const StBrowsableEvent&);
    
    ClassDef(StBrowsableEvent,1)
};
#endif
