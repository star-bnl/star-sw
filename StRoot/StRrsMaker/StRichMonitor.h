/*********************************************************
 * $Id: StRichMonitor.h,v 1.2 2000/01/27 17:09:57 lasiuk Exp $
 *
 * Description:
 *  Struct holds different histograms filled by different
 *  function throughout the program. 
 *
 *  It creates a root Canvas, and draws histograms in it.
 *
 ************************************************************
 * $Log: StRichMonitor.h,v $
 * Revision 1.2  2000/01/27 17:09:57  lasiuk
 * modify to work stand-alone from ROOT
 *
 * Revision 1.1  2000/01/25 22:02:21  lasiuk
 * Second Revision
 *
 ***********************************************************/
#ifdef __ROOT__
#include "StRichRrsMacros.h"

#ifdef RICH_WITH_MONITOR

#ifndef ST_RICH_MONITOR_H
#define ST_RICH_MONITOR_H

#include "TROOT.h"
#include "TFile.h"
#include "TCanvas.h"

#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif

#include "StRichGeometryDb.h"

class StRichMonitor {
private:
    StRichMonitor();

public:
    ~StRichMonitor();
    static StRichMonitor* getInstance();   // sole instance access
    void update();
    
    static StRichMonitor *mInstance;      // handle to only instance
    
    TCanvas * mCanvas;

private:
    StRichGeometryDb* mGeometryDb;
};

#ifndef ST_NO_NAMESPACES
//}
#endif

#endif // ST_RICH_VIEWER_H

#endif // RICH_WITH_VIEWER

#endif // __ROOT__
