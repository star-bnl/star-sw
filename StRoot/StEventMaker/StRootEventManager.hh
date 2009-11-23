/***************************************************************************
 *
 * $Id: StRootEventManager.hh,v 2.8 2009/11/23 16:37:08 fisyak Exp $
 *
 * Author: Original version by T. Wenaus, BNL
 *         Revised version for new StEvent by T. Ullrich, Yale
 ***************************************************************************
 *
 * Description:
 * Concrete implementation of DST table server (here from ROOT).
 *
 ***************************************************************************
 *
 * $Log: StRootEventManager.hh,v $
 * Revision 2.8  2009/11/23 16:37:08  fisyak
 * Clean up, fix problem with bunch crossing information in StEventInfo and StHddr
 *
 * Revision 2.6  2009/11/19 16:54:09  fisyak
 * Clean up
 *
 * Revision 2.5  2002/04/18 23:29:35  jeromel
 * Implementation of the SVT 2 tables scheme ...
 *
 * Revision 2.4  2001/11/07 21:20:46  ullrich
 * Added L1 trigger.
 *
 * Revision 2.3  2001/09/12 23:48:34  ullrich
 * Removed code to load run_header and run_summary tables.
 *
 * Revision 2.2  2000/08/17 00:38:09  ullrich
 * Added CpyTrk table.
 *
 * Revision 2.1  2000/05/25 14:44:51  ullrich
 * Removed remaining pieces of the RICH pixel table.
 *
 * Revision 2.0  1999/11/04 19:03:00  ullrich
 * Revised to build new StEvent version
 *
 **************************************************************************/
#ifndef StRootEventManager_HH
#define StRootEventManager_HH

#include "StEventManager.hh"
class StMaker;

class StRootEventManager : public StEventManager {
public:
    StRootEventManager();
    ~StRootEventManager();
    
    ooStatus openEvent(const char* colName);
    ooStatus readEvent();
    void closeEvent();
    void setup();
    void shutdown();
    
    dst_L0_Trigger_st*     returnTable_dst_L0_Trigger(long&)    const;
    dst_L1_Trigger_st*     returnTable_dst_L1_Trigger(long&)    const;
    dst_TrgDet_st*         returnTable_dst_TrgDet(long&)        const;

protected:
    St_DataSetIter mDst;
};
#endif
