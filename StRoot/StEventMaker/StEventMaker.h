/***************************************************************************
 *
 * $Id: StEventMaker.h,v 2.3 2000/05/24 15:48:20 ullrich Exp $
 *
 * Author: Original version by T. Wenaus, BNL
 *         Revised version for new StEvent by T. Ullrich, Yale
 ***************************************************************************
 *
 * Description: Setup of StEvent
 *
 ***************************************************************************
 *
 * $Log: StEventMaker.h,v $
 * Revision 2.3  2000/05/24 15:48:20  ullrich
 * Instance of StEvent now also created if no DST dataset
 * is available.
 *
 * Revision 2.3  2000/05/24 15:48:20  ullrich
 * Instance of StEvent now also created if no DST dataset
 * is available.
 *
 * Revision 2.2  2000/01/05 16:07:47  ullrich
 * Added loading of SSD hits and handling of runco branch.
 *
 * Revision 2.1  1999/11/05 18:35:57  ullrich
 * Added methods and flags for debugging and monitoring.
 *
 * Revision 2.0  1999/11/04 19:03:00  ullrich
 * Revised to build new StEvent version
 *
 **************************************************************************/
#ifndef STAR_StEventMaker
#define STAR_StEventMaker

#include "StMaker.h"

#include "StEventMaker/StEventManager.hh"
class dst_summary_param_st;
class StEvent;
class StRun;
class StTrack;

class StEventMaker : public StMaker {
public: 
    StEventMaker(const char *name="StEventMaker", const char *title="");
    virtual ~StEventMaker();
    
    virtual Int_t           Init();
    virtual Int_t           Make();
    virtual void            setEventManager(StEventManager* mgr); 
    virtual StEventManager* eventManager();
    virtual StEvent*        event();
    virtual StRun*          run();
    virtual void            Clear(const char* opt="");
       
    virtual const char *GetCVS() const
    {
	static const char cvs[]="$Id: StEventMaker.h,v 2.3 2000/05/24 15:48:20 ullrich Exp $ built "__DATE__" "__TIME__ ;
	return cvs;
    }

public:    
    Bool_t  doLoadTpcHits;         //!
    Bool_t  doLoadFtpcHits;        //!
    Bool_t  doLoadSvtHits;         //!
    Bool_t  doLoadSsdHits;         //!
    
    Bool_t  doPrintRunInfo;        //! lots of screen output
    Bool_t  doPrintEventInfo;      //! lots of screen output
    Bool_t  doPrintMemoryInfo;     //! 
    Bool_t  doPrintCpuInfo;        //!
    
protected:
    Int_t  makeRun();
    Int_t  makeEvent();
    Bool_t isNewRun();
    Int_t  loadRunConstants();
    
    void   printRunInfo();
    void   printEventInfo();
    void   printTrackInfo(StTrack*);
    
private:
    StEventManager*       mEventManager;		//!
    StEvent*              mCurrentEvent;                //!
    StRun*                mCurrentRun; 			//!
    dst_summary_param_st* mDstSummaryParam;             //!
    Bool_t                mCreateEmptyInstance;         //!
    ClassDef(StEventMaker, 1)
};
#endif
