// $Id: StEventMaker.h,v 1.4 1999/07/12 15:08:35 fisyak Exp $
// $Log: StEventMaker.h,v $
// Revision 1.4  1999/07/12 15:08:35  fisyak
// Add  type_of_call F77_NAME
//
// Revision 1.3  1999/07/11 23:27:50  fisyak
// dst_TriggerDetectors => dst_TrgDet
//
// Revision 1.2  1999/05/22 17:59:01  perev
// Can read also mdc2 and last format
//
// Revision 1.1  1999/05/04 22:40:35  fisyak
// Initial revision of persistent StEventMaker
//
// Revision 1.5  1999/05/01 01:49:15  fisyak
// Add StRootEvent fill
//
// Revision 1.4  1999/03/11 03:12:18  perev
// new schema
//
// Revision 1.3  1999/02/19 16:28:55  fisyak
// Reactivate dst Maker
//
// Revision 1.2  1999/01/20 23:58:03  fisyak
// Tree 2 GetTree
//
// Revision 1.1  1999/01/02 19:09:23  fisyak
// Add Clones
//
// Revision 1.7  1998/10/31 00:25:45  fisyak
// Makers take care about branches
//
// Revision 1.6  1998/10/06 18:00:31  perev
// cleanup
//
// Revision 1.5  1998/08/26 12:15:13  fisyak
// Remove asu & dsl libraries
//
// Revision 1.4  1998/08/14 15:25:58  fisyak
// add options
//
// Revision 1.3  1998/08/10 02:32:07  fisyak
// Clean up
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//
 * Instance of StEvent now also created if no DST dataset
 * is available.
 *
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StEventMaker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

 * Revision 2.2  2000/01/05 16:07:47  ullrich
 * Added loading of SSD hits and handling of runco branch.
 *
 * Revision 2.1  1999/11/05 18:35:57  ullrich
 * Added methods and flags for debugging and monitoring.
class dst_run_header_st;
class dst_event_header_st;
class dst_event_summary_st;
class dst_monitor_hard_st;
class dst_monitor_soft_st;
class gen_header_st;
class particle_st;
class dst_TrgDet_st;
 *
 * Revision 2.0  1999/11/04 19:03:00  ullrich
 private:
  StEventManager* theEventManager; 			//!
  StEvent* currentEvent; 				//!
  StRun* currentRun; 					//!
  
 protected:
 public: 
                  StEventMaker(const char *name="StEventMaker",const char *titl="");
   virtual       ~StEventMaker();
   virtual Int_t  Init();
   virtual Int_t  Make();
   virtual void   PrintInfo();
   virtual void   setEventManager(StEventManager* mgr); 
   virtual StEventManager* eventManager(){return theEventManager;};
   virtual StEvent* event() { return currentEvent;}; 
   virtual StRun* run() {return currentRun;};
   virtual void Clear(const char* opt="");

// For local use
  ooStatus status;					//!
  dst_run_header_st 		*dstRunHeader;		//!
  dst_run_header_st 		*defRunHeader;		//!
  dst_event_header_st  		*dstEventHeader;	//!
  dst_event_header_st  		*defEventHeader;	//!
  dst_event_summary_st 		*dstEventSummary;	//!
  dst_event_summary_st 		*defEventSummary;	//!
  dst_monitor_hard_st 		*dstMonitorHard;	//!
  dst_monitor_hard_st 		*defMonitorHard;	//!
  dst_monitor_soft_st 		*dstMonitorSoft;	//!
  dst_monitor_soft_st 		*defMonitorSoft;	//!
  gen_header_st 		*genHeader;		//!
  particle_st 			*particle;		//!
  dst_TrgDet_st 	        *dstTriggerDetectors;	//!
  dst_TrgDet_st 	        *defTriggerDetectors;	//!
  Bool_t doLoad;					//!
   
  virtual const char *GetCVS()
  {static const char cvs[]="Tag $Name:  $ $Id: StEventMaker.h,v 1.4 1999/07/12 15:08:35 fisyak Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StEventMaker, 1)   //StAF chain virtual base class for Makers
    Int_t  makeEvent();

    Bool_t isNewRun();
    StEventManager* mEventManager; 			//!
    StEvent*        mCurrentEvent;	                //!
    StRun*          mCurrentRun; 			//!
    void   printTrackInfo(StTrack*);
    
private:
    StEvent*              mCurrentEvent;                //!
    StRun*                mCurrentRun; 			//!
    dst_summary_param_st* mDstSummaryParam;             //!
    Bool_t                mCreateEmptyInstance;         //!
    ClassDef(StEventMaker, 1)
};
#endif
