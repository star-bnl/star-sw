/**********************************************
 *
 * $Id: StMcEventMaker.h,v 1.4 2000/04/20 16:53:39 calderon Exp $
 * $Log: StMcEventMaker.h,v $
 * Revision 1.4  2000/04/20 16:53:39  calderon
 * change maker name from "MCEvent" to "StMcEvent".
 *
 * Revision 1.3  1999/12/03 00:55:21  calderon
 * Completely revised for StMcEvent 2.0
 * Using StDbUtilities for coordinate transformations.
 * Tested g2t_event table is read properly (when available).
 * Added messages for diagnostics.
 * Tested in Linux, Solaris 4.2  and HP.
 *
 * Revision 1.2  1999/07/28 20:27:43  calderon
 * Version with SL99f libraries
 *
 *
 **********************************************/

#ifndef StMcEventMaker_HH
#define StMcEventMaker_HH

#ifndef StMaker_H
#include "StMaker.h"
#endif

class StMcEvent;
class StMcTrack;

class StMcEventMaker : public StMaker {
public:

    StMcEventMaker(const char* name = "StMcEvent", const char* title = "");
    virtual ~StMcEventMaker();

    virtual void  Clear(const char* opt="");
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();

    StMcEvent* currentMcEvent() { return mCurrentMcEvent;}; 

    virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StMcEventMaker.h,v 1.4 2000/04/20 16:53:39 calderon Exp $ built "__DATE__" "__TIME__; return cvs;}	
    
public:

    Bool_t  doPrintEventInfo;      //! lots of screen output
    Bool_t  doPrintMemoryInfo;     //! 
    Bool_t  doPrintCpuInfo;        //! 

protected:
    void   printEventInfo();
    void   printTrackInfo(StMcTrack*);

private:
    Bool_t drawinit;
    StMcEvent* mCurrentMcEvent; //!  This tells CINT not to parse it.
    ClassDef(StMcEventMaker, 1)

};

#endif
