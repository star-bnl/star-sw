/**********************************************
 *
 * $Id: StMcEventMaker.h,v 1.7 2000/06/22 23:53:31 calderon Exp $
 * $Log: StMcEventMaker.h,v $
 * Revision 1.7  2000/06/22 23:53:31  calderon
 * Changes from Aleksei for filling of emc hits.
 * ttemp and ttempParticle are now data members.
 *
 * Revision 1.6  2000/06/06 03:00:18  calderon
 * Introduction of Calorimeter classes.  Filled according to algorithm from
 * Aleksei, plus some additional checks.
 *
 * Revision 1.5  2000/05/11 14:40:29  calderon
 * Added switches to do/do not load hit information from different detectors.
 * By default, all the detectors' hit information is loaded.
 *
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
#include <vector>
#ifndef StMaker_H
#include "StMaker.h"
#endif
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

class StMcEvent;
class StMcTrack;
class StMcEmcHitCollection;
class St_g2t_emc_hit;

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
    {static const char cvs[]="Tag $Name:  $ $Id: StMcEventMaker.h,v 1.7 2000/06/22 23:53:31 calderon Exp $ built "__DATE__" "__TIME__; return cvs;}	
    
public:

    Bool_t  doPrintEventInfo;      //! lots of screen output
    Bool_t  doPrintMemoryInfo;     //! 
    Bool_t  doPrintCpuInfo;        //! 
    Bool_t  doUseTpc;              //!
    Bool_t  doUseSvt;              //!
    Bool_t  doUseFtpc;             //!
    Bool_t  doUseRich;             //!
    Bool_t  doUseBemc;             //!
    Bool_t  doUseBsmd;             //!
protected:
    void   fillBemc(St_g2t_emc_hit*);
    void   fillBsmd(St_g2t_emc_hit*);

    void   printEventInfo();
    void   printTrackInfo(StMcTrack*);
    void   printEventInfoForEmc(StMcEmcHitCollection*);

private:
#ifndef ST_NO_TEMPLATE_DEF_ARGS	  
    vector<StMcTrack*> ttemp; //! Temporary array for Step 4 in Make
    vector<StMcTrack*> ttempParticle; //!
#else
    vector<StMcTrack*, allocator<StMcTrack*> > ttemp; //!
    vector<StMcTrack*, allocator<StMcTrack*> > ttempParticle; //!
#endif
    Bool_t drawinit;
    StMcEvent* mCurrentMcEvent; //!  This tells CINT not to parse it.
    
    ClassDef(StMcEventMaker, 1)

};

#endif
