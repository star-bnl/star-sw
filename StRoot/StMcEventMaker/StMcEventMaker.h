/**********************************************
 *
 * $Id: StMcEventMaker.h,v 1.28 2014/08/06 11:43:26 jeromel Exp $
 * $Log: StMcEventMaker.h,v $
 * Revision 1.28  2014/08/06 11:43:26  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.27  2013/03/25 23:51:22  perev
 * Mustafa.Pxl corrs
 *
 * Revision 1.26  2012/03/22 01:10:13  perev
 * Etr add
 *
 * Revision 1.25  2011/10/11 01:24:52  perev
 * Mtd added
 *
 * Revision 1.24  2011/07/20 17:36:52  perev
 * Fsc added
 *
 * Revision 1.23  2011/01/26 19:48:35  perev
 * FPD ==> STAR Soft
 *
 * Revision 1.22  2010/07/21 17:31:23  perev
 * useBtof cancelled useTof is ON instead (F.Geurt)
 *
 * Revision 1.21  2009/07/24 19:06:41  perev
 * Btof added (Geurts)
 *
 * Revision 1.20  2007/10/16 19:49:46  fisyak
 * rename Hft => Pxl, remove Hpd, Igt and Fst
 *
 * Revision 1.19  2006/09/25 14:21:46  fisyak
 * Add Hpd Hits
 *
 * Revision 1.18  2005/09/28 21:30:51  fisyak
 * Persistent StMcEvent
 *
 * Revision 1.17  2005/07/07 18:21:18  calderon
 * Added code for filling of IGT classes.
 *
 * Revision 1.16  2005/06/06 19:15:07  calderon
 * Update for filling EEMC hits.  All filling now done in one function,
 * StMcEventMaker::fillEemc(), towers, prs, smdu, smdv.
 *
 * Revision 1.15  2005/05/27 23:38:06  calderon
 * Update of EEMC filling for eprs, esmdu and esmdv hits.
 *
 * Revision 1.14  2005/05/11 20:53:13  calderon
 * Added loading of SSD hits from g2t_ssd_hit table.
 *
 * Revision 1.13  2005/04/18 20:12:40  calderon
 * Modifications to build the Fgt and Fst classes from the g2t tables.
 *
 * Revision 1.12  2003/12/04 05:58:15  calderon
 * Introduction of Endcap EMC collections into StMcEvent.  Read the corresponding
 * g2t table for the hits, decode the volume Id and add it to the proper
 * containers in StMcEvent and StMcTrack.
 *
 * Revision 1.11  2003/09/10 19:47:22  perev
 * ansi corrs
 *
 * Revision 1.10  2003/08/20 18:51:01  calderon
 * Filling of Tof and Pixel classes.
 *
 * Revision 1.9  2003/02/19 03:17:04  calderon
 * Code to fill the StMcCtbHitCollection from the g2t tables by the Gansinator.
 *
 * Revision 1.8  2001/05/13 21:14:49  calderon
 * Modifications from Aleksei : StMcEmcHitCollections changed, added
 * method for printing Emc information of the event
 *
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

    StMcEventMaker(const char* name = "StMcEventMaker", const char* title = "");
    virtual ~StMcEventMaker();

    virtual void  Clear(const char* opt="");
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();

    virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StMcEventMaker.h,v 1.28 2014/08/06 11:43:26 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs;}	
    
public:

    Bool_t  doPrintEventInfo;      //! lots of screen output
    Bool_t  doPrintMemoryInfo;     //! 
    Bool_t  doPrintCpuInfo;        //! 
    Bool_t  doUseTpc;              //!
    Bool_t  doUseSvt;              //!
    Bool_t  doUseSsd;              //!
    Bool_t  doUseFtpc;             //!
    Bool_t  doUseRich;             //!
    Bool_t  doUseBemc;             //!
    Bool_t  doUseBsmd;             //!
    Bool_t  doUseCtb;              //!
    Bool_t  doUseTofp;             //!
    Bool_t  doUseTof;              //!
    Bool_t  doUseMtd;              //!
    Bool_t  doUseEemc;             //!
    Bool_t  doUseFpd;		   //!
    Bool_t  doUseFsc;		   //!
    Bool_t  doUsePxl;            //!
    Bool_t  doUseIst;              //!
    Bool_t  doUseFgt;              //!
    Bool_t  doUseEtr;              //!

    void   printEventInfo();                               // *MENU* 
protected:
    void   fillBemc(St_g2t_emc_hit*);
    void   fillBsmd(St_g2t_emc_hit*);
    void   fillEemc(St_g2t_emc_hit* g2t_tile,St_g2t_emc_hit* g2t_smd);
    void   fillFpd(St_g2t_emc_hit*);
    void   fillFsc(St_g2t_emc_hit*);


private:
#ifndef ST_NO_TEMPLATE_DEF_ARGS	  
    vector<StMcTrack*> ttemp; //! Temporary array for Step 4 in Make
    vector<StMcTrack*> ttempParticle; //!
#else
    vector<StMcTrack*, allocator<StMcTrack*> > ttemp; //!
    vector<StMcTrack*, allocator<StMcTrack*> > ttempParticle; //!
#endif
    StMcEvent* mCurrentMcEvent; //!  This tells CINT not to parse it.

    ClassDef(StMcEventMaker,0)

};

#endif
