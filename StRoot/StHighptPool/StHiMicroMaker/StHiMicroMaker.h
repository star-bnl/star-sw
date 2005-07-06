/***************************************************************************
 *
 * $Id: StHiMicroMaker.h,v 1.4 2005/07/06 22:20:57 fisyak Exp $
 *
 * Author: Bum Choi, UT Austin, Apr 2002
 *
 ***************************************************************************
 *
 * Description:  This Maker creates the highpt uDST's from StEvent 
 *		 for highpt Analysis.
 *
 ***************************************************************************
 *
 * $Log: StHiMicroMaker.h,v $
 * Revision 1.4  2005/07/06 22:20:57  fisyak
 * Use templated StPhysicalHelixD and StThreeVectorF
 *
 * Revision 1.3  2002/05/31 21:50:14  jklay
 * Fixed the way centrality is calculated, see README
 *
 * Revision 1.2  2002/04/03 00:37:41  jklay
 * Fixed some bugs, added new version of dcaz
 *
 * Revision 1.1  2002/04/02 20:00:41  jklay
 * Bums highpt uDST Maker
 *
 *
 **************************************************************************/
#ifndef StHiMicroMaker_hh     
#define StHiMicroMaker_hh

#ifndef __CINT__
#include <utility>
#endif

#include "StMaker.h"

#include "TString.h"
#include "TFile.h"
#include "TTree.h"
#include "TNtuple.h"
#include "StChain/StMaker.h"

//  technique is that the these classes do not get passed
//  through rootcint.
//
class StHiMicroEvent;

#include "StPhysicalHelixD.hh"
#include "StThreeVectorF.hh"
class StEvent;
class StTrack;
class StIOMaker;

//class StHiMicroEvent;

#ifndef ST_NO_NAMESPACES
using std::string;
#endif

class StHiMicroMaker : public StMaker {
 public:

    StHiMicroMaker(const Char_t *makerName="micro");
    ~StHiMicroMaker();                         
    
    void   Clear(Option_t *option="");   
    Int_t  Init(); 
    Int_t  InitRun(int runumber); 
    Int_t  Make(); 
    Int_t  Finish();

    void     setDebug(Int_t debug=1) { mDebug = debug; }
    void     setSaveAllEvents(bool val=false)	{mSaveAllEvents = val;}

    bool   SaveAllEvents() const {return mSaveAllEvents;}
    
    void     setHitLoop(Int_t doit=1) { mHitLoop=doit; }
    void     setVertexZCut(Float_t x) { mVertexZCut=x; }
    void     setOutDir(const char* dir= "./")  { mOutDir = dir; }  
    

 private:
    void     fillEvent(StEvent*);
    Int_t    fillTracks(StEvent*);
    void     dump(StTrack* prTrack,StTrack* glTrack);
    Float_t  computeXY(const StThreeVectorF&, const StTrack*);
    double dcaz(const StPhysicalHelixD& helix, const StThreeVectorF& point);
    double dcaz(const StPhysicalHelixD& helix, const StThreeVectorF& point, const StTrack* track);

    Int_t    openFile();
    Int_t    closeFile();

    bool accept(StEvent*);            // this method serves as an event filter
    bool accept(StTrack*);            // and this is used to select tracks
    bool acceptCentrality(StTrack*);
    bool goodGlobal(StTrack*);
    bool goodGlobalA(StTrack*);
    bool goodGlobalB(StTrack*);
    bool goodGlobalC(StTrack*);
    bool goodGlobalD(StTrack*);
    bool goodGlobalE(StTrack*);
    bool goodGlobalFlag(StTrack*);

    TString    mDSTName;      //!
    TFile*     mDSTFile;      //!
    TTree*     mDSTTree;      //!
    StIOMaker* mIOMaker;      //!
    TString    mInFileName;   //!
    TString    mOutDir;       //!

    Int_t      mNEvent;       //!
    Int_t      mNAcceptedEvent;//!
    Int_t      mNAcceptedTrack;//!
    bool       mSaveAllEvents; //!If all events are saved, can look at vertex efficiency with ZDC info
    Int_t      mDebug;         //!
    Float_t    mVertexZCut;    //!
    Int_t      mHitLoop;       //!

    StHiMicroEvent* mHiMicroEvent; //!


    ClassDef(StHiMicroMaker,1)
};
#endif
