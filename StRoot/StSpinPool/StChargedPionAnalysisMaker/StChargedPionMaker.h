/***************************************************************************
*
* $Id: StChargedPionMaker.h,v 1.7 2008/01/15 21:26:05 kocolosk Exp $
*
* Author:  Adam Kocoloski
***************************************************************************
*
* Description:  Collects charged pions from muDst.  Intent is to use 
* StJetSkimMaker in the same chain to get all spin-related event info
*
***************************************************************************
*
* $Log: StChargedPionMaker.h,v $
* Revision 1.7  2008/01/15 21:26:05  kocolosk
* grab StJets from StJetMaker if it's in the chain
*
* Revision 1.6  2008/01/08 17:33:15  kocolosk
* StChargedPionMaker fills a full StChargedPionEvent on its own now
* Added trigger prescales and extra simulator info to Event
* Added detectorEta() definition to Jet
* Removed unused Header class
*
* Revision 1.5  2007/12/31 19:53:04  kocolosk
* new tree structure separate from StJetSkimEvent
*
* Revision 1.4  2007/03/12 15:01:50  kocolosk
* use StChargedPionTrack instead of StMuTrack so we can read the trees offline
*
* Revision 1.3  2007/03/10 16:28:28  kocolosk
* log each new file in job
*
* Revision 1.2  2007/03/08 22:13:59  kocolosk
* stores StMuTracks directly
*
* Revision 1.1  2007/02/02 13:59:42  kocolosk
* new Maker StChargedPionMaker intended to be used with StJetSkimEventMaker for spin analysis
*
**************************************************************************/
#ifndef ST_CHARGED_PION_MAKER_HH
#define ST_CHARGED_PION_MAKER_HH

#ifndef StMaker_H
#include "StMaker.h"
#endif

class TFile;
class TTree;
class TClonesArray;
class TString;
class TUnixSystem;

class StMuDstMaker;
class StSpinDbMaker;
class StEmcTriggerMaker;
class StJetMaker;

class StMuTrack;
class StJetSkimEvent;
class StJet;
class StJets;
class TrackToJetIndex;
class StChargedPionEvent;
class StChargedPionTrack;
class StChargedPionJet;

class StChargedPionMaker : public StMaker {
public:
    StChargedPionMaker(const char *name = "chargedPionMaker", const char *outfile = "test.tracks.root");
    virtual ~StChargedPionMaker();
    
    virtual void Clear(const char *option="");
    virtual Int_t Init();
    virtual Int_t InitRun(int runnumber);
    virtual Int_t Make();
    virtual Int_t Finish();
    
    static void translateEvent(StJetSkimEvent *skimEvent, StChargedPionEvent *ev);
    static void translateJet(StJet* oldJet, vector<TrackToJetIndex*> particles, StChargedPionJet* jet);
    static void translateJets(StJets* jets, StChargedPionEvent *ev);
    
    virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StChargedPionMaker.h,v 1.7 2008/01/15 21:26:05 kocolosk Exp $ built "__DATE__" "__TIME__; return cvs;}
    
private:
    TFile *mFile;               //!
    TTree *mTree;               //!
    StChargedPionEvent *mEvent; //!
    
    TFile *mJetFile;            //!
    TTree *mJetTree;            //!
    StJets *mJets;              //!
    
    TH1 *mBadTracks;            //!
    enum badTrackTypes {kFlagged=1, kBadHelix, kBadOuterHelix, kMissingGlobal};
    
    TString currentFile;        //!
    TUnixSystem *theSystem;      //!

    //pointers to makers - get them in Init()
    StMuDstMaker *muDstMaker;   //!
    StSpinDbMaker *spDbMaker;   //!
    StEmcTriggerMaker *emcTrgMaker; //!
    StJetMaker *jetMaker; //!
    
    //translate StMuTrack into StChargedPionTrack so we can read it anywhere
    StChargedPionTrack & chargedPionTrack(StMuTrack *track);
    
    ClassDef(StChargedPionMaker,1)
};

#endif
