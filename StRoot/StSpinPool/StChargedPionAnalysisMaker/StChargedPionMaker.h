#ifndef ST_CHARGED_PION_MAKER_HH
#define ST_CHARGED_PION_MAKER_HH

// $Id: StChargedPionMaker.h,v 1.12 2014/08/06 11:43:38 jeromel Exp $

#ifndef StMaker_H
#include "StMaker.h"
#endif

class TFile;
class TTree;
class TClonesArray;
class TString;

class StEmcTriggerMaker;
class StJetMaker;
class StMuDstMaker;
class StSpinDbMaker;
class StTriggerSimuMaker;
class StJets;
class StMiniMcEvent;

class StChargedPionBaseEv;
class StChargedPionEvent;
class StChargedPionJet;
class StChargedPionMcEvent;
class StChargedPionTrack;

class StChargedPionMaker : public StMaker {
public:
    StChargedPionMaker(const char *name = "chargedPionMaker", const char *outfile = "test.tracks.root");
    virtual ~StChargedPionMaker();
    
    void Clear(const char *option="");
    Int_t Init();
    Int_t InitRun(int runnumber);
    Int_t Make();
    Int_t Finish();
    
    void addTrigger(int trigId);
    
    const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StChargedPionMaker.h,v 1.12 2014/08/06 11:43:38 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs;}
    
private:
    TFile *mFile;                           //!
    TTree *mTree;                           //!
    StChargedPionBaseEv *mEvent;            //!
    
    TFile *mJetFile;                        //!
    TTree *mJetTree;                        //!
    StJets *mJets;                          //!
    StJets *mPyJets;                        //!
    
    TH1 *mBadTracks;                        //!
    enum badTrackTypes {kFlagged=1, kBadHelix, kBadOuterHelix, kMissingGlobal};
    
    TString mCurrentFile;                   //!
        
    TFile *mMiniMcFile;                     //!
    TTree *mMiniMcTree;                     //!
    StMiniMcEvent *mMiniMcEvent;            //!

    //pointers to makers - get them in Init()
    StMuDstMaker *mMuDstMk;                 //!
    StSpinDbMaker *mSpDbMk;                 //!
    StEmcTriggerMaker *mEmcTrgMk;           //!
    StJetMaker *mJetMk;                     //!
    StTriggerSimuMaker *mTrgSimuMk;         //!
    
    vector<int> mTriggers;                  //!
    
    void makeTriggerSimu(StChargedPionBaseEv*);
    
    ClassDef(StChargedPionMaker,1)
};

inline void StChargedPionMaker::
addTrigger(int trigId) { mTriggers.push_back(trigId); }

#endif

/*****************************************************************************
 * $Log: StChargedPionMaker.h,v $
 * Revision 1.12  2014/08/06 11:43:38  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.11  2012/11/09 03:31:34  perev
 * Cleanup
 *
 * Revision 1.10  2008/12/29 15:58:30  kocolosk
 * removed commented code and added $Id: StChargedPionMaker.h,v 1.12 2014/08/06 11:43:38 jeromel Exp $/$Log: StChargedPionMaker.h,v $
 * removed commented code and added $Id: StChargedPionMaker.h,v 1.11 2012/11/09 03:31:34 perev Exp $/Revision 1.12  2014/08/06 11:43:38  jeromel
 * removed commented code and added $Id: StChargedPionMaker.h,v 1.11 2012/11/09 03:31:34 perev Exp $/Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 * removed commented code and added $Id: StChargedPionMaker.h,v 1.11 2012/11/09 03:31:34 perev Exp $/
 * removed commented code and added $Id: StChargedPionMaker.h,v 1.12 2014/08/06 11:43:38 jeromel Exp $/Revision 1.11  2012/11/09 03:31:34  perev
 * removed commented code and added $Id: StChargedPionMaker.h,v 1.12 2014/08/06 11:43:38 jeromel Exp $/Cleanup
 * removed commented code and added $Id: StChargedPionMaker.h,v 1.12 2014/08/06 11:43:38 jeromel Exp $/ as needed
 *
 *****************************************************************************/

