#ifndef STAR_StChargedPionMcEvent
#define STAR_StChargedPionMcEvent

// $Id: StChargedPionMcEvent.h,v 1.3 2009/01/04 16:47:25 kocolosk Exp $

/*****************************************************************************
 * @class StChargedPionMcEvent
 * @author Adam Kocoloski
 *
 * Description of class.
 *****************************************************************************/

#include <string>
using std::string;

#include <vector>
using std::vector;

#include <map>
using std::map;

#include "TObject.h"
#include "TClonesArray.h"
#include "TVector3.h"

#include "StChargedPionTypes.h"
#include "StChargedPionBaseEv.h"

class StChargedPionJet;
class StChargedPionTrack;
class StChargedPionVertex;

class StChargedPionMcEvent : public StChargedPionBaseEv
{
public:
    StChargedPionMcEvent();
    virtual ~StChargedPionMcEvent();
    
    virtual void Clear(Option_t* = "");
    
    // methods shared with StChargedPionEvent
    unsigned int runId() const;
    unsigned int eventId() const;
    unsigned int bbcTimeBin() const;
    const string& muDstName() const;
    
    bool isSimuTrigger(unsigned int trigId) const;
    
    int highTowerAdc(short towerId) const;
    int triggerPatchAdc(short patchId) const;
    int jetPatchAdc(short patchId) const;
    
    unsigned int                    nVertices() const;
    TClonesArray*                   vertices();
    const TClonesArray*             vertices() const;
                                    
    unsigned int                    nTracks() const;
    TClonesArray*                   tracks();
    const TClonesArray*             tracks() const;
                                    
    unsigned int                    nJets() const;
    TClonesArray*                   jets();
    const TClonesArray*             jets() const;
                                    
    StChargedPionVertex*            vertex(int i);
    const StChargedPionVertex*      vertex(int i) const;
                                    
    StChargedPionTrack*             track(int i);
    const StChargedPionTrack*       track(int i) const;
                                    
    StChargedPionJet*               jet(int i);
    const StChargedPionJet*         jet(int i) const;
    
    // methods unique to simulations
    double s() const;
    double t() const;
    double u() const;
    double cosTheta() const;
    double pt() const;
    double hardP() const;
    double Q2() const;
    double x1() const;
    double x2() const;
    
    double t_alternative() const;
    double u_alternative() const;
    double Q2_alternative() const;
    double Q2_alternative2() const;

    double tau() const;
    double y() const;
    double xF() const;
    double beta34() const;
    
    int processId() const;
    
    enum Frame {LAB, CM};
    StChargedPionLorentzVector parton(int id, Frame = LAB) const;
    
    // the incoming partons before initial-state radiation
    StChargedPionLorentzVector&         isr1();
    StChargedPionLorentzVector&         isr2();
    
    // maybe I should be saving parton flavors here
    StChargedPionLorentzVector&         parton1();
    StChargedPionLorentzVector&         parton2();
    StChargedPionLorentzVector&         parton3();
    StChargedPionLorentzVector&         parton4();
    
    short flavor(int i) const;
    void setFlavor(int i, short f);
    
    static TLorentzVector lv(const StChargedPionLorentzVector&);
    
    StChargedPion3Vector&               mcVertex();
    const StChargedPion3Vector&         mcVertex() const;
    
    vector<StChargedPionMcTrack>&       mcTracks();
    const vector<StChargedPionMcTrack>& mcTracks() const;
    
    vector<StChargedPionJet>&           mcJets();
    const vector<StChargedPionJet>&     mcJets() const;
    
    // matched pairs, etc. containers
    vector<StChargedPionTrackPair>&         matchedPairs();
    const vector<StChargedPionTrackPair>&   matchedPairs() const;

    vector<StChargedPionTrackPair>&         mergedPairs();
    const vector<StChargedPionTrackPair>&   mergedPairs() const;

    vector<StChargedPionTrackPair>&         splitPairs();
    const vector<StChargedPionTrackPair>&   splitPairs() const;

    vector<StChargedPionTrackPair>&         contamPairs();
    const vector<StChargedPionTrackPair>&   contamPairs() const;

    vector<StChargedPionTrackPair>&         ghostPairs();
    const vector<StChargedPionTrackPair>&   ghostPairs() const;
    
    vector<StChargedPionPythiaRow>&         pythiaRecord();
    const vector<StChargedPionPythiaRow>&   pythiaRecord() const;
    
    // setters
    void setRunId(unsigned int);
    void setEventId(unsigned int);
    void setBbcTimeBin(unsigned short);
    void setMuDstName(const char*);
    void addSimuTrigger(unsigned int);
    void addHighTower(short towerId, int ADC);
    void addTriggerPatch(short patchId, int ADC);
    void addJetPatch(short patchId, int ADC);
    void addVertex(const StChargedPionVertex*);
    void addTrack(const StChargedPionTrack*);
    void addJet(const StChargedPionJet*);
    void setProcessId(int);
    void setX1(float);
    void setHardP(float);
    
    void setL2Result(const void *address, bool emulated=false);
    
private:
    UInt_t mRunId;
    UInt_t mEventId;
    UShort_t mBbcTimeBin;
    string mMuDstName;
    
    UInt_t mSimuTriggerBits;
    map<short, int> mHighTowers;
    map<short, int> mTriggerPatches;
    map<short, int> mJetPatches;
    UInt_t mL2ResultEmulated[9];
    
    TClonesArray *mVertices;
    TClonesArray *mTracks;
    TClonesArray *mJets;
    
    StChargedPionLorentzVector mISR1;
    StChargedPionLorentzVector mISR2;
    StChargedPionLorentzVector mParton1;
    StChargedPionLorentzVector mParton2;
    StChargedPionLorentzVector mParton3;
    StChargedPionLorentzVector mParton4;
    Int_t mProcessId;
    Float_t mX1;
    Float_t mHardP;
    Short_t mFlavor[4];
    
    StChargedPion3Vector mMcVertex;
    vector<StChargedPionMcTrack> mMcTracks;
    vector<StChargedPionJet> mMcJets;
    
    vector<StChargedPionTrackPair> mMatchedPairs;
    vector<StChargedPionTrackPair> mMergedPairs;
    vector<StChargedPionTrackPair> mSplitPairs;
    vector<StChargedPionTrackPair> mContamPairs;
    vector<StChargedPionTrackPair> mGhostPairs;
    //vector<StChargedPionTrackPair> mMatchedGlobals;
    
    vector<StChargedPionPythiaRow> mPythiaRecord;
    
    ClassDef(StChargedPionMcEvent, 2)
};

inline unsigned int 
StChargedPionMcEvent::runId() const { return mRunId; }

inline unsigned int 
StChargedPionMcEvent::eventId() const { return mEventId; }

inline unsigned int 
StChargedPionMcEvent::bbcTimeBin() const { return mBbcTimeBin; }

inline const string& 
StChargedPionMcEvent::muDstName() const { return mMuDstName; }

inline unsigned int 
StChargedPionMcEvent::nVertices() const { return mVertices->GetEntriesFast(); }

inline TClonesArray* 
StChargedPionMcEvent::vertices() { return mVertices; }

inline const TClonesArray* 
StChargedPionMcEvent::vertices() const { return mVertices; }

inline unsigned int 
StChargedPionMcEvent::nTracks() const { return mTracks->GetEntriesFast(); }

inline TClonesArray* 
StChargedPionMcEvent::tracks() { return mTracks; }

inline const TClonesArray* 
StChargedPionMcEvent::tracks() const { return mTracks; }

inline unsigned int 
StChargedPionMcEvent::nJets() const { return mJets->GetEntriesFast(); }

inline TClonesArray*
StChargedPionMcEvent::jets() { return mJets; }

inline const TClonesArray* 
StChargedPionMcEvent::jets() const { return mJets; }

inline int 
StChargedPionMcEvent::processId() const { return mProcessId; }

inline double
StChargedPionMcEvent::hardP() const { return mHardP; }

inline StChargedPionLorentzVector&
StChargedPionMcEvent::isr1() { return mISR1; }

inline StChargedPionLorentzVector&
StChargedPionMcEvent::isr2() { return mISR2; }

inline StChargedPionLorentzVector&
StChargedPionMcEvent::parton1() { return mParton1; }

inline StChargedPionLorentzVector&
StChargedPionMcEvent::parton2() { return mParton2; }

inline StChargedPionLorentzVector&
StChargedPionMcEvent::parton3() { return mParton3; }

inline StChargedPionLorentzVector&
StChargedPionMcEvent::parton4() { return mParton4; }

inline short
StChargedPionMcEvent::flavor(int i) const { return mFlavor[i-1]; }

inline void 
StChargedPionMcEvent::setFlavor(int i, short f) { mFlavor[i-1] = f; }

inline StChargedPion3Vector& 
StChargedPionMcEvent::mcVertex() { return mMcVertex; }

inline const StChargedPion3Vector& 
StChargedPionMcEvent::mcVertex() const { return mMcVertex; }

inline vector<StChargedPionMcTrack>& 
StChargedPionMcEvent::mcTracks() { return mMcTracks; }

inline const vector<StChargedPionMcTrack>& 
StChargedPionMcEvent::mcTracks() const { return mMcTracks; }

inline vector<StChargedPionJet>& 
StChargedPionMcEvent::mcJets() { return mMcJets; }

inline const vector<StChargedPionJet>& 
StChargedPionMcEvent::mcJets() const { return mMcJets; }

inline vector<StChargedPionTrackPair>&
StChargedPionMcEvent::matchedPairs() { return mMatchedPairs; }
    
inline const vector<StChargedPionTrackPair>&   
StChargedPionMcEvent::matchedPairs() const { return mMatchedPairs; }

inline vector<StChargedPionTrackPair>&
StChargedPionMcEvent::mergedPairs() { return mMergedPairs; }

inline const vector<StChargedPionTrackPair>&
StChargedPionMcEvent::mergedPairs() const { return mMergedPairs; }

inline vector<StChargedPionTrackPair>&
StChargedPionMcEvent::splitPairs() { return mSplitPairs; }

inline const vector<StChargedPionTrackPair>&
StChargedPionMcEvent::splitPairs() const { return mSplitPairs; }

inline vector<StChargedPionTrackPair>&
StChargedPionMcEvent::contamPairs() { return mContamPairs; }

inline const vector<StChargedPionTrackPair>&
StChargedPionMcEvent::contamPairs() const { return mContamPairs; }

inline vector<StChargedPionTrackPair>&
StChargedPionMcEvent::ghostPairs() { return mGhostPairs; }

inline const vector<StChargedPionTrackPair>&
StChargedPionMcEvent::ghostPairs() const { return mGhostPairs; }

inline vector<StChargedPionPythiaRow>&
StChargedPionMcEvent::pythiaRecord() { return mPythiaRecord; }

inline const vector<StChargedPionPythiaRow>&
StChargedPionMcEvent::pythiaRecord() const { return mPythiaRecord; }

inline void 
StChargedPionMcEvent::setRunId(unsigned int a) { mRunId = a; }

inline void 
StChargedPionMcEvent::setEventId(unsigned int a) { mEventId = a; }

inline void 
StChargedPionMcEvent::setBbcTimeBin(unsigned short a) { mBbcTimeBin = a; }

inline void 
StChargedPionMcEvent::setMuDstName(const char* a) { mMuDstName = a; }

inline void 
StChargedPionMcEvent::setProcessId(int i) { mProcessId = i; }

inline void
StChargedPionMcEvent::setX1(float a) { mX1 = a; }

inline void
StChargedPionMcEvent::setHardP(float a) { mHardP = a; }

inline void 
StChargedPionMcEvent::addHighTower(short towerId, int ADC) { 
    mHighTowers[towerId] = ADC; 
}

inline void 
StChargedPionMcEvent::addTriggerPatch(short patchId, int ADC) { 
    mTriggerPatches[patchId] = ADC; 
}

inline void 
StChargedPionMcEvent::addJetPatch(short patchId, int ADC) { 
    mJetPatches[patchId] = ADC; 
}

#endif

/*****************************************************************************
 * $Log: StChargedPionMcEvent.h,v $
 * Revision 1.3  2009/01/04 16:47:25  kocolosk
 * increment ClassDef, not even sure why
 *
 * Revision 1.2  2008/12/29 15:58:31  kocolosk
 * removed commented code and added $Id: StChargedPionMcEvent.h,v 1.3 2009/01/04 16:47:25 kocolosk Exp $/$Log: StChargedPionMcEvent.h,v $
 * removed commented code and added $Id$/Revision 1.3  2009/01/04 16:47:25  kocolosk
 * removed commented code and added $Id$/increment ClassDef, not even sure why
 * removed commented code and added $Id$/ as needed
 *
 * Revision 1.1  2008/07/17 17:06:31  kocolosk
 * big-bang integration StChargedPionMcEvent framework
 *
 *****************************************************************************/
