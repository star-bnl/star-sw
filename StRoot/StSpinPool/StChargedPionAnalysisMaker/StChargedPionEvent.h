#ifndef ST_CHARGED_PION_EVENT_HH
#define ST_CHARGED_PION_EVENT_HH

#include <string>
using std::string;

#include <vector>
using std::vector;

#include <map>
using std::map;

#include "TObject.h"
#include "TClonesArray.h"
#include "StEvent/StRunInfo.h"

class StChargedPionVertex;
class StChargedPionTrack;
class StChargedPionJet;

class StChargedPionEvent : public TObject
{
public:
    StChargedPionEvent();
    StChargedPionEvent(const StChargedPionEvent&);
    virtual ~StChargedPionEvent();
    
    StChargedPionEvent& operator=(const StChargedPionEvent&);
    void copy(const StChargedPionEvent&);
    
    virtual void Clear(Option_t* = "");
    
    unsigned int runId() const;
    unsigned int eventId() const;
    unsigned int bx7() const;
    unsigned int bbcTimeBin() const;
    const string& muDstName() const;
    const StRunInfo& runInfo() const;
    
    unsigned int spinBit() const;
    bool isSpinValid() const;
    bool isPolValid() const;
    bool isPolLong() const;
    bool isPolTrans() const;
    bool isBxingMasked() const;
    bool isNullOffset() const;
    
    bool isTrigger(unsigned int trigId) const;
    bool isSimuTrigger(unsigned int trigId) const;
    float prescale(unsigned int trigId) const;
    static unsigned int triggerBit(unsigned int trigId);
    
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
    
    void setRunId(unsigned int);
    void setEventId(unsigned int);
    void setBx7(unsigned char);
    void setBbcTimeBin(unsigned short);
    void setMuDstName(const char*);
    void setRunInfo(const StRunInfo&);
    
    void setSpinBit(unsigned char);
    void setPolValid(bool);
    void setPolLong(bool);
    void setPolTrans(bool);
    void setBxingMasked(bool);
    void setBxingOffset(int);
    
    void addTrigger(unsigned int);
    void addSimuTrigger(unsigned int);
    void setPrescale(unsigned int trigId, float prescale);
    
    void addHighTower(short towerId, int ADC);
    void addTriggerPatch(short patchId, int ADC);
    void addJetPatch(short patchId, int ADC);
    
    /// address to dijet result
    void setL2Result(const void *address, bool emulated=false);
    
    void addVertex(const StChargedPionVertex*);
    void addTrack(const StChargedPionTrack*);
    void addJet(const StChargedPionJet*);
    
private:
    UInt_t mRunId;
    UInt_t mEventId;
    UChar_t mBx7;
    UShort_t mBbcTimeBin;
    UChar_t mSpinBit;
    UChar_t mSpinQA;
    
    StRunInfo mRunInfo;
    
    string mMuDstName;
    
    static map<unsigned int, unsigned int> mTriggerLookup; //!
    map<unsigned int, float> mTriggerPrescales;
    UInt_t mTriggerBits;
    UInt_t mSimuTriggerBits;
    
    map<short, int> mHighTowers;
    map<short, int> mTriggerPatches;
    map<short, int> mJetPatches;
    
    // this is enough to store L2Jet, L2Ped, L2GammaBemc, L2GammaEemc for 2006
    UInt_t mL2Result[9];
    UInt_t mL2ResultEmulated[9];
    
    TClonesArray *mVertices;
    TClonesArray *mTracks;
    TClonesArray *mJets;
    
    enum {kIsPolValid=0x01, kIsPolLong=0x02, kIsPolTrans=0x04, kIsBxingMasked=0x8, kNullOffset=0x10}; //!
    
    ClassDef(StChargedPionEvent,3)
};

inline unsigned int StChargedPionEvent::runId() const { return mRunId; }
inline unsigned int StChargedPionEvent::eventId() const { return mEventId; }
inline unsigned int StChargedPionEvent::bx7() const { return mBx7; }
inline unsigned int StChargedPionEvent::bbcTimeBin() const { return mBbcTimeBin; }
inline const string& StChargedPionEvent::muDstName() const { return mMuDstName; }
inline const StRunInfo& StChargedPionEvent::runInfo() const { return mRunInfo; }

inline unsigned int StChargedPionEvent::spinBit() const { return mSpinBit; }
inline bool StChargedPionEvent::isPolValid() const { return mSpinQA & kIsPolValid; }
inline bool StChargedPionEvent::isPolLong() const { return mSpinQA & kIsPolLong; }
inline bool StChargedPionEvent::isPolTrans() const { return mSpinQA & kIsPolTrans; }
inline bool StChargedPionEvent::isBxingMasked() const { return mSpinQA & kIsBxingMasked; }
inline bool StChargedPionEvent::isNullOffset() const { return mSpinQA & kNullOffset; }

inline unsigned int StChargedPionEvent::nVertices() const { return mVertices->GetEntriesFast(); }
inline TClonesArray* StChargedPionEvent::vertices() { return mVertices; }
inline const TClonesArray* StChargedPionEvent::vertices() const { return mVertices; }

inline unsigned int StChargedPionEvent::nTracks() const { return mTracks->GetEntriesFast(); }
inline TClonesArray* StChargedPionEvent::tracks() { return mTracks; }
inline const TClonesArray* StChargedPionEvent::tracks() const { return mTracks; }

inline unsigned int StChargedPionEvent::nJets() const { return mJets->GetEntriesFast(); }
inline TClonesArray* StChargedPionEvent::jets() { return mJets; }
inline const TClonesArray* StChargedPionEvent::jets() const { return mJets; }

inline void StChargedPionEvent::setRunId(unsigned int a) { mRunId = a; }
inline void StChargedPionEvent::setEventId(unsigned int a) { mEventId = a; }
inline void StChargedPionEvent::setBx7(unsigned char a) { mBx7 = a; }
inline void StChargedPionEvent::setBbcTimeBin(unsigned short a) { mBbcTimeBin = a; }
inline void StChargedPionEvent::setMuDstName(const char* a) { mMuDstName = a; }
inline void StChargedPionEvent::setRunInfo(const StRunInfo& a) { mRunInfo = a; }

inline void StChargedPionEvent::setSpinBit(unsigned char a) { mSpinBit = a; }
inline void StChargedPionEvent::setPolValid(bool a) { a ? mSpinQA |= kIsPolValid : mSpinQA &= ~kIsPolValid; }
inline void StChargedPionEvent::setPolLong(bool a) { a ? mSpinQA |= kIsPolLong : mSpinQA &= ~kIsPolLong; }
inline void StChargedPionEvent::setPolTrans(bool a) { a ? mSpinQA |= kIsPolTrans : mSpinQA &= ~kIsPolTrans; }
inline void StChargedPionEvent::setBxingMasked(bool a) { a ? mSpinQA |= kIsBxingMasked : mSpinQA &= ~kIsBxingMasked; }
inline void StChargedPionEvent::setBxingOffset(int a) { (a==0) ? mSpinQA |= kNullOffset : mSpinQA &= ~kNullOffset; }

inline void StChargedPionEvent::setPrescale(unsigned int trigId, float prescale) { mTriggerPrescales[trigId] = prescale; }

inline void StChargedPionEvent::addHighTower(short towerId, int ADC) { mHighTowers[towerId] = ADC; }
inline void StChargedPionEvent::addTriggerPatch(short patchId, int ADC) { mTriggerPatches[patchId] = ADC; }
inline void StChargedPionEvent::addJetPatch(short patchId, int ADC) { mJetPatches[patchId] = ADC; }

#endif
