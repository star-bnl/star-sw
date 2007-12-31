#ifndef ST_CHARGED_PION_EVENT_HH
#define ST_CHARGED_PION_EVENT_HH

#include <vector>
using std::vector;

#include <map>
using std::map;

#include "TObject.h"
#include "TClonesArray.h"

class StChargedPionVertex;
class StChargedPionTrack;
class StChargedPionJet;
//class StChargedPionJetParticle;

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
    
    unsigned int spinBit() const;
    bool isSpinValid() const;
    bool isPolValid() const;
    bool isPolLong() const;
    bool isPolTrans() const;
    bool isBxingMasked() const;
    bool isNullOffset() const;
    
    bool isTrigger(unsigned int trigId) const;
    bool isSimuTrigger(unsigned int trigId) const;
    
    unsigned int                    nVertices() const;
    TClonesArray*                   vertices();
    const TClonesArray*             vertices() const;
                                    
    unsigned int                    nTracks() const;
    TClonesArray*                   tracks();
    const TClonesArray*             tracks() const;
                                    
    unsigned int                    nJets() const;
    TClonesArray*                   jets();
    const TClonesArray*             jets() const;
    
    //unsigned int                    nJetParticles();
    //TClonesArray*                   jetParticles();
    //const TClonesArray*             jetParticles() const;
                                    
    StChargedPionVertex*            vertex(int i);
    const StChargedPionVertex*      vertex(int i) const;
                                    
    StChargedPionTrack*             track(int i);
    const StChargedPionTrack*       track(int i) const;
                                    
    StChargedPionJet*               jet(int i);
    const StChargedPionJet*         jet(int i) const;
                                    
    //StChargedPionJetParticle*       jetParticle(int i);
    //const StChargedPionJetParticle* jetParticle(int i) const;
    
    
    void setRunId(unsigned int);
    void setEventId(unsigned int);
    void setBx7(unsigned char);
    void setBbcTimeBin(unsigned short);
    
    void setSpinBit(unsigned char);
    void setPolValid(bool);
    void setPolLong(bool);
    void setPolTrans(bool);
    void setBxingMasked(bool);
    void setBxingOffset(int);
    
    void addTrigger(unsigned int);
    void addSimuTrigger(unsigned int);
    
    void addVertex(const StChargedPionVertex*);
    void addTrack(const StChargedPionTrack*);
    void addJet(const StChargedPionJet*);
    //void addJetParticle(StChargedPionJetParticle*);
    
private:
    UInt_t mRunId;
    UInt_t mEventId;
    UChar_t mBx7;
    UShort_t mBbcTimeBin;
    UChar_t mSpinBit;
    UChar_t mSpinQA;
    
    //vector<UInt_t> mTriggers;
    //vector<UInt_t> mSimuTriggers;
    map<unsigned int, unsigned int> mTriggerLookup; //!
    UInt_t mTriggerBits;
    UInt_t mSimuTriggerBits;
    
    TClonesArray *mVertices;
    TClonesArray *mTracks;
    TClonesArray *mJets;
    //TClonesArray *mJetParticles;
    
    enum {kIsPolValid=0x01, kIsPolLong=0x02, kIsPolTrans=0x04, kIsBxingMasked=0x8, kNullOffset=0x10}; //!
    
    ClassDef(StChargedPionEvent,1)
};

inline unsigned int StChargedPionEvent::runId() const { return mRunId; }
inline unsigned int StChargedPionEvent::eventId() const { return mEventId; }
inline unsigned int StChargedPionEvent::bx7() const { return mBx7; }
inline unsigned int StChargedPionEvent::bbcTimeBin() const { return mBbcTimeBin; }

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

//inline unsigned int StChargedPionEvent::nJetParticles() { return mJetParticles->GetEntriesFast(); }
//inline TClonesArray* StChargedPionEvent::jetParticles() { return mJetParticles; }
//inline const TClonesArray* StChargedPionEvent::jetParticles() const { return mJetParticles; }

inline void StChargedPionEvent::setRunId(unsigned int a) { mRunId = a; }
inline void StChargedPionEvent::setEventId(unsigned int a) { mEventId = a; }
inline void StChargedPionEvent::setBx7(unsigned char a) { mBx7 = a; }
inline void StChargedPionEvent::setBbcTimeBin(unsigned short a) { mBbcTimeBin = a; }

inline void StChargedPionEvent::setSpinBit(unsigned char a) { mSpinBit = a; }
inline void StChargedPionEvent::setPolValid(bool a) { a ? mSpinQA |= kIsPolValid : mSpinQA &= ~kIsPolValid; }
inline void StChargedPionEvent::setPolLong(bool a) { a ? mSpinQA |= kIsPolLong : mSpinQA &= ~kIsPolLong; }
inline void StChargedPionEvent::setPolTrans(bool a) { a ? mSpinQA |= kIsPolTrans : mSpinQA &= ~kIsPolTrans; }
inline void StChargedPionEvent::setBxingMasked(bool a) { a ? mSpinQA |= kIsBxingMasked : mSpinQA &= ~kIsBxingMasked; }
inline void StChargedPionEvent::setBxingOffset(int a) { (a==0) ? mSpinQA |= kNullOffset : mSpinQA &= ~kNullOffset; }

#endif
