#ifndef ST_CHARGED_PION_HEADER_HH
#define ST_CHARGED_PION_HEADER_HH

#include <map>
using std::map;

#include "TObject.h"

class StChargedPionHeader : public TObject
{
public:
    StChargedPionHeader();
    virtual ~StChargedPionHeader();
    
    float        fill();
    unsigned int run();
    unsigned int nEvents();
    unsigned int nEventsWithVertex();
    float        prescale(int trigId);
    
    void setFill(float);
    void setRun(unsigned int);
    void setNEvents(unsigned int);
    void setNEventsWithVertex(unsigned int);
    void addPrescale(int trigId, float prescale);
    
private:
    Float_t mFill;
    UInt_t mRunId;
    UInt_t mEventCount;
    UInt_t mEventsWithVertex;
    map<UInt_t,Float_t> mTriggerPrescales;
    
    ClassDef(StChargedPionHeader,1)
};

inline float StChargedPionHeader::fill() { return mFill; }
inline unsigned int StChargedPionHeader::run() { return mRunId; }
inline unsigned int StChargedPionHeader::nEvents() { return mEventCount; }
inline unsigned int StChargedPionHeader::nEventsWithVertex() { return mEventsWithVertex; }

inline void StChargedPionHeader::setFill(float a) { mFill = a; }
inline void StChargedPionHeader::setRun(unsigned int a) { mRunId = a; }
inline void StChargedPionHeader::setNEvents(unsigned int a) { mEventCount = a; }
inline void StChargedPionHeader::setNEventsWithVertex(unsigned int a) { mEventsWithVertex = a; }
inline void StChargedPionHeader::addPrescale(int trigId, float prescale) { mTriggerPrescales[trigId] = prescale; }

#endif
