#ifndef StTimeRandomizerMaker_StTimeRandomizerMaker_H
#define StTimeRandomizerMaker_StTimeRandomizerMaker_H

#include <list>
using std::list;

#include <TRandom3.h>

#include <StMaker.h>

class StTimeRandomizerMaker : public StMaker {
public:
    typedef StMaker inherited;
    typedef TRandom3 random_type;

    StTimeRandomizerMaker(const Char_t *name = "StTimeRandomizerMaker");
    virtual ~StTimeRandomizerMaker();
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();

    const TString &getRunTimesFilename() const;
    void setRunTimesFilename(const TString &filename);

    Float_t getNormalizeEventsTotal() const;
    void setNormalizeEventsTotal(Float_t evNum);

    const TString &getDatasetNameStEvent() const;
    void setDatasetNameStEvent(const TString &name);

    UInt_t getSeed() const;
    void setSeed(UInt_t seed);

    ClassDef(StTimeRandomizerMaker, 1);

protected:
    TString mRunTimesFilename; // text file with requested timestamps and corresponding statistics
    Float_t mNormalizeEventsTotal; // if not zero, tells in how many events the requested list must be covered
    TString mDatasetNameStEvent; // StEvent dataset name to use, default is "StEvent", one may also try "IO_Root/.data/bfcTree/eventBranch/StEvent"
    UInt_t mSeed; // random number generator seed to be used in Init()

    list<Int_t> mRunsUsed;
    list<Int_t> mRuns;
    list<Int_t> mDates;
    list<Int_t> mTimes;
    list<Float_t> mEvents;
    Float_t mEventsTotal;
    random_type mRandom;
    Int_t mEventNum;
    Float_t mNormalizedEventsCounter;
};

#endif
