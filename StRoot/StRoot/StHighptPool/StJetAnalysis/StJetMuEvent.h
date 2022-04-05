//StJetMuEvent.h
//A class to be put in a TTree

#ifndef StJetMuEvent_HH
#define StJetMuEvent_HH

#include "TObject.h"
#include "StThreeVectorF.hh"
#include "AnaCuts.h"

class TClonesArray;
class StMuDstMaker;
class StMuEvent;
class StMuTrack;

class StJetMuEvent : public TObject
{
public:
    StJetMuEvent();
    virtual ~StJetMuEvent();

    //Fill the event
    bool fill(StMuDstMaker*);

    //Set the cut object
    void setCuts(const AnaCuts& cuts) {mCuts=cuts;}

    //access to tracks
    TClonesArray* tracks() {return mTracks;}

    //Basic types

    //Pair combinations that passed our cuts
    unsigned short nPlusPlus() const {return mPlusPlus;}
    unsigned short nMinusMinus() const {return mMinusMinus;}
    unsigned short nPlusMinus() const {return mPlusMinus;}

    //Basic event info
    int eventId() const {return mEventId;}
    int eventNumber() const {return mEventNumber;}
    int runId() const {return mRunId;}
    int runNumber() const {return mRunNumber;}
    int triggerWord() const {return mTriggerWord;}
    
    //Event characterization
    unsigned short refMultPos() const {return mRefMultPos;}
    unsigned short refMultNeg() const {return mRefMultNeg;}
    unsigned short refMult() const {return mRefMult;}

    double magneticField() const {return mMagneticField;}
    double zdcAdcAttenuatedSumWest() const {return mZdcAdcAttenuatedSumWest;}
    double zdcAdcAttenuatedSumEast() const {return mZdcAdcAttenuatedSumEast;}
    double ctbMultiplicity() const {return mCtbMultiplicity;}
    StThreeVectorF primaryVertexPosition() const {return mPrimaryVertexPosition;}

    double reactionPlane(unsigned short);
    double reactionPlanePtWgt(unsigned short);
    
private:
    bool accept(StMuDstMaker*);
    bool acceptTrack(StMuTrack*);
    void reset();
    void fillBasicTypes(StMuEvent* event);

private:
    
    //Basic types:

    ///Number of various pairs for this event at time of filter:
    unsigned short mPlusPlus;
    unsigned short mMinusMinus;
    unsigned short mPlusMinus;
    Bool_t mL3Fired;
    
    int mEventId;
    int mEventNumber;
    int mRunId;
    int mRunNumber;
    int mTriggerWord;

    unsigned short mRefMultPos;
    unsigned short mRefMultNeg;
    unsigned short mRefMult;

    double mMagneticField;
    double mZdcAdcAttenuatedSumWest;
    double mZdcAdcAttenuatedSumEast;
    double mCtbMultiplicity;
    StThreeVectorF mPrimaryVertexPosition;

    //TClones array of tracks
    int mTrackCounter;
    TClonesArray* mTracks;

    //Encapsulation of various cuts
    AnaCuts mCuts; //! Don't want this stored, don't know why CINT grabs it
    
    ClassDef(StJetMuEvent,1)
};

#endif
