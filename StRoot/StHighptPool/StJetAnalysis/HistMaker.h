//HistMaker.h

#ifndef HistMaker_HH
#define HistMaker_HH

#include "TObject.h"
#include "AnaCuts.h"

class StUpsilonMuEvent;
class StMuTrack;
class TH1;

class HistMaker
{
public:
    HistMaker();
    virtual ~HistMaker();

    //Gets/sets
    void setCuts(const AnaCuts& c) {mCuts=c;}
    
    TH1* plusPlus() const {return mPlusPlus;}
    TH1* minusMinus() const {return mMinusMinus;}
    TH1* plusMinus() const {return mPlusMinus;}
    
    //action
    void fill(StUpsilonMuEvent*);
    
private:
    bool acceptTrack(StMuTrack* track);

    TH1* mPlusPlus; //!
    TH1* mMinusMinus; //!
    TH1* mPlusMinus; //!
    
    //Work variables
    unsigned int mAllInspectedEvents;   //!
    unsigned int mCandidateEvents;      //!
    unsigned int mUnlikeSignEvents;     //!
    unsigned int mLikeSignEvents;       //!
    unsigned int mPlusPlusPairs;        //!
    unsigned int mMinusMinusPairs;      //!
    unsigned int mUnlikeSignPairs;      //!

    AnaCuts mCuts;
    
    ClassDef(HistMaker,1)
};
#endif
