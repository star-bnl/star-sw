//StiRootDrawableStiEvaluableTrack.h
//M.L. Miller (Yale Software)
//07/01

#ifndef StiRootDrawableStiEvaluableTrack_HH
#define StiRootDrawableStiEvaluableTrack_HH


class StiRootDrawableLine;
class StiRootDrawableHits;

//std
#include <iostream.h>
using std::cout;
using std::endl;

#include <utility>
using std::pair;

//Sti
#include "Sti/SubjectObserver.h"
#include "Sti/StiEvaluableTrack.h"

//StiGui
class StiGuiIOBroker;

class StiRootDrawableStiEvaluableTrack : public StiEvaluableTrack, public Observer
{
public:
    typedef pair<StiRootDrawableLine*, StiRootDrawableHits*> RDLineHitPair;
    
    StiRootDrawableStiEvaluableTrack();
    virtual ~StiRootDrawableStiEvaluableTrack();

    virtual void update();
    virtual void fillHitsForDrawing();
    virtual void reset();

    //Inherited from Observer
    virtual void update(Subject* changedSubject);
    virtual void forgetSubject(Subject* theObsoleteSubject);
    
protected:
    RDLineHitPair mLineHitPair;
    
private:
    void setLineInfo();
    void getNewValues();
    
    StiGuiIOBroker* mBroker;
    Subject* mSubject;
};

//inlines

inline void StiRootDrawableStiEvaluableTrack::update(Subject* changedSubject)
{
    // cout <<"StiRootDrawableStiEvaluableTrack::update(Subject*)"<<endl;
    if (changedSubject!=mSubject) {
	cout <<"StiRootDrawableStiEvaluableTrack::update(Subject*). ERROR:\t"
	     <<"changedSubject!=mSubject"<<endl;
    }
    else {
	// cout <<"getting new values"<<endl;
	getNewValues();
	// cout <<"\tdone getting new values"<<endl;
    }
}

inline void StiRootDrawableStiEvaluableTrack::forgetSubject(Subject* obsolete)
{
    // cout <<"StiRootDrawableStiEvaluableTrack::forgetSubject(Subject*)"<<endl;
    if (obsolete==mSubject) {
	mSubject=0;
    }
    else {
	cout <<"StiRootDrawableStiEvaluableTrack::forgetSubject(Subject*). ERROR:\t"
	     <<"changedSubject!=mSubject"<<endl;
    }
}

#endif
