//StiRootDrawableKalmanTrack.h
//M.L. Miller (Yale Software)
//11/01

#ifndef StiRootDrawableKalmanTrack_HH
#define StiRootDrawableKalmanTrack_HH

/*! \class StiRootDrawableKalmanTrack
  StiRootDrawableKalmanTrack is a class used to visualize kalman tracks.

  \author M.L. Miller (Yale Software)

*/

//std
#include <iostream.h>
using std::cout;
using std::endl;

#include <utility>
using std::pair;

//Sti
#include "Sti/SubjectObserver.h"
#include "Sti/StiKalmanTrack.h"

//StiGui
class StiGuiIOBroker;

class StiRootDrawableKalmanTrack : public StiKalmanTrack, public Observer
{
public:
    typedef pair<StiRootDrawableLine*, StiRootDrawableHits*> RDLineHitPair;
    
    StiRootDrawableKalmanTrack();
    virtual ~StiRootDrawableKalmanTrack();

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

inline void StiRootDrawableKalmanTrack::update(Subject* changedSubject)
{
    // cout <<"StiRootDrawableKalmanTrack::update(Subject*)"<<endl;
    if (changedSubject!=mSubject) {
	cout <<"StiRootDrawableKalmanTrack::update(Subject*). ERROR:\t"
	     <<"changedSubject!=mSubject"<<endl;
    }
    else {
	// cout <<"getting new values"<<endl;
	getNewValues();
	// cout <<"\tdone getting new values"<<endl;
    }
}

inline void StiRootDrawableKalmanTrack::forgetSubject(Subject* obsolete)
{
    // cout <<"StiRootDrawableKalmanTrack::forgetSubject(Subject*)"<<endl;
    if (obsolete==mSubject) {
	mSubject=0;
    }
    else {
	cout <<"StiRootDrawableKalmanTrack::forgetSubject(Subject*). ERROR:\t"
	     <<"changedSubject!=mSubject"<<endl;
    }
}

#endif
