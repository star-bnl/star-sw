//StiTrackMerger.h
//M.L. Miller (Yale Software)
//12/01

/*! \class StiTrackMerger
  StiTrackMerger is a pure virtual class that defines the interface for a class that
  encapsulates a track-merging algorithm.  It's purpose is to
  combine track segments that belong to the same trajectory, but were initially reconstructed
  as seperate pieces.
  
  \author M.L. Miller (Yale Software)
  
  \note All information passed to and from an instance of StiTrackMerger is performed
  via the pointer to StiTrackContainer.

 */

#ifndef StiTrackMerger_HH
#define StiTrackMerger_HH

#include "SubjectObserver.h"

class StiTrackContainer;

class StiTrackMerger : public Observer
{
public:

    ///One must provide a valid pointer to the track container.
    StiTrackMerger(StiTrackContainer*);
    
    virtual ~StiTrackMerger();

    ///Merge the tracks in the track container.
    virtual void mergeTracks() = 0;

    //Implmentation of Observer pattern
    virtual void update(Subject* changedSubject);
    virtual void forgetSubject(Subject* theObsoleteSubject);

protected:
    StiTrackMerger(); //This is not implemented
    virtual void getNewState() = 0;
    
    StiTrackContainer* mTrackStore;
    Subject* mSubject;
};


//inlines

inline void StiTrackMerger::update(Subject* changedSubject)
{
    if (changedSubject!=mSubject) {
	cout <<"StiTrackMerger::update(Subject*). ERROR:\t"
	     <<"changedSubject!=mSubject"<<endl;
    }
    else {
	getNewState();
    }   
}

inline void StiTrackMerger::forgetSubject(Subject* obsolete)
{
    if (obsolete==mSubject) {
	mSubject=0;
    }
    else {
	cout <<"StiTrackMerger::forgetSubject(Subject*). ERROR:\t"
	     <<"changedSubject!=mSubject"<<endl;
    }
}

#endif
