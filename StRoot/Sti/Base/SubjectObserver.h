//SubjectObserver.h
//M.L. Miller (Yale Software)
//11/01

#ifndef SubjectObserver_HH
#define SubjectObserver_HH
#include "Stiostream.h"
#include <vector>
using std::vector;

#include <algorithm>
using std::for_each;

class Subject;
class Observer;

class Subject
{
public:
    ///For convenience
    typedef vector<Observer*> ObserverVec;
    
    //ctr-dstr
    Subject();
    virtual ~Subject();
    
    //gets/sets
    virtual void attach(Observer*);
    virtual void detach(Observer*);
    
    //Action
    virtual void notify();
    
protected:
    ObserverVec mObservers;
};

class Observer
{
public:
    
    Observer();
    Observer(Subject * subject);
    virtual ~Observer();
    virtual void getNewState()=0;
    virtual void changed(Subject* changedSubject);
    virtual void forgetSubject(Subject* theObsoleteSubject);
    
protected:  
    
    Subject* mSubject;
};

/*
  inline void Observer::changed(Subject* changedSubject)
  {
  if (changedSubject!=mSubject) 
  cout <<"Observer::changed(Subject*) - ERROR - changedSubject!=mSubject"<<endl;
  else 
  getNewState();
  }*/

inline void Observer::forgetSubject(Subject* obsolete)
{
  if (obsolete==mSubject) 
      mSubject=0;
  else 
      cout <<"Observer::forgetSubject(Subject*) - ERROR - obsolete!=mSubject"<<endl;
}

#endif
