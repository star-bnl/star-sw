//SubjectObserver.h
//M.L. Miller (Yale Software)
//11/01

#ifndef SubjectObserver_HH
#define SubjectObserver_HH

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
    virtual ~Observer();
    
    virtual void update(Subject* changedSubject) = 0;
    virtual void forgetSubject(Subject* theObsoleteSubject)=0;
    
protected:
};

#endif
