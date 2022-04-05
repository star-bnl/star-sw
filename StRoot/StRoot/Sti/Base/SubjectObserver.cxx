//SubjectObserver.cxx
//M.L. Miller (Yale Software)
//11/01

//std
#include "Stiostream.h"
using std::cout;
using std::endl;
#include <algorithm>
using std::find;

//Sti
#include "SubjectObserver.h"

Subject::Subject()
{
    // cout <<"Subject::Subject()"<<endl;
}

Subject::~Subject()
{
    // cout <<"Subject::~Subject()"<<endl;
    for (ObserverVec::iterator it=mObservers.begin(); it!=mObservers.end(); ++it) {
	(*it)->forgetSubject(this);
    }
}

void Subject::attach(Observer* obs)
{
    // cout <<"Subject::attach()"<<endl;
    //check for existence before we add again:
    ObserverVec::iterator where = find(mObservers.begin(), mObservers.end(), obs);
    if (where==mObservers.end()) {
	mObservers.push_back(obs);
    }
    else {
	cout <<"Subject::attach(). ERROR:\t"
	     <<"observer already known to subject"<<endl;
    }
    // cout <<"\tSubject::attach() done"<<endl;
}

void Subject::detach(Observer* obs)
{
    // cout <<"Subject::detach()"<<endl;
    ObserverVec::iterator where = find(mObservers.begin(), mObservers.end(), obs);
    if (where!=mObservers.end()) {
	mObservers.erase(where);
    }
    else {
	cout <<"Subject::detach(). ERROR:\t"
	     <<"observer not known to subject"<<endl;
    }
    // cout <<"\tSubject::detach() done"<<endl;
}

void Subject::notify()
{
  //cout <<"Subject::notify()"<<endl;
  for (ObserverVec::iterator it=mObservers.begin(); it!=mObservers.end(); ++it) 
    (*it)->changed(this);
}

Observer::Observer()
  : mSubject(0)
{}

Observer::Observer(Subject * subject)
  : mSubject(subject)
{}

Observer::~Observer()
{}

void Observer::changed(Subject* changedSubject)
{
  //if (changedSubject!=mSubject) 
  //  cout <<"Observer::changed(Subject*) - ERROR - changedSubject!=mSubject"<<endl;
  //else 
  getNewState();
}
