
//StiAbstractFilter
//M.L. Miller (Yale Software)
//3/02

/*! \class StiAbstractFilter
  StiAbstractFilter is a templated base class meant to provide a common interface for
  polymorphic filtering of objects.  We inherit from Observer and store a pointer to
  StiIOBroker so that objects states are handled dynamically.
  
  \author M.L. Miller (Yale Software)
*/

#ifndef StiAbstractFilter_HH
#define StiAbstractFilter_HH


#include "SubjectObserver.h"
class StiIOBroker;

template <class T>
class StiAbstractFilter : public Observer
{
public:

    ///We must pass in valid pointers to Subject and IOBroker
    StiAbstractFilter(Subject* s, StiIOBroker* b) : mSubject(s), mBroker(b) {
	mSubject->attach(this);
    }
    
    virtual ~StiAbstractFilter() {
	if (mSubject) {
	    mSubject->detach(this);
	}
    }
    
    ///Enforce the interface to the filtering of the object of type T
    virtual bool operator()(T) const = 0;

    ///Enforce a polymorphic update of state when the IOBroker notifies:
    virtual void getNewState() = 0;
    
protected:
    Subject* mSubject;
    StiIOBroker* mBroker;
    
private:
    StiAbstractFilter(); //Not implemented
    
    ///Implement the update() function from the base class.
    virtual void update(Subject*);
    virtual void forgetSubject(Subject*);

};

template <class T>
inline void StiAbstractFilter<T>::update(Subject* changedSubject)
{
    if (changedSubject==mSubject) {
	getNewState();
    }   
}

template <class T>
inline void StiAbstractFilter<T>::forgetSubject(Subject* obsolete)
{
    if (obsolete==mSubject) {
	mSubject=0;
    }
}

#endif
