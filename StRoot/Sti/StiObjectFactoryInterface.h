//StiObjectFactoryInterface

#ifndef StiObjectFactoryInterface_HH
#define StiObjectFactoryInterface_HH

#include <string>
using std::string;

#include "StiObjectFactory.h"

//templated interface that returns (maybe) polymorphic objects

template <class T>
class StiObjectFactoryInterface : public StiObjectFactory
{
public:
    StiObjectFactoryInterface(const string& newName, int original, int incremental, int maxInc);
    virtual ~ StiObjectFactoryInterface();
    
    virtual T* getObject();
    
protected:
    virtual void* makeNewObject() const = 0;
    virtual void destroyObject(void*);
    
private:
    StiObjectFactoryInterface(); //Not implemented
};

//Implementation

template <class T>
StiObjectFactoryInterface<T>::StiObjectFactoryInterface(const string& newName,
							int original, int incremental, int maxInc)
    : StiObjectFactory(newName, original, incremental, maxInc)
{
}

template <class T>
StiObjectFactoryInterface<T>::~StiObjectFactoryInterface()
{
    clearAndDestroy();
}

template <class T>
inline void StiObjectFactoryInterface<T>::destroyObject(void* obj)
{
    T* temp = static_cast<T*>(obj);
    delete temp;
    temp=0;
}

template <class T>
inline T* StiObjectFactoryInterface<T>::getObject()
{
    return static_cast<T*>(returnObject());
}

#endif
