//StiObjectFactory.h
//Original Author: Claude Pruneau (Wayne State U)
//Converted to template 5/17/01, Mike Miller (Yale)

#ifndef StiObjectFactory_H
#define StiObjectFactory_H

#include "StiFactory.h"
#include <iostream>
#include <vector>

using std::vector;
using std::cout;
using std::endl;
using std::ostream;

template <class T>
class StiObjectFactory : public StiFactory
{
public:
    
    typedef vector<T*> t_vector;
    
    StiObjectFactory(const char* newName, int original=-1, int incremental=-1, int maxInc=-1);
    
    virtual ~StiObjectFactory();
    
    // Declare all objects owned by this container as unused.
    void reset();
    
    void setIncrementalSize(int);
    
    void setMaxIncrementCount(int);
    
    int getIncrementalSize() const;
    
    int getMaxIncrementCount() const;
    
    int getCurrentSize() const;
    
    T* getObject();
    
protected:
    
    enum StiObjectFacotryDefaults {defaultMaxIncrementCount = 10, 
				   defaultIncrementSize=5000,defaultOriginalSize=10000};
    
    virtual void createObjects(int);
    
    int originalSize;
    int incrementalSize;
    int maxIncrementCount;
    int incrementCount;
    
    t_vector container;
    t_vector::iterator mCurrent;
    
private:
    
};

template <class T>
StiObjectFactory<T>::StiObjectFactory(const char* newName, int original, int incremental, int maxInc)
    : StiFactory(newName)
{
    originalSize = (original>0) ? original : defaultOriginalSize;
    incrementalSize = (incremental>0) ? incremental : defaultIncrementSize;
    maxIncrementCount = (maxInc>0) ? maxInc : defaultMaxIncrementCount;
    incrementCount = 0;
    
    mCurrent = container.begin();
    createObjects(originalSize);
}

template <class T>
StiObjectFactory<T>::~StiObjectFactory() 
{
    for (t_vector::iterator it=container.begin(); it!=container.end(); ++it) {
	delete (*it);
	(*it) = 0;
    }
}

template <class T>
inline void StiObjectFactory<T>::reset()  
{
    mCurrent = container.begin();
}

template <class T>    
inline void StiObjectFactory<T>::setIncrementalSize(int increment) 
{
    incrementalSize = increment;
}
    
template <class T>
inline void StiObjectFactory<T>::setMaxIncrementCount(int maxCount) 
{
    maxIncrementCount = maxCount;
}

template <class T>	
inline int StiObjectFactory<T>::getIncrementalSize() const 
{
    return incrementalSize;
}

template <class T>    
inline int StiObjectFactory<T>::getMaxIncrementCount() const 
{
    return maxIncrementCount;
}

template <class T>
inline int StiObjectFactory<T>::getCurrentSize() const 
{
    return container.size();
}

template <class T>
T* StiObjectFactory<T>::getObject() 
{
    if ( mCurrent < container.end() ) {
	return *mCurrent++;
    }
    
    else {
	if (incrementCount< maxIncrementCount) {
	    // expand container size
	    ++incrementCount;
	    createObjects(incrementalSize);
	    return *mCurrent++;
	}
	else {
	    // s.o.l.
	    cout << "StiObjectFactory::getObject() - FATAL" << endl;
	    cout << "     Too many expension request " << endl;
	    cout << "     incrementCount : " << incrementCount << endl;
	    return 0;
	}
    }
}

template <class T>
void StiObjectFactory<T>::createObjects(int n) 
{
    int currentDistance = mCurrent-container.begin();
    container.reserve( container.size()+n );
    for (int i=0;i<n; ++i) {
	container.push_back( new T() );
    }
    mCurrent = container.begin()+currentDistance;
}

#endif

