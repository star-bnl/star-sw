//StiObjectFactory.h
//Original Author: Claude Pruneau (Wayne State U)
//Converted to template 5/17/01, Mike Miller (Yale)

#ifndef StiObjectFactory_H
#define StiObjectFactory_H

#include "StiFactory.h"
#include <iostream>
#include <vector>

template <class T>
class StiObjectFactory : public StiFactory
{
 public:

    enum StiObjectFacotryDefaults {defaultMaxIncrementCount = 10, defaultIncrementSize=5000,
				   defaultOriginalSize=10000};
    typedef vector<T*> t_vector;

    StiObjectFactory( const char* newName, int original=-1, int incremental=-1, int maxInc=-1)
	: StiFactory(newName)
    {
	originalSize          = original>0    ? original    : defaultOriginalSize;
	incrementalSize       = incremental>0 ? incremental : defaultIncrementSize;
	maxIncrementCount     = maxInc>0      ? maxInc      : defaultMaxIncrementCount;
	currentSize      = 0;
	nextObjectIndex  = 0;
	incrementCount   = 0;
	
	createObjects(originalSize);
	currentSize = container.size();
    }
    
    virtual ~StiObjectFactory() {
	for (t_vector::iterator it=container.begin(); it!=container.end(); ++it) {
	    delete (*it);
	    (*it) = 0;
	}
    }
    
    inline void reset()  {// Declare all objects owned by this container as unused.
	nextObjectIndex    = 0; 
    }
    
    inline void setIncrementalSize(int increment) {// Set the increment used in further expension of the container
	incrementalSize = increment;
    }
    
    inline void setMaxIncrementCount(int maxCount) {//Set the max number of permitted expension
	maxIncrementCount = maxCount;
    }
    
    inline int getOriginalSize() const {//Get the original size of the container
	return originalSize;
    }
	
    inline int getIncrementalSize() const {// Get the increment used in further expensions of the container
	return incrementalSize;
    }
    
    inline int getMaxIncrementCount() const {// Get the max number of permitted expension
	return maxIncrementCount;
    }

    inline int getCurrentSize() const {// Get the current size of the container
	return container.size();
    }

    inline int getNextObjectIndex() const {
	// Get the index  of the next object (within the container) to be served by the factory
	return nextObjectIndex;
    }
    
    T* getObject() {// serve the next object
	if ( nextObjectIndex<getCurrentSize() ) {
	    // return next object available
	    return container[nextObjectIndex++];
	}
	else {
	    if (incrementCount< maxIncrementCount) {
		// expand container size
		++incrementCount;
		createObjects(incrementalSize);
		currentSize = container.size();
		return container[nextObjectIndex++];
	    }
	    else {
		// s.o.l.
		cout << "StiObjectFactory::getObject() - FATAL" << endl
		     << "     Too many expension request " << endl
		     << "     incrementCount : " << incrementCount << endl;
		return 0;
	    }
	}
    }
    
protected:
    
    //static int defaultOriginalSize;
    //static int defaultIncrementSize;
    //static int defaultMaxIncrementCount;
    
    
    virtual void createObjects(int n) {
	for (int i=0;i<n; ++i) {
	    container.push_back( new T() );
	}
	currentSize = container.size();
    }
    
    int originalSize;
    int incrementalSize;
    int maxIncrementCount;
    int currentSize;
    int nextObjectIndex;
    int incrementCount;

    t_vector container;

private:
    
};

#endif
