//StiObjectFactory.h
//Original Author: Claude Pruneau (Wayne State U)
//Converted to template 5/17/01, Mike Miller (Yale)

#ifndef StiObjectFactory_H
#define StiObjectFactory_H

#include <iostream>
#include <vector>
#include <string>

using std::vector;
using std::cout;
using std::endl;
using std::ostream;
using std::string;

class StiObjectFactory 
{
public:

    typedef vector<void*> t_vector;
    StiObjectFactory(const string& newName, int original, int incremental, int maxInc);
    
    virtual ~StiObjectFactory();

    // Declare all objects owned by this container as unused.
    void reset();
    
    void setIncrementalSize(int);
    
    void setMaxIncrementCount(int);
    
    int getIncrementalSize() const;
    
    int getMaxIncrementCount() const;

    int getCurrentSize() const;
    
    const string& getName() const;
    
protected:

    //Author of derived classes must call initialize after construction.
    //Internal protection against re-initialization;
    void initialize();
    
    void* returnObject();
    
    virtual void* makeNewObject() const = 0;

    //Sub class must clean up vector    
    void clearAndDestroy();
    virtual void destroyObject(void*) = 0;

private:
    
    StiObjectFactory(); //Not implemented
    
    enum StiObjectFacotryDefaults {defaultMaxIncrementCount = 10, 
				   defaultIncrementSize=5000,defaultOriginalSize=10000};
    
    void createObjects(int);
    bool initialized;
    string name;
    int originalSize;
    int incrementalSize;
    int maxIncrementCount;
    int incrementCount;

    t_vector container;
    t_vector::iterator mCurrent;    
    
    
};


inline void StiObjectFactory::reset()  
{
    mCurrent = container.begin();
}

inline void StiObjectFactory::setIncrementalSize(int increment) 
{
    incrementalSize = increment;
}   

inline void StiObjectFactory::setMaxIncrementCount(int maxCount) 
{
    maxIncrementCount = maxCount;
}    

inline int StiObjectFactory::getIncrementalSize() const 
{
    return incrementalSize;
}

inline int StiObjectFactory::getMaxIncrementCount() const 
{
    return maxIncrementCount;
}

inline int StiObjectFactory::getCurrentSize() const 
{
    return container.size();
}

inline void StiObjectFactory::initialize()
{
    if (initialized==false) {
	createObjects(originalSize);
	initialized=true;
    }
}

inline const string& StiObjectFactory::getName() const
{
    return name;
}

#endif

