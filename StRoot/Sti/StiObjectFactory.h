
#ifndef StiObjectFactory_H
#define StiObjectFactory_H 1

#include "StiFactory.h"
#include <vector>

class TObject;

class StiObjectFactory : public StiFactory
{
 public:

    typedef vector<TObject*> tobject_vector;

    //StiObjectFactory(); //Not implemented
    StiObjectFactory( const char *newName,
		      int original, 
		      int incremental=-1, 
		      int maxIncrement=-1);
    virtual ~StiObjectFactory();
    
    void reset();
    void setIncrementalSize(int increment);
    void setMaxIncrementCount(int maxCount);
    
    int getOriginalSize();
    int getIncrementalSize();
    int getMaxIncrementCount();
    int getCurrentSize();
    int getNextObjectIndex();
    
    
protected:
    
    static int defaultOriginalSize;
    static int defaultIncrementSize;
    static int defaultMaxIncrementCount;
    
    TObject * getObject();
    virtual void createObjects(int n)=0;
    
    int originalSize;
    int incrementalSize;
    int maxIncrementCount;
    int currentSize;
    int nextObjectIndex;
    int incrementCount;

    tobject_vector container;

private:
    void init();
    
};

#endif
