//StiObjectFactory.cxx
//M.L. Miller (Yale Software)
//09/01

#include "StiObjectFactory.h"

StiObjectFactory::StiObjectFactory(const string& newName,  int original, int incremental, int maxInc)
    : initialized(false), name(newName)
{
    originalSize  = (original>0) ? original : defaultOriginalSize;
    incrementalSize = (incremental>0) ? incremental : defaultIncrementSize;
    maxIncrementCount = (maxInc>0) ? maxInc : defaultMaxIncrementCount;
    incrementCount = 0;
    
    mCurrent = container.begin();
}

StiObjectFactory::~StiObjectFactory() 
{
    // cout <<"StiObjectFactor destructor.  Deleting "<<getName()<<endl;
    //for (t_vector::iterator it=container.begin(); it!=container.end(); ++it) {
    //delete (*it);
    //(*it) = 0;
    //}
}

void StiObjectFactory::clearAndDestroy()
{
    for (t_vector::iterator it=container.begin(); it!=container.end(); ++it) {
	destroyObject(*it);
    }
    container.clear();
}

void* StiObjectFactory::returnObject() 
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
	    cout << "StiObjectFactory::getObject() - FATAL" <<endl;
	    cout << "     Too many expension request " <<endl;
	    cout << "     incrementCount : " << incrementCount << endl;
	    return 0;
	}
    }
}

void StiObjectFactory::createObjects(int n) 
{
    int currentDistance = mCurrent-container.begin();
    container.reserve( container.size()+n );
    for (int i=0;i<n; ++i) {
	container.push_back( makeNewObject() );
    }
    mCurrent = container.begin()+currentDistance;
}

