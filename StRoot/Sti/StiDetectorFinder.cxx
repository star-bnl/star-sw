#include "StiDetector.h" 
#include "StiDetectorFinder.h"

StiDetectorFinder *StiDetectorFinder::sInstance = NULL;

StiDetectorFinder *StiDetectorFinder::instance(){
    return (sInstance) ? sInstance : new StiDetectorFinder();
} // instance()

void StiDetectorFinder::kill()
{
    if (sInstance) {
	delete sInstance;
	sInstance = 0;
    }
}
	
void StiDetectorFinder::addDetector(StiDetector *pDetector){
  mDetectorMap.insert( detectorMapValType( NameMapKey(pDetector->getName()),
                                           pDetector ) );
} // addDetector()

StiDetector *StiDetectorFinder::findDetector(const string& szName)
{
    detectorIterator where = mDetectorMap.find(NameMapKey(szName));
    return (where!= mDetectorMap.end()) ? (*where).second : NULL;
} // findDetector()
