#include "StiDetector.h" 
#include "StiDetectorFinder.h"

StiDetectorFinder *StiDetectorFinder::sInstance = NULL;

StiDetectorFinder *StiDetectorFinder::instance(){
  if (sInstance==NULL){ new StiDetectorFinder; }
  return sInstance;
} // instance()

void StiDetectorFinder::addDetector(StiDetector *pDetector){
  mDetectorMap.insert( detectorMapValType( NameMapKey(pDetector->getName()),
                                           pDetector ) );
} // addDetector()

StiDetector *StiDetectorFinder::findDetector(const char *szName){
  detectorIterator where = mDetectorMap.find(NameMapKey(szName));
  return (where!= mDetectorMap.end()) ? (*where).second : NULL;
} // findDetector()
