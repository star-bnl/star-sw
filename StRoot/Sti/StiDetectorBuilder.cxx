#include "StiDetector.h"
#include "StiDetectorBuilder.h"

StiDetectorBuilder::StiDetectorBuilder(){
}

StiDetectorBuilder::~StiDetectorBuilder(){
  detectorIterator iterator = mDetectorVector.begin();

  while(iterator != mDetectorVector.end()){
    StiDetector *pDetector = *(iterator++);
    delete pDetector;
  }
}

bool StiDetectorBuilder::hasMore() const {
  return mDetectorIterator != mDetectorVector.end();
} // hasMore()

void StiDetectorBuilder::fillNext(StiDetector *detector){
  if(hasMore()){
    StiDetector *pDetector = *(mDetectorIterator++);
    detector->copy(*pDetector);
    detector->build(NULL);
  }
} // fillNext()
