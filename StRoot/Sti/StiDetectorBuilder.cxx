#include <fstream.h>

#include "Messenger.h"
#include "MessageType.h"
#include "StiDetector.h"
#include "StiDetectorBuilder.h"

StiDetectorBuilder::StiDetectorBuilder():
        m_messenger(*Messenger::instance(MessageType::kDetectorMessage)){
  MessageType::getTypeByCode(MessageType::kDetectorMessage)->setOstream(
      new ofstream("DetectorMessageFile"));
}

StiDetectorBuilder::~StiDetectorBuilder(){
  detectorIterator iterator = mDetectorMap.begin();

  while(iterator != mDetectorMap.end()){
    StiDetector *pDetector = (iterator++)->second;
    delete pDetector;
  }
}

bool StiDetectorBuilder::hasMore() const {
  return mDetectorIterator != mDetectorMap.end();
} // hasMore()

void StiDetectorBuilder::fillNext(StiDetector *detector){
  if(hasMore()){
    StiDetector *pDetector = (mDetectorIterator++)->second;
    detector->copy(*pDetector);
    detector->build();
  }
} // fillNext()

StiMaterial* StiDetectorBuilder::findMaterial(const string& szName) const{
  
  materialMap::const_iterator where = mMaterialMap.find(NameMapKey(szName));
  return (where!= mMaterialMap.end()) ? (*where).second : 0;

} // findMaterial()

StiShape* StiDetectorBuilder::findShape(const string& szName) const{

  shapeMap::const_iterator where = mShapeMap.find(NameMapKey(szName));
  return (where!=mShapeMap.end()) ? (*where).second: 0;

} // findShape()

StiDetector* StiDetectorBuilder::findDetector(const string& szName) const{

  detectorMap::const_iterator where = mDetectorMap.find(NameMapKey(szName));
  return (where!=mDetectorMap.end()) ? (*where).second: 0;

} // findDetector()
