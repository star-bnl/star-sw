//StiDetectorLayerContainer.cxx
//M.L. Miller (Yale Software)
//02/02/01

#include <iostream.h>

#include "StiMapUtilities.h"
#include "StiDetector.h"
#include "StiDetectorLayerContainer.h"

StiDetectorLayerContainer::StiDetectorLayerContainer()
{
    cout <<"StiDetectorLayerContainer::StiDetectorLayerContainer()"<<endl;
}

StiDetectorLayerContainer::~StiDetectorLayerContainer()
{
    cout <<"StiDetectorLayerContainer::~StiDetectorLayerContainer()"<<endl;
    clearAndDestroy();
}

void StiDetectorLayerContainer::clearAndDestroy()
{
  for (detectormap::iterator it=begin(); it!=end(); ++it) {
    delete (*it).second;
    (*it).second=0;
  }
  clear();
  return;
}

void StiDetectorLayerContainer::push_back(StiDetector* layer)
{
  mkey.position = layer->getPosition();
  mkey.refangle = layer->getRefAngle();
  mkey.z = layer->getZCenter();
  insert( detectorMapValType( mkey,layer ) );
  return;
}

void StiDetectorLayerContainer::print() const
{
    cout <<"\nStiDetectorLayerContainer::print()"<<endl;
    for (detectormap::const_iterator it= begin(); it!=end(); it++) {
	//cout <<"\tKey:\t"<<(*it).first<<"\tDetectorLayer:\t"<<*(*it).second<<endl;
    }
    return;
}






