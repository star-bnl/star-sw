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

void StiDetectorLayerContainer::build(const char* buildDirectory)
{
    char* buildfile = new char[200];
    for (int sector=1; sector<=24; ++sector) {
	for (int padrow=1; padrow<=45; ++padrow) {
	    
	    sprintf(buildfile, "%sTpc/Sector_%i/Padrow_%i.txt", buildDirectory, static_cast<int>(sector), padrow);
	    //cout <<"buildfile:\t"<<buildfile<<endl;
	    StiDetector* layer = new StiDetector();
	    layer->build(buildfile);
	    if (layer->isOn()) push_back(layer);
	}
    }
    return;
}

void StiDetectorLayerContainer::print() const
{
    cout <<"\nStiDetectorLayerContainer::print()"<<endl;
    for (detectormap::const_iterator it= begin(); it!=end(); it++) {
	cout <<"\tKey:\t"<<(*it).first<<"\tDetectorLayer:\t"<<*(*it).second<<endl;
    }
    return;
}






