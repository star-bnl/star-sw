//StiDetectorLayerContainer.cxx
//M.L. Miller (Yale Software)
//02/02/01

#include <iostream.h>

#include "StiMapUtilities.h"
#include "StiDetector.h"
#include "StiDrawableDetector.h"
#include "StiDetectorLayerContainer.h"

StiDetectorLayerContainer* StiDetectorLayerContainer::sinstance = 0;

StiDetectorLayerContainer* StiDetectorLayerContainer::instance()
{
    return (sinstance) ? sinstance : new StiDetectorLayerContainer();
}

void StiDetectorLayerContainer::kill()
{
    if (sinstance) {
	delete sinstance;
	sinstance = 0;
    }
    return;
}

StiDetectorLayerContainer::StiDetectorLayerContainer() : mdraw(true)
{
    cout <<"StiDetectorLayerContainer::StiDetectorLayerContainer()"<<endl;
    sinstance = this;
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
    mkey.padrow = layer->getPadrow();
    mkey.sector = layer->getSector();
    mkey.z = layer->getZCenter();
    insert( detectorMapValType( mkey,layer ) );
    return;
}

void StiDetectorLayerContainer::reset()
{
  mcurrent=begin();
  mkey.sector=(*mcurrent).first.sector;
  mkey.padrow=(*mcurrent).first.padrow;
  
  return;
}

const StiDetector* StiDetectorLayerContainer::operator*() const
{
  return (mcurrent!=end()) ? (*mcurrent).second : 0;
}

bool StiDetectorLayerContainer::sectorStepPlus()
{
  DetectorMapKey nextkey;
  nextkey.padrow = (*mcurrent).first.padrow+1;
  if ( (*mcurrent).first.sector!=mmaxsector) nextkey.sector=(*mcurrent).first.sector+1;
  else nextkey.sector = mminsector;
  nextkey.z = 1e6;

  detectormap::const_iterator where = lower_bound( nextkey );
  if (where==end()) {
    return false;
  }
  
  else {
    mcurrent = where;
    return true;
  }  
}

bool StiDetectorLayerContainer::sectorStepMinus()
{  
  DetectorMapKey nextkey;
  nextkey.padrow = (*mcurrent).first.padrow+1;
  if ( (*mcurrent).first.sector!=mminsector) nextkey.sector=(*mcurrent).first.sector-1;
  else nextkey.sector = mmaxsector;
  nextkey.z = 1e6;

  detectormap::const_iterator where = lower_bound( nextkey );
  if (where==end()) {
    return false;
  }
  
  else {
    mcurrent = where;
    return true;
  }  
}

bool StiDetectorLayerContainer::padrowStepMinus()
{
  int sector = (*mcurrent).first.sector;
  ++mcurrent;
  if (mcurrent==end()) {
    --mcurrent;
    return false;
  }

  if ( (*mcurrent).first.sector == sector ) return true;
  
  else {
    --mcurrent;
    return false;
  }
}

bool StiDetectorLayerContainer::padrowStepPlus()
{
  if (mcurrent==begin()) return false; //nowhere to go

  int sector = (*mcurrent).first.sector;
  --mcurrent;

  if ( (*mcurrent).first.sector == sector ) {
    return true;
  }
  
  else {
    ++mcurrent;
    return false;
  }
}

void StiDetectorLayerContainer::setRefDetector(int sector)
{
  mkey.sector = sector;
  mkey.padrow = 1000000;
  mkey.z = 1e6;

  detectormap::const_iterator where = lower_bound(mkey);
  if (where!=end()) {
    mcurrent = where;
  }
  
  return;  
}

void StiDetectorLayerContainer::setRefDetector(int sector, int padrow)
{
  mkey.sector = sector;
  mkey.padrow = padrow+1;
  mkey.z = 1e6;

  detectormap::const_iterator where = lower_bound(mkey);
  if (where!=end() && (*where).first.sector==sector && (*where).first.padrow==padrow) {
    mcurrent = where;
  }
  else (setRefDetector(sector));
  
  return;  
}

void StiDetectorLayerContainer::build(const char* buildDirectory)
{
    char* buildfile = new char[200];
    for (int sector=12; sector<=12; ++sector) {
	for (int padrow=1; padrow<=45; ++padrow) {
	    
	    sprintf(buildfile, "%sTpc/Sector_%i/Padrow_%i.txt", buildDirectory, static_cast<int>(sector), padrow);
	    //cout <<"buildfile:\t"<<buildfile<<endl;
	    StiDetector* layer;
	    if (mdraw) {
		layer = new StiDrawableDetector();
	    }
	    else {
		layer = new StiDetector();
	    }
	    
	    layer->build(buildfile);
	    
	    if (layer->isOn()) push_back(layer);
	}
    }
    return;
}

void StiDetectorLayerContainer::buildNext(const char* buildDirectory)
{
    if (mdone) return;
    
    char* buildfile = new char[200];
    sprintf(buildfile, "%sTpc/Sector_%i/Padrow_%i.txt", buildDirectory, static_cast<int>(msector), mpadrow);
    //cout <<"buildfile:\t"<<buildfile<<endl;
    StiDetector* layer;
    if (mdraw) {
	layer = new StiDrawableDetector();
    }
    else {
	layer = new StiDetector();
    }
    
    layer->build(buildfile);
    
    if (layer->isOn()) push_back(layer);

    //increment
    if (mpadrow<mmaxpadrow) ++mpadrow;
    else if (msector<mmaxsector) {
	mpadrow = mminpadrow;
	++msector;
    }
    else mdone=true;
	
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






