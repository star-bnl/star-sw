//StiDetectorLayerContainer.cxx
//M.L. Miller (Yale Software)
//02/02/01

#include <iostream.h>

#include "StiMapUtilities.h"
#include "StiDetector.h"
#include "StiDrawableDetector.h"
#include "StiDetectorLayerContainer.h"

StiDetectorLayerContainer* StiDetectorLayerContainer::sinstance = 0;
static double gInfintessimal = 1.e-10;
static double gInfinite = 1.e100;

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
    mkey.position = layer->getCenterRadius();
    mkey.refangle = layer->getCenterRefAngle();
    mkey.z = layer->getZCenter();
    insert( detectorMapValType( mkey,layer ) );
    return;
}

void StiDetectorLayerContainer::reset()
{
  mcurrent=begin();
  mkey.refangle=(*mcurrent).first.refangle;
  mkey.position=(*mcurrent).first.position;
  
  return;
}

const StiDetector* StiDetectorLayerContainer::operator*() const
{
  return (mcurrent!=end()) ? (*mcurrent).second : 0;
}

bool StiDetectorLayerContainer::setRefDetector(const StiDetector* layer)
{
    mkey.position = layer->getCenterRadius();
    mkey.refangle = layer->getCenterRefAngle();
    mkey.z = layer->getZCenter();
    detectormap::const_iterator where = find(mkey);
    if (where!=end()) {
	mcurrent = where;
	return true;
    }
    else {
	return false;
    }
}

bool StiDetectorLayerContainer::sectorStepPlus()
{
  DetectorMapKey nextkey;
  nextkey.position = (*mcurrent).first.position+gInfintessimal;
  nextkey.refangle=(*mcurrent).first.refangle+gInfintessimal;
  nextkey.z = 1.e6;

  //find the nearest ref-angle
  detectormap::const_iterator lower = lower_bound( nextkey );
  if ( lower==end() ) {
    //cout <<"Error, search failed, wrap around 2pi"<<endl;
    lower = begin();
  }

  //now we know the ref-angle, look for the same position
  nextkey.refangle = (*lower).first.refangle;
  detectormap::const_iterator where = lower_bound( nextkey );
  if (where==end() ) {
    cout <<"Didn't find the position"<<endl;
    mcurrent = lower;
    return true;
  }

  mcurrent = where;
  return true;
}

bool StiDetectorLayerContainer::sectorStepMinus()
{  
  DetectorMapKey nextkey;
  nextkey.position = gInfinite;
  nextkey.refangle=(*mcurrent).first.refangle-gInfintessimal;
  nextkey.z = gInfinite;
  
  //find the nearest ref-angle
  detectormap::const_iterator lower = lower_bound( nextkey );
  if ( lower==end() ) {
    cout <<"Error, search failed, wrap around 2pi"<<endl;
    lower = begin();
    cout <<"Error, search failed"<<endl;
  }

  //Now look one below, to get the nearest ref-angle
  if (lower==begin()) {
    //cout <<"Error, lower=begin"<<endl;
    lower=end();
  }
  --lower;
  //now we know the ref-agngle, look for the same position
  nextkey.refangle = (*lower).first.refangle;
  nextkey.position = (*mcurrent).first.position+gInfintessimal;
  detectormap::const_iterator where = lower_bound( nextkey );
  if (where==end() ) {
    cout <<"Didn't find the position"<<endl;
    mcurrent = lower;
    return true;
  }

  mcurrent = where;

  return true;
}

bool StiDetectorLayerContainer::padrowStepMinus()
{
  double refangle = (*mcurrent).first.refangle;
  ++mcurrent;
  if (mcurrent==end()) {
    --mcurrent;
    return false;
  }

  if ( (*mcurrent).first.refangle == refangle ) return true;
  
  else {
    --mcurrent;
    return false;
  }
}

bool StiDetectorLayerContainer::padrowStepPlus()
{
  if (mcurrent==begin()) return false; //nowhere to go

  double refangle = (*mcurrent).first.refangle;
  --mcurrent;

  if ( (*mcurrent).first.refangle == refangle ) {
    return true;
  }
  
  else {
    ++mcurrent;
    return false;
  }
}

void StiDetectorLayerContainer::setRefDetector(double refangle)
{
  mkey.refangle = refangle;
  mkey.position = gInfinite;
  mkey.z = gInfinite;
  
  detectormap::const_iterator where = lower_bound(mkey);
  if (where!=end()) {
    mcurrent = where;
  }
  return;
}

void StiDetectorLayerContainer::setRefDetector(double refangle, double position)
{
  mkey.refangle = refangle;
  mkey.position = position+1;
  mkey.z = 1e6;

  detectormap::const_iterator where = lower_bound(mkey);
  if (where!=end() && (*where).first.refangle==refangle && (*where).first.position==position) {
    mcurrent = where;
  }
  else (setRefDetector(refangle));
  
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






