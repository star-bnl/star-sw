#include "StiMcTrack.h"
#include "StMcTrack.hh"

StiMcTrack::StiMcTrack()
  : mcTrack(0)
{}

StiMcTrack::~StiMcTrack()
{}

void StiMcTrack::fit(int direction=kOutsideIn)
{}

bool StiMcTrack::find(int direction=kOutsideIn)
{
  return true;
}

void StiMcTrack::reset()
{
  mcTrack = 0;
}

void StiMcTrack::getMomentum(double p[3], double e[6]) const 
{}

StThreeVector<double> StiMcTrack::getMomentumAtOrigin() const
{
  return StThreeVector<double>(0.,0.,0.);
}

StThreeVector<double> StiMcTrack::getMomentumNear(double x)
{
    return StThreeVector<double>(0.,0.,0.);
}

StThreeVector<double> StiMcTrack::getHitPositionNear(double x) const
{  
  return StThreeVector<double>(0.,0.,0.);
}

double  StiMcTrack::getP()              const
{
  return 0;
}

/////////////  BUG IN THE MAKING!!!!!!!!!!!!!!!!!!!!!!! FIELD IS HARDWIRED!!!!!!!!!!
double  StiMcTrack::getCurvature() const
{
  double pt = mcTrack->pt();
  if (pt<1e-8)
    pt = 1e-8;
  return 0.003*0.5/pt;
}

double  StiMcTrack::getPt()             const
{
  return mcTrack->pt();
}

double  StiMcTrack::getRapidity()       const
{
  return 0;
}

double  StiMcTrack::getPseudoRapidity() const
{
  return 0;
}

double  StiMcTrack::getPhi()            const
{
  return 0;
}

double  StiMcTrack::getTanL()           const
{
  return 0;
}

double  StiMcTrack::getDca(StiHit *h)  const
{
  return 0;
}

double  StiMcTrack::getDca2(StiTrack *t) const
{
  return 0;
}

double  StiMcTrack::getDca3(StiTrack *t) const
{
  return 0;
}

int     StiMcTrack::getPointCount() const
{
  return 0;
}

int     StiMcTrack::getFitPointCount() const
{
  return 0;
}
 
int     StiMcTrack::getGapCount() const
{
  return 0;
}

int     StiMcTrack::getMaxPointCount() const
{
  return 0;
}

int     StiMcTrack::getSeedHitCount() const
{
  return 0;
}

void    StiMcTrack::setSeedHitCount(int c)
{}

double  StiMcTrack::getTrackLength() const
{
  return 0;
}

vector<StMeasuredPoint*> StiMcTrack::stHits() const
{
  return vector<StMeasuredPoint*>();
}


double  StiMcTrack::getMass() const
{
  return 0;
}


int     StiMcTrack::getCharge() const
{
  return 0;
}


double  StiMcTrack::getChi2() const
{
  return 0;
}

void    StiMcTrack::setFlag(long v)
{
}

long    StiMcTrack::getFlag() const
{
  return (long)0;
}

void* StiMcTrackFactory::makeNewObject() const
{
  return new StiMcTrack();
}

StiMcTrackFactory::StiMcTrackFactory(const string& newName,
				     int original,
				     int incremental, 
				     int maxInc)
  : StiObjectFactoryInterface<StiMcTrack>(newName, 
					  original, 
					  incremental, 
					  maxInc)
{
  initialize();
}

StiMcTrackFactory::~StiMcTrackFactory()
{
  // cout <<"StiMcTrackFactory::~StiMcTrackFactory()"<<endl;
}
