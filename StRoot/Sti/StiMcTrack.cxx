#include <stdexcept>
#include "StiMcTrack.h"
#include "StMcTrack.hh"
#include "StMcVertex.hh"
#include "StThreeVectorF.hh"
#include "StParticleTable.hh"
#include "StParticleDefinition.hh"

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
  _hits.clear();
  mcTrack = 0;
}

void StiMcTrack::getMomentum(double p[3], double e[6]) const 
{  StThreeVectorF mom = mcTrack->momentum();
  p[0]=mom.x();
  p[1]=mom.y();
  p[2]=mom.z();
  e[0]=0.;
  e[1]=0.;
  e[2]=0.;
  e[3]=0.;
  e[4]=0.;
  e[5]=0.;
}

StThreeVector<double> StiMcTrack::getMomentumAtOrigin() const
{
  StThreeVectorF mom = mcTrack->momentum();
  return StThreeVector<double>(mom.x(),mom.y(),mom.z());
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
  //cout << "StiMcTrack::getPt() -I- pt:"<<mcTrack->pt()<<endl;
  return (mcTrack->pt());
}

double  StiMcTrack::getRapidity()       const
{
  return mcTrack->rapidity();
}

double  StiMcTrack::getPseudoRapidity() const
{
  return mcTrack->pseudoRapidity();
}

double  StiMcTrack::getPhi()            const
{
  StThreeVectorF mom = mcTrack->momentum();

  return atan2(mom.y(),mom.x());
}

double  StiMcTrack::getTanL()           const
{
  StThreeVectorF mom = mcTrack->momentum();
  double pt = mcTrack->pt();
  if (fabs(pt)<1e-8)
    return mom.z()/1e-8; // sign may be wrong half the time.
  else
    return mom.z()/pt;
}

double  StiMcTrack::getDca()  const
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
  return mcTrack->tpcHits().size();
}

int     StiMcTrack::getFitPointCount() const
{
  return mcTrack->tpcHits().size();
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
  const StParticleDefinition * pd = mcTrack->particleDefinition();
  if (pd)
    return pd->mass();
  else
    return -99;
}


int     StiMcTrack::getCharge() const
{
  //  const StParticleDefinition * pd = mcTrack->particleDefinition();
  //if (pd)
  //  return (int) pd->charge();
  //else
  //  throw runtime_error("StiMcTrack::getCharge() -E- StParticleDefinition * pd==0");
  int gid = mcTrack->geantId();
  /*
  if (gid==2  ||
      gid==5  ||
      gid==8  ||
      gid==11 ||
      gid==14 ||
      gid==19 )
    return 1;
  else if (gid==3  ||
	   gid==6  ||
	   gid==9  ||
	   gid==12 ||
	   gid==15 ||
	   gid==21 )
    return -1;
  else
  return 0;*/ 

  if (gid==8  ||
      gid==11 ||
      gid==14)
    return 1;
  else if (gid==9  ||
	   gid==12 ||
	   gid==15)
    return -1;
  else
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



bool StiMcTrack::isPrimary() const
{
  const StThreeVectorF & pos = mcTrack->startVertex()->position();
  float x = pos.x();
  float y = pos.y();
  return sqrt(x*x+y*y)<2.;
}
