#include "StiKalmanTrack.h"

void StiKalmanTrack::reset()
{
  svtDedx = -1;
  tpcDedx = -1;
  firstNode = 0;
  lastNode  = 0;
}
    
double  StiKalmanTrack::getMomentum(double p[3], double e[6]) const
{
  // return the momentum of the track at the inner most node held by this track
  // which may (or not) be the primary vertex. 
  // this will need to be refined...
  lastNode->getMomentum(p,e);
}

double  StiKalmanTrack::getPt()             const
{
  // returns the transverse momentum of the track at the inner most node held by this track
  // which may (or not) be the primary vertex. 
  // this will need to be refined...

  return lastNode->getPt();
}

double  StiKalmanTrack::getRapidity()       const 
{
  // returns the rapidity of the particle at the inner most node held by this track
  // which may (or not) be the primary vertex. 
  // this will need to be refined...
  double p[3];
  lastNode->getMomentum(p,0);
  double mass = getMass();
  if (mass>=0)
    // mass is known, return actual rapidity
    {
      double e = sqrt(mass*mass+p[0]*p[0]+p[1]*p[1]+p[2]*p[2]);
      return 0.5*log(e+p[2])/(e-p[2]);
    }
  else
    return getPseudoRapidity();
}

double  StiKalmanTrack::getPseudoRapidity() const
{
  // Return pseudo rapidity of the particle at the inner most node held by this track
  // which may (or not) be the primary vertex. 
  // this will need to be refined...
  return -log(tan(3.1415927/4.-lastNode->getTanL()));
}
  
double  StiKalmanTrack::getPhi()            const 
{
  // Return the azimuthal angle of the particle at the inner most node held by this track
  // which may (or not) be the primary vertex. 
  // this will need to be refined...
  double p[3];
  lastNode->getMomentum(p,0);
  return atan2(p[1],p[0]);
}

double  StiKalmanTrack::getTanL()           const 
{
  // Return tan(lambda) of the particle at the inner most node held by this track
  // which may (or not) be the primary vertex. 
  // this will need to be refined...
  return lastNode->getTanL();
}

double  StiKalmanTrack::getDca(StiHit *h=0)    const
{
  // Return the distance of closest approach to given point/hit
  // If no hit is specified assume the primary vertex i.e the last point 
  // on the track
  // set to 0 for now
  return 0;
}
double  StiKalmanTrack::getDca2(StiTrack *t)   const
{
  // distance of closest approach to given track - 2D calc
  return 0;
}

double  StiKalmanTrack::getDca3(StiTrack *t)   const
{
  // distance of closest approach to given track - 3D calc
  return 0;
}
