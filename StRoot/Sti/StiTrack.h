#ifndef StiTrack_H
#define StiTrack_H 1

#include <math.h>
#include "StiTrackNode.h"

class StiHit;

class StiTrack 
{
public:

    // constructor/destructor/copy/etc
    
    StiTrack();
    virtual ~StiTrack();
    
    // action methods
    
    virtual void reset();
    
    // accessor methods
    
    double  getMass()           const;   // mass when pid known
    int     getCharge()         const;   // charge of the particle
    double  getChi2()           const;   // chi2 of fit

    virtual void    getMomentum(double p[3], double e[6]) const =0;
    virtual double  getPt()             const                   =0;   // transverse momentum
    virtual double  getRapidity()       const                   =0;   // rapidity
    virtual double  getPseudoRapidity() const                   =0;   // pseudo rapidity
    virtual double  getPhi()            const                   =0;   // azimuthal angle
    virtual double  getTanL()           const                   =0;   // tan(lambda)
    virtual double  getDca(StiHit *h=0)    const=0;   // distance of closest approach to given point/hit
    virtual double  getDca2(StiTrack *t)   const=0;   // distance of closest approach to given track - 2D calc
    virtual double  getDca3(StiTrack *t)   const=0;   // distance of closest approach to given track - 3D calc
    
    int    getFitPointCount()   const;  // number of points used in fit
    int    getPointCount()      const;  // number of total number of points currently assigned to the track
    int    getStatus()          const;  // status of track
    
    StiHit * getVertex() const;  // return pointer to vertex associated with this track if any. 
    
    void  setCharge(int v)   ;   // charge of the particle
    void  setChi2(double v)        ;  // chi2 of fit
    void  setFitPointCount(int v) ;  // number of points used in fit
    void  setPointCount(int v)    ;  // number of points currently assigned to the track;
    void  setStatus(int v)      ;  // status of track
    void  setVertex(StiHit *v);
  

protected:
    friend ostream& operator<<(ostream& os, const StiTrack& track);

    int    q;          // charge of the track 
    int    nPts;       // number of points on the track
    int    nFitPts;    // number of points included in the fit of the track
    StiHit * vertex; // parent vertex of this track
    int    status;     // status code associated with this track
    double  m;          // mass hypothesis
    double  chi2;
};


inline  double  StiTrack::getMass()           const   
{
  // return mass of particle
  // mass is value of identified particle mass
  return m;
}


inline  int    StiTrack::getCharge()              const   
{
  // return charge of particle
  return q;
}

inline  double  StiTrack::getChi2()           const  
{
  // return fit chi2
  return chi2;
}

inline  int    StiTrack::getFitPointCount()    const  
{
  // return number of points used in fit
  return nFitPts;
}

inline  int    StiTrack::getPointCount()       const  
{
  // return number of points associated with track
  return nPts;
}


inline  int    StiTrack::getStatus()         const  
{
  // return track status 
  // see class documentation for definition of possible status
  return status;
}

inline StiHit * StiTrack::getVertex() const
{
  return vertex;
}

inline  void  StiTrack::setCharge(int v)         
{
  // set value of charge
  q = v;
}

inline  void  StiTrack::setChi2(double v)        
{
  // set value of chi2 of track
  chi2 = v;
}

inline  void  StiTrack::setFitPointCount(int v)   
{
  // set number of points used in fit
  nFitPts = v;
}

inline  void  StiTrack::setPointCount(int v)      
{
  // set value of number of points associated with track
  nPts = v;
}

inline  void  StiTrack::setStatus(int v)        
{
  // set value of track status
  status = v;
}


inline void  StiTrack::setVertex(StiHit *v)
{
  vertex = v;
}


#endif
