#ifndef StiTrack_H
#define StiTrack_H 1

#include <iostream.h>
#include <stdlib.h>
#include "TObject.h"
#include "TMath.h"

class StMeasuredPoint;
class StVertex;

class StiTrack : public TObject
{
 public:

  static const int Global    = 0; // fitted without vertex info
  static const int Primary   = 1; // fitted with primary vertex
  static const int Secondary = 2; // fitted with constraint on secondary vertex
  

  // constructor/destructor/copy/etc

  StiTrack();
  //StiTrack(const StiTrack & tf); // use compiler default
  virtual ~StiTrack();
  
  // action methods

  virtual void reset();

  // accessor methods

  float  getE()              const;   // energy
  float  getP()              const;   // 3-momentum
  float  getPt()             const;   // transverse momentum
  float  getPx()             const;   // px
  float  getPy()             const;   // py
  float  getPz()             const;   // longitudinal momentum
  float  getRapidity()       const;   // rapidity
  float  getPseudoRapidity() const;   // pseudo rapidity
  float  getPhi()            const;   // azimuthal angle
  float  getTanPhi()         const;   // tan(phi)
  float  getTanL()           const;   // tan(lambda)
  float  getLambda()         const;   // lambda (pitch angle)
  float  getMass()           const;   // mass when pid known
  int    getCharge()         const;   // charge of the particle
  float  getDca()            const;   // distance of closest approach to track vertex
  float  getDca(StMeasuredPoint *h)       const;   // distance of closest approach to given point/hit
  float  getDca2(StiTrack *t)   const;   // distance of closest approach to given track - 2D calc
  float  getDca3(StiTrack *t)   const;   // distance of closest approach to given track - 3D calc
  float  getChi2()            const;  // chi2 of fit
  float  getSvtDedx()         const;
  float  getTpcDedx()         const;

  int    getTpcPointCount()   const;  // number of points used in TPC
  int    getSvtPointCount()   const;  // number of points used in SVT/SSD
  int    getFitPointCount()   const;  // number of points used in fit
  int    getPointCount()      const;  // number of total number of points currently assigned to the track;
  int    getStatus()          const;  // status of track
  
  StVertex * getVertex() const;  // return pointer to vertex associated with this track if any. 

  void  setPt(float v)     ;   // transverse momentum
  void  setTanL(float v)   ;   // tan(lambda)
  void  setTanPhi(float v) ;   // tan(phi)
  void  setSvtDedx(float v);   // "average" dEdx of the track from TPC
  void  setTpcDedx(float v);   // "average" dEdx of the track from SVT
  void  setCharge(int v)   ;   // charge of the particle
  void  setDca(float v)    ;   // distance of closest approach to track vertex

  void  setChi2(float v)        ;  // chi2 of fit
  void  setFitPointCount(int v) ;  // number of points used in fit
  void  setPointCount(int v)    ;  // number of points currently assigned to the track;
  void  setStatus(int v)      ;  // status of track
  void  setVertex(StVertex *v);

  virtual float getSigmaPx2()                     const=0;
  virtual float getSigmaPy2()                     const=0;
  virtual float getSigmaPz2()                     const=0;
  virtual float getSigmaPt2()                     const=0;
  virtual float getSigmaP2()                      const=0;
  virtual float getSigmaE2()                      const=0;
  virtual float getSigmaRapidity2()               const=0;
  virtual float getSigmaPseudoRapidity2()         const=0;
  virtual float getSigmaTanL2()                   const=0;
  virtual float getSigmaTanPhi2()                 const=0;
  virtual void  getErrorMatrix(double c[15])      const=0;
  
 protected:

  int    q;
  float  pt, tanL, tanPhi, svtDedx, tpcDedx, dca, m, chi2;
  int    nPts, nFitPts;
    StVertex * vertex; //!
  int    status;

  ClassDef(StiTrack, 1)

};

inline  float  StiTrack::getE()              const
{ 
  // Returns the energy of the particle
  float pz = pt*tanL;
  float p  = sqrt(pz*pz+pt*pt);
  return sqrt(m*m+p*p); 
}

inline  float  StiTrack::getP()              const
{
  // Returns the 3-momentum of the particle
  float pz = pt*tanL;
  return sqrt(pz*pz+pt*pt);
}   

inline  float  StiTrack::getPt()             const
{
  // Returns the transverse momentum
  return pt;
}
   
inline  float  StiTrack::getPx()             const
{
  // Returns the px component of momentum
  return pt/sqrt(1+tanPhi*tanPhi);
}

inline  float  StiTrack::getPy()             const
{
  // Returns the py component of momentum
  return pt*tanPhi/sqrt(1+tanPhi*tanPhi);
}

inline  float  StiTrack::getPz()             const
{
  // Returns the longitudnal momentum
  return pt*tanL;
}

inline  float  StiTrack::getRapidity()       const
{
  // Returns the rapidity
  float pz = pt*tanL;
  float e  = sqrt(m*m+pz*pz+pt*pt);
  return TMath::Log((e+pz)/(e-pz));
}

inline  float  StiTrack::getPseudoRapidity()    const
{
  // Returns pseudorapidity of the particle
  return -TMath::Log(TMath::Tan(TMath::Pi()/4.-TMath::ATan(tanL)));
}

inline  float  StiTrack::getPhi()            const
{
  // return azimuthal angle of track in radian
  return TMath::ATan(tanPhi);
}

inline  float  StiTrack::getTanPhi()         const   
{
  // return tan(phi)
  return tanPhi;
}

inline  float  StiTrack::getTanL()           const   
{
  // return tan(lambda)
  return tanL;
}

inline  float  StiTrack::getLambda()         const   
{
  // return lambda
  return TMath::ATan(tanL);
}

inline  float  StiTrack::getMass()           const   
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

inline  float  StiTrack::getChi2()           const  
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

inline   float  StiTrack::getSvtDedx()         const
{
  return svtDedx;
}

inline   float  StiTrack::getTpcDedx()         const
{
  return tpcDedx;
}

inline  int    StiTrack::getStatus()         const  
{
  // return track status 
  // see class documentation for definition of possible status
  return status;
}

inline StVertex * StiTrack::getVertex() const
{
  return vertex;
}


inline  void  StiTrack::setPt(float v)        
{
  // set value of transverse momentum
  pt = v;
}

inline  void  StiTrack::setTanL(float v)      
{
  // set value of tan(lambda)
  tanL = v;
}

inline  void  StiTrack::setTanPhi(float v)    
{
  // set value of tan(phi)
  tanPhi = v;
}

inline  void  StiTrack::setTpcDedx(float v)      
{
  // set value of dedx
  tpcDedx = v;
}
inline  void  StiTrack::setSvtDedx(float v)      
{
  // set value of dedx
  svtDedx = v;
}

inline  void  StiTrack::setCharge(int v)         
{
  // set value of charge
  q = v;
}

inline  void  StiTrack::setDca(float v)       
{
  // set value of distance of closest approach
  dca = v;
}


inline  void  StiTrack::setChi2(float v)        
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


inline void  StiTrack::setVertex(StVertex *v)
{
  vertex = v;
}


#endif


