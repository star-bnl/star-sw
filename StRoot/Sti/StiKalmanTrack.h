#ifndef StiKalmanTrack_H
#define StiKalmanTrack_H 1

#include <math.h>
#include "StiTrack.h"
#include "StiTrackNode.h"

class StiKalmanTrack : StiTrack 
{
 public:
  
  // constructor/destructor/copy/etc
  
  StiKalmanTrack() 
    {
      reset();
    };
  virtual ~StiKalmanTrack()
    {
    };
  
  // Action methods
  // Implementation of virtua methods inherited
  // from StiTrack
  virtual void reset();
  virtual double  getMomentum(double p[3], double e[6]) const ;
  virtual double  getPt()             const                   ;   // transverse momentum
  virtual double  getRapidity()       const                   ;   // rapidity
  virtual double  getPseudoRapidity() const                   ;   // pseudo rapidity
  virtual double  getPhi()            const                   ;   // azimuthal angle
  virtual double  getTanL()           const                   ;   // tan(lambda)
  virtual double  getDca(StiHit *h=0)    const;   // distance of closest approach to given point/hit
  virtual double  getDca2(StiTrack *t)   const;   // distance of closest approach to given track - 2D calc
  virtual double  getDca3(StiTrack *t)   const;   // distance of closest approach to given track - 3D calc
  
  
  // Methods of this class
  StiTrackNode * getFirstNode()  const { return firstNode; };
  StiTrackNode * getLastNode()   const { return lastNode;  };
  void setFirstNode(StiTrackNode * n) {firstNode = n;};
  void setLastNode(StiTrackNode * n)  {lastNode  = n;};
  
  double  getSvtDedx()         const { return svtDedx;};
  double  getTpcDedx()         const { return tpcDedx;};
  void   setSvtDedx(double dedx) { svtDedx = dedx; };
  void   setTpcDedx(double dedx) { tpcDedx = dedx; };
  
 protected:
  
  StiTrackNode * firstNode;
  StiTrackNode * lastNode;
  double svtDedx;
  double tpcDedx;
};



#endif

