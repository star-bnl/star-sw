#ifndef StiKalmanTrack_H
#define StiKalmanTrack_H 1
//SCL
#include "StThreeVector.hh"
//STD
#include <math.h>
//Sti
//#include "StiFactoryTypedefs.h"
#include "StiTrack.h"
#include "StiObjectFactory.h"
#include "StiKalmanTrackNode.h"
#include "StiHitContainer.h"

class StiKalmanTrack : public StiTrack 
{
 public:
  //  typedef StiObjectFactory<StiKalmanKalmanTrackNode> StiKalmanKalmanTrackNodeFactory;
  
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
  virtual void    reset();
  virtual void    getMomentum(double p[3], double e[6]) const ;
  virtual double  getPt()             const                   ;   // transverse momentum
  virtual double  getRapidity()       const                   ;   // rapidity
  virtual double  getPseudoRapidity() const                   ;   // pseudo rapidity
  virtual double  getPhi()            const                   ;   // azimuthal angle
  virtual double  getTanL()           const                   ;   // tan(lambda)
  virtual double  getDca(StiHit *h=0)    const;   // distance of closest approach to given point/hit
  virtual double  getDca2(StiTrack *t)   const;   // distance of closest approach to given track - 2D calc
  virtual double  getDca3(StiTrack *t)   const;   // distance of closest approach to given track - 3D calc
  
  
  // Methods of this class
  StiKalmanTrackNode * getFirstNode()  const { return firstNode; };
  StiKalmanTrackNode * getLastNode()   const { return lastNode;  };
  void setFirstNode(StiKalmanTrackNode * n) {firstNode = n;};
  void setLastNode(StiKalmanTrackNode * n)  {lastNode  = n;};
  
  double  getSvtDedx()         const { return svtDedx;};
  double  getTpcDedx()         const { return tpcDedx;};
  void   setSvtDedx(double dedx) { svtDedx = dedx; };
  void   setTpcDedx(double dedx) { tpcDedx = dedx; };
  
  // convenience methods for adding/retrieving points
  StiKalmanTrackNode * addHit(StiHit *h);
  StiKalmanTrackNode * insertHit(StiHit *hInserted, StiHit * targetParent);
  void removeHit(StiHit *h);
  void removeAllHits();
  int  getHitCount();
  StiHit * getHit(int index);
  StiKalmanTrackNode * findHit(StiHit * h);
  
  void initialize(double alpha, 
		  double eta,
		  double curvature,
		  double tanl,
		  const hitvector &);
  StiKalmanTrackNode * getNodeNear(double x) const;
  StThreeVector<double> getPointNear(double x) const;
  StThreeVector<double> getGlobalPointNear(double x) const;
  
  // static StiTrackNodeFactory * trackNodeFactory; // moved to base class...
  
 protected:
  
  StiKalmanTrackNode * firstNode;
  StiKalmanTrackNode * lastNode;
  double svtDedx;
  double tpcDedx;
};

typedef StiObjectFactory<StiKalmanTrack> StiKalmanTrackFactory; 


#endif

