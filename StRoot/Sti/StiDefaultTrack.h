#ifndef StiDefaultTrack_H
#define StiDefaultTrack_H 1

#include <iostream.h>
#include <stdlib.h>
#include "StiTrack.h"
#include "StiKalmanTrack.h"

class StiDefaultTrack : public StiTrack, public StiKalmanTrack
{
 public:

  // methods inherited from StiKalmanTrack
  double getPredictedChi2(const Hit *cluster) const;
  int    propagateTo(double xr,double x0,double rho,double pm);
  int    update(const Hit* c, double chi2, int i);
  float  getYWindow() const;
  float  getZWindow() const;
  
  // methods inherited from StiTrack
  float getSigmaPx2()                     const;
  float getSigmaPy2()                     const;
  float getSigmaPz2()                     const;
  float getSigmaPt2()                     const;
  float getSigmaP2()                      const;
  float getSigmaE2()                      const;
  float getSigmaRapidity2()               const;
  float getSigmaPseudoRapidity2()         const;
  float getSigmaTanL2()                   const;
  float getSigmaTanPhi2()                 const;
  void  getErrorMatrix(double c[15])      const;

  // methods not inherited from base classes
  int  rotate(Double_t angle);

  double getX()     const {return fX;}
  double getAlpha() const {return fAlpha;}
  double getdEdx()  const {return fdEdx;}

  double getY()   const {return fP0;}
  double getZ()   const {return fP1;}
  double getSnp() const {return fX*fP3 - fP2;}             
  double get1Pt() const {return fP3*kConversionConstant;}             
  double getTgl() const {return fP4;}
  double getSigmaY2() const {return fC00;}
  double getSigmaZ2() const {return fC11;}
  double sigmaY2() const;
  double sigmaZ2() const;

 
  void  addHit(StHit * hit);
  void  removeHit(StHit * hit);
  void  removeAllHits();
  void  reset();

  static setSvtDedxCalculator(StiDedxCalculator * calculator);
  static setTpcDedxCalculator(StiDedxCalculator * calculator);
  static setTreeNodeFactory(StiTreeNodeFactory  * factory);
  static StiDedxCalculator  * getSvtDedxCalculator();
  static StiDedxCalculator  * getTpcDedxCalculator();
  static StiTreeNodeFactory * getTreeNodeFactory();
  
 protected:

  static StiTreeNodeFactory    * treeNodeFactory;
  static StiSvtDedxCalculator  * svtDedxCalculator;
  static StiTpcDedxCalculator  * tpcDedxCalculator;
  
  void   addHitNode(StiMutableTreeNode * addedNode, StiMutableTreeNode * target);

  double fX;              // X-coordinate of this track (reference plane)
  double fAlpha;          // rotation angle

  double fdEdx;           // dE/dx 

  double fP0;             // Y-coordinate of a track
  double fP1;             // Z-coordinate of a track
  double fP2;             // C*x0
  double fP3;             // track curvature
  double fP4;             // tangent of the track momentum dip angle

  double fC00;                         // covariance
  double fC10, fC11;                   // matrix
  double fC20, fC21, fC22;             // of the
  double fC30, fC31, fC32, fC33;       // track
  double fC40, fC41, fC42, fC43, fC44; // parameters
 
  StiTreeNode * first;  // first point on this track
  StiTreeNode * last;   // current/last work node

  ClassDef(StiDefaultTrack, 1)

};

inline  float  getDedx()           const   
{
  // return average dedx
  return dedx;
}
#endif


