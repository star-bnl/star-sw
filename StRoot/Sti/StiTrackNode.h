#ifndef StiTrackNode_H
#define StiTrackNode_H 1
#include <iostream.h>
#include <stdlib.h>
#include "StiDefaultMutableTreeNode.h"
#include "StiHit.h"
#include "StiDetector.h"

class StiTrackNode : public StiDefaultMutableTreeNode
{
  /* 
   * A work class used to handle Kalman filter information while
   * constructing tracks
   *
   */

 public:

  double fX;              // X-coordinate of this track (reference plane)
  double fAlpha;          // rotation angle

  double fdEdx;           // dE/dx 

  double fP0;             // Y-coordinate of a track
  double fP1;             // Z-coordinate of a track
  double fP2;             // C*x0
  double fP3;             // track curvature==C
  double fP4;             // tangent of the track momentum dip angle

  double fC00;                         // covariance
  double fC10, fC11;                   // matrix
  double fC20, fC21, fC22;             // of the
  double fC30, fC31, fC32, fC33;       // track
  double fC40, fC41, fC42, fC43, fC44; // parameters
 
  double fChi2;
  int    mDepth;
  
  StiHit * hit;

  void set(const double xx[5],
	   const double cc[15], 
	   double xref, 
	   double alpha);
  void setHit(StiHit * h);
  StiHit * getHit() const;
  //void getState(double x[5]);
  void getState(double x[5], double e[15]=0) const;
  void getMomentum(double p[3], double e[6]=0) const;
  double getTanL() const;
  double getPt() const;
  void getGlobalMomentum(double p[3], double e[6]=0) const;
  void setAsCopyOf(StiTrackNode * node);

  void           setDetector(StiDetector * d) { detector = d;    };
  StiDetector *  getDetector() const          { return detector; };
  

  // static methods
  static void    setFieldConstant(double f) { kField = f;};
  static double  getFieldConstant()         { return kField;}; 

 protected:

  static double kField;
  StiDetector * detector;
};

ostream& operator<<(ostream& os, const StiTrackNode& n);

#endif



