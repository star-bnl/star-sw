#ifndef StiTrackNode_H
#define StiTrackNode_H 1
#include <iostream.h>
#include <stdlib.h>
#include "StiDefaultMutableTreeNode.h"

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
  double fP3;             // track curvature
  double fP4;             // tangent of the track momentum dip angle

  double fC00;                         // covariance
  double fC10, fC11;                   // matrix
  double fC20, fC21, fC22;             // of the
  double fC30, fC31, fC32, fC33;       // track
  double fC40, fC41, fC42, fC43, fC44; // parameters
 
  double fChi2;
  int    mDepth;

  void set(const double xx[5],
	   const double cc[15], 
	   double xref, 
	   double alpha);
  void setAsCopyOf(StiTrackNode * node);
};

ostream& operator<<(ostream& os, const StiTrackNode& n);

#endif



