//////////////////////////////////////////////////////////////////
// $Id: StFtpcMomentumFit.hh,v 1.1 2000/05/10 13:39:20 oldi Exp $                                   
//
// $Log: StFtpcMomentumFit.hh,v $
// Revision 1.1  2000/05/10 13:39:20  oldi
// Initial version of StFtpcTrackMaker
//
// Revision 1.1  1999/06/29 13:55:49  hummler
// replace old fte by Michael Konrad with c++-wrapper for the new
// StFtpcMomentumFit class
//
//////////////////////////////////////////////////////////////////
// FtpcMomentumFit
// class for approximate momentum fitting in the Star Forward TPCs
// Holm Huemmler, MPI Munich 1999
// hummler@mppmu.mpg.de
//////////////////////////////////////////////////////////////////

#ifndef FTPC_MOMENTUM_FIT
#define FTPC_MOMENTUM_FIT

#include <iostream.h>
#include "StPhysicalHelix.hh"
#include "StThreeVector.hh"

class StFtpcMomentumFit: public StPhysicalHelix
{
public:
  StFtpcMomentumFit(StThreeVector<double> *Vertex, StThreeVector<double> *Hit, int nHits);
  StFtpcMomentumFit(StThreeVector<double> *Vertex, double xVertexWeight, double yVertexWeight, StThreeVector<double> *Hit, double *xWeightVector, double *yWeightVector, int nHits);
  StFtpcMomentumFit(StThreeVector<double> *Hit, int nHits);
  StFtpcMomentumFit(StThreeVector<double> *Hit, double *xWeight, double *yWeight, int nHits);
  StFtpcMomentumFit(double vx, double vy, double vz, double *posx, double *posy, double *posz, int nHits);
  StFtpcMomentumFit(double vx, double vy, double vz, double xVertexWeight, double yVertexWeight, double *posx, double *posy, double *posz, double *xWeightVector, double *yWeightVector, int nHits);
  StFtpcMomentumFit(double *posx, double *posy, double *posz, int nHits);
  StFtpcMomentumFit(double *posx, double *posy, double *posz, double *xWeight, double *yWeight, int nHits);
  ~StFtpcMomentumFit();

  // use default for copy constructor and = operator, only results needed
  
//   void setVertex(StThreeVector *Vertex);
//   void setVertex(double vx, double vy, double vz);
//   void setVertex(float vx, float vy, float vz);
//   void setHits(StThreeVector *Hit, int nHits);
//   void setHits(double *x, double *y, double *z, int nHits);
//   void setHits(float *x, float *y, float *z, int nHits);
  
  StThreeVector<double> helixMomentum() const;
  StThreeVector<double> momentum() const;
  StThreeVector<double> localMomentum(double s);
  double chi2Rad() const;
  double chi2Lin() const;
  int usedCharge() const;
protected:

  void fitPoints(StThreeVector<double> *Hit,  double *xWeight, double *yWeight); 
  int CircleFit(double x[],double y[], double xw[], double yw[], int num);
  void LineFit(double *x, double *y, double *z, double *xw, double *yw, int num);

  StThreeVector<double> mHelixMomentum;
  StThreeVector<double> mFullMomentum;
  int mCharge;
  StThreeVector<double> mVertex;
  StThreeVector<double> mHit[10];
  double mXCenter, mYCenter, mRadius, mChi2Rad;
  double mArcOffset, mArcSlope, mChi2Lin;
  double mZField;
  int mNumHits;
  int mIterSteps;
  int mVertexPointOffset;

};

inline StThreeVector<double> StFtpcMomentumFit::helixMomentum() const
{
  return mHelixMomentum;
}

inline StThreeVector<double> StFtpcMomentumFit::momentum() const
{
  return mFullMomentum;
}

inline int StFtpcMomentumFit::usedCharge() const
{
  return mCharge;
}

inline double StFtpcMomentumFit::chi2Rad() const
{
  return mChi2Rad;
}

inline double StFtpcMomentumFit::chi2Lin() const
{
  return mChi2Lin;
}

inline StThreeVector<double> StFtpcMomentumFit::localMomentum(double s)
{
  return momentumAt(s, mZField);
}


#endif
