//////////////////////////////////////////////////////////////////
// $Id: StFtpcMomentumFit.cc,v 1.1 2000/05/10 13:39:19 oldi Exp $
//
// $Log: StFtpcMomentumFit.cc,v $
// Revision 1.1  2000/05/10 13:39:19  oldi
// Initial version of StFtpcTrackMaker
//
// Revision 1.2  1999/08/17 14:39:39  fisyak
// iostream => iostream.h for HP
//
// Revision 1.1  1999/06/29 13:55:45  hummler
// replace old fte by Michael Konrad with c++-wrapper for the new
// StFtpcMomentumFit class
//
//////////////////////////////////////////////////////////////////
// FtpcMomentumFit
// class for approximate momentum fitting in the Star Forward TPCs
// Holm Huemmler, MPI Munich 1999
// hummler@mppmu.mpg.de
//////////////////////////////////////////////////////////////////

#include "StFtpcMomentumFit.hh"
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
//#ifdef __ROOT__
#ifndef gufld
#define gufld gufld_
extern "C" {void gufld(float *, float *);}
#endif

StFtpcMomentumFit::StFtpcMomentumFit(StThreeVector<double> *Vertex, StThreeVector<double> *Hit, int nHits)
{
  StThreeVector<double> Point[11];
  double xWeight[11]={100,100,100,100,100,100,100,100,100,100,100};
  double yWeight[11]={100,100,100,100,100,100,100,100,100,100,100};
  // assume hit resolution = 0.01 cm if not otherwise stated

  mIterSteps=10;
  mNumHits=nHits;
  mVertexPointOffset=1;

  Point[0]=Vertex[0];
  for(int i=0; i<10; i++)
    {
      Point[i+1]=Hit[i];
    }

  fitPoints(Point, xWeight, yWeight);
}

StFtpcMomentumFit::StFtpcMomentumFit(StThreeVector<double> *Vertex, double xVertexWeight, double yVertexWeight, StThreeVector<double> *Hit, double *xWeightVector, double *yWeightVector, int nHits)
{
  StThreeVector<double> Point[11];
  double xWeight[11];
  double yWeight[11];

  mIterSteps=10;
  mNumHits=nHits;
  mVertexPointOffset=1;

  Point[0]=Vertex[0];
  xWeight[0]=xVertexWeight;
  yWeight[0]=yVertexWeight;
  for(int i=0; i<10; i++)
    {
      Point[i+1]=Hit[i];
      xWeight[i+1]=xWeightVector[i];
      yWeight[i+1]=yWeightVector[i];
    }

  fitPoints(Point, xWeight, yWeight);
}

StFtpcMomentumFit::StFtpcMomentumFit(StThreeVector<double> *Hit, int nHits)
{
  StThreeVector<double> Point[11];
  double xWeight[11]={100,100,100,100,100,100,100,100,100,100,100};
  double yWeight[11]={100,100,100,100,100,100,100,100,100,100,100};
  // assume hit resolution = 0.01 cm if not otherwise stated

  mIterSteps=10;
  mNumHits=nHits;
  mVertexPointOffset=0;

  for(int i=0; i<10; i++)
    {
      Point[i]=Hit[i];
    }

  fitPoints(Point, xWeight, yWeight);
}

StFtpcMomentumFit::StFtpcMomentumFit(StThreeVector<double> *Hit, double *xWeightVector, double *yWeightVector, int nHits)
{
  StThreeVector<double> Point[11];
  double xWeight[11];
  double yWeight[11];
  // assume hit resolution = 0.01 cm if not otherwise stated

  mIterSteps=10;
  mNumHits=nHits;
  mVertexPointOffset=0;

  for(int i=0; i<10; i++)
    {
      Point[i]=Hit[i];
      xWeight[i]=xWeightVector[i];
      yWeight[i]=yWeightVector[i];
    }

  fitPoints(Point, xWeight, yWeight);
}

StFtpcMomentumFit::StFtpcMomentumFit(double vx, double vy, double vz, double *posx, double *posy, double *posz, int nHits)
{
  StThreeVector<double> Point[11];
  double xWeight[11]={100,100,100,100,100,100,100,100,100,100,100};
  double yWeight[11]={100,100,100,100,100,100,100,100,100,100,100};
  // assume hit resolution = 0.01 cm if not otherwise stated

  mIterSteps=10;
  mNumHits=nHits;
  mVertexPointOffset=1;

  Point[0].setX(vx);
  Point[0].setY(vy);
  Point[0].setZ(vz);
  for(int i=0; i<10; i++)
    {
      Point[i+1].setX(posx[i]);
      Point[i+1].setY(posy[i]);
      Point[i+1].setZ(posz[i]);
    }

  fitPoints(Point, xWeight, yWeight);
}

StFtpcMomentumFit::StFtpcMomentumFit(double vx, double vy, double vz, double xVertexWeight, double yVertexWeight, double *posx, double *posy, double *posz, double *xWeightVector, double *yWeightVector, int nHits)
{
  StThreeVector<double> Point[11];
  double xWeight[11];
  double yWeight[11];

  mIterSteps=10;
  mNumHits=nHits;
  mVertexPointOffset=1;

  Point[0].setX(vx);
  Point[0].setY(vy);
  Point[0].setZ(vz);
  xWeight[0]=xVertexWeight;
  yWeight[0]=yVertexWeight;
  for(int i=0; i<10; i++)
    {
      Point[i+1].setX(posx[i]);
      Point[i+1].setY(posy[i]);
      Point[i+1].setZ(posz[i]);
      xWeight[i+1]=xWeightVector[i];
      yWeight[i+1]=yWeightVector[i];
    }

  fitPoints(Point, xWeight, yWeight);
}

StFtpcMomentumFit::StFtpcMomentumFit(double *posx, double *posy, double *posz, int nHits)
{
  StThreeVector<double> Point[11];
  double xWeight[11]={100,100,100,100,100,100,100,100,100,100,100};
  double yWeight[11]={100,100,100,100,100,100,100,100,100,100,100};
  // assume hit resolution = 0.01 cm if not otherwise stated

  mIterSteps=10;
  mNumHits=nHits;
  mVertexPointOffset=0;

  for(int i=0; i<10; i++)
    {
      Point[i].setX(posx[i]);
      Point[i].setY(posy[i]);
      Point[i].setZ(posz[i]);
    }

  fitPoints(Point, xWeight, yWeight);
}

StFtpcMomentumFit::StFtpcMomentumFit(double *posx, double *posy, double *posz, double *xWeightVector, double *yWeightVector, int nHits)
{
  StThreeVector<double> Point[11];
  double xWeight[11];
  double yWeight[11];

  mIterSteps=10;
  mNumHits=nHits;
  mVertexPointOffset=0;

  for(int i=0; i<10; i++)
    {
      Point[i].setX(posx[i]);
      Point[i].setY(posy[i]);
      Point[i].setZ(posz[i]);
      xWeight[i]=xWeightVector[i];
      yWeight[i]=yWeightVector[i];
    }

  fitPoints(Point, xWeight, yWeight);
}

StFtpcMomentumFit::~StFtpcMomentumFit()
{

}

void StFtpcMomentumFit::fitPoints(StThreeVector<double> *Hit, double *xWeight, double *yWeight)
{
  double xval[11], yval[11], zval[11];
  double xhelix[11], yhelix[11], zhelix[11];
  int i,j;
  
  // initialize position arrays
  // these values will later be manipulated, Hit array will not be touched
  for(i=0; i<mNumHits+mVertexPointOffset; i++)
    {
      xval[i]=Hit[i].x()*centimeter;
      yval[i]=Hit[i].y()*centimeter;
      zval[i]=Hit[i].z()*centimeter;
    }

  /////////////////////////////////////////////////////////////////////
  // calculate first guess momentum from helix fit
  /////////////////////////////////////////////////////////////////////
  CircleFit(xval, yval, xWeight, yWeight, mNumHits+mVertexPointOffset);

  LineFit(xval, yval, zval, xWeight, yWeight, mNumHits+mVertexPointOffset);

  // determine helix parameters
  double dipAngle = fabs(atan(1/(mRadius*mArcSlope)));
  if(zval[1]<0) 
    dipAngle*=-1;

  double startPhase = atan((yval[0]-mYCenter)/(xval[0]-mXCenter));
  if(xval[0]-mXCenter<0)
    startPhase+=pi;
  else if(yval[0]-mYCenter<0)
    startPhase+=twopi;

  int orientation =1;
  if(mArcSlope*zval[1] < 0)
    orientation=-1;

  // create helix
  StThreeVector<double> startHit(xval[0], yval[0], zval[0]);
  setParameters(1/mRadius, dipAngle, startPhase, startHit, orientation);

  // get z-component of B-field at 0,0,0 for first momentum guess
  float pos[3]={0,0,0};
  float centralField[3];
  gufld(pos,centralField);
  centralField[0] *= kilogauss;
  centralField[1] *= kilogauss;
  centralField[2] *= kilogauss;
  mZField = (double) centralField[2];
  
  // get momentum at track origin and charge
  StThreeVector<double> rv(0,0,zval[0]);
  StThreeVector<double> nv(0,0,1);
  double pl=pathLength(rv,nv);
  mHelixMomentum= momentumAt(pl, mZField);
  mCharge=charge(mZField);

  // store helix fitted hit positions
  for(i=0; i<mNumHits+mVertexPointOffset; i++)
    {
      StThreeVector<double> rvec(0,0,zval[i]);
      StThreeVector<double> nvec(0,0,1);
      double plength=pathLength(rvec,nvec);
      xhelix[i]=x(plength);
      yhelix[i]=y(plength);
      zhelix[i]=z(plength);
    }

  ///////////////////////////////////////////////////////////////////////
  // track helix momentum through measured field:
  ///////////////////////////////////////////////////////////////////////

  // initialize position and momentum
  StThreeVector<double> currentPosition(xhelix[0+mVertexPointOffset], 
					yhelix[0+mVertexPointOffset],
					zhelix[0+mVertexPointOffset]);
  pl=pathLength(currentPosition,nv);
  StThreeVector<double> 
    currentMomentum(momentumAt(pl,mZField));

  // iterate over points
  double stepSize;
  for(i=1+mVertexPointOffset; i<mNumHits+mVertexPointOffset; i++)
    {
      stepSize=(zval[i]-zval[i-1])/mIterSteps;
      
      // iterate between points
      for(j=0; j<mIterSteps; j++)
	{
	  // store momentum for position propagation
	  double propagateXMomentum=currentMomentum.x();
	  double propagateYMomentum=currentMomentum.y();
	  double propagateZMomentum=currentMomentum.z();
	  
	  // get local magnetic field
	  float positionArray[3]={currentPosition.x(), 
				  currentPosition.y(), 
				  currentPosition.z()+stepSize/2};
	  float localField[3];
	  gufld(positionArray, localField);
	  StThreeVector<double> fieldVector
	    ((double) localField[0]*kilogauss/tesla*c_light*nanosecond/meter, 
	     (double) localField[1]*kilogauss/tesla*c_light*nanosecond/meter, 
	     (double) localField[2]*kilogauss/tesla*c_light*nanosecond/meter); 
	  
	  // calculate new momentum as helix segment
	  double absMomentum=abs(currentMomentum);
	  StThreeVector<double> perpField = 
	    currentMomentum.cross(fieldVector)*(float)mCharge/absMomentum;
	  double twistRadius=(absMomentum/abs(perpField))*meter/GeV;
	  
	  double stepLength=stepSize/cos(currentMomentum.theta());
	  
	  double newMomentumCross = absMomentum*stepLength/twistRadius;
	  double newMomentumParallel = 
	    sqrt(absMomentum*absMomentum-newMomentumCross*newMomentumCross);
	  currentMomentum.setMagnitude(newMomentumParallel);
	  StThreeVector<double> momentumChange(perpField);
	  momentumChange.setMagnitude(newMomentumCross);
	  currentMomentum=currentMomentum+momentumChange;
	  
	  // propagate position
	  propagateXMomentum = (propagateXMomentum+currentMomentum.x())/2;
	  propagateYMomentum = (propagateYMomentum+currentMomentum.y())/2;
	  propagateZMomentum = (propagateZMomentum+currentMomentum.z())/2;
	  currentPosition.setX(currentPosition.x()+stepSize*
			       (propagateXMomentum/propagateZMomentum));
	  currentPosition.setY(currentPosition.y()+stepSize*
			       (propagateYMomentum/propagateZMomentum));
	  currentPosition.setZ(currentPosition.z()+stepSize);
	}
      
      // change position array to compensate for distortion
      StThreeVector<double> rvec(0,0,zval[i]);
      StThreeVector<double> nvec(0,0,1);
      double plength=pathLength(rvec,nvec);
      if(zval[1]>0)
	{
	  xval[i]+=(x(plength)-currentPosition.x());
	  yval[i]+=(y(plength)-currentPosition.y());
	}
      else
	{
	  xval[i]-=(x(plength)-currentPosition.x());
	  yval[i]-=(y(plength)-currentPosition.y());
	}

      // calculate fit quality indicators only if needed
//       double distHitHelix=sqrt((xval[i]-x(plength))*(xval[i]-x(plength))+(yval[i]-y(plength))*(yval[i]-y(plength)));
//       double distHelixFit=sqrt((x(plength)-currentPosition.x())*(x(plength)-currentPosition.x())+(y(plength)-currentPosition.y())*(y(plength)-currentPosition.y()));

    }

  //////////////////////////////////////////////////////////////////////
  // refit helix
  //////////////////////////////////////////////////////////////////////

  CircleFit(xval, yval, xWeight, yWeight, mNumHits+mVertexPointOffset);
  LineFit(xval, yval, zval, xWeight, yWeight, mNumHits+mVertexPointOffset);

  // determine helix parameters
  dipAngle = fabs(atan(1/(mRadius*mArcSlope)));
  if(zval[1]<0) 
    dipAngle*=-1;
  
  startPhase = atan((yval[0]-mYCenter)/(xval[0]-mXCenter));
  if(xval[0]-mXCenter<0)
    startPhase+=pi;
  else if(yval[0]-mYCenter<0)
    startPhase+=twopi;

  orientation =1;
  if(mArcSlope*zval[1] < 0)
    orientation=-1;

  // set helix parameters to new values
  startHit.setX(xval[0]);
  startHit.setY(yval[0]);
  setParameters(1/mRadius, dipAngle, startPhase, startHit, orientation);
  
  // set final momentum value
  pl=pathLength(rv,nv);
  mFullMomentum= momentumAt(pl, mZField);

}

//////////////////////////////////////////////////////////////////
// Circle fitting program
//
// This function will fit a circle to the points in the matrix x and y.
// 'num' is the number of points in x and y
// 'xc' is the found center in x
// 'yc' is the found center in y
// 'R' is the radius of the fitted circle
// 'chi2' error in fit
//
// Written by Mike Heffner Sept 21 1998
// error calculation added Oct 3 1998, Mike Heffner
// fit with point errors added May 1999 Holm Huemmler
//
// Fitting algorithm by: N.Chernov,G.Ososkov, Computer Physics 
//          Communications 33(1984) 329-333
//////////////////////////////////////////////////////////////////  


int StFtpcMomentumFit::CircleFit(double x[],double y[], double xw[], double yw[], int num)
{
#ifndef __IOSTREAM__
#include <iostream.h>
#endif
  
  int i;
  int debug =0; //set to 1 for debug messages
  if(num==0)return 0; // added to remove error from zero input
  
  //-------------------------------------------------
  ////////////////////////////////////////////////
  // calculate the center of gravity of the points.
  // then transform to that origin
  double xav=0;
  double yav=0;
  double xwav=0;
  double ywav=0;
  double wei[11];
  for(i=0;i<num;i++)
    {
      wei[i]=xw[i]+yw[i];
      xav += x[i]*wei[i];
      yav += y[i]*wei[i];
      xwav += wei[i];
      ywav += wei[i];
    }
  
  xav = xav/xwav;
  yav = yav/ywav;
  
  cout.precision(16);
  if(debug)   cout<<"from circle fitting program"<<endl;
  for(i=0;i<num;i++)
    {
      if(debug) 
	cout<<"x: "<<x[i]<<" y: "<<y[i]<<"xw: "<<xw[i]<<" yw: "<<yw[i]<<endl;
      x[i] = x[i] - xav;
      y[i] = y[i] - yav;
    }
  
  /////////////////////////////////////////////////
  // calculate some moments of the points
  
  double F = 0;
  double G = 0;
  double H = 0;
  double P = 0;
  double Q = 0;
  double T = 0;
  double gamma0 = 0;
  double wF = 0;
  double wG = 0;
  double wH = 0;
  double wP = 0;
  double wQ = 0;
  double wT = 0;
  double wgamma0 = 0;
  
  // change error parameters to 1d, 2d errors not usable in this fit
  for(i=0;i<num;i++)
    {
      F += wei[i]*(3*x[i]*x[i] + y[i]*y[i]);
      wF += wei[i];
      G += wei[i]*(x[i]*x[i] + 3*y[i]*y[i]);
      wG += wei[i];
      H += wei[i]*2*x[i]*y[i];
      wH += wei[i];
      P += wei[i]*x[i]*(x[i]*x[i] + y[i]*y[i]);
      wP += wei[i];
      Q += wei[i]*y[i]*(x[i]*x[i] + y[i]*y[i]);
      wQ += wei[i];
      T += wei[i]*(x[i]*x[i] + y[i]*y[i])*(x[i]*x[i] + y[i]*y[i]);
      wT += wei[i];
      gamma0 += wei[i]*(x[i]*x[i] + y[i]*y[i]);
      wgamma0 += wei[i];
    }

  gamma0 = gamma0/wgamma0;
  F = F/wF;
  G = G/wG;
  H = H/wH;
  P = P/wP;
  Q = Q/wQ;
  T = T/wT;
  

  double  A = -F-G;
  double  B = F*G-T-H*H;
  double  C = T*(F+G)-2*(P*P+Q*Q);
  double  D = T*(H*H-F*G)+2*(P*P*G+Q*Q*F)-4*P*Q*H;
  
  double A0 = A/gamma0;
  double B0 = B/(gamma0*gamma0);
  double C0 = C/(gamma0*gamma0*gamma0);
  double D0 = D/(gamma0*gamma0*gamma0*gamma0);

  ///////////////////////////////////////////////////////
  // now solve the equation by Newton method

  int MaxIter=100;
  double w=1;
  double wNew;
  double f,fp;
  double xc, yc;

  if(debug)  
    cout<<"Solving by Newton method"<<endl;  
  for(i=0;i<MaxIter;i++)
    {
      f = w*w*w*w + A0*w*w*w + B0*w*w + C0*w + D0;
      fp = 4*w*w*w + 3*A0*w*w + 2*B0*w + C0;
      wNew = w - f/fp;
      
      if(debug)  
	cout<<"Iteration Number"<<i<<endl;
      if((wNew-w)<10e-16 && (w-wNew)<10e-16) 
	break;
      w = wNew;
    }
  
  ////////////////////////////////////////////
  // compute the output variables
  double gamma = gamma0*wNew;
  double b = (Q - H*P/(F-gamma))/(G-gamma-H*H/(F-gamma));
  double a = (P-H*b)/(F-gamma);
  
  double R=0;
  if( (wNew-w)<10e-16 && (w-wNew)<10e-16 )
    {
      R = sqrt(a*a + b*b + gamma);
      xc = a + xav;
      yc = b + yav;
    }
  
  // compute chi2
  double chi2 =0;
  double wchi2=0;
  for(i=0;i<num;i++)
    {
      x[i] = x[i] + xav;
      y[i] = y[i] + yav;
      
      double err = R - sqrt(xw[i]*(x[i]-xc)*xw[i]*(x[i]-xc) 
			    + yw[i]*(y[i]-yc)*yw[i]*(y[i]-yc));
      chi2 += err*err;
      wchi2 += xw[i]*xw[i]+yw[i]*yw[i];
    }
  chi2 *= num / wchi2;
  
  mXCenter=xc;
  mYCenter=yc;
  mRadius=R;
  mChi2Rad=chi2;

  return 1;
}

void StFtpcMomentumFit::LineFit(double *xval, double *yval, double *zval, double *xw, double *yw, int num)
{
  double x_ss=0, x_sang=0, x_sz=0, x_szang=0, x_szz=0;
  double angle=0, weight, t;
  int i;
  
  for(i=0; i<num; i++)
    {
      // calculate angle and eliminate steps in atan function
      double angle = atan((yval[i]-mYCenter)/(xval[i]-mXCenter));
      if(xval[i]-mXCenter<0)
	angle+=pi;
      else if(yval[i]-mYCenter<0)
	angle+=twopi;
      
      // shift into same phase
      double lastangle;
      if(i!=0)
	{
	  if(angle>lastangle+pi)
	    angle-=twopi;
	  if(angle<lastangle-pi)
	    angle+=twopi;
	}
      lastangle=angle;

      // do sums
      weight = sqrt(xw[i]*xw[i]*cos(angle)*cos(angle)
		    +yw[i]*yw[i]*sin(angle)*sin(angle));
      x_ss+=weight;
      x_sang+=weight*angle;
      x_sz+=weight*zval[i];
      x_szang+=weight*zval[i]*angle;
      x_szz+=weight*zval[i]*zval[i];
    }
  t=x_ss*x_szz-x_sz*x_sz;
  if(t!=0)
    {
      mArcOffset=((x_szz*x_sang)-(x_sz*x_szang))/t;
      mArcSlope=((x_ss*x_szang)-(x_sz*x_sang))/t;
    }
  else
    {
      mArcOffset=0;
      mArcSlope=0;
    }

  double chi2, wchi2;
  for(i=0;i<num;i++)
    {      
      angle = atan((xval[i]-mXCenter)/(yval[i]-mYCenter));
      weight = sqrt(xw[i]*cos(angle)*xw[i]*cos(angle)
		    +yw[i]*sin(angle)*yw[i]*sin(angle));
      double err = weight*(angle - (mArcOffset + mArcSlope*zval[i]));
      chi2 += err*err;
      wchi2 += weight*weight;
    }
  chi2 *= num / wchi2;

  mChi2Lin = chi2;
}
