#include "Stiostream.h"
#include "StTptCircleFitter.h"
#include <cmath>

StTptCircleFitter::StTptCircleFitter()
{
    clear();
}

StTptCircleFitter::~StTptCircleFitter() {/* nop */}

void StTptCircleFitter::addPoint(double x, double y, double err)
{
  mX.push_back(x);
  mY.push_back(y);
  // protect against singularities
  double error = (err) ? err : .0001;
  mError.push_back(error);
}

void StTptCircleFitter::clear()
{
    mX.clear();
    mY.clear();
    mError.clear();
    mRadius   = 0;     
    mXCenter  = 0;    
    mYCenter  = 0;
    mVariance = 0;
    mRC	= 0;
}

unsigned int StTptCircleFitter::numberOfPoints() const {return mX.size();}

double StTptCircleFitter::radius() const {return mRadius;}   

double StTptCircleFitter::xcenter() const {return mXCenter;}    

double StTptCircleFitter::ycenter() const {return mYCenter;}    

double StTptCircleFitter::variance() const {return mVariance;}   

int StTptCircleFitter::rc() const {return mRC;}

int StTptCircleFitter::sign() const { return mSign;}

bool StTptCircleFitter::fit()
{
  // note: 
  // 
  // [xy] := \sum (x_i*y_i/err_i)
  // [n]  := \sum (1/err_i)
  // <xy> := [xy]/[n]

  double xav(0),yav(0); // <x>,<y>
  double wsum(0);       // [n]
  double xxav(0),yyav(0),xyav(0); // <xx>,<yy>,<xy> 
  double cosRot(0),sinRot(0); // cos and sin of rotated frame 
  double rscale(0); // ::sqrt(<xx>+<yy>) 
  double xrrav(0),yrrav(0),rrrrav(0);

  // first check if we have enough points
  int nPoint = mX.size();
  if(nPoint<=3){
    mRC = 1;
    return false;
  }
  
  // calcuate <x>,<y>,[n]
  for(int i=0; i<nPoint; i++){
    xav  += mX[i]/mError[i];
    yav  += mY[i]/mError[i];
    wsum += 1./mError[i];
  }
  xav /= wsum; yav /= wsum;

  // calculate <xx>,<xy>,<yy> in the cordinate frame where <x>,<y>=0
  for(int i=0; i<nPoint; i++){
    double x = mX[i] - xav;
    double y = mY[i] - yav;
    
    xxav += x*x/mError[i];
    yyav += y*y/mError[i];
    xyav += x*y/mError[i];
  }
  xxav /= wsum; yyav /= wsum; xyav /= wsum;

  // rotate coordinate such that <xy>=0.
  // the simple quadratic equation to solve is:
  // u^2(A^2+B^2)+u(-A^2-B)+B/4, 
  // where 
  //       u=sin^2(rot), B=4[xy]^2, A=|[y^2-x^2]|
  // roots are
  // u1 = .5(1-A/::sqrt(B+A^2)), u2 = 0.5(1+A/::sqrt(B+A^2))
    
  double A         = fabs(xxav-yyav);
  double B         = 4.*xyav*xyav;
  double D         = A/::sqrt(B+A*A);

  double uPlus  = .5*(1.+ D); // only need positive root

  // sinRotMinus = cosRotPlus, sinRotPlus = cosRotMinus
  double sinRotPlus  = ::sqrt(uPlus);
  double cosRotPlus  = ::sqrt(1-uPlus);

  // require sign(cosRot^2-sinRot^2)=sign([x^2]-[y^2])
  if(xxav<yyav){ // means cosRot<sinRot, i.e take pos solution
    cosRot = cosRotPlus;
    sinRot = sinRotPlus;
  }
  else{ // negative solution
    cosRot = sinRotPlus;
    sinRot = cosRotPlus;
  }

  // require sign(sinRot) = sign(<xy>)*sign(C) (first assume sign(C)>0);
  if(xyav<0.) sinRot *= -1;

  // make sure the new xaxis points out from the origin
  if((cosRot*xav + sinRot*yav)<0){
    cosRot *= -1; sinRot *=-1;
  }
  // BUM

  // calculate rscale, gauss brackets in new coordinate system

  rscale = ::sqrt(xxav + yyav);
 
  // NOTE: i'm reusing the variables xxav, yyav, xyav.
  // from now on, they refer to the rotated+translated frame values.
  // reset them to zero.

  xxav=yyav=xyav=0;
  
  for(int i=0; i<nPoint; i++){
    double x = mX[i] - xav; // translate
    double y = mY[i] - yav;
    
    // rotate and divide by rscale
    double xRot = (cosRot*x + sinRot*y)/rscale;
    double yRot = (-sinRot*x + cosRot*y)/rscale;

    double xxRot     = xRot*xRot;
    double yyRot     = yRot*yRot;
    double xxPyyRot  = xxRot+yyRot;

    xxav += xxRot/mError[i];
    yyav += yyRot/mError[i];
    xyav += xRot*yRot/mError[i];

    xrrav += xxPyyRot*xRot/mError[i];
    yrrav += xxPyyRot*yRot/mError[i];
    rrrrav+= xxPyyRot*xxPyyRot/mError[i];
  }
  
  xxav    /= wsum;
  yyav    /= wsum;
  xrrav   /= wsum;
  yrrav   /= wsum;
  rrrrav  /= wsum;
  xyav    /= wsum;

  // find the solution

  double xrrxrr = xrrav*xrrav;
  double yrryrr = yrrav*yrrav;
  double rrrrm1  = rrrrav - 1.;
  double xxyy    = xxav*yyav;

  double C0 =      rrrrm1*xxyy - xrrxrr*yyav - yrryrr*xxav;
  double C1 =    - rrrrm1      + xrrxrr      + yrryrr       - 4.*xxyy;
  double C2 = 4. + rrrrm1;
  double C4 = -4.;

  // coefficients of the derivative
  double C2D = 2.*C2;
  double C4D = 4.*C4;

  double lambda(0), dlambda(0), p(0), pd(0);
  double chiscl = wsum*rscale*rscale;
  double dlamax = .001/chiscl;

  // newton's method
  for(int i=0; i<5; i++){
    p = C0 + lambda*(C1+lambda*(C2+lambda*lambda*C4));
    pd= C1 + lambda*(C2D+lambda*lambda*C4D);
    
    dlambda = -p/pd;

    lambda += dlambda;

    //if(fabs(dlambda)<dlamax) break;
  }  
     
  if(fabs(dlambda)>dlamax){
    cout << "\tnewton's method failed"<< endl;
    mRC=2;
    return false;
  }

  //  double chisq = chiscl*lambda;
  //  double dchisq = chiscl*dlambda;

  // calc matrix elements
  double H11 = xxav - lambda;
  double H14 = xrrav;
  double H22 = yyav - lambda;
  double H24 = yrrav;
  double H34 = 1.+ 2.*lambda;

  if(H11==0. || H22==0.){
    mRC=3;
    return false;
  }
  
  double rootsq = (H14/H11)*(H14/H11) + 4.*H34;
  double ratio(0),kappa(0),beta(0),alpha(0);

  if(fabs(H22) > fabs(H24)){
    ratio = H24/H22;
    rootsq += ratio*ratio;
    kappa = 1./::sqrt(rootsq);
    beta  = -ratio*kappa;
  }
  else{
    ratio = H22/H24;
    rootsq = 1. + ratio*ratio*rootsq;
    beta = 1./::sqrt(rootsq);
    if(H24 > 0.) beta *= -1;
    kappa = -ratio*beta;
  }

  alpha = -(H14/H11)*kappa;

  // rotate back
  double kappa1 = kappa/rscale;
  double dbr0   = .5/kappa1;
  double alphar = (cosRot*alpha-sinRot*beta)*dbr0;
  double betar  = (sinRot*alpha+cosRot*beta)*dbr0;

  // translate back
  mXCenter = -(alphar-xav);
  mYCenter = -(betar-yav);
  mRadius  = dbr0;
  mSign    = (yrrav>0) ? -1 : 1;

  if(isnan(1./mRadius)){
    mRC = 4; return false;
  }

  double radius2 = mRadius*mRadius;

  // calculate the variance
  for(int i=0; i<nPoint; i++){
    double dx = mX[i]-(mXCenter);
    double dy = mY[i]-(mYCenter);
    double dradius2 = dx*dx+dy*dy;
    mVariance += dradius2+radius2-2.*::sqrt(dradius2*radius2);
  }
  mVariance /= nPoint-3;

  mRC = 0;

  return true;

}
