/***************************************************************************
 *
 *  
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : implementation of StHbtThPair
 *
 ***************************************************************************
 *
 *  
 *
 ***************************************************************************/

#include <iostream.h>  
#include <strstream.h>

#include "StHbtMaker/Base/StHbtThPair.hh"
#include "StHbtMaker/Base/StHbtFsiWeight.hh"

#ifdef __ROOT__
ClassImp(StHbtThPair)
#endif

StHbtThPair::StHbtThPair(){
  mMomentum1=mMomentum2=mEmPoint1=mEmPoint2=0;
  mPid1=mPid2=0;
  mMeasPair=0;
  mWeight=0;
  mWeightNum=mWeightDen=1.;
  mWeightOk=false;};

void StHbtThPair::UpdateWeight() {
  if (mWeight) {
    mWeightNum=mWeight->GetWeight(this);
    mWeightDen=mWeight->GetWeightDen();
  } else {
    cout << "StHbtThPair Error - No Weight Generator plugged - set to 1 " <<endl;
    mWeightNum=mWeightDen=1.;
  }
  mWeightOk=true;
}

StHbtString StHbtThPair::Report() {
  ostrstream tStr; 
  tStr << "Default StHbtThPair Report" << endl;
  if (mWeight) {
    tStr << mWeight->Report() << endl;
  } else {
    tStr <<   "No Weight Generator plugged - Weight set to 1 " << endl;
  }
  StHbtString returnThis = tStr.str();
  return returnThis;
}

double StHbtThPair::RealqSideCMS() const {
    double x1 = mMomentum1->x();  double y1 = mMomentum1->y();
    double x2 = mMomentum2->x();  double y2 = mMomentum2->y();

    double xt = x1+x2;  double yt = y1+y2;
    double k1 = sqrt(xt*xt+yt*yt);

    double tmp = 2.0*(x1*y2-x2*y1)/k1;

    return (tmp);
}
double StHbtThPair::RealqOutCMS() const {
    double dx = mMomentum1->x() - mMomentum2->x();
    double xt = mMomentum1->x() + mMomentum2->x();
    
    double dy = mMomentum1->y() - mMomentum2->y();
    double yt = mMomentum1->y() + mMomentum2->y();

    double k1 = (sqrt(xt*xt+yt*yt));
    double k2 = (dx*xt+dy*yt);
    double tmp = k2/k1;
    return (tmp);
}
double StHbtThPair::RealqLongCMS() const {
    double dz = mMomentum1->z() - mMomentum2->z();
    double zz = mMomentum1->z() + mMomentum2->z();

    double dt = mMomentum1->t() - mMomentum2->t();
    double tt = mMomentum1->t() + mMomentum2->t();

    double beta = zz/tt;
    double gamma = 1.0/sqrt(1.0 - beta*beta);

    double temp = gamma*(dz - beta*dt);
    return (temp);
}

//________________________________
double StHbtThPair::RealqOutPf() const
{
  double dt = mMomentum1->t() - mMomentum2->t();
  double tt = mMomentum1->t() + mMomentum2->t();
  
  double xt = mMomentum1->x() + mMomentum2->x();
  double yt = mMomentum1->y() + mMomentum2->y();
  
  double k1 = sqrt(xt*xt + yt*yt);
  double bOut = k1/tt;
  double gOut = 1.0/sqrt(1.0 - bOut*bOut);
  
  double temp = gOut*(this->RealqOutCMS() - bOut*dt);
  return (temp);
}

//___________________________________
double StHbtThPair::RealqSidePf() const
{
 return(this->RealqSideCMS());
}

//___________________________________

double StHbtThPair::RealqLongPf() const
{
 return(this->RealqLongCMS());
}

