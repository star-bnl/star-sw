// -*- mode: c++;-*-
// $Id: StFourVec.h,v 1.4 2008/04/23 01:13:08 tai Exp $
#ifndef STFOURVEC_H
#define STFOURVEC_H

#include <iostream>
#include <cmath>

class StFourVec {

public:
  StFourVec() : mPx(0), mPy(0), mPz(0), mE(0), mCharge(0) {}
	
  StFourVec(double pt, double phi, double eta, double m, double charge = 0)
    : mPx(pt*cos(phi)), mPy(pt*sin(phi)), mPz(pt*sinh(eta))
    , mE(::sqrt(fabs(mPx*mPx + mPy*mPy +mPz*mPz + m*m)))
    , mCharge(charge)
  { }
	
  virtual ~StFourVec() { };
	
  //momenta
  double pt() const { return ::sqrt(mPx*mPx + mPy*mPy); }
  double px() const { return mPx; }
  double py() const { return mPy; }
  double pz() const { return mPz; }
  
  //angles
  double phi() const { return atan2(mPy, mPx); }
  double eta() const
  {
    double arg = tan(theta()/2.);
    return (arg > 0) ? -::log(arg) : -999;
  }

  //4-th component
  double e() const { return fabs(mE); }
  double eT() const
  {
    if(p() == 0.0) return 0.0;
    return ::sqrt(e()*e()*pt()*pt()/(p()*p()));
  }
  double mass() const { return ::sqrt(fabs(mE*mE - p()*p() ) ); }
	
  //charge
  double charge() const { return mCharge; }
	
  //actions (these should be templated)
  void add(const StFourVec& other);
  void add(const AbstractFourVec& rhs);
  void operator=(const StFourVec& rhs);
  void operator=(const AbstractFourVec& rhs);

  void clear()
  {
    mPx = mPy = mPz = mE = mCharge = 0;
  }
	
protected:
  double mPx;
  double mPy;
  double mPz;
  double mE;
  double mCharge;

private:
  double p() const { return ::sqrt(mPx*mPx + mPy*mPy + mPz*mPz); }
  double theta() const { return acos( mPz/p()); }

};


inline void StFourVec::add(const StFourVec& rhs)
{
    mPx += rhs.mPx;
    mPy += rhs.mPy;
    mPz += rhs.mPz;
    mE  += rhs.mE;
    mCharge += rhs.mCharge;
}

inline void StFourVec::add(const AbstractFourVec& rhs)
{
    mPx += rhs.px();
    mPy += rhs.py();
    mPz += rhs.pz();
    mE  += rhs.e();
    mCharge += rhs.charge();
}

inline void StFourVec::operator=(const StFourVec& rhs)
{
    mPx = rhs.mPx;
    mPy = rhs.mPy;
    mPz = rhs.mPz;
    mE  = rhs.mE;
    mCharge = rhs.mCharge;
}

inline void StFourVec::operator=(const AbstractFourVec& rhs)
{
    mPx = rhs.px();
    mPy = rhs.py();
    mPz = rhs.pz();
    mE  = rhs.e();
    mCharge = rhs.charge();
}

//non-members

inline std::ostream& operator<<(std::ostream& os, const AbstractFourVec& v)
{
    return os <<"et: "<<v.eT()<<"\tphi: "<<v.phi()<<"\teta: "<<v.eta()<<"\tmass: "<<v.mass()
		<<"\tcharge: "<<v.charge();
}

struct StreamFourVec
{
    void operator()(AbstractFourVec* v) {
      std::cout << *v << std::endl;
    }
};

#endif // STFOURVEC_H
