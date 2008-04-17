// -*- mode: c++;-*-
// $Id: FourVec.h,v 1.7 2008/04/17 17:30:02 tai Exp $

#ifndef FourVec_HH
#define FourVec_HH

#include <iostream>
#include <cmath>

/*!
  \class AbstractFourVec
  \author M.L. Miller (Yale Software)
  Abstract base class to define required interface of a four vector to be fed to a derived instance
  of StJetFinder
 */
class AbstractFourVec {

public:
  AbstractFourVec() {}; //provide default
  virtual ~AbstractFourVec() {};
	
  //access
	
  //momenta
  virtual double pt() const = 0;
  virtual double px() const = 0;
  virtual double py() const = 0;
  virtual double pz() const = 0;
  virtual double p() const = 0;
	
  //angles
  virtual double theta() const = 0;
  virtual double phi() const = 0;
  virtual double eta() const = 0;
  virtual double rapidity() const = 0;
	
  //4-th component
  virtual double eT() const = 0;
  virtual double eZ() const = 0;
  virtual double e() const = 0;
  virtual double mass() const = 0;
	
  //charge
  virtual double charge() const = 0;
	
private:

};

/*!
  \class FourVec
  \author M.L. Miller (Yale Software)
  Template class derived from AbstractFourVec to simultaneously implement interface of a
  four vector and to wrap the corresponding functionality of the template arguement mParticle.
  i.e., a class to interface, e.g. StMuTrack with StAbstractFourVec
 */
template <class T>
class FourVec : public AbstractFourVec {

public:
  FourVec(T* p) : mParticle(p) {};
  //virtual ~FourVec() {};
  virtual ~FourVec() {};//{delete mParticle;mParticle=0;}
	
  //required accessors
	
  //momenta
  virtual double pt() const {return mParticle->pt();}
  virtual double px() const {return mParticle->px();}
  virtual double py() const {return mParticle->py();}
  virtual double pz() const {return mParticle->pz();}
  virtual double p() const {return ::sqrt(pt()*pt()+pz()*pz());}
	
  //angles
  virtual double theta() const {return mParticle->theta();}
  virtual double phi() const {return mParticle->phi();}
  virtual double rapidity() const {return mParticle->rapidity();}
  virtual double eta() const {return mParticle->eta();}
	
  //fourth component
  virtual double eT() const {return mParticle->eT(); }
  virtual double eZ() const {return mParticle->eZ(); }
  virtual double e() const {return mParticle->e(); }
  virtual double mass() const {return mParticle->mass();}
	
  //charge
  virtual double charge() const {return mParticle->charge();}
	
  //access to base object
  T* particle() {return mParticle;}
	
protected:
  FourVec() {}; //not implemented
  T* mParticle;
};

class StFourVec {

public:
  StFourVec() : mPx(0), mPy(0), mPz(0), mE(0), mCharge(0) {}
	
  StFourVec(double pt, double phi, double eta, double m, double charge=0.)
    : mPx(pt*cos(phi)), mPy(pt*sin(phi)), mPz(pt*sinh(eta))
    , mE(::sqrt(fabs(mPx*mPx + mPy*mPy +mPz*mPz + m*m)))
    , mCharge(charge)
  { }
	
  virtual ~StFourVec() {};
	
  //momenta
  virtual double pt() const;
  virtual double px() const;
  virtual double py() const;
  virtual double pz() const;
  virtual double p() const;
	
  //angles
  virtual double theta() const;
  virtual double phi() const;
  virtual double eta() const;
  virtual double rapidity() const;
	
  //4-th component
  virtual double eT() const;
  virtual double eZ() const;
  virtual double e() const;
  virtual double mass() const;
	
  //charge
  virtual double charge() const;
	
  //actions (these should be templated)
  void add(const StFourVec& other);
  void add(const AbstractFourVec& rhs);
  void operator=(const StFourVec& rhs);
  void operator=(const AbstractFourVec& rhs);
  void clear();
	
protected:
  double mPx;
  double mPy;
  double mPz;
  double mE;
  double mCharge;
};

inline void StFourVec::clear()
{
	mPx=mPy=mPz=mE=mCharge=0.;
}

inline double StFourVec::pt() const
{
    return ::sqrt( mPx*mPx + mPy*mPy );
}

inline double StFourVec::px() const
{
    return mPx;
}

inline double StFourVec::py() const
{
    return mPy;
}

inline double StFourVec::pz() const
{
    return mPz;
}

inline double StFourVec::p() const
{
    return ::sqrt(mPx*mPx + mPy*mPy + mPz*mPz);
}

inline double StFourVec::theta() const
{
    return acos( mPz/p() );
}

inline double StFourVec::phi() const
{
    return atan2(mPy, mPx);
}

inline double StFourVec::rapidity() const
{
    double num = e()+mPz;
    double den = e()-mPz;
	
    if (den==0.) {return -999;}
    double arg = num/den;
    if (arg<0.) {return -999;}
    return 0.5 * ::log(arg);
}

inline double StFourVec::eta() const
{
    double arg = tan(theta()/2.);
    return (arg>0.) ? -::log(arg) : -999;
}

//4-th component
inline double StFourVec::eT() const
{
    if(p() == 0.0) return 0.0;
    return ::sqrt(e()*e()*pt()*pt()/(p()*p()));
}

inline double StFourVec::eZ() const
{
    return eT()*sinh(eta());
}

inline double StFourVec::e() const
{
    return fabs(mE);
}

inline double StFourVec::mass() const
{
    return ::sqrt(fabs(mE*mE - p()*p() ) );
}

inline double StFourVec::charge() const
{
    return mCharge;
}

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

#endif // FourVec_HH


