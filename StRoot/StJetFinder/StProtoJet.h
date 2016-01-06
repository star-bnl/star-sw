// -*- mode: c++;-*-
// $Id: StProtoJet.h,v 1.16 2016/01/06 22:00:17 gdwebb Exp $
#ifndef StProtoJet_HH
#define StProtoJet_HH

#include "AbstractFourVec.h"

#include <iostream>
#include <vector>
#include <cmath>


/*!
  \class StProtoJet
  \author M.L. Miller (Yale Software)
  StProtoJet encapsulates the concept of a jet.  It is the work object to be used by the jet finding
  algorithms.  It implements the interface of a four vector and contains a list of
  AbstractFourVec objects.  That way, protojets can be "clustered" to form jets.
 */
class StProtoJet {

public:

  typedef std::vector<const AbstractFourVec*> FourVecList;
	
  StProtoJet();
  StProtoJet(const AbstractFourVec* particle);
  virtual ~StProtoJet();
	
  unsigned int numberOfParticles() const {return _particleList.size();}
  unsigned int size() const {return _particleList.size();}
  const FourVecList& list() const {return _particleList;}
	
  ///The d=et^2 of the protojet
  double d() const {return eT()*eT();}
	
  void merge(const StProtoJet& protoJet);
	
  void add(const StProtoJet& protoJet);
  void add(const AbstractFourVec& particle);

  void update();
	
  void clear();
	
  friend std::ostream& operator<<(std::ostream& os, const StProtoJet& j);
	
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
  double charge() const { return _charge; }
 //area
  double area() const {return mArea;}
  void setArea(double area) { mArea = area;}
  //area error
  double areaError() const {return mAreaError;}
  void setAreaError(double areaError) { mAreaError = areaError;}	
private:

  double p() const { return ::sqrt(mPx*mPx + mPy*mPy + mPz*mPz); }
  double theta() const { return acos( mPz/p()); }

  FourVecList _particleList;


  double mPx;
  double mPy;
  double mPz;
  double mE;
  double mArea;
  double mAreaError;

  double _charge;

};


inline std::ostream& operator<<(std::ostream& os, const StProtoJet& j)
{
  os <<"et: "<<j.eT()<<"\tphi: "<<j.phi()<<"\teta: "<<j.eta()
     <<"\tmass: "<<j.mass()<<"\tcharge: "<<j.charge()<<"\tParticles"<< std::endl;
  for (StProtoJet::FourVecList::const_iterator it=j._particleList.begin(); it!=j._particleList.end(); ++it) {
    os << **it << std::endl;
  }
  return os;
}

#endif

