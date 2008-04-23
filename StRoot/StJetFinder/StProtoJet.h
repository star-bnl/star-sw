// -*- mode: c++;-*-
// $Id: StProtoJet.h,v 1.9 2008/04/23 01:28:53 tai Exp $
#ifndef StProtoJet_HH
#define StProtoJet_HH

#include "AbstractFourVec.h"
#include "StFourVec.h"

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
class StProtoJet : public StFourVec
{

public:
  typedef std::vector<AbstractFourVec*> FourVecList;
	
  StProtoJet();
  StProtoJet(AbstractFourVec*);
  virtual ~StProtoJet();
	
  //access
	
  ///Number of particles in this protojet
  unsigned int numberOfParticles() const {return size();}
  unsigned int size() const {return mList.size();}
  FourVecList& list() {return mList;}
	
  ///The d=et^2 of the protojet
  double d() const {return eT()*eT();}
	
  ///Add a protojet to this one and calculate new parameters.  
  void merge(const StProtoJet&);
	
  ///Add a protojet to this one w/o calculating the new parameters
  void add(const StProtoJet&);
  void add(const AbstractFourVec& rhs) { StFourVec::add(rhs); }
  void remove(StProtoJet&);

  ///update the parameters of this protojet (in case some have been added via add())
  void update();
	
  ///clear
  void clear()
  {
    mList.clear();
    StFourVec::clear(); 
  }
	
  friend std::ostream& operator<<(std::ostream& os, const StProtoJet& j);
	
private:

  FourVecList mList;

};


inline std::ostream& operator<<(std::ostream& os, const StProtoJet& j)
{
    os <<"et: "<<j.eT()<<"\tphi: "<<j.phi()<<"\teta: "<<j.eta()
       <<"\tmass: "<<j.mass()<<"\tcharge: "<<j.charge()<<"\tParticles"<< std::endl;
    for (StProtoJet::FourVecList::const_iterator it=j.mList.begin(); it!=j.mList.end(); ++it) {
      os << **it << std::endl;
    }
    return os;
}

#endif

