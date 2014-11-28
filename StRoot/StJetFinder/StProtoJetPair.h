//StProtoJetPair.h
//M.L. Miller (Yale Software)
//4/02

#ifndef StProtoJetPair_HH
#define StProtoJetPair_HH

#include "StProtoJet.h"

double deltaphi(double p1, double p2);

/*!
  \class StProtoJetPair
  \author M.L. Miller (Yale Software)
  StProtoJetPair is a simple class used internally by the kt cluster algorithm.  It calculates
  the et-weighted distance measure between two protojets.  In principle, StProtoJetPair
  could be transformed to a polymorphic design, allowing for the use of the same kt-cluster
  algorithm, but with differing distance measures.  Currently only the Ellis/Soper distance
  measure is available.
 */
class StProtoJetPair
{
public:

    StProtoJetPair(const StProtoJet& j1, const StProtoJet& j2, double r);
    virtual ~StProtoJetPair() {};

    //access
    double d() const;

    const StProtoJet& jet1() {return mJet1;}
    const StProtoJet& jet2() {return mJet2;}

private:
    //StProtoJetPair() {}; //not implemented
    const StProtoJet& mJet1; 
    const StProtoJet& mJet2;
    double mR;
};

inline double StProtoJetPair::d() const
{
    double deta = mJet1.eta()-mJet2.eta();
    double dphi = deltaphi(mJet1.phi(), mJet2.phi());
    double d1 = mJet1.d();
    double d2 = mJet2.d();
    double d = (d1<d2) ? d1 : d2;

    return d*(deta*deta + dphi*dphi)/ (mR*mR);
}

#endif

