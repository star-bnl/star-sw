/***************************************************************************
 *
 * $Id: StHbtTriplet.cc,v 1.2 2000/04/12 01:55:59 willson Exp $
 *
 * Author: Robert Willson, Ohio State, willson@bnl.gov
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package.
 *    The triplet object is passed to the TripletCuts for verification, and
 *    then to the AddRealTriplet and AddMixedTriplet methods of the
 *    three-particle correlation functions.
 *
 ***************************************************************************
 *
 * $Log: StHbtTriplet.cc,v $
 * Revision 1.2  2000/04/12 01:55:59  willson
 * Qinv Correlation Functions corrected
 *
 *
 ***************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtTriplet.hh"

StHbtTriplet::StHbtTriplet(){
  mTrack1 = 0;
  mTrack2 = 0;
  mTrack3 = 0;
}

StHbtTriplet::StHbtTriplet(StHbtParticle* a, StHbtParticle* b, StHbtParticle* c)
 : mTrack1(a), mTrack2(b), mTrack3(c)
{ }


StHbtTriplet::~StHbtTriplet() {/* no-op */}

//StHbtTriplet::StHbtTriplet(const StHbtTriplet &a) {/* missing */}

//StHbtTriplet& StHbtTriplet::operator=(const StHbtTriplet &a)

//_________________
double StHbtTriplet::qInv() const
{
    double dq = sqrt(fabs((mTrack1->FourMomentum() - mTrack2->FourMomentum()).m2()) +
                fabs((mTrack2->FourMomentum() - mTrack3->FourMomentum()).m2()) +
                fabs((mTrack3->FourMomentum() - mTrack1->FourMomentum()).m2()));
    return (dq);
}
//_________________
double StHbtTriplet::qInv12() const
{
    double dq = abs(mTrack1->FourMomentum() - mTrack2->FourMomentum());
    return (dq);
}
//_________________
double StHbtTriplet::qInv23() const
{
    double dq = abs(mTrack2->FourMomentum() - mTrack3->FourMomentum());
    return (dq);
}
//_________________
double StHbtTriplet::qInv31() const
{
    double dq = abs(mTrack3->FourMomentum() - mTrack1->FourMomentum());
    return (dq);
}
//_________________
double StHbtTriplet::mInv() const
{
    double InvariantMass = abs(mTrack1->FourMomentum() + mTrack2->FourMomentum() + mTrack3->FourMomentum());
    return (InvariantMass);
}
//_________________
double StHbtTriplet::kT() const
{

  double  tmp = 
    (mTrack1->FourMomentum() + mTrack2->FourMomentum() + mTrack3->FourMomentum()).perp();
  tmp /= 3.0;

  return (tmp);
}
//_________________
StHbtLorentzVector StHbtTriplet::fourMomentum() const
{
  StHbtLorentzVector temp = mTrack1->FourMomentum()+mTrack2->FourMomentum()+mTrack3->FourMomentum();
  return temp;
}
