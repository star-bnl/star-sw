/***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *    the Triplet object is passed to the TripletCuts for verification, and
 *    then to the AddRealTriplet and AddMixedTriplet methods of the
 *    Three Particle Correlation Functions
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
    double dq = abs(mTrack1->FourMomentum() - mTrack2->FourMomentum()) +
                abs(mTrack2->FourMomentum() - mTrack3->FourMomentum()) +
                abs(mTrack3->FourMomentum() - mTrack1->FourMomentum());
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
