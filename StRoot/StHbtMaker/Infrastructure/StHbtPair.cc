/***************************************************************************
 *
 * $Id: StHbtPair.cc,v 1.3 1999/07/06 22:33:22 lisa Exp $
 *
 * Author: Brian Laziuk, Yale University
 *         slightly modified by Mike Lisa
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *    the Pair object is passed to the PairCuts for verification, and
 *    then to the AddRealPair and AddMixedPair methods of the
 *    Correlation Functions
 *
 ***************************************************************************
 *
 * $Log: StHbtPair.cc,v $
 * Revision 1.3  1999/07/06 22:33:22  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.2  1999/06/29 17:50:27  fisyak
 * formal changes to account new StEvent, does not complie yet
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtPair.hh"

StHbtPair::StHbtPair(StHbtParticle* a, StHbtParticle* b)
  : mTrack1(a), mTrack2(b)
{ }


StHbtPair::~StHbtPair() {/* no-op */}

//StHbtPair::StHbtPair(const StHbtPair &a) {/* missing */}

//StHbtPair& StHbtPair::operator=(const StHbtPair &a)

//_________________
StHbtParticle* StHbtPair::track1() const
{
    return mTrack1;
}
//_________________
StHbtParticle* StHbtPair::track2() const
{
    return mTrack2;
}
//_________________
double StHbtPair::qInv() const
{
    double dq = abs(mTrack1->FourMomentum() - mTrack2->FourMomentum());
    return (dq);
}
//_________________
double StHbtPair::mInv() const
{
    double InvariantMass = abs(mTrack1->FourMomentum() + mTrack2->FourMomentum());
    return (InvariantMass);
}
//_________________
double StHbtPair::kT() const
{

  double  tmp = 
    (mTrack1->FourMomentum() + mTrack2->FourMomentum()).perp();
  tmp *= .5;

  return (tmp);
}
//_________________
double StHbtPair::qOut() const
{
  //brian    StThreeVectorD tmp1 = mTrack1.initialP();
  //brian    StThreeVectorD tmp2 = mTrack2.initialP();
    StHbtThreeVector tmp1 = mTrack1->FourMomentum().vect();
    StHbtThreeVector tmp2 = mTrack2->FourMomentum().vect();

    double dx = tmp1.x() - tmp2.x();
    double xt = tmp1.x() + tmp2.x();
    
    double dy = tmp1.y() - tmp2.y();
    double yt = tmp1.y() + tmp2.y();

    double k1 = (sqrt(xt*xt+yt*yt));
    double k2 = (dx*xt+dy*yt);
    double tmp = k2/k1;
    return (tmp);
}
//_________________
StHbtLorentzVector StHbtPair::fourMomentum() const
{
  StHbtLorentzVector temp = mTrack1->FourMomentum()-mTrack2->FourMomentum();
  return temp;
}

//_________________
double StHbtPair::qSide() const
{
  //brian    StThreeVectorD tmp1 = mTrack1.initialP();
  //brian    StThreeVectorD tmp2 = mTrack2.initialP();
    StHbtThreeVector tmp1 = mTrack1->FourMomentum().vect();
    StHbtThreeVector tmp2 = mTrack2->FourMomentum().vect();

    double dx = tmp1.x() - tmp2.x();
    double xt = tmp1.x() + tmp2.x();
    
    double dy = tmp1.y() - tmp2.y();
    double yt = tmp1.y() + tmp2.y();

    double k1 = (sqrt(xt*xt+yt*yt));
    double k2 = (dx*xt+dy*yt);

    double k3 = dx - (k2/k1)*xt;
    double k4 = (k2/k1)*yt - dy;
    
    double tmp = sqrt(k3*k3 + k4*k4);
    return (tmp);
}



