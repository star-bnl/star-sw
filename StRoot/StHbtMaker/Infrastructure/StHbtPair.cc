/***************************************************************************
 *
 * $Id: StHbtPair.cc,v 1.10 2000/04/04 16:27:03 rcwells Exp $
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
 * Revision 1.10  2000/04/04 16:27:03  rcwells
 * Removed an errant cout in StHbtPair.cc
 *
 * Revision 1.9  2000/04/04 16:13:09  lisa
 * StHbtPair:quality() now returns normalized value (and so is double) and add a CorrFctn which looks at quality()
 *
 * Revision 1.8  2000/04/03 22:09:12  rcwells
 * Add member function ... quality().
 *
 * Revision 1.7  2000/02/13 21:13:33  lisa
 * changed ambiguous StHbtPair::fourMomentum() to fourMomentumSum() and fourMomentumDiff() and fixed related bug in QvecCorrFctn
 *
 * Revision 1.6  1999/07/29 16:16:34  lisa
 * Selemons upgrade of StHbtPair class
 *
 * Revision 1.5  1999/07/22 18:49:10  lisa
 * Implement idea of Fabrice to not create and delete StHbtPair all the time
 *
 * Revision 1.4  1999/07/12 18:57:05  lisa
 * fixed small bug in fourMomentum method of StHbtPair
 *
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

StHbtPair::StHbtPair(){
  mTrack1 = 0;
  mTrack2 = 0;
}

StHbtPair::StHbtPair(StHbtParticle* a, StHbtParticle* b)
  : mTrack1(a), mTrack2(b)
{ }


StHbtPair::~StHbtPair() {/* no-op */}

//StHbtPair::StHbtPair(const StHbtPair &a) {/* missing */}

//StHbtPair& StHbtPair::operator=(const StHbtPair &a)

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
// get rid of ambiguously-named method fourMomentum() and replace it with
// fourMomentumSum() and fourMomentumDiff() - mal 13feb2000
StHbtLorentzVector StHbtPair::fourMomentumSum() const
{
  StHbtLorentzVector temp = mTrack1->FourMomentum()+mTrack2->FourMomentum();
  return temp;
}
StHbtLorentzVector StHbtPair::fourMomentumDiff() const
{
  StHbtLorentzVector temp = mTrack1->FourMomentum()-mTrack2->FourMomentum();
  return temp;
}

//_________________
double StHbtPair::qOutCMS() const
{
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
double StHbtPair::qSideCMS() const
{
    StHbtThreeVector tmp1 = mTrack1->FourMomentum().vect();
    StHbtThreeVector tmp2 = mTrack2->FourMomentum().vect();

    double dx = tmp1.x() - tmp2.x();
    double xt = tmp1.x() + tmp2.x();
    
    double dy = tmp1.y() - tmp2.y();
    double yt = tmp1.y() + tmp2.y();


    double k1 = (sqrt(xt*xt+yt*yt));
    double k2 = (dx*xt+dy*yt);

    double k3 = dx - (k2/(k1*k1))*xt;
    double k4 = (k2/(k1*k1))*yt - dy;
    
    double tmp = sqrt(k3*k3 + k4*k4);
    return (tmp);
}

//_________________________
double StHbtPair::qLongCMS() const
{
    StHbtLorentzVector tmp1 = mTrack1->FourMomentum();
    StHbtLorentzVector tmp2 = mTrack2->FourMomentum();

    double dz = tmp1.z() - tmp2.z();
    double zz = tmp1.z() + tmp2.z();

    double dt = tmp1.t() - tmp2.t();
    double tt = tmp1.t() + tmp2.t();

    double beta = zz/tt;
    double gamma = 1.0/sqrt(1.0 - beta*beta);

    double temp = gamma*(dz - beta*dt);
    return (temp);
}

//________________________________
double StHbtPair::qOutPf() const
{
 StHbtLorentzVector tmp1 = mTrack1->FourMomentum();
 StHbtLorentzVector tmp2 = mTrack2->FourMomentum();

    double dt = tmp1.t() - tmp2.t();
    double tt = tmp1.t() + tmp2.t();

    double xt = tmp1.x() + tmp2.x();
    double yt = tmp1.y() + tmp2.y();

    double k1 = sqrt(xt*xt + yt*yt);
    double bOut = k1/tt;
    double gOut = 1.0/sqrt(1.0 - bOut*bOut);

    double temp = gOut*(this->qOutCMS() - bOut*dt);
    return (temp);
}

//___________________________________
double StHbtPair::qSidePf() const
{
 return(this->qSideCMS());
}

//___________________________________

double StHbtPair::qLongPf() const
{
 return(this->qLongCMS());
}

//___________________________________
double StHbtPair::qOutBf(double beta) const
{
 return(this->qOutCMS());
}

//___________________________________

double StHbtPair::qSideBf(double beta) const
{
 return(this->qSideCMS());
}

//___________________________________
double StHbtPair::qLongBf(double beta) const
{
    StHbtLorentzVector tmp1 = mTrack1->FourMomentum();
    StHbtLorentzVector tmp2 = mTrack2->FourMomentum();

    double dz = tmp1.z() - tmp2.z();
    double dt = tmp1.t() + tmp2.t();

    double gamma = 1.0/sqrt(1.0 - beta*beta);

    double temp = gamma*(dz - beta*dt);
    return (temp);
}

double StHbtPair::quality() const {
  unsigned long mapMask0 = 0xFFFFFF00;
  unsigned long mapMask1 = 0x1FFFFF;
  unsigned long padRow1To24Track1 = mTrack1->TopologyMap(0) & mapMask0;
  unsigned long padRow25To45Track1 = mTrack1->TopologyMap(1) & mapMask1;
  unsigned long padRow1To24Track2 = mTrack2->TopologyMap(0) & mapMask0;
  unsigned long padRow25To45Track2 = mTrack2->TopologyMap(1) & mapMask1;
  // AND logic
  unsigned long bothPads1To24 = padRow1To24Track1 & padRow1To24Track2;
  unsigned long bothPads25To45 = padRow25To45Track1 & padRow25To45Track2;
  // XOR logic
  unsigned long onePad1To24 = padRow1To24Track1 ^ padRow1To24Track2;
  unsigned long onePad25To45 = padRow25To45Track1 ^ padRow25To45Track2;
  unsigned long bitI;
  int ibits;
  int Quality = 0;
  double normQual = 0.0;
  int MaxQuality = mTrack1->NumberOfHits() + mTrack2->NumberOfHits();
  for (ibits=8;ibits<=31;ibits++) {
    bitI = 0;
    bitI |= 1UL<<(ibits);
    if ( onePad1To24 & bitI ) {
      Quality++;
      continue;
    }
    else{
      if ( bothPads1To24 & bitI ) Quality--;
    }
  }
  for (ibits=0;ibits<=20;ibits++) {
    bitI = 0;
    bitI |= 1UL<<(ibits);
    if ( onePad25To45 & bitI ) {
      Quality++;
      continue;
    }
    else{
      if ( bothPads25To45 & bitI ) Quality--;
    }
  }
  normQual = (double)Quality/( (double) MaxQuality );
  return ( normQual );

}
