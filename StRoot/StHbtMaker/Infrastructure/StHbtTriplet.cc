/***************************************************************************
 *
 * $Id: StHbtTriplet.cc,v 1.4 2003/09/02 17:58:32 perev Exp $
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
 * Revision 1.4  2003/09/02 17:58:32  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  2001/06/05 00:59:29  willson
 * Added entrance separation and quality methods
 *
 * Revision 1.2  2000/04/12 01:55:59  willson
 * Qinv Correlation Functions corrected
 *
 *
 ***************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtTriplet.hh"

double Triplet_Quality_Calc(StHbtParticle* Track1, StHbtParticle* Track2) {
  unsigned long mapMask0 = 0xFFFFFF00;
  unsigned long mapMask1 = 0x1FFFFF;
  unsigned long padRow1To24Track1 = Track1->TopologyMap(0) & mapMask0;
  unsigned long padRow25To45Track1 = Track1->TopologyMap(1) & mapMask1;
  unsigned long padRow1To24Track2 = Track2->TopologyMap(0) & mapMask0;
  unsigned long padRow25To45Track2 = Track2->TopologyMap(1) & mapMask1;
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
  int MaxQuality = Track1->NumberOfHits() + Track2->NumberOfHits();
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
    double dq = ::sqrt(fabs((mTrack1->FourMomentum() - mTrack2->FourMomentum()).m2()) +
                fabs((mTrack2->FourMomentum() - mTrack3->FourMomentum()).m2()) +
                fabs((mTrack3->FourMomentum() - mTrack1->FourMomentum()).m2()));
    return (dq);
}
//_________________
double StHbtTriplet::qInv12() const
{
    double dq = ::sqrt(fabs((mTrack1->FourMomentum() - mTrack2->FourMomentum()).m2()));
    return (dq);
}
//_________________
double StHbtTriplet::qInv23() const
{
    double dq = ::sqrt(fabs((mTrack2->FourMomentum() - mTrack3->FourMomentum()).m2()));
    return (dq);
}
//_________________
double StHbtTriplet::qInv31() const
{
    double dq = ::sqrt(fabs((mTrack3->FourMomentum() - mTrack1->FourMomentum()).m2()));
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
//_________________
double StHbtTriplet::quality() const {

  double Q1 = Triplet_Quality_Calc(mTrack1, mTrack2);
  double Q2 = Triplet_Quality_Calc(mTrack2, mTrack3);
  double Q3 = Triplet_Quality_Calc(mTrack3, mTrack1);
  
  if (Q1>Q2) {
    if (Q1>Q3) return Q1;
    else return Q3;
  }
  else
    if (Q2>Q3) return Q2;
    else return Q3;
  
}

double StHbtTriplet::NominalTpcExitSeparation() const {
  StHbtThreeVector diff1 = mTrack1->NominalTpcExitPoint() - mTrack2->NominalTpcExitPoint();
  StHbtThreeVector diff2 = mTrack2->NominalTpcExitPoint() - mTrack3->NominalTpcExitPoint();
  StHbtThreeVector diff3 = mTrack3->NominalTpcExitPoint() - mTrack1->NominalTpcExitPoint();
  if (diff1.mag()<diff2.mag()) {
    if (diff1.mag()<diff3.mag()) return (diff1.mag());
    else return (diff3.mag());
  }
  else if (diff2.mag()<diff3.mag()) return (diff2.mag());
  else return (diff3.mag());
}

double StHbtTriplet::NominalTpcEntranceSeparation() const {
  StHbtThreeVector diff1 = mTrack1->NominalTpcEntrancePoint() - mTrack2->NominalTpcEntrancePoint();
  StHbtThreeVector diff2 = mTrack2->NominalTpcEntrancePoint() - mTrack3->NominalTpcEntrancePoint();
  StHbtThreeVector diff3 = mTrack3->NominalTpcEntrancePoint() - mTrack1->NominalTpcEntrancePoint();
  if (diff1.mag()<diff2.mag()) {
    if (diff1.mag()<diff3.mag()) return (diff1.mag());
    else return (diff3.mag());
  }
  else if (diff2.mag()<diff3.mag()) return (diff2.mag());
  else return (diff3.mag());
}

double StHbtTriplet::NominalTpcAverageSeparation() const {
  StHbtThreeVector diff1,diff2,diff3;
  double AveSep1 = 0.0;
  double AveSep2 = 0.0;
  double AveSep3 = 0.0;
  int ipt=0;
  for (ipt=0; ipt<11; ipt++){
    diff1 = mTrack1->mNominalPosSample[ipt] - mTrack2->mNominalPosSample[ipt];
    AveSep1 += diff1.mag();
    diff2 = mTrack2->mNominalPosSample[ipt] - mTrack3->mNominalPosSample[ipt];
    AveSep2 += diff2.mag();
    diff3 = mTrack3->mNominalPosSample[ipt] - mTrack1->mNominalPosSample[ipt];
    AveSep3 += diff3.mag();
  }
  AveSep1 = AveSep1/11.0;
  AveSep2 = AveSep1/11.0;
  AveSep3 = AveSep1/11.0;
  if (AveSep1<AveSep2) {
    if (AveSep1<AveSep3) return (AveSep1);
    else return (AveSep3);
  }
  else if (AveSep2<AveSep3) return (AveSep2);
  else return (AveSep3);
}

