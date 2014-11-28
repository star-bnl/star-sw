/***************************************************************************
 *
 * $Id: StHbtPair.cc,v 1.23
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
 * Revision 1.23  2002/09/25 19:23:25  rcwells
 * Added const to emissionAngle()
 *
 * Revision 1.22  2002/04/22 22:48:11  laue
 * corrected calculation of opening angle 
 **
 * $Log: StHbtPair.cc,v $
 * Revision 1.29  2009/09/23 04:43:41  fine
 * Fix StLorentxVector ctor
 *
 * Revision 1.28  2009/09/23 00:51:21  jeromel
 * Fix for StThreevector
 *
 * Revision 1.27  2003/09/02 17:58:32  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.26  2003/01/31 19:57:15  magestro
 * Cleared up simple compiler warnings on i386_linux24
 *
 * Revision 1.25  2003/01/14 09:44:08  renault
 * corrections on average separation calculation for tracks which doesn't cross
 * all 45 padrows.
 *
 * Revision 1.24  2002/11/19 23:33:10  renault
 * Enable average separation calculation for all combinaisons of
 * V0 daughters and tracks
 *
 * Revision 1.21  2002/02/28 14:18:36  rcwells
 * Added emissionAngle function to StHbtPair
 *
 * Revision 1.20  2001/12/14 23:11:30  fretiere
 * Add class HitMergingCut. Add class fabricesPairCut = HitMerginCut + pair purity cuts. Add TpcLocalTransform function which convert to local tpc coord (not pretty). Modify StHbtTrack, StHbtParticle, StHbtHiddenInfo, StHbtPair to handle the hit information and cope with my code
 *
 * Revision 1.19  2001/04/25 18:05:09  perev
 * HPcorrs
 *
 * Revision 1.18  2001/04/03 21:04:36  kisiel
 *
 *
 *   Changes needed to make the Theoretical code
 *   work. The main code is the ThCorrFctn directory.
 *   The most visible change is the addition of the
 *   HiddenInfo to StHbtPair.
 *
 * Revision 1.17  2001/03/28 22:35:20  flierl
 * changes and bugfixes in qYKP*
 * add pairrapidity
 *
 * Revision 1.16  2001/02/15 19:23:00  rcwells
 * Fixed sign in qSideCMS
 *
 * Revision 1.15  2001/01/22 22:56:41  laue
 * Yano-Koonin-Podgoretskii Parametrisation added
 *
 * Revision 1.14  2000/12/11 21:44:30  rcwells
 * Corrected qSideCMS function
 *
 * Revision 1.13  2000/10/26 16:09:16  lisa
 * Added OpeningAngle PairCut class and method to StHbtPair
 *
 * Revision 1.12  2000/10/05 23:09:05  lisa
 * Added kT-dependent radii to mixed-event simulator AND implemented AverageSeparation Cut and CorrFctn
 *
 * Revision 1.11  2000/07/17 20:03:16  lisa
 * Implemented tools for addressing and assessing trackmerging
 *
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

double StHbtPair::mMaxDuInner = .8;
double StHbtPair::mMaxDzInner = 3.;
double StHbtPair::mMaxDuOuter = 1.4;
double StHbtPair::mMaxDzOuter = 3.2;


StHbtPair::StHbtPair(){
  mTrack1 = 0;
  mTrack2 = 0;
  setDefaultHalfFieldMergingPar();
}

StHbtPair::StHbtPair(StHbtParticle* a, StHbtParticle* b)
  : mTrack1(a), mTrack2(b)
{ 
  setDefaultHalfFieldMergingPar();
}

void StHbtPair::setDefaultHalfFieldMergingPar(){
  mMaxDuInner = 3;
  mMaxDzInner = 4.;
  mMaxDuOuter = 4.;
  mMaxDzOuter = 6.;
}
void StHbtPair::setDefaultFullFieldMergingPar(){
  mMaxDuInner = 0.8;
  mMaxDzInner = 3.;
  mMaxDuOuter = 1.4;
  mMaxDzOuter = 3.2;
}
void StHbtPair::setMergingPar(double aMaxDuInner, double aMaxDzInner,
			      double aMaxDuOuter, double aMaxDzOuter){
  mMaxDuInner = aMaxDuInner;
  mMaxDzInner = aMaxDzInner;
  mMaxDuOuter = aMaxDuOuter;
  mMaxDzOuter = aMaxDzOuter;
};

StHbtPair::~StHbtPair() {/* no-op */}

//StHbtPair::StHbtPair(const StHbtPair &a) {/* missing */}

//StHbtPair& StHbtPair::operator=(const StHbtPair &a)

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
double StHbtPair::rap() const
{
  // longitudinal pair rapidity : Y = 0.5 ::log( E1 + E2 + pz1 + pz2 / E1 + E2 - pz1 - pz2 )
  double  tmp = 0.5 * log (
			   (mTrack1->FourMomentum().e() + mTrack2->FourMomentum().e() + mTrack1->FourMomentum().z() + mTrack2->FourMomentum().z()) / 
			   (mTrack1->FourMomentum().e() + mTrack2->FourMomentum().e() - mTrack1->FourMomentum().z() - mTrack2->FourMomentum().z()) 
			   ) ;
  return (tmp);
}
//_________________
double StHbtPair::emissionAngle()const {
  double pxTotal = this->fourMomentumSum().x();
  double pyTotal = this->fourMomentumSum().y();
  double angle = atan2(pyTotal,pxTotal)*180.0/3.1415926536;
  if (angle<0.0) angle+=360.0;
  return angle;
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
//__________________________________
// Yano-Koonin-Podgoretskii Parametrisation in CMS
void StHbtPair::qYKPCMS(double& qP, double& qT, double& q0) const
{
  ////
  // calculate momentum difference in source rest frame (= lab frame)
  ////
  StHbtLorentzVector l1 = mTrack1->FourMomentum() ;
  StHbtLorentzVector l2 = mTrack2->FourMomentum() ;
  StHbtLorentzVector  l ;
  // random ordering of the particles
  if ( rand()/(double)RAND_MAX > 0.50 )  
    { l = l1-l2 ; } 
  else 
    { l = l2-l1 ; } ;
  // fill momentum differences into return variables
  qP = l.z() ;
  qT = l.vect().perp() ;
  q0 = l.e() ;
}
//___________________________________
// Yano-Koonin-Podgoretskii Parametrisation in LCMS
void StHbtPair::qYKPLCMS(double& qP, double& qT, double& q0) const
{
  ////
  //  calculate momentum difference in LCMS : frame where pz1 + pz2 = 0
  ////
  StHbtLorentzVector l1 = mTrack1->FourMomentum() ;
  StHbtLorentzVector l2 = mTrack2->FourMomentum() ;
  // determine beta to LCMS
  double beta = (l1.z()+l2.z()) / (l1.e()+l2.e()) ;
  double beta2 =  beta*beta ;
  // unfortunately STAR Class lib knows only boost(particle) not boost(beta) :(
  // -> create particle with velocity beta and mass 1.0
  // actually this is : dummyPz = ::sqrt( (dummyMass*dummyMass*beta2) / (1-beta2) ) ; 
  double dummyPz = ::sqrt( (beta2) / (1-beta2) ) ;
  // boost in the correct direction
  if (beta>0.0) { dummyPz = -dummyPz; } ;

  // create dummy particle
  StHbtLorentzVector  l(0.0, 0.0, dummyPz,0) ; 
  double dummyMass = 1.0 ;

  l.setZ(dummyPz);
  l.setE(l.vect().massHypothesis(dummyMass) );

  // boost particles along the beam into a frame with velocity beta 
  StHbtLorentzVector l1boosted = l1.boost(l) ;
  StHbtLorentzVector l2boosted = l2.boost(l) ;
  // caculate the momentum difference with random ordering of the particle
  if ( rand()/(double)RAND_MAX >0.50)  
    { l = l1boosted-l2boosted ; } 
  else 
    { l = l2boosted-l1boosted ;} ;
  // fill momentum differences into return variables
  qP = l.z() ;
  qT = l.vect().perp() ;
  q0 = l.e() ;
}
//___________________________________
// Yano-Koonin-Podgoretskii Parametrisation in pair rest frame
void StHbtPair::qYKPPF(double& qP, double& qT, double& q0) const
{
  ////
  //  calculate momentum difference in pair rest frame : frame where (pz1 + pz2, py1 + py2, px1 + px2) = (0,0,0)
  ////
  StHbtLorentzVector l1 = mTrack1->FourMomentum() ;
  StHbtLorentzVector l2 = mTrack2->FourMomentum() ;
  // the center of gravity of the pair travels with l
  StHbtLorentzVector  l = l1 + l2 ; 
  l = -l ;
  l.setE(-l.e()) ;
  // boost particles  
  StHbtLorentzVector l1boosted = l1.boost(l) ;
  StHbtLorentzVector l2boosted = l2.boost(l) ;
  // caculate the momentum difference with random ordering of the particle
  if ( rand()/(double)RAND_MAX > 0.50)  
    { l = l1boosted-l2boosted ; } 
  else 
    { l = l2boosted-l1boosted ;} ;
  // fill momentum differences into return variables
  qP = l.z();
  qT = l.vect().perp();
  q0 = l.e();
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

    double k1 = (::sqrt(xt*xt+yt*yt));
    double k2 = (dx*xt+dy*yt);
    double tmp = k2/k1;
    return (tmp);
}
//_________________
double StHbtPair::qSideCMS() const
{
    StHbtThreeVector tmp1 = mTrack1->FourMomentum().vect();
    StHbtThreeVector tmp2 = mTrack2->FourMomentum().vect();

    double x1 = tmp1.x();  double y1 = tmp1.y();
    double x2 = tmp2.x();  double y2 = tmp2.y();

    double xt = x1+x2;  double yt = y1+y2;
    double k1 = ::sqrt(xt*xt+yt*yt);

    double tmp = 2.0*(x2*y1-x1*y2)/k1;
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
    double gamma = 1.0/::sqrt(1.0 - beta*beta);

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

    double k1 = ::sqrt(xt*xt + yt*yt);
    double bOut = k1/tt;
    double gOut = 1.0/::sqrt(1.0 - bOut*bOut);

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

    double gamma = 1.0/::sqrt(1.0 - beta*beta);

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

double StHbtPair::quality2() const {
  unsigned long mapMask0 = 0xFFFFFF00;
  unsigned long mapMask1 = 0x1FFFFF;
  unsigned long padRow1To24Track1 = mTrack1->TopologyMap(0) & mapMask0;
  unsigned long padRow25To45Track1 = mTrack1->TopologyMap(1) & mapMask1;
  unsigned long padRow1To24Track2 = mTrack2->TopologyMap(0) & mapMask0;
  unsigned long padRow25To45Track2 = mTrack2->TopologyMap(1) & mapMask1;

  // AND logic
  //unsigned long bothPads1To24 = padRow1To24Track1 & padRow1To24Track2;
  //unsigned long bothPads25To45 = padRow25To45Track1 & padRow25To45Track2;

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
    //else{
    //if ( bothPads1To24 & bitI ) Quality--;
    //}
  }
  for (ibits=0;ibits<=20;ibits++) {
    bitI = 0;
    bitI |= 1UL<<(ibits);
    if ( onePad25To45 & bitI ) {
      Quality++;
      continue;
    }
    //else{
    //if ( bothPads25To45 & bitI ) Quality--;
    //}
  }
  normQual = (double)Quality/( (double) MaxQuality );
  return ( normQual );

}


double StHbtPair::NominalTpcExitSeparation() const {
  StHbtThreeVector diff = mTrack1->NominalTpcExitPoint() - mTrack2->NominalTpcExitPoint();
  return (diff.mag());
}

double StHbtPair::NominalTpcEntranceSeparation() const {
  StHbtThreeVector diff = mTrack1->NominalTpcEntrancePoint() - mTrack2->NominalTpcEntrancePoint();
  return (diff.mag());
}

double StHbtPair::NominalTpcAverageSeparation() const {
  StHbtThreeVector diff;
  double AveSep = 0.0;
  int ipt = 0;
  if (mTrack1->mNominalPosSample && mTrack2->mNominalPosSample){
  while (fabs(mTrack1->mNominalPosSample[ipt].x())<9999. &&
	 fabs(mTrack1->mNominalPosSample[ipt].y())<9999. && 
	 fabs(mTrack1->mNominalPosSample[ipt].z())<9999. &&
	 fabs(mTrack2->mNominalPosSample[ipt].x())<9999. &&
	 fabs(mTrack2->mNominalPosSample[ipt].y())<9999. && 
	 fabs(mTrack2->mNominalPosSample[ipt].z())<9999. &&
	 ipt<11
	 ){
    //  for (int ipt=0; ipt<11; ipt++){
    diff = mTrack1->mNominalPosSample[ipt] - mTrack2->mNominalPosSample[ipt];
    ipt++;
    AveSep += diff.mag();
  }
  AveSep = AveSep/(ipt+1.);
  return (AveSep);}
  else return -1;
}

double StHbtPair::OpeningAngle() const {
 return 57.296* mTrack1->FourMomentum().vect().angle( mTrack2->FourMomentum().vect() );
//   StHbtThreeVector p1 = mTrack1->FourMomentum().vect();
//   StHbtThreeVector p2 = mTrack2->FourMomentum().vect();
//   return 57.296*(p1.phi()-p2.phi());
//   //double dAngInv = 57.296*acos((p1.dot(p2))/(p1.mag()*p2.mag()));
//   //return (dAngInv);
}
//_________________


double StHbtPair::KStarFlipped() const {
  StHbtLorentzVector tP1 = mTrack1->FourMomentum();

  StThreeVectorD qwe = tP1.vect();
  qwe *= -1.; // flip it
  tP1.setVect(qwe);
  
  StHbtLorentzVector tSum = (tP1+mTrack2->FourMomentum());
  double tMass = abs(tSum);
  StThreeVectorD tGammaBeta = (1./tMass)*tSum.vect(); 
  double tGamma = tSum.e()/tMass;
  StThreeVectorD tLongMom  = ((tP1.vect()*tGammaBeta)/
			      (tGammaBeta*tGammaBeta))*tGammaBeta;
  StLorentzVectorD tK(tGamma*tP1.e() - tP1.vect()*tGammaBeta,
		      tP1.vect() + (tGamma-1.)*tLongMom - tP1.e()*tGammaBeta);
//VP  tP1.vect() *= -1.; // unflip it
  return tK.vect().mag();
}

//double StHbtPair::CVK() const{
//const StHbtLorentzVector& tP1 = mTrack1->FourMomentum();
//StHbtLorentzVector tSum = (tP1+mTrack2->FourMomentum());
//double tMass = abs(tSum);
//StThreeVectorD tGammaBeta = (1./tMass)*tSum.vect(); 
//double tGamma = tSum.e()/tMass;
//StThreeVectorD tLongMom  = ((tP1.vect()*tGammaBeta)/
//		      (tGammaBeta*tGammaBeta))*tGammaBeta;
//StLorentzVectorD tK(tGamma*tP1.e() - tP1.vect()*tGammaBeta,
//	      tP1.vect() + (tGamma-1.)*tLongMom - tP1.e()*tGammaBeta);
//return (tK.vect())*tGammaBeta/tK.vect().magnitude()/tGammaBeta.magnitude();
//}

double StHbtPair::CVKFlipped() const{
  StHbtLorentzVector tP1 = mTrack1->FourMomentum();
  StThreeVectorD qwe = tP1.vect();
  qwe *= -1.; // flip it
  tP1.setVect(qwe);
  
  StHbtLorentzVector tSum = (tP1+mTrack2->FourMomentum());
  double tMass = abs(tSum);
  StThreeVectorD tGammaBeta = (1./tMass)*tSum.vect(); 
  double tGamma = tSum.e()/tMass;
  StThreeVectorD tLongMom  = ((tP1.vect()*tGammaBeta)/
			      (tGammaBeta*tGammaBeta))*tGammaBeta;
  StLorentzVectorD tK(tGamma*tP1.e() - tP1.vect()*tGammaBeta,
		      tP1.vect() + (tGamma-1.)*tLongMom - tP1.e()*tGammaBeta);
//VP  tP1.vect() *= -1.; // unflip it
  return (tK.vect())*tGammaBeta/tGamma;
}

double StHbtPair::pInv() const{
  StHbtLorentzVector tP1 = mTrack1->FourMomentum();
  StHbtLorentzVector tP2 = mTrack2->FourMomentum();
  double tP = (tP1.px()+tP2.px())*(tP1.px()+tP2.px())+
              (tP1.py()+tP2.py())*(tP1.py()+tP2.py())+
              (tP1.pz()+tP2.pz())*(tP1.pz()+tP2.pz())-
              (tP1.e() -tP2.e() )*(tP1.e() -tP2.e() );
  return ::sqrt(fabs(tP));
}

double StHbtPair::qInvFlippedXY() const{
  StHbtLorentzVector tP1 = mTrack1->FourMomentum();
  tP1.setX(-1.*tP1.x());
  tP1.setY(-1.*tP1.y());
  StHbtLorentzVector tDiff = (tP1-mTrack2->FourMomentum());
  return ( -1.* tDiff.m());
}

void StHbtPair::calcNonIdPar() const{ // fortran like function! faster?
  mNonIdParNotCalculated=0;
  double px1 = mTrack1->FourMomentum().vect().x();
  double py1 = mTrack1->FourMomentum().vect().y();
  double pz1 = mTrack1->FourMomentum().vect().z();
  double pE1  = mTrack1->FourMomentum().e();
  double Particle1Mass = ::sqrt(pE1*pE1 - px1*px1 - py1*py1 - pz1*pz1);
  double px2 = mTrack2->FourMomentum().vect().x();
  double py2 = mTrack2->FourMomentum().vect().y();
  double pz2 = mTrack2->FourMomentum().vect().z();
  double pE2  = mTrack2->FourMomentum().e();
  double Particle2Mass = ::sqrt(pE2*pE2 - px2*px2 - py2*py2 - pz2*pz2);

  double Px = px1+px2;
  double Py = py1+py2;
  double Pz = pz1+pz2;
  double PE = pE1+pE2;
      
  double Ptrans = Px*Px + Py*Py;
  double Mtrans = PE*PE - Pz*Pz;
  double Pinv =   ::sqrt(Mtrans - Ptrans);
  Mtrans = ::sqrt(Mtrans);
  Ptrans = ::sqrt(Ptrans);
	
  double QinvL = (pE1-pE2)*(pE1-pE2) - (px1-px2)*(px1-px2) -
    (py1-py2)*(py1-py2) - (pz1-pz2)*(pz1-pz2);

  double Q = (Particle1Mass*Particle1Mass - Particle2Mass*Particle2Mass)/Pinv;
  Q = sqrt ( Q*Q - QinvL);
	  
  kStarCalc = Q/2;

  // ad 1) go to LCMS
  double beta = Pz/PE;
  double gamma = PE/Mtrans;
	    
  double pz1L = gamma * (pz1 - beta * pE1);
  double pE1L = gamma * (pE1 - beta * pz1);
  
  // fill histogram for beam projection ( z - axis )
  mDKLong = pz1L;

  // ad 2) rotation px -> Pt
  double px1R = (px1*Px + py1*Py)/Ptrans;
  double py1R = (-px1*Py + py1*Px)/Ptrans;
  
  //fill histograms for side projection ( y - axis )
  mDKSide = py1R;

  // ad 3) go from LCMS to CMS
  beta = Ptrans/Mtrans;
  gamma = Mtrans/Pinv;
  
  double px1C = gamma * (px1R - beta * pE1L);
  
  // fill histogram for out projection ( x - axis )
  mDKOut  = px1C;

  mCVK = (mDKOut*Ptrans + mDKLong*Pz)/kStarCalc/::sqrt(Ptrans*Ptrans+Pz*Pz);
}


void StHbtPair::calcNonIdParGlobal() const{ // fortran like function! faster?
  mNonIdParNotCalculatedGlobal=0;
  double px1 = mTrack1->Track()->PGlobal().x();
  double py1 = mTrack1->Track()->PGlobal().y();
  double pz1 = mTrack1->Track()->PGlobal().z();
  double Particle1Mass =  mTrack1->FourMomentum().m2();
  double pE1  = ::sqrt(Particle1Mass + px1*px1 + py1*py1 + pz1*pz1);
  Particle1Mass = ::sqrt(Particle1Mass);

  double px2 = mTrack2->Track()->PGlobal().x();
  double py2 = mTrack2->Track()->PGlobal().y();
  double pz2 = mTrack2->Track()->PGlobal().z();
  double Particle2Mass =  mTrack2->FourMomentum().m2();
  double pE2  = ::sqrt(Particle2Mass + px2*px2 + py2*py2 + pz2*pz2);
  Particle2Mass = ::sqrt(Particle2Mass);

  double Px = px1+px2;
  double Py = py1+py2;
  double Pz = pz1+pz2;
  double PE = pE1+pE2;
      
  double Ptrans = Px*Px + Py*Py;
  double Mtrans = PE*PE - Pz*Pz;
  double Pinv =   ::sqrt(Mtrans - Ptrans);
  Mtrans = ::sqrt(Mtrans);
  Ptrans = ::sqrt(Ptrans);
	
  double QinvL = (pE1-pE2)*(pE1-pE2) - (px1-px2)*(px1-px2) -
    (py1-py2)*(py1-py2) - (pz1-pz2)*(pz1-pz2);

  double Q = (Particle1Mass*Particle1Mass - Particle2Mass*Particle2Mass)/Pinv;
  Q = sqrt ( Q*Q - QinvL);
	  
  kStarCalcGlobal = Q/2;

  // ad 1) go to LCMS
  double beta = Pz/PE;
  double gamma = PE/Mtrans;
	    
  double pz1L = gamma * (pz1 - beta * pE1);
  double pE1L = gamma * (pE1 - beta * pz1);
  
  // fill histogram for beam projection ( z - axis )
  mDKLongGlobal = pz1L;

  // ad 2) rotation px -> Pt
  double px1R = (px1*Px + py1*Py)/Ptrans;
  double py1R = (-px1*Py + py1*Px)/Ptrans;
  
  //fill histograms for side projection ( y - axis )
  mDKSideGlobal = py1R;

  // ad 3) go from LCMS to CMS
  beta = Ptrans/Mtrans;
  gamma = Mtrans/Pinv;
  
  double px1C = gamma * (px1R - beta * pE1L);
  
  // fill histogram for out projection ( x - axis )
  mDKOutGlobal  = px1C;

  mCVKGlobal = (mDKOutGlobal*Ptrans + mDKLongGlobal*Pz)/
    kStarCalcGlobal/::sqrt(Ptrans*Ptrans+Pz*Pz);
}



double StHbtPair::dcaInsideTpc() const{

  double tMinDist=NominalTpcEntranceSeparation();
  double tExit = NominalTpcExitSeparation();
  tMinDist = (tExit>tMinDist) ? tMinDist : tExit;
  double tInsideDist;
  //tMinDist = 999.;

  double rMin = 60.;
  double rMax = 190.;
  const StPhysicalHelixD& tHelix1 = mTrack1->Helix();
  const StPhysicalHelixD& tHelix2 = mTrack2->Helix();
  // --- One is a line and other one a helix
  //if (tHelix1.mSingularity != tHelix2.mSingularity) return -999.;
  // --- 2 lines : don't care right now
  //if (tHelix1.mSingularity)  return -999.;
  // --- 2 helix
  double dx = tHelix2.xcenter() - tHelix1.xcenter();
  double dy = tHelix2.ycenter() - tHelix1.ycenter();
  double dd = ::sqrt(dx*dx + dy*dy);
  double r1 = 1/tHelix1.curvature();
  double r2 = 1/tHelix2.curvature();
  double cosAlpha = (r1*r1 + dd*dd - r2*r2)/(2*r1*dd);
    
  double x, y, r;
  double s;
  if (fabs(cosAlpha) < 1) {           // two solutions
    double sinAlpha = sin(acos(cosAlpha));
    x = tHelix1.xcenter() + r1*(cosAlpha*dx - sinAlpha*dy)/dd;
    y = tHelix1.ycenter() + r1*(sinAlpha*dx + cosAlpha*dy)/dd;
    r = ::sqrt(x*x+y*y);
    if( r > rMin &&  r < rMax && 
	fabs(atan2(y,x)-mTrack1->NominalTpcEntrancePoint().phi())< 0.5
	){ // first solution inside
      s = tHelix1.pathLength(x, y);
      tInsideDist=tHelix2.distance(tHelix1.at(s));
      if(tInsideDist<tMinDist) tMinDist = tInsideDist;
    }
    else{ 
      x = tHelix1.xcenter() + r1*(cosAlpha*dx + sinAlpha*dy)/dd;
      y = tHelix1.ycenter() + r1*(cosAlpha*dy - sinAlpha*dx)/dd;
      r = ::sqrt(x*x+y*y);
      if( r > rMin &&  r < rMax &&
	  fabs(atan2(y,x)-mTrack1->NominalTpcEntrancePoint().phi())< 0.5
	  ) {  // second solution inside
        s = tHelix1.pathLength(x, y);
        tInsideDist=tHelix2.distance(tHelix1.at(s));
        if(tInsideDist<tMinDist) tMinDist = tInsideDist;
      }     
    }
  }
  return tMinDist;
}

void StHbtPair::calcMergingPar() const{
  mMergingParNotCalculated=0;

  double tDu, tDz;
  int tN = 0;
  mFracOfMergedRow = 0.;
  mWeightedAvSep =0.;
  double tDist;
  double tDistMax = 200.;
  for(int ti=0 ; ti<45 ; ti++){
    if(mTrack1->mSect[ti]==mTrack2->mSect[ti] && mTrack1->mSect[ti]!=-1){
      tDu = fabs(mTrack1->mU[ti]-mTrack2->mU[ti]);
      tDz = fabs(mTrack1->mZ[ti]-mTrack2->mZ[ti]);
      tN++;
      if(ti<13){
	mFracOfMergedRow += (tDu<mMaxDuInner && tDz<mMaxDzInner);
	tDist = ::sqrt(tDu*tDu/mMaxDuInner/mMaxDuInner+
		     tDz*tDz/mMaxDzInner/mMaxDzInner);
	//mFracOfMergedRow += (tDu<mMaxDuInner && tDz<mMaxDzInner);
      }
      else{
	mFracOfMergedRow += (tDu<mMaxDuOuter && tDz<mMaxDzOuter);
	tDist = ::sqrt(tDu*tDu/mMaxDuOuter/mMaxDuOuter+
		     tDz*tDz/mMaxDzOuter/mMaxDzOuter);
	//mFracOfMergedRow += (tDu<mMaxDuOuter && tDz<mMaxDzOuter);
      }
      if(tDist<tDistMax){
	mClosestRowAtDCA = ti+1;
	tDistMax = tDist;
      }
      mWeightedAvSep += tDist;
    }
  }
  if(tN>0){
    mWeightedAvSep /= tN;
    mFracOfMergedRow /= tN;
  }
  else{
    mClosestRowAtDCA = -1;
    mFracOfMergedRow = -1.;
    mWeightedAvSep = -1.;
  }
}
//________________V0 daughters exit/entrance/average separation calc.
//_______1st part is a track 2nd is a V0 considering Pos daughter
double StHbtPair::TpcExitSeparationTrackV0Pos() const {
  StHbtThreeVector diff = mTrack1->NominalTpcExitPoint() - mTrack2->TpcV0PosExitPoint();
  return (diff.mag());
}

double StHbtPair::TpcEntranceSeparationTrackV0Pos() const {
  StHbtThreeVector diff = mTrack1->NominalTpcEntrancePoint() - mTrack2->TpcV0PosEntrancePoint();
  return (diff.mag());
}

double StHbtPair::TpcAverageSeparationTrackV0Pos() const {
  StHbtThreeVector diff;
  double AveSep = 0.0;
  int ipt = 0;
  if (mTrack1->mNominalPosSample && mTrack2->mNominalPosSample){
  while (fabs(mTrack1->mNominalPosSample[ipt].x())<9999. &&
	 fabs(mTrack1->mNominalPosSample[ipt].y())<9999. && 
	 fabs(mTrack1->mNominalPosSample[ipt].z())<9999. &&
	 fabs(mTrack2->mNominalPosSample[ipt].x())<9999. &&
	 fabs(mTrack2->mNominalPosSample[ipt].y())<9999. && 
	 fabs(mTrack2->mNominalPosSample[ipt].z())<9999. &&
	 (ipt<11)
	 ){
    diff = mTrack1->mNominalPosSample[ipt] - mTrack2->mNominalPosSample[ipt];
    ipt++;
    AveSep += diff.mag();
  }
  AveSep = AveSep/(ipt+1.);
  return (AveSep);}
  else return -1;
}
//_______1st part is a track 2nd is a V0 considering Neg daughter
double StHbtPair::TpcExitSeparationTrackV0Neg() const {
  StHbtThreeVector diff = mTrack1->NominalTpcExitPoint() - mTrack2->TpcV0NegExitPoint();
  return (diff.mag());
}

double StHbtPair::TpcEntranceSeparationTrackV0Neg() const {
  StHbtThreeVector diff = mTrack1->NominalTpcEntrancePoint() - mTrack2->TpcV0NegEntrancePoint();
  return (diff.mag());
}

double StHbtPair::TpcAverageSeparationTrackV0Neg() const {
  StHbtThreeVector diff;
  double AveSep = 0.0;
  int ipt = 0;
  if (mTrack1->mNominalPosSample && mTrack2->mTpcV0NegPosSample){
  while (fabs(mTrack1->mNominalPosSample[ipt].x())<9999. &&
	 fabs(mTrack1->mNominalPosSample[ipt].y())<9999. && 
	 fabs(mTrack1->mNominalPosSample[ipt].z())<9999. &&
	 fabs(mTrack2->mTpcV0NegPosSample[ipt].x())<9999. &&
	 fabs(mTrack2->mTpcV0NegPosSample[ipt].y())<9999. && 
	 fabs(mTrack2->mTpcV0NegPosSample[ipt].z())<9999. &&
	 (ipt<11)
	 ){
    diff = mTrack1->mNominalPosSample[ipt] - mTrack2->mTpcV0NegPosSample[ipt];
    ipt++;
    AveSep += diff.mag();
  }
  AveSep = AveSep/(ipt+1.);
  return (AveSep);}
  else return -1;
}

//_______1st part is a V0 considering Pos daughter 2nd is a V0 considering Pos daughter
double StHbtPair::TpcExitSeparationV0PosV0Pos() const {
  StHbtThreeVector diff = mTrack1->TpcV0PosExitPoint() - mTrack2->TpcV0PosExitPoint();
  return (diff.mag());
}

double StHbtPair::TpcEntranceSeparationV0PosV0Pos() const {
  StHbtThreeVector diff = mTrack1->TpcV0PosEntrancePoint() - mTrack2->TpcV0PosEntrancePoint();
  return (diff.mag());
}
double StHbtPair::TpcAverageSeparationV0PosV0Pos() const {
  StHbtThreeVector diff;
  double AveSep = 0.0;
  int ipt=0;
  if (mTrack1->mNominalPosSample && (mTrack2->mNominalPosSample)){
    while ((fabs(mTrack1->mNominalPosSample[ipt].x())<9999.) &&
	(fabs(mTrack1->mNominalPosSample[ipt].y())<9999.) &&
	(fabs(mTrack1->mNominalPosSample[ipt].z())<9999.) &&
	(fabs(mTrack2->mNominalPosSample[ipt].x())<9999.) &&
	(fabs(mTrack2->mNominalPosSample[ipt].y())<9999.) &&
	(fabs(mTrack2->mNominalPosSample[ipt].z())<9999.) &&
	 (ipt<11)  
	){
      diff = mTrack1->mNominalPosSample[ipt] - mTrack2->mNominalPosSample[ipt];
      ipt++;
      AveSep += diff.mag();
    }
    AveSep = AveSep/(ipt+1);
    return (AveSep);}
  else return -1;
}

//_______1st part is a V0 considering Pos daughter 2nd is a V0 considering Neg daughter
double StHbtPair::TpcExitSeparationV0PosV0Neg() const {
  StHbtThreeVector diff = mTrack1->TpcV0PosExitPoint() - mTrack2->TpcV0NegExitPoint();
  return (diff.mag());
}

double StHbtPair::TpcEntranceSeparationV0PosV0Neg() const {
  StHbtThreeVector diff = mTrack1->TpcV0PosEntrancePoint() - mTrack2->TpcV0NegEntrancePoint();
  return (diff.mag());
}
double StHbtPair::TpcAverageSeparationV0PosV0Neg() const {
  StHbtThreeVector diff;
  double AveSep = 0.0;
  int ipt = 0;
  if (mTrack1->mNominalPosSample && mTrack2->mTpcV0NegPosSample){
  while (fabs(mTrack1->mNominalPosSample[ipt].x())<9999. &&
	 fabs(mTrack1->mNominalPosSample[ipt].y())<9999. && 
	 fabs(mTrack1->mNominalPosSample[ipt].z())<9999. &&
	 fabs(mTrack2->mTpcV0NegPosSample[ipt].x())<9999. &&
	 fabs(mTrack2->mTpcV0NegPosSample[ipt].y())<9999. && 
	 fabs(mTrack2->mTpcV0NegPosSample[ipt].z())<9999. &&
	 (ipt<11)
	 ){
    diff = mTrack1->mNominalPosSample[ipt] - mTrack2->mTpcV0NegPosSample[ipt];
    ipt++;
    AveSep += diff.mag();
  }
  AveSep = AveSep/(ipt+1.);
  return (AveSep);}
  else return -1; 
}
//_______1st part is a V0 considering Neg daughter 2nd is a V0 considering Pos daughter
// this is to check the upper case
double StHbtPair::TpcExitSeparationV0NegV0Pos() const {
  StHbtThreeVector diff = mTrack1->TpcV0NegExitPoint() - mTrack2->TpcV0PosExitPoint();
  return (diff.mag());
}

double StHbtPair::TpcEntranceSeparationV0NegV0Pos() const {
  StHbtThreeVector diff = mTrack1->TpcV0NegEntrancePoint() - mTrack2->TpcV0PosEntrancePoint();
  return (diff.mag());
}
double StHbtPair::TpcAverageSeparationV0NegV0Pos() const {
   StHbtThreeVector diff;
   double AveSep = 0.0;
   int ipt = 0;
   if ( mTrack1->mTpcV0NegPosSample &&  mTrack2->mNominalPosSample){
     while (fabs(mTrack1->mTpcV0NegPosSample[ipt].x())<9999. &&
	    fabs(mTrack1->mTpcV0NegPosSample[ipt].y())<9999. && 
	    fabs(mTrack1->mTpcV0NegPosSample[ipt].z())<9999. &&
	    fabs(mTrack2->mNominalPosSample[ipt].x())<9999. &&
	    fabs(mTrack2->mNominalPosSample[ipt].y())<9999. && 
	    fabs(mTrack2->mNominalPosSample[ipt].z())<9999. &&
	    (ipt<11)
	    ){
       diff = mTrack1->mTpcV0NegPosSample[ipt] - mTrack2->mNominalPosSample[ipt];
       ipt++;
       AveSep += diff.mag();
     }
     AveSep = AveSep/(ipt+1);
     return (AveSep);}
     else return -1;
}
//_______1st part is a V0 considering Neg daughter 2nd is a V0 considering Neg daughter
double StHbtPair::TpcExitSeparationV0NegV0Neg() const {
  StHbtThreeVector diff = mTrack1->TpcV0NegExitPoint() - mTrack2->TpcV0NegExitPoint();
  return (diff.mag());
}

double StHbtPair::TpcEntranceSeparationV0NegV0Neg() const {
  StHbtThreeVector diff = mTrack1->TpcV0NegEntrancePoint() - mTrack2->TpcV0NegEntrancePoint();
  return (diff.mag());
}
double StHbtPair::TpcAverageSeparationV0NegV0Neg() const {
   StHbtThreeVector diff;
   double AveSep = 0.0;
   int ipt=0;
   if (mTrack1->mTpcV0NegPosSample && mTrack2->mTpcV0NegPosSample){
     while (fabs(mTrack1->mTpcV0NegPosSample[ipt].x())<9999. &&
	    fabs(mTrack1->mTpcV0NegPosSample[ipt].y())<9999. && 
	    fabs(mTrack1->mTpcV0NegPosSample[ipt].z())<9999. &&
	    fabs(mTrack2->mTpcV0NegPosSample[ipt].x())<9999. &&
	    fabs(mTrack2->mTpcV0NegPosSample[ipt].y())<9999. && 
	    fabs(mTrack2->mTpcV0NegPosSample[ipt].z())<9999. &&
	    (ipt<11)
	    ){
       diff = mTrack1->mTpcV0NegPosSample[ipt] - mTrack2->mTpcV0NegPosSample[ipt];
       ipt++;
       AveSep += diff.mag();
     }
     AveSep = AveSep/(ipt+1);
     return (AveSep);}
   else return -1;
}

//________________end V0 daughters exit/entrance/average separation calc.
void StHbtPair::CalcMergingParFctn(short* tmpMergingParNotCalculatedFctn,
				   float* tmpZ1,float* tmpU1,
				   float* tmpZ2,float* tmpU2,
				   int *tmpSect1,int *tmpSect2,
				   double* tmpFracOfMergedRow,
				   double* tmpClosestRowAtDCA
				   ) const{
  tmpMergingParNotCalculatedFctn=0;
  double tDu, tDz;
  int tN = 0;
  *tmpFracOfMergedRow = 0.;
  *tmpClosestRowAtDCA = 0.;
  double tDist;
  double tDistMax = 100000000.;
  for(int ti=0 ; ti<45 ; ti++){
    if(tmpSect1[ti]==tmpSect2[ti] && tmpSect1[ti]!=-1){
	tDu = fabs(tmpU1[ti]-tmpU2[ti]);
	tDz = fabs(tmpZ1[ti]-tmpZ2[ti]);
	tN++;
      if(ti<13){
	*tmpFracOfMergedRow += (tDu<mMaxDuInner && tDz<mMaxDzInner);
	tDist = ::sqrt(tDu*tDu/mMaxDuInner/mMaxDuInner+
		     tDz*tDz/mMaxDzInner/mMaxDzInner);
      }
      else{
	*tmpFracOfMergedRow += (tDu<mMaxDuOuter && tDz<mMaxDzOuter);
	tDist = ::sqrt(tDu*tDu/mMaxDuOuter/mMaxDuOuter+
		     tDz*tDz/mMaxDzOuter/mMaxDzOuter);
	}
      if(tDist<tDistMax){
	mClosestRowAtDCA = ti+1;
	tDistMax = tDist;
      }
      //mWeightedAvSep += tDist; // now, wrong but not used
    }	
  }
  if(tN>0){
    //mWeightedAvSep /= tN;
    *tmpFracOfMergedRow /= tN;
  }
  else{
    *tmpClosestRowAtDCA = -1;
    *tmpFracOfMergedRow = -1.;
    //mWeightedAvSep = -1.;
  }
}

