// Include files 



// local

#include "EvtGenModels/EvtRareLbToLllFFlQCD.hh"
#include "EvtGenModels/EvtWilsonCoefficients.hh"
#include "EvtGenBase/EvtVector4R.hh" 
#include "EvtGenBase/EvtPDL.hh"
#include "EvtGenBase/EvtIdSet.hh"
#include "EvtGenBase/EvtConst.hh"
#include <stdlib.h>
#include <cmath>

//-----------------------------------------------------------------------------
// Implementation file for class : EvtRareLbToLllFFlQCD
//
// 2016-04-19 : Michal Kreps
// 2014-10-22 : Michal Kreps
//-----------------------------------------------------------------------------

//=============================================================================
// Standard constructor, initializes variables
//=============================================================================


EvtRareLbToLllFFlQCD::EvtRareLbToLllFFlQCD() : EvtRareLbToLllFFBase()  
  {}


void EvtRareLbToLllFFlQCD::init()
{
  EvtId LbID = EvtPDL::getId(std::string("Lambda_b0"));
  EvtId LID = EvtPDL::getId(std::string("Lambda0"));
  EvtId BID = EvtPDL::getId(std::string("B+"));
  EvtId KID = EvtPDL::getId(std::string("K-"));
  double m1 = EvtPDL::getMass(LbID);
  double m2 = EvtPDL::getMass(LID);
  double mB = EvtPDL::getMass(BID);
  double mK = EvtPDL::getMass(KID);
  t0 = (m1 - m2)*(m1 - m2);
  tplus = (mB + mK)*(mB + mK);

  fconsts[0][0] = 0.4221;
  fconsts[0][1] = -1.1386;
  fconsts[0][2] = 5.416;
  fconsts[1][0] = 0.5182;
  fconsts[1][1] = -1.3495;
  fconsts[1][2] = 5.416;
  fconsts[2][0] = 0.3725;
  fconsts[2][1] = -0.9389;
  fconsts[2][2] = 5.711;

  gconsts[0][0] = 0.3563;
  gconsts[0][1] = -1.0612;
  gconsts[0][2] = 5.750;
  gconsts[1][0] = 0.3563;
  gconsts[1][1] = -1.1357;
  gconsts[1][2] = 5.750;
  gconsts[2][0] = 0.4028;
  gconsts[2][1] = -1.0290;
  gconsts[2][2] = 5.367;

  hconsts[0][0] = 0.4960;
  hconsts[0][1] = -1.1275;
  hconsts[0][2] = 5.416;
  hconsts[1][0] = 0.3876;
  hconsts[1][1] = -0.9623;
  hconsts[1][2] = 5.416;
  hconsts[2][0] = 0;
  hconsts[2][1] = 0;
  hconsts[2][2] = 0;

  htildaconsts[0][0] = 0.3403;
  htildaconsts[0][1] = -0.7697;
  htildaconsts[0][2] = 5.750;
  htildaconsts[1][0] = 0.3403;
  htildaconsts[1][1] = -0.8008;
  htildaconsts[1][2] = 5.750;
  htildaconsts[2][0] = 0;
  htildaconsts[2][1] = 0;
  htildaconsts[2][2] = 0;

  EvtGenReport(EVTGEN_INFO,"EvtGen") << " EvtRareLbToLll is using form factors from arXiv:1602.01399 " << std::endl;
}

//=============================================================================
// Destructor
//=============================================================================

EvtRareLbToLllFFlQCD::~EvtRareLbToLllFFlQCD() {
  
}

//=============================================================================


void EvtRareLbToLllFFlQCD::getFF( EvtParticle* parent, 
                             EvtParticle* lambda,
                             EvtRareLbToLllFFBase::FormFactors& FF )
{
  // Find the FF information for this particle, start by setting all to zero
  FF.areZero();
  
  double m1 = parent->getP4().mass();
  double m2 = lambda->getP4().mass();
//  double m21=m2/m1;
  EvtVector4R p4parent;
  p4parent.set( parent->mass(), 0 , 0 , 0 );
  double q2 = ( p4parent - lambda->getP4() ).mass2();

  double massSum = m1 + m2;
  double massDiff = m1 - m2;
  double massSumSq = massSum * massSum;
  double massDiffSq = massDiff * massDiff;
  double q2Sum = q2 - massSumSq;
  double q2Diff = q2 - massDiffSq;
 
  double f[3];
  double g[3];
  double h[2];
  double htilda[2];

  for (int i=0;i<=2;++i) {
    f[i] = formFactorParametrization(q2, fconsts[i][0], fconsts[i][1], fconsts[i][2]);
    g[i] = formFactorParametrization(q2, gconsts[i][0], gconsts[i][1], gconsts[i][2]);
  }
  for (int i=0;i<=1;++i) {
    h[i] = formFactorParametrization(q2, hconsts[i][0], hconsts[i][1], hconsts[i][2]);
    htilda[i] = formFactorParametrization(q2, htildaconsts[i][0], htildaconsts[i][1], 
                                          htildaconsts[i][2]);
  }

  // Both v^2==v'^2==1 by definition
  FF.F_[0] = f[1];
  FF.F_[1] = m1*((f[1]-f[0])*massSum + massDiff*(q2*(f[2]-f[1])-(f[2]-f[0])*massSumSq)/q2)/
             q2Sum;
  FF.F_[2] = -m2* (massSum*(f[0]-f[1]) + massDiff*(q2*(f[2]-f[1])-massSumSq*(f[2]-f[0]))/q2) / q2Sum;

  FF.G_[0] = g[1];
  FF.G_[1] = m1/q2Diff*( massDiff*(g[0]-g[1]) + massSum*(q2*(g[1]-g[2])+massDiffSq*(g[2]-g[0]) )/q2 ) ;
  FF.G_[2] = -m2/q2Diff * ( massDiff*(g[1]-g[0]) + massSum*(q2*(g[1]-g[2])+massDiffSq*(g[2]-g[0]) )/q2 ) ;


  FF.FT_[0] = -massSum*h[1];

  FF.FT_[1] = -m1/q2Sum*( 2*h[1]*m2*massSum-h[0]*(q2-massSum*massDiff));
  FF.FT_[2] = -m2/q2Sum*( 2*h[1]*m1*massSum-h[0]*(q2+massSum*massDiff));

  FF.GT_[0] = massDiff*htilda[1]; 

  FF.GT_[1] = m1/q2Diff*(2*htilda[1]*massDiff*m2+htilda[0]*(q2-massSum*massDiff)); 
  FF.GT_[2] = m2/q2Diff*(-2*htilda[1]*massDiff*m1+htilda[0]*(q2+massSum*massDiff));

  return ;
}

double EvtRareLbToLllFFlQCD::formFactorParametrization(double q2, double a0, 
                                                       double a1, double pole){ 

  double z = zvar(q2);
  return 1./(1.-q2/(pole * pole))*(a0 + a1 * z);
}

double EvtRareLbToLllFFlQCD::zvar(double q2) {

  double a = std::sqrt(tplus-q2);
  double b = std::sqrt(tplus-t0);

  return (a-b)/(a+b);
}

