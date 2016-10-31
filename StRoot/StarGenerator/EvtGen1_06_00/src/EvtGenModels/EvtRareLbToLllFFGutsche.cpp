// Include files 



// local

#include "EvtGenModels/EvtRareLbToLllFFGutsche.hh"
#include "EvtGenBase/EvtVector4R.hh" 
#include "EvtGenBase/EvtPDL.hh"
#include "EvtGenBase/EvtIdSet.hh"


//-----------------------------------------------------------------------------
// Implementation file for class : EvtRareLbToLllFFGutsche
//
// 2014-10-21 : Michal Kreps
//-----------------------------------------------------------------------------

//=============================================================================
// Standard constructor, initializes variables
//=============================================================================


EvtIdSet EvtRareLbToLllFFGutsche::fParents("Lambda_b0", "anti-Lambda_b0");
EvtIdSet EvtRareLbToLllFFGutsche::fDaughters("Lambda0", "anti-Lambda0");

EvtRareLbToLllFFGutsche::EvtRareLbToLllFFGutsche() : EvtRareLbToLllFFBase()  
  {}


void EvtRareLbToLllFFGutsche::init()
{
  fVconsts[0][0] = 0.107;
  fVconsts[0][1] = 2.27;
  fVconsts[0][2] = 1.367;
  fVconsts[1][0] = 0.043;
  fVconsts[1][1] = 2.411;
  fVconsts[1][2] = 1.531;
  fVconsts[2][0] = -0.003;
  fVconsts[2][1] = 2.815;
  fVconsts[2][2] = 2.041;

  fAconsts[0][0] = 0.104;
  fAconsts[0][1] = 2.232;
  fAconsts[0][2] = 1.328;
  fAconsts[1][0] = -0.003;
  fAconsts[1][1] = 2.955;
  fAconsts[1][2] = 3.620;
  fAconsts[2][0] = -0.052;
  fAconsts[2][1] = 2.437;
  fAconsts[2][2] = 1.559;

  fTVconsts[0][0] = -0.043;
  fTVconsts[0][1] = 2.411;
  fTVconsts[0][2] = 1.531;
  fTVconsts[1][0] = -0.105;
  fTVconsts[1][1] = 2.27118;
  fTVconsts[1][2] = 1.36776;
  fTVconsts[2][0] = 0; // Not used anywhere
  fTVconsts[2][1] = 0;
  fTVconsts[2][2] = 0;

  fTAconsts[0][0] = 0.003;
  fTAconsts[0][1] = 2.955;
  fTAconsts[0][2] = 3.620;
  fTAconsts[1][0] = -0.105;
  fTAconsts[1][1] = 2.233;
  fTAconsts[1][2] = 1.328;
  fTAconsts[2][0] = 0; // Not used anywhere
  fTAconsts[2][1] = 0;
  fTAconsts[2][2] = 0;

  EvtGenReport(EVTGEN_INFO,"EvtGen") << " EvtRareLbToLll is using form factors from arXiv:1301.3737 " << std::endl;
}

//=============================================================================
// Destructor
//=============================================================================

EvtRareLbToLllFFGutsche::~EvtRareLbToLllFFGutsche() {
  
}

//=============================================================================


void EvtRareLbToLllFFGutsche::getFF( EvtParticle* parent, 
                             EvtParticle* lambda,
                             EvtRareLbToLllFFBase::FormFactors& FF )
{
  // Find the FF information for this particle, start by setting all to zero
  FF.areZero();
  
/*
  if ( ! ( fParents.contains(parent->getId()) && 
           fDaughters.contains(lambda->getId()) ) )
  {
    EvtGenReport(EVTGEN_ERROR,"EvtGen") << " EvtRareLbToLllFFGutsche: Unknown mother and/or daughter. " << std::endl;
    return; 
  }
*/

  double m1 = parent->getP4().mass();
  double m2 = lambda->getP4().mass();
  EvtVector4R p4parent;
  p4parent.set( parent->mass(), 0 , 0 , 0 );
  double q2 = ( p4parent - lambda->getP4() ).mass2();
  double m21 = m2/m1;
  double shat = q2/m1/m1;

  double fV[3];
  double fA[3];
  for (int i=0;i<=2;++i) {
    fV[i] = formFactorParametrization(shat, fVconsts[i][0], fVconsts[i][1],
                                      fVconsts[i][2]);
    fA[i] = formFactorParametrization(shat, fAconsts[i][0], fAconsts[i][1],
                                      fAconsts[i][2]);
  }
  double fTV[2];
  double fTA[2];
  for (int i=0;i<=1;++i) {
    fTV[i] = formFactorParametrization(shat, fTVconsts[i][0], fTVconsts[i][1],
                                      fTVconsts[i][2]);
    fTA[i] = formFactorParametrization(shat, fTAconsts[i][0], fTAconsts[i][1],
                                      fTAconsts[i][2]);
  }

  // Both v^2==v'^2==1 by definition
  FF.F_[0] = fV[0] + fV[1]*( 1 + m21 );
  FF.F_[1] = fV[2] - fV[1];
  FF.F_[2] = -m21*( fV[1] + fV[2] );

  FF.G_[0] = fA[0] - fA[1]*( 1 - m21 );
  FF.G_[1] = fA[2] - fA[1];
  FF.G_[2] = -m21*( + fA[1] + fA[2] );


  FF.FT_[0] = fTV[1]*( m1 + m2) + fTV[0]*( q2/m1 );
  FF.FT_[1] = +fTV[0] * ( m2 - m1 )  - fTV[1] * m1;
  FF.FT_[2] = m2 * ( fTV[0] - fTV[1] ) - fTV[0] * m21 * m2 ;

  FF.GT_[0] = -fTA[1]*( m1 - m2) + fTA[0]*( q2/m1 );
  FF.GT_[1] = -fTA[1] * m1 + fTA[0] * ( m1 + m2 );
  FF.GT_[2] = -fTA[0] * m2 * m21 - m2 * ( fTA[0] + fTA[1] );

  return ;
}

double EvtRareLbToLllFFGutsche::formFactorParametrization(double s, double f0, 
                                     double a, double b) {
  return f0 / (1 - a * s + b * s * s);
}


