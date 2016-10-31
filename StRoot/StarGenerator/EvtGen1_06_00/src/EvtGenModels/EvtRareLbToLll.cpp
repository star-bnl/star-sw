//--------------------------------------------------------------------------
//
// Environment:
//      This software is part of the EvtGen package developed jointly
//      for the BaBar and CLEO collaborations.  If you use all or part
//      of it, please give an appropriate acknowledgement.
//
// Copyright Information: See EvtGen/COPYRIGHT
//      Copyright (C) 2003      Caltech, UCSB
//
// Module: EvtRareLbToLll
//
// Description:  
//      Implements the rare Lb --> Lambda^(*) ell ell models described in
//      http://arxiv.org/pdf/1108.6129.pdf
//
// Modification history:
//
//    T. Blake       November 2013         Created module
//
//------------------------------------------------------------------------
// 


 
#include <stdlib.h>
#include "EvtGenModels/EvtRareLbToLll.hh"
#include "EvtGenModels/EvtRareLbToLllFF.hh"
#include "EvtGenModels/EvtRareLbToLllFFGutsche.hh"
#include "EvtGenModels/EvtRareLbToLllFFlQCD.hh"


#include "EvtGenBase/EvtComplex.hh"
#include "EvtGenBase/EvtVector4C.hh"
#include "EvtGenBase/EvtVector4R.hh"
#include "EvtGenBase/EvtTensor4C.hh"
#include "EvtGenBase/EvtDiracSpinor.hh"
#include "EvtGenBase/EvtDiracParticle.hh"
#include "EvtGenBase/EvtRaritaSchwinger.hh"
#include "EvtGenBase/EvtSpinDensity.hh"
#include "EvtGenBase/EvtPDL.hh"


EvtRareLbToLll::EvtRareLbToLll() : m_maxProbability( 0 ), ffmodel_( 0 ), wcmodel_( 0 ) {} 

EvtRareLbToLll::~EvtRareLbToLll() {
  if ( wcmodel_ ) delete wcmodel_;
  if ( ffmodel_ ) delete ffmodel_;
}


// The module name specification
std::string EvtRareLbToLll::getName( ) {
  return "RareLbToLll" ;
}


// The implementation of the clone() method
EvtDecayBase* EvtRareLbToLll::clone(){
  return new EvtRareLbToLll;
}

void EvtRareLbToLll::init(){
  checkNArg(1);
  
  // check that there are 3 daughteres
  checkNDaug(3);

  // Parent should be spin 1/2 Lambda_b0
  const EvtSpinType::spintype spin = EvtPDL::getSpinType(getDaug(0));
  
  if ( !( spin == EvtSpinType::DIRAC || spin == EvtSpinType::RARITASCHWINGER ) )
  {
    EvtGenReport(EVTGEN_ERROR,"EvtGen") << " EvtRareLbToLll expects DIRAC or RARITASWINGER daughter " << std::endl;
  }
  
  // We expect that the second and third daughters 
  // are the ell+ and ell- 
  checkSpinDaughter(1,EvtSpinType::DIRAC);
  checkSpinDaughter(2,EvtSpinType::DIRAC);

  std::string model = getArgStr(0);
  if ( model == "Gutsche" ) {
    ffmodel_ = new EvtRareLbToLllFFGutsche();
  } else if ( model == "LQCD" ) {
    ffmodel_ = new EvtRareLbToLllFFlQCD();
  } else if ( model == "MR" ) {
    ffmodel_ = new EvtRareLbToLllFF();
  }
  else {
    EvtGenReport(EVTGEN_ERROR ,"EvtGen") << "  Unknown form-factor model, valid options are MR, LQCD, Gutsche." << std::endl;
    ::abort();
  }
  wcmodel_  = new EvtRareLbToLllWC();

  ffmodel_->init();
  
  return;
}

void EvtRareLbToLll::initProbMax(){

  EvtGenReport(EVTGEN_INFO,"EvtGen") << " EvtRareLbToLll is finding maximum probability ... " << std::endl;

  m_maxProbability=0;

  if(m_maxProbability==0){

    EvtDiracParticle *parent = new EvtDiracParticle;
    parent->noLifeTime();
    parent->init(getParentId(),EvtVector4R(EvtPDL::getMass(getParentId()),0,0,0));
    parent->setDiagonalSpinDensity();

    EvtAmp amp;
    EvtId daughters[3] = {getDaug(0),getDaug(1),getDaug(2)};
    amp.init(getParentId(),3,daughters);
    parent->makeDaughters(3,daughters);
    EvtParticle *lambda = parent->getDaug(0);
    EvtParticle *lep1   = parent->getDaug(1);
    EvtParticle *lep2   = parent->getDaug(2);
    lambda -> noLifeTime();
    lep1   -> noLifeTime();
    lep2   -> noLifeTime();

    EvtSpinDensity rho;
    rho.setDiag(parent->getSpinStates());

    double M0 = EvtPDL::getMass(getParentId());
    double mL = EvtPDL::getMass(getDaug(0));
    double m1 = EvtPDL::getMass(getDaug(1));
    double m2 = EvtPDL::getMass(getDaug(2));

    double q2,pstar,elambda,theta;
    double q2min = (m1+m2)*(m1+m2);
    double q2max = (M0-mL)*(M0-mL);

    EvtVector4R p4lambda,p4lep1,p4lep2,boost;

    EvtGenReport(EVTGEN_INFO,"EvtGen") << " EvtRareLbToLll is probing whole phase space ..." << std::endl;

    int i,j;
    double prob=0;
    for(i=0;i<=100;i++){
      q2 = q2min+i*(q2max-q2min)/100.;
      elambda = (M0*M0+mL*mL-q2)/2/M0;
      if(i==0) pstar = 0;
      else     pstar = sqrt(q2-(m1+m2)*(m1+m2))*sqrt(q2-(m1-m2)*(m1-m2))/2/sqrt(q2);
      boost.set(M0-elambda,0,0,+sqrt(elambda*elambda-mL*mL));
      if ( i != 100 ) {
        p4lambda.set(elambda,0,0,-sqrt(elambda*elambda-mL*mL));
      } else {
        p4lambda.set(mL,0,0,0); 
      }
      for(j=0;j<=45;j++){
        theta = j*EvtConst::pi/45;
        p4lep1.set(sqrt(pstar*pstar+m1*m1),0,+pstar*sin(theta),+pstar*cos(theta));
        p4lep2.set(sqrt(pstar*pstar+m2*m2),0,-pstar*sin(theta),-pstar*cos(theta));
	//std::cout << "p1: " << p4lep1 << " p2: " << p4lep2 << " pstar: " << pstar << std::endl;
        if ( i != 100 ) // At maximal q2 we are already in correct frame as Lambda and W/Zvirtual are at rest
        {
          p4lep1 = boostTo(p4lep1,boost);
          p4lep2 = boostTo(p4lep2,boost);
        }
        lambda -> init(getDaug(0),p4lambda);
        lep1   -> init(getDaug(1),p4lep1  );
        lep2   -> init(getDaug(2),p4lep2  );
        calcAmp(amp,parent);
        prob = rho.normalizedProb(amp.getSpinDensity());
        //std::cout << "q2:  " << q2 << " \t theta:  " << theta << " \t prob:  " << prob << std::endl;
        //std::cout << "p1: " << p4lep1 << " p2: " << p4lep2 << " q2-q2min: " << q2-(m1+m2)*(m1+m2) << std::endl;
        if(prob>m_maxProbability){
          EvtGenReport(EVTGEN_INFO,"EvtGen") << "  - probability " << prob 
                                << " found at q2 = " << q2 << " (" << 100*(q2-q2min)/(q2max-q2min) 
                                << " %) and theta = " << theta*180/EvtConst::pi << std::endl;
          m_maxProbability=prob;
        }
      }
      //::abort();
    }

    //m_poleSize = 0.04*q2min;
    m_maxProbability *= 1.2;
    delete parent;
  }

  setProbMax(m_maxProbability);
  EvtGenReport(EVTGEN_INFO,"EvtGen") << " EvtRareLbToLll set up maximum probability to " << m_maxProbability << std::endl;

}


void EvtRareLbToLll::decay( EvtParticle *parent ){
  parent->initializePhaseSpace( getNDaug(),getDaugs() );

  calcAmp( _amp2,parent );
}

bool EvtRareLbToLll::isParticle( EvtParticle *parent ) const 
{
  static EvtIdSet partlist("Lambda_b0");
  
  return partlist.contains( parent->getId() );
}


void  EvtRareLbToLll::calcAmp( EvtAmp& amp, EvtParticle *parent ){
  //parent->setDiagonalSpinDensity();

  EvtParticle* lambda = parent->getDaug(0);

  static EvtIdSet leptons("e-","mu-","tau-");
  
  const bool isparticle = isParticle( parent );
  
  EvtParticle* lp = 0;
  EvtParticle* lm = 0;
  
  if ( leptons.contains(parent->getDaug(1)->getId()) )
  {
    lp = parent->getDaug(1);
    lm = parent->getDaug(2);
  }
  else 
  {
    lp = parent->getDaug(2);
    lm = parent->getDaug(1);
  }  
    
  EvtVector4R P;
  P.set(parent->mass(), 0.0,0.0,0.0);  
  
  EvtVector4R q = lp->getP4() + lm->getP4();
  double qsq    = q.mass2();
  
  // Leptonic currents
  EvtVector4C LV[2][2]; // \bar{\ell} \gamma^{\mu} \ell 
  EvtVector4C LA[2][2]; // \bar{\ell} \gamma^{\mu} \gamma^{5} \ell

  for ( int i =0 ; i < 2; ++i ){
    for ( int j = 0; j < 2; ++j ){
      if ( isparticle )
      {
        LV[i][j] = EvtLeptonVCurrent( lp->spParent(i), lm->spParent(j) );
        LA[i][j] = EvtLeptonACurrent( lp->spParent(i), lm->spParent(j) );
      }
      else 
      {
        LV[i][j] = EvtLeptonVCurrent( lp->spParent(1-i), lm->spParent(1-j) );
        LA[i][j] = EvtLeptonACurrent( lp->spParent(1-i), lm->spParent(1-j) );
      }
    }
  }
  
  EvtRareLbToLllFF::FormFactors FF;
  //F, G, FT and GT
  ffmodel_->getFF( parent, lambda, FF );
  
  EvtComplex  C7eff  = wcmodel_-> GetC7Eff(qsq);
  EvtComplex  C9eff  = wcmodel_-> GetC9Eff(qsq);
  EvtComplex  C10eff = wcmodel_->GetC10Eff(qsq);
    
  EvtComplex AC[4];
  EvtComplex BC[4];
  EvtComplex DC[4];
  EvtComplex EC[4];

  // check to see if particle is same or opposite parity to Lb
  const int parity = ffmodel_->isNatural( lambda ) ? 1 : -1 ;

  // Lambda spin type
  const  EvtSpinType::spintype spin  = EvtPDL::getSpinType(lambda->getId());
  
  static const double mb = 5.209;
  
  // Eq. 48 + 49
  for ( unsigned int i = 0; i < 4; ++i ) {
    if ( parity > 0 )
    {
      AC[i] = -2.*mb*C7eff*FF.FT_[i]/qsq + parity*C9eff*FF.F_[i];
      BC[i] = -2.*mb*C7eff*FF.GT_[i]/qsq - parity*C9eff*FF.G_[i];
      DC[i] =  C10eff*FF.F_[i];
      EC[i] = -C10eff*FF.G_[i];
    }
    else 
    {
      AC[i] = -2.*mb*C7eff*FF.GT_[i]/qsq - C9eff*FF.G_[i];
      BC[i] = -2.*mb*C7eff*FF.FT_[i]/qsq + C9eff*FF.F_[i];
      DC[i] = -C10eff*FF.G_[i];
      EC[i] =  C10eff*FF.F_[i];
    } 
  }
  

  // handle particle -> antiparticle in Hadronic currents
  const double cv = ( isparticle > 0 ) ? 1.0 : -1.0*parity;
  const double ca = ( isparticle > 0 ) ? 1.0 :  1.0*parity;
  const double cs = ( isparticle > 0 ) ? 1.0 :  1.0*parity;
  const double cp = ( isparticle > 0 ) ? 1.0 : -1.0*parity;
  
 
  if (EvtSpinType::DIRAC == spin ){
    EvtVector4C H1[2][2]; // vector current
    EvtVector4C H2[2][2]; // axial-vector
      
    EvtVector4C T[6];
    // Hadronic currents
    for ( int i =0 ; i < 2; ++i ){
       for ( int j = 0; j < 2; ++j ){
         HadronicAmp( parent, lambda, T, i, j );
         
         H1[i][j] = ( cv*AC[0]*T[0] + ca*BC[0]*T[1] + cs*AC[1]*T[2] + 
                      cp*BC[1]*T[3] + cs*AC[2]*T[4] + cp*BC[2]*T[5] );
         
         H2[i][j] = ( cv*DC[0]*T[0] + ca*EC[0]*T[1] + cs*DC[1]*T[2] + 
                      cp*EC[1]*T[3] + cs*DC[2]*T[4] + cp*EC[2]*T[5] );
       }
    }

    // Spin sums
    int spins[4];

    for ( int i =0; i < 2; ++i ) {
      for ( int ip = 0; ip < 2; ++ip ) {
        for ( int j = 0; j < 2; ++j ) {
          for ( int jp = 0; jp < 2; ++jp ) {
            spins[0] = i;
            spins[1] = ip;
            spins[2] = j;
            spins[3] = jp;
          
            amp.vertex( spins, H1[i][ip]*LV[j][jp] + H2[i][ip]*LA[j][jp] );
          }
        }  
      }
    }
  }
  else if ( EvtSpinType::RARITASCHWINGER == spin )
  {
    EvtVector4C T[8];
    
    EvtVector4C H1[4][2]; // vector current
    EvtVector4C H2[4][2]; // axial-vector
    
    // Build hadronic amplitude
     for ( int i =0 ; i < 4; ++i ){
       for ( int j = 0; j < 2; ++j ){
         HadronicAmpRS( parent, lambda, T, i, j );
         
         H1[i][j] = ( cv*AC[0]*T[0] + ca*BC[0]*T[1] + 
                      cs*AC[1]*T[2] + cp*BC[1]*T[3] +
                      cs*AC[2]*T[4] + cp*BC[2]*T[5] + 
                      cs*AC[3]*T[6] + cp*BC[3]*T[7] );
         H2[i][j] = ( cv*DC[0]*T[0] + ca*EC[0]*T[1] + 
                      cs*DC[1]*T[2] + cp*EC[1]*T[3] +
                      cs*DC[2]*T[4] + cp*EC[2]*T[5] + 
                      cs*DC[3]*T[6] + cp*EC[3]*T[7] );
       }
    }
     
     // Spin sums
     int spins[4];
     
     for ( int i =0; i < 4; ++i ) {
       for ( int ip = 0; ip < 2; ++ip ) {
         for ( int j = 0; j < 2; ++j ) {
           for ( int jp = 0; jp < 2; ++jp ) {
             spins[0] = i;
             spins[1] = ip;
             spins[2] = j;
             spins[3] = jp;
             
             amp.vertex( spins, H1[i][ip]*LV[j][jp] + H2[i][ip]*LA[j][jp] );
           }
         }  
       }
     }
  }
  else 
  {
    EvtGenReport(EVTGEN_ERROR,"EvtGen" ) << " EvtRareLbToLll expects DIRAC or RARITASWINGER daughter " << std::endl;
  }
  
  return;
}


// spin 1/2 daughters

void EvtRareLbToLll::HadronicAmp( EvtParticle* parent, 
                                 EvtParticle* lambda, 
                                 EvtVector4C* T,
                                 const int i, 
                                 const int j )
{
  
  const EvtDiracSpinor Sfinal = lambda->spParent(i);
  const EvtDiracSpinor Sinit  = parent->sp(j); 
  
  const EvtVector4R L = lambda->getP4();
  
  EvtVector4R P;
  P.set(parent->mass(), 0.0,0.0,0.0);
  
  const double Pm = parent->mass();
  const double Lm = lambda->mass();
  
  // \bar{u} \gamma^{\mu} u
  T[0] = EvtLeptonVCurrent( Sfinal, Sinit );

  // \bar{u} \gamma^{\mu}\gamma^{5} u
  T[1] = EvtLeptonACurrent( Sfinal, Sinit );
  
  // \bar{u} v^{\mu} u
  T[2] = EvtLeptonSCurrent( Sfinal, Sinit )*( P/Pm );
  
  // \bar{u} v^{\mu} \gamma^{5} u
  T[3] = EvtLeptonPCurrent( Sfinal, Sinit )*( P/Pm );
  
  // \bar{u} v^{\prime\mu} u
  T[4] = EvtLeptonSCurrent( Sfinal, Sinit )*( L/Lm );
  
  // \bar{u} v^{\prime\mu} \gamma^{5} 
  T[5] = EvtLeptonPCurrent( Sfinal, Sinit )*( L/Lm );
  
  // v = p_{\Lambda_b}/m_{\Lambda_b} 
  // v^{\prime} =  p_{\Lambda}/m_{\Lambda} 
  
  return;
}


// spin 3/2 daughters

void EvtRareLbToLll::HadronicAmpRS( EvtParticle* parent, 
                                   EvtParticle* lambda, 
                                   EvtVector4C* T,
                                   const int i, 
                                   const int j )
{
  const EvtRaritaSchwinger Sfinal = lambda->spRSParent(i);
  const EvtDiracSpinor     Sinit  = parent->sp(j);
  
  EvtVector4R P;
  P.set(parent->mass(), 0.0,0.0,0.0);
  
  const EvtVector4R L = lambda->getP4();
  
  EvtTensor4C ID;
  ID.setdiag(1.0,1.0,1.0,1.0);

  EvtDiracSpinor Sprime;
  
  for(int i=0 ; i<4 ; i++ ){
    Sprime.set_spinor(i,Sfinal.getVector(i)*P);
  }

  const double Pmsq = P.mass2();
  const double Pm   = parent->mass();
  const double PmLm = Pm*lambda->mass();
  
  
  EvtVector4C V1,V2;
  
  for(int i=0;i<4;i++){
    V1.set(i,EvtLeptonSCurrent(Sfinal.getSpinor(i),Sinit));
    V2.set(i,EvtLeptonPCurrent(Sfinal.getSpinor(i),Sinit));
  }

  // \bar{u}_{alpha} v^{\alpha} \gamma^{\mu} u
  T[0] = EvtLeptonVCurrent(Sprime, Sinit)*(1/Pm); 

  // \bar{u}_{alpha}  v^{\alpha} \gamma^{\mu} \gamma^{5} u 
  T[1] = EvtLeptonACurrent(Sprime, Sinit)*(1/Pm);

  // \bar{u}_{\alpha} v^{\alpha} v^{\mu} u
  T[2] = EvtLeptonSCurrent(Sprime, Sinit)*(P/Pmsq);

  // \bar{u}_{\alpha} v^{\alpha} v^{\mu} \gamma^{5} u
  T[3] = EvtLeptonPCurrent(Sprime, Sinit)*(P/Pmsq);
  
  // \bar{u}_{\alpha} v^{\alpha} v^{\prime \mu} u
  T[4] = EvtLeptonSCurrent(Sprime, Sinit)*(L/PmLm);
  
  // \bar{u}_{\alpha} v^{\alpha} v^{\prime \mu} \gamma^{5} u
  T[5] = EvtLeptonPCurrent(Sprime, Sinit)*(L/PmLm);
  
  // \bar{u}_{\alpha} g^{\alpha\mu} u
  T[6] = ID.cont2(V1);

  // \bar{u}_{\alpha} g^{\alpha\mu} \gamma^{5} u
  T[7] = ID.cont2(V2);
  
  // Where:
  //  v = p_{\Lambda_b}/m_{\Lambda_b} 
  //  v^{\prime} =  p_{\Lambda}/m_{\Lambda} 
  
  return;
}
