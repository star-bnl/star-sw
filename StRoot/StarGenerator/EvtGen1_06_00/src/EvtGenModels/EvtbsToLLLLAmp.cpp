//--------------------------------------------------------------------------
//
// Environment:
//      This software is part of the EvtGen package developed jointly
//      for the BaBar and CLEO collaborations. If you use all or part
//      of it, please give an appropriate acknowledgement.
//
// Copyright Information: See EvtGen/COPYRIGHT
//      Copyright (C) 2000      Caltech, UCSB
//
// Module: EvtbsToLLLLAmp.cpp
//
// Description: Preparation of the decay amplitude for the process:
//              B^0_{q}(p,M1) -> ell^+(k1,m) ell^-(k2,m) ell^+(k3,m) ell^-(k4,m).
//
// Note: The code of this module is based on the EvtbTosllVectorAmp.cpp
//	 module code.
//	 The main functiom for the amplitude calculation retuns the
//	 amplitude for the decay  B^0_q -> ell^+ ell^- ell^+ ell^-
//
//
// Modification history:
//
//  N.Nikitin (nnikit@mail.cerrn.ch)   July      21, 2011     Module created
//
//-----------------------------------------------------------------------------------------
//
#include "EvtGenBase/EvtPatches.hh"
#include "EvtGenBase/EvtGenKine.hh"
#include "EvtGenBase/EvtPDL.hh"
#include "EvtGenBase/EvtReport.hh"
#include "EvtGenBase/EvtComplex.hh"
#include "EvtGenBase/EvtVector4C.hh"
#include "EvtGenBase/EvtTensor4C.hh"
#include "EvtGenBase/EvtParticle.hh"
#include "EvtGenBase/EvtScalarParticle.hh"
#include "EvtGenBase/EvtDiracSpinor.hh"
#include "EvtGenBase/EvtId.hh"
#include "EvtGenBase/EvtIdSet.hh"
#include "EvtGenBase/EvtAmp.hh"
#include "EvtGenModels/EvtbTosllWilsCoeffNLO.hh"
#include "EvtGenModels/Evtbs2llGammaFFMNT.hh"
// The header file for current class memeber functions description
#include "EvtGenModels/EvtbsToLLLLAmp.hh" 
#include <cstdlib>

// input:   *parent      - the pointer to the parent particle (B-meson, the 
//                                          object of the EvtParticle class);
//          *formFactors - the pointer to instance of EvtbTosllGammaFF class object;
//          *WilsCoeff   - the pointer to the Standart Model Wilson Coefficients class;
//           mu          - the scale parameter, GeV;
//           Nf          - number of "effective" flavors (for b-quark Nf=5);
//           res_swch    - resonant switching parameter:
//                         = 0 the resonant contribution switched OFF, 
//                         = 1 the resonant contribution switched ON;
//           ias         - switching parameter for \alpha_s(M_Z) value:
//                         = 0 PDG 1sigma minimal alpha_s(M_Z), 
//                         = 1 PDG average value  alpha_s(M_Z), 
//                         = 2 PDG 1sigma maximal alpha_s(M_Z).
//           Wolfenstein parameterization for CKM matrix
//                         CKM_A, CKM_lambda, CKM_barrho, CKM_bareta

void EvtbsToLLLLAmp::CalcAmp(EvtParticle *parent, EvtAmp& amp,
			    Evtbs2llGammaFF *formFactors, EvtbTosllWilsCoeffNLO *WilsCoeff, 
                            double mu, int Nf, int res_swch, int ias,
                            double CKM_A, double CKM_lambda, 
                            double CKM_barrho, double CKM_bareta){

//  FILE *mytest;

  int il1=0, il2=1, il3=2, il4=3;  // leptons are the first, second, thirds 
                                   //                      and fourth daughter particles 

  EvtComplex unit1(1.0,0.0); // real unit
  EvtComplex uniti(0.0,1.0); // imaginary unit

  double M1 = parent->mass();                // B - meson mass, GeV
  double ml = parent->getDaug(il1)->mass();  // leptonic mass, GeV
  double mq = 0.0;                           // light quark mass from the dispersion QM, GeV
  double mc = formFactors->getQuarkMass(4);  // m_c mass from the dispersion QM, GeV  
  double mb = formFactors->getQuarkMass(5);  // m_b mass from the dispersion QM, GeV
  double Mw = 80.403;                        // GeV W-boson mass, GeV
  double mt = 174.2;                         // GeV t-quark mass, GeV
  double fb = 0.0;                           // leptonic decay constant of B-meson, Gev

  EvtComplex Vtb, Vtq, Vub, Vuq, Vcb, Vcq;  // V_{tb}, V_{tq}, V_{ub}, V_{uq}, V_{cb}, V_{cq}
  EvtComplex CKM_factor; // V^*_{tq}*V_{tb}, where q={d,s}
  EvtComplex lambda_qu;  // V^*_{uq}*V_{ub}/V^*_{tq}*V_{tb}, where q={d,s}
  EvtComplex lambda_qc;  // V^*_{cq}*V_{cb}/V^*_{tq}*V_{tb}, where q={d,s}
  double Relambda_qu, Imlambda_qu;

  //
  // Setting of the mq and CKM matrix elements for different Bq-mesons tipes
  //
  EvtId idparent = parent->getId(); // Bq-meson Id
  EvtId IdMu1, IdMu2, IdMu3, IdMu4;

  if(idparent == EvtPDL::getId(std::string("B_s0"))||
     idparent == EvtPDL::getId(std::string("anti-B_s0"))){
     mq = formFactors->getQuarkMass(3);  // m_s mass from the dispersion QM
     fb = 0.24;                          // leptonic decay constant
         // V_{ts}
     Vtq = unit1*(1.0-0.5*pow(CKM_lambda,2.0)) + 
           pow(CKM_lambda,2.0)*(CKM_barrho*unit1 +    
           CKM_bareta*uniti)/sqrt(1.0-pow(CKM_lambda,2.0));
     Vtq = -CKM_A*pow(CKM_lambda,2.0)*Vtq; 
	 // V_{us}
     Vuq = CKM_lambda*unit1;
	 // V_{cs}
     Vcq = unit1 - 0.5*pow(CKM_lambda,2.0) - 
               0.125*pow(CKM_lambda,4.0)*(1.0 + 4.0*pow(CKM_A,2.0));
  }

  if(idparent == EvtPDL::getId(std::string("B0"))||
     idparent == EvtPDL::getId(std::string("anti-B0"))){
     mq = formFactors->getQuarkMass(2);  // m_d mass from the dispersion QM 
     fb = 0.20;                          // leptonic decay constant
         // V_{td}
     Vtq = unit1 - (1.0 - 0.5*pow(CKM_lambda,2.0))*(CKM_barrho*unit1 + 
           CKM_bareta*uniti)/sqrt(1.0-pow(CKM_lambda,2.0));
     Vtq = CKM_A*pow(CKM_lambda,3.0)*Vtq; 
	 // V_{ud}
     Vuq = unit1*(1.0-0.5*pow(CKM_lambda,2.0)-0.125*pow(CKM_lambda,4.0));
	 // V_{cd}
     Vcq = unit1*(-CKM_lambda + 0.5*pow(CKM_A,2.0)*pow(CKM_lambda,5.0)*
           (1.0 - 2.0*(CKM_barrho*unit1 + CKM_bareta*uniti)/sqrt(1.0-pow(CKM_lambda,2.0))));
  }

  if(mq<0.001){
     EvtGenReport(EVTGEN_ERROR,"EvtGen") 
            << "\n\n The function EvtbsToLLLLAmp::CalcAmp(...)"
            << "\n Error in the mq setting!"
            << "\n mq = " << mq << "< 0.001" 
            << "\n idparent = " << idparent << std::endl; 
     ::abort();
  }

  Vtb = unit1*(1.0-0.5*pow(CKM_A*CKM_lambda*CKM_lambda,2.0));      // V_{tb}
  Vub = CKM_A*pow(CKM_lambda,3.0)*(CKM_barrho*unit1 - 
                  CKM_bareta*uniti)/sqrt(1.0-pow(CKM_lambda,2.0)); // V_{ub}
  Vcb = unit1*CKM_A*pow(CKM_lambda,2.0);                           // V_{cb}

  CKM_factor  = conj(Vtq)*Vtb;                                     // V^*_{tq}*V_{tb}

  lambda_qu = conj(Vuq)*Vub/CKM_factor;            // V^*_{uq}*V_{ub}/V^*_{tq}*V_{tb}
  Relambda_qu = real(lambda_qu); 
  Imlambda_qu = imag(lambda_qu);

  lambda_qc = conj(Vcq)*Vcb/CKM_factor;            // V^*_{cq}*V_{cb}/V^*_{tq}*V_{tb}



  //
  // Setting the leptonic kinematical properties
  //

  // to find charges of ell^+ and ell^- in the B-meson  daughters
  int charge1 = (EvtPDL::chg3(parent->getDaug(il1)->getId()))/3;
  int charge2 = (EvtPDL::chg3(parent->getDaug(il2)->getId()))/3;
  int charge3 = (EvtPDL::chg3(parent->getDaug(il3)->getId()))/3;
  int charge4 = (EvtPDL::chg3(parent->getDaug(il4)->getId()))/3;
  if((abs(charge1)!=1)||(abs(charge2)!=1)||(abs(charge3)!=1)||(abs(charge4)!=1)||
     (charge1+charge2+charge3+charge4!=0)){
     EvtGenReport(EVTGEN_ERROR,"EvtGen") 
            << "\n\n The function EvtbsToLLLLAmp::CalcAmp(...)"
            << "\n Error in the leptonic charge definition!"
            << "\n charge1             =" << charge1
            << "\n charge2             =" << charge2
            << "\n charge3             =" << charge3
            << "\n charge4             =" << charge4
            << "\n number of daughters =" << parent->getNDaug()
            << std::endl;
     ::abort();
  }

  EvtParticle* lep1Plus  = 0;
  EvtParticle* lep1Minus = 0;
  EvtParticle* lep2Plus  = 0;
  EvtParticle* lep2Minus = 0;

  EvtVector4R p;    // B-meson momentum in the B-rest frame
  EvtVector4R q;    // first transition 4-momentum in the B-rest frame
  EvtVector4R k;    // second transition 4-momentum in the B-rest frame

  double q2;        // Mandelstam variable s=q^2

  // Nondimentional 4-momentums
  EvtVector4R hatp;
  EvtVector4R hatq;
  EvtVector4R hatk;

  EvtVector4R qsecond;    // first transition 4-momentum in the B-rest frame
  EvtVector4R ksecond;    // second transition 4-momentum in the B-rest frame

  double q2second;        // Mandelstam variable s=q^2

  // Nondimentional 4-momentums
  EvtVector4R hatpsecond;
  EvtVector4R hatqsecond;
  EvtVector4R hatksecond;

  EvtVector4R k_1;  // 4-momentum of ell^+ in the B-rest frame
  EvtVector4R k_2;  // 4-momentum of ell^- in the B-rest frame
  EvtVector4R k_3;  // 4-momentum of ell^+ in the B-rest frame
  EvtVector4R k_4;  // 4-momentum of ell^- in the B-rest frame

  k_1.set(0.0,0.0,0.0,0.0);
  k_2.set(0.0,0.0,0.0,0.0);
  k_3.set(0.0,0.0,0.0,0.0);
  k_4.set(0.0,0.0,0.0,0.0);

  if ((charge1+charge2==0)&&(charge3+charge4==0)){
     // positive charged lepton 1
     lep1Plus  = (charge1 > charge2) ? parent->getDaug(il1) : parent->getDaug(il2);
     // negative charged lepton 1
     lep1Minus = (charge1 < charge2) ? parent->getDaug(il1) : parent->getDaug(il2);
     if(charge1 > charge2){
       k_1 = parent->getDaug(il1)->getP4();
       k_2 = parent->getDaug(il2)->getP4();
       IdMu1 = parent->getDaug(il1)->getId();
       IdMu2 = parent->getDaug(il2)->getId();
     } else {
       k_1 = parent->getDaug(il2)->getP4();
       k_2 = parent->getDaug(il1)->getP4();
       IdMu1 = parent->getDaug(il2)->getId();
       IdMu2 = parent->getDaug(il1)->getId();
     }
     // positive charged lepton 2
     lep2Plus  = (charge3 > charge4) ? parent->getDaug(il3) : parent->getDaug(il4); 
     // negative charged lepton 2
     lep2Minus = (charge3 < charge4) ? parent->getDaug(il3) : parent->getDaug(il4);
     if(charge3 > charge4){
       k_3 = parent->getDaug(il3)->getP4();
       k_4 = parent->getDaug(il4)->getP4();
       IdMu3 = parent->getDaug(il3)->getId();
       IdMu4 = parent->getDaug(il4)->getId();
     } else {
       k_3 = parent->getDaug(il4)->getP4();
       k_4 = parent->getDaug(il3)->getP4();
       IdMu3 = parent->getDaug(il4)->getId();
       IdMu4 = parent->getDaug(il3)->getId();
     }
  }
  if ((charge1+charge3==0)&&(charge2+charge4==0)){
     // positive charged lepton 1
     lep1Plus  = (charge1 > charge3) ? parent->getDaug(il1) : parent->getDaug(il3); 
     // negative charged lepton 1
     lep1Minus = (charge1 < charge3) ? parent->getDaug(il1) : parent->getDaug(il3);
     if(charge1 > charge3){
       k_1 = parent->getDaug(il1)->getP4();
       k_2 = parent->getDaug(il3)->getP4();
       IdMu1 = parent->getDaug(il1)->getId();
       IdMu2 = parent->getDaug(il3)->getId();
     } else {
       k_1 = parent->getDaug(il3)->getP4();
       k_2 = parent->getDaug(il1)->getP4();
       IdMu1 = parent->getDaug(il3)->getId();
       IdMu2 = parent->getDaug(il1)->getId();
     }
     // positive charged lepton 2
     lep2Plus  = (charge2 > charge4) ? parent->getDaug(il2) : parent->getDaug(il4); 
     // negative charged lepton 2
     lep2Minus = (charge2 < charge4) ? parent->getDaug(il2) : parent->getDaug(il4); 
     if(charge2 > charge4){
       k_3 = parent->getDaug(il2)->getP4();
       k_4 = parent->getDaug(il4)->getP4();
       IdMu3 = parent->getDaug(il2)->getId();
       IdMu4 = parent->getDaug(il4)->getId();
     } else {
       k_3 = parent->getDaug(il4)->getP4();
       k_4 = parent->getDaug(il2)->getP4();
       IdMu3 = parent->getDaug(il4)->getId();
       IdMu4 = parent->getDaug(il2)->getId();
     }
  }

  p    = parent->getP4Restframe();
  hatp = p/M1;
  
  //
  // The calculation of the FIRST part of the amplitude
  //

  q    = k_1 + k_2;
  k    = k_3 + k_4;
  q2   = q.mass2();       // Mandelstam variable s=q^2
  hatq = q/M1;
  hatk = k/M1;

  // The Wilson Coefficients preparation according to the paper
  // A.J.Buras, M.Munz, Phys.Rev.D52, p.189 (1995)
  double c1,c2;
  EvtComplex a1,c7gam,c9eff_b2q,c9eff_barb2barq,c10a;

  // Excluded of the J/psi and psi' resonant area 
  if((res_swch == 1) && (q2 >= 9.199) && (q2 <= 15.333)){
    c1              = 0.0;
    c2              = 0.0;
    a1              = unit1*0.0;
    c7gam           = unit1*0.0;
    c9eff_b2q       = unit1*0.0;
    c9eff_barb2barq = unit1*0.0;
    c10a            = unit1*0.0;
  }
  else{
    c1              = WilsCoeff->C1(mu, Mw, Nf, ias); 
    c2              = WilsCoeff->C2(mu, Mw, Nf, ias); 
    a1              = unit1*(c1 + c2/3.0);
    c7gam           = WilsCoeff->GetC7Eff(mu, Mw, mt, Nf, ias);
    c9eff_b2q       = WilsCoeff->GetC9Eff(0,res_swch,ias,Nf,q2,mb,mq,mc,mu,mt,Mw,ml,
                               Relambda_qu,Imlambda_qu);
    c9eff_barb2barq = WilsCoeff->GetC9Eff(1,res_swch,ias,Nf,q2,mb,mq,mc,mu,mt,Mw,ml,
                               Relambda_qu,Imlambda_qu);
    c10a            =  WilsCoeff->GetC10Eff(mt, Mw);
  }

  EvtComplex Fv, Fa; // The change of the sign is included in the amplitudes definition! 
  EvtComplex Ftv_b2q, Ftv_barb2barq;
  EvtComplex Fta_b2q, Fta_barb2barq;

  // Excluded of the J/psi and psi' resonant area
  if((res_swch == 1) && (q2 >= 9.199) && (q2 <= 15.333)){
    fb            = 0.0;
    Fa            = unit1*0.0;
    Fv            = unit1*0.0;
    Fta_b2q       = unit1*0.0;
    Fta_barb2barq = unit1*0.0;
    Ftv_b2q       = unit1*0.0;
    Ftv_barb2barq = unit1*0.0; 
  }
  else{

    if(fb<0.01){
      EvtGenReport(EVTGEN_ERROR,"EvtGen") 
        << "\n\n The function EvtbsToLLLLAmp::CalcAmp(...)"
        << "\n Leptonic decay constant fb is not uninitialized in this function!"
        << " fb = " << fb << std::endl;
     ::abort();
    }

    // For \bar B^0_q -> l^+ l^- gamma
    formFactors->getPhotonFF(0, fb, parent->getId(),    
                             q2, M1, mb, mq, c7gam, a1, 
                             lambda_qu, lambda_qc, 
                             Fv, Fa, Ftv_b2q, Fta_b2q);

    // For B^0_q -> l^+ l^- gamma
    formFactors->getPhotonFF(1, fb, parent->getId(),    
                             q2, M1, mb, mq, c7gam, a1, 
                             lambda_qu, lambda_qc, 
                             Fv, Fa, Ftv_barb2barq, Fta_barb2barq);
  }
 
  
  // The functions for the hadronic matrix element calculation
  EvtComplex a_b2q, a_barb2barq, b_b2q, b_barb2barq, c_b2q, c_barb2barq;  
  EvtComplex e_b2q, e_barb2barq, f_b2q, f_barb2barq, g_b2q, g_barb2barq;
  
  a_b2q       =  c9eff_b2q*Fv             + 2.0*c7gam*Ftv_b2q*mb*M1/q2;
  a_barb2barq =  c9eff_barb2barq*Fv + 2.0*c7gam*Ftv_barb2barq*mb*M1/q2;

  b_b2q       = (c9eff_b2q*Fa             + 2.0*c7gam*Fta_b2q*mb*M1/q2)*(hatp*hatk);
  b_barb2barq = (c9eff_barb2barq*Fa + 2.0*c7gam*Fta_barb2barq*mb*M1/q2)*(hatp*hatk);

  c_b2q       =  c9eff_b2q*Fa             + 2.0*c7gam*Fta_b2q*mb*M1/q2;
  c_barb2barq =  c9eff_barb2barq*Fa + 2.0*c7gam*Fta_barb2barq*mb*M1/q2;

  e_b2q       =  c10a*Fv;
  e_barb2barq =  e_b2q;

  f_b2q       =  c10a*Fa*(hatp*hatk);
  f_barb2barq =  f_b2q;

  g_b2q       =  c10a*Fa;
  g_barb2barq =  g_b2q;


  //
  // The calculation of the SECOND part of the amplitude
  //

  qsecond    = k_1 + k_4;
  ksecond    = k_3 + k_2;
  q2second   = qsecond.mass2();       // Mandelstam variable s=q^2
  hatqsecond = qsecond/M1;
  hatksecond = ksecond/M1;

  // Excluded of the J/psi and psi' resonant area 
  if((res_swch == 1) && (q2second >= 9.199) && (q2second <= 15.333)){
    c1              = 0.0;
    c2              = 0.0;
    a1              = unit1*0.0;
    c7gam           = unit1*0.0;
    c9eff_b2q       = unit1*0.0;
    c9eff_barb2barq = unit1*0.0;
    c10a            = unit1*0.0;
  }
  else{
    c1              = WilsCoeff->C1(mu, Mw, Nf, ias); 
    c2              = WilsCoeff->C2(mu, Mw, Nf, ias); 
    a1              = unit1*(c1 + c2/3.0);
    c7gam           = WilsCoeff->GetC7Eff(mu, Mw, mt, Nf, ias);
    c9eff_b2q       = WilsCoeff->GetC9Eff(0,res_swch,ias,Nf,q2second,mb,mq,mc,mu,mt,Mw,ml,
                               Relambda_qu,Imlambda_qu);
    c9eff_barb2barq = WilsCoeff->GetC9Eff(1,res_swch,ias,Nf,q2second,mb,mq,mc,mu,mt,Mw,ml,
                               Relambda_qu,Imlambda_qu);
    c10a            =  WilsCoeff->GetC10Eff(mt, Mw);
  }


  // Excluded of the J/psi and psi' resonant area
  if((res_swch == 1) && (q2second >= 9.199) && (q2second <= 15.333)){
    fb            = 0.0;
    Fa            = unit1*0.0;
    Fv            = unit1*0.0;
    Fta_b2q       = unit1*0.0;
    Fta_barb2barq = unit1*0.0;
    Ftv_b2q       = unit1*0.0;
    Ftv_barb2barq = unit1*0.0; 
  }
  else{

    if(fb<0.01){
      EvtGenReport(EVTGEN_ERROR,"EvtGen") 
        << "\n\n The function EvtbsToLLLLAmp::CalcAmp(...)"
        << "\n Leptonic decay constant fb is not uninitialized in this function!"
        << " fb = " << fb << std::endl;
     ::abort();
    }

    // For \bar B^0_q -> l^+ l^- gamma
    formFactors->getPhotonFF(0, fb, parent->getId(),    
                             q2second, M1, mb, mq, c7gam, a1, 
                             lambda_qu, lambda_qc, 
                             Fv, Fa, Ftv_b2q, Fta_b2q);

    // For B^0_q -> l^+ l^- gamma
    formFactors->getPhotonFF(1, fb, parent->getId(),    
                             q2second, M1, mb, mq, c7gam, a1, 
                             lambda_qu, lambda_qc, 
                             Fv, Fa, Ftv_barb2barq, Fta_barb2barq);
  }
 
  
  // The functions for the hadronic matrix element calculation
  EvtComplex a_b2qsecond, a_barb2barqsecond, b_b2qsecond, b_barb2barqsecond, c_b2qsecond, c_barb2barqsecond;  
  EvtComplex e_b2qsecond, e_barb2barqsecond, f_b2qsecond, f_barb2barqsecond, g_b2qsecond, g_barb2barqsecond;
  
  a_b2qsecond       =  c9eff_b2q*Fv             + 2.0*c7gam*Ftv_b2q*mb*M1/q2second;
  a_barb2barqsecond =  c9eff_barb2barq*Fv + 2.0*c7gam*Ftv_barb2barq*mb*M1/q2second;

  b_b2qsecond       = (c9eff_b2q*Fa             + 2.0*c7gam*Fta_b2q*mb*M1/q2second)*(hatpsecond*hatksecond);
  b_barb2barqsecond = (c9eff_barb2barq*Fa + 2.0*c7gam*Fta_barb2barq*mb*M1/q2second)*(hatpsecond*hatksecond);

  c_b2qsecond       =  c9eff_b2q*Fa             + 2.0*c7gam*Fta_b2q*mb*M1/q2second;
  c_barb2barqsecond =  c9eff_barb2barq*Fa + 2.0*c7gam*Fta_barb2barq*mb*M1/q2second;

  e_b2qsecond       =  c10a*Fv;
  e_barb2barqsecond =  e_b2qsecond;

  f_b2qsecond       =  c10a*Fa*(hatpsecond*hatksecond);
  f_barb2barqsecond =  f_b2qsecond;

  g_b2qsecond       =  c10a*Fa;
  g_barb2barqsecond =  g_b2qsecond;


     
  EvtTensor4C T1,T2;              // Tensor structures for 
  EvtTensor4C T1second,T2second;  //                the hadronic matrix element calculation 

  // B - and barB - mesons descriptors
  static EvtIdSet bmesons("anti-B0","anti-B_s0");
  static EvtIdSet bbarmesons("B0","B_s0");

  EvtId parentID = parent->getId();

  if (bmesons.contains(parentID)){

    // The amplitude for the decay barB -> gamma ell^+ ell^-  or
    // b \bar q -> gamma ell^+ ell^- 

    T1 =   a_b2q*unit1*dual(EvtGenFunctions::directProd(hatq,hatk))
         - b_b2q*uniti*EvtTensor4C::g()
         + c_b2q*uniti*EvtGenFunctions::directProd(hatk,hatq);

    T2 =   e_b2q*unit1*dual(EvtGenFunctions::directProd(hatp,hatk))
         - f_b2q*uniti*EvtTensor4C::g()
         + g_b2q*uniti*EvtGenFunctions::directProd(hatk,hatq);

    T1second =   a_b2qsecond*unit1*dual(EvtGenFunctions::directProd(hatqsecond,hatksecond))
               - b_b2qsecond*uniti*EvtTensor4C::g()
               + c_b2qsecond*uniti*EvtGenFunctions::directProd(hatksecond,hatqsecond);

    T2second =   e_b2qsecond*unit1*dual(EvtGenFunctions::directProd(hatpsecond,hatksecond))
               - f_b2qsecond*uniti*EvtTensor4C::g()
               + g_b2qsecond*uniti*EvtGenFunctions::directProd(hatksecond,hatqsecond);


    int i1, i2, i3, i4;  // leptonic spin structures counters
    int leptonicspin[4]; // array for the saving of the leptonic spin configuration

   // Tables for correspondings
   // l^+(k_1) && lep1Plus  && k_1 && i1
   // l^-(k_2) && lep1Minus && k_2 && i2
   // l^+(k_3) && lep2Plus  && k_3 && i3 
   // l^-(k_4) && lep2Minus && k_4 && i4

    for(i2=0;i2<2;i2++){
      leptonicspin[0] = i2;
      for(i1=0;i1<2;i1++){
        leptonicspin[1] = i1;
        for(i4=0;i4<2;i4++){
          leptonicspin[2] = i4;
          for(i3=0;i3<2;i3++){
            leptonicspin[3] = i3;
            EvtVector4C VL2L1, AL2L1, VL4L3;
            EvtVector4C E1, E2;
            EvtVector4C VL2L1second, AL2L1second, VL4L3second;
            EvtVector4C E1second, E2second;
            
            VL2L1 = EvtLeptonVCurrent(lep1Minus->spParent(i2),lep1Plus->spParent(i1));
            AL2L1 = EvtLeptonACurrent(lep1Minus->spParent(i2),lep1Plus->spParent(i1));
            VL4L3 = EvtLeptonVCurrent(lep2Minus->spParent(i4),lep2Plus->spParent(i3));
            E1=T1.cont2(VL4L3);
            E2=T2.cont2(VL4L3);

            VL2L1second = EvtLeptonVCurrent(lep2Minus->spParent(i2),lep1Plus->spParent(i1));
            AL2L1second = EvtLeptonACurrent(lep2Minus->spParent(i2),lep1Plus->spParent(i1));
            VL4L3second = EvtLeptonVCurrent(lep1Minus->spParent(i4),lep2Plus->spParent(i3));
            E1second=T1second.cont2(VL4L3second);
            E2second=T2second.cont2(VL4L3second);

            amp.vertex(leptonicspin,CKM_factor*(VL2L1*E1+AL2L1*E2+
                                                VL2L1second*E1second+AL2L1second*E2second));

//            EvtGenReport(EVTGEN_ERROR,"EvtGen")
//             << "\n\n ============================================================================"
//             << "\n The matrix element (first + second) = "
//             <<     CKM_factor*(VL2L1*E1+AL2L1*E2+VL2L1second*E1second+AL2L1second*E2second)
//             << "\n The matrix element (only first) = "
//             <<     CKM_factor*(VL2L1*E1+AL2L1*E2)
//             << "============================================================================\n\n"
//             << std::endl;
          }
        }
      }
    } 

//    EvtGenReport(EVTGEN_ERROR,"EvtGen") << "\n The function EvtbsToLLLLAmp::CalcAmp(...) passed with arguments:"
//      << "\n ============================================================================"
//      << "\n Input parameters:"
//      << "\n mu = " << mu 
//      << "\n Nf =" << Nf 
//      << "\n res_swch = " << res_swch 
//      << "\n ias = " << ias 
//      << "\n CKM_A = " << CKM_A
//      << "\n CKM_lambda = " << CKM_lambda
//      << "\n CKM_barrho = " << CKM_barrho
//      << "\n CKM_bareta = " << CKM_bareta 
//      << "\n CKM_factor = " << CKM_factor
//      << "\n ============================================================================"
//      << "\n Kinematics:"
//      << "\n k_1        = " << k_1
//      << "\n m_ell_1  =" << parent->getDaug(il1)->mass()
//      << "\n k_2        = " << k_2
//      << "\n m_ell_2  =" << parent->getDaug(il2)->mass()
//      << "\n k_3        = " << k_3
//      << "\n m_ell_3  =" << parent->getDaug(il3)->mass()
//      << "\n k_4        = " << k_4
//      << "\n m_ell_4  =" << parent->getDaug(il4)->mass()
//      << "\n p          = " << p
//      << "\n q          = " << q
//      << "\n k          = " << k
//      << "\n ============================================================================"
//      << "\n Form-factors"
//      << "\n Fv            = " << Fv
//      << "\n Fa            = " << Fa 
//      << "\n Ftv_b2q       = " << Ftv_b2q
//      << "\n Fta_b2q       = " << Fta_b2q
//      << "\n Ftv_barb2barq = " << Ftv_barb2barq
//      << "\n Fta_barb2barq = " << Fta_barb2barq
//      << "\n fb            = " << fb
//      << "\n ============================================================================"
//      << "\n Wilson Coefficients:"
//      << "\n Re(c7gam)           = " << real(c7gam) 
//      << " Im(c7gam)           = " << imag(c7gam)
//      << "\n Re(c9eff_b2q)       = " << real(c9eff_b2q) 
//      << " Im(c9eff_b2q)       = " << imag(c9eff_b2q)
//      << "\n Re(c9eff_barb2barq) = " << real(c9eff_barb2barq) 
//      << " Im(c9eff_barb2barq) = " << imag(c9eff_barb2barq)
//      << "\n Re(c10a)            = " << real(c10a)  
//      << " Im(c10a)            = " << imag(c10a)
//      << "\n ============================================================================"
//      << "\n Functions in the matrix element:"
//      << "\n a_b2q  = " << a_b2q 
//      << "\n b_b2q  = " << b_b2q 
//      << "\n c_b2q  = " << c_b2q 
//      << "\n e_b2q  = " << e_b2q 
//      << "\n f_b2q  = " << f_b2q 
//      << "\n g_b2q  = " << g_b2q 
//      << "\n ============================================================================"
//      << "\n Partical Properties:"
//      << "\n IdB     = " << idparent  << " == " << EvtPDL::getId(std::string("anti-B_s0"))
//      << "\n IdMu1   = " << IdMu1           << " == " << EvtPDL::getId(std::string("mu+"))
//      << "\n IdMu2   = " << IdMu2           << " == " << EvtPDL::getId(std::string("mu-"))
//      << "\n IdMu3   = " << IdMu3           << " == " << EvtPDL::getId(std::string("mu+"))
//      << "\n IdMu4   = " << IdMu4           << " == " << EvtPDL::getId(std::string("mu-"))
//      << "\n\n\n\n"
//      << std::endl;

  } else {
    
    if (bbarmesons.contains(parentID)){

    // The amplitude for the decay B -> gamma ell^+ ell^- or
    // q bar b -> gamma ell^+ ell^-

    T1 = - a_barb2barq*unit1*dual(EvtGenFunctions::directProd(hatq,hatk))
         - b_barb2barq*uniti*EvtTensor4C::g()
         + c_barb2barq*uniti*EvtGenFunctions::directProd(hatk,hatq);


    T2 = - e_barb2barq*unit1*dual(EvtGenFunctions::directProd(hatq,hatk))
         - f_barb2barq*uniti*EvtTensor4C::g()
         + g_barb2barq*uniti*EvtGenFunctions::directProd(hatk,hatq);

    T1second =  - a_barb2barqsecond*unit1*dual(EvtGenFunctions::directProd(hatqsecond,hatksecond))
                - b_barb2barqsecond*uniti*EvtTensor4C::g()
                + c_barb2barqsecond*uniti*EvtGenFunctions::directProd(hatksecond,hatqsecond);

    T2second =  - e_barb2barqsecond*unit1*dual(EvtGenFunctions::directProd(hatpsecond,hatksecond))
                - f_barb2barqsecond*uniti*EvtTensor4C::g()
                + g_barb2barqsecond*uniti*EvtGenFunctions::directProd(hatksecond,hatqsecond);


    int i1, i2, i3, i4;  // leptonic spin structures counters
    int leptonicspin[4]; // array for the saving of the leptonic spin configuration

   // Tables for correspondings
   // l^+(k_1) && lep1Plus  && k_1 && i1
   // l^-(k_2) && lep1Minus && k_2 && i2
   // l^+(k_3) && lep2Plus  && k_3 && i3 
   // l^-(k_4) && lep2Minus && k_4 && i4

    for(i2=1;i2<0;i2--){
      leptonicspin[0] = i2;
      for(i1=1;i1<0;i1--){
        leptonicspin[1] = i1;
        for(i4=1;i4<0;i4--){
          leptonicspin[2] = i4;
          for(i3=1;i3<0;i3--){
            leptonicspin[3] = i3;
            EvtVector4C VL2L1, AL2L1, VL4L3;
            EvtVector4C E1, E2;
            EvtVector4C VL2L1second, AL2L1second, VL4L3second;
            EvtVector4C E1second, E2second;

            VL2L1 = EvtLeptonVCurrent(lep1Minus->spParent(i2),lep1Plus->spParent(i1));
            AL2L1 = EvtLeptonACurrent(lep1Minus->spParent(i2),lep1Plus->spParent(i1));
            VL4L3 = EvtLeptonVCurrent(lep2Minus->spParent(i4),lep2Plus->spParent(i3));
            E1=T1.cont2(VL4L3);
            E2=T2.cont2(VL4L3);

            VL2L1second = EvtLeptonVCurrent(lep2Minus->spParent(i2),lep1Plus->spParent(i1));
            AL2L1second = EvtLeptonACurrent(lep2Minus->spParent(i2),lep1Plus->spParent(i1));
            VL4L3second = EvtLeptonVCurrent(lep1Minus->spParent(i4),lep2Plus->spParent(i3));
            E1second=T1second.cont2(VL4L3second);
            E2second=T2second.cont2(VL4L3second);

            amp.vertex(leptonicspin,conj(CKM_factor)*(VL2L1*E1+AL2L1*E2+
                                                VL2L1second*E1second+AL2L1second*E2second));

          }
        }
      }
    }

//    EvtGenReport(EVTGEN_ERROR,"EvtGen") << "\n The function EvtbsToLLLLAmp::CalcAmp(...) passed with arguments:"
//      << "\n ============================================================================"
//      << "\n Input parameters:"
//      << "\n mu = " << mu 
//      << "\n Nf =" << Nf 
//      << "\n res_swch = " << res_swch 
//      << "\n ias = " << ias 
//      << "\n CKM_A = " << CKM_A
//      << "\n CKM_lambda = " << CKM_lambda
//      << "\n CKM_barrho = " << CKM_barrho
//      << "\n CKM_bareta = " << CKM_bareta 
//      << "\n CKM_factor = " << CKM_factor
//      << "\n ============================================================================"
//      << "\n Kinematics:"
//      << "\n k_1        = " << k_1
//      << "\n m_ell_1  =" << parent->getDaug(il1)->mass()
//      << "\n k_2        = " << k_2
//      << "\n m_ell_2  =" << parent->getDaug(il2)->mass()
//      << "\n k_3        = " << k_3
//      << "\n m_ell_3  =" << parent->getDaug(il3)->mass()
//      << "\n k_4        = " << k_4
//      << "\n m_ell_4  =" << parent->getDaug(il4)->mass()
//      << "\n p          = " << p
//      << "\n q          = " << q
//      << "\n k          = " << k
//      << "\n ============================================================================"
//      << "\n Form-factors"
//      << "\n Fv            = " << Fv
//      << "\n Fa            = " << Fa 
//      << "\n Ftv_b2q       = " << Ftv_b2q
//      << "\n Fta_b2q       = " << Fta_b2q
//      << "\n Ftv_barb2barq = " << Ftv_barb2barq
//      << "\n Fta_barb2barq = " << Fta_barb2barq
//      << "\n fb            = " << fb
//      << "\n ============================================================================"
//      << "\n Wilson Coefficients:"
//      << "\n Re(c7gam)           = " << real(c7gam) 
//      << " Im(c7gam)           = " << imag(c7gam)
//      << "\n Re(c9eff_b2q)       = " << real(c9eff_b2q) 
//      << " Im(c9eff_b2q)       = " << imag(c9eff_b2q)
//      << "\n Re(c9eff_barb2barq) = " << real(c9eff_barb2barq) 
//      << " Im(c9eff_barb2barq) = " << imag(c9eff_barb2barq)
//      << "\n Re(c10a)            = " << real(c10a)  
//      << " Im(c10a)            = " << imag(c10a)
//      << "\n ============================================================================"
//      << "\n Functions in the matrix element:"
//      << "\n a_barb2barq  = " << a_barb2barq 
//      << "\n b_barb2barq  = " << b_barb2barq 
//      << "\n c_barb2barq  = " << c_barb2barq 
//      << "\n e_barb2barq  = " << e_barb2barq 
//      << "\n f_barb2barq  = " << f_barb2barq 
//      << "\n g_barb2barq  = " << g_barb2barq 
//      << "\n ============================================================================"
//      << "\n Partical Properties:"
//      << "\n IdB     = " << idparent       << " == " << EvtPDL::getId(std::string("B_s0"))
//      << "\n IdMu1   = " << IdMu1           << " == " << EvtPDL::getId(std::string("mu+"))
//      << "\n IdMu2   = " << IdMu2           << " == " << EvtPDL::getId(std::string("mu-"))
//      << "\n IdMu3   = " << IdMu3           << " == " << EvtPDL::getId(std::string("mu+"))
//      << "\n IdMu4   = " << IdMu4           << " == " << EvtPDL::getId(std::string("mu-"))
//      << "\n\n\n\n"
//      << std::endl;     
    }
    else{
      EvtGenReport(EVTGEN_ERROR,"EvtGen") << "\n\n The function EvtbsToLLLLAmp::CalcAmp(...)"
                             << "\n Wrong Bq-meson number"
                             << std::endl;
      ::abort();
    }    
  }

}

//
// The decays Bq ->  ell^+ ell^- ell^+ ell^- maximum probability calculation 
//
double EvtbsToLLLLAmp::CalcMaxProb(
//                                     EvtId parnum,
//                                     EvtId l1num, EvtId l2num, 
//                                     EvtId l3num, EvtId l4num,
//		                       Evtbs2llGammaFF *formFactors,
//                                     EvtbTosllWilsCoeffNLO *WilsCoeff, 
//                                     double mu, int Nf,   
//                                     int res_swch, int ias,
//                                     double CKM_A, double CKM_lambda, 
//                                     double CKM_barrho, double CKM_bareta
                                  ){

  double maxfoundprob = 12650.0; // maximum of the probability

  return maxfoundprob;
}


// Triangular function
double EvtbsToLLLLAmp::lambda(double a, double b, double c){
  double l;

  l=pow(a,2.0)+pow(b,2.0)+pow(c,2.0)-2.0*a*b-2.0*a*c-2.0*b*c;
 
  return l;
}



