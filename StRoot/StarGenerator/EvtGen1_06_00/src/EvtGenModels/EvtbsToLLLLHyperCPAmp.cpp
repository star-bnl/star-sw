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
// Module: EvtbsToLLLLHyperCPAmp.cpp
//
// Description: Preparation of the decay amplitude for the process:
//              B^0_{q}(p,M1) -> ell^+(k1,m) ell^-(k2,m) ell^+(k3,m) ell^-(k4,m)
//              in the HyperCP model.
//
//              [1] D.S.Gorbunov, Nucl.Phys.B602, pp.213-237 (2001);
//              [2] S.V. Demidov, D.S.Gorbunov, hep-ph/1112.5230v2, 17 April 2012. 
//
// Note: The code of this module is based on the EvtbsToLLLLAmp.cpp module code.
//
//
// Modification history:
//
//  N.Nikitin (nnikit@mail.cerrn.ch)  June     20, 2012     New CalcMaxProb() method preparation     
//  N.Nikitin (nnikit@mail.cerrn.ch)   May     11, 2012     Module created
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
// The header file for current class memeber functions description
#include "EvtGenModels/EvtbsToLLLLHyperCPAmp.hh" 
#include <cstdlib>

// input:   *parent      - the pointer to the parent particle (B-meson, the 
//                                               object of the EvtParticle class);
//               mS      - the mass of the scalar sgoldstino "S" (GeV);
//               mP      - the mass of the pseudoscalar sgoldstino "P" (GeV);
//           gammaS      - the decay width of the scalar sgoldstino "S" (GeV);
//           gammaP      - the decay width of the pseudoscalar sgoldstino "P" (GeV);
//           mLiiLR      -
//               Fc      - coupling constant (GeV^2);
//        mDijLL(RR)     - parameters for \bar Bq-decays
//        mDjiLL(RR)     - parameters for Bq-decays (i <-> j!)
//                         d==1, s==2, b==3
//
void EvtbsToLLLLHyperCPAmp::CalcAmp(EvtParticle *parent, EvtAmp& amp, 
                             double mS, double mP, 
                             double gammaS, double gammaP, 
                             double mLiiLR, 
                             double Fc, 
                             double mD23LL, double mD23RR, 
                             double mD32LL, double mD32RR, 
                             double mD13LL, double mD13RR, 
                             double mD31LL, double mD31RR){

//  FILE *mytest;

  int il1=0, il2=1, il3=2, il4=3;  // leptons are the first, second, thirds 
                                   //                      and fourth daughter particles 

  EvtComplex unit1(1.0,0.0); // real unit
  EvtComplex uniti(0.0,1.0); // imaginary unit

  parent->mass();                // B - meson mass, GeV
  double fb = 0.0;                           // leptonic decay constant of B-meson, GeV

  double Cl = 0.0;                           // LPL and LSL - vertexes
  if(Fc != 0.0){
     Cl =  mLiiLR*mLiiLR/(sqrt(2)*Fc);
  }
  if(Cl == 0.0){
     EvtGenReport(EVTGEN_ERROR,"EvtGen") 
            << "\n\n The function EvtbsToLLLLHyperCPAmp::CalcAmp(...)"
            << "\n Error in the Cl setting!"
            << "\n     Cl = " << Cl
            << "\n mLiiLR = " << mLiiLR 
            << "\n     Fc = " << Fc << std::endl; 
     ::abort();
  }

  EvtComplex  MS = unit1*mS - uniti*gammaS/2.0; // complex mass of the scalar sgoldstino

  EvtComplex  MP = unit1*mP - uniti*gammaP/2.0; // complex mass of the pseudoscalar sgoldstino



  //
  // Setting of the different Bq-mesons tipes
  //
  EvtId idparent = parent->getId(); // Bq-meson Id
  EvtId IdMu1, IdMu2, IdMu3, IdMu4;
  
  double CB = 0.0;

  if(idparent == EvtPDL::getId(std::string("B_s0"))){
     fb = 0.24;                          // leptonic decay constant
     CB = mD32LL*mD32LL + mD32RR*mD32RR;
  }

  if(idparent == EvtPDL::getId(std::string("anti-B_s0"))){
     fb = 0.24;                          // leptonic decay constant
     CB = mD23LL*mD23LL + mD23RR*mD23RR;
  }

  if(idparent == EvtPDL::getId(std::string("B0"))){
     fb = 0.20;                          // leptonic decay constant
     CB = mD31LL*mD31LL + mD31RR*mD31RR;
  }

  if(idparent == EvtPDL::getId(std::string("anti-B0"))){
     fb = 0.20;                          // leptonic decay constant
     CB = mD13LL*mD13LL + mD13RR*mD13RR;
  }

  if(CB == 0.0){
     EvtGenReport(EVTGEN_ERROR,"EvtGen") 
            << "\n\n The function EvtbsToLLLLHyperCPAmp::CalcAmp(...)"
            << "\n Error in the CB setting!"
            << "\n       CB = " << CB
            << "\n   mD32LL = " << mD32LL
            << "\n   mD32RR = " << mD32RR
            << "\n   mD23LL = " << mD23LL
            << "\n   mD23RR = " << mD23RR
            << "\n   mD31LL = " << mD31LL
            << "\n   mD31RR = " << mD31RR
            << "\n   mD13LL = " << mD13LL
            << "\n   mD13RR = " << mD13RR
            << "\n idparent = " << idparent << std::endl; 
     ::abort();
  }


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
            << "\n\n The function EvtbsToLLLLHyperCPAmp::CalcAmp(...)"
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
  double k2;        // Mandelstam variable t=k^2

  EvtVector4R qsecond;    // first transition 4-momentum in the B-rest frame
  EvtVector4R ksecond;    // second transition 4-momentum in the B-rest frame
  double q2second;        // Mandelstam variable s=q^2
  double k2second;        // Mandelstam variable t=k^2

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
  
  //
  // The calculation of the FIRST part of the amplitude
  //

  q    = k_1 + k_2;
  k    = k_3 + k_4;
  q2   = q.mass2();       // Mandelstam variable s=q^2
  k2   = k.mass2();       // Mandelstam variable t=k^2



  //
  // The calculation of the SECOND part of the amplitude
  //

  qsecond    = k_1 + k_4;
  ksecond    = k_3 + k_2;
  q2second   = qsecond.mass2();       // Mandelstam variable s=q^2
  k2second   = ksecond.mass2();       // Mandelstam variable t=k^2

     
  // B - and barB - mesons descriptors
  static EvtIdSet bmesons("anti-B0","anti-B_s0");
  static EvtIdSet bbarmesons("B0","B_s0");

  EvtId parentID = parent->getId();

  if (bmesons.contains(parentID)){

    // The amplitude for the decay barB -> ell^+ ell^- ell^+ ell^-  or
    // b \bar q -> ell^+ ell^- ell^+ ell^- 

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

            EvtComplex SL2L1, PL4L3;
            EvtComplex SL2L1second, PL4L3second; 
            
            SL2L1 = EvtLeptonSCurrent(lep1Minus->spParent(i2),lep1Plus->spParent(i1));
            PL4L3 = EvtLeptonPCurrent(lep2Minus->spParent(i4),lep2Plus->spParent(i3));

            SL2L1second = EvtLeptonSCurrent(lep2Minus->spParent(i2),lep1Plus->spParent(i1));
            PL4L3second = EvtLeptonPCurrent(lep1Minus->spParent(i4),lep2Plus->spParent(i3));

            amp.vertex(leptonicspin,Cl*Cl*CB*fb*(
              SL2L1*PL4L3*(q2-k2)/((q2 - MS*MS)*(k2 - MP*MP)) -
              SL2L1second*PL4L3second*(q2second-k2second)/((q2second - MS*MS)*(k2second - MP*MP))
                                                )/(4.0*Fc*Fc));
          }
        }
      }
    } 

//    EvtGenReport(EVTGEN_ERROR,"EvtGen") << "\n The function EvtbsToLLLLHyperCPAmp::CalcAmp(...) passed with arguments:"
//      << "\n ============================================================================"
//      << "\n Input parameters:"
//      << "\n     mS = " << mS 
//      << "\n     mP = " << mP
//      << "\n gammaS = " << gammaS
//      << "\n gammaP = " << gammaP
//      << "\n mLiiLR = " << mLiiLR
//      << "\n     Fc = " << Fc
//      << "\n mD23LL = " << mD23LL
//      << "\n mD23RR = " << mD23RR
//      << "\n mD32LL = " << mD32LL
//      << "\n mD32RR = " << mD32RR
//      << "\n mD13LL = " << mD13LL
//      << "\n mD13RR = " << mD13RR
//      << "\n mD31LL = " << mD31LL
//      << "\n mD31RR = " << mD31RR
//      << "\n ============================================================================"
//      << "\n Kinematics:"
//      << "\n k_1        = " << k_1
//      << "\n m_ell_1    =" << parent->getDaug(il1)->mass()
//      << "\n k_2        = " << k_2
//      << "\n m_ell_2    =" << parent->getDaug(il2)->mass()
//      << "\n k_3        = " << k_3
//      << "\n m_ell_3    =" << parent->getDaug(il3)->mass()
//      << "\n k_4        = " << k_4
//      << "\n m_ell_4    =" << parent->getDaug(il4)->mass()
//      << "\n p          = " << p
//      << "\n q          = " << q
//      << "\n k          = " << k
//      << "\n qsecond    = " << qsecond
//      << "\n ksecond    = " << ksecond
//      << "\n ============================================================================"
//      << "\n Form-factors"
//      << "\n fb         = " << fb
//      << "\n ============================================================================"
//      << "\n Coefficients:"
//      << "\n Cl         = " << Cl
//      << "\n CB         = " << CB
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

    // The amplitude for the decay B -> ell^+ ell^- ell^+ ell^- or
    // q bar b -> ell^+ ell^- ell^+ ell^-

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

            EvtComplex SL2L1, PL4L3;
            EvtComplex SL2L1second, PL4L3second;

            SL2L1 = EvtLeptonSCurrent(lep1Minus->spParent(i2),lep1Plus->spParent(i1));
            PL4L3 = EvtLeptonPCurrent(lep2Minus->spParent(i4),lep2Plus->spParent(i3));

            SL2L1second = EvtLeptonSCurrent(lep2Minus->spParent(i2),lep1Plus->spParent(i1));
            PL4L3second = EvtLeptonPCurrent(lep1Minus->spParent(i4),lep2Plus->spParent(i3));

            amp.vertex(leptonicspin,Cl*Cl*CB*fb*(
               SL2L1*PL4L3*(q2-k2)/((q2 - MS*MS)*(k2 - MP*MP)) -
               SL2L1second*PL4L3second*(q2second-k2second)/((q2second - MS*MS)*(k2second - MP*MP))
                                                )/(4.0*Fc*Fc));
          }
        }
      }
    }

//    EvtGenReport(EVTGEN_ERROR,"EvtGen") << "\n The function EvtbsToLLLLHyperCPAmp::CalcAmp(...) passed with arguments:"
//      << "\n ============================================================================"
//      << "\n Input parameters:"
//      << "\n     mS = " << mS 
//      << "\n     mP = " << mP
//      << "\n gammaS = " << gammaS
//      << "\n gammaP = " << gammaP
//      << "\n mLiiLR = " << mLiiLR
//      << "\n     Fc = " << Fc
//      << "\n mD23LL = " << mD23LL
//      << "\n mD23RR = " << mD23RR
//      << "\n mD32LL = " << mD32LL
//      << "\n mD32RR = " << mD32RR
//      << "\n mD13LL = " << mD13LL
//      << "\n mD13RR = " << mD13RR
//      << "\n mD31LL = " << mD31LL
//      << "\n mD31RR = " << mD31RR
//      << "\n ============================================================================"
//      << "\n Kinematics:"
//      << "\n k_1        = " << k_1
//      << "\n m_ell_1    =" << parent->getDaug(il1)->mass()
//      << "\n k_2        = " << k_2
//      << "\n m_ell_2    =" << parent->getDaug(il2)->mass()
//      << "\n k_3        = " << k_3
//      << "\n m_ell_3    =" << parent->getDaug(il3)->mass()
//      << "\n k_4        = " << k_4
//      << "\n m_ell_4    =" << parent->getDaug(il4)->mass()
//      << "\n p          = " << p
//      << "\n q          = " << q
//      << "\n k          = " << k
//      << "\n qsecond    = " << qsecond
//      << "\n ksecond    = " << ksecond
//      << "\n ============================================================================"
//      << "\n Form-factors"
//      << "\n fb         = " << fb
//      << "\n ============================================================================"
//      << "\n Coefficients:"
//      << "\n Cl         = " << Cl
//      << "\n CB         = " << CB
//      << "\n ============================================================================"
//      << "\n Partical Properties:"
//      << "\n IdB     = " << idparent  << " == " << EvtPDL::getId(std::string("anti-B_s0"))
//      << "\n IdMu1   = " << IdMu1           << " == " << EvtPDL::getId(std::string("mu+"))
//      << "\n IdMu2   = " << IdMu2           << " == " << EvtPDL::getId(std::string("mu-"))
//      << "\n IdMu3   = " << IdMu3           << " == " << EvtPDL::getId(std::string("mu+"))
//      << "\n IdMu4   = " << IdMu4           << " == " << EvtPDL::getId(std::string("mu-"))
//      << "\n\n\n\n"
//      << std::endl;

    }
    else{
      EvtGenReport(EVTGEN_ERROR,"EvtGen") << "\n\n The function EvtbsToLLLLHyperCPAmp::CalcAmp(...)"
                             << "\n Wrong Bq-meson number"
                             << std::endl;
      ::abort();
    }    
  }

}

//

// The decays Bq ->  ell^+ ell^- ell^+ ell^- maximum probability calculation 

//
double EvtbsToLLLLHyperCPAmp::CalcMaxProb(
                                     EvtId parnum,
                                     EvtId l1num, EvtId /*l2num*/,
                                     EvtId /*l3num*/, EvtId /*l4num*/,
                                     double mS, double mP,
                                     double gammaS, double gammaP,
                                     double mLiiLR,
                                     double Fc,
                                     double mD23LL, double mD23RR,
                                     double mD32LL, double mD32RR,
                                     double mD13LL, double mD13RR,
                                     double mD31LL, double mD31RR
                                  ){

  if(Fc == 0.0){
     EvtGenReport(EVTGEN_ERROR,"EvtGen") 
            << "\n\n The function EvtbsToLLLLHyperCPAmp::CalcMaxProb"
            << "\n Error in the Fc setting!"
            << "\n       Fc = " << Fc 
            << "\n   mD32LL = " << mD32LL
            << "\n   mD32RR = " << mD32RR
            << "\n   mD23LL = " << mD23LL
            << "\n   mD23RR = " << mD23RR
            << "\n   mD31LL = " << mD31LL
            << "\n   mD31RR = " << mD31RR
            << "\n   mD13LL = " << mD13LL
            << "\n   mD13RR = " << mD13RR
            << "\n   parnum = " << parnum << std::endl; 
 
     ::abort();
  }
  

  double Cl = 0.0;                           // LPL and LSL - vertexes
  if(Fc != 0.0){
     Cl =  mLiiLR*mLiiLR/(sqrt(2)*Fc);
  }
  if(Cl == 0.0){
     EvtGenReport(EVTGEN_ERROR,"EvtGen") 
            << "\n\n The function EvtbsToLLLLHyperCPAmp::CalcMaxProb"
            << "\n Error in the Cl setting!"
            << "\n     Cl = " << Cl
            << "\n mLiiLR = " << mLiiLR 
            << "\n     Fc = " << Fc << std::endl; 
     ::abort();
  }

  //
  // Setting of the different Bq-mesons tipes
  //

  double fb = 0.0;  
  double CB = 0.0;

  if(parnum == EvtPDL::getId(std::string("B_s0"))){
     fb = 0.24;                          // leptonic decay constant
     CB = mD32LL*mD32LL + mD32RR*mD32RR;
  }

  if(parnum == EvtPDL::getId(std::string("anti-B_s0"))){
     fb = 0.24;                          // leptonic decay constant
     CB = mD23LL*mD23LL + mD23RR*mD23RR;
  }

  if(parnum == EvtPDL::getId(std::string("B0"))){
     fb = 0.20;                          // leptonic decay constant
     CB = mD31LL*mD31LL + mD31RR*mD31RR;
  }

  if(parnum == EvtPDL::getId(std::string("anti-B0"))){
     fb = 0.20;                          // leptonic decay constant
     CB = mD13LL*mD13LL + mD13RR*mD13RR;
  }

  if(CB == 0.0){
     EvtGenReport(EVTGEN_ERROR,"EvtGen") 
            << "\n\n The function EvtbsToLLLLHyperCPAmp::CalcMaxProb"
            << "\n Error in the CB setting!"
            << "\n       CB = " << CB
            << "\n   mD32LL = " << mD32LL
            << "\n   mD32RR = " << mD32RR
            << "\n   mD23LL = " << mD23LL
            << "\n   mD23RR = " << mD23RR
            << "\n   mD31LL = " << mD31LL
            << "\n   mD31RR = " << mD31RR
            << "\n   mD13LL = " << mD13LL
            << "\n   mD13RR = " << mD13RR
            << "\n   parnum = " << parnum << std::endl; 
     ::abort();
  }

  double M1 = EvtPDL::getMeanMass(parnum);  // B - meson mass
  EvtPDL::getMeanMass(l1num);   // leptonic mass


  // We find the maximum amplitude probability
  double maxfoundprob = Cl*Cl*CB*fb*fabs(mS*mS+mP*mP+M1*M1)*10000000.0/
                                          (4.0*Fc*Fc*mS*gammaS*mP*gammaP);

  if(maxfoundprob<=0.0){
     EvtGenReport(EVTGEN_ERROR,"EvtGen") 
       << "\n\n In the function EvtbsToLLLLHyperCPAmp::CalcMaxProb"
       << "\n maxfoundprob = " << maxfoundprob << " < 0 or =0!"
       << "\n           mS = " << mS
       << "\n           mP = " << mP
       << "\n       gammaS = " << gammaS
       << "\n       gammaP = " << gammaP
       << "\n       mLiiLR = " << mLiiLR
       << "\n           Fc = " << Fc
       << "\n       mD32LL = " << mD32LL
       << "\n       mD32RR = " << mD32RR
       << "\n       mD23LL = " << mD23LL
       << "\n       mD23RR = " << mD23RR
       << "\n       mD31LL = " << mD31LL
       << "\n       mD31RR = " << mD31RR
       << "\n       mD13LL = " << mD13LL
       << "\n       mD13RR = " << mD13RR
       << "\n       parnum = " << parnum
       << std::endl;
     ::abort();
  }

  EvtGenReport(EVTGEN_NOTICE,"EvtGen")
    << "\n maxfoundprob (...) = " 
    << maxfoundprob
    << std::endl;

  maxfoundprob *=1.01;

  return maxfoundprob;

}


// Triangular function
double EvtbsToLLLLHyperCPAmp::lambda(double a, double b, double c){
  double l;

  l=pow(a,2.0)+pow(b,2.0)+pow(c,2.0)-2.0*a*b-2.0*a*c-2.0*b*c;
 
  return l;
}



