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
// Module: EvtB2MuMuMuNuAmp.cpp
//
// Description: Amplitude preparation for the very rare four-leptonic decays 
//              B^-(p) -> Mu^+(k_1) Mu^-(k_2) \bar Nu_{Mu}(k_3) Mu^-(k_4).  
//
// Note: This my code is based on the "EvtbsToLLLLAmp.cpp" code.
//	 The main functiom for the amplitude calculation retuns the
//	 amplitude of the decay  B^- \to \mu^- \mu^+ \bar\nu_{\mu} \mu^-
//             or the decay  B^+ \to \mu^+ \mu^- \nu_{\mu} \mu^+
//
//
// Modification history:
//
//  Nikolai Nikitin (Lomonosov Moscow State Univ.)  August  05, 2015    Module created
//  Nikolai Nikitin (Lomonosov Moscow State Univ.)  October 05, 2015    Maximum value of amplitude was fixed
//                   Email: Nikolai.Nikitine@cern.ch
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
#include "EvtGenModels/EvtbTosllMSFF.hh"     // FF for Bu -> Rho and Omega transitions
// The header files for current class memeber functions description
#include "EvtGenModels/EvtB2MuMuMuNuAmp.hh"


#include <cstdlib>

// input:   *parent        - the pointer on the parent particle (B-meson, the 
//                                          object of the EvtParticle class);
//          *formFactorsms - the pointer on the EvtbTosllMSFF class object;


void EvtB2MuMuMuNuAmp::CalcAmp(EvtParticle *parent, 
                               EvtAmp& amp,
			       EvtbTosllMSFF   *formFactorsms){ 
                            
    
  // Check the charge conservation in the reaction 
  int charge[4];
  charge[0] = (EvtPDL::chg3(parent->getDaug(0)->getId()))/3;
  charge[1] = (EvtPDL::chg3(parent->getDaug(1)->getId()))/3;
  charge[2] = (EvtPDL::chg3(parent->getDaug(2)->getId()))/3;
  charge[3] = (EvtPDL::chg3(parent->getDaug(3)->getId()))/3;
  if(abs(charge[0]+charge[1]+charge[2]+charge[3])!=1){
     EvtGenReport(EVTGEN_ERROR,"EvtGen") 
            << "\n\n The function EvtB2MuMuMuNuAmp::CalcAmp(...)"
            << "\n Error in the daughters charge definition!"
            << "\n charge1 = " << charge[0] 
            << "\t KF code1 = " << EvtPDL::getLundKC(parent->getDaug(0)->getId())
            << "\n charge2 = " << charge[1]
            << "\t KF code2 = " << EvtPDL::getLundKC(parent->getDaug(1)->getId())
            << "\n charge3 = " << charge[2]
            << "\t KF code3 = " << EvtPDL::getLundKC(parent->getDaug(2)->getId())
            << "\n charge4 = " << charge[3]
            << "\t KF code4 = " << EvtPDL::getLundKC(parent->getDaug(3)->getId())
            << "\n number of daughters = " << parent->getNDaug()
            << std::endl;
     ::abort();
  }
  
  
  // Daughter's positions in the matrix element
  int il1, il2, il3, il4;  //   Mu^+(k_1), Mu^-(k_2), \bar Nu_{Mu}(k_3) and Mu^-(k_4)
                           //or Mu^-(k_1), Mu^+(k_2),      Nu_{Mu}(k_3) and Mu^+(k_4)
                           //   This is the "canonical set" for current matrix element.
                           
  il1 = -101; //initialization of the daughter's positions 
  il2 = -102;
  il3 = -103; 
  il4 = -104;                        
  
  // Daughter's positions for the decay 
  //                   B^-(p) -> Mu^+(k_1) Mu^-(k_2) \bar Nu_{Mu}(k_3) Mu^-(k_4).
  if(charge[0]+charge[1]+charge[2]+charge[3] == -1){
     int ll;
     int min_charge, min_charge_il;
     min_charge    = charge[0];
     min_charge_il = 0;
     for(ll = 1; ll < 4; ll++){
        if(min_charge > charge[ll]){ 
               min_charge = charge[ll];
               min_charge_il = ll;
        }              
     }
     il2 = min_charge_il;                        // this is Mu^-(k_2)  
     if(il2 > 2){
       EvtGenReport(EVTGEN_ERROR,"EvtGen") 
            << "\n\n The function EvtB2MuMuMuNuAmp::CalcAmp(...)"
            << "\n Error in the particles distribution!"
            << "\n il2 = "          << il2
            << "\n min_charge = "   << min_charge
            << "\n total charge = -1 = " << (charge[0]+charge[1]+charge[2]+charge[3])
            << std::endl;
       ::abort();
     }       
     for(ll = il2 + 1; ll < 4; ll++){
        if(charge[ll] == -1) il4 = ll;           // this is  Mu^-(k_4)
        }          
     for(ll = 0; ll < 4; ll++){
        if(charge[ll] == +1) il1 = ll;           // this is  Mu^+(k_1)
        }           
     for(ll = 0; ll < 4; ll++){
        if(charge[ll] == 0) il3 = ll;            // this is  \bar Nu_{Mu}(k_3)
        }               
  }

  // Daughter's positions for the decay 
  //                   B^+(p) -> Mu^-(k_1) Mu^+(k_2) Nu_{Mu}(k_3) Mu^+(k_4).
  if(charge[0]+charge[1]+charge[2]+charge[3] == 1){
     int ll;
     int max_charge, max_charge_il;
     max_charge    = charge[0];
     max_charge_il = 0;
     for(ll = 1; ll < 4; ll++){
        if(max_charge < charge[ll]){ 
               max_charge = charge[ll];
               max_charge_il = ll;
        }              
     }
     il2 = max_charge_il;                        // this is Mu^+(k_2)  
     if(il2 > 2){
       EvtGenReport(EVTGEN_ERROR,"EvtGen") 
            << "\n\n The function EvtB2MuMuMuNuAmp::CalcAmp(...)"
            << "\n Error in the particles distribution!"
            << "\n il2 = "          << il2
            << "\n max_charge = "   << max_charge
            << "\n total charge = +1 = " << (charge[0]+charge[1]+charge[2]+charge[3])
            << std::endl;
       ::abort();
     }       
     for(ll = il2 + 1; ll < 4; ll++){
        if(charge[ll] == 1) il4 = ll;            // this is  Mu^+(k_4)
        }          
     for(ll = 0; ll < 4; ll++){
        if(charge[ll] == -1) il1 = ll;           // this is  Mu^-(k_1)
        }           
     for(ll = 0; ll < 4; ll++){
        if(charge[ll] == 0) il3 = ll;            // this is  Nu_{Mu}(k_3)
        }               
  }

  
  if((il1 < 0)||(il2 < 0)|| (il3 < 0) || (il4 < 0)){
     EvtGenReport(EVTGEN_ERROR,"EvtGen") 
            << "\n\n The function EvtB2MuMuMuNuAmp::CalcAmp(...)"
            << "\n ilX < 0 !!!"
            << "\n il1 = " << il1
            << "\t il2 = " << il2
            << "\t il3 = " << il3
            << "\t il4 = " << il4
            << std::endl;
     ::abort();     
  }
  
  // Output for program work check.
  // Need to comment in the final version!
//  EvtGenReport(EVTGEN_ERROR,"EvtGen") 
//            << "\n il1 = " << il1
//            << "\t il2 = " << il2
//            << "\t il3 = " << il3
//            << "\t il4 = " << il4
//            << std::endl;
   //END of the daughter's positions initialization           
            
  
  // Kinematics initialization          
  
  EvtComplex unit1(1.0,0.0); // real unit
  EvtComplex uniti(0.0,1.0); // imaginary unit  

  double M1 = parent->mass();                // B-meson mass, GeV
  double ml = parent->getDaug(il1)->mass();  // mass of muon, GeV
  
  // Id and 4-momentums of the particles
  
  EvtId idparent = parent->getId(); // B-meson Id
  
  EvtVector4R p;                    // B-meson 4-momentum
  p.set(0.0,0.0,0.0,0.0);
  
  
  EvtId id_L1, id_L2, id_L3, id_L4; // leptonic Id
  
  EvtVector4R k_1;  // 4-momentum of mu^+ in the    B^- rest frame; (il1)
  EvtVector4R k_2;  // 4-momentum of mu^- in the    B^- rest frame; (il2) 
  EvtVector4R k_3;  // 4-momentum of \bar nu in the B^- rest frame; (il3)
  EvtVector4R k_4;  // 4-momentum of mu^- in the    B^- rest frame; (il4)

  k_1.set(0.0,0.0,0.0,0.0);
  k_2.set(0.0,0.0,0.0,0.0);
  k_3.set(0.0,0.0,0.0,0.0);
  k_4.set(0.0,0.0,0.0,0.0);

  EvtVector4R q_fierst;     // q = k_1 + k_2 4-momentum in the B-rest frame
  EvtVector4R k_fierst;     // k = k_3 + k_4 4-momentum in the B-rest frame
  double q2_fierst;         // Mandelstam variable s=q^2
  double k2_fierst;         // Mandelstam variable t=k^2

  EvtVector4R q_second;     // q = k_1 + k_4 4-momentum in the B-rest frame
  EvtVector4R k_second;     // k = k_3 + k_2 4-momentum in the B-rest frame
  double q2_second;         // Mandelstam variable s=q^2
  double k2_second;         // Mandelstam variable t=k^2

  p = parent->getP4Restframe();     // B-meson 4-momentum in the B-rest frame

  k_1 = parent->getDaug(il1)->getP4();
  k_2 = parent->getDaug(il2)->getP4();
  k_3 = parent->getDaug(il3)->getP4();
  k_4 = parent->getDaug(il4)->getP4();
  
  q_fierst = k_1 + k_2;
  k_fierst = k_3 + k_4;
  q2_fierst = q_fierst.mass2();
  k2_fierst = k_fierst.mass2(); 
  
  q_second = k_1 + k_4;
  k_second = k_3 + k_2; 
  q2_second = q_second.mass2();
  k2_second = k_second.mass2(); 
  
  
  // For "B^-" - and "B^+" - mesons amplitudeы separately calculations 
  static EvtIdSet bmesons("B-","B_c-");
  static EvtIdSet bbarmesons("B+","B_c+");
  
//  // Information for test of 4-momentum.
//  // Need to comment in the final version!
//  EvtGenReport(EVTGEN_ERROR,"EvtGen") 
//            << "\n 4-momentum initialization  test"
//            << "\n k_1 = " << k_1
//            << "\n k_2 = " << k_2
//            << "\n k_3 = " << k_3
//            << "\n k_4 = " << k_4
//            << "\n q_fierst = " << q_fierst
//            << "\n q_second = " << q_second
//            << "\n k_fierst = " << k_fierst
//            << "\n k_second = " << k_second
//            << std::endl; 

  
  //
  // I. VMD Contribution
  //
  
  double M2[4];   //intermediate vector mesons mass for VMD contribution
  // M2[0] = EvtPDL::getMass(EvtPDL::getId(std::string("rho0")));  // Rho^0
  // M2[1] = EvtPDL::getMass(EvtPDL::getId(std::string("omega"))); // Omega^0
  // M2[2] = M2[0];  // GeV Rho^0
  // M2[3] = M2[1];  // GeV Omega^0
  M2[0] = 0.77526; // GeV Rho^0
  M2[1] = 0.78265; // GeV Omega^0
  M2[2] = M2[0];  // GeV Rho^0
  M2[3] = M2[1];  // GeV Omega^0
  
  double Width2[4]; //intermediate vector mesons width for VMD contribution
  // Width2[0] = EvtPDL::getWidth(EvtPDL::getId(std::string("rho0")));  // Rho^0
  // Width2[1] = EvtPDL::getWidth(EvtPDL::getId(std::string("omega"))); // Omega^0
  // Width2[2] = Width2[0];  // GeV Rho^0
  // Width2[3] = Width2[1];  // GeV Omega^0
  Width2[0] = 0.1491;   // GeV Rho^0
  Width2[1] = 0.00849;  // GeV Omega^0
  Width2[2] = Width2[0];  // GeV Rho^0
  Width2[3] = Width2[1];  // GeV Omega^0
  
  double fV2[4]; //intermediate vector mesons leptonic constant for VMD contribution
                 // see D.Melikhov, N.Nikitin. PRD70, 114028 (2004)
  fV2[0] = 5.04; // GeV Rho^0    
  fV2[1] = 17.1; // GeV Omega^0
  fV2[2] = 5.04; // GeV Rho^0    
  fV2[3] = 17.1; // GeV Omega^0
  
  // For taking the form factors values
  
  // B -> V intermediate vector mesons
  // transition form-factors for VMD contribution
  // 0 -- Rho^0 for k2_fierst
  // 1 -- Omega^0 for k2_fierst
  // 2 -- Rho^0 for k2_second
  // 3 -- Omega^0 for k2_second
  double a1[4],a2[4],a3[4],a0[4],v[4],t1[4],t2[4],t3[4];  
                                         
  
  EvtId B_meson_for_FF     = EvtPDL::getId(std::string("B0"));
  EvtId rho_meson_for_FF   = EvtPDL::getId(std::string("rho0"));
  EvtId omega_meson_for_FF = EvtPDL::getId(std::string("omega"));
  
  formFactorsms->getVectorFF(B_meson_for_FF, rho_meson_for_FF, k2_fierst,
                             a1[0],a2[0],a0[0],v[0],t1[0],t2[0],t3[0]);
                             
  formFactorsms->getVectorFF(B_meson_for_FF, omega_meson_for_FF, k2_fierst,
                             a1[1],a2[1],a0[1],v[1],t1[1],t2[1],t3[1]);
                             
  formFactorsms->getVectorFF(B_meson_for_FF, rho_meson_for_FF, k2_second,
                             a1[2],a2[2],a0[2],v[2],t1[2],t2[2],t3[2]);
                             
  formFactorsms->getVectorFF(B_meson_for_FF, omega_meson_for_FF, k2_second,
                             a1[3],a2[3],a0[3],v[3],t1[3],t2[3],t3[3]);
                                
   int aa;
   for(aa = 0; aa < 4; aa++){
          a3[aa] = ((M1 + M2[aa])*a1[aa] - (M1 - M2[aa])*a2[aa])/(2.0*M2[aa]);
       }                         
  
//  // Information for test of VMD contribution.
//  // Need to comment in the final version!
//  EvtGenReport(EVTGEN_ERROR,"EvtGen") 
//            << "\n VMD Contribution test"
//            << "\n M(Bu) = " << M1 << " GeV;"
//            << "\n M(Rho) = " << M2[0] << " GeV;"
//            << "\t M(Omega) = " << M2[1] << " GeV;"
//            << "\t Gamma(Rho) = " << Width2[0] << " Gev;"
//            << "\t Gamma(Omega) = " << Width2[1] << " Gev."
//            << "\n\n a1[0] = " << a1[0]
//            << "\t  a2[0] = " << a2[0]
//            << "\t  a0[0] = " << a0[0]
//            << "\t  a3[0] = " << a3[0]
//            << "\t   v[0] = " <<  v[0]
//            << "\n\n a1[1] = " << a1[1]
//            << "\t  a2[1] = " << a2[1]
//            << "\t  a0[1] = " << a0[1]
//            << "\t  a3[1] = " << a3[1]
//            << "\t   v[1] = " <<  v[1]
//            << "\n\n a1[2] = " << a1[2]
//            << "\t  a2[2] = " << a2[2]
//            << "\t  a0[2] = " << a0[2]
//            << "\t  a3[2] = " << a3[2]
//            << "\t   v[2] = " <<  v[2]
//            << "\n\n a1[3] = " << a1[3]
//            << "\t  a2[3] = " << a2[3]
//            << "\t  a0[3] = " << a0[3]
//            << "\t  a3[3] = " << a3[3]
//            << "\t   v[3] = " <<  v[3]
//            << std::endl; 
  
  
  
  // Tensor structures for VMD contribution
  EvtTensor4C Tvmd_fierst, Tvmd_second; 
  
  
  //
  // II. Electromagnetic foton emission from B-meson contribution 
  //                     (EM contribution)  
  //
  
  // EM form factors
  // 0 -- u-quark emission for q2_fierst
  // 1 -- b-quark emission for q2_fierst
  // 2 -- u-quark emission for q2_second
  // 3 -- b-quark emission for q2_second
  double v_em[4];
  double MBstar = 5.325; // GeV
  
  v_em[0] = 4.0*MBstar*FF_B2BstarGamma_fromU(q2_fierst)/(3.0*(M1 + MBstar));
  v_em[1] = 2.0*MBstar*FF_B2BstarGamma_fromB(q2_fierst)/(3.0*(M1 + MBstar));
  v_em[2] = 4.0*MBstar*FF_B2BstarGamma_fromU(q2_second)/(3.0*(M1 + MBstar));
  v_em[3] = 2.0*MBstar*FF_B2BstarGamma_fromB(q2_second)/(3.0*(M1 + MBstar));
  
  // Tensor structures for EM contribution
  EvtTensor4C Tem_fierst, Tem_second; 
  
  
  //
  // ***
  //
  
  
  
  // Leptonic currents 
  //   L1 = (\bar mu \gamma^{\mu} (1 - \gamma^5) nu) 
  //                             or (\bar nu \gamma^{\mu} (1 - \gamma^5) mu)
  //   L2 = (\bar mu \gamma^{\nu} mu)
  EvtVector4C L1_fierst, L2_fierst;
  EvtVector4C L1_second, L2_second;

  
  int i1, i2, i3, i4;  // leptonic spin structures counters
  int leptonicspin[4]; // array for the saving of the leptonic spin configuration



  // ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // +                Contribution for B- decay               +
  // ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if (bmesons.contains(idparent)){
  
  Tvmd_fierst = 0.0*EvtTensor4C::g();  
  for(aa = 0; aa < 2; aa++){
      EvtComplex coeff_vmd;
      
      double Re_vmd, Im_vmd, znam_vmd;
      Re_vmd = q2_fierst - M2[aa]*M2[aa];
      Im_vmd = Width2[aa]*M2[aa];
      znam_vmd = fV2[aa]*(Re_vmd*Re_vmd + Im_vmd*Im_vmd);
      coeff_vmd = unit1*Re_vmd/znam_vmd - uniti*Im_vmd/znam_vmd;
      
      double vaa, a2aa, a3aa;
      vaa  = 2.0*v[aa]/(M1 + M2[aa]);
      a2aa = a2[aa]/(M1 + M2[aa]);
      a3aa = 2.0*M2[aa]*(a3[aa] - a0[aa])/k2_fierst;
           
      Tvmd_fierst = Tvmd_fierst 
        +coeff_vmd*(0.0*EvtTensor4C::g()
        -(unit1*vaa*dual(EvtGenFunctions::directProd(p,q_fierst)))
        -uniti*a1[aa]*(M1 + M2[aa])*EvtTensor4C::g()
        +uniti*a2aa*EvtGenFunctions::directProd((p + q_fierst),p)
        +uniti*a3aa*EvtGenFunctions::directProd((p - q_fierst),p));
      }
 
  Tvmd_second = 0.0*EvtTensor4C::g();
  for(aa = 2; aa < 4; aa++){
      EvtComplex coeff_vmd;
      
      double Re_vmd, Im_vmd, znam_vmd;
      Re_vmd = q2_second - M2[aa]*M2[aa];
      Im_vmd = Width2[aa]*M2[aa];
      znam_vmd = fV2[aa]*(Re_vmd*Re_vmd + Im_vmd*Im_vmd);
      coeff_vmd = unit1*Re_vmd/znam_vmd - uniti*Im_vmd/znam_vmd;
      
      double vaa, a2aa, a3aa;
      vaa  = 2.0*v[aa]/(M1 + M2[aa]);
      a2aa = a2[aa]/(M1 + M2[aa]);
      a3aa = 2.0*M2[aa]*(a3[aa] - a0[aa])/k2_second;
      
      Tvmd_second = Tvmd_second  
        +coeff_vmd*(0.0*EvtTensor4C::g()
        -unit1*vaa*dual(EvtGenFunctions::directProd(p,q_second))
        -uniti*a1[aa]*(M1 + M2[aa])*EvtTensor4C::g()
        +uniti*a2aa*EvtGenFunctions::directProd((p + q_second),p)
        +uniti*a3aa*EvtGenFunctions::directProd((p - q_second),p));
      }
      
  Tem_fierst = 0.0*EvtTensor4C::g(); 
  for(aa = 0; aa < 2; aa++){
      double fBstar = 0.2; // GeV
      double coeff;
      coeff = fBstar/(q2_fierst*(k2_fierst - MBstar*MBstar));
      Tem_fierst = Tem_fierst - coeff*v_em[aa]*dual(EvtGenFunctions::directProd(p,k_fierst)); 
     }  
     
  Tem_second = 0.0*EvtTensor4C::g(); 
  for(aa = 2; aa < 4; aa++){
      double fBstar = 0.2; // GeV
      double coeff;
      coeff = fBstar/(q2_fierst*(k2_fierst - MBstar*MBstar));
      Tem_second = Tem_second - coeff*v_em[aa]*dual(EvtGenFunctions::directProd(p,k_second)); 
     }     
      
  // Amplitude calculation
  for(i2=0;i2<2;i2++){
    leptonicspin[0] = i2;
    for(i1=0;i1<2;i1++){
      leptonicspin[1] = i1;
      for(i4=0;i4<2;i4++){
        leptonicspin[2] = i4;
        i3 = 0;                  // neutrino spin structure!
        leptonicspin[3] = i3;
        
        //(\bar mu(k_4) \gamma^{\mu} (1 - \gamma^5) nu(- k_3))
        L1_fierst = EvtLeptonVACurrent(parent->getDaug(il4)->spParent(i4),
                                       parent->getDaug(il3)->spParentNeutrino());
        
        
        //(\bar mu(k_2) \gamma^{\mu} mu(- k_1))
        L2_fierst = EvtLeptonVCurrent(parent->getDaug(il2)->spParent(i2),
                                       parent->getDaug(il1)->spParent(i1));
        
        
        //(\bar mu(k_2) \gamma^{\mu} (1 - \gamma^5) nu(- k_3))
        L1_second = EvtLeptonVACurrent(parent->getDaug(il2)->spParent(i2),
                                       parent->getDaug(il3)->spParentNeutrino());
        
        
        //(\bar mu(k_4) \gamma^{\mu} mu(- k_1))
        L2_second = EvtLeptonVCurrent(parent->getDaug(il4)->spParent(i4),
                                       parent->getDaug(il1)->spParent(i1));
         
        // VMD                               
        EvtVector4C Evmd_fierst, Evmd_second;
        Evmd_fierst=Tvmd_fierst.cont2(L2_fierst);
        Evmd_second=Tvmd_second.cont2(L2_second);
        
        // EM
        EvtVector4C Eem_fierst, Eem_second;
        Eem_fierst=Tem_fierst.cont2(L1_fierst);
        Eem_second=Tem_second.cont2(L1_second);
           
        // Bremsstrahlung   
        EvtComplex Ebrst_fierst, Ebrst_second;
        double fB = 0.20; // GeV leptonic constant of B^{\pm} mesons
        Ebrst_fierst = uniti*fB*L2_fierst*L1_fierst/q2_fierst;
        Ebrst_second = uniti*fB*L2_second*L1_second/q2_second;
        
        amp.vertex(leptonicspin, L1_fierst*Evmd_fierst + L2_fierst*Eem_fierst + Ebrst_fierst 
                               - L1_second*Evmd_second - L2_second*Eem_second - Ebrst_second);
                               
         if(q2_fierst < 4.0*ml*ml)
            {
               amp.vertex(leptonicspin, unit1*0.0);
            }                      
          if(q2_second < 4.0*ml*ml)
            {
               amp.vertex(leptonicspin, unit1*0.0);
            }  
           if(k2_fierst < ml*ml)
            {
               amp.vertex(leptonicspin, unit1*0.0);
            }                      
          if(k2_second < ml*ml)
            {
               amp.vertex(leptonicspin, unit1*0.0);
            }                                                                    
          
        }  // End of the operator "for(i4=0;i4<2;i4++)"
      }    // End of the operator "for(i1=0;i1<2;i1++)"
    }      // End of the operator "for(i2=0;i2<2;i2++)"
      
   }  // End of the operator: "if (bmesons.contains(idparent))"

   
   else { // Start of the operator "else N1" 
  // ++++++++++++++++++++++++++++++++++++++++++++++++++
  // +           Contribution for B+ decay            +
  // ++++++++++++++++++++++++++++++++++++++++++++++++++
  if (bbarmesons.contains(idparent)){
  
  Tvmd_fierst = 0.0*EvtTensor4C::g();  
  for(aa = 0; aa < 2; aa++){
      EvtComplex coeff_vmd;
      
      double Re_vmd, Im_vmd, znam_vmd;
      Re_vmd = q2_fierst - M2[aa]*M2[aa];
      Im_vmd = Width2[aa]*M2[aa];
      znam_vmd = fV2[aa]*(Re_vmd*Re_vmd + Im_vmd*Im_vmd);
      coeff_vmd = unit1*Re_vmd/znam_vmd - uniti*Im_vmd/znam_vmd;
      
      double vaa, a2aa, a3aa;
      vaa  = 2.0*v[aa]/(M1 + M2[aa]);
      a2aa = a2[aa]/(M1 + M2[aa]);
      a3aa = 2.0*M2[aa]*(a3[aa] - a0[aa])/k2_fierst;
            
      Tvmd_fierst = Tvmd_fierst 
        +coeff_vmd*(0.0*EvtTensor4C::g()
        +unit1*vaa*dual(EvtGenFunctions::directProd(p,q_fierst))
        -uniti*a1[aa]*(M1 + M2[aa])*EvtTensor4C::g()
        +uniti*a2aa*EvtGenFunctions::directProd((p + q_fierst),p)
        +uniti*a3aa*EvtGenFunctions::directProd((p - q_fierst),p));
      }
 
  Tvmd_second = 0.0*EvtTensor4C::g();
  for(aa = 2; aa < 4; aa++){
      EvtComplex coeff_vmd;
      
      double Re_vmd, Im_vmd, znam_vmd;
      Re_vmd = q2_second - M2[aa]*M2[aa];
      Im_vmd = Width2[aa]*M2[aa];
      znam_vmd = fV2[aa]*(Re_vmd*Re_vmd + Im_vmd*Im_vmd);
      coeff_vmd = unit1*Re_vmd/znam_vmd - uniti*Im_vmd/znam_vmd;
      
      double vaa, a2aa, a3aa;
      vaa  = 2.0*v[aa]/(M1 + M2[aa]);
      a2aa = a2[aa]/(M1 + M2[aa]);
      a3aa = 2.0*M2[aa]*(a3[aa] - a0[aa])/k2_second;
   
      Tvmd_second = Tvmd_second  
        +coeff_vmd*(0.0*EvtTensor4C::g()
        +unit1*vaa*dual(EvtGenFunctions::directProd(p,q_second))
        -uniti*a1[aa]*(M1 + M2[aa])*EvtTensor4C::g()
        +uniti*a2aa*EvtGenFunctions::directProd((p + q_second),p)
        +uniti*a3aa*EvtGenFunctions::directProd((p - q_second),p));
      }
      
  Tem_fierst = 0.0*EvtTensor4C::g(); 
  for(aa = 0; aa < 2; aa++){
      double fBstar = 0.0 - 0.2; // GeV
      double coeff;
      coeff = fBstar/(q2_fierst*(k2_fierst - MBstar*MBstar));
      Tem_fierst = Tem_fierst + coeff*v_em[aa]*dual(EvtGenFunctions::directProd(p,k_fierst)); 
     }  
     
  Tem_second = 0.0*EvtTensor4C::g(); 
  for(aa = 2; aa < 4; aa++){
      double fBstar = 0.0 - 0.2; // GeV
      double coeff;
      coeff = fBstar/(q2_fierst*(k2_fierst - MBstar*MBstar));
      Tem_second = Tem_second + coeff*v_em[aa]*dual(EvtGenFunctions::directProd(p,k_second)); 
     }        
      
  // Amplitude calculation
  for(i2=0;i2<2;i2++){
    leptonicspin[0] = i2;
    for(i1=0;i1<2;i1++){
      leptonicspin[1] = i1;
      for(i4=0;i4<2;i4++){
        leptonicspin[2] = i4;
        i3 = 0;                  // neutrino spin structure!
        leptonicspin[3] = i3;
        
        //(\bar nu(k_3) \gamma^{\mu} (1 - \gamma^5) mu(- k_4))
        L1_fierst = EvtLeptonVACurrent(parent->getDaug(il3)->spParentNeutrino(),
                                       parent->getDaug(il4)->spParent(i4));
        
        
        //(\bar mu(k_1) \gamma^{\mu} mu(- k_2))
        L2_fierst = EvtLeptonVCurrent(parent->getDaug(il1)->spParent(i1),
                                       parent->getDaug(il2)->spParent(i2));
        
        
        //(\bar nu(k_3) \gamma^{\mu} (1 - \gamma^5) mu(- k_2))
        L1_second = EvtLeptonVACurrent(parent->getDaug(il3)->spParentNeutrino(),
                                       parent->getDaug(il2)->spParent(i2));
        
        
        //(\bar mu(k_1) \gamma^{\mu} mu(- k_4))
        L2_second = EvtLeptonVCurrent(parent->getDaug(il1)->spParent(i1),
                                       parent->getDaug(il4)->spParent(i4));
         
        // VMD        
        EvtVector4C Evmd_fierst, Evmd_second;
        Evmd_fierst=Tvmd_fierst.cont2(L2_fierst);
        Evmd_second=Tvmd_second.cont2(L2_second);
        
        // EM
        EvtVector4C Eem_fierst, Eem_second;
        Eem_fierst=Tem_fierst.cont2(L1_fierst);
        Eem_second=Tem_second.cont2(L1_second);
           
        // Bremsstrahlung                                                               
        EvtComplex Ebrst_fierst, Ebrst_second;
        double fB = 0.20; // GeV leptonic constant of B^{\pm} mesons
        Ebrst_fierst = uniti*fB*L2_fierst*L1_fierst/q2_fierst;
        Ebrst_second = uniti*fB*L2_second*L1_second/q2_second;
        
        
        amp.vertex(leptonicspin, L1_fierst*Evmd_fierst + L2_fierst*Eem_fierst + Ebrst_fierst 
                               - L1_second*Evmd_second - L2_second*Eem_second - Ebrst_second);
                               
         if(q2_fierst < 4.0*ml*ml)
            {
               amp.vertex(leptonicspin, unit1*0.0);
            }                      
          if(q2_second < 4.0*ml*ml)
            {
               amp.vertex(leptonicspin, unit1*0.0);
            }  
           if(k2_fierst < ml*ml)
            {
               amp.vertex(leptonicspin, unit1*0.0);
            }                      
          if(k2_second < ml*ml)
            {
               amp.vertex(leptonicspin, unit1*0.0);
            }                          
          
        }  // End of the operator "for(i4=0;i4<2;i4++)"
      }    // End of the operator "for(i1=0;i1<2;i1++)"
    }      // End of the operator "for(i2=0;i2<2;i2++)"
      
   }  // End of the operator: "if (bbarmesons.contains(idparent))"
   
   else{ // Start of the operator "else N2"
         EvtGenReport(EVTGEN_ERROR,"EvtGen") 
             << "\n\n The function EvtB2MuMuMuNuAmp::CalcAmp(...)"
             << "\n Wrong B-meson number"
             << "\n B-meson KF code = " << EvtPDL::getLundKC(parent->getId())
             << std::endl;
         ::abort();    
   }  // End of the operator "else N2" 

   }  // End of the operator "else N1" 
}



//
// The decays B^- -> Mu^+ Mu^- \bar Nu_{Mu} Mu^- maximum probability calculation 
//
double EvtB2MuMuMuNuAmp::CalcMaxProb(){
 
  double maxfoundprob = 60000000.0; // maximum of the probability

  return maxfoundprob;
}


// Triangular function
double EvtB2MuMuMuNuAmp::lambda(double a, double b, double c){
  double l;

  l=pow(a,2.0)+pow(b,2.0)+pow(c,2.0)-2.0*a*b-2.0*a*c-2.0*b*c;
 
  return l;
}


// Electromagnetic FF for B -> B* gamma transition, when gamma emitted from u-quark 
// q2 = q^2 in GeV^2 - transition 4-momentum 
// D.Melikhov privat communication
double EvtB2MuMuMuNuAmp::FF_B2BstarGamma_fromU(double q2){
   double V;
   double MRho = 0.77; // GeV
   double y;
   
   y = q2/(MRho*MRho);
   V = 10.036/((1.0 - y)*(1.0 - 0.09*y + 0.04*y*y));
    
   return V;
}


// Electromagnetic FF for B -> B* gamma transition, when gamma emitted from b-quark 
// q2 = q^2 in GeV^2 - transition 4-momentum 
// D.Melikhov privat communication
double EvtB2MuMuMuNuAmp::FF_B2BstarGamma_fromB(double q2){
   double V;
   double MUpsilon = 8.50; // GeV
   double y;
   
   y = q2/(MUpsilon*MUpsilon);
   V = 1.0461/((1.0 - y)*(1.0 - 0.79*y - 0.07*y*y));
    
   return V;
}
