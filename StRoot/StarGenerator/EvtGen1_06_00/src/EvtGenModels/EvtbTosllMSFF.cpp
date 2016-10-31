//--------------------------------------------------------------------------
//
// Environment:
//      This software is part of the EvtGen package developed jointly
//      for the BaBar and CLEO collaborations.  If you use all or part
//      of it, please give an appropriate acknowledgement.
//
// Copyright Information: See EvtGen/COPYRIGHT
//      Copyright (C) 2000      Caltech, UCSB
//
// Module:      EvtbTosllMSFF.cpp
// Description: Form factors for B -> (P, V) ell^+ ell^- transitions according 
//              to the paper: D.Melikhov, B.Stech, PRD62, 014006 (2000).
//
// Modification history:
//
//  N.Nikitin (nnikit@mail.cern.ch)  March 13, 2008   Module created
//  N.Nikitin (nnikit@mail.cern.ch)  March 27, 2008   add \bar B -> \bar (K,K^*) transition ff
//  N.Nikitin (nnikit@mail.cern.ch)  April 26, 2008   add \bar Bs -> phi transition ff
//  N.Nikitin (nnikit@mail.cern.ch)  April 26, 2008   add \bar Bs -> K*  transition ff
//  N.Nikitin (nnikit@mail.cern.ch)  April 27, 2008   add \bar B -> \bar rho transition ff
//  N.Nikitin (nnikit@mail.cern.ch)  Nvmbr 04, 2011   add \bar B -> omega transition ff
//  N.Nikitin (nnikit@mail.cern.ch)  Dec   16, 2011   add \bar B -> \bar K_1(1270) transition ff (from H.Hatanaka and Kwei-Chou Yang, PRD78, 074007 (2008))
//  N.Nikitin (nnikit@mail.cern.ch)  Dec   16, 2011   add \bar B -> \bar K_1(1400) transition ff (from H.Hatanaka and Kwei-Chou Yang, PRD78, 074007 (2008))
//  N.Nikitin (nnikit@mail.cern.ch)  May   11, 2012   add \bar Bs -> f_0(980) transition ff with NLO corrections from Table II (see P.Colangelo et al., PRD81, 074001 (2010))
//
//------------------------------------------------------------------------

#include "EvtGenBase/EvtPatches.hh"
#include "EvtGenBase/EvtReport.hh"
#include "EvtGenBase/EvtComplex.hh"
#include "EvtGenBase/EvtPDL.hh"
#include "EvtGenModels/EvtbTosllMSFF.hh"
#include <cmath>
#include <cstdlib>

EvtbTosllMSFF::EvtbTosllMSFF(){}

double EvtbTosllMSFF::equation9_10(double ff0, double M2, double q2, 
                                double sigma1, double sigma2, int eq_num)
         {
           double ff;

           ff=1.0;
           
           switch(eq_num)
             {
               case 9:
                 ff=1./(1.-q2/M2);
               case 10:
                 ff=ff*ff0/(1.-sigma1*q2/M2+sigma2*pow(q2,2)/pow(M2,2));
                 break;
               default:
                 EvtGenReport(EVTGEN_ERROR,"EvtGen") << 
                   "In the function EvtbTosllMSFF::equation9_10   \n" <<
                   "the parameter eq_num non equal to the 9 or 10! \n" <<
                   "eq_num =" << eq_num <<std::endl;
                 ::abort();
             }

           return ff;
         }



void EvtbTosllMSFF::getScalarFF(EvtId parent, EvtId daught,
                                 double  t, double& fp,double& f0, double& ft){

  int models_counter=0; // counter of the accepted models

  // B -> K transition form factors
  if((parent == EvtPDL::getId(std::string("B+"))&&
      daught == EvtPDL::getId(std::string("K+")))||
     (parent == EvtPDL::getId(std::string("B-"))&&
      daught == EvtPDL::getId(std::string("K-")))||
     (parent == EvtPDL::getId(std::string("B0"))&&
      daught == EvtPDL::getId(std::string("K0")))||
     (parent == EvtPDL::getId(std::string("anti-B0"))&&
      daught == EvtPDL::getId(std::string("anti-K0")))||
     (parent == EvtPDL::getId(std::string("B0"))&&
      daught == EvtPDL::getId(std::string("K_S0")))||
     (parent == EvtPDL::getId(std::string("anti-B0"))&&
      daught == EvtPDL::getId(std::string("K_S0")))||
     (parent == EvtPDL::getId(std::string("B0"))&&
      daught == EvtPDL::getId(std::string("K_L0")))||
     (parent == EvtPDL::getId(std::string("anti-B0"))&&
      daught == EvtPDL::getId(std::string("K_L0")))){
     double ff0[]   ={0.36, 0.36, 0.35};
     double sigma1[]={0.43, 0.70, 0.43};
     double sigma2[]={0.00, 0.27, 0.00};
     int    eq_num[]={   9,   10,    9};
     double M_P2=5.37*5.37; // GeV^2 for B^0_s - meson
     double M_V2=5.42*5.42; // GeV^2 for B^*_s - meson

     fp = equation9_10(ff0[0], M_P2, t, sigma1[0], sigma2[0], eq_num[0]);
     f0 = equation9_10(ff0[1], M_V2, t, sigma1[1], sigma2[1], eq_num[1]);
     ft = equation9_10(ff0[2], M_P2, t, sigma1[2], sigma2[2], eq_num[2]);

     models_counter=models_counter+1;

//     EvtGenReport(EVTGEN_NOTICE,"EvtGen") <<"\n The function  EvtbTosllMSFF::getVectorFF(...) passed."
//     << "\n B -> K transition form factors"
//     << std::endl;          
  }

  // B -> \pi transition form factors
  if((parent == EvtPDL::getId(std::string("B+"))&&
      daught == EvtPDL::getId(std::string("pi+")))||
     (parent == EvtPDL::getId(std::string("B-"))&&
      daught == EvtPDL::getId(std::string("pi-")))||
     (parent == EvtPDL::getId(std::string("B0"))&&
      daught == EvtPDL::getId(std::string("pi0")))||
     (parent == EvtPDL::getId(std::string("anti-B0"))&&
      daught == EvtPDL::getId(std::string("pi0")))){
     double ff0[]   ={0.29, 0.29, 0.28};
     double sigma1[]={0.48, 0.76, 0.48};
     double sigma2[]={0.00, 0.28, 0.00};
     int    eq_num[]={   9,   10,    9};
     double M_P2=5.27*5.27; // GeV^2 for B^0 - meson
     double M_V2=5.32*5.32; // GeV^2 for B^* - meson

     fp = equation9_10(ff0[0], M_P2, t, sigma1[0], sigma2[0], eq_num[0]);
     f0 = equation9_10(ff0[1], M_V2, t, sigma1[1], sigma2[1], eq_num[1]);
     ft = equation9_10(ff0[2], M_P2, t, sigma1[2], sigma2[2], eq_num[2]);

     models_counter=models_counter+1;

//     EvtGenReport(EVTGEN_NOTICE,"EvtGen") <<"\n The function  EvtbTosllMSFF::getVectorFF(...) passed."
//     << "\n B -> pi transition form factors"
//     << std::endl;          
  }

  // B_d -> \eta transition form factors
  if((parent == EvtPDL::getId(std::string("B0"))&&
      daught == EvtPDL::getId(std::string("eta")))||
     (parent == EvtPDL::getId(std::string("anti-B0"))&&
      daught == EvtPDL::getId(std::string("eta")))){
     double ff0[]   ={0.36, 0.36, 0.36};
     double sigma1[]={0.60, 0.80, 0.58};
     double sigma2[]={0.20, 0.40, 0.18};
     int    eq_num[]={   9,   10,    9};
     double M_P2=5.27*5.27; // GeV^2 for B_d^0 - meson
     double M_V2=5.32*5.32; // GeV^2 for B_d^* - meson

     fp = equation9_10(ff0[0], M_P2, t, sigma1[0], sigma2[0], eq_num[0]);
     fp = -0.5*fp;
     f0 = equation9_10(ff0[1], M_V2, t, sigma1[1], sigma2[1], eq_num[1]);
     f0 = -0.5*f0;
     ft = equation9_10(ff0[2], M_P2, t, sigma1[2], sigma2[2], eq_num[2]);
     ft = -0.5*ft;

     models_counter=models_counter+1;

//     EvtGenReport(EVTGEN_NOTICE,"EvtGen") <<"\n The function  EvtbTosllMSFF::getVectorFF(...) passed."
//     << "\n Bd -> eta transition form factors"
//     << std::endl;          
  }


  // B_d -> \eta' transition form factors
  if((parent == EvtPDL::getId(std::string("B0"))&&
      daught == EvtPDL::getId(std::string("eta'")))||
     (parent == EvtPDL::getId(std::string("anti-B0"))&&
      daught == EvtPDL::getId(std::string("eta'")))){
     double ff0[]   ={0.36, 0.36, 0.39};
     double sigma1[]={0.60, 0.80, 0.58};
     double sigma2[]={0.20, 0.45, 0.18};
     int    eq_num[]={   9,   10,    9};
     double M_P2=5.27*5.27; // GeV^2 for B_d^0 - meson
     double M_V2=5.32*5.32; // GeV^2 for B_d^* - meson

     fp = equation9_10(ff0[0], M_P2, t, sigma1[0], sigma2[0], eq_num[0]);
     f0 = equation9_10(ff0[1], M_V2, t, sigma1[1], sigma2[1], eq_num[1]);
     ft = equation9_10(ff0[2], M_P2, t, sigma1[2], sigma2[2], eq_num[2]);

     models_counter=models_counter+1;

//     EvtGenReport(EVTGEN_NOTICE,"EvtGen") <<"\n The function  EvtbTosllMSFF::getVectorFF(...) passed."
//     << "\n Bd -> eta transition form factors"
//     << std::endl;          
  }


  // B_s -> \eta transition form factors
  if((parent == EvtPDL::getId(std::string("B_s0"))&&
      daught == EvtPDL::getId(std::string("eta")))||
     (parent == EvtPDL::getId(std::string("anti-B_s0"))&&
      daught == EvtPDL::getId(std::string("eta")))){
     double ff0[]   ={0.36, 0.36, 0.36};
     double sigma1[]={0.60, 0.80, 0.58};
     double sigma2[]={0.20, 0.40, 0.18};
     int    eq_num[]={   9,   10,    9};
     double M_P2=5.37*5.37; // GeV^2 for B_s^0 - meson
     double M_V2=5.42*5.42; // GeV^2 for B_s^* - meson

     fp = equation9_10(ff0[0], M_P2, t, sigma1[0], sigma2[0], eq_num[0]);
     f0 = equation9_10(ff0[1], M_V2, t, sigma1[1], sigma2[1], eq_num[1]);
     ft = equation9_10(ff0[2], M_P2, t, sigma1[2], sigma2[2], eq_num[2]);

     models_counter=models_counter+1;

//     EvtGenReport(EVTGEN_NOTICE,"EvtGen") <<"\n The function  EvtbTosllMSFF::getVectorFF(...) passed."
//     << "\n Bs -> eta transition form factors"
//     << std::endl;          
  }


  // B_s -> \eta' transition form factors
  if((parent == EvtPDL::getId(std::string("B_s0"))&&
      daught == EvtPDL::getId(std::string("eta'")))||
     (parent == EvtPDL::getId(std::string("anti-B_s0"))&&
      daught == EvtPDL::getId(std::string("eta'")))){
     double ff0[]   ={0.36, 0.36, 0.39};
     double sigma1[]={0.60, 0.80, 0.58};
     double sigma2[]={0.20, 0.45, 0.18};
     int    eq_num[]={   9,   10,    9};
     double M_P2=5.37*5.37; // GeV^2 for B_s^0 - meson
     double M_V2=5.42*5.42; // GeV^2 for B_s^* - meson

     fp = equation9_10(ff0[0], M_P2, t, sigma1[0], sigma2[0], eq_num[0]);
     f0 = equation9_10(ff0[1], M_V2, t, sigma1[1], sigma2[1], eq_num[1]);
     ft = equation9_10(ff0[2], M_P2, t, sigma1[2], sigma2[2], eq_num[2]);

     models_counter=models_counter+1;

//     EvtGenReport(EVTGEN_NOTICE,"EvtGen") <<"\n The function  EvtbTosllMSFF::getVectorFF(...) passed."
//     << "\n Bs -> eta transition form factors"
//     << std::endl;          
  }


  // B_s -> f_0(980) transition form factors
  if((parent == EvtPDL::getId(std::string("B_s0"))&&
      daught == EvtPDL::getId(std::string("f_0")))||
     (parent == EvtPDL::getId(std::string("anti-B_s0"))&&
      daught == EvtPDL::getId(std::string("f_0")))){
     double ff0[]   ={0.238, 0.238, 0.308};
     double sigma1[]={ 1.50,  0.53,  1.46};
     double sigma2[]={ 0.58, -0.36,  0.58};
     int    eq_num[]={   10,    10,    10};
     double M_P2=5.366*5.366;   // GeV^2 for B_s^0 - meson

     fp = 0.0 - equation9_10(ff0[0], M_P2, t, sigma1[0], sigma2[0], eq_num[0]);
     f0 = 0.0 - equation9_10(ff0[1], M_P2, t, sigma1[1], sigma2[1], eq_num[1]);
     ft = equation9_10(ff0[2], M_P2, t, sigma1[2], sigma2[2], eq_num[2]);

     models_counter=models_counter+1;

//     EvtGenReport(EVTGEN_NOTICE,"EvtGen") <<"\n The function  EvtbTosllMSFF::getVectorFF(...) passed."
//     << "\n Bs -> eta transition form factors"
//     << std::endl;          
  }


//  EvtGenReport(EVTGEN_NOTICE,"EvtGen") << "\n models_counter  = " << models_counter
//      << "\n Scalar form-factors at q^2 = " << t 
//      << "   for B -> P transition:"
//      << "\n fp = " << fp
//      << "\n f0 = " << f0 
//      << "\n ft = " << ft << std::endl;


  if(models_counter!=1){
      EvtGenReport(EVTGEN_ERROR,"EvtGen") << 
        "\n In the function EvtbTosllMSFF::getScalarFF(...) \n" <<
        "the parameter models_counter not equal 1! \n" <<
        "models_counter = " << models_counter <<std::endl;
      ::abort();
  }
  
}





void EvtbTosllMSFF::getVectorFF(EvtId parent, EvtId daught, double t, 
                                 double& a1,double& a2,double& a0, double& v,
                                 double& t1, double& t2, double& t3 ){

  int models_counter=0; // counter of the accepted models
  double thetaK = -34.0 * 3.14159 / 180; // K_1(1270) - K_1(1400) mixing angle


  // \bar B -> \bar K* transition form factors
  if((parent == EvtPDL::getId(std::string("B+"))&&
      daught == EvtPDL::getId(std::string("K*+")))||
     (parent == EvtPDL::getId(std::string("B-"))&&
      daught == EvtPDL::getId(std::string("K*-")))||
     (parent == EvtPDL::getId(std::string("B0"))&&
      daught == EvtPDL::getId(std::string("K*0")))||
     (parent == EvtPDL::getId(std::string("anti-B0"))&&
      daught == EvtPDL::getId(std::string("anti-K*0")))){
     double ff0[]   ={0.44, 0.45, 0.36, 0.32, 0.39, 0.39, 0.27};
     double sigma1[]={0.45, 0.46, 0.64, 1.23, 0.45, 0.72, 1.31};
     double sigma2[]={0.00, 0.00, 0.36, 0.38, 0.00, 0.62, 0.41};
     int    eq_num[]={   9,    9,   10,   10,    9,   10,   10};
     double M_P2=5.37*5.37; // GeV^2 for B^0_s - meson
     double M_V2=5.42*5.42; // GeV^2 for B^*_s - meson
  
     v =equation9_10(ff0[0], M_P2, t, sigma1[0], sigma2[0], eq_num[0]);
     a0=equation9_10(ff0[1], M_P2, t, sigma1[1], sigma2[1], eq_num[1]);
     a1=equation9_10(ff0[2], M_V2, t, sigma1[2], sigma2[2], eq_num[2]);
     a2=equation9_10(ff0[3], M_V2, t, sigma1[3], sigma2[3], eq_num[3]);
    
     t1=equation9_10(ff0[4], M_P2, t, sigma1[4], sigma2[4], eq_num[4]);
     t2=equation9_10(ff0[5], M_V2, t, sigma1[5], sigma2[5], eq_num[5]);
     t3=equation9_10(ff0[6], M_V2, t, sigma1[6], sigma2[6], eq_num[6]);

     models_counter=models_counter+1;

//     EvtGenReport(EVTGEN_NOTICE,"EvtGen") <<"\n The function  EvtbTosllMSFF::getVectorFF(...) passed."
//     << "\n barB -> barK* transition form factors"
//     << std::endl;     
  }


  // \bar B -> \bar\rho transition form factors
  if((parent == EvtPDL::getId(std::string("B+"))&&
      daught == EvtPDL::getId(std::string("rho+")))||
     (parent == EvtPDL::getId(std::string("B-"))&&
      daught == EvtPDL::getId(std::string("rho-")))||
     (parent == EvtPDL::getId(std::string("B0"))&&
      daught == EvtPDL::getId(std::string("rho0")))||
     (parent == EvtPDL::getId(std::string("anti-B0"))&&
      daught == EvtPDL::getId(std::string("rho0")))){
     double ff0[]   ={0.31, 0.30, 0.26, 0.24, 0.27, 0.27, 0.19};
     double sigma1[]={0.59, 0.54, 0.73, 1.40, 0.60, 0.74, 1.42};
     double sigma2[]={0.00, 0.00, 0.10, 0.50, 0.00, 0.19, 0.51};
     int    eq_num[]={   9,    9,   10,   10,    9,   10,   10};
     double M_P2=5.27*5.27; // GeV^2 for B   - meson
     double M_V2=5.32*5.32; // GeV^2 for B^* - meson
  
     v =equation9_10(ff0[0], M_P2, t, sigma1[0], sigma2[0], eq_num[0]);
     a0=equation9_10(ff0[1], M_P2, t, sigma1[1], sigma2[1], eq_num[1]);
     a1=equation9_10(ff0[2], M_V2, t, sigma1[2], sigma2[2], eq_num[2]);
     a2=equation9_10(ff0[3], M_V2, t, sigma1[3], sigma2[3], eq_num[3]);
    
     t1=equation9_10(ff0[4], M_P2, t, sigma1[4], sigma2[4], eq_num[4]);
     t2=equation9_10(ff0[5], M_V2, t, sigma1[5], sigma2[5], eq_num[5]);
     t3=equation9_10(ff0[6], M_V2, t, sigma1[6], sigma2[6], eq_num[6]);

     models_counter=models_counter+1;

//     EvtGenReport(EVTGEN_NOTICE,"EvtGen") <<"\n The function  EvtbTosllMSFF::getVectorFF(...) passed."
//     << "\n barB -> bar rho transition form factors"
//     << std::endl;     
  }


  // \bar B -> \omega transition form factors (exactly as for \bar B -> \rho^0 ff!)
  if((parent == EvtPDL::getId(std::string("B0"))&&
      daught == EvtPDL::getId(std::string("omega")))||
     (parent == EvtPDL::getId(std::string("anti-B0"))&&
      daught == EvtPDL::getId(std::string("omega")))){
     double ff0[]   ={0.31, 0.30, 0.26, 0.24, 0.27, 0.27, 0.19};
     double sigma1[]={0.59, 0.54, 0.73, 1.40, 0.60, 0.74, 1.42};
     double sigma2[]={0.00, 0.00, 0.10, 0.50, 0.00, 0.19, 0.51};
     int    eq_num[]={   9,    9,   10,   10,    9,   10,   10};
     double M_P2=5.27*5.27; // GeV^2 for B   - meson
     double M_V2=5.32*5.32; // GeV^2 for B^* - meson
  
     v =equation9_10(ff0[0], M_P2, t, sigma1[0], sigma2[0], eq_num[0]);
     a0=equation9_10(ff0[1], M_P2, t, sigma1[1], sigma2[1], eq_num[1]);
     a1=equation9_10(ff0[2], M_V2, t, sigma1[2], sigma2[2], eq_num[2]);
     a2=equation9_10(ff0[3], M_V2, t, sigma1[3], sigma2[3], eq_num[3]);
    
     t1=equation9_10(ff0[4], M_P2, t, sigma1[4], sigma2[4], eq_num[4]);
     t2=equation9_10(ff0[5], M_V2, t, sigma1[5], sigma2[5], eq_num[5]);
     t3=equation9_10(ff0[6], M_V2, t, sigma1[6], sigma2[6], eq_num[6]);

     models_counter=models_counter+1;

//     EvtGenReport(EVTGEN_NOTICE,"EvtGen") <<"\n The function  EvtbTosllMSFF::getVectorFF(...) passed."
//     << "\n barB -> omega transition form factors"
//     << std::endl;     
  }


  // \bar Bs -> phi transition form factors
  if((parent == EvtPDL::getId(std::string("B_s0"))&&
      daught == EvtPDL::getId(std::string("phi")))||
     (parent == EvtPDL::getId(std::string("anti-B_s0"))&&
      daught == EvtPDL::getId(std::string("phi")))){
     double ff0[]   ={0.44, 0.42, 0.34, 0.31, 0.38, 0.38, 0.26};
     double sigma1[]={0.62, 0.55, 0.73, 1.30, 0.62, 0.83, 1.41};
     double sigma2[]={0.20, 0.12, 0.42, 0.52, 0.20, 0.71, 0.57};
     int    eq_num[]={   9,    9,   10,   10,    9,   10,   10};
     double M_P2=5.37*5.37; // GeV^2 for B^0_s - meson
     double M_V2=5.42*5.42; // GeV^2 for B^*_s - meson
  
     v =equation9_10(ff0[0], M_P2, t, sigma1[0], sigma2[0], eq_num[0]);
     a0=equation9_10(ff0[1], M_P2, t, sigma1[1], sigma2[1], eq_num[1]);
     a1=equation9_10(ff0[2], M_V2, t, sigma1[2], sigma2[2], eq_num[2]);
     a2=equation9_10(ff0[3], M_V2, t, sigma1[3], sigma2[3], eq_num[3]);
    
     t1=equation9_10(ff0[4], M_P2, t, sigma1[4], sigma2[4], eq_num[4]);
     t2=equation9_10(ff0[5], M_V2, t, sigma1[5], sigma2[5], eq_num[5]);
     t3=equation9_10(ff0[6], M_V2, t, sigma1[6], sigma2[6], eq_num[6]);

     models_counter=models_counter+1;

//     EvtGenReport(EVTGEN_NOTICE,"EvtGen") <<"\n The function  EvtbTosllMSFF::getVectorFF(...) passed."
//     << "\n barBs -> phi transition form factors"
//     << std::endl;     
  }

  // \bar Bs -> K* (without \bar !) transition form factors
  if((parent == EvtPDL::getId(std::string("B_s0"))&&
      daught == EvtPDL::getId(std::string("anti-K*0")))||
     (parent == EvtPDL::getId(std::string("anti-B_s0"))&&
      daught == EvtPDL::getId(std::string("K*0")))){
     double ff0[]   ={0.38, 0.37, 0.29, 0.26, 0.32, 0.32, 0.23};
     double sigma1[]={0.66, 0.60, 0.86, 1.32, 0.66, 0.98, 1.42};
     double sigma2[]={0.30, 0.16, 0.60, 0.54, 0.31, 0.90, 0.62};
     int    eq_num[]={   9,    9,   10,   10,    9,   10,   10};
     double M_P2=5.27*5.27; // GeV^2 for B   - meson
     double M_V2=5.32*5.32; // GeV^2 for B^* - meson
  
     v =equation9_10(ff0[0], M_P2, t, sigma1[0], sigma2[0], eq_num[0]);
     a0=equation9_10(ff0[1], M_P2, t, sigma1[1], sigma2[1], eq_num[1]);
     a1=equation9_10(ff0[2], M_V2, t, sigma1[2], sigma2[2], eq_num[2]);
     a2=equation9_10(ff0[3], M_V2, t, sigma1[3], sigma2[3], eq_num[3]);
    
     t1=equation9_10(ff0[4], M_P2, t, sigma1[4], sigma2[4], eq_num[4]);
     t2=equation9_10(ff0[5], M_V2, t, sigma1[5], sigma2[5], eq_num[5]);
     t3=equation9_10(ff0[6], M_V2, t, sigma1[6], sigma2[6], eq_num[6]);

     models_counter=models_counter+1;

//     EvtGenReport(EVTGEN_NOTICE,"EvtGen") <<"\n The function  EvtbTosllMSFF::getVectorFF(...) passed."
//     << "\n barBs -> K* transition form factors"
//     << std::endl;     
  }


  // \bar B -> \bar K_1(1270) transition form factors
  // See the paper: H.Hatanaka and Kwei-Chou Yang, PRD78, 074007 (2008)
  if((parent == EvtPDL::getId(std::string("B+"))&&
      daught == EvtPDL::getId(std::string("K_1+")))||
     (parent == EvtPDL::getId(std::string("B-"))&&
      daught == EvtPDL::getId(std::string("K_1-")))||
     (parent == EvtPDL::getId(std::string("B0"))&&
      daught == EvtPDL::getId(std::string("K_10")))||
     (parent == EvtPDL::getId(std::string("anti-B0"))&&
      daught == EvtPDL::getId(std::string("anti-K_10")))){
     double ff0A[]   ={0.450, 0.340, 0.41, 0.22, 0.31, 0.310, 0.28};
     double sigma1A[]={1.600, 0.635, 1.51, 2.40, 2.01, 0.629, 1.36};
     double sigma2A[]={0.974, 0.211, 1.18, 1.78, 1.50, 0.387, 0.72};
     double ff0B[]   ={-0.37, -0.29, -0.17, -0.45, -0.25, -0.250, -0.11};
     double sigma1B[]={ 1.72, 0.729, 0.919,  1.34,  1.59,  0.378, -1.61};
     double sigma2B[]={0.912, 0.074, 0.855,  0.69,  0.79, -0.755,  10.2};
     int    eq_num[]={  10,   10,   10,   10,   10,   10,   10};

     double MM2 =5.279*5.279; // GeV^2
     double MB  =5.279; // GeV
     double MK1 =1.272; // GeV
     double MK1A=1.31;  // GeV
     double MK1B=1.34;  // GeV

     double sinK=sin(thetaK); // sin(-34^o)
     double cosK=cos(thetaK); // cos(-34^o)
     
     double a, v0, v1, v2;
   
     a =  sinK*equation9_10(ff0A[0], MM2, t, sigma1A[0], sigma2A[0], eq_num[0])*(MB+MK1)/(MB+MK1A);
     a =a+cosK*equation9_10(ff0B[0], MM2, t, sigma1B[0], sigma2B[0], eq_num[0])*(MB+MK1)/(MB+MK1B);

     v0=   sinK*equation9_10(ff0A[1], MM2, t, sigma1A[1], sigma2A[1], eq_num[1])*MK1A/MK1;
     v0=v0+cosK*equation9_10(ff0B[1], MM2, t, sigma1B[1], sigma2B[1], eq_num[1])*MK1B/MK1;

     v1=   sinK*equation9_10(ff0A[2], MM2, t, sigma1A[2], sigma2A[2], eq_num[2])*(MB+MK1A)/(MB+MK1);
     v1=v1+cosK*equation9_10(ff0B[2], MM2, t, sigma1B[2], sigma2B[2], eq_num[2])*(MB+MK1B)/(MB+MK1);

     v2=   sinK*equation9_10(ff0A[3], MM2, t, sigma1A[3], sigma2A[3], eq_num[3])*(MB+MK1)/(MB+MK1A);
     v2=v2+cosK*equation9_10(ff0B[3], MM2, t, sigma1B[3], sigma2B[3], eq_num[3])*(MB+MK1)/(MB+MK1B);

     v =a;
     a0=v0;
     a1=v1;
     a2=v2;
    
     t1=   sinK*equation9_10(ff0A[4], MM2, t, sigma1A[4], sigma2A[4], eq_num[4]);
     t1=t1+cosK*equation9_10(ff0B[4], MM2, t, sigma1B[4], sigma2B[4], eq_num[4]);

     t2=   sinK*equation9_10(ff0A[5], MM2, t, sigma1A[5], sigma2A[5], eq_num[5])*(MB*MB-MK1A*MK1A)/(MB*MB-MK1*MK1);
     t2=t2+cosK*equation9_10(ff0B[5], MM2, t, sigma1B[5], sigma2B[5], eq_num[5])*(MB*MB-MK1B*MK1B)/(MB*MB-MK1*MK1);

     t3=   sinK*equation9_10(ff0A[6], MM2, t, sigma1A[6], sigma2A[6], eq_num[6]);
     t3=t3+cosK*equation9_10(ff0B[6], MM2, t, sigma1B[6], sigma2B[6], eq_num[6]);

     models_counter=models_counter+1;

//     EvtGenReport(EVTGEN_NOTICE,"EvtGen") <<"\n The function  EvtbTosllMSFF::getVectorFF(...) passed."
//     << "\n barB -> bar K_1(1270) transition form factors"
//     << std::endl;     
  }


  // \bar B -> \bar K_1(1400) transition form factors
  // See the paper: H.Hatanaka and Kwei-Chou Yang, PRD78, 074007 (2008)
  if((parent == EvtPDL::getId(std::string("B+"))&&
      daught == EvtPDL::getId(std::string("K'_1+")))||
     (parent == EvtPDL::getId(std::string("B-"))&&
      daught == EvtPDL::getId(std::string("K'_1-")))||
     (parent == EvtPDL::getId(std::string("B0"))&&
      daught == EvtPDL::getId(std::string("K'_10")))||
     (parent == EvtPDL::getId(std::string("anti-B0"))&&
      daught == EvtPDL::getId(std::string("anti-K'_10")))){
     double ff0A[]   ={0.450, 0.340, 0.41, 0.22, 0.31, 0.310, 0.28};
     double sigma1A[]={1.600, 0.635, 1.51, 2.40, 2.01, 0.629, 1.36};
     double sigma2A[]={0.974, 0.211, 1.18, 1.78, 1.50, 0.387, 0.72};
     double ff0B[]   ={-0.37, -0.29, -0.17, -0.45, -0.25, -0.250, -0.11};
     double sigma1B[]={ 1.72, 0.729, 0.919,  1.34,  1.59,  0.378, -1.61};
     double sigma2B[]={0.912, 0.074, 0.855,  0.69,  0.79, -0.755,  10.2};
     int    eq_num[]={  10,   10,   10,   10,   10,   10,   10};

     double MM2 =5.279*5.279; // GeV^2
     double MB  =5.279; // GeV
     double MK1 =1.403; // GeV
     double MK1A=1.31;  // GeV
     double MK1B=1.34;  // GeV

     double sinK=sin(thetaK); // sin(-34^o)
     double cosK=cos(thetaK); // cos(-34^o)
     
     double a, v0, v1, v2;
   
     a =  cosK*equation9_10(ff0A[0], MM2, t, sigma1A[0], sigma2A[0], eq_num[0])*(MB+MK1)/(MB+MK1A);
     a =a-sinK*equation9_10(ff0B[0], MM2, t, sigma1B[0], sigma2B[0], eq_num[0])*(MB+MK1)/(MB+MK1B);

     v0=   cosK*equation9_10(ff0A[1], MM2, t, sigma1A[1], sigma2A[1], eq_num[1])*MK1A/MK1;
     v0=v0-sinK*equation9_10(ff0B[1], MM2, t, sigma1B[1], sigma2B[1], eq_num[1])*MK1B/MK1;

     v1=   cosK*equation9_10(ff0A[2], MM2, t, sigma1A[2], sigma2A[2], eq_num[2])*(MB+MK1A)/(MB+MK1);
     v1=v1-sinK*equation9_10(ff0B[2], MM2, t, sigma1B[2], sigma2B[2], eq_num[2])*(MB+MK1B)/(MB+MK1);

     v2=   cosK*equation9_10(ff0A[3], MM2, t, sigma1A[3], sigma2A[3], eq_num[3])*(MB+MK1)/(MB+MK1A);
     v2=v2-sinK*equation9_10(ff0B[3], MM2, t, sigma1B[3], sigma2B[3], eq_num[3])*(MB+MK1)/(MB+MK1B);

     v =a;
     a0=v0;
     a1=v1;
     a2=v2;
    
     t1=   cosK*equation9_10(ff0A[4], MM2, t, sigma1A[4], sigma2A[4], eq_num[4]);
     t1=t1-sinK*equation9_10(ff0B[4], MM2, t, sigma1B[4], sigma2B[4], eq_num[4]);

     t2=   cosK*equation9_10(ff0A[5], MM2, t, sigma1A[5], sigma2A[5], eq_num[5])*(MB*MB-MK1A*MK1A)/(MB*MB-MK1*MK1);
     t2=t2-sinK*equation9_10(ff0B[5], MM2, t, sigma1B[5], sigma2B[5], eq_num[5])*(MB*MB-MK1B*MK1B)/(MB*MB-MK1*MK1);

     t3=   cosK*equation9_10(ff0A[6], MM2, t, sigma1A[6], sigma2A[6], eq_num[6]);
     t3=t3-sinK*equation9_10(ff0B[6], MM2, t, sigma1B[6], sigma2B[6], eq_num[6]);

     models_counter=models_counter+1;

//     EvtGenReport(EVTGEN_NOTICE,"EvtGen") <<"\n The function  EvtbTosllMSFF::getVectorFF(...) passed."
//     << "\n barB -> bar K_1(1270) transition form factors"
//     << std::endl;     
  }


//  EvtGenReport(EVTGEN_NOTICE,"EvtGen") << "\n models_counter  = " << models_counter
//      << "\n Vector form-factors at q^2 = " << t 
//      << "   for B -> V transition:"
//      << "\n v  = " << v
//      << "\n a0 = " << a0 
//      << "\n a1 = " << a1 
//      << "\n a2 = " << a2 
//      << "\n t1 = " << t1 
//      << "\n t2 = " << t2  
//      << "\n t3 = " << t3 << std::endl;


  if(models_counter!=1){
      EvtGenReport(EVTGEN_ERROR,"EvtGen") << 
        "\n In the function EvtbTosllMSFF::getVectorFF(...) \n" <<
        "the parameter models_counter not equal 1! \n" <<
        "models_counter = " << models_counter <<std::endl;
      ::abort();
  }

}



// Getting the quark mass (in GeV) using to the dispersion quark model
// of D.Melikhov, B.Stech, PRD62, 014006 (2000).
// 
// i=1 => return m_u;
// i=2 => return m_d;
// i=3 => return m_s;
// i=4 => return m_c;
// i=5 => return m_b;
double EvtbTosllMSFF::getQuarkMass(int i){

  double qm=0.0;

  switch(i)
    {
      case 1:
        qm=0.23; // m_u
        break;
      case 2:
        qm=0.23; // m_d = m_u
        break;
      case 3:
        qm=0.35; // m_s
        break;
      case 4:
        qm=1.45; // m_c
        break;
      case 5:
        qm=4.85; // m_b
        break;
      default:
        EvtGenReport(EVTGEN_ERROR,"EvtGen") << 
          "In the function EvtbTosllMSFF::getQuarkMass   \n" <<
          "the parameter i not equal 1, 2,  3, 4 or 5! \n" <<
          "i =" << i <<std::endl;
      ::abort();
    }

  return qm;
}
