//--------------------------------------------------------------------------
//
// Environment:
//      This software is part of the EvtGen package.
//
// Module: 
// Description: Form-factors for B_q -> gamma transitions,  q={d,s}
//              according to the papers:
//              1) F.Kruger, D.Melikhov,  Phys. Rev. D67, 034002, 2003.  
//              2) D.Melikhov, N.Nikitin, Phys. Rev. D70, 114028, 2004. 
//              3) I.Balakireva, D.Melikhov, N.Nikitin, D.Tlisov, 
//                                        Phys. Rev. D81, 054024, 2010.
//
// Modification history:
//
//    A.Popov     October 03, 2008         Module created
//    N.Nikitin   October 10, 2008         Prepare the main classes 
//    A.Popov	  October 30, 2008	   Add the Ftv(0,q^2) and Fta(0,q^2) form-factors
//    N.Nikitin   March   01, 2010         Add the weak annihilation contribution
//    N.Nikitin   July    16, 2010         Has corrected the found bugs
//
//------------------------------------------------------------------------

#include "EvtGenBase/EvtPatches.hh"
#include "EvtGenBase/EvtComplex.hh"
#include "EvtGenBase/EvtVector4C.hh"
#include "EvtGenBase/EvtPDL.hh"
#include "EvtGenBase/EvtReport.hh"
#include "EvtGenBase/EvtParticle.hh"
#include "EvtGenBase/EvtId.hh"
#include "EvtGenModels/Evtbs2llGammaFFMNT.hh"
#include <cmath>
#include <cstdlib>

Evtbs2llGammaFFMNT::Evtbs2llGammaFFMNT(){}

/*                                                              *
 *  decay_id = 0 for b \bar q -> l^+ l^- \gamma transitions     *
 *             1 for q \bar b -> l^+ l^- \gamma transitions;    * 
 *  fb    - leptonic decay constant of the B_q - meson;         *
 *  mb    - the mass of the b-quark;                            *
 *  mq    - the mass of the light quark (d or s);               *
 *  c7gam - Wilson coefficient C_{7\gamma};                     *
 *  a1 = c1 + c2/3.0 - linear combination of the Wils. Coeff.;  *
 *  lambda_qu = V^*_{uq}*V_{ub}/V^*_{tq}*V_{tb}, where q={d,s}; *
 *  lambda_qc = V^*_{cq}*V_{cb}/V^*_{tq}*V_{tb}, where q={d,s}. *
 *                                                              */
void Evtbs2llGammaFFMNT::getPhotonFF(int decay_id, double fb, 
                                 EvtId parent,
                                 double q2, double M1,
                                 double mb, double mq,
                                 EvtComplex c7gam, EvtComplex a1,  
                                 EvtComplex lambda_qu, EvtComplex lambda_qc, 
                                 EvtComplex& Fv,  EvtComplex& Fa,
                                 EvtComplex& Ftv, EvtComplex& Fta){

  EvtComplex unit1(1.0,0.0); // real unit
  EvtComplex uniti(0.0,1.0); // imaginary unit
  EvtComplex unit0(0.0,0.0); // complex zero unit

  // characteristics of resonances rho, omega, phi
  double M_res[] = {0.7758, 0.78259, 1.019456};  // particle masses, Gev
  double Gamma[] = {0.1503, 0.00849, 0.00426};   // particle widthes, Gev

  double f_lept[]  = {5.04,  17.1, -13.2};	 // decay constants f_i
  double g_plus[]  = {0.27, -0.27, -0.38};	 // and form-factors g+(0)
	 g_plus[0] = g_plus[0]/sqrt(2.0);	 // by D.Melikhov, N.Nikitin, K.Toms, 
	 g_plus[1] = g_plus[1]/sqrt(2.0);	 // Phys.At.Nucl. 68, p.1842 (2005)  

  double hatq2 = q2/pow(M1,2);
  // E - photon energy in the B-meson rest frame 
  double E = 0.5*M1*(1-hatq2);

  // parametrs for form-factors Fv, Ftv, Fa, Fta

  //(by D.Melikhov, N.Nikitin, K.Toms, Yad. Fiz. 62, No 11)
  double beta[]  = {0.28, 0.30, 0.26, 0.33};  // beta, Gev^(-1)
  double Delta[] = {0.04, 0.04, 0.30, 0.30};  // Delta, Gev

  // form-factors
  EvtComplex Ftvq0, Ftaq0, Ftv00, Fta00;
  Fv    = unit1*beta[0]*fb*M1/(Delta[0]+E);	    // Fv(q^2)
  Ftvq0 = unit1*beta[1]*fb*M1/(Delta[1]+E);	    // Ftv(q^2,0)
  Fa    = unit1*beta[2]*fb*M1/(Delta[2]+E);	    // Fa(q^2)
  Ftaq0 = unit1*beta[3]*fb*M1/(Delta[3]+E);	    // Fta(q^2,0)
  Ftv00 = unit1*beta[1]*fb*M1/(Delta[1] + 0.5*M1);  // Ftv(0,0)
  Fta00 = unit1*beta[3]*fb*M1/(Delta[3] + 0.5*M1);  // Fta(0,0)
  EvtComplex Ftv_WA(0.0,0.0);                       // the weak annihilation contribution

  // Resonant contribution to the form-factors Ftv(0,q^2) and Fta(0,q^2)
  EvtComplex ResSum(0.0, 0.0);

  if(parent == EvtPDL::getId(std::string("B_s0"))||
     parent == EvtPDL::getId(std::string("anti-B_s0"))){
     // only \phi-resonant contribution to the Bs-decays
     ResSum = 
       2.0*g_plus[2]*q2/(f_lept[2]*(unit1*(q2-pow(M_res[2],2))+uniti*M_res[2]*Gamma[2]));
  }
  if(parent == EvtPDL::getId(std::string("B_d0"))||
     parent == EvtPDL::getId(std::string("anti-B_d0"))){
     // \rho- and \omega-resonant contribution to the Bd-decays
     for (int i=0; i<2; i++){
       ResSum = ResSum + 
         2.0*g_plus[i]*q2/(f_lept[i]*(unit1*(q2-pow(M_res[i],2))+uniti*M_res[i]*Gamma[i]));
    }
  }

  EvtComplex Ftv0q = Ftv00 - ResSum; // form-factor Ftv(0,q^2)
  EvtComplex Fta0q = Fta00 - ResSum; // form-factor Fta(0,q^2)

   // Ftv(q^2,q^2) = Ftv(q^2,0)+Ftv(0,q^2)
  Ftv = Ftvq0+Ftv0q;
   // Fta(q^2,q^2) = Fta(q^2,0)+Fta(0,q^2)
  Fta = Ftaq0+Fta0q;

   // Weak annihilation
  if(abs(c7gam)<0.0000001){
     EvtGenReport(EVTGEN_ERROR,"EvtGen") << "\n\n The function Evtbs2llGammaFFMNT::getPhotonFF"
                            << "\n Error: the Wilson coefficient C7gamma = 0!"
                            << " c7gam = " << c7gam << std::endl;
     ::abort();
  }
  if(mb<0.001){
     EvtGenReport(EVTGEN_ERROR,"EvtGen") << "\n\n The function Evtbs2llGammaFFMNT::getPhotonFF"
                            << " mb = " << mb << " << 5 GeV!" << std::endl;
     ::abort();
  } 
  
  switch(decay_id){ 
    /* b \bar q -> l^+ l^- \gamma transitions */
    case 0:
      Ftv_WA =(16.0/3.0)*(lambda_qu + lambda_qc)*(a1/c7gam)*(fb/mb);
      Ftv = (1.0 + mq/mb)*Ftv - Ftv_WA;
      Fta = (1.0 - mq/mb)*Fta;
      Fv  = Fv;
      Fa  = Fa;
    break;
    /* q \bar b -> l^+ l^- \gamma transitions */
    case 1:
      Ftv_WA =(16.0/3.0)*conj(lambda_qu+lambda_qc)*(a1/c7gam)*(fb/mb);
      Ftv = (1.0 + mq/mb)*Ftv + Ftv_WA;
      Fta = (1.0 - mq/mb)*Fta;          // The change of the sign 
      Fv  = Fv;                         // is included in the
      Fa  = Fa;                         // amplitudes definition!

    break; 
  };

}


// Getting the quark mass (in GeV) using to the dispersion quark model
// of D.Melikhov, B.Stech, PRD62, 014006 (2000).
// 
// i=1 => return m_u;
// i=2 => return m_d;
// i=3 => return m_s;
// i=4 => return m_c;
// i=5 => return m_b;
double Evtbs2llGammaFFMNT::getQuarkMass(int i){

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
          "the parametr i not equal 1, 2,  3, 4 or 5! \n" <<
          "i =" << i <<std::endl;
      ::abort();
    }

  return qm;
}

