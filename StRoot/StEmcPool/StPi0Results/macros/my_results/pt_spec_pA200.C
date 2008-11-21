#ifndef PA_PI0_C
#define PA_PI0_C

// From: "Xin-Nian Wang" <xnwang@lbl.gov>
//     Nuclear Science Division, MS 70R0319
//     Lawrence Berkeley National Laboratory
//     Berkeley, CA 94720
//
// Here are NLO p_T spectra (invariant cross sections)
// in p+p and p+Au collisions at 200GeV with 4 shadowing.
// pAu data have been divided by 197.
// \mu=1.2pt
// The shadowing parametrizations are noted by their names.
//
// The reference for the nDS shadowing parameterization is 
// Phys.Rev.D69:074028,2004 (hep-ph/0311227).
//*************************************************************

#include <StEmcPool/StPi0Common/array_funcs.h>

#define pA_dA_CONST (2*197)

// 1) p+p->pi^0
Float_t pA_pi0_mu12_x[] = {1.25,1.75,2.25,2.75,3.25,3.75,4.25,4.75,5.25,5.75,6.25,6.75,7.25,7.75,8.25,8.75,9.25,9.75,10.25,10.75,11.25,11.75,12.25,12.75,13.25,13.75,14.25,14.75,15.5,16.5,17.5,19};
Float_t pA_pi0_mu12_y[] = {0.5949,0.05871,0.00983,0.0023,6.789E-4,2.36E-4,9.033E-5,3.885E-5,1.822E-5,9.178E-6,4.879E-6,2.698E-6,1.547E-6,9.229E-7,5.652E-7,3.556E-7,2.282E-7,1.508E-7,1.007E-7,6.852E-8,4.74E-8,3.317E-8,2.351E-8,1.687E-8,1.22E-8,8.916E-9,6.595E-9,4.876E-9,3.214E-9,1.857E-9,1.095E-9,5.309E-10};
DERIVE_E_FROM(pA_pi0_mu12_ex, pA_pi0_mu12_x);
DERIVE_E_FROM(pA_pi0_mu12_ey, pA_pi0_mu12_y);
MULTIPLY(pA_pi0_mu12_y, pA_dA_CONST);
const Char_t *pA_pi0_mu12_title = "NLO pQCD #mu=1.2#font[12]{p}_{#font[12]{T}}";

// 2) p+Au->pi^0 with EKS98 shadowing parameterization
Float_t pA_pi0_mu12_EKS98_x[] = {1.25,1.75,2.25,2.75,3.25,3.75,4.25,4.75,5.25,5.75,6.25,6.75,7.25,7.75,8.25,8.75,9.25,9.75,10.25,10.75,11.25,11.75,12.25,12.75,13.25,13.75,14.25,14.75,15.5,16.5,17.5,19};
Float_t pA_pi0_mu12_EKS98_y[] = {0.5781,0.06081,0.01029,0.00244,7.256E-4,2.52E-4,9.75E-5,4.194E-5,1.959E-5,9.899E-6,5.147E-6,2.862E-6,1.638E-6,9.662E-7,5.928E-7,3.742E-7,2.394E-7,1.562E-7,1.043E-7,7.042E-8,4.844E-8,3.388E-8,2.392E-8,1.712E-8,1.233E-8,8.983E-9,6.585E-9,4.914E-9,3.215E-9,1.84E-9,1.081E-9,5.213E-10};
DERIVE_E_FROM(pA_pi0_mu12_EKS98_ex, pA_pi0_mu12_EKS98_x);
DERIVE_E_FROM(pA_pi0_mu12_EKS98_ey, pA_pi0_mu12_EKS98_y);
MULTIPLY(pA_pi0_mu12_EKS98_y, pA_dA_CONST);
const Char_t *pA_pi0_mu12_EKS98_title = "NLO pQCD #mu=1.2#font[12]{p}_{#font[12]{T}} + EKS98";

// 3) p+Au->pi^0 with nDS shadowing parameterization
Float_t pA_pi0_mu12_nDS_x[] = {1.25,1.75,2.25,2.75,3.25,3.75,4.25,4.75,5.25,5.75,6.25,6.75,7.25,7.75,8.25,8.75,9.25,9.75,10.25,10.75,11.25,11.75,12.25,12.75,13.25,13.75,14.25,14.75,15.5,16.5,17.5,19};
Float_t pA_pi0_mu12_nDS_y[] = {0.5837,0.05882,0.00984,0.00232,6.915E-4,2.395E-4,9.21E-5,3.947E-5,1.866E-5,9.406E-6,4.971E-6,2.753E-6,1.583E-6,9.489E-7,5.816E-7,3.667E-7,2.351E-7,1.54E-7,1.036E-7,7.027E-8,4.862E-8,3.416E-8,2.412E-8,1.744E-8,1.253E-8,9.144E-9,6.766E-9,5.057E-9,3.303E-9,1.907E-9,1.132E-9,5.487E-10};
DERIVE_E_FROM(pA_pi0_mu12_nDS_ex, pA_pi0_mu12_nDS_x);
DERIVE_E_FROM(pA_pi0_mu12_nDS_ey, pA_pi0_mu12_nDS_y);
MULTIPLY(pA_pi0_mu12_nDS_y, pA_dA_CONST);
const Char_t *pA_pi0_mu12_nDS_title = "NLO pQCD #mu=1.2#font[12]{p}_{#font[12]{T}} + nDS";

// 4) p+Au->pi^0 with nPDF shadowing parameterization
Float_t pA_pi0_mu12_nPDF_x[] = {1.25,1.75,2.25,2.75,3.25,3.75,4.25,4.75,5.25,5.75,6.25,6.75,7.25,7.75,8.25,8.75,9.25,9.75,10.25,10.75,11.25,11.75,12.25,12.75,13.25,13.75,14.25,14.75,15.5,16.5,17.5,19};
Float_t pA_pi0_mu12_nPDF_y[] = {0.5377,0.05471,0.00936,0.00222,6.713E-4,2.34E-4,9.277E-5,3.989E-5,1.893E-5,9.622E-6,5.094E-6,2.841E-6,1.631E-6,9.763E-7,6.034E-7,3.764E-7,2.417E-7,1.595E-7,1.059E-7,7.188E-8,4.961E-8,3.474E-8,2.46E-8,1.763E-8,1.262E-8,9.286E-9,6.831E-9,5.047E-9,3.309E-9,1.898E-9,1.113E-9,5.362E-10};
DERIVE_E_FROM(pA_pi0_mu12_nPDF_ex, pA_pi0_mu12_nPDF_x);
DERIVE_E_FROM(pA_pi0_mu12_nPDF_ey, pA_pi0_mu12_nPDF_y);
MULTIPLY(pA_pi0_mu12_nPDF_y, pA_dA_CONST);
const Char_t *pA_pi0_mu12_nPDF_title = "NLO pQCD #mu=1.2#font[12]{p}_{#font[12]{T}} + nPDF";

// 5) p+Au->pi^0 with HIJING shadowing parameterization
Float_t pA_pi0_mu12_HIJING_x[] = {1.25,1.75,2.25,2.75,3.25,3.75,4.25,4.75,5.25,5.75,6.25,6.75,7.25,7.75,8.25,8.75,9.25,9.75,10.25,10.75,11.25,11.75,12.25,12.75,13.25,13.75,14.25,14.75,15.5,16.5,17.5,19};
Float_t pA_pi0_mu12_HIJING_y[] = {0.4734,0.04784,0.00834,0.00203,6.068E-4,2.17E-4,8.416E-5,3.674E-5,1.747E-5,8.946E-6,4.741E-6,2.634E-6,1.528E-6,9.083E-7,5.592E-7,3.523E-7,2.269E-7,1.492E-7,9.989E-8,6.824E-8,4.731E-8,3.281E-8,2.329E-8,1.67E-8,1.214E-8,8.783E-9,6.518E-9,4.808E-9,3.168E-9,1.82E-9,1.069E-9,5.181E-10};
DERIVE_E_FROM(pA_pi0_mu12_HIJING_ex, pA_pi0_mu12_HIJING_x);
DERIVE_E_FROM(pA_pi0_mu12_HIJING_ey, pA_pi0_mu12_HIJING_y);
MULTIPLY(pA_pi0_mu12_HIJING_y, pA_dA_CONST);
const Char_t *pA_pi0_mu12_HIJING_title = "NLO pQCD #mu=1.2#font[12]{p}_{#font[12]{T}} + HIJING";

#undef pA_dA_CONST

#endif
