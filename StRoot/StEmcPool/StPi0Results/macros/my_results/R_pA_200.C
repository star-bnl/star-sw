#ifndef RPA_PI0_C
#define RPA_PI0_C

// From: "Xin-Nian Wang" <xnwang@lbl.gov>
//     Nuclear Science Division, MS 70R0319
//     Lawrence Berkeley National Laboratory
//     Berkeley, CA 94720
//

// Here are NLO results of R_pA(p_T) in p+Au collisions at 200GeV with 4 shadowing.

#include <StEmcPool/StPi0Common/array_funcs.h>

// 1) p+Au->pi^0 with EKS98 shadowing parameterization
Float_t RpA_pi0_EKS98_x[] = {1.25,1.75,2.25,2.75,3.25,3.75,4.25,4.75,5.25,5.75,6.25,6.75,7.25,7.75,8.25,8.75,9.25,9.75,10.25,10.75,11.25,11.75,12.25,12.75,13.25,13.75,14.25,14.75,15.5,16.5,17.5,19};
Float_t RpA_pi0_EKS98_y[] = {0.9718,1.0358,1.0468,1.0609,1.0688,1.0678,1.0794,1.0795,1.0752,1.0786,1.0549,1.0608,1.0588,1.0469,1.0488,1.0523,1.0491,1.0358,1.0357,1.0277,1.0219,1.0214,1.0174,1.0148,1.0107,1.0075,0.9985,1.0078,1.003,0.9908,0.9872,0.9819};
DERIVE_E_FROM(RpA_pi0_EKS98_ex, RpA_pi0_EKS98_x);
DERIVE_E_FROM(RpA_pi0_EKS98_ey, RpA_pi0_EKS98_y);
const Char_t *RpA_pi0_EKS98_title = "NLO pQCD + EKS98";

// 2) p+Au->pi^0 with nDS shadowing parameterization
Float_t RpA_pi0_nDS_x[] = {1.25,1.75,2.25,2.75,3.25,3.75,4.25,4.75,5.25,5.75,6.25,6.75,7.25,7.75,8.25,8.75,9.25,9.75,10.25,10.75,11.25,11.75,12.25,12.75,13.25,13.75,14.25,14.75,15.5,16.5,17.5,19};
Float_t RpA_pi0_nDS_y[] = {0.9812,1.0019,1.001,1.0087,1.0186,1.0148,1.0196,1.016,1.0241,1.0248,1.0189,1.0204,1.0233,1.0282,1.029,1.0312,1.0302,1.0212,1.0288,1.0255,1.0257,1.0298,1.0259,1.0338,1.027,1.0256,1.0259,1.0371,1.0277,1.0269,1.0338,1.0335};
DERIVE_E_FROM(RpA_pi0_nDS_ex, RpA_pi0_nDS_x);
DERIVE_E_FROM(RpA_pi0_nDS_ey, RpA_pi0_nDS_y);
const Char_t *RpA_pi0_nDS_title = "NLO pQCD + nDS";

// 3) p+Au->pi^0 with nPDF shadowing parameterization
Float_t RpA_pi0_nPDF_x[] = {1.25,1.75,2.25,2.75,3.25,3.75,4.25,4.75,5.25,5.75,6.25,6.75,7.25,7.75,8.25,8.75,9.25,9.75,10.25,10.75,11.25,11.75,12.25,12.75,13.25,13.75,14.25,14.75,15.5,16.5,17.5,19};
Float_t RpA_pi0_nPDF_y[] = {0.9038,0.9319,0.9522,0.9652,0.9897,0.9915,1.027,1.0268,1.039,1.0484,1.0441,1.053,1.0543,1.0579,1.0676,1.0585,1.0592,1.0577,1.0516,1.049,1.0466,1.0473,1.0464,1.0451,1.0344,1.0415,1.0358,1.0351,1.0296,1.0221,1.0164,1.01};
DERIVE_E_FROM(RpA_pi0_nPDF_ex, RpA_pi0_nPDF_x);
DERIVE_E_FROM(RpA_pi0_nPDF_ey, RpA_pi0_nPDF_y);
const Char_t *RpA_pi0_nPDF_title = "NLO pQCD + nPDF";

// 4) p+Au->pi^0 with HIJING shadowing parameterization
Float_t RpA_pi0_HIJING_x[] = {1.25,1.75,2.25,2.75,3.25,3.75,4.25,4.75,5.25,5.75,6.25,6.75,7.25,7.75,8.25,8.75,9.25,9.75,10.25,10.75,11.25,11.75,12.25,12.75,13.25,13.75,14.25,14.75,15.5,16.5,17.5,19};
Float_t RpA_pi0_HIJING_y[] = {0.7958,0.8149,0.8484,0.8826,0.8938,0.9195,0.9317,0.9457,0.9588,0.9747,0.9717,0.9763,0.9877,0.9842,0.9894,0.9907,0.9943,0.9894,0.992,0.9959,0.9981,0.9891,0.9906,0.9899,0.9951,0.9851,0.9883,0.9861,0.9857,0.9801,0.9763,0.9759};
DERIVE_E_FROM(RpA_pi0_HIJING_ex, RpA_pi0_HIJING_x);
DERIVE_E_FROM(RpA_pi0_HIJING_ey, RpA_pi0_HIJING_y);
const Char_t *RpA_pi0_HIJING_title = "NLO pQCD + HIJING";

#endif
