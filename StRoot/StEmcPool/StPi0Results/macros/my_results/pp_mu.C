#ifndef PP_PI0_MU_C
#define PP_PI0_MU_C

// From: "Xin-Nian Wang" <xnwang@lbl.gov>
//     Nuclear Science Division, MS 70R0319
//     Lawrence Berkeley National Laboratory
//     Berkeley, CA 94720
//
// Here are NLO p_T spectra (invariant cross sections)
// in p+p collisions with 3 scales.
// Note: Besause of the difference of the p_T bin width, the spectra value is
// a bit different from in other plots.
// Descriptions of the NLO calculation of pp spectra can be found in the
// following paper:
//     \bibitem{Zhang:2007ja}
//     H.~Zhang, J.~F.~Owens, E.~Wang and X.~N.~Wang,
//     %``Dihadron Tomography of High-Energy Nuclear Collisions in NLO pQCD,''
//     Phys.\ Rev.\ Lett.\  {\bf 98}, 212301 (2007)
//     [arXiv:nucl-th/0701045].
//     %%CITATION = PRLTA,98,212301;%%
// (we used KKP for FF, CTEQ6M for PDF)

#include <StEmcPool/StPi0Common/array_funcs.h>

// 1) p+p->pi^0 with mu=0.9p_T
Float_t pp_pi0_mu09_x[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20};
Float_t pp_pi0_mu09_y[] = {274.20001,0.03931,0.00155,1.69E-4,3.091E-5,7.324E-6,2.224E-6,7.956E-7,3.107E-7,1.335E-7,6.24E-8,3.12E-8,1.556E-8,8.422E-9,4.704E-9,2.715E-9,1.567E-9,9.352E-10,5.615E-10,3.516E-10};
DERIVE_E_FROM(pp_pi0_mu09_ex, pp_pi0_mu09_x);
DERIVE_E_FROM(pp_pi0_mu09_ey, pp_pi0_mu09_y);
const Char_t *pp_pi0_mu09_title = "NLO pQCD #mu=0.9#font[12]{p}_{#font[12]{T}}";

// 2) p+p->pi^0 with mu=1.2p_T
Float_t pp_pi0_mu12_x[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20};
Float_t pp_pi0_mu12_y[] = {12.61,0.02825,0.00124,1.385E-4,2.496E-5,6.19E-6,1.891E-6,6.711E-7,2.652E-7,1.151E-7,5.328E-8,2.586E-8,1.36E-8,7.385E-9,3.94E-9,2.285E-9,1.292E-9,8.117E-10,5.033E-10,3.053E-10};
DERIVE_E_FROM(pp_pi0_mu12_ex, pp_pi0_mu12_x);
DERIVE_E_FROM(pp_pi0_mu12_ey, pp_pi0_mu12_y);
const Char_t *pp_pi0_mu12_title = "NLO pQCD #mu=1.2#font[12]{p}_{#font[12]{T}}";

// 3) p+p->pi^0 with mu=1.5p_T
Float_t pp_pi0_mu15_x[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20};
Float_t pp_pi0_mu15_y[] = {4.369,0.02313,0.00105,1.167E-4,2.155E-5,5.375E-6,1.649E-6,5.809E-7,2.333E-7,1.009E-7,4.605E-8,2.309E-8,1.163E-8,6.294E-9,3.508E-9,1.974E-9,1.16E-9,6.848E-10,4.315E-10,2.617E-10};
DERIVE_E_FROM(pp_pi0_mu15_ex, pp_pi0_mu15_x);
DERIVE_E_FROM(pp_pi0_mu15_ey, pp_pi0_mu15_y);
const Char_t *pp_pi0_mu15_title = "NLO pQCD #mu=1.5#font[12]{p}_{#font[12]{T}}";

#endif
