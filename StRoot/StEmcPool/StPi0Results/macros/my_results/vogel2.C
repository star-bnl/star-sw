#ifndef VOGEL2_C
#define VOGEL2_C

#include <TGraph.h>

//#include <TBinParameters.h>
//#include <TBinStatistics.h>

#include <StEmcPool/StPi0Common/array_funcs.h>

//  NLO calculations by Werner et al. for mid-rapidity hadrons.
//
// - Unpolarized pdfs's: Cteq6M5 (This seems to be the latest full set. There is 
//   a certain proliferation of cteq6 sets now, with special focus on things like 
//   charm, s-sbar etc. 6M5 seems to be the current "standard". Differences to 6M 
//   are not huge).
//
// - DSS fragmentation functions (For pi0, pi+, pi-)
//
//   See papers:
//
//   http://arXiv.org/pdf/0707.1506
//
//   http://arXiv.org/pdf/hep-ph/0703242
//
// - The calculations give the cross section dsigma/dpt/deta/dphi, INTEGRATED over bins in 
//   pt from 1-2-3-...-20, and integrated over -1<eta<1, and integrated over 0<phi<2pi. So the result is just a number 
//   in pb, the cross section over that bin. The pt value is just the center of that 
//   bin. The results for the scales pt/2,pt,2*pt are included to show the impact on the 
//   unpolarized cross-section due to scale variations.

// assuming values are at precise center of pT bins
Float_t Vogel2x[]={1.5,     2.5,    3.5,     4.5,     5.5,     6.5,     7.5,     8.5,     9.5,    10.5,    11.5,    12.5,    13.5,    14.5,    15.5,    16.5,    17.5,    18.5,    19.5};
// assuming values are integrated over pT bins
//Float_t Vogel2x[] = {1.31086, 2.3608, 3.39966, 4.42038, 5.43445, 6.44381, 7.45095, 8.45588, 9.4602, 10.4633, 11.4668, 12.4686, 13.4701, 14.4725, 15.4738, 16.4747, 17.4761, 18.4765, 19.4752};

// Factorization scale mu = pT
Float_t Vogel2y[]  = {6.240E+09, 2.225E+08, 2.352E+07, 4.352E+06, 1.118E+06, 3.555E+05, 1.314E+05, 5.412E+04, 2.431E+04, 1.170E+04, 5.969E+03, 3.170E+03, 1.746E+03, 9.978E+02, 5.849E+02, 3.502E+02, 2.142E+02, 1.332E+02, 8.455E+01};
// Factorization scale mu = pT/2
Float_t Vogel21y[] = {(6.165E+08 * (6.240E+09 / 2.225E+08)), 6.165E+08, 4.911E+07, 7.924E+06, 1.875E+06, 5.664E+05, 2.021E+05, 8.157E+04, 3.605E+04, 1.721E+04, 8.712E+03, 4.618E+03, 2.548E+03, 1.454E+03, 8.526E+02, 5.122E+02, 3.137E+02, 1.962E+02, 1.242E+02};
// Factorization scale mu = pT*2
Float_t Vogel22y[] = {3.071E+09, 1.085E+08, 1.229E+07, 2.378E+06, 6.301E+05, 2.041E+05, 7.658E+04, 3.196E+04, 1.449E+04, 7.038E+03, 3.603E+03, 1.923E+03, 1.066E+03, 6.102E+02, 3.578E+02, 2.146E+02, 1.316E+02, 8.188E+01, 5.184E+01};
/*
// 1/pT
DIVIDE_POINTS(Vogel2y, Vogel2x, Vogel2y)
DIVIDE_POINTS(Vogel21y, Vogel2x, Vogel21y)
DIVIDE_POINTS(Vogel22y, Vogel2x, Vogel22y)

// 1/detadphi
MULTIPLY(Vogel2y, 1.0/(2.0*TMath::TwoPi()))
MULTIPLY(Vogel21y, 1.0/(2.0*TMath::TwoPi()))
MULTIPLY(Vogel22y, 1.0/(2.0*TMath::TwoPi()))
*/

DERIVE_E_FROM(Vogel2ex, Vogel2x)
DERIVE_E_FROM(Vogel2ey, Vogel2y)

const Char_t *showVogel2Legend = "NLO pQCD, CTEQ6M5 PDF, DSS FF";
const Char_t *showVogel2ErrorsLegend = "pQCD scale uncertainty";

#endif
