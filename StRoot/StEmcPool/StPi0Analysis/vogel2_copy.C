#ifndef VOGEL2_COPY_C
#define VOGEL2_COPY_C

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
//Float_t Vogel2x[]={1.5,     2.5,    3.5,     4.5,     5.5,     6.5,     7.5,     8.5,     9.5,    10.5,    11.5,    12.5,    13.5,    14.5,    15.5,    16.5,    17.5,    18.5,    19.5};
// assuming values are integrated over pT bins
Float_t Vogel2x[] = {1.31086, 2.3608, 3.39966, 4.42038, 5.43445, 6.44381, 7.45095, 8.45588, 9.4602, 10.4633, 11.4668, 12.4686, 13.4701, 14.4725, 15.4738, 16.4747, 17.4761, 18.4765, 19.4752};

// Factorization scale mu = pT
Float_t Vogel2y[]  = {6.240E+09, 2.225E+08, 2.352E+07, 4.352E+06, 1.118E+06, 3.555E+05, 1.314E+05, 5.412E+04, 2.431E+04, 1.170E+04, 5.969E+03, 3.170E+03, 1.746E+03, 9.978E+02, 5.849E+02, 3.502E+02, 2.142E+02, 1.332E+02, 8.455E+01};
// Factorization scale mu = pT/2
Float_t Vogel21y[] = {(6.165E+08 * (6.240E+09 / 2.225E+08)), 6.165E+08, 4.911E+07, 7.924E+06, 1.875E+06, 5.664E+05, 2.021E+05, 8.157E+04, 3.605E+04, 1.721E+04, 8.712E+03, 4.618E+03, 2.548E+03, 1.454E+03, 8.526E+02, 5.122E+02, 3.137E+02, 1.962E+02, 1.242E+02};
// Factorization scale mu = pT*2
Float_t Vogel22y[] = {3.071E+09, 1.085E+08, 1.229E+07, 2.378E+06, 6.301E+05, 2.041E+05, 7.658E+04, 3.196E+04, 1.449E+04, 7.038E+03, 3.603E+03, 1.923E+03, 1.066E+03, 6.102E+02, 3.578E+02, 2.146E+02, 1.316E+02, 8.188E+01, 5.184E+01};

TGraph *pQCDPPgr_2 = 0;
TGraph *pQCDPP1gr_2 = 0;
TGraph *pQCDPP2gr_2 = 0;
TWeightCalculator pQCDPPweight_2("pQCDPPweight_2", "pQCD p+p weight (CTEQ6M5 PDF + DSS FF)");
Double_t getNLOpQCDPP_2(Double_t *x, Double_t *p) {
        const Float_t pQCDPPmultiplier = (1 * 1 * 1e-9)/* / (30.0e-3)*/; // NSD cross section is 30.0 +/- 3.5 mb
        if (!pQCDPPgr_2) {

// 1/pT dsigma/dpT
DIVIDE_POINTS(Vogel2y, Vogel2x, Vogel2y)
DIVIDE_POINTS(Vogel21y, Vogel2x, Vogel21y)
DIVIDE_POINTS(Vogel22y, Vogel2x, Vogel22y)

// 1/detadphi
MULTIPLY(Vogel2y, 1.0/(2.0*TMath::TwoPi()))
MULTIPLY(Vogel21y, 1.0/(2.0*TMath::TwoPi()))
MULTIPLY(Vogel22y, 1.0/(2.0*TMath::TwoPi()))
                Float_t *QyL = new Float_t[sizeof(Vogel2x) / sizeof(Vogel2x[0])];
                bin_stat_list_type pQCDPPspectrum;
                for (Int_t i = 0;i < Int_t(sizeof(Vogel2x) / sizeof(Vogel2x[0]));i++) {
                        QyL[i] = log(Vogel2y[i]);
                        TBinParameters par;
                        par.min = Vogel2x[i];
                        par.max = Vogel2x[i];
                        TBinStatistics bin;
                        bin.setParameters(par);
                        bin.setValue(Vogel2y[i] * pQCDPPmultiplier);
                        bin.setError(Vogel2y[i] * pQCDPPmultiplier * 0.1);
                        pQCDPPspectrum.push_back(bin);
                }
                pQCDPPweight_2.Fit(pQCDPPspectrum);
                pQCDPPgr_2 = new TGraph(sizeof(Vogel2x) / sizeof(Vogel2x[0]), Vogel2x, QyL);
        }
        if (!pQCDPP1gr_2) {
                Float_t *Q1yR = new Float_t[sizeof(Vogel21y) / sizeof(Vogel21y[0])];
                for (Int_t i = 0;i < Int_t(sizeof(Vogel21y) / sizeof(Vogel21y[0]));i++) Q1yR[i] = Vogel21y[i] / Vogel2y[i];
                pQCDPP1gr_2 = new TGraph(sizeof(Vogel21y) / sizeof(Vogel21y[0]), Vogel2x, Q1yR);
        }
        if (!pQCDPP2gr_2) {
                Float_t *Q2yR = new Float_t[sizeof(Vogel22y) / sizeof(Vogel22y[0])];
                for (Int_t i = 0;i < Int_t(sizeof(Vogel22y) / sizeof(Vogel22y[0]));i++) Q2yR[i] = Vogel22y[i] / Vogel2y[i];
                pQCDPP2gr_2 = new TGraph(sizeof(Vogel22y) / sizeof(Vogel22y[0]), Vogel2x, Q2yR);
        }
        Float_t y = 0;
        if (x && p) {
                if (p[0] == 0) {
                        y = exp(pQCDPPgr_2->Eval(x[0], 0, "S")) * pQCDPPmultiplier;
                } else if (p[0] == 1) {
                        y = exp(pQCDPPgr_2->Eval(x[0], 0, "S")) * pQCDPPmultiplier * pQCDPP1gr_2->Eval(x[0], 0, "S");
                } else if (p[0] == 2) {
                        y = exp(pQCDPPgr_2->Eval(x[0], 0, "S")) * pQCDPPmultiplier * pQCDPP2gr_2->Eval(x[0], 0, "S");
                } else if (p[0] == 3) {
                        y = exp(pQCDPPgr_2->Eval(x[0], 0, "S")) * pQCDPPmultiplier * x[0];
                } else if (p[0] == 10) {
                        y = 1.0;
                } else if (p[0] == 11) {
                        y = pQCDPP1gr_2->Eval(x[0], 0, "S");
                } else if (p[0] == 12) {
                        y = pQCDPP2gr_2->Eval(x[0], 0, "S");
                } 
        }
        return y;
}

#endif
