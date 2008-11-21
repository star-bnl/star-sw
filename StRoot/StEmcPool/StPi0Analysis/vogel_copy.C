#ifndef VOGEL_COPY_C 
#define VOGEL_COPY_C 

#include <StEmcPool/StPi0Common/array_funcs.h> 

// NLO pQCD (W. Vogelsang et al.)
// CTEQ6M PDF
// KKP and Kretzer FF

// assuming values are at precise center of pT bins
Float_t Qx[] = {1.25, 1.75, 2.25, 2.75, 3.25, 3.75, 4.25, 4.75, 5.25, 5.75, 6.25, 6.75, 7.25, 7.75, 8.25, 8.75, 9.25, 9.75, 10.25, 10.75, 
11.25, 11.75, 12.25, 12.75, 13.25, 13.75, 14.25, 14.75, 15.25, 15.75, 20.00};
// assuming values are average in pT bins
//Float_t Qx[] = {0.25, 1.21906, 1.70669, 2.21482, 2.72042, 3.22404, 3.72759, 4.22966, 4.73176, 5.23301, 5.73484, 6.23582, 6.73643, 7.23788, 7.73810, 
//8.23857, 8.74029, 9.23920, 9.74062, 10.2407, 10.7417, 11.2414, 11.7415, 12.2425, 12.7420, 13.2430, 13.7428, 14.2423, 14.7449, 15.2426, 15.7427, 19.9909};

// Factorization scale mu = pT
Float_t Qy[]  = {   5.202E+08, 6.268E+07, 1.102E+07, 2.649E+06, 7.886E+05, 2.768E+05, 1.093E+05, 4.745E+04, 2.223E+04, 1.115E+04, 
		    5.882E+03, 3.239E+03, 1.860E+03, 1.100E+03, 6.710E+02, 4.210E+02, 2.678E+02, 1.749E+02, 1.165E+02, 7.893E+01, 
		    5.399E+01, 3.743E+01, 2.636E+01, 1.874E+01, 1.349E+01, 9.774E+00, 7.151E+00, 5.296E+00,      3.90,      2.90,    
		    2.90*0.2};
// Factorization scale mu = pT/2
Float_t Q1y[] = {   3.421E+09, 1.720E+08, 2.556E+07, 6.150E+06, 1.618E+06, 5.143E+05, 1.902E+05, 7.861E+04, 3.539E+04, 1.726E+04, 
		    8.937E+03, 4.813E+03, 2.731E+03, 1.596E+03, 9.665E+02, 6.008E+02, 3.808E+02, 2.480E+02, 1.651E+02, 1.120E+02, 
		    7.643E+01, 5.296E+01, 3.742E+01, 2.656E+01, 1.915E+01, 1.391E+01, 1.016E+01, 7.529E+00,      5.55,      4.12,    
		    4.12*0.2};
// Factorization scale mu = pT*2
Float_t Q2y[] = {   2.640E+08, 3.002E+07, 5.497E+06, 1.374E+06, 4.219E+05, 1.521E+05, 6.130E+04, 2.710E+04, 1.284E+04, 6.492E+03, 
		    3.453E+03, 1.918E+03, 1.110E+03, 6.605E+02, 4.049E+02, 2.552E+02, 1.631E+02, 1.070E+02, 7.156E+01, 4.859E+01, 
		    3.336E+01, 2.319E+01, 1.638E+01, 1.167E+01, 8.408E+00, 6.105E+00, 4.476E+00, 3.319E+00,      2.45,      1.82,    
		    1.82*0.2};

TGraph *pQCDgr = 0;
TGraph *pQCD1gr = 0;
TGraph *pQCD2gr = 0;
TWeightCalculator pQCDweight("pQCDweight", "pQCD weight");
Double_t getNLOpQCD(Double_t *x, Double_t *p) {
        const Float_t pQCDmultiplier = (2 * 197 * 1e-9);// / (0.93 * 0.95 * 1.03 * 2.21e3);
        if (!pQCDgr) {
                Float_t *QyL = new Float_t[sizeof(Qy) / sizeof(Qy[0])];
                bin_stat_list_type pQCDspectrum;
                for (Int_t i = 0;i < Int_t(sizeof(Qy) / sizeof(Qy[0]));i++) {
                        QyL[i] = log(Qy[i]);
                        TBinParameters par;
                        par.min = Qx[i];
                        par.max = Qx[i];
                        TBinStatistics bin;
                        bin.setParameters(par);
                        bin.setValue(Qy[i] * pQCDmultiplier);
                        bin.setError(Qy[i] * pQCDmultiplier * 0.1);
                        pQCDspectrum.push_back(bin);
                }
                pQCDweight.Fit(pQCDspectrum);
                pQCDgr = new TGraph(sizeof(Qx) / sizeof(Qx[0]), Qx, QyL);
        }
        if (!pQCD1gr) {
                Float_t *Q1yR = new Float_t[sizeof(Q1y) / sizeof(Q1y[0])];
                for (Int_t i = 0;i < Int_t(sizeof(Q1y) / sizeof(Q1y[0]));i++) Q1yR[i] = Q1y[i] / Qy[i];
                pQCD1gr = new TGraph(sizeof(Qx) / sizeof(Qx[0]), Qx, Q1yR);
        }
        if (!pQCD2gr) {
                Float_t *Q2yR = new Float_t[sizeof(Q2y) / sizeof(Q2y[0])];
                for (Int_t i = 0;i < Int_t(sizeof(Q2y) / sizeof(Q2y[0]));i++) Q2yR[i] = Q2y[i] / Qy[i];
                pQCD2gr = new TGraph(sizeof(Qx) / sizeof(Qx[0]), Qx, Q2yR);
        }
        Float_t y = 0;
        if (x && p) {
                if (p[0] == 0) {
                        y = exp(pQCDgr->Eval(x[0], 0, "S")) * pQCDmultiplier;
                } else if (p[0] == 1) {
                        y = exp(pQCDgr->Eval(x[0], 0, "S")) * pQCDmultiplier * pQCD1gr->Eval(x[0], 0, "S");
                } else if (p[0] == 2) {
                        y = exp(pQCDgr->Eval(x[0], 0, "S")) * pQCDmultiplier * pQCD2gr->Eval(x[0], 0, "S");
                } else if (p[0] == 3) {
                        y = exp(pQCDgr->Eval(x[0], 0, "S")) * pQCDmultiplier * x[0];
                } else if (p[0] == 10) {
			y = 1.0;
                } else if (p[0] == 11) {
                        y = pQCD1gr->Eval(x[0], 0, "S");
                } else if (p[0] == 12) {
                        y = pQCD2gr->Eval(x[0], 0, "S");
                }
        }
        return y;
}

// Vogelsang calculation
// KKP FF
Float_t nloXSect[] = {7e+10,   1.769E+09, 1.867E+08, 2.532E+07, 5.069E+06, 1.347E+06, 4.329E+05, 1.594E+05, 6.592E+04, 2.986E+04, 1.448E+04, 
7.452E+03, 4.029E+03, 2.276E+03, 1.323E+03, 7.979E+02, 4.946E+02, 3.128E+02, 2.026E+02, 1.336E+02, 8.977E+01, 6.126E+01, 4.223E+01, 
2.965E+01, 2.098E+01, 1.506E+01, 1.087E+01, 7.935E+00, 5.843E+00, 4.330E+00, 3.232E+00, 2.433E+00, 1.840E+00, 1.398E+00, 1.069E+00, 
8.220E-01, 6.337E-01, 4.909E-01, 3.818E-01, .984E-01};
// Kretzer FF
Float_t nloXSectK[] = {7.5e+11,   1.233E+09, 7.542E+07,9.887E+06,2.001E+06,5.456E+05,1.808E+05,6.890E+04,2.956E+04,1.381E+04,6.947E+03,3.688E+03,
2.051E+03,1.193E+03,7.117E+02,4.391E+02,2.786E+02,1.799E+02,1.187E+02,7.960E+01,5.443E+01,3.773E+01,2.637E+01,1.874E+01,1.341E+01,
9.719E+00,7.082E+00,5.217E+00,3.875E+00,2.893E+00,2.173E+00,1.645E+00,1.251E+00,9.548E-01,7.328E-01,5.653E-01,4.375E-01,3.398E-01,2.651E-01,2.077E-01};

//Pt for both:
// assuming values are at precise center of pT bins
Float_t nloPt[] = {0.4,   1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5,6.0,6.5,7.0,7.5,8.0,8.5,9.0,9.5,10.0,10.5,11.0,11.5,12.0,12.5,13.0,13.5,14.0,14.5,15.0,
15.5,16.0,16.5,17.0,17.5,18.0,18.5,19.0,19.5,20.0};
// assuming values are average in pT bins
//Float_t nloPt[] = {0.5,   0.96905, 1.45358, 1.95938, 2.46733, 2.97211, 3.47584, 3.97812, 4.48063, 4.98265, 5.48385, 5.98529, 6.48604, 6.98773, 7.48738, 7.98870, 
//8.48953, 8.98944, 9.49045, 9.99051, 10.4910, 10.9919, 11.4909, 11.9928, 12.4916, 12.9933, 13.4923, 13.9931, 14.4933, 14.9933, 15.4933, 15.9939, 16.4939, 
//16.9936, 17.4945, 17.9928, 18.4991, 18.9735, 19.4728, 19.9721};


// additional NLO results: KKP with different scales
Float_t nloXSectmu2[] = {5.0e+11,   1.126E+09, 8.320E+07, 1.194E+07, 2.525E+06, 7.014E+05, 2.333E+05, 8.847E+04, 3.742E+04, 1.719E+04, 8.440E+03, 4.390E+03, 
2.396E+03, 1.363E+03, 7.982E+02, 4.848E+02, 3.015E+02, 1.918E+02, 1.248E+02, 8.252E+01, 5.565E+01, 3.808E+01, 2.635E+01, 1.852E+01, 1.314E+01, 
9.447E+00, 6.835E+00, 4.994E+00, 3.681E+00, 2.732E+00, 2.040E+00, 1.536E+00, 1.163E+00, 8.839E-01, 6.759E-01, 5.195E-01, 4.007E-01, 3.103E-01, 2.414E-01, 1.887E-01};
Float_t nloXSectmu05[] = {7.0e+12,   4.931E+10, 5.666E+08, 5.814E+07, 1.152E+07, 2.987E+06, 8.516E+05, 2.924E+05, 1.134E+05, 4.894E+04, 2.289E+04, 1.145E+04, 
6.052E+03, 3.365E+03, 1.927E+03, 1.146E+03, 7.080E+02, 4.423E+02, 2.858E+02, 1.880E+02, 1.264E+02, 8.619E+01, 5.922E+01, 4.168E+01, 2.946E+01, 
2.119E+01, 1.532E+01, 1.117E+01, 8.256E+00, 6.113E+00, 4.575E+00, 3.444E+00, 2.613E+00, 1.990E+00, 1.523E+00, 1.175E+00, 9.079E-01, 7.042E-01, 5.485E-01, 4.291E-01};

TGraph *pQCDPPgr = 0;
TGraph *pQCDPPKgr = 0;
TGraph *pQCDPP1gr = 0;
TGraph *pQCDPP2gr = 0;
TWeightCalculator pQCDPPweight("pQCDPPweight", "pQCD p+p weight (KKP FF)");
TWeightCalculator pQCDPPKweight("pQCDPPKweight", "pQCD p+p weight (Kretzer FF)");
Double_t getNLOpQCDPP(Double_t *x, Double_t *p) {
        const Float_t pQCDPPmultiplier = (1 * 1 * 1e-9)/* / (30.0e-3)*/; // NSD cross section is 30.0 +/- 3.5 mb
        if (!pQCDPPgr) {
                Float_t *QyL = new Float_t[sizeof(nloXSect) / sizeof(nloXSect[0])];
                bin_stat_list_type pQCDPPspectrum;
                for (Int_t i = 0;i < Int_t(sizeof(nloXSect) / sizeof(nloXSect[0]));i++) {
                        QyL[i] = log(nloXSect[i]);
                        TBinParameters par;
                        par.min = nloPt[i];
                        par.max = nloPt[i];
                        TBinStatistics bin;
                        bin.setParameters(par);
                        bin.setValue(nloXSect[i] * pQCDPPmultiplier);
                        bin.setError(nloXSect[i] * pQCDPPmultiplier * 0.1);
                        pQCDPPspectrum.push_back(bin);
                }
                pQCDPPweight.Fit(pQCDPPspectrum);
                pQCDPPgr = new TGraph(sizeof(nloXSect) / sizeof(nloXSect[0]), nloPt, QyL);
        }
        if (!pQCDPPKgr) {
                Float_t *QyL = new Float_t[sizeof(nloXSectK) / sizeof(nloXSectK[0])];
                bin_stat_list_type pQCDPPspectrum;
                for (Int_t i = 0;i < Int_t(sizeof(nloXSectK) / sizeof(nloXSectK[0]));i++) {
                        QyL[i] = log(nloXSectK[i]);
                        TBinParameters par;
                        par.min = nloPt[i];
                        par.max = nloPt[i];
                        TBinStatistics bin;
                        bin.setParameters(par);
                        bin.setValue(nloXSectK[i] * pQCDPPmultiplier);
                        bin.setError(nloXSectK[i] * pQCDPPmultiplier * 0.1);
                        pQCDPPspectrum.push_back(bin);
                }
                pQCDPPKweight.Fit(pQCDPPspectrum);
                pQCDPPKgr = new TGraph(sizeof(nloXSectK) / sizeof(nloXSectK[0]), nloPt, QyL);
        }
        if (!pQCDPP1gr) {
                Float_t *Q1yR = new Float_t[sizeof(nloXSectmu05) / sizeof(nloXSectmu05[0])];
                for (Int_t i = 0;i < Int_t(sizeof(nloXSectmu05) / sizeof(nloXSectmu05[0]));i++) Q1yR[i] = nloXSectmu05[i] / nloXSect[i];
                pQCDPP1gr = new TGraph(sizeof(nloXSectmu05) / sizeof(nloXSectmu05[0]), nloPt, Q1yR);
        }
        if (!pQCDPP2gr) {
                Float_t *Q2yR = new Float_t[sizeof(nloXSectmu2) / sizeof(nloXSectmu2[0])];
                for (Int_t i = 0;i < Int_t(sizeof(nloXSectmu2) / sizeof(nloXSectmu2[0]));i++) Q2yR[i] = nloXSectmu2[i] / nloXSect[i];
                pQCDPP2gr = new TGraph(sizeof(nloXSectmu2) / sizeof(nloXSectmu2[0]), nloPt, Q2yR);
        }
        Float_t y = 0;
        if (x && p) {
                if (p[0] == 0) {
                        y = exp(pQCDPPgr->Eval(x[0], 0, "S")) * pQCDPPmultiplier;
                } else if (p[0] == 1) {
                        y = exp(pQCDPPgr->Eval(x[0], 0, "S")) * pQCDPPmultiplier * pQCDPP1gr->Eval(x[0], 0, "S");
                } else if (p[0] == 2) {
                        y = exp(pQCDPPgr->Eval(x[0], 0, "S")) * pQCDPPmultiplier * pQCDPP2gr->Eval(x[0], 0, "S");
                } else if (p[0] == 3) {
                        y = exp(pQCDPPgr->Eval(x[0], 0, "S")) * pQCDPPmultiplier * x[0];
                } else if (p[0] == 10) {
			y = 1.0;
                } else if (p[0] == 11) {
                        y = pQCDPP1gr->Eval(x[0], 0, "S");
                } else if (p[0] == 12) {
                        y = pQCDPP2gr->Eval(x[0], 0, "S");
                } else if (p[0] == 20) {
                        y = exp(pQCDPPKgr->Eval(x[0], 0, "S")) * pQCDPPmultiplier;
                }
        }
        return y;
}

#endif
