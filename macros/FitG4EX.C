//________________________________________________________________________________
Double_t gf4EXFunc(Double_t *x, Double_t *par) {
  Double_t XX[1] = {x[0]};
  // par[0] - norm
  // par[1] - pion position wrt Z_pion (Bichsel prediction)
  // par[2] - sigma 
  // par[3] - proton signal
  // par[4] - Kaon    -"-
  // par[5] - electorn -"-
  // par[6] - deuteron -"-
  // par[7] - Total
  // par[8] - case (-1 all, >-0 hyp no.)
  // par[9] - occupancy = probability to have 2d hits in the cluster 
  Double_t mu    = par[1];
  Double_t sigma = par[2];
  Double_t occupancy = par[9];
  //#define __BETA_SHIFT__
#ifdef __BETA_SHIFT__
  Double_t shift = par[12];
#else /* ! __BETA_SHIFT__ */
#define __RECOMBINATION__
#ifdef __RECOMBINATION__
  Double_t recombination = par[12];
#else /* ! __BETA_SHIFT__ && ! __RECOMBINATION__ */
#define __SCALE__
#ifdef __SCALE__
  Double_t scale = 1 + par[12];
  XX[0] *= scale;
#endif /* __SCALE__ */
#endif /* __RECOMBINATION__ */
#endif /* __BETA_SHIFT__ */
  Int_t IO = par[10];
  Int_t sign = par[11];
  Double_t frac[5];
  Double_t ff[5] = {0};
  for (Int_t i = 1; i < 5; i++) {
    ff[i] = TMath::Sin(par[2+i]);
    ff[i] *= ff[i];
  }
  frac[1] = ff[1];
  //  frac[0] = (1 - frac[1])/(1. + ff[2] + ff[3] + ff[4]);
  frac[0] = (1 - ff[1]*(1 + ff[4]))/(1 + ff[2] + ff[3]);
  frac[2] = frac[0]*ff[2];
  frac[3] = frac[0]*ff[3];
  frac[4] = ff[1]*ff[4];
  if (frac[0] < 0.4 && frac[1] < 0.4) return 0;
#ifdef __BETA_SHIFT__
  static const Char_t *hNames[5] = {"pion",       "proton",  "kaon",    "electron","deuteron"};
  static Double_t     hMasses[5] = {0.13956995, 0.93827231,0.493677, 0.51099907e-3,   2.80923};
  static Double_t pMoMIP = 0.526;
  static Double_t betaL[5] = {0};
  if (betaL[0] == 0.0) {
    for (Int_t i = 0; i < 5; i++) {
      Double_t bg = pMoMIP/hMasses[i];
      Double_t beta = bg/TMath::Sqrt(1. + bg*bg);
      betaL[i] = TMath::Log(beta);
    }
  }
#endif /* __BETA_SHIFT__ */
  //                            P  S IO
  static MIPFitPar_t parMIPs[5+15][3][2] = {
    {{
	// particle, norml, mu, sigma, alpha
	{"zIpionN",  12.49894,   -0.01516,    0.28723,    0.73392},
	{"zOpionN",  12.47612,   -0.05624,    0.27891,    0.79258} },{
	{"zIpionP",  12.89333,   -0.03122,    0.29183,    0.74512},
	{"zOpionP",  12.87095,   -0.06885,    0.28490,    0.81509} },{
	{"zIpion",  13.40867,   -0.02404,    0.29067,    0.74409},
	{"zOpion",  13.38591,   -0.06234,    0.28372,    0.81507} }},{{ // 0
	
	{"zIprotonN",  12.58330,    1.14718,    0.25439,    1.04387},
	{"zOprotonN",  12.36705,    1.15124,    0.24542,    1.36157} },{
	{"zIprotonP",  13.02326,    1.15043,    0.25458,    1.03516},
	{"zOprotonP",  12.81453,    1.15696,    0.24587,    1.33466} },{
	{"zIproton",  13.52026,    1.14975,    0.25495,    1.04605},
	{"zOproton",  13.30892,    1.15473,    0.24580,    1.34502} }},{{ // 1
	
	{"zIkaonN",  12.99150,    0.36075,    0.27950,    0.81084},
	{"zOkaonN",  12.86706,    0.35384,    0.27093,    0.88852} },{
	{"zIkaonP",  12.94309,    0.35420,    0.28282,    0.81350},
	{"zOkaonP",  12.80451,    0.34797,    0.26976,    0.87855} },{
	{"zIkaon",  13.66098,    0.35760,    0.28132,    0.81212},
	{"zOkaon",  13.52978,    0.35092,    0.27047,    0.88260} }},{{ // 2
	
	{"zIelectronN",  12.84218,    0.28655,    0.32475,    1.05302},
	{"zOelectronN",  12.82214,    0.23696,    0.26943,    0.89888} },{
	{"zIelectronP",  13.06714,    0.24452,    0.28483,    0.84283},
	{"zOelectronP",  12.96374,    0.23128,    0.27261,    0.90743} },{
	{"zIelectron",  13.68898,    0.24821,    0.28292,    0.83882},
	{"zOelectron",  13.55024,    0.28482,    0.30564,    1.55241} }},{{ // 3
	
	{"zIdeuteronP",  11.41599,    2.19135,    0.15619,    2.54299},
	{"zOdeuteronP",   9.98441,    2.10469,    0.13408,    2.40409} },{
	{"zIdeuteronP",  11.41599,    2.19135,    0.15619,    2.54299},
	{"zOdeuteronP",   9.98441,    2.10469,    0.13408,    2.40409} },{
	{"zIdeuteron",  11.41599,    2.19135,    0.15619,    2.54299},
	{"zOdeuteron",   9.98441,    2.10469,    0.13408,    2.40409} }},{{ // 4
	
	{"zIpionN+zIpionN",  12.48814,    0.75604,    0.23106,    0.77915},
	{"zOpionN+zOpionN",  12.47080,    0.70275,    0.22213,    0.81399} },{
	{"zIpionP+zIpionP",  12.88593,    0.73568,    0.23279,    0.76316},
	{"zOpionP+zOpionP",  12.86773,    0.68325,    0.22190,    0.79262} },{
	{"zIpion+zIpion",  13.39967,    0.74540,    0.23322,    0.77851},
	{"zOpion+zOpion",  13.38194,    0.69226,    0.22290,    0.80911} }},{{ // 5 = 0 + 0
	
	{"zIpionN+zIprotonN",  12.49140,    1.47603,    0.22231,    1.08234},
	{"zOpionN+zOprotonN",  12.47317,    1.45213,    0.20852,    1.19349} },{
	{"zIpionP+zIprotonP",  12.88707,    1.46950,    0.22062,    1.01891},
	{"zOpionP+zOprotonP",  12.86717,    1.45228,    0.20922,    1.18455} },{
	{"zIpion+zIproton",  13.40111,    1.47345,    0.22211,    1.06101},
	{"zOpion+zOproton",  13.38274,    1.45229,    0.20911,    1.18788} }},{{ // 6 = 0 + 1
	
	{"zIpionN+zIkaonN",  12.49067,    0.95550,    0.22862,    0.83032},
	{"zOpionN+zOkaonN",  12.47136,    0.92277,    0.21847,    0.87325} },{
	{"zIpionP+zIkaonP",  12.88717,    0.94037,    0.22877,    0.80558},
	{"zOpionP+zOkaonP",  12.86450,    0.91509,    0.22042,    0.89346} },{
	{"zIpion+zIkaon",  13.39865,    0.95187,    0.23112,    0.85475},
	{"zOpion+zOkaon",  13.38175,    0.91592,    0.21806,    0.86083} }},{{ // 7 = 0 + 2
	
	{"zIpionN+zIelectronN",  12.49219,    0.88970,    0.22683,    0.81303},
	{"zOpionN+zOelectronN",  12.47158,    0.85333,    0.21662,    0.86648} },{
	{"zIpionP+zIelectronP",  12.88753,    0.87506,    0.22809,    0.80547},
	{"zOpionP+zOelectronP",  12.86590,    0.84272,    0.21848,    0.86847} },{
	{"zIpion+zIelectron",  13.40057,    0.88420,    0.22882,    0.83175},
	{"zOpion+zOelectron",  13.38114,    0.84768,    0.21791,    0.87049} }},{{ // 8 = 0 + 3
	
	{"zIpionP+zIdeuteronP",  12.89576,    2.31634,    0.14983,    1.69887},
	{"zOpionP+zOdeuteronP",  12.87313,    2.23009,    0.12942,    1.64580} },{
	{"zIpionP+zIdeuteronP",  12.89576,    2.31634,    0.14983,    1.69887},
	{"zOpionP+zOdeuteronP",  12.87313,    2.23009,    0.12942,    1.64580} },{
	{"zIpion+zIdeuteron",  13.41140,    2.31716,    0.14955,    1.68935},
	{"zOpion+zOdeuteron",  13.38872,    2.23089,    0.12946,    1.64750} }},{{ // 9 = 0 + 4
	
	{"zIprotonN+zIprotonN",  12.57827,    1.88631,    0.19825,    1.18853},
	{"zOprotonN+zOprotonN",  12.36900,    1.87207,    0.18438,    1.44194} },{
	{"zIprotonP+zIprotonP",  13.01975,    1.88891,    0.19796,    1.13906},
	{"zOprotonP+zOprotonP",  12.81526,    1.87865,    0.18481,    1.41587} },{
	{"zIproton+zIproton",  13.51495,    1.88953,    0.19910,    1.19310},
	{"zOproton+zOproton",  13.31006,    1.87615,    0.18472,    1.42727} }},{{ // 10 = 1 + 1
	
	{"zIprotonN+zIkaonN",  12.57679,    1.57700,    0.21373,    1.07656},
	{"zOprotonN+zOkaonN",  12.36760,    1.56204,    0.20082,    1.20026} },{
	{"zIprotonP+zIkaonP",  13.01751,    1.57718,    0.21457,    1.06758},
	{"zOprotonP+zOkaonP",  12.81421,    1.56559,    0.20153,    1.20170} },{
	{"zIproton+zIkaon",  13.51442,    1.57752,    0.21428,    1.07208},
	{"zOproton+zOkaon",  13.30882,    1.56440,    0.20124,    1.20040} }},{{ // 11 = 1 + 2
	
	{"zIprotonN+zIelectronN",  12.58045,    1.53788,    0.21268,    1.02838},
	{"zOprotonN+zOelectronN",  12.36733,    1.52603,    0.20185,    1.24158} },{
	{"zIprotonP+zIelectronP",  13.01848,    1.54110,    0.21555,    1.07802},
	{"zOprotonP+zOelectronP",  12.81378,    1.52946,    0.20335,    1.24689} },{
	{"zIproton+zIelectron",  13.51579,    1.54137,    0.21493,    1.07437},
	{"zOproton+zOelectron",  13.30856,    1.52834,    0.20278,    1.24434} }},{{ // 12 = 1 + 3
	
	{"zIprotonP+zIdeuteronP",  13.02825,    2.51395,    0.14094,    1.53430},
	{"zOprotonP+zOdeuteronP",  12.82028,    2.44511,    0.12453,    1.74580} },{
	{"zIprotonP+zIdeuteronP",  13.02825,    2.51395,    0.14094,    1.53430},
	{"zOprotonP+zOdeuteronP",  12.82028,    2.44511,    0.12453,    1.74580} },{
	{"zIproton+zIdeuteron",  13.52522,    2.51341,    0.14092,    1.54556},
	{"zOproton+zOdeuteron",  13.31487,    2.44441,    0.12439,    1.76094} }},{{ // 13 = 1 + 4
	
	{"zIkaonN+zIkaonN",  12.98374,    1.12011,    0.22224,    0.85729},
	{"zOkaonN+zOkaonN",  12.86319,    1.10253,    0.21191,    0.90889} },{
	{"zIkaonP+zIkaonP",  12.93511,    1.11402,    0.22446,    0.86289},
	{"zOkaonP+zOkaonP",  12.80057,    1.10040,    0.21426,    0.93064} },{
	{"zIkaon+zIkaon",  13.64990,    1.12119,    0.22548,    0.89932},
	{"zOkaon+zOkaon",  13.52510,    1.10249,    0.21362,    0.92982} }},{{ // 14 = 2 + 2
	
	{"zIkaonN+zIelectronN",  12.98296,    1.06810,    0.22327,    0.87807},
	{"zOkaonN+zOelectronN",  12.86317,    1.04539,    0.21202,    0.91706} },{
	{"zIkaonP+zIelectronP",  12.93579,    1.05893,    0.22472,    0.86766},
	{"zOkaonP+zOelectronP",  12.80091,    1.04107,    0.21383,    0.92498} },{
	{"zIkaon+zIelectron",  13.65346,    1.06240,    0.22353,    0.86445},
	{"zOkaon+zOelectron",  13.52493,    1.04497,    0.21381,    0.94042} }},{{ // 15 = 2 + 3
	
	{"zIkaonP+zIdeuteronP",  12.94524,    2.36193,    0.14717,    1.60177},
	{"zOkaonP+zOdeuteronP",  12.80918,    2.28302,    0.12821,    1.59473} },{
	{"zIkaonP+zIdeuteronP",  12.94524,    2.36193,    0.14717,    1.60177},
	{"zOkaonP+zOdeuteronP",  12.80918,    2.28302,    0.12821,    1.59473} },{
	{"zIkaon+zIdeuteron",  13.66315,    2.36243,    0.14705,    1.60166},
	{"zOkaon+zOdeuteron",  13.53389,    2.28321,    0.12801,    1.58450} }},{{ // 16 = 2 + 4
	
	{"zIelectronN+zIelectronN",  12.91257,    1.00946,    0.22216,    0.86696}, 
	{"zOelectronN+zOelectronN",  12.81809,    0.98630,    0.21205,    0.93703} },{
	{"zIelectronP+zIelectronP",  13.06008,    1.00154,    0.22411,    0.88141},
	{"zOelectronP+zOelectronP",  12.95818,    0.98334,    0.21572,    0.97355} },{
	{"zIelectron+zIelectron",  13.68012,    1.00926,    0.22582,    0.91292},
	{"zOelectron+zOelectron",  13.58465,    0.98341,    0.21336,    0.94212} }},{{ // 17 = 3 + 3
	
	{"zIelectronP+zIdeuteronP",  13.07082,    2.34628,    0.14769,    1.78049},
	{"zOelectronP+zOdeuteronP",  12.96880,    2.26540,    0.12823,    1.72863} },{
	{"zIelectronP+zIdeuteronP",  13.07082,    2.34628,    0.14769,    1.78049},
	{"zOelectronP+zOdeuteronP",  12.96880,    2.26540,    0.12823,    1.72863} },{
	{"zIelectron+zIdeuteron",  13.69275,    2.34685,    0.14769,    1.78342},
	{"zOelectron+zOdeuteron",  13.59348,    2.26575,    0.12815,    1.72507} }},{{ // 18 = 3 + 4
	
	{"zIdeuteronP+zIdeuteronP",  11.41668,    2.89079,    0.11240,    2.65575},
	{"zOdeuteronP+zOdeuteronP",   9.98603,    2.80277,    0.09768,    2.84616} },{
	{"zIdeuteronP+zIdeuteronP",  11.41668,    2.89079,    0.11240,    2.65575},
	{"zOdeuteronP+zOdeuteronP",   9.98603,    2.80277,    0.09768,    2.84616} },{
	{"zIdeuteron+zIdeuteron",  11.41672,    2.89112,    0.11231,    2.57210},
	{"zOdeuteron+zOdeuteron",   9.98621,    2.80256,    0.09789,    2.92922} }} // 19 = 4 + 4
  };
#ifdef __RECOMBINATION__  
  /*
                        bgL10min Tcut            bgL10MIP     bg  log(beta)                 dN/dx    scale = -1.13000e-01; alpha = 1.13000e-01/9.27077905202451547e+01 = 1.21888354113372510e-03
                                                                                                                    scale = 1/(1 +alpha*dNdx) - 1 

minimu log10(p/m) alpga    -0.7   40 keV          -0.850  0.141 -1.96883836551568292e+00  3.45122406159667935e+03  -0.807
                  He3      -0.7   40              -0.727  0.187 -1.69383239941652541e+00  2.13316907245180437e+03  -0.722
                  deuteron -1.0    1 keV          -0.552  0.280 -1.31070490640084025e+00  2.70689788452519849e+02  -0.248
                  proton   -1.0    1 keV          -0.251  0.561 -7.14846611368781581e-01  9.27077905202451547e+01  -0.101
                  kaon     -0.5  100 keV           0.027  1.064 -3.16517375796772749e-01  4.48350287856880172e+01
                  pion     -0.4  100 keV  bgL10 =  0.576  3.767 -3.40492423408368661e-02  2.97372112287192998e+01
                  e         2.0  100 keV           3.012  1028. -4.73133358730884409e-07  4.03895078708591129e+01

  */
  //                           "pion",       "proton",  "kaon",    "electron","deuteron"
  static Double_t dNdxMIP[5] = {29.734,        92.708,   44.835,        40.389, 270.689};
#endif /* __RECOMBINATION__  */
  Double_t Value = 0;
  Int_t icase = (Int_t) par[8];
  Int_t i1 = 0;
  Int_t i2 = 4;
  if (icase >= 0) {i1 = i2 = icase;}
  TF1 *g = GausExp();
  static Int_t _debug = 0;
  for (Int_t i = i1; i <= i2; i++) { 
    Double_t Mu = mu + parMIPs[i][sign][IO].mu - parMIPs[0][sign][IO].mu;
#ifdef __BETA_SHIFT__
    Double_t dbetaL = betaL[i] - betaL[0];
    if (dbetaL > 0) dbetaL = 0;
    Mu += shift*dbetaL;
#endif /* __BETA_SHIFT__ */
#ifdef __RECOMBINATION__ 
    //    Double_t ar = recombination*dNdxMIP[i];
    //    if (ar > -1)  Mu += - TMath::Log(1 + ar);
    Double_t alpha = recombination;
    Double_t recom = (1. - alpha*dNdxMIP[i])/(1. - alpha*dNdxMIP[0]);
    if (recom < 0.8) recom = 0.8;
    Mu += TMath::Log(recom);
#endif
    Double_t pars[4] = {0, Mu, parMIPs[i][sign][IO].sigma + sigma, parMIPs[i][sign][IO].alpha};
    Value += frac[i]*g->EvalPar(XX, pars);
    if (_debug) {
      cout << "i: " << i << " " << parMIPs[i][sign][IO].Name << " frac[" << i <<"] = " << frac[i] << "\t"; parMIPs[i][sign][IO].Print();
    }
  }
  if (occupancy > 0) {
    static Int_t indx[5][5] = {0};
    if (indx[0][0] == 0) {
      Int_t ii = 5;
      for (Int_t j = 0; j < 5; j++) {
	for (Int_t i = 0; i < 5; i++) {
	  if (j <= i) {
	    indx[i][j] = ii;
	    ii++;
	  } else {
	    indx[i][j] = indx[j][i];
	  }
	}
      }
    }
    Double_t overlap = 0;
    for (Int_t i = i1; i <= i2; i++) { 
      Double_t cont = 0;
      for (Int_t j = 0; j < 5; j++) {
	Int_t l = indx[i][j];
	Double_t Mu = mu + parMIPs[l][sign][IO].mu - parMIPs[0][sign][IO].mu;
	Double_t pars[4] = {0, Mu, parMIPs[l][sign][IO].sigma + sigma, parMIPs[l][sign][IO].alpha};
	cont += frac[j]*g->EvalPar(XX, pars);
	if (_debug) {
	  cout << "i:" << i << " " << parMIPs[i][sign][IO].Name << "\t+ j:" << j << " " <<  parMIPs[j][sign][IO].Name << "\t frac[" << j <<"] = " << frac[j] << "\t l:" << l << " "; parMIPs[l][sign][IO].Print();
	}
      }
      overlap += frac[i]*cont;
    }
    Value += occupancy*overlap;
  }
  return par[7]*TMath::Exp(par[0])*Value;
}
//________________________________________________________________________________
TF1 *FitG4EX(TH1 *proj, Option_t *opt="RM", Int_t IO = 0, Int_t Sign = 2) {
  // fit in momentum range p = 0.526 +/- 0.05;
  if (! proj) return 0;
  TString Opt(opt);
  //  Bool_t quet = Opt.Contains("Q",TString::kIgnoreCase);
  TF1 *g2 = (TF1*) gROOT->GetFunction("G4EX");
  if (! g2) {
    g2 = new TF1("G4EX",gf4EXFunc, -5, 5, 13);
    g2->SetParName(0,"norm");      g2->SetParLimits(0,-0.2,0.2); // g2->FixParameter(0,0.0); // 
    g2->SetParName(1,"mu");        g2->SetParLimits(1,-0.2,0.4);				     
    g2->SetParName(2,"Sigma");     g2->FixParameter(2,0.0); g2->SetParLimits(2,0.0,0.1);	     
    g2->SetParName(3,"P");         g2->SetParLimits(3,0.0,1.2); // TMath::Pi()/2);		     
    g2->SetParName(4,"K");         g2->SetParLimits(4,0.0,0.6); // TMath::Pi()/2); 	     
    g2->SetParName(5,"e");         g2->SetParLimits(5,0.0,TMath::Pi()/2);			     
    g2->SetParName(6,"d");         g2->SetParLimits(6,0.0, 1.0); // TMath::Pi()/2);             
    g2->SetParName(7,"Total");
    g2->SetParName(8,"Case");
    g2->SetParName(9,"occupancy"); g2->FixParameter(9,0); g2->SetParLimits(9,0.0,0.25);
    g2->SetParName(10,"IO");       g2->FixParameter(10,IO); 
    g2->SetParName(11,"sign");     g2->FixParameter(11,Sign); 
#ifdef __RECOMBINATION__
    g2->SetParName(12,"recombin"); {g2->FixParameter(12,0.0); g2->SetParLimits(12,-0.01,0.01);}
#endif
#ifdef __BETA_SHIFT__
    g2->SetParName(12,"b-shift"); {g2->FixParameter(12,0.0); g2->SetParLimits(12,-0.1,0.1);}
#endif
#ifdef __SCALE__
    g2->SetParName(12,"scale"); {g2->FixParameter(12,0.0); g2->SetParLimits(12,-0.1,0.1);}
#endif
    //    g2->SetParName(7,"factor"); g2->SetParLimits(7,-.1,0.1);
  }
  PreSetParameters(proj, g2);
  g2->FixParameter(2, 0.0);
  g2->ReleaseParameter(3);
  g2->FixParameter(4,0.01); 
  g2->FixParameter(5,0.01);
  g2->FixParameter(6,0.01);
  g2->FixParameter(9,0.);
  g2->FixParameter(10,IO); 
  g2->FixParameter(11,Sign); 
  g2->FixParameter(12,0.0);
  proj->Fit(g2,Opt.Data());
  //  g2->ReleaseParameter(2);
  g2->ReleaseParameter(4);     g2->SetParLimits(4,0.0,0.6); // TMath::Pi()/2); 
  g2->ReleaseParameter(5);     g2->SetParLimits(5,0.0,TMath::Pi()/2);		
  g2->ReleaseParameter(6);     g2->SetParLimits(6,0.0, 1.0); // TMath::Pi()/2);
  //  g2->ReleaseParameter(9);
  //  g2->ReleaseParameter(9); g2->SetParLimits(9,-2.0,2.0);
  Int_t iok = proj->Fit(g2,Opt.Data());
  if ( iok < 0) {
    cout << g2->GetName() << " fit has failed with " << iok << " for " 
	 << proj->GetName() << "/" << proj->GetTitle() << " Try one again" << endl; 
    proj->Fit(g2,Opt.Data());
  }
#if 0
  g2->ReleaseParameter(12);
  g2->SetParameter(12,0.);
  iok = proj->Fit(g2,Opt.Data());
  if ( iok < 0) {
    cout << g2->GetName() << " fit has failed with " << iok << " for " 
	 << proj->GetName() << "/" << proj->GetTitle() << " Try one again" << endl; 
    proj->Fit(g2,Opt.Data());
  }
#endif
  g2->ReleaseParameter(9);
  iok = proj->Fit(g2,Opt.Data());
  if ( iok < 0) {
    cout << g2->GetName() << " fit has failed with " << iok << " for " 
	 << proj->GetName() << "/" << proj->GetTitle() << " Try one again" << endl; 
    proj->Fit(g2,Opt.Data());
  }
  Opt += "m";
  iok = proj->Fit(g2,Opt.Data());
  if (iok < 0 ) return 0;
  if (! Opt.Contains("q",TString::kIgnoreCase)) {
    Double_t params[20];
    g2->GetParameters(params);
    Double_t X = params[1];
    Double_t Y = TMath::Exp(params[0]);
    TPolyMarker *pm = new TPolyMarker(1, &X, &Y);
    proj->GetListOfFunctions()->Add(pm);
    pm->SetMarkerStyle(23);
    pm->SetMarkerColor(kRed);
    pm->SetMarkerSize(1.3);
    Double_t occupancy = params[9];
    for (int i = 0; i <= 4; i++) {
      TF1 *f = new TF1(*g2);
      f->SetName(Peaks[i].Name);
      f->FixParameter(8,i);
      f->SetLineColor(i+2);
      proj->GetListOfFunctions()->Add(f);
      if (occupancy > 0) {
	TF1 *f = new TF1(*g2);
	f->SetName(TString(Peaks[i].Name)+"C");
	f->FixParameter(8,i);
	f->FixParameter(9,0);
	f->SetLineColor(i+2);
	f->SetLineStyle(2);
	proj->GetListOfFunctions()->Add(f);
      }
    }
    proj->Draw();
  }
  return g2;
}
