//----------------------------------------------------------------------------
// Implementation of the KFParticle class
// .
// @author  I.Kisel, I.Kulakov, M.Zyzak
// @version 1.0
// @since   20.08.13
// 
// 
//  -= Copyright &copy ALICE HLT and CBM L1 Groups =-
//____________________________________________________________________________

#ifndef KFPartEfficiencies_H
#define KFPartEfficiencies_H

#ifndef HLTCA_STANDALONE
#include "TNamed.h"
#endif

#include <map>
#include <iomanip>
#include "KFMCCounter.h"

class KFPartEfficiencies: public TNamed
{
 public:

  KFPartEfficiencies():
    partDaughterPdg(0),
    names(),
    indices(),
    fPdgToIndex(),
    ratio_reco1(),
    ratio_reco2(),
    ratio_reco3(),
    mc1(),
    mc2(),
    mc3(),
    reco(),
    ratio_ghost(),
    ratio_bg(),
    ratio_clone(),
    ghost(),
    bg(),
    clone()
  {
    int mPartPDG[nParticles] = {310,3122,-3122,3312,-3312,3322,-3322,3334,-3334,3212,-3212,3222,-3222, //strange meson and hyperons
                                313,-313,323,-323, 100313, 100323, -100323,//K* resonances
                                3224,3114,-3114,-3224, 3214, -3214,//sigma resonances
                                3124,-3124, //Lambda resonances
                                3324, -3324, 1003314, -1003314, 3314, -3314, //Xi resonances
                                1003334, -1003334, //Omega resonances
                                3000, //exotics
                                333,113, //vector mesons, hadron chanel
                                100113, 200113, //light vector mesons
                                22, //dielectrons
                                111,221, //pi0, eta
                                443,100443, // J/Psi
                                421,-421,429,-429, 428, //D0
                                411,-411, //D+, D-
                                431,-431, //Ds+, Ds-
                                4122,-4122, //Lambdac
                                10421, -10421, 10411, -10411, 20411, -20411, 10428,
                                3001, //H->Lambda p pi
                                3003, -3003, 3004, -3004, 3005, -3005, 3006, -3006, 3007, -3007, // hyper nuclei
                                11, -11, 13, -13, 211,  -211, 321, -321, 2212, -2212, // stable particles
                                1000010020, -1000010020, 1000010030, -1000010030, 1000020030, -1000020030, 1000020040, -1000020040, // stable fragments
                                123456789 //V0
                               };
    TString mPartName[nParticles] = {"ks","lambda","lambdab","xi-","xi+","xi0","xi0b","omega-","omega+","#Sigma^0","#Sigma^0b", "#Sigma^+", "#Sigma^+b",
                                     "k*0","k*0b","k*+","k*-", "k*0_{K0,#pi0}", "k*+_{K+,#pi0}", "k*-_{K-,#pi0}",
                                     "sigma*+","sigma*-","sigma*+b","sigma*-b","sigma*0","sigma*0b",
                                     "lambda*","lambda*b",
                                     "xi*0", "xi*0b", "xi*-_{#Lambda,K}", "xi*+_{#Lambda,K}", "xi*-_{#xi-,#pi0}", "xi*+_{#xi+,#pi0}",
                                     "omega*-","omega*+",
                                     "Hdb",
                                     "phi_{KK}", "rho_{#pi#pi}",
                                     "rho_{ee}", "rho_{#mu#mu}",
                                     "gamma",
                                     "#pi^{0}","eta",
                                     "J#Psi_ee","J#Psi_#mu#mu",
                                     "D0","D0b","D0_4","D0b_4", "D0_{#pi0}",
                                     "D+","D-",
                                     "Ds+","Ds-",
                                     "lambdac", "lambdacb",
                                     "D*0", "D*0b", "D*+", "D*-", "D*+_4", "D*-_4", "D0*_{#pi0}",
                                     "H0",
                                     "LambdaN","LambdaNb","H3L","H3Lb","H4L","H4Lb","He4L","He4Lb","He5L","He5Lb",
                                     "e-", "e+", "mu-", "mu+", "pi+", "pi-", "K+", "K-", "p+", "p-",
                                     "d+", "d-", "t+", "t-", "He3+", "He3-", "He4+", "He4-",
                                     "V0"
                                    };
    TString mPartTitle[nParticles] = {"KShort   ", //0
                                      "Lambda   ", //1
                                      "Lambda b ", //2
                                      "Xi-      ", //3
                                      "Xi+      ", //4
                                      "Xi0      ", //5
                                      "Xi0 b    ", //6
                                      "Omega-   ", //7
                                      "Omega+   ", //8
                                      "Sigma0   ", //9
                                      "Sigma0 b ", //10
                                      "Sigma+   ", //11
                                      "Sigma+ b ", //12
                                      "K*0      ", //13
                                      "K*0 b    ", //14
                                      "K*+      ", //15
                                      "K*-      ", //16
                                      "K*0_K0pi0", //17
                                      "K*+_K+pi0", //18
                                      "K*-_K-pi0", //19
                                      "Sigma*+  ", //20
                                      "Sigma*-  ", //21
                                      "Sigma*+ b", //22
                                      "Sigma*- b", //23
                                      "Sigma*0  ", //24
                                      "Sigma*0 b", //25
                                      "Lambda*  ", //26
                                      "Lambda* b", //27
                                      "Xi*0     ", //28
                                      "Xi*0 b   ", //29
                                      "Xi*-_lk  ", //30
                                      "Xi*+_lk  ", //31
                                      "Xi*-_XiPi", //32
                                      "Xi*+_XiPi", //33
                                      "Omega*-  ", //34
                                      "Omega*+  ", //35
                                      "Hdb      ", //36
                                      "phi_kk   ", //37
                                      "rho_pipi ", //38
                                      "rho_ee   ", //39
                                      "rho_mm   ", //40
                                      "gamma    ", //41
                                      "Pi0      ", //42
                                      "eta      ", //43
                                      "JPsi_ee  ", //44
                                      "JPsi_mm  ", //45
                                      "D0       ", //46
                                      "D0b      ", //47
                                      "D0_4     ", //48
                                      "D0b_4    ", //49
                                      "D0_#pi0  ", //50
                                      "D+       ", //51
                                      "D-       ", //52
                                      "Ds+      ", //53
                                      "Ds-      ", //54
                                      "Lambdac  ", //55
                                      "Lambdac b", //56
                                      "D*0      ", //57
                                      "D*0 b    ", //58
                                      "D*+      ", //59
                                      "D*-      ", //60
                                      "D*+_4    ", //61
                                      "D*-_4    ", //62
                                      "D0*_#pi0 ", //63
                                      "H0       ", //64
                                      "LambdaN  ", //65
                                      "LambdaN b", //66
                                      "H3L      ", //67
                                      "H3L b    ", //68
                                      "H4L      ", //69
                                      "H4L b    ", //70
                                      "He4L     ", //71
                                      "He4L b   ", //72
                                      "He5L     ", //73
                                      "He5L b   ", //74
                                      "e-       ", //75
                                      "e+       ", //76
                                      "mu-      ", //77
                                      "mu+      ", //78
                                      "pi+      ", //79
                                      "pi-      ", //80
                                      "K+       ", //81
                                      "K-       ", //82
                                      "p+       ", //83
                                      "p-       ", //84
                                      "d+       ", //85
                                      "d-       ", //86
                                      "t+       ", //87
                                      "t-       ", //88
                                      "He3+     ", //89
                                      "He3-     ", //90
                                      "He4+     ", //91
                                      "He4-     ", //92
                                      "V0       " //93
                                     };

    float mPartMHistoMin[nParticles] = {0.3, 1., 1., 1., 1., 1., 1.,1.,1.,1.,1.,1.,1.,
                                        0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6,
                                        1.,1.,1.,1.,1.,1.,
                                        1.4, 1.4,
                                        1.4, 1.4, 1.4, 1.4, 1.4, 1.4,
                                        1.8,1.8,
                                        1.,
                                        0.8, 0.1,
                                        0.1, 0.1,
                                        0.,
                                        0.,0.,
                                        1.,1.,
                                        1.,1.,1.,1.,1.,
                                        1.,1.,
                                        1.,1.,
                                        1.8,1.8,
                                        1.8,1.8,1.8,1.8,1.8,1.8,1.8,
                                        1.,
                                        1., 1., 2., 2., 3., 3., 3., 3., 4., 4.,
                                        0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
                                        0., 0., 0., 0., 0., 0., 0., 0., 
                                        0.3 };
    float mPartMHistoMax[nParticles] = {1.3, 2., 2., 3., 3., 3., 3., 3., 3.,3.,3.,3.,3.,
                                        2.6, 2.6, 2.6, 2.6, 2.6, 2.6, 2.6,
                                        3., 3., 3., 3.,3.,3.,
                                        3.4, 3.4,
                                        3.4, 3.4, 3.4, 3.4, 3.4, 3.4,
                                        3.8, 3.8,
                                        3.,
                                        2.8, 2.1,
                                        2.1, 2.1,
                                        3.,
                                        3.,3.,
                                        4.,4.,
                                        3.,3.,3.,3.,3.,
                                        3.,3.,
                                        3.,3.,
                                        3.8,3.8,
                                        3.8,3.8,3.8,3.8,3.8,3.8,3.8,
                                        3.,
                                        3., 3., 4., 4., 5., 5., 5., 5., 6., 6.,
                                        0.01, 0.01, 1., 1., 1., 1., 1., 1., 1.5, 1.5,
                                        2.5, 2.5, 3.5, 3.5, 3.5, 3.5, 4.5, 4.5,
                                        1.3};
                                        
    int mPartMaxMult[nParticles];
    for(int i=0; i<nParticles; i++)
      mPartMaxMult[i] = 20;
    mPartMaxMult[75] = 20;
    mPartMaxMult[76] = 20;
    mPartMaxMult[77] = 20;
    mPartMaxMult[78] = 20;
    mPartMaxMult[79] = 500;
    mPartMaxMult[80] = 500;
    mPartMaxMult[81] = 50;
    mPartMaxMult[82] = 50;
    mPartMaxMult[83] = 500;
    mPartMaxMult[84] = 20;

    float mPartMass[nParticles] = { 0.497614, // 0 K0
                                    1.115683, // 1 Lambda
                                    1.115683, // 2 Lambda b
                                    1.32171,  // 3 Xi-
                                    1.32171,  // 4 Xi+
                                    1.31486,  // 5 Xi0
                                    1.31486,  // 6 Xi0 b
                                    1.67245,  // 7 Omega-
                                    1.67245,  // 8 Omega+
                                    1.192642, // 9 Sigma0 
                                    1.192642, //10 Sigma0 b
                                    1.18937,  //11 Sigma+
                                    1.18937,  //12 Sigma+ b
                                    0.8958,   //13 K*0
                                    0.8958,   //14 K*0 b
                                    0.89166,  //15 K*+
                                    0.89166,  //16 K*-
                                    0.8958,   //17 K*0
                                    0.89166,  //18 K*+
                                    0.89166,  //19 K*-
                                    1.3828,   //20 Sigma*+
                                    1.3872,   //21 Sigma*-
                                    1.3828,   //22 Sigma*+ b
                                    1.3872,   //23 Sigma*- b 
                                    1.3837,   //24 Sigma*0 
                                    1.3837,   //25 Sigma*0 b
                                    1.5195,   //26 Labmda*
                                    1.5195,   //27 Labmda* b
                                    1.53180,  //28 Xi*0
                                    1.53180,  //29 Xi*0 b
                                    1.823,    //30 Xi*-
                                    1.823,    //31 Xi*+
                                    1.535,    //32 Xi*- 
                                    1.535,    //33 Xi*+
                                    2.252,    //34 Omega*-
                                    2.252,    //35 Omega*+
                                    2.21,     //36 Hdb
                                    1.019455, //37 phi
                                    0.77526,  //38 rho
                                    0.77526,  //39 rho
                                    0.77526,  //40 rho
                                    0.,       //41 gamma
                                    0.1349766,//42 pi0
                                    0.547862, //43 eta0
                                    3.096916, //44 J/Psi
                                    3.096916, //45 J/Psi
                                    1.86486,  //46 D0
                                    1.86486,  //47 D0 b
                                    1.86486,  //48 D0
                                    1.86486,  //49 D0 b
                                    1.86486,  //50 D0 ->K0pi+pi-pi0
                                    1.86962,  //51 D+
                                    1.86962,  //52 D-
                                    1.96850,  //53 Ds+
                                    1.96850,  //54 Ds-
                                    2.28646,  //55 Lambdac
                                    2.28646,  //56 Lambdac b
                                    2.00699,  //57 D*0
                                    2.00699,  //58 D*0 b
                                    2.01029,  //59 D*+
                                    2.01029,  //60 D*-
                                    2.01029,  //61 D*+
                                    2.01029,  //62 D*-
                                    2.00699,  //63 D*0 ->D0pi0
                                    2.21,     //64 H0
                                    2.05395,  //65 LambdaN
                                    2.05395,  //66 LambdaN b
                                    2.99339,  //67 H3L
                                    2.99339,  //68 H3L b
                                    3.92975,  //69 H4L
                                    3.92975,  //70 H4L b
                                    3.93070,  //71 He4L
                                    3.93070,  //72 He4L b
                                    4.86824,  //73 He5L
                                    4.86824,  //74 He5L b
                                    5.485799e-4, //75 e-
                                    5.485799e-4, //76 e+
                                    0.105658, //77 mu-
                                    0.105658, //78 mu+
                                    0.139570, //79 pi+
                                    0.139570, //80 pi-
                                    0.493677, //81 K+
                                    0.493677, //82 K-
                                    0.938272, //83 p
                                    0.938272, //84 p-
                                    1.876124, //85 d+
                                    1.876124, //86 d-
                                    2.809432, //87 t+
                                    2.809432, //88 t+
                                    2.809413, //89 He3+
                                    2.809413, //90 He3-
                                    3.728400, //91 He4+
                                    3.728400, //92 He4-
                                    0};       //93 V0
                                    
    float mPartLifeTime[nParticles] = { 8.954e-11, // 0 K0
                                        2.632e-10, // 1 Lambda
                                        2.632e-10, // 2 Lambda b
                                        1.639e-10, // 3 Xi-
                                        1.7e-10,   // 4 Xi+
                                        2.9e-10,   // 5 Xi0
                                        2.9e-10,   // 6 Xi0 b
                                        0.821e-10, // 7 Omega-
                                        0.821e-10, // 8 Omega+
                                        7.4e-20,   // 9 Sigma0 
                                        7.4e-20,   //10 Sigma0 b
                                        0.8018e-10,//11 Sigma+
                                        0.8018e-10,//12 Sigma+ b
                                        1.38e-23,  //13 K*0
                                        1.38e-23,  //14 K*0 b
                                        1.30e-23,  //15 K*+
                                        1.30e-23,  //16 K*-
                                        1.38e-23,  //17 K*0
                                        1.30e-23,  //18 K*+
                                        1.30e-23,  //19 K*-
                                        1.83e-23,   //20 Sigma*+
                                        1.67e-23,   //21 Sigma*-
                                        1.83e-23,   //22 Sigma*+ b
                                        1.67e-23,   //23 Sigma*- b 
                                        1.83e-23,   //24 Sigma*0 
                                        1.83e-23,   //25 Sigma*0 b
                                        4.22e-23,   //26 Labmda*
                                        4.22e-23,   //27 Labmda* b
                                        7.23e-23,   //28 Xi*0
                                        7.23e-23,   //29 Xi*0 b
                                        2.74e-23,   //30 Xi*-
                                        2.74e-23,   //31 Xi*+
                                        6.65e-23,   //32 Xi*-
                                        6.65e-23,   //33 Xi*+
                                        1.2e-23,    //34 Omega*-
                                        1.2e-23,    //35 Omega*+
                                        1.32e-10,   //36 Hdb
                                        1.55e-22,   //37 phi
                                        4.45e-24,   //38 rho
                                        4.45e-24,   //39 rho
                                        4.45e-24,   //40 rho
                                        1.e20,      //41 gamma
                                        8.52e-17,   //42 pi0
                                        5.0e-19,    //43 eta0
                                        7.1e-21,    //44 J/Psi
                                        7.1e-21,    //45 J/Psi
                                        4.1e-13,    //46 D0
                                        4.1e-13,    //47 D0 b
                                        4.1e-13,    //48 D0
                                        4.1e-13,    //49 D0 b
                                        4.1e-13,    //50 D0 ->K0pi+pi-pi0
                                        1.04e-13,   //51 D+
                                        1.04e-13,   //52 D-
                                        5.0e-13,    //53 Ds+
                                        5.0e-13,    //54 Ds-
                                        2.0e-13,    //55 Lambdac
                                        2.0e-13,    //56 Lambdac b
                                        3.0e-22,    //57 D*0
                                        3.0e-22,    //58 D*0 b
                                        6.86e-21,   //59 D*+
                                        6.86e-21,   //60 D*-
                                        6.86e-21,   //61 D*+
                                        6.86e-21,   //62 D*-
                                        6.86e-21,   //63 D*0 ->D0pi0
                                        1.32e-10,   //64 H0
                                        1.00e-10,   //65 LambdaN
                                        1.00e-10,   //66 LambdaN b
                                        1.85e-10,   //67 H3L
                                        1.85e-10,   //68 H3L b
                                        1.80e-10,   //69 H4L
                                        1.80e-10,   //70 H4L b
                                        1.50e-10,   //71 He4L
                                        1.50e-10,   //72 He4L b
                                        1.40e-10,   //73 He5L
                                        1.40e-10,   //74 He5L b
                                        1.0e20,     //75 e-
                                        1.0e20,     //76 e+
                                        2.2e-6,     //77 mu-
                                        2.2e-6,     //78 mu+
                                        2.6e-8,     //79 pi+
                                        2.6e-8,     //80 pi-
                                        1.238e-8,   //81 K+
                                        1.238e-8,   //82 K-
                                        1.0e20,     //83 p
                                        1.0e20,     //84 p-
                                        1.0e20,     //85 d+
                                        1.0e20,     //86 d-
                                        1.0e20,     //87 t+
                                        1.0e20,     //88 t-
                                        1.0e20,     //89 He3+
                                        1.0e20,     //90 He3-
                                        1.0e20,     //91 He4+
                                        1.0e20,     //92 He4-
                                        0};         //93 V0
                                        
    int   mPartCharge[nParticles] = { 0, // 0 K0
                                      0, // 1 Lambda
                                      0, // 2 Lambda b
                                     -1, // 3 Xi-
                                      1, // 4 Xi+
                                      0, // 5 Xi0
                                      0, // 6 Xi0 b
                                     -1, // 7 Omega-
                                      1, // 8 Omega+
                                      0, // 9 Sigma0 
                                      0, //10 Sigma0 b
                                      1, //11 Sigma+
                                     -1, //12 Sigma+ b
                                      0,  //13 K*0
                                      0,  //14 K*0 b
                                      1,  //15 K*+
                                     -1,  //16 K*-
                                      0,  //17 K*0
                                      1,  //18 K*+
                                     -1,  //19 K*-
                                      1,   //20 Sigma*+
                                     -1,   //21 Sigma*-
                                     -1,   //22 Sigma*+ b
                                      1,   //23 Sigma*- b 
                                      0,   //24 Sigma*0 
                                      0,   //25 Sigma*0 b   
                                      0,   //26 Labmda*0
                                      0,   //27 Labmda*0 b
                                      0,   //28 Xi*0
                                      0,   //29 Xi*0 b
                                     -1,   //30 Xi*-
                                      1,   //31 Xi*+
                                     -1,   //32 Xi*-
                                      1,   //33 Xi*+
                                     -1,   //34 Omega*-
                                      1,   //35 Omega*+
                                      0,   //36 Hdb
                                      0,   //37 phi
                                      0,   //38 rho
                                      0,   //39 rho
                                      0,   //40 rho
                                      0,   //41 gamma
                                      0,   //42 pi0
                                      0,   //43 eta0
                                      0,   //44 J/Psi
                                      0,   //45 J/Psi
                                      0,   //46 D0
                                      0,   //47 D0 b
                                      0,   //48 D0
                                      0,   //49 D0 b
                                      0,   //50 D0 ->K0pi+pi-pi0
                                      1,   //51 D+
                                     -1,   //52 D-
                                      1,   //53 Ds+
                                     -1,   //54 Ds-
                                      1,   //55 Lambdac
                                     -1,   //56 Lambdac
                                      0,   //57 D*0
                                      0,   //58 D*0 b
                                      1,   //59 D*+
                                     -1,   //60 D*-
                                      1,   //61 D*+
                                     -1,   //62 D*-
                                      0,   //63 D*0 ->D0pi0
                                      0,   //64 H0
                                      0,   //65 LambdaN
                                      0,   //66 LambdaN b
                                      1,   //67 H3L
                                     -1,   //68 H3L b
                                      1,   //69 H4L
                                     -1,   //70 H4L b
                                      2,   //71 He4L
                                     -2,   //72 He4L b
                                      2,   //73 He5L
                                     -2,   //74 He5L b                                      
                                     -1,   //75 e-
                                      1,   //76 e+
                                     -1,   //77 mu-
                                      1,   //78 mu+
                                      1,   //79 pi+
                                     -1,   //80 pi-
                                      1,   //81 K+
                                     -1,   //82 K-
                                      1,   //83 p
                                     -1,   //84 p-
                                      1,   //85 d+
                                     -1,   //86 d-
                                      1,   //87 t+
                                     -1,   //88 t-
                                      2,   //89 He3+
                                     -2,   //90 He3-
                                      2,   //91 He4+
                                     -2,   //92 He4-
                                      0};  //93 V0                                      
    //set decay mode
    partDaughterPdg.resize(nParticles);

    int curPart = 0;
    
    partDaughterPdg[curPart].push_back(  211); //K0s -> pi+ pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 2212); //Lambda -> p pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(-2212); //Lambda_bar -> p- pi+
    partDaughterPdg[curPart].push_back(  211);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 3122); //Xi- -> Lambda pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(-3122); //Xi+ -> Lambda_bar pi+
    partDaughterPdg[curPart].push_back(  211);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 3122); //Xi0 -> Lambda pi0
    partDaughterPdg[curPart].push_back(  111);
    curPart++;
    
    partDaughterPdg[curPart].push_back(-3122); //Xi0_bar -> Lambda_bar pi0
    partDaughterPdg[curPart].push_back(  111);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 3122); //Omega- -> Lambda K-
    partDaughterPdg[curPart].push_back( -321);
    curPart++;
    
    partDaughterPdg[curPart].push_back(-3122); //Omega+ -> Lambda_bar K+
    partDaughterPdg[curPart].push_back(  321);
    curPart++;
    
    partDaughterPdg[curPart].push_back(   22); //Sigma0 -> Lambda Gamma
    partDaughterPdg[curPart].push_back( 3122);
    curPart++;
    
    partDaughterPdg[curPart].push_back(   22); //Sigma0_bar -> Lambda_bar Gamma
    partDaughterPdg[curPart].push_back(-3122);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  111); //Sigma+ -> p pi0
    partDaughterPdg[curPart].push_back( 2212);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  111); //Sigma+_bar -> p- pi0
    partDaughterPdg[curPart].push_back(-2212);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  321); //K*0 -> K+ pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back( -321); //K*0_bar -> K- pi+
    partDaughterPdg[curPart].push_back(  211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  310); //K*+ -> K0s pi+
    partDaughterPdg[curPart].push_back(  211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  310); //K*- -> K0s pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  310); //K*0 -> K0 pi0
    partDaughterPdg[curPart].push_back(  111);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  111); //K*+ -> K+ pi0
    partDaughterPdg[curPart].push_back(  321);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  111); //K*- -> K- pi0
    partDaughterPdg[curPart].push_back( -321);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 3122); //Sigma+ -> Lambda pi+
    partDaughterPdg[curPart].push_back(  211);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 3122); //Sigma- -> Lambda pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(-3122); //Sigma+_bar -> Lambda_bar pi+
    partDaughterPdg[curPart].push_back(  211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(-3122); //Sigma-_bar -> Lambda_bar pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 3122); //Sigma*0 -> Lambda pi0
    partDaughterPdg[curPart].push_back(  111);
    curPart++;
    
    partDaughterPdg[curPart].push_back(-3122); //Sigma*0_bar -> Lambda_bar pi0
    partDaughterPdg[curPart].push_back(  111);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 2212); //Lambda* -> p K-
    partDaughterPdg[curPart].push_back( -321);
    curPart++;
    
    partDaughterPdg[curPart].push_back(-2212); //Lambda*_bar -> p- K+
    partDaughterPdg[curPart].push_back(  321);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 3312); //Xi*0 -> Xi- pi+
    partDaughterPdg[curPart].push_back(  211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(-3312); //Xi*0_bar -> Xi+ pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 3122); //Xi*- -> Lambda K-
    partDaughterPdg[curPart].push_back( -321);
    curPart++;
    
    partDaughterPdg[curPart].push_back(-3122); //Xi*+ -> Lambda_bar K+
    partDaughterPdg[curPart].push_back(  321);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 3312); //Xi*- -> Xi- pi0
    partDaughterPdg[curPart].push_back(  111);
    curPart++;
    
    partDaughterPdg[curPart].push_back(-3312); //Xi*+ -> Xi+ pi0
    partDaughterPdg[curPart].push_back(  111);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 3312); //Omega*- -> Xi- pi+ K-
    partDaughterPdg[curPart].push_back(  211);
    partDaughterPdg[curPart].push_back( -321);
    curPart++;
    
    partDaughterPdg[curPart].push_back(-3312); //Omega*- -> Xi+ pi- K+
    partDaughterPdg[curPart].push_back( -211);
    partDaughterPdg[curPart].push_back(  321);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 3122); //H-dibar -> Lambda Lambda
    partDaughterPdg[curPart].push_back( 3122);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  321); //phi -> K+ K-
    partDaughterPdg[curPart].push_back( -321);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  211); //rho, omega, phi -> pi+ pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(   11); //rho, omega, phi -> e+ e-
    partDaughterPdg[curPart].push_back(  -11);
    curPart++;
    
    partDaughterPdg[curPart].push_back(   13); //rho, omega, phi -> mu+ mu-
    partDaughterPdg[curPart].push_back(  -13);
    curPart++;
    
    partDaughterPdg[curPart].push_back(   11); //gamma -> e+ e-
    partDaughterPdg[curPart].push_back(  -11);
    curPart++;
    
    partDaughterPdg[curPart].push_back(   22); //pi0 -> gamma gamma
    partDaughterPdg[curPart].push_back(   22);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  111); //eta -> pi0 pi0
    partDaughterPdg[curPart].push_back(  111);
    partDaughterPdg[curPart].push_back(  111);
    curPart++;
    
    partDaughterPdg[curPart].push_back(   11); //JPsi -> e+ e-
    partDaughterPdg[curPart].push_back(  -11);
    curPart++;
    
    partDaughterPdg[curPart].push_back(   13); //JPsi -> mu+ mu-
    partDaughterPdg[curPart].push_back(  -13);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  211); //D0 -> pi+ K-
    partDaughterPdg[curPart].push_back( -321);
    curPart++;
    
    partDaughterPdg[curPart].push_back( -211); //D0_bar -> K+ pi-
    partDaughterPdg[curPart].push_back(  321);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  211); //D0 -> pi+ pi+ pi- K-
    partDaughterPdg[curPart].push_back(  211);
    partDaughterPdg[curPart].push_back( -211);
    partDaughterPdg[curPart].push_back( -321);
    curPart++;
    
    partDaughterPdg[curPart].push_back( -211); //D0_bar -> pi- pi- pi+ K+
    partDaughterPdg[curPart].push_back( -211);
    partDaughterPdg[curPart].push_back(  211);
    partDaughterPdg[curPart].push_back(  321);
    curPart++;

    partDaughterPdg[curPart].push_back(  310); //D0_bar -> K0 pi+ pi- pi0
    partDaughterPdg[curPart].push_back(  211);
    partDaughterPdg[curPart].push_back( -211);
    partDaughterPdg[curPart].push_back(  111);
    curPart++;
    
    partDaughterPdg[curPart].push_back( -321); //D+ -> K- pi+ pi+
    partDaughterPdg[curPart].push_back(  211);
    partDaughterPdg[curPart].push_back(  211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  321); //D- -> K+ pi- pi-
    partDaughterPdg[curPart].push_back( -211);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back( -321); //Ds+ -> K- K+ pi+
    partDaughterPdg[curPart].push_back(  321);
    partDaughterPdg[curPart].push_back(  211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  321); //Ds- -> K+ K- pi-
    partDaughterPdg[curPart].push_back( -321);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  211); //Lambdac -> pi+ K- p
    partDaughterPdg[curPart].push_back( -321);
    partDaughterPdg[curPart].push_back( 2212);
    curPart++;
    
    partDaughterPdg[curPart].push_back( -211); //Lambdac_bar -> pi- K+ p-
    partDaughterPdg[curPart].push_back(  321);
    partDaughterPdg[curPart].push_back(-2212);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  411); //D*0 -> D+ pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back( -411); //D*0_bar -> D- pi+
    partDaughterPdg[curPart].push_back(  211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  421); //D*+ -> D0 pi+
    partDaughterPdg[curPart].push_back(  211);
    curPart++;
    
    partDaughterPdg[curPart].push_back( -421); //D*- -> D0_bar pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  421); //D*+ -> D04 pi+
    partDaughterPdg[curPart].push_back(  211);
    curPart++;
    
    partDaughterPdg[curPart].push_back( -421); //D*- -> D04_bar pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back(  428); //D*0 -> D04 pi0
    partDaughterPdg[curPart].push_back(  111);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 3122); //H0-> Lambda pi- p
    partDaughterPdg[curPart].push_back( -211);
    partDaughterPdg[curPart].push_back( 2212);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 1000010020); //LambdaN -> d+ pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back(-1000010020); //LambdaN_bar-> d- pi+
    partDaughterPdg[curPart].push_back(  211);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000020030); //H3Lambda -> He3+ pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back(-1000020030); //H3Lambda_bar -> He3- pi+
    partDaughterPdg[curPart].push_back(  211);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000020040); //H4Lambda -> He4+ pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back(-1000020040); //H4Lambda_bar -> He4- pi+
    partDaughterPdg[curPart].push_back(  211);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 1000020030); //He4Lambda -> He3+ p+ pi-
    partDaughterPdg[curPart].push_back( 2212);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back(-1000020030); //He4Lambda_bar -> He3- p- pi+
    partDaughterPdg[curPart].push_back(-2212);
    partDaughterPdg[curPart].push_back(  211);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 1000020040); //He5Lambda -> He4+ p+ pi-
    partDaughterPdg[curPart].push_back( 2212);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back(-1000020040); //He5Lambda_bar -> He4- p- pi+
    partDaughterPdg[curPart].push_back(-2212);
    partDaughterPdg[curPart].push_back(  211);
    
    for(int iP=0; iP<nParticles; iP++)
    {
      partPDG[iP] = mPartPDG[iP];
      partName[iP] = mPartName[iP];
      partTitle[iP] = mPartTitle[iP];
      partMHistoMin[iP] = mPartMHistoMin[iP];
      partMHistoMax[iP] = mPartMHistoMax[iP];
      partMaxMult[iP] = mPartMaxMult[iP];
      partMass[iP] = mPartMass[iP];
      partLifeTime[iP] = mPartLifeTime[iP];
      partCharge[iP] = mPartCharge[iP];
    }

    for(int iP=0; iP<nParticles; iP++)
    {
      AddCounter(Form("%s",partName[iP].Data()), Form("%-*s",14,partTitle[iP].Data()));
      AddCounter(Form("%s_prim",partName[iP].Data()), Form("%s Prim",partTitle[iP].Data()));
      AddCounter(Form("%s_sec",partName[iP].Data()), Form("%s Sec ",partTitle[iP].Data()));
    }

    for(int iP=0; iP<nParticles; iP++)
      fPdgToIndex[mPartPDG[iP]] = iP;
  }

  virtual ~KFPartEfficiencies(){};

  int GetParticleIndex(int pdg)
  {
    std::map<int, int>::iterator it;
    it=fPdgToIndex.find(pdg);
    if(it != fPdgToIndex.end()) return it->second;
    else return -1;
  }

  virtual void AddCounter(TString shortname, TString name){
    indices[shortname] = names.size();
    names.push_back(name);

    ratio_reco1.AddCounter();
    ratio_reco2.AddCounter();
    ratio_reco3.AddCounter();

    mc1.AddCounter();
    mc2.AddCounter();
    mc3.AddCounter();
    
    reco.AddCounter();

    ratio_ghost.AddCounter();
    ratio_bg.AddCounter();
    ratio_clone.AddCounter();
    ghost.AddCounter();
    bg.AddCounter();
    clone.AddCounter();
  };

  KFPartEfficiencies& operator+=(KFPartEfficiencies& a){
    mc1 += a.mc1; mc2 += a.mc2; mc3 += a.mc3; reco += a.reco;
    ghost += a.ghost; bg += a.bg; clone += a.clone;
    return *this;
  };
  
  void CalcEff(){
    ratio_reco1 = reco/mc1;
    ratio_reco2 = reco/mc2;
    ratio_reco3 = reco/mc3;

    KFMCCounter<int> allReco = reco + ghost + bg;
    ratio_ghost = ghost/allReco;
    ratio_bg  = bg/allReco;
    ratio_clone  = clone/allReco;
  };
  

  void Inc(bool isReco, int nClones, bool isMC1, bool isMC2, bool isMC3, TString name)
  {
    const int index = indices[name];
    
    if(isMC1) mc1.counters[index]++;
    if(isMC2) mc2.counters[index]++;
    if(isMC3) mc3.counters[index]++;
    
    if(isReco) reco.counters[index]++;
    if(nClones > 0)
      clone.counters[index] += nClones;
  };

  void IncReco(bool isGhost, bool isBg, TString name){
    const int index = indices[name];

    if (isGhost) ghost.     counters[index]++;
    if (isBg)    bg.counters[index]++;
  };

  void PrintEff(){
    std::ios_base::fmtflags original_flags = std::cout.flags();
    std::cout.setf(std::ios::fixed);
    std::cout.setf(std::ios::showpoint);
    std::cout.precision(3);
    std::cout << "Particle        : "
         <<        "   Eff 4pi "
         <<" / "<< " Eff accept"
         <<" / "<< "  Eff KFPF "
         <<" / "<< "     Ghost "
         <<" / "<< "    BackGr "
         <<" / "<< "   N Ghost "
         <<" / "<< "  N BackGr "
         <<" / "<< "    N Reco "
         <<" / "<< "   N Clone "
         <<" | "<< "  N MC 4pi " 
         <<" | "<< "N MC accept" 
         <<" | "<< " N MC KFPF "  << std::endl;
    
    int NCounters = mc1.NCounters;
    for (int iC = 0; iC < NCounters; iC++){
        std::cout << names[iC]
             << "  : " << std::setw(10) << ratio_reco1.counters[iC]    
             << "  / " << std::setw(10) << ratio_reco2.counters[iC]
             << "  / " << std::setw(10) << ratio_reco3.counters[iC]
             << "  / " << std::setw(10) << ratio_ghost.counters[iC]  // particles w\o MCParticle
             << "  / " << std::setw(10) << ratio_bg.counters[iC]     // particles with incorrect MCParticle
             << "  / " << std::setw(10) << ghost.counters[iC]
             << "  / " << std::setw(10) << bg.counters[iC]
             << "  / " << std::setw(10) << reco.counters[iC]
             << "  / " << std::setw(10) << clone.counters[iC]
             << "  | " << std::setw(10) << mc1.counters[iC] 
             << "  | " << std::setw(10) << mc2.counters[iC]
             << "  | " << std::setw(10) << mc3.counters[iC]  << std::endl;
    }
    std::cout.flags(original_flags); 
  };

  friend std::fstream & operator<<(std::fstream &strm, KFPartEfficiencies &a) {

    strm << a.ratio_reco1;
    strm << a.ratio_reco2;
    strm << a.ratio_reco3;
    strm << a.mc1;
    strm << a.mc2;
    strm << a.mc3;
    strm << a.reco;
    strm << a.ratio_ghost;
    strm << a.ratio_bg;
    strm << a.ratio_clone;
    strm << a.ghost;
    strm << a.bg;
    strm << a.clone;

    return strm;
  }

  friend std::fstream & operator>>(std::fstream &strm, KFPartEfficiencies &a){

    strm >> a.ratio_reco1;
    strm >> a.ratio_reco2;
    strm >> a.ratio_reco3;
    strm >> a.mc1;
    strm >> a.mc2;
    strm >> a.mc3;
    strm >> a.reco;
    strm >> a.ratio_ghost;
    strm >> a.ratio_bg;
    strm >> a.ratio_clone;
    strm >> a.ghost;
    strm >> a.bg;
    strm >> a.clone;

    return strm;
  }

  void AddFromFile(TString fileName)
  {
    std::fstream file(fileName.Data(),std::fstream::in);
    file >> *this;
  }
  
  int GetNDaughters(int iParticle) const { return partDaughterPdg[iParticle].size(); }
  int GetDaughterPDG(int iParticle, int iDaughter) const { return partDaughterPdg[iParticle][iDaughter]; }
  
  static const int nParticles = 94;
  int partPDG[nParticles];
  TString partName[nParticles];
  TString partTitle[nParticles];
  std::vector<std::vector<int> > partDaughterPdg;
  float partMHistoMin[nParticles];
  float partMHistoMax[nParticles];
  int partMaxMult[nParticles];
  float partMass[nParticles];
  float partLifeTime[nParticles];
  int partCharge[nParticles];

 private:
  std::vector<TString> names; // names counters indexed by index of counter
  std::map<TString, int> indices; // indices of counters indexed by a counter shortname

  std::map<int, int> fPdgToIndex;

  KFMCCounter<double> ratio_reco1;
  KFMCCounter<double> ratio_reco2;
  KFMCCounter<double> ratio_reco3;

  KFMCCounter<int> mc1;
  KFMCCounter<int> mc2;
  KFMCCounter<int> mc3;

  KFMCCounter<int> reco;

  KFMCCounter<double> ratio_ghost;
  KFMCCounter<double> ratio_bg;
  KFMCCounter<double> ratio_clone;

  KFMCCounter<int> ghost;
  KFMCCounter<int> bg; // background
  KFMCCounter<int> clone; // background
  
#ifndef KFParticleStandalone
  ClassDef( KFPartEfficiencies, 1 )
#endif
};

#endif
