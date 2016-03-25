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

#include <map>
#include <iomanip>
#include "KFMCCounter.h"

#ifdef HLTCA_STANDALONE
#include "RootTypesDef.h"
#else
#include "TObject.h"
#endif

class KFEfficiencyParticleInfo
{
 public:
  KFEfficiencyParticleInfo():fName("null"),fTitle("null"),fPDG(0),fHistoMin(0.f),fHistoMax(0.f),fMass(0.f),fLifeTime(0.f),fCharge(0) {};
  KFEfficiencyParticleInfo(std::string name, std::string title, int pdg, float histoMin, float histoMax, float mass, float lifeTime, int charge ):
    fName(name), fTitle(title), fPDG(pdg), fHistoMin(histoMin), fHistoMax(histoMax), fMass(mass), fLifeTime(lifeTime), fCharge(charge) {};
  ~KFEfficiencyParticleInfo() {};
  
  //accessors
  std::string Name()     const { return fName; }
  std::string Title()    const { return fTitle; }
  int         PDG()      const { return fPDG; }
  float       HistoMin() const { return fHistoMin; }
  float       HistoMax() const { return fHistoMax; }
  float       Mass()     const { return fMass; }
  float       LifeTime() const { return fLifeTime; }
  int         Charge()   const { return fCharge; } 
  
 private:
  std::string fName;
  std::string fTitle;
  int fPDG;
  float fHistoMin;
  float fHistoMax;
  float fMass;
  float fLifeTime;
  int fCharge; 
};

class KFPartEfficiencies :public TObject
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
    KFEfficiencyParticleInfo particleInfo[nParticles] = 
    {
      //                       name                title               PDG code   min   max    mass       lifetime    Q
      KFEfficiencyParticleInfo("Ks",               "KShort        ",        310, 0.3f, 1.3f, 0.497614   , 8.954e-11,  0), //0
      KFEfficiencyParticleInfo("Lambda",           "Lambda        ",       3122, 1.0f, 2.0f, 1.115683   , 2.632e-10,  0), //1
      KFEfficiencyParticleInfo("Lambdab",          "Lambda b      ",      -3122, 1.0f, 2.0f, 1.115683   , 2.632e-10,  0), //2
      KFEfficiencyParticleInfo("Xi-",              "Xi-           ",       3312, 1.0f, 3.0f, 1.32171    , 1.639e-10, -1), //3
      KFEfficiencyParticleInfo("Xi+",              "Xi+           ",      -3312, 1.0f, 3.0f, 1.32171    , 1.639e-10,  1), //4
      KFEfficiencyParticleInfo("Xi0",              "Xi0           ",       3322, 1.0f, 3.0f, 1.31486    , 2.9e-10,    0), //5
      KFEfficiencyParticleInfo("Xi0b",             "Xi0 b         ",      -3322, 1.0f, 3.0f, 1.31486    , 2.9e-10,    0), //6
      KFEfficiencyParticleInfo("Omega-",           "Omega-        ",       3334, 1.0f, 3.0f, 1.67245    , 0.821e-10, -1), //7
      KFEfficiencyParticleInfo("Omega+",           "Omega+        ",      -3334, 1.0f, 3.0f, 1.67245    , 0.821e-10,  1), //8
      KFEfficiencyParticleInfo("#Sigma^0",         "Sigma0        ",       3212, 1.0f, 3.0f, 1.192642   , 7.4e-20,    0), //9
      KFEfficiencyParticleInfo("#Sigma^0b",        "Sigma0 b      ",      -3212, 1.0f, 3.0f, 1.192642   , 7.4e-20,    0), //10
      KFEfficiencyParticleInfo("#Sigma^+",         "Sigma+        ",       3222, 1.0f, 3.0f, 1.18937    , 0.8018e-10, 1), //11
      KFEfficiencyParticleInfo("#Sigma^-b",        "Sigma- b      ",      -3222, 1.0f, 3.0f, 1.18937    , 0.8018e-10,-1), //12
      KFEfficiencyParticleInfo("K*0",              "K*0           ",        313, 0.6f, 2.6f, 0.8958     , 1.38e-23,   0), //13
      KFEfficiencyParticleInfo("K*0b",             "K*0 b         ",       -313, 0.6f, 2.6f, 0.8958     , 1.38e-23,   0), //14
      KFEfficiencyParticleInfo("K*+",              "K*+           ",        323, 0.6f, 2.6f, 0.89166    , 1.30e-23,   1), //15
      KFEfficiencyParticleInfo("K*-",              "K*-           ",       -323, 0.6f, 2.6f, 0.89166    , 1.30e-23,  -1), //16
      KFEfficiencyParticleInfo("K*0_{K0,#pi0}",    "K*0_K0pi0     ",     100313, 0.6f, 2.6f, 0.8958     , 1.38e-23,   0), //17
      KFEfficiencyParticleInfo("K*+_{K+,#pi0}",    "K*+_K+pi0     ",     100323, 0.6f, 2.6f, 0.89166    , 1.30e-23,   1), //18
      KFEfficiencyParticleInfo("K*-_{K-,#pi0}",    "K*-_K-pi0     ",    -100323, 0.6f, 2.6f, 0.89166    , 1.30e-23,  -1), //19
      KFEfficiencyParticleInfo("Sigma*+",          "Sigma*+       ",       3224, 1.0f, 3.0f, 1.3828     , 1.83e-23,   1), //20
      KFEfficiencyParticleInfo("Sigma*-",          "Sigma*-       ",       3114, 1.0f, 3.0f, 1.3872     , 1.67e-23,  -1), //21
      KFEfficiencyParticleInfo("Sigma*+b",         "Sigma*+ b     ",      -3114, 1.0f, 3.0f, 1.3828     , 1.83e-23,  -1), //22
      KFEfficiencyParticleInfo("Sigma*-b",         "Sigma*- b     ",      -3224, 1.0f, 3.0f, 1.3872     , 1.67e-23,   1), //23
      KFEfficiencyParticleInfo("Sigma*0",          "Sigma*0       ",       3214, 1.0f, 3.0f, 1.3837     , 1.83e-23,   0), //24
      KFEfficiencyParticleInfo("Sigma*0b",         "Sigma*0 b     ",      -3214, 1.0f, 3.0f, 1.3837     , 1.83e-23,   0), //25
      KFEfficiencyParticleInfo("Lambda*",          "Lambda*       ",       3124, 1.4f, 3.4f, 1.5195     , 4.22e-23,   0), //26
      KFEfficiencyParticleInfo("Lambda*b",         "Lambda* b     ",      -3124, 1.4f, 3.4f, 1.5195     , 4.22e-23,   0), //27
      KFEfficiencyParticleInfo("Xi*0",             "Xi*0          ",       3324, 1.4f, 3.4f, 1.53180    , 7.23e-23,   0), //28
      KFEfficiencyParticleInfo("Xi*0b",            "Xi*0 b        ",      -3324, 1.4f, 3.4f, 1.53180    , 7.23e-23,   0), //29
      KFEfficiencyParticleInfo("Xi*-_{LK}",        "Xi*-_lk       ",    1003314, 1.4f, 3.4f, 1.823      , 2.74e-23,  -1), //30
      KFEfficiencyParticleInfo("Xi*+_{LK}",        "Xi*+_lk       ",   -1003314, 1.4f, 3.4f, 1.823      , 2.74e-23,   1), //31
      KFEfficiencyParticleInfo("Xi*-_{xi-,pi0}",   "Xi*-_XiPi     ",       3314, 1.4f, 3.4f, 1.535      , 6.65e-23,  -1), //32
      KFEfficiencyParticleInfo("Xi*+_{xi+,pi0}",   "Xi*+_XiPi     ",      -3314, 1.4f, 3.4f, 1.535      , 6.65e-23,   1), //33
      KFEfficiencyParticleInfo("Omega*-",          "Omega*-       ",    1003334, 1.8f, 3.8f, 2.252      , 1.2e-23,   -1), //34
      KFEfficiencyParticleInfo("Omega*+",          "Omega*+       ",   -1003334, 1.8f, 3.8f, 2.252      , 1.2e-23,    1), //35
      KFEfficiencyParticleInfo("H0_{LL}",          "H0_LL         ",       3000, 1.5f, 3.5f, 2.21       , 1.32e-10,   0), //36
      KFEfficiencyParticleInfo("phi_{KK}",         "phi_KK        ",        333, 0.8f, 2.8f, 1.019455   , 1.55e-22,   0), //37
      KFEfficiencyParticleInfo("rho_{#pi#pi}",     "rho_pipi      ",        113, 0.0f, 2.0f, 0.77526    , 4.45e-24,   0), //38
      KFEfficiencyParticleInfo("rho_{ee}",         "rho_ee        ",     100113, 0.0f, 2.0f, 0.77526    , 4.45e-24,   0), //39
      KFEfficiencyParticleInfo("rho_{#mu#mu}",     "rho_mm        ",     200113, 0.0f, 2.0f, 0.77526    , 4.45e-24,   0), //40
      KFEfficiencyParticleInfo("gamma",            "gamma         ",         22, 0.0f, 3.0f, 0.         , 1.e20,      0), //41
      KFEfficiencyParticleInfo("#pi^{0}",          "pi0           ",        111, 0.0f, 3.0f, 0.1349766  , 8.52e-17,   0), //42
      KFEfficiencyParticleInfo("eta",              "eta           ",        221, 0.0f, 3.0f, 0.547862   , 5.0e-19,    0), //43
//Delta and N resonances
      KFEfficiencyParticleInfo("Delta0",           "Delta0        ",       2114, 1.0f, 3.0f, 1.232      , 5.63e-24,   0), //44
      KFEfficiencyParticleInfo("Delta0 b",         "Delta0 b      ",      -2114, 1.0f, 3.0f, 1.232      , 5.63e-24,   0), //45
      KFEfficiencyParticleInfo("Delta++",          "Delta++       ",       2224, 1.0f, 3.0f, 1.232      , 5.63e-24,   2), //46
      KFEfficiencyParticleInfo("Delta-- b",        "Delta-- b     ",      -2224, 1.0f, 3.0f, 1.232      , 5.63e-24,  -2), //47
//charmonium
      KFEfficiencyParticleInfo("J#Psi_ee",         "JPsi_ee       ",        443, 1.0f, 4.0f, 3.096916   , 7.1e-21,    0), //48
      KFEfficiencyParticleInfo("J#Psi_mumu",       "JPsi_mm       ",     100443, 1.0f, 4.0f, 3.096916   , 7.1e-21,    0), //49
      KFEfficiencyParticleInfo("J#Psi_pp",         "JPsi_pp       ",     200443, 1.0f, 4.0f, 3.096916   , 7.1e-21,    0), //50
      KFEfficiencyParticleInfo("J#Psi_LL",         "JPsi_LL       ",     300443, 2.0f, 5.0f, 3.096916   , 7.1e-21,    0), //51
      KFEfficiencyParticleInfo("J#Psi_XiXi",       "JPsi_XiXi     ",     400443, 2.0f, 5.0f, 3.096916   , 7.1e-21,    0), //52
      KFEfficiencyParticleInfo("Psi_OO",           "Psi_OO        ",     500443, 3.0f, 6.0f, 3.686109   , 2.1e-22,    0), //53
//open charm
      KFEfficiencyParticleInfo("D0",               "D0            ",        421, 1.0f, 3.0f, 1.86486    , 4.1e-13,    0), //54
      KFEfficiencyParticleInfo("D0b",              "D0b           ",       -421, 1.0f, 3.0f, 1.86486    , 4.1e-13,    0), //55
      KFEfficiencyParticleInfo("D0_4",             "D0_4          ",        429, 1.0f, 3.0f, 1.86486    , 4.1e-13,    0), //56
      KFEfficiencyParticleInfo("D0b_4",            "D0b_4         ",       -429, 1.0f, 3.0f, 1.86486    , 4.1e-13,    0), //57
      KFEfficiencyParticleInfo("D0_{pi0}",         "D0_#pi0       ",        428, 1.0f, 3.0f, 1.86486    , 4.1e-13,    0), //58
      KFEfficiencyParticleInfo("D+",               "D+            ",        411, 1.0f, 3.0f, 1.86962    , 1.04e-13,   1), //59
      KFEfficiencyParticleInfo("D-",               "D-            ",       -411, 1.0f, 3.0f, 1.86962    , 1.04e-13,  -1), //60
      KFEfficiencyParticleInfo("Ds+",              "Ds+           ",        431, 1.0f, 3.0f, 1.96850    , 5.0e-13,    1), //61
      KFEfficiencyParticleInfo("Ds-",              "Ds-           ",       -431, 1.0f, 3.0f, 1.96850    , 5.0e-13,   -1), //62
      KFEfficiencyParticleInfo("Lc",               "Lambdac       ",       4122, 1.8f, 3.8f, 2.28646    , 2.0e-13,    1), //63
      KFEfficiencyParticleInfo("Lcb",              "Lambdac b     ",      -4122, 1.8f, 3.8f, 2.28646    , 2.0e-13,   -1), //64
      KFEfficiencyParticleInfo("D*0",              "D*0           ",      10421, 1.8f, 3.8f, 2.00699    , 3.0e-22,    0), //65
      KFEfficiencyParticleInfo("D*0b",             "D*0 b         ",     -10421, 1.8f, 3.8f, 2.00699    , 3.0e-22,    0), //66
      KFEfficiencyParticleInfo("D*+",              "D*+           ",      10411, 1.8f, 3.8f, 2.01029    , 6.86e-21,   1), //67
      KFEfficiencyParticleInfo("D*-",              "D*-           ",     -10411, 1.8f, 3.8f, 2.01029    , 6.86e-21,  -1), //68
      KFEfficiencyParticleInfo("D*+_4",            "D*+_4         ",      20411, 1.8f, 3.8f, 2.01029    , 6.86e-21,   1), //69
      KFEfficiencyParticleInfo("D*-_4",            "D*-_4         ",     -20411, 1.8f, 3.8f, 2.01029    , 6.86e-21,  -1), //70
      KFEfficiencyParticleInfo("D0*_{#pi0}",       "D0*_#pi0      ",      10428, 1.8f, 3.8f, 2.00699    , 6.86e-21,   0), //71
      KFEfficiencyParticleInfo("H0_{Lppi}",        "H0            ",       3001, 1.0f, 3.0f, 2.21       , 1.32e-10,   0), //72
//hypernuclei
      KFEfficiencyParticleInfo("LambdaN",          "LambdaN       ",       3003, 1.0f, 3.0f, 2.05395    , 1.00e-10,   0), //73
      KFEfficiencyParticleInfo("LambdaNb",         "LambdaN b     ",      -3003, 1.0f, 3.0f, 2.05395    , 1.00e-10,   0), //74
      KFEfficiencyParticleInfo("LambdaNN",         "LambdaNN      ",       3103, 2.0f, 4.0f, 2.99352    , 1.00e-10,   0), //75
      KFEfficiencyParticleInfo("LambdaNNb",        "LambdaNN b    ",      -3103, 2.0f, 4.0f, 2.99352    , 1.00e-10,   0), //76
      KFEfficiencyParticleInfo("H3L",              "H3L           ",       3004, 2.0f, 4.0f, 2.99339    , 1.85e-10,   1), //77
      KFEfficiencyParticleInfo("H3Lb",             "H3L b         ",      -3004, 2.0f, 4.0f, 2.99339    , 1.85e-10,  -1), //78
      KFEfficiencyParticleInfo("H4L",              "H4L           ",       3005, 3.0f, 5.0f, 3.92975    , 1.80e-10,   1), //79
      KFEfficiencyParticleInfo("H4Lb",             "H4L b         ",      -3005, 3.0f, 5.0f, 3.92975    , 1.80e-10,  -1), //80
      KFEfficiencyParticleInfo("He4L",             "He4L          ",       3006, 3.0f, 5.0f, 3.93070    , 1.50e-10,   2), //81
      KFEfficiencyParticleInfo("He4Lb",            "He4L b        ",      -3006, 3.0f, 5.0f, 3.93070    , 1.50e-10,  -2), //82
      KFEfficiencyParticleInfo("He5L",             "He5L          ",       3007, 4.0f, 6.0f, 4.86824    , 1.40e-10,   2), //83
      KFEfficiencyParticleInfo("He5Lb",            "He5L b        ",      -3007, 4.0f, 6.0f, 4.86824    , 1.40e-10,  -2), //84
      KFEfficiencyParticleInfo("LLn",              "LLn           ",       3203, 3.0f, 5.0f, 3.16964    , 1.00e-10,   0), //85
      KFEfficiencyParticleInfo("H4LL_{He4Lpi-}",   "H4LL_{He4Lpi-}",       3008, 3.0f, 5.0f, 4.10791    ,  1.0e-10,   1), //86
      KFEfficiencyParticleInfo("H4LL_{H3Lppi-}",   "H4LL_{H3Lppi-}",       3009, 3.0f, 5.0f, 4.10791    ,  1.0e-10,   1), //87
      KFEfficiencyParticleInfo("H5LL_{He5Lpi-}",   "H5LL_{He5Lpi-}",       3010, 4.0f, 6.0f, 5.04748    ,  1.0e-10,   1), //88
      KFEfficiencyParticleInfo("He6LL",            "He6LL         ",       3011, 5.0f, 7.0f, 5.98575    ,  1.0e-10,   2), //89
//missing mass method      
      KFEfficiencyParticleInfo("pi-_{mu,nu}",      "pi-_{mnu}     ",   -7000211,-1.0f, 1.0f, 0.139570   , 2.6e-8,    -1), //90
      KFEfficiencyParticleInfo("nu_{pi-}",         "nu_{mupi-} b  ",   -7000014,-1.0f, 1.0f, 0.         , 1.0e20,     0), //91
      KFEfficiencyParticleInfo("pi+_{mu,nu}",      "pi+_{mnu}     ",    7000211,-1.0f, 1.0f, 0.139570   , 2.6e-8,     1), //92
      KFEfficiencyParticleInfo("nu_{pi+}",         "nu_{mupi+}    ",    7000014,-1.0f, 1.0f, 0.         , 1.0e20,     0), //93
      KFEfficiencyParticleInfo("K-_{mu,nu}",       "K-_{mnu}      ",   -7000321,-1.0f, 1.0f, 0.493677   , 1.238e-8,  -1), //94
      KFEfficiencyParticleInfo("nu_{K-}",          "nu_{K-} b     ",   -8000014,-1.0f, 1.0f, 0.         , 1.0e20,     0), //95
      KFEfficiencyParticleInfo("K+_{mu,nu}",       "K+_{mnu}      ",    7000321,-1.0f, 1.0f, 0.493677   , 1.238e-8,   1), //96
      KFEfficiencyParticleInfo("nu_{K+}",          "nu_{K+}       ",    8000014,-1.0f, 1.0f, 0.         , 1.0e20,     0), //97
      KFEfficiencyParticleInfo("Sigma-_{pi-,n}",   "Sigma-_{pi-n} ",    7003112, 0.0f, 3.0f, 1.192642   , 1.479e-10, -1), //98
      KFEfficiencyParticleInfo("n_{Sigma-}",       "n_{Sigma-}    ",    7002112, 0.0f, 1.5f, 0.9395654  , 880,        0), //99
      KFEfficiencyParticleInfo("Sigma+_{pi+n}b",   "Sigma+{pi+n} b",   -7003112, 0.0f, 3.0f, 1.192642   , 1.479e-10, -1), //100
      KFEfficiencyParticleInfo("n_{Sigma+} b",     "n_{Sigma+b} b ",   -7002112, 0.0f, 1.5f, 0.9395654  , 880,        0), //101
      KFEfficiencyParticleInfo("Sigma-_{pi-n}b",   "Sigma+{pi-n} b",   -7003222, 0.0f, 3.0f, 1.18937    , 0.8018e-10, 1), //102
      KFEfficiencyParticleInfo("n_{Sigma-} b",     "n_{Sigma-_b} b",   -8002112, 0.0f, 1.5f, 0.9395654  , 0.93956541, 0), //103
      KFEfficiencyParticleInfo("Sigma+_{pi+n}",    "Sigma+_{pi+n} ",    7003222, 0.0f, 3.0f, 1.18937    , 0.8018e-10, 1), //104
      KFEfficiencyParticleInfo("n_{Sigma+}",       "n_{Sigma+}    ",    8002112, 0.0f, 1.5f, 0.9395654  , 880,        0), //105
      KFEfficiencyParticleInfo("Xi-_{pi-L}",       "Xi-_{pi-L}    ",    7003312, 0.0f, 3.0f, 1.32171    , 1.639e-10, -1), //106
      KFEfficiencyParticleInfo("Lambda_{Xi-}",     "Lambda_{Xi-}  ",    7003122, 0.0f, 2.0f, 1.115683   , 2.632e-10,  0), //107
      KFEfficiencyParticleInfo("Xi+_{pi+L_b}",     "Xi+_{pi+L_b}  ",   -7003312, 0.0f, 3.0f, 1.32171    , 1.639e-10,  1), //108
      KFEfficiencyParticleInfo("Lambda_{Xi+} b",   "Lambda_{Xi+} b",   -7003122, 0.0f, 2.0f, 1.115683   , 2.632e-10,  0), //109
      KFEfficiencyParticleInfo("Omega-_{Xi-pi0}",  "Omega-{pi-Xi0}",    7003334, 0.0f, 3.0f, 1.67245    , 0.821e-10, -1), //110
      KFEfficiencyParticleInfo("Xi0_{Omega-}",     "Xi0_{Omega-}  ",    7003322, 0.0f, 3.0f, 1.31486    , 2.9e-10,    0), //111
      KFEfficiencyParticleInfo("Omega+_{Xi+pi0}",  "Omega+_{Xipi0}",   -7003334, 0.0f, 3.0f, 1.67245    , 0.821e-10,  1), //112
      KFEfficiencyParticleInfo("Xi0_{Omega+} b",   "Xi0_{Omega+} b",   -7003322, 0.0f, 3.0f, 1.31486    , 2.9e-10,    0), //113
      KFEfficiencyParticleInfo("K-_{pi-pi0}",      "K-_{pi-pi0}   ",   -9000321, 0.0f, 3.0f, 0.493677   , 1.24e-8,   -1), //114
      KFEfficiencyParticleInfo("pi0_{K-}",         "pi0_{K-}      ",   -9000111, 0.0f, 3.0f, 0.1349766  , 8.52e-17,   0), //115
      KFEfficiencyParticleInfo("K+_{pi+pi0}",      "K+_{pi+pi0}   ",    9000321, 0.0f, 3.0f, 0.493677   , 1.24e-8,    1), //116
      KFEfficiencyParticleInfo("pi0_{K+}",         "pi0_{K+}      ",    9000111, 0.0f, 3.0f, 0.1349766  , 8.52e-17,   0), //117
      KFEfficiencyParticleInfo("Omega-{K-L}",      "Omega-_{K-L}  ",    8003334, 0.0f, 3.0f, 1.67245    , 0.821e-10, -1), //118
      KFEfficiencyParticleInfo("Lambda_{Omega-}",  "Lambda_{O-}   ",    8003122, 0.0f, 3.0f, 1.115683   , 2.632e-10,  0), //119
      KFEfficiencyParticleInfo("Omega+_{K+L_b}",   "Omega+_{K+Lb} ",   -8003334, 0.0f, 3.0f, 1.67245    , 0.821e-10,  1), //120
      KFEfficiencyParticleInfo("Lamda_{Omega+} b", "Lambda_{O+} b ",   -8003122, 0.0f, 3.0f, 1.115683   , 2.632e-10,  0), //121
      KFEfficiencyParticleInfo("Sigma-{p_b pi0} b","Sigma-{ppi0} b",   -8003222, 0.0f, 3.0f, 1.18937    , 0.8018e-10, 1), //122
      KFEfficiencyParticleInfo("pi0_{Sigma-b}",    "pi0_{Sigma-_b}",   -8000111, 0.0f, 3.0f, 0.1349766  , 8.52e-17,   0), //123
      KFEfficiencyParticleInfo("Sigma+_{p pi0}",   "Sigma+_{ppi0} ",    8003222, 0.0f, 3.0f, 1.18937    , 0.8018e-10, 1), //124
      KFEfficiencyParticleInfo("pi0_{Sigma+}",     "pi0_{Sigma+}  ",    8000111, 0.0f, 3.0f, 0.1349766  , 8.52e-17,   0), //125
//tracks
      KFEfficiencyParticleInfo("e-",               "e-            ",         11, 0.0f,0.01f, 5.485799e-4, 1.0e20,    -1), //126
      KFEfficiencyParticleInfo("e+",               "e+            ",        -11, 0.0f,0.01f, 5.485799e-4, 1.0e20,     1), //127
      KFEfficiencyParticleInfo("mu-",              "mu-           ",         13, 0.0f, 1.0f, 0.105658   , 2.2e-6,    -1), //128
      KFEfficiencyParticleInfo("mu+",              "mu+           ",        -13, 0.0f, 1.0f, 0.105658   , 2.2e-6,     1), //129
      KFEfficiencyParticleInfo("pi+",              "pi+           ",        211, 0.0f, 1.0f, 0.139570   , 2.6e-8,     1), //130
      KFEfficiencyParticleInfo("pi-",              "pi-           ",       -211, 0.0f, 1.0f, 0.139570   , 2.6e-8,    -1), //131
      KFEfficiencyParticleInfo("K+",               "K+            ",        321, 0.0f, 1.0f, 0.493677   , 1.238e-8,   1), //132
      KFEfficiencyParticleInfo("K-",               "K-            ",       -321, 0.0f, 1.0f, 0.493677   , 1.238e-8,  -1), //133
      KFEfficiencyParticleInfo("p+",               "p+            ",       2212, 0.0f, 1.5f, 0.938272   , 1.0e20,     1), //134
      KFEfficiencyParticleInfo("p-",               "p-            ",      -2212, 0.0f, 1.5f, 0.938272   , 1.0e20,    -1), //135
      KFEfficiencyParticleInfo("d+",               "d+            ", 1000010020, 0.0f, 2.5f, 1.876124   , 1.0e20,     1), //136
      KFEfficiencyParticleInfo("d-",               "d-            ",-1000010020, 0.0f, 2.5f, 1.876124   , 1.0e20,    -1), //137
      KFEfficiencyParticleInfo("t+",               "t+            ", 1000010030, 0.0f, 3.5f, 2.809432   , 1.0e20,     1), //138
      KFEfficiencyParticleInfo("t-",               "t-            ",-1000010030, 0.0f, 3.5f, 2.809432   , 1.0e20,    -1), //139
      KFEfficiencyParticleInfo("He3+",             "He3+          ", 1000020030, 0.0f, 3.5f, 2.809413   , 1.0e20,     2), //140
      KFEfficiencyParticleInfo("He3-",             "He3-          ",-1000020030, 0.0f, 3.5f, 2.809413   , 1.0e20,    -2), //141
      KFEfficiencyParticleInfo("He4+",             "He4+          ", 1000020040, 0.0f, 4.5f, 3.728400   , 1.0e20,     2), //142
      KFEfficiencyParticleInfo("He4-",             "He4-          ",-1000020040, 0.0f, 4.5f, 3.728400   , 1.0e20,    -2), //143
//background for subtraction
      KFEfficiencyParticleInfo("pi+pi+",           "pi+pi+        ",       9001, 0.0f, 2.0f, 0          , 1.0e20,     0), //144
      KFEfficiencyParticleInfo("pi+K+",            "pi+K+         ",       9002, 0.6f, 2.6f, 0          , 1.0e20,     0), //145
      KFEfficiencyParticleInfo("K+K+",             "K+K+          ",       9003, 0.8f, 2.8f, 0          , 1.0e20,     0), //146
      KFEfficiencyParticleInfo("K+p+",             "K+p+          ",       9004, 1.4f, 3.4f, 0          , 1.0e20,     0), //147
      KFEfficiencyParticleInfo("pi-pi-",           "pi-pi-        ",      -9001, 0.0f, 2.0f, 0          , 1.0e20,     0), //148
      KFEfficiencyParticleInfo("pi-K-",            "pi-K-         ",      -9002, 0.6f, 2.6f, 0          , 1.0e20,     0), //149
      KFEfficiencyParticleInfo("K-K-",             "K-K-          ",      -9003, 0.8f, 2.8f, 0          , 1.0e20,     0), //150
      KFEfficiencyParticleInfo("K-p-",             "K-p-          ",      -9004, 1.4f, 3.4f, 0          , 1.0e20,     0), //151
//V0
      KFEfficiencyParticleInfo("V0",               "V0            ",  123456789, 0.3f, 1.3f, 0          , 0,          0)  //152
    };
                                        
    int mPartMaxMult[nParticles];
    for(int i=0; i<nParticles; i++)
      mPartMaxMult[i] = 20;
    mPartMaxMult[fFirstStableParticleIndex + 4] = 500;
    mPartMaxMult[fFirstStableParticleIndex + 5] = 500;
    mPartMaxMult[fFirstStableParticleIndex + 6] = 50;
    mPartMaxMult[fFirstStableParticleIndex + 7] = 50;
    mPartMaxMult[fFirstStableParticleIndex + 8] = 500;
                                    
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
    
    partDaughterPdg[curPart].push_back( 3122); //Xi0 -> Lambda Pi0
    partDaughterPdg[curPart].push_back(  111);
    curPart++;
    
    partDaughterPdg[curPart].push_back(-3122); //Xi0_bar -> Lambda_bar Pi0
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
    
    partDaughterPdg[curPart].push_back(  111); //Sigma+ -> p Pi0
    partDaughterPdg[curPart].push_back( 2212);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  111); //Sigma+_bar -> p- Pi0
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
    
    partDaughterPdg[curPart].push_back( 2212); //Delta0 -> p pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back(-2212); //Delta0_bar -> p- pi+
    partDaughterPdg[curPart].push_back(  211);
    curPart++;

    partDaughterPdg[curPart].push_back( 2212); //Delta++ -> p pi+
    partDaughterPdg[curPart].push_back(  211);
    curPart++;

    partDaughterPdg[curPart].push_back(-2212); //Delta--_bar -> p- pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(   11); //JPsi -> e+ e-
    partDaughterPdg[curPart].push_back(  -11);
    curPart++;
    
    partDaughterPdg[curPart].push_back(   13); //JPsi -> mu+ mu-
    partDaughterPdg[curPart].push_back(  -13);
    curPart++;

    partDaughterPdg[curPart].push_back(   2212); //JPsi -> p p-
    partDaughterPdg[curPart].push_back(  -2212);
    curPart++;

    partDaughterPdg[curPart].push_back(   3122); //JPsi -> Lambda Lambda_bar
    partDaughterPdg[curPart].push_back(  -3122);
    curPart++;

    partDaughterPdg[curPart].push_back(   3312); //JPsi -> Xi- Xi+
    partDaughterPdg[curPart].push_back(  -3312);
    curPart++;

    partDaughterPdg[curPart].push_back(   3334); //Psi -> Omega- Omega+
    partDaughterPdg[curPart].push_back(  -3334);
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

    partDaughterPdg[curPart].push_back( 1000010030); //LambdaNN -> t+ pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back(-1000010030); //LambdaNN_bar -> t- pi+
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
    curPart++;
    
    partDaughterPdg[curPart].push_back( 3004); //LLn -> H3Lambda pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 3006); //H4LL -> He4Lambda pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back( 3004); //H4LL -> H3Lambda p pi-
    partDaughterPdg[curPart].push_back( 2212);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back( 3007); //H5LL -> He5Lambda pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 3007); //He6LL -> He5Lambda p pi-
    partDaughterPdg[curPart].push_back( 2212);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(      13); // pi- -> mu- + nu_mu_bar
    partDaughterPdg[curPart].push_back(-7000014); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(    -211); // nu_mu_bar <- pi- - mu-
    partDaughterPdg[curPart].push_back(      13); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(     -13); // pi+ -> mu+ + nu_mu
    partDaughterPdg[curPart].push_back( 7000014); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(    211); // nu_mu <- pi+ - mu+
    partDaughterPdg[curPart].push_back(    -13); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(      13); // K- -> mu- + nu_mu_bar
    partDaughterPdg[curPart].push_back(-8000014); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(    -321); // nu_mu_bar <- K- - mu-
    partDaughterPdg[curPart].push_back(      13); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(     -13); // K+ -> mu+ + nu_mu
    partDaughterPdg[curPart].push_back( 8000014); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(     321); // nu_mu <- K+ - mu+
    partDaughterPdg[curPart].push_back(     -13); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(     -211); // Sigma- -> pi- + n
    partDaughterPdg[curPart].push_back(  7002112); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(     3112); // n <- Sigma- - pi-
    partDaughterPdg[curPart].push_back(     -211); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(       211); // Sigma-b -> pi+ + nb
    partDaughterPdg[curPart].push_back(  -7002112); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(     -3112); // nb <- Sigma-b - pi+
    partDaughterPdg[curPart].push_back(       211); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(      -211); // Sigma+b -> pi- + nb
    partDaughterPdg[curPart].push_back(  -8002112); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(    -3222); // nb <- Sigma+b - pi-
    partDaughterPdg[curPart].push_back(     -211); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(       211); // Sigma+ -> pi+ + n
    partDaughterPdg[curPart].push_back(   8002112); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(      3222); // n <- Sigma+ - pi+
    partDaughterPdg[curPart].push_back(       211); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(      -211); // Xi- -> pi- + lam
    partDaughterPdg[curPart].push_back(   7003122); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(      3312); // lam <- Xi- - pi-
    partDaughterPdg[curPart].push_back(      -211); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(       211); // Xi-b -> pi+ + lam_b
    partDaughterPdg[curPart].push_back(  -7003122); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(     -3312); // lam_b <- Xi-b - pi+
    partDaughterPdg[curPart].push_back(       211); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(      -211); // Om- -> pi- + Xi0
    partDaughterPdg[curPart].push_back(   7003322); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(      3334); // Xi0 <- Om- - pi-
    partDaughterPdg[curPart].push_back(      -211); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(       211); // Om-b -> pi+ + Xi0_b
    partDaughterPdg[curPart].push_back(  -7003322); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(     -3334); // Xi0_b <- Om-b - pi+
    partDaughterPdg[curPart].push_back(       211); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(      -211); // K- -> pi- + Pi0_b
    partDaughterPdg[curPart].push_back(  -9000111); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(      -321); // Pi0_b <- K- - pi-
    partDaughterPdg[curPart].push_back(      -211); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(       211); // K+ -> pi+ + Pi0
    partDaughterPdg[curPart].push_back(   9000111); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(       321); // Pi0 <- K+ - pi+
    partDaughterPdg[curPart].push_back(       211); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(      -321); // Om- -> K- + Lam
    partDaughterPdg[curPart].push_back(   8003122); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(      3334); // Lam <- Om- - K-
    partDaughterPdg[curPart].push_back(      -321); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(       321); // Om+ -> K+ + Lam_b
    partDaughterPdg[curPart].push_back(  -8003122); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(     -3334); // Lam_b <- Om+ - K+
    partDaughterPdg[curPart].push_back(       321); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(     -2212); // Si+b -> p_b + Pi0
    partDaughterPdg[curPart].push_back(  -8000111); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(     -3222); // Pi0 <- Si+b - p_b
    partDaughterPdg[curPart].push_back(     -2212); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(      2212); // Si+ -> p + Pi0
    partDaughterPdg[curPart].push_back(   8000111); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(      3222); // Pi0 <- Si+ - p
    partDaughterPdg[curPart].push_back(      2212); //
    curPart++;
    
    for(int iP=0; iP<nParticles; iP++)
    {
      partPDG[iP] = particleInfo[iP].PDG();
      partName[iP] = particleInfo[iP].Name();
      partTitle[iP] = particleInfo[iP].Title();
      partMHistoMin[iP] = particleInfo[iP].HistoMin();
      partMHistoMax[iP] = particleInfo[iP].HistoMax();
      partMaxMult[iP] = mPartMaxMult[iP];
      partMass[iP] = particleInfo[iP].Mass();
      partLifeTime[iP] = particleInfo[iP].LifeTime();
      partCharge[iP] = particleInfo[iP].Charge();
    }

    for(int iP=0; iP<nParticles; iP++)
    {
      AddCounter(partName[iP],           partTitle[iP] + "     ");
      AddCounter(partName[iP] + "_prim", partTitle[iP] + " Prim");
      AddCounter(partName[iP] + "_sec",  partTitle[iP] + " Sec ");
    }

    for(int iP=0; iP<nParticles; iP++)
      fPdgToIndex[particleInfo[iP].PDG()] = iP;
  }

  virtual ~KFPartEfficiencies(){};

  int GetParticleIndex(int pdg)
  {
    std::map<int, int>::iterator it;
    it=fPdgToIndex.find(pdg);
    if(it != fPdgToIndex.end()) return it->second;
    else return -1;
  }

  std::map<int,int> GetPdgToIndexMap() const { return fPdgToIndex; }
  
  virtual void AddCounter(std::string shortname, std::string name){
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
  

  void Inc(bool isReco, int nClones, bool isMC1, bool isMC2, bool isMC3, std::string name)
  {
    const int index = indices[name];
    
    if(isMC1) mc1.counters[index]++;
    if(isMC2) mc2.counters[index]++;
    if(isMC3) mc3.counters[index]++;
    
    if(isReco) reco.counters[index]++;
    if(nClones > 0)
      clone.counters[index] += nClones;
  };

  void IncReco(bool isGhost, bool isBg, std::string name){
    const int index = indices[name];

    if (isGhost) ghost.     counters[index]++;
    if (isBg)    bg.counters[index]++;
  };

  void PrintEff(){
    std::ios_base::fmtflags original_flags = std::cout.flags();
    std::cout.setf(std::ios::fixed);
    std::cout.setf(std::ios::showpoint);
    std::cout.precision(3);
    std::cout << "Particle             : "
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

  friend std::fstream & operator<<(std::fstream &strm, KFPartEfficiencies &a) 
  {

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

  friend std::fstream & operator>>(std::fstream &strm, KFPartEfficiencies &a)
  {

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

  void AddFromFile(std::string fileName)
  {
    std::fstream file(fileName.data(),std::fstream::in);
    file >> *this;
  }
  
  int GetNDaughters(int iParticle) const { return partDaughterPdg[iParticle].size(); }
  int GetDaughterPDG(int iParticle, int iDaughter) const { return partDaughterPdg[iParticle][iDaughter]; }
  
  static const int nParticles = 153;
  static const int fFirstHypernucleusIndex = 73;
  static const int fLastHypernucleusIndex = 89;  
  static const int fFirstMissingMassParticleIndex = 90;
  static const int fLastMissingMassParticleIndex = 125;  
  static const int fFirstStableParticleIndex = 126;
  static const int fLastStableParticleIndex = 143;
  
  int partPDG[nParticles];
  std::string partName[nParticles];
  std::string partTitle[nParticles];
  std::vector<std::vector<int> > partDaughterPdg;
  float partMHistoMin[nParticles];
  float partMHistoMax[nParticles];
  int partMaxMult[nParticles];
  float partMass[nParticles];
  float partLifeTime[nParticles];
  int partCharge[nParticles];

 private:
  std::vector<std::string> names; // names counters indexed by index of counter
  std::map<std::string, int> indices; // indices of counters indexed by a counter shortname

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
