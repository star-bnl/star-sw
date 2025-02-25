/*
 * This file is part of KFParticle package
 * Copyright (C) 2007-2019 FIAS Frankfurt Institute for Advanced Studies
 *               2007-2019 Goethe University of Frankfurt
 *               2007-2019 Ivan Kisel <I.Kisel@compeng.uni-frankfurt.de>
 *               2007-2019 Maksym Zyzak
 *
 * KFParticle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * KFParticle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

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

/** @class KFEfficiencyParticleInfo
 ** @brief A helper class to define parameters of the decay list in KFPartEfficiencies.
 ** @author  M.Zyzak, I.Kisel
 ** @date 05.02.2019
 ** @version 1.0
 **/

class KFEfficiencyParticleInfo
{
 public:
  KFEfficiencyParticleInfo():fName("null"),fTitle("null"),fPDG(0),fHistoMin(0.f),fHistoMax(0.f),fMass(0.f),fLifeTime(0.f),fCharge(0), fMassSigma(0.001) {};
  /** \brief Constructor with all parameters set in. There is no other way to define the parameters other then use this constructor.*/
  KFEfficiencyParticleInfo(std::string name, std::string title, int pdg, float histoMin, float histoMax, float mass, float lifeTime, int charge, float massSigma ):
    fName(name), fTitle(title), fPDG(pdg), fHistoMin(histoMin), fHistoMax(histoMax), fMass(mass), fLifeTime(lifeTime), fCharge(charge), fMassSigma(massSigma) {};
  ~KFEfficiencyParticleInfo() {};
  
  //accessors
  std::string Name()      const { return fName; }      ///< Returns name of the decay in the file with histograms.
  std::string Title()     const { return fTitle; }     ///< Returns name of the decay in the output table with efficiency.
  int         PDG()       const { return fPDG; }       ///< Returns the assigned PDG code.
  float       HistoMin()  const { return fHistoMin; }  ///< Returns lower boundary in the mass histogram for the current decay.
  float       HistoMax()  const { return fHistoMax; }  ///< Returns upper boundary in the mass histogram for the current decay.
  float       Mass()      const { return fMass; }      ///< Returns table mass of the particle.
  float       LifeTime()  const { return fLifeTime; }  ///< Returns lifetime of the particle.
  int         Charge()    const { return fCharge; }    ///< Returns charge of the particle in units of the elementary charge.
  float       MassSigma() const { return fMassSigma; } ///< Returns expected width of the mass peak, used in the side bands method.
  
 private:
  std::string fName;  ///< Name of the decay in the file with histograms.
  std::string fTitle; ///< Name of the decay in the output table with efficiency.
  int fPDG;           ///< PDG code assigned to the current decay in the scheme of KF Particle Finder.
  float fHistoMin;    ///< Lower boundary in the mass histogram for the current decay.
  float fHistoMax;    ///< Upper boundary in the mass histogram for the current decay.
  float fMass;        ///< Table mass of the particle.
  float fLifeTime;    ///< Lifetime of the particle in seconds.
  int fCharge;        ///< Charge in units of the elementary charge.
  float fMassSigma;   ///< Expected width of the decay, determines peak sigma for the side bands method.
};

/** @class KFPartEfficiencies
 ** @brief Class to calculate efficiency of KF Particle Finder.
 ** @author  M.Zyzak, I.Kisel
 ** @date 05.02.2019
 ** @version 1.0
 **
 ** The class has two main purposes:\n
 ** 1) Defines the list of decays to be analysed: a unique code of the decay, its mass, lifetime,
 ** a list of daughter particles, etc. See KFPartEfficiencies::KFPartEfficiencies() for more details.\n
 ** 2) It calculates reconstruction efficiency of the decays from the KF Particle Finder scheme.\n
 ** Definitions:\n
 ** background - physics background, when daughter particle come from the real particle, but the pdg
 ** hypothesis is incorrect, for example, Lambda->p pi will create a physics background for
 ** K0s if the proton is misidentified;\n
 ** ghost - combinatorial background, tracks do not form a real vertex;\n
 ** clone - a particle is reconstructed several times, for example, particle track is split into 
 ** to parts due to the multiple scattering.
 **/

class KFPartEfficiencies :public TObject
{
 public:

  /** \brief The default constructor. Defines the list of decays to be analysed and their properties. Please, see the code for indexing scheme. */
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
#ifdef CBM
      KFEfficiencyParticleInfo("Ks",               "KShort        ",        310, 0.3f, 1.3f, 0.497614   , 8.954e-11,  0, 0.0045), //0
#else
      KFEfficiencyParticleInfo("Ks",               "KShort        ",        310, 0.3f, 1.3f, 0.497614   , 8.954e-11,  0, 0.0057), //0
#endif
      KFEfficiencyParticleInfo("Lambda",           "Lambda        ",       3122, 1.0f, 2.0f, 1.115683   , 2.632e-10,  0, 0.0020), //1
      KFEfficiencyParticleInfo("Lambdab",          "Lambda b      ",      -3122, 1.0f, 2.0f, 1.115683   , 2.632e-10,  0, 0.0020), //2
      KFEfficiencyParticleInfo("Xi-",              "Xi-           ",       3312, 1.0f, 3.0f, 1.32171    , 1.639e-10, -1, 0.0022), //3
      KFEfficiencyParticleInfo("Xi+",              "Xi+           ",      -3312, 1.0f, 3.0f, 1.32171    , 1.639e-10,  1, 0.0022), //4
      KFEfficiencyParticleInfo("Xi0",              "Xi0           ",       3322, 1.0f, 3.0f, 1.31486    , 2.9e-10,    0, 0.0030), //5
      KFEfficiencyParticleInfo("Xi0b",             "Xi0 b         ",      -3322, 1.0f, 3.0f, 1.31486    , 2.9e-10,    0, 0.0030), //6
      KFEfficiencyParticleInfo("Omega-",           "Omega-        ",       3334, 1.0f, 3.0f, 1.67245    , 0.821e-10, -1, 0.0022), //7
      KFEfficiencyParticleInfo("Omega+",           "Omega+        ",      -3334, 1.0f, 3.0f, 1.67245    , 0.821e-10,  1, 0.0022), //8
      KFEfficiencyParticleInfo("Sigma^0",          "Sigma0        ",       3212, 1.0f, 3.0f, 1.192642   , 7.4e-20,    0, 0.0030), //9
      KFEfficiencyParticleInfo("Sigma^0b",         "Sigma0 b      ",      -3212, 1.0f, 3.0f, 1.192642   , 7.4e-20,    0, 0.0030), //10
      KFEfficiencyParticleInfo("Sigma^+",          "Sigma+        ",       3222, 1.0f, 3.0f, 1.18937    , 0.8018e-10, 1, 0.0030), //11
      KFEfficiencyParticleInfo("Sigma^-b",         "Sigma- b      ",      -3222, 1.0f, 3.0f, 1.18937    , 0.8018e-10,-1, 0.0030), //12
      KFEfficiencyParticleInfo("K*0",              "K*0           ",        313, 0.6f, 2.6f, 0.8958     , 1.38e-23,   0, 0.0300), //13
      KFEfficiencyParticleInfo("K*0b",             "K*0 b         ",       -313, 0.6f, 2.6f, 0.8958     , 1.38e-23,   0, 0.0300), //14
      KFEfficiencyParticleInfo("K*+",              "K*+           ",        323, 0.6f, 2.6f, 0.89166    , 1.30e-23,   1, 0.0300), //15
      KFEfficiencyParticleInfo("K*-",              "K*-           ",       -323, 0.6f, 2.6f, 0.89166    , 1.30e-23,  -1, 0.0300), //16
      KFEfficiencyParticleInfo("K*0_K0,pi0",       "K*0_K0pi0     ",     100313, 0.6f, 2.6f, 0.8958     , 1.38e-23,   0, 0.0030), //17
      KFEfficiencyParticleInfo("K*+_K+,pi0",       "K*+_K+pi0     ",     100323, 0.6f, 2.6f, 0.89166    , 1.30e-23,   1, 0.0030), //18
      KFEfficiencyParticleInfo("K*-_K-,pi0",       "K*-_K-pi0     ",    -100323, 0.6f, 2.6f, 0.89166    , 1.30e-23,  -1, 0.0030), //19
      KFEfficiencyParticleInfo("Sigma*+",          "Sigma*+       ",       3224, 1.0f, 3.0f, 1.3828     , 1.83e-23,   1, 0.0100), //20
      KFEfficiencyParticleInfo("Sigma*-",          "Sigma*-       ",       3114, 1.0f, 3.0f, 1.3872     , 1.67e-23,  -1, 0.0100), //21
      KFEfficiencyParticleInfo("Sigma*+b",         "Sigma*+ b     ",      -3114, 1.0f, 3.0f, 1.3828     , 1.83e-23,  -1, 0.0100), //22
      KFEfficiencyParticleInfo("Sigma*-b",         "Sigma*- b     ",      -3224, 1.0f, 3.0f, 1.3872     , 1.67e-23,   1, 0.0100), //23
      KFEfficiencyParticleInfo("Sigma*0",          "Sigma*0       ",       3214, 1.0f, 3.0f, 1.3837     , 1.83e-23,   0, 0.0030), //24
      KFEfficiencyParticleInfo("Sigma*0b",         "Sigma*0 b     ",      -3214, 1.0f, 3.0f, 1.3837     , 1.83e-23,   0, 0.0030), //25
      KFEfficiencyParticleInfo("Lambda*",          "Lambda*       ",       3124, 1.4f, 3.4f, 1.5195     , 4.22e-23,   0, 0.0100), //26
      KFEfficiencyParticleInfo("Lambda*b",         "Lambda* b     ",      -3124, 1.4f, 3.4f, 1.5195     , 4.22e-23,   0, 0.0100), //27
      KFEfficiencyParticleInfo("Xi*0",             "Xi*0          ",       3324, 1.4f, 3.4f, 1.53180    , 7.23e-23,   0, 0.0100), //28
      KFEfficiencyParticleInfo("Xi*0b",            "Xi*0 b        ",      -3324, 1.4f, 3.4f, 1.53180    , 7.23e-23,   0, 0.0100), //29
      KFEfficiencyParticleInfo("Xi*-_LK",          "Xi*-_lk       ",    1003314, 1.4f, 3.4f, 1.823      , 2.74e-23,  -1, 0.0030), //30
      KFEfficiencyParticleInfo("Xi*+_LK",          "Xi*+_lk       ",   -1003314, 1.4f, 3.4f, 1.823      , 2.74e-23,   1, 0.0030), //31
      KFEfficiencyParticleInfo("Xi*-_xi-,pi0",     "Xi*-_XiPi     ",       3314, 1.4f, 3.4f, 1.535      , 6.65e-23,  -1, 0.0030), //32
      KFEfficiencyParticleInfo("Xi*+_xi+,pi0",     "Xi*+_XiPi     ",      -3314, 1.4f, 3.4f, 1.535      , 6.65e-23,   1, 0.0030), //33
      KFEfficiencyParticleInfo("Omega*-",          "Omega*-       ",    1003334, 1.8f, 3.8f, 2.252      , 1.2e-23,   -1, 0.0030), //34
      KFEfficiencyParticleInfo("Omega*+",          "Omega*+       ",   -1003334, 1.8f, 3.8f, 2.252      , 1.2e-23,    1, 0.0030), //35
      KFEfficiencyParticleInfo("H0_LL",            "H0_LL         ",       3000, 1.5f, 3.5f, 2.21       , 1.32e-10,   0, 0.0030), //36
      KFEfficiencyParticleInfo("phi_KK",           "phi_KK        ",        333, 0.8f, 2.8f, 1.019455   , 1.55e-22,   0, 0.0030), //37
      KFEfficiencyParticleInfo("rho_pipi",         "rho_pipi      ",        113, 0.0f, 2.0f, 0.77526    , 4.45e-24,   0, 0.0030), //38
      KFEfficiencyParticleInfo("rho_ee",           "rho_ee        ",     100113, 0.0f, 2.0f, 0.77526    , 4.45e-24,   0, 0.0030), //39
      KFEfficiencyParticleInfo("rho_mm",           "rho_mm        ",     200113, 0.0f, 2.0f, 0.77526    , 4.45e-24,   0, 0.0030), //40
      KFEfficiencyParticleInfo("gamma",            "gamma         ",         22, 0.0f, 3.0f, 0.         , 1.e20,      0, 0.0030), //41
      KFEfficiencyParticleInfo("pi0",              "pi0           ",        111, 0.0f, 3.0f, 0.1349766  , 8.52e-17,   0, 0.0030), //42
      KFEfficiencyParticleInfo("eta",              "eta           ",        221, 0.0f, 3.0f, 0.547862   , 5.0e-19,    0, 0.0030), //43
      KFEfficiencyParticleInfo("K+_{3pi}",         "K+3pi         ",     100321, 0.0f, 1.0f, 0.493677   , 1.238e-8,   1, 0.0030), //44
      KFEfficiencyParticleInfo("K-_{3pi}",         "K+3pi         ",    -100321, 0.0f, 1.0f, 0.493677   , 1.238e-8,  -1, 0.0030), //45
      KFEfficiencyParticleInfo("K+_{3piK}",        "K+3piK        ",     200321, 0.0f, 1.0f, 0.493677   , 1.238e-8,   1, 0.0030), //46
      KFEfficiencyParticleInfo("K-_{3piK}",        "K+3piK        ",    -200321, 0.0f, 1.0f, 0.493677   , 1.238e-8,  -1, 0.0030), //47
//Delta and N resonances
      KFEfficiencyParticleInfo("Delta0",           "Delta0        ",       2114, 1.0f, 3.0f, 1.232      , 5.63e-24,   0, 0.0030), //48
      KFEfficiencyParticleInfo("Delta0 b",         "Delta0 b      ",      -2114, 1.0f, 3.0f, 1.232      , 5.63e-24,   0, 0.0030), //49
      KFEfficiencyParticleInfo("Delta++",          "Delta++       ",       2224, 1.0f, 3.0f, 1.232      , 5.63e-24,   2, 0.0030), //50
      KFEfficiencyParticleInfo("Delta-- b",        "Delta-- b     ",      -2224, 1.0f, 3.0f, 1.232      , 5.63e-24,  -2, 0.0030), //51
//Nuclear resonances
      KFEfficiencyParticleInfo("dpi-",             "dpi-          ",     100001, 2.0f, 4.0f, 2.170      , 5.63e-24,   0, 0.0030), //52
      KFEfficiencyParticleInfo("dpi+",             "dpi+          ",     100002, 2.0f, 4.0f, 2.170      , 5.63e-24,   0, 0.0030), //53
      KFEfficiencyParticleInfo("tpi-",             "tpi-          ",     100003, 2.8f, 4.8f, 3.108      , 5.63e-24,   0, 0.0030), //54
      KFEfficiencyParticleInfo("tpi+",             "tpi+          ",     100004, 2.8f, 4.8f, 3.108      , 5.63e-24,   0, 0.0030), //55
      KFEfficiencyParticleInfo("He3pi-",           "He3pi-        ",     100005, 2.8f, 4.8f, 3.108      , 5.63e-24,   0, 0.0030), //56
      KFEfficiencyParticleInfo("He3pi+",           "He3pi+        ",     100006, 2.8f, 4.8f, 3.108      , 5.63e-24,   0, 0.0030), //57
      KFEfficiencyParticleInfo("He4pi-",           "He4pi-        ",     100007, 3.7f, 5.7f, 4.046      , 5.63e-24,   0, 0.0030), //58
      KFEfficiencyParticleInfo("He4pi+",           "He4pi+        ",     100008, 3.7f, 5.7f, 4.046      , 5.63e-24,   0, 0.0030), //59
      KFEfficiencyParticleInfo("He6pi-",           "He6pi-        ",     100009, 5.6f, 7.6f, 5.922      , 5.63e-24,   0, 0.0030), //60
      KFEfficiencyParticleInfo("He6pi+",           "He6pi+        ",     100010, 5.6f, 7.6f, 5.922      , 5.63e-24,   0, 0.0030), //61
      KFEfficiencyParticleInfo("Li6pi-",           "Li6pi-        ",     100011, 5.6f, 7.6f, 5.922      , 5.63e-24,   0, 0.0030), //62
      KFEfficiencyParticleInfo("Li6pi+",           "Li6pi+        ",     100012, 5.6f, 7.6f, 5.922      , 5.63e-24,   0, 0.0030), //63
      KFEfficiencyParticleInfo("Li7pi-",           "Li7pi-        ",     100013, 6.5f, 8.5f, 6.860      , 5.63e-24,   0, 0.0030), //64
      KFEfficiencyParticleInfo("Li7pi+",           "Li7pi+        ",     100014, 6.5f, 8.5f, 6.860      , 5.63e-24,   0, 0.0030), //65
      KFEfficiencyParticleInfo("Be7pi-",           "Be7pi-        ",     100015, 6.5f, 8.5f, 6.860      , 5.63e-24,   0, 0.0030), //66
      KFEfficiencyParticleInfo("Be7pi+",           "Be7pi+        ",     100016, 6.5f, 8.5f, 6.860      , 5.63e-24,   0, 0.0030), //67
      KFEfficiencyParticleInfo("dK-",              "dK-           ",     110001, 2.2f, 4.2f, 2.170      , 5.63e-24,   0, 0.0030), //68
      KFEfficiencyParticleInfo("dK+",              "dK+           ",     110002, 2.2f, 4.2f, 2.170      , 5.63e-24,   0, 0.0030), //69
      KFEfficiencyParticleInfo("tK-",              "tK-           ",     110003, 2.8f, 4.8f, 3.108      , 5.63e-24,   0, 0.0030), //70
      KFEfficiencyParticleInfo("tK+",              "tK+           ",     110004, 2.8f, 4.8f, 3.108      , 5.63e-24,   0, 0.0030), //71
      KFEfficiencyParticleInfo("He3K-",            "He3K-         ",     110005, 2.8f, 4.8f, 3.108      , 5.63e-24,   0, 0.0030), //72
      KFEfficiencyParticleInfo("He3K+",            "He3K+         ",     110006, 2.8f, 4.8f, 3.108      , 5.63e-24,   0, 0.0030), //73
      KFEfficiencyParticleInfo("He4K-",            "He4K-         ",     110007, 3.7f, 5.7f, 4.046      , 5.63e-24,   0, 0.0030), //74
      KFEfficiencyParticleInfo("He4K+",            "He4K+         ",     110008, 3.7f, 5.7f, 4.046      , 5.63e-24,   0, 0.0030), //75
      KFEfficiencyParticleInfo("He6K-",            "He6K-         ",     110009, 5.6f, 7.6f, 5.922      , 5.63e-24,   0, 0.0030), //76
      KFEfficiencyParticleInfo("He6K+",            "He6K+         ",     110010, 5.6f, 7.6f, 5.922      , 5.63e-24,   0, 0.0030), //77
      KFEfficiencyParticleInfo("Li6K-",            "Li6K-         ",     110011, 5.6f, 7.6f, 5.922      , 5.63e-24,   0, 0.0030), //78
      KFEfficiencyParticleInfo("Li6K+",            "Li6K+         ",     110012, 5.6f, 7.6f, 5.922      , 5.63e-24,   0, 0.0030), //79
      KFEfficiencyParticleInfo("Li7K-",            "Li7K-         ",     110013, 6.5f, 8.5f, 6.860      , 5.63e-24,   0, 0.0030), //80
      KFEfficiencyParticleInfo("Li7K+",            "Li7K+         ",     110014, 6.5f, 8.5f, 6.860      , 5.63e-24,   0, 0.0030), //81
      KFEfficiencyParticleInfo("Be7K-",            "Be7K-         ",     110015, 6.5f, 8.5f, 6.860      , 5.63e-24,   0, 0.0030), //82
      KFEfficiencyParticleInfo("Be7K+",            "Be7K+         ",     110016, 6.5f, 8.5f, 6.860      , 5.63e-24,   0, 0.0030), //83
      KFEfficiencyParticleInfo("pp",               "pp            ",     200001, 1.7f, 3.7f, 1.876      , 5.63e-24,   0, 0.0030), //84
      KFEfficiencyParticleInfo("dp",               "dp            ",     200002, 2.7f, 4.7f, 2.813      , 5.63e-24,   0, 0.0030), //85
      KFEfficiencyParticleInfo("tp",               "tp            ",     200003, 3.6f, 4.6f, 3.747      , 5.63e-24,   0, 0.0030), //86
      KFEfficiencyParticleInfo("He3p",             "He3p          ",     200004, 3.6f, 4.6f, 3.747      , 5.63e-24,   0, 0.0030), //87
      KFEfficiencyParticleInfo("He4p",             "He4p          ",     200005, 4.5f, 6.5f, 4.665      , 5.63e-24,   0, 0.0030), //88
      KFEfficiencyParticleInfo("He6p",             "He6p          ",     200006, 6.4f, 8.4f, 6.539      , 5.63e-24,   0, 0.0030), //89
      KFEfficiencyParticleInfo("Li6p",             "Li6p          ",     200007, 6.4f, 8.4f, 6.539      , 5.63e-24,   0, 0.0030), //90
      KFEfficiencyParticleInfo("Li7p",             "Li7p          ",     200008, 7.3f, 9.3f, 7.472      , 5.63e-24,   0, 0.0030), //91
      KFEfficiencyParticleInfo("Be7p",             "Be7p          ",     200009, 7.3f, 9.3f, 7.472      , 5.63e-24,   0, 0.0030), //92
//charmonium
      KFEfficiencyParticleInfo("JPsi_ee",          "JPsi_ee       ",        443, 1.0f, 4.0f, 3.096916   , 7.1e-21,    0, 0.0030), //93
      KFEfficiencyParticleInfo("JPsi_mumu",        "JPsi_mm       ",     100443, 1.0f, 4.0f, 3.096916   , 7.1e-21,    0, 0.0030), //94
      KFEfficiencyParticleInfo("JPsi_pp",          "JPsi_pp       ",     200443, 1.0f, 4.0f, 3.096916   , 7.1e-21,    0, 0.0030), //95
      KFEfficiencyParticleInfo("JPsi_LL",          "JPsi_LL       ",     300443, 2.0f, 5.0f, 3.096916   , 7.1e-21,    0, 0.0030), //96
      KFEfficiencyParticleInfo("JPsi_XiXi",        "JPsi_XiXi     ",     400443, 2.0f, 5.0f, 3.096916   , 7.1e-21,    0, 0.0030), //97
      KFEfficiencyParticleInfo("Psi_OO",           "Psi_OO        ",     500443, 3.0f, 6.0f, 3.686109   , 2.1e-22,    0, 0.0030), //98
//open charm
      KFEfficiencyParticleInfo("D0",               "D0            ",        421, 0.6f, 3.6f, 1.86486    , 4.1e-13,    0, 0.0154), //99
      KFEfficiencyParticleInfo("D0b",              "D0b           ",       -421, 0.6f, 3.6f, 1.86486    , 4.1e-13,    0, 0.0154), //100
      KFEfficiencyParticleInfo("D0_4",             "D0_4          ",        429, 0.6f, 3.6f, 1.86486    , 4.1e-13,    0, 0.0100), //101
      KFEfficiencyParticleInfo("D0b_4",            "D0b_4         ",       -429, 0.6f, 3.6f, 1.86486    , 4.1e-13,    0, 0.0100), //102
      KFEfficiencyParticleInfo("D0_pipi",          "D0_pipi       ",        420, 0.6f, 3.6f, 1.86486    , 4.1e-13,    0, 0.0154), //103
      KFEfficiencyParticleInfo("D0_2pi2pi",        "D0_2pi2pi     ",        470, 0.6f, 3.6f, 1.86486    , 4.1e-13,    0, 0.0154), //104
      KFEfficiencyParticleInfo("D0_K0pipi",        "D0_K0pipi     ",        425, 0.6f, 3.6f, 1.86486    , 4.1e-13,    0, 0.0150), //105
      KFEfficiencyParticleInfo("D0_KK",            "D0_KK         ",        426, 0.6f, 3.6f, 1.86486    , 4.1e-13,    0, 0.0130), //106
      KFEfficiencyParticleInfo("D0_KKK0",          "D0_KKK0       ",        427, 0.6f, 3.6f, 1.86486    , 4.1e-13,    0, 0.0154), //107
      KFEfficiencyParticleInfo("D0_pi0",           "D0_#pi0       ",        428, 1.0f, 3.0f, 1.86486    , 4.1e-13,    0, 0.0030), //108
      KFEfficiencyParticleInfo("D+",               "D+            ",        411, 1.0f, 3.0f, 1.86962    , 1.04e-13,   1, 0.0114), //109
      KFEfficiencyParticleInfo("D-",               "D-            ",       -411, 1.0f, 3.0f, 1.86962    , 1.04e-13,  -1, 0.0114), //110
      KFEfficiencyParticleInfo("D+_K0pi+",         "D+_K0pi+      ",     100411, 0.6f, 4.6f, 1.86962    , 1.04e-13,   1, 0.0030), //111
      KFEfficiencyParticleInfo("D-_K0pi-",         "D-_K0pi-      ",    -100411, 0.6f, 4.6f, 1.86962    , 1.04e-13,  -1, 0.0030), //112
      KFEfficiencyParticleInfo("D+_K03pi",         "D+_K03pi      ",     200411, 0.6f, 4.6f, 1.86962    , 1.04e-13,   1, 0.0030), //113
      KFEfficiencyParticleInfo("D-_K03pi",         "D-_K03pi      ",    -200411, 0.6f, 4.6f, 1.86962    , 1.04e-13,  -1, 0.0030), //114
      KFEfficiencyParticleInfo("D+_3pi",           "D+_3pi        ",     300411, 0.6f, 4.6f, 1.86962    , 1.04e-13,   1, 0.0030), //115
      KFEfficiencyParticleInfo("D-_3pi",           "D-_3pi        ",    -300411, 0.6f, 4.6f, 1.86962    , 1.04e-13,  -1, 0.0030), //116
      KFEfficiencyParticleInfo("Ds+",              "Ds+           ",        431, 1.0f, 3.0f, 1.96850    , 5.0e-13,    1, 0.0110), //117
      KFEfficiencyParticleInfo("Ds-",              "Ds-           ",       -431, 1.0f, 3.0f, 1.96850    , 5.0e-13,   -1, 0.0110), //118
      KFEfficiencyParticleInfo("Ds+_K0K+",         "Ds+_K0K+      ",     100431, 1.0f, 3.0f, 1.96850    , 5.0e-13,    1, 0.0030), //119
      KFEfficiencyParticleInfo("Ds-_K0K-",         "Ds-_K0K-      ",    -100431, 1.0f, 3.0f, 1.96850    , 5.0e-13,   -1, 0.0030), //120
      KFEfficiencyParticleInfo("Ds+_K0K0pi+",      "Ds+_K0K0pi+   ",     200431, 1.0f, 3.0f, 1.96850    , 5.0e-13,    1, 0.0030), //121
      KFEfficiencyParticleInfo("Ds-_K0K0pi-",      "Ds-_K0K0pi-   ",    -200431, 1.0f, 3.0f, 1.96850    , 5.0e-13,   -1, 0.0030), //122
      KFEfficiencyParticleInfo("Ds+_K0K+pipi",     "Ds+_K0K+pipi  ",     300431, 1.0f, 3.0f, 1.96850    , 5.0e-13,    1, 0.0030), //123
      KFEfficiencyParticleInfo("Ds-_K0K-pipi",     "Ds-_K0K-pipi  ",    -300431, 1.0f, 3.0f, 1.96850    , 5.0e-13,   -1, 0.0030), //124
      KFEfficiencyParticleInfo("Ds+_K+pipi",       "Ds+_K+pipi    ",     400431, 1.0f, 3.0f, 1.96850    , 5.0e-13,    1, 0.0030), //125
      KFEfficiencyParticleInfo("Ds-_K-pipi",       "Ds-_K-pipi    ",    -400431, 1.0f, 3.0f, 1.96850    , 5.0e-13,   -1, 0.0030), //126
      KFEfficiencyParticleInfo("Lc",               "Lambdac       ",       4122, 1.8f, 3.8f, 2.28646    , 2.0e-13,    1, 0.0110), //127
      KFEfficiencyParticleInfo("Lcb",              "Lambdac b     ",      -4122, 1.8f, 3.8f, 2.28646    , 2.0e-13,   -1, 0.0110), //128
      KFEfficiencyParticleInfo("Lc_{pK0}",         "Lc   {pK0}    ",     104122, 1.8f, 3.8f, 2.28646    , 2.0e-13,    1, 0.0030), //129
      KFEfficiencyParticleInfo("Lcb_{pK0}",        "Lc b {pK0}    ",    -104122, 1.8f, 3.8f, 2.28646    , 2.0e-13,   -1, 0.0030), //130
      KFEfficiencyParticleInfo("Lc_{pK02pi}",      "Lc   {pK02pi} ",     204122, 1.8f, 3.8f, 2.28646    , 2.0e-13,    1, 0.0030), //131
      KFEfficiencyParticleInfo("Lcb_{pK02pi}",     "Lc b {pK02pi} ",    -204122, 1.8f, 3.8f, 2.28646    , 2.0e-13,   -1, 0.0030), //132
      KFEfficiencyParticleInfo("Lc_{Lpi}",         "Lc   {Lpi}    ",     304122, 1.8f, 3.8f, 2.28646    , 2.0e-13,    1, 0.0030), //133
      KFEfficiencyParticleInfo("Lcb_{Lpi}",        "Lc b {Lpi}    ",    -304122, 1.8f, 3.8f, 2.28646    , 2.0e-13,   -1, 0.0030), //134 
      KFEfficiencyParticleInfo("Lc_{L3pi}",        "Lc   {L3pi}   ",     404122, 1.8f, 3.8f, 2.28646    , 2.0e-13,    1, 0.0030), //135
      KFEfficiencyParticleInfo("Lcb_{L3pi}",       "Lc b {L3pi}   ",    -404122, 1.8f, 3.8f, 2.28646    , 2.0e-13,   -1, 0.0030), //136 
      KFEfficiencyParticleInfo("Lc_{p2pi}",        "Lc   {p2pi}   ",     504122, 1.8f, 3.8f, 2.28646    , 2.0e-13,    1, 0.0030), //137
      KFEfficiencyParticleInfo("Lcb_{p2pi}",       "Lc b {p2pi}   ",    -504122, 1.8f, 3.8f, 2.28646    , 2.0e-13,   -1, 0.0030), //138 
      KFEfficiencyParticleInfo("Xic0",             "Xic0          ",       4132, 2.1f, 4.1f, 2.47087    , 1.0e-13,    0, 0.0030), //139
      KFEfficiencyParticleInfo("Xic0b",            "Xic0b         ",      -4132, 2.1f, 4.1f, 2.47087    , 1.0e-13,    0, 0.0030), //140
      KFEfficiencyParticleInfo("D*0",              "D*0           ",      10421, 1.8f, 3.8f, 2.00699    , 3.0e-22,    0, 0.0030), //141
      KFEfficiencyParticleInfo("D*0b",             "D*0 b         ",     -10421, 1.8f, 3.8f, 2.00699    , 3.0e-22,    0, 0.0030), //142
      KFEfficiencyParticleInfo("D*+",              "D*+           ",      10411, 1.8f, 3.8f, 2.01029    , 6.86e-21,   1, 0.0030), //143
      KFEfficiencyParticleInfo("D*-",              "D*-           ",     -10411, 1.8f, 3.8f, 2.01029    , 6.86e-21,  -1, 0.0030), //144
      KFEfficiencyParticleInfo("D*+_4",            "D*+_4         ",      20411, 1.8f, 3.8f, 2.01029    , 6.86e-21,   1, 0.0030), //145
      KFEfficiencyParticleInfo("D*-_4",            "D*-_4         ",     -20411, 1.8f, 3.8f, 2.01029    , 6.86e-21,  -1, 0.0030), //146
      KFEfficiencyParticleInfo("D0*_pi0",          "D0*_#pi0      ",      10428, 1.8f, 3.8f, 2.00699    , 6.86e-21,   0, 0.0030), //147
//B mesons
      KFEfficiencyParticleInfo("B_Jpsi_ee",        "B_Jpsi_ee     ",        500, 1.0f, 4.0f, 3.096916   , 7.1e-21,    0, 0.0030), //148
      KFEfficiencyParticleInfo("B_Jpsi_mm",        "B_Jpsi_mm     ",        501, 1.0f, 4.0f, 3.096916   , 7.1e-21,    0, 0.0030), //149
      KFEfficiencyParticleInfo("B+_D0bPi+",        "B+ {D0bPi+}   ",        521, 3.0f, 7.0f, 5.27931    , 1.638e-12,  0, 0.0030), //150
      KFEfficiencyParticleInfo("B-_D0Pi-",         "B- {D0Pi-}    ",       -521, 3.0f, 7.0f, 5.27931    , 1.638e-12,  0, 0.0030), //151
      KFEfficiencyParticleInfo("B+_D0bK+",         "B+ {D0bK+}    ",        529, 3.0f, 7.0f, 5.27931    , 1.638e-12,  0, 0.0030), //152
      KFEfficiencyParticleInfo("B-_D0K-",          "B- {D0K+}     ",       -529, 3.0f, 7.0f, 5.27931    , 1.638e-12,  0, 0.0030), //153
      KFEfficiencyParticleInfo("B0_D-Pi+",         "B0 {D-Pi+}    ",        511, 3.0f, 7.0f, 5.27962    , 1.520e-12,  0, 0.0030), //154
      KFEfficiencyParticleInfo("B0b_D+Pi-",        "B0b {D+Pi-}   ",       -511, 3.0f, 7.0f, 5.27962    , 1.520e-12,  0, 0.0030), //155
      KFEfficiencyParticleInfo("B0_D-K+",          "B0 {D-K+}     ",        519, 3.0f, 7.0f, 5.27962    , 1.520e-12,  0, 0.0030), //156
      KFEfficiencyParticleInfo("B0b_D+K-",         "B0b {D+K-}    ",       -519, 3.0f, 7.0f, 5.27962    , 1.520e-12,  0, 0.0030), //157
      KFEfficiencyParticleInfo("H0_{Lppi}",        "H0            ",       3001, 2.0f, 4.0f, 2.21       , 1.32e-10,   0, 0.0030), //158
//hypernuclei
      KFEfficiencyParticleInfo("LambdaN",          "LambdaN       ",       3003, 1.0f, 3.0f, 2.046      , 2.632e-10,  0, 0.0025), //159
      KFEfficiencyParticleInfo("LambdaNb",         "LambdaN b     ",      -3003, 1.0f, 3.0f, 2.046      , 2.632e-10,  0, 0.0025), //160
      KFEfficiencyParticleInfo("LambdaNN",         "LambdaNN      ",       3103, 2.0f, 4.0f, 2.99352    , 2.632e-10,  0, 0.0020), //161
      KFEfficiencyParticleInfo("LambdaNNb",        "LambdaNN b    ",      -3103, 2.0f, 4.0f, 2.99352    , 2.632e-10,  0, 0.0020), //162
      KFEfficiencyParticleInfo("H3L",              "H3L           ",       3004, 2.0f, 4.0f, 2.9908     , 2.632e-10,  1, 0.0020), //163
      KFEfficiencyParticleInfo("H3Lb",             "H3L b         ",      -3004, 2.0f, 4.0f, 2.9908     , 2.632e-10, -1, 0.0020), //164
      KFEfficiencyParticleInfo("H3L_{dppi}",       "H3L_{dppi}    ",       3012, 2.0f, 4.0f, 2.9908     , 2.632e-10,  1, 0.0025), //165
      KFEfficiencyParticleInfo("H3L_{dppi}b",      "H3L_{dppi} b  ",      -3012, 2.0f, 4.0f, 2.9908     , 2.632e-10, -1, 0.0025), //166
      KFEfficiencyParticleInfo("H4L",              "H4L           ",       3005, 3.0f, 5.0f, 3.9224     , 2.632e-10,  1, 0.0020), //167
      KFEfficiencyParticleInfo("H4Lb",             "H4L b         ",      -3005, 3.0f, 5.0f, 3.9224     , 2.632e-10, -1, 0.0020), //168
      KFEfficiencyParticleInfo("H4L_{tppi}",       "H4L_{tppi}    ",       3013, 3.0f, 5.0f, 3.9224     , 2.632e-10,  1, 0.0025), //169
      KFEfficiencyParticleInfo("H4L_{tppi}b",      "H4L_{tppi} b  ",      -3013, 3.0f, 5.0f, 3.9224     , 2.632e-10, -1, 0.0025), //170
      KFEfficiencyParticleInfo("H4L_{ddpi}",       "H4L_{ddpi}    ",       3014, 3.0f, 5.0f, 3.9224     , 2.632e-10,  1, 0.0025), //171
      KFEfficiencyParticleInfo("H4L_{ddpi}b",      "H4L_{ddpi} b  ",      -3014, 3.0f, 5.0f, 3.9224     , 2.632e-10, -1, 0.0025), //172
      KFEfficiencyParticleInfo("H5L_{tdpi}",       "H5L_{tdpi}    ",       3015, 4.0f, 6.0f, 4.845      , 2.632e-10,  1, 0.0030), //173
      KFEfficiencyParticleInfo("H5L_{tdpi}b",      "H5L_{tdpi} b  ",      -3015, 4.0f, 6.0f, 4.845      , 2.632e-10, -1, 0.0030), //174
      KFEfficiencyParticleInfo("H6L_{He6pi}",      "H6L_{He6pi}   ",       3016, 5.0f, 7.0f, 5.775      , 2.632e-10,  1, 0.0030), //175
      KFEfficiencyParticleInfo("H6L_{He6pi}b",     "H6L_{He6pi} b ",      -3016, 5.0f, 7.0f, 5.775      , 2.632e-10, -1, 0.0030), //176
      KFEfficiencyParticleInfo("H6L_{ttpi}",       "H6L_{ttpi}    ",       3017, 5.0f, 7.0f, 5.775      , 2.632e-10,  1, 0.0030), //177
      KFEfficiencyParticleInfo("H6L_{ttpi}b",      "H6L_{ttpi} b  ",      -3017, 5.0f, 7.0f, 5.775      , 2.632e-10, -1, 0.0030), //178
      KFEfficiencyParticleInfo("He4L",             "He4L          ",       3006, 3.0f, 5.0f, 3.9210     , 2.632e-10,  2, 0.0025), //179
      KFEfficiencyParticleInfo("He4Lb",            "He4L b        ",      -3006, 3.0f, 5.0f, 3.9210     , 2.632e-10, -2, 0.0025), //180
      KFEfficiencyParticleInfo("He5L",             "He5L          ",       3007, 4.0f, 6.0f, 4.8393     , 2.632e-10,  2, 0.0025), //181
      KFEfficiencyParticleInfo("He5Lb",            "He5L b        ",      -3007, 4.0f, 6.0f, 4.8393     , 2.632e-10, -2, 0.0025), //182
      KFEfficiencyParticleInfo("He5L_{He3dpi}",    "He5L_{He3dpi} ",       3018, 4.0f, 6.0f, 4.8393     , 2.632e-10,  2, 0.0025), //183
      KFEfficiencyParticleInfo("He5L_{He3dpi}b",   "He5L_{He3dpi}b",      -3018, 4.0f, 6.0f, 4.8393     , 2.632e-10, -2, 0.0025), //184
      KFEfficiencyParticleInfo("He6L_{Li6pi}",     "He6L_{Li6pi} ",        3019, 5.0f, 7.0f, 5.775      , 2.632e-10,  2, 0.0030), //185
      KFEfficiencyParticleInfo("He6L_{Li6pi}b",    "He6L_{Li6pi}b",       -3019, 5.0f, 7.0f, 5.775      , 2.632e-10, -2, 0.0030), //186
      KFEfficiencyParticleInfo("He6L_{He3tpi}",    "He6L_{He3tpi} ",       3020, 5.0f, 7.0f, 5.775      , 2.632e-10,  2, 0.0030), //187
      KFEfficiencyParticleInfo("He6L_{He3tpi}b",   "He6L_{He3tpi}b",      -3020, 5.0f, 7.0f, 5.775      , 2.632e-10, -2, 0.0030), //188
      KFEfficiencyParticleInfo("He6L_{He4dpi}",    "He6L_{He4dpi} ",       3021, 5.0f, 7.0f, 5.775      , 2.632e-10,  2, 0.0030), //189
      KFEfficiencyParticleInfo("He6L_{He4dpi}b",   "He6L_{He4dpi}b",      -3021, 5.0f, 7.0f, 5.775      , 2.632e-10, -2, 0.0030), //190
      KFEfficiencyParticleInfo("He7L_{Li7pi}",     "He7L_{Li7pi} ",        3022, 6.0f, 8.0f, 6.717      , 2.632e-10,  2, 0.0030), //191
      KFEfficiencyParticleInfo("He7L_{Li7pi}b",    "He7L_{Li7pi}b",       -3022, 6.0f, 8.0f, 6.717      , 2.632e-10, -2, 0.0030), //192
      KFEfficiencyParticleInfo("He7L_{He4tpi}",    "He7L_{He4tpi} ",       3023, 6.0f, 8.0f, 6.717      , 2.632e-10,  2, 0.0030), //193
      KFEfficiencyParticleInfo("He7L_{He4tpi}b",   "He7L_{He4tpi}b",      -3023, 6.0f, 8.0f, 6.717      , 2.632e-10, -2, 0.0030), //194
      KFEfficiencyParticleInfo("Li6L_{2He3pi}",    "Li6L_{2He3pi} ",       3024, 5.0f, 7.0f, 5.77       , 2.632e-10,  3, 0.0030), //195
      KFEfficiencyParticleInfo("Li6L_{2He3pi}b",   "Li6L_{2He3pi}b",      -3024, 5.0f, 7.0f, 5.77       , 2.632e-10, -3, 0.0030), //196
      KFEfficiencyParticleInfo("Li7L_{Be7pi}",     "Li7L_{Be7pi}  ",       3025, 6.0f, 8.0f, 6.711      , 2.632e-10,  3, 0.0030), //197
      KFEfficiencyParticleInfo("Li7L_{Be7pi}b",    "Li7L_{Be7pi} b",      -3025, 6.0f, 8.0f, 6.711      , 2.632e-10, -3, 0.0030), //198
      KFEfficiencyParticleInfo("Li7L_{He3He4pi}",  "Li7L          ",       3026, 6.0f, 8.0f, 6.711      , 2.632e-10,  3, 0.0025), //199
      KFEfficiencyParticleInfo("Li7L_{He3He4pi}b", "Li7L b        ",      -3026, 6.0f, 8.0f, 6.711      , 2.632e-10, -3, 0.0025), //200
      KFEfficiencyParticleInfo("Li8L_{2He4pi}",    "Li8L_{2He4pi} ",       3027, 7.0f, 9.0f, 7.642      , 2.632e-10,  3, 0.0025), //201
      KFEfficiencyParticleInfo("Li8L_{2He4pi}b",   "Li8L_{2He4pi}b",      -3027, 7.0f, 9.0f, 7.642      , 2.632e-10, -3, 0.0025), //202
      KFEfficiencyParticleInfo("LLn",              "LLn           ",       3203, 3.0f, 5.0f, 3.16964    , 2.632e-10,  0, 0.0030), //203
      KFEfficiencyParticleInfo("LLnn",             "LLnn          ",       3040, 4.0f, 6.0f, 4.16964    , 2.632e-10,  0, 0.0030), //204
      KFEfficiencyParticleInfo("H4LL_{He4Lpi-}",   "H4LL_{He4Lpi-}",       3008, 3.0f, 5.0f, 4.10791    , 2.632e-10,  1, 0.0030), //205
      KFEfficiencyParticleInfo("H4LL_{H3Lppi-}",   "H4LL_{H3Lppi-}",       3009, 3.0f, 5.0f, 4.10791    , 2.632e-10,  1, 0.0030), //206
      KFEfficiencyParticleInfo("H5LL_{He5Lpi-}",   "H5LL_{He5Lpi-}",       3010, 4.0f, 6.0f, 5.04748    , 2.632e-10,  1, 0.0030), //207
      KFEfficiencyParticleInfo("He6LL",            "He6LL         ",       3011, 5.0f, 7.0f, 5.98575    , 2.632e-10,  2, 0.0030), //208
      KFEfficiencyParticleInfo("H2L",              "H2L           ",       3028, 1.5f, 3.5f, 2.05       , 2.632e-10,  1, 0.0020), //209
      KFEfficiencyParticleInfo("H2Lb",             "H2L b         ",      -3028, 1.5f, 3.5f, 2.05       , 2.632e-10, -1, 0.0020), //210
      KFEfficiencyParticleInfo("He3L",             "He3L          ",       3029, 2.0f, 4.0f, 2.992      , 2.632e-10,  2, 0.0020), //211
      KFEfficiencyParticleInfo("He3Lb",            "He3L b        ",      -3029, 2.0f, 4.0f, 2.992      , 2.632e-10, -2, 0.0020), //212
      KFEfficiencyParticleInfo("H2L*",             "H2L*          ",    1003003, 1.5f, 2.5f, 2.053      ,  1.55e-22,  1, 0.0030), //213
      KFEfficiencyParticleInfo("H2L*b",            "H2L* b        ",   -1003003, 1.5f, 2.5f, 2.053      ,  1.55e-22,  1, 0.0030), //214
      KFEfficiencyParticleInfo("H3L*",             "H3L*          ",    1003004, 2.5f, 3.5f, 2.992      ,  1.55e-22,  1, 0.0030), //215
      KFEfficiencyParticleInfo("H3L*b",            "H3L* b        ",   -1003004, 2.5f, 3.5f, 2.992      ,  1.55e-22,  1, 0.0030), //216
      KFEfficiencyParticleInfo("H4L*",             "H4L*          ",    1003005, 3.5f, 4.5f, 3.923      ,  1.55e-22,  1, 0.0030), //217
      KFEfficiencyParticleInfo("H4L*b",            "H4L* b        ",   -1003005, 3.5f, 4.5f, 3.923      ,  1.55e-22,  1, 0.0030), //218
      KFEfficiencyParticleInfo("He4L*_{HeL}",      "He4L*_{HeL}   ",    1003006, 3.5f, 4.5f, 3.923      ,  1.55e-22,  2, 0.0030), //219
      KFEfficiencyParticleInfo("He4L*_{HeL}b",     "He4L*_{HeL} b ",   -1003006, 3.5f, 4.5f, 3.923      ,  1.55e-22,  2, 0.0030), //220
      KFEfficiencyParticleInfo("He4L*_{H3Lp}",     "He4L*_{H3Lp}  ",    2003006, 3.5f, 4.5f, 3.923      ,  1.55e-22,  2, 0.0030), //221
      KFEfficiencyParticleInfo("He4L*_{H3Lp}b",    "He4L*_{H3Lp} b",   -2003006, 3.5f, 4.5f, 3.923      ,  1.55e-22,  2, 0.0030), //222
      KFEfficiencyParticleInfo("He5L*_{HeL}",      "He5L*_{HeL}   ",    1003007, 4.5f, 5.5f, 4.842      ,  1.55e-22,  2, 0.0030), //223
      KFEfficiencyParticleInfo("He5L*_{HeL}b",     "He5L*_{HeL} b ",   -1003007, 4.5f, 5.5f, 4.842      ,  1.55e-22,  2, 0.0030), //224
      KFEfficiencyParticleInfo("He5L*_{H4Lp}",     "He5L*_{H4Lp}  ",    2003007, 4.5f, 5.5f, 4.842      ,  1.55e-22,  2, 0.0030), //225
      KFEfficiencyParticleInfo("He5L*_{H4Lp}b",    "He5L*_{H4Lp} b",   -2003007, 4.5f, 5.5f, 4.842      ,  1.55e-22,  2, 0.0030), //226
      KFEfficiencyParticleInfo("H2Sp",             "H2Sp          ",       3030, 1.0f, 3.0f, 2.135      , 8.018e-11,  0, 0.0025), //227
      KFEfficiencyParticleInfo("H3Sp",             "H3Sp          ",       3031, 2.0f, 4.0f, 3.075      , 8.018e-11,  0, 0.0025), //228
      KFEfficiencyParticleInfo("He3Sp",            "He3Sp         ",       3032, 2.0f, 4.0f, 3.073      , 8.018e-11,  0, 0.0025), //229
      KFEfficiencyParticleInfo("He4Sp",            "He4Sp         ",       3033, 3.0f, 5.0f, 4.013      , 8.018e-11,  0, 0.0025), //230
      KFEfficiencyParticleInfo("He6Sp",            "He6Sp         ",       3034, 5.0f, 7.0f, 5.892      , 8.018e-11,  0, 0.0025), //231
      KFEfficiencyParticleInfo("Li6Sp",            "Li6Sp         ",       3035, 5.0f, 7.0f, 5.891      , 8.018e-11,  0, 0.0025), //232
      KFEfficiencyParticleInfo("Li7Sp",            "Li7Sp         ",       3036, 6.0f, 8.0f, 6.830      , 8.018e-11,  0, 0.0025), //233
      KFEfficiencyParticleInfo("Be7Sp",            "Be7Sp         ",       3037, 6.0f, 8.0f, 6.829      , 8.018e-11,  0, 0.0025), //234
      KFEfficiencyParticleInfo("Li5L",             "Li5L          ",       3038, 4.0f, 6.0f, 4.840      , 2.632e-10,  0, 0.0025), //235
      KFEfficiencyParticleInfo("Li6L",             "Li6L          ",       3039, 5.0f, 7.0f, 5.775      , 2.632e-10,  0, 0.0025), //236
//missing mass method      
      KFEfficiencyParticleInfo("pi-_{mu,nu}",      "pi-_{mnu}     ",   -7000211,  0.f, 1.0f, 0.139570   , 2.6e-8,    -1, 0.0030), //237
      KFEfficiencyParticleInfo("nu_{pi-}",         "nu_{mupi-} b  ",   -7000014,-1.0f, 1.0f, 0.         , 1.0e20,     0, 0.0030), //238
      KFEfficiencyParticleInfo("pi+_{mu,nu}",      "pi+_{mnu}     ",    7000211,  0.f, 1.0f, 0.139570   , 2.6e-8,     1, 0.0030), //239
      KFEfficiencyParticleInfo("nu_{pi+}",         "nu_{mupi+}    ",    7000014,-1.0f, 1.0f, 0.         , 1.0e20,     0, 0.0030), //240
      KFEfficiencyParticleInfo("K-_{mu,nu}",       "K-_{mnu}      ",   -7000321,  0.f, 1.0f, 0.493677   , 1.238e-8,  -1, 0.0030), //241
      KFEfficiencyParticleInfo("nu_{K-}",          "nu_{K-} b     ",   -8000014,-1.0f, 1.0f, 0.         , 1.0e20,     0, 0.0030), //242
      KFEfficiencyParticleInfo("K+_{mu,nu}",       "K+_{mnu}      ",    7000321,  0.f, 1.0f, 0.493677   , 1.238e-8,   1, 0.0030), //243
      KFEfficiencyParticleInfo("nu_{K+}",          "nu_{K+}       ",    8000014,-1.0f, 1.0f, 0.         , 1.0e20,     0, 0.0030), //244
      KFEfficiencyParticleInfo("Sigma-_{pi-,n}",   "Sigma-_{pi-n} ",    7003112, 0.0f, 2.0f, 1.197449   , 1.479e-10, -1, 0.0100), //245
      KFEfficiencyParticleInfo("n_{Sigma-}",       "n_{Sigma-}    ",    7002112, 0.0f, 1.5f, 0.9395654  , 880,        0, 0.0030), //246
      KFEfficiencyParticleInfo("Sigma+_{pi+n}b",   "Sigma+{pi+n} b",   -7003112, 0.0f, 2.0f, 1.197449   , 1.479e-10, -1, 0.0030), //247
      KFEfficiencyParticleInfo("n_{Sigma+} b",     "n_{Sigma+b} b ",   -7002112, 0.0f, 1.5f, 0.9395654  , 880,        0, 0.0030), //248
      KFEfficiencyParticleInfo("Sigma-_{pi-n}b",   "Sigma+{pi-n} b",   -7003222, 0.0f, 2.0f, 1.18937    , 0.8018e-10, 1, 0.0030), //249
      KFEfficiencyParticleInfo("n_{Sigma-} b",     "n_{Sigma-_b} b",   -8002112, 0.0f, 1.5f, 0.9395654  , 0.93956541, 0, 0.0030), //250
      KFEfficiencyParticleInfo("Sigma+_{pi+n}",    "Sigma+_{pi+n} ",    7003222, 0.0f, 2.0f, 1.18937    , 0.8018e-10, 1, 0.0100), //251
      KFEfficiencyParticleInfo("n_{Sigma+}",       "n_{Sigma+}    ",    8002112, 0.0f, 1.5f, 0.9395654  , 880,        0, 0.0030), //252
      KFEfficiencyParticleInfo("Xi-_{pi-L}",       "Xi-_{pi-L}    ",    7003312, 0.0f, 2.0f, 1.32171    , 1.639e-10, -1, 0.0030), //253
      KFEfficiencyParticleInfo("Lambda_{Xi-}",     "Lambda_{Xi-}  ",    7003122, 0.0f, 2.0f, 1.115683   , 2.632e-10,  0, 0.0030), //254
      KFEfficiencyParticleInfo("Xi+_{pi+L_b}",     "Xi+_{pi+L_b}  ",   -7003312, 0.0f, 2.0f, 1.32171    , 1.639e-10,  1, 0.0030), //255
      KFEfficiencyParticleInfo("Lambda_{Xi+} b",   "Lambda_{Xi+} b",   -7003122, 0.0f, 2.0f, 1.115683   , 2.632e-10,  0, 0.0030), //256
      KFEfficiencyParticleInfo("Omega-_{Xi-pi0}",  "Omega-{pi-Xi0}",    7003334, 0.0f, 2.0f, 1.67245    , 0.821e-10, -1, 0.0030), //257
      KFEfficiencyParticleInfo("Xi0_{Omega-}",     "Xi0_{Omega-}  ",    7003322, 0.0f, 2.0f, 1.31486    , 2.9e-10,    0, 0.0030), //258
      KFEfficiencyParticleInfo("Omega+_{Xi+pi0}",  "Omega+_{Xipi0}",   -7003334, 0.0f, 2.0f, 1.67245    , 0.821e-10,  1, 0.0030), //259
      KFEfficiencyParticleInfo("Xi0_{Omega+} b",   "Xi0_{Omega+} b",   -7003322, 0.0f, 2.0f, 1.31486    , 2.9e-10,    0, 0.0030), //260
      KFEfficiencyParticleInfo("K-_{pi-pi0}",      "K-_{pi-pi0}   ",   -9000321, 0.0f, 1.0f, 0.493677   , 1.24e-8,   -1, 0.0030), //261
      KFEfficiencyParticleInfo("pi0_{K-}",         "pi0_{K-}      ",   -9000111, 0.0f, 1.0f, 0.1349766  , 8.52e-17,   0, 0.0030), //262
      KFEfficiencyParticleInfo("K+_{pi+pi0}",      "K+_{pi+pi0}   ",    9000321, 0.0f, 1.0f, 0.493677   , 1.24e-8,    1, 0.0030), //263
      KFEfficiencyParticleInfo("pi0_{K+}",         "pi0_{K+}      ",    9000111, 0.0f, 1.0f, 0.1349766  , 8.52e-17,   0, 0.0030), //264
      KFEfficiencyParticleInfo("Omega-{K-L}",      "Omega-_{K-L}  ",    8003334, 0.0f, 2.0f, 1.67245    , 0.821e-10, -1, 0.0030), //265
      KFEfficiencyParticleInfo("Lambda_{Omega-}",  "Lambda_{O-}   ",    8003122, 0.0f, 2.0f, 1.115683   , 2.632e-10,  0, 0.0030), //266
      KFEfficiencyParticleInfo("Omega+_{K+L_b}",   "Omega+_{K+Lb} ",   -8003334, 0.0f, 2.0f, 1.67245    , 0.821e-10,  1, 0.0030), //267
      KFEfficiencyParticleInfo("Lamda_{Omega+} b", "Lambda_{O+} b ",   -8003122, 0.0f, 2.0f, 1.115683   , 2.632e-10,  0, 0.0030), //268
      KFEfficiencyParticleInfo("Sigma-{p_b pi0} b","Sigma-{ppi0} b",   -8003222, 0.0f, 2.0f, 1.18937    , 0.8018e-10, 1, 0.0030), //269
      KFEfficiencyParticleInfo("pi0_{Sigma-b}",    "pi0_{Sigma-_b}",   -8000111, 0.0f, 1.0f, 0.1349766  , 8.52e-17,   0, 0.0030), //270
      KFEfficiencyParticleInfo("Sigma+_{p pi0}",   "Sigma+_{ppi0} ",    8003222, 0.0f, 2.0f, 1.18937    , 0.8018e-10, 1, 0.0250), //271
      KFEfficiencyParticleInfo("pi0_{Sigma+}",     "pi0_{Sigma+}  ",    8000111, 0.0f, 1.0f, 0.1349766  , 8.52e-17,   0, 0.0030), //272
      KFEfficiencyParticleInfo("He3L_{He3pi0}",    "He3L_{He3pi0} ",    7003029, 2.9f, 3.9f, 2.991      , 2.632e-10,  2, 0.0020), //273
      KFEfficiencyParticleInfo("pi0_{He3L+}",      "pi0_{He3L+}   ",    7700111, 0.0f, 1.0f, 0.1349766  , 8.52e-17,   0, 0.0030), //274
      KFEfficiencyParticleInfo("He3L_{He3pi0} b",  "He3L_{He3pi0}b",   -7003029, 2.9f, 3.9f, 2.991      , 2.632e-10,  2, 0.0020), //275
      KFEfficiencyParticleInfo("pi0_{He3L-}",      "pi0_{He3L-}   ",   -7700111, 0.0f, 1.0f, 0.1349766  , 8.52e-17,   0, 0.0030), //276
      KFEfficiencyParticleInfo("He4L_{He4pi0}",    "He4L_{He4pi0} ",    7003006, 3.8f, 4.8f, 3.921      , 2.632e-10,  2, 0.0020), //277
      KFEfficiencyParticleInfo("pi0_{He4L+}",      "pi0_{He4L+}   ",    7800111, 0.0f, 1.0f, 0.1349766  , 8.52e-17,   0, 0.0030), //278
      KFEfficiencyParticleInfo("He4L_{He4pi0} b",  "He4L_{He4pi0}b",   -7003006, 3.8f, 4.8f, 3.921      , 2.632e-10,  2, 0.0020), //279
      KFEfficiencyParticleInfo("pi0_{He4L-}",      "pi0_{He4L-}   ",   -7800111, 0.0f, 1.0f, 0.1349766  , 8.52e-17,   0, 0.0030), //280
//tracks                                                                                                                             
      KFEfficiencyParticleInfo("e-",               "e-            ",         11, 0.0f,0.01f, 5.109989461E-04, 1.0e20,-1, 0.0030), //281
      KFEfficiencyParticleInfo("e+",               "e+            ",        -11, 0.0f,0.01f, 5.109989461E-04, 1.0e20, 1, 0.0030), //282
      KFEfficiencyParticleInfo("mu-",              "mu-           ",         13, 0.0f, 1.0f, 0.1056583745, 2.2e-6,   -1, 0.0030), //283
      KFEfficiencyParticleInfo("mu+",              "mu+           ",        -13, 0.0f, 1.0f, 0.1056583745, 2.2e-6,    1, 0.0030), //284
      KFEfficiencyParticleInfo("pi+",              "pi+           ",        211, 0.0f, 1.0f, 0.13957039 , 2.6e-8,     1, 0.0030), //285
      KFEfficiencyParticleInfo("pi-",              "pi-           ",       -211, 0.0f, 1.0f, 0.13957039 , 2.6e-8,    -1, 0.0030), //286
      KFEfficiencyParticleInfo("K+",               "K+            ",        321, 0.0f, 1.0f, 0.493677   , 1.238e-8,   1, 0.0030), //287
      KFEfficiencyParticleInfo("K-",               "K-            ",       -321, 0.0f, 1.0f, 0.493677   , 1.238e-8,  -1, 0.0030), //288
      KFEfficiencyParticleInfo("p+",               "p+            ",       2212, 0.0f, 1.5f, 0.9382720813, 1.0e20,    1, 0.0030), //289
      KFEfficiencyParticleInfo("p-",               "p-            ",      -2212, 0.0f, 1.5f, 0.9382720813, 1.0e20,   -1, 0.0030), //290
      KFEfficiencyParticleInfo("d+",               "d+            ", 1000010020, 0.0f, 2.5f, 1.87561294257, 1.0e20,   1, 0.0030), //291
      KFEfficiencyParticleInfo("d-",               "d-            ",-1000010020, 0.0f, 2.5f, 1.87561294257, 1.0e20,  -1, 0.0030), //292
      KFEfficiencyParticleInfo("t+",               "t+            ", 1000010030, 0.0f, 3.5f, 2.80892113298, 1.0e20,   1, 0.0030), //293
      KFEfficiencyParticleInfo("t-",               "t-            ",-1000010030, 0.0f, 3.5f, 2.80892113298, 1.0e20,  -1, 0.0030), //294
      KFEfficiencyParticleInfo("He3+",             "He3+          ", 1000020030, 0.0f, 3.5f, 2.80839160743, 1.0e20,   2, 0.0030), //295
      KFEfficiencyParticleInfo("He3-",             "He3-          ",-1000020030, 0.0f, 3.5f, 2.80839160743, 1.0e20,  -2, 0.0030), //296
      KFEfficiencyParticleInfo("He4+",             "He4+          ", 1000020040, 0.0f, 4.5f, 3.7273794066, 1.0e20,    2, 0.0030), //297
      KFEfficiencyParticleInfo("He4-",             "He4-          ",-1000020040, 0.0f, 4.5f, 3.7273794066, 1.0e20,   -2, 0.0030), //298
      KFEfficiencyParticleInfo("He6+",             "He6+          ", 1000020060, 0.0f, 6.5f, 5.6055375  , 1.0e20,     2, 0.0030), //299
      KFEfficiencyParticleInfo("He6-",             "He6-          ",-1000020060, 0.0f, 6.5f, 5.6055375  , 1.0e20,    -2, 0.0030), //300
      KFEfficiencyParticleInfo("Li6+",             "Li6+          ", 1000030060, 0.0f, 6.5f, 5.6015181  , 1.0e20,     3, 0.0030), //301
      KFEfficiencyParticleInfo("Li6-",             "Li6-          ",-1000030060, 0.0f, 6.5f, 5.6015181  , 1.0e20,    -3, 0.0030), //302
      KFEfficiencyParticleInfo("Li7+",             "Li7+          ", 1000030070, 0.0f, 7.5f, 6.5338336  , 1.0e20,     3, 0.0030), //303
      KFEfficiencyParticleInfo("Li7-",             "Li7-          ",-1000030070, 0.0f, 7.5f, 6.5338336  , 1.0e20,    -3, 0.0030), //304
      KFEfficiencyParticleInfo("Be7+",             "Be7+          ", 1000040070, 0.0f, 7.5f, 6.5341844  , 1.0e20,     4, 0.0030), //305
      KFEfficiencyParticleInfo("Be7-",             "Be7-          ",-1000040070, 0.0f, 7.5f, 6.5341844  , 1.0e20,    -4, 0.0030), //306
//background for subtraction
      KFEfficiencyParticleInfo("pi+pi+",           "pi+pi+        ",       9001, 0.0f, 2.0f, 0          , 1.0e20,     0, 0.0030), //307
      KFEfficiencyParticleInfo("pi+K+",            "pi+K+         ",       9002, 0.6f, 5.6f, 0          , 1.0e20,     0, 0.0030), //308
      KFEfficiencyParticleInfo("K+K+",             "K+K+          ",       9003, 0.8f, 3.8f, 0          , 1.0e20,     0, 0.0030), //309
      KFEfficiencyParticleInfo("K+p+",             "K+p+          ",       9004, 1.4f, 5.4f, 0          , 1.0e20,     0, 0.0030), //310
      KFEfficiencyParticleInfo("pi-pi-",           "pi-pi-        ",      -9001, 0.0f, 2.0f, 0          , 1.0e20,     0, 0.0030), //311
      KFEfficiencyParticleInfo("pi-K-",            "pi-K-         ",      -9002, 0.6f, 5.6f, 0          , 1.0e20,     0, 0.0030), //312
      KFEfficiencyParticleInfo("K-K-",             "K-K-          ",      -9003, 0.8f, 3.8f, 0          , 1.0e20,     0, 0.0030), //313
      KFEfficiencyParticleInfo("K-p-",             "K-p-          ",      -9004, 1.4f, 5.4f, 0          , 1.0e20,     0, 0.0030), //314
//V0
      KFEfficiencyParticleInfo("V0",               "V0            ",  123456789, 0.3f, 1.3f, 0          , 0,          0, 0.0030)  //315
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
    
    partDaughterPdg[curPart].push_back(  211); //K+ -> pi+ pi- pi+
    partDaughterPdg[curPart].push_back( -211);
    partDaughterPdg[curPart].push_back(  211);
    curPart++;

    partDaughterPdg[curPart].push_back(  211); //K+ -> pi+ pi- pi-
    partDaughterPdg[curPart].push_back( -211);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back(  211); //K+ -> pi+ pi- pi+ and mother K+
    partDaughterPdg[curPart].push_back( -211);
    partDaughterPdg[curPart].push_back(  211);
    partDaughterPdg[curPart].push_back(  321);
    curPart++;

    partDaughterPdg[curPart].push_back(  211); //K+ -> pi+ pi- pi- and mother K-
    partDaughterPdg[curPart].push_back( -211);
    partDaughterPdg[curPart].push_back( -211);
    partDaughterPdg[curPart].push_back( -321);
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

    partDaughterPdg[curPart].push_back( 1000010020); //d pi-
    partDaughterPdg[curPart].push_back(       -211);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000010020); //d pi+
    partDaughterPdg[curPart].push_back(        211);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000010030); //t pi-
    partDaughterPdg[curPart].push_back(       -211);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000010030); //t pi+
    partDaughterPdg[curPart].push_back(        211);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000020030); //He3 pi-
    partDaughterPdg[curPart].push_back(       -211);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000020030); //He3 pi+
    partDaughterPdg[curPart].push_back(        211);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000020040); //He4 pi-
    partDaughterPdg[curPart].push_back(       -211);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000020040); //He4 pi+
    partDaughterPdg[curPart].push_back(        211);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000020060); //He6 pi-
    partDaughterPdg[curPart].push_back(       -211);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000020060); //He6 pi+
    partDaughterPdg[curPart].push_back(        211);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000030060); //Li6 pi-
    partDaughterPdg[curPart].push_back(       -211);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000030060); //Li6 pi+
    partDaughterPdg[curPart].push_back(        211);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000030070); //Li7 pi-
    partDaughterPdg[curPart].push_back(       -211);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000030070); //Li7 pi+
    partDaughterPdg[curPart].push_back(        211);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000040070); //Be7 pi-
    partDaughterPdg[curPart].push_back(       -211);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000040070); //Be7 pi+
    partDaughterPdg[curPart].push_back(        211);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000010020); //d K-
    partDaughterPdg[curPart].push_back(       -321);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000010020); //d K+
    partDaughterPdg[curPart].push_back(        321);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000010030); //t K-
    partDaughterPdg[curPart].push_back(       -321);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000010030); //t K+
    partDaughterPdg[curPart].push_back(        321);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000020030); //He3 K-
    partDaughterPdg[curPart].push_back(       -321);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000020030); //He3 K+
    partDaughterPdg[curPart].push_back(        321);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000020040); //He4 K-
    partDaughterPdg[curPart].push_back(       -321);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000020040); //He4 K+
    partDaughterPdg[curPart].push_back(        321);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000020060); //He6 K-
    partDaughterPdg[curPart].push_back(       -321);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000020060); //He6 K+
    partDaughterPdg[curPart].push_back(        321);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000030060); //Li6 K-
    partDaughterPdg[curPart].push_back(       -321);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000030060); //Li6 K+
    partDaughterPdg[curPart].push_back(        321);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000030070); //Li7 K-
    partDaughterPdg[curPart].push_back(       -321);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000030070); //Li7 K+
    partDaughterPdg[curPart].push_back(        321);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000040070); //Be7 K-
    partDaughterPdg[curPart].push_back(       -321);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000040070); //Be7 K+
    partDaughterPdg[curPart].push_back(        321);
    curPart++;

    partDaughterPdg[curPart].push_back(       2212); //p p
    partDaughterPdg[curPart].push_back(       2212);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000010020); //d p
    partDaughterPdg[curPart].push_back(       2212);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000010030); //t p
    partDaughterPdg[curPart].push_back(       2212);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000020030); //He3 p
    partDaughterPdg[curPart].push_back(       2212);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000020040); //He4 p
    partDaughterPdg[curPart].push_back(       2212);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000020060); //He6 p
    partDaughterPdg[curPart].push_back(       2212);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000030060); //Li6 p
    partDaughterPdg[curPart].push_back(       2212);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000030070); //Li7 p
    partDaughterPdg[curPart].push_back(       2212);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000040070); //Be7 p
    partDaughterPdg[curPart].push_back(       2212);
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

    partDaughterPdg[curPart].push_back(  211); //D0 -> pi+ pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back(  211); //D0 -> 2pi+ 2pi-
    partDaughterPdg[curPart].push_back(  211);
    partDaughterPdg[curPart].push_back( -211);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  310); //D0_bar -> K0 pi+ pi-
    partDaughterPdg[curPart].push_back(  211);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  321); //D0_bar -> K+ K-
    partDaughterPdg[curPart].push_back( -321);
    curPart++;

    partDaughterPdg[curPart].push_back(  321); //D0_bar -> K+ K- K0
    partDaughterPdg[curPart].push_back( -321);
    partDaughterPdg[curPart].push_back(  310);
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
    
    partDaughterPdg[curPart].push_back(  310); //D+ -> K0 pi+
    partDaughterPdg[curPart].push_back(  211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  310); //D- -> K0 pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  310); //D+ -> K0 pi+ pi+ pi-
    partDaughterPdg[curPart].push_back(  211);
    partDaughterPdg[curPart].push_back(  211);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  310); //D- -> K0 pi+ pi- pi-
    partDaughterPdg[curPart].push_back(  211);
    partDaughterPdg[curPart].push_back( -211);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  211); //D+ -> pi+ pi+ pi-
    partDaughterPdg[curPart].push_back(  211);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  211); //D- -> pi+ pi- pi-
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
    
    partDaughterPdg[curPart].push_back(  310); //Ds+ -> K0 K+
    partDaughterPdg[curPart].push_back(  321);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  310); //Ds- -> K0 K-
    partDaughterPdg[curPart].push_back( -321);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  310); //Ds+ -> K0 K0 pi+
    partDaughterPdg[curPart].push_back(  310);
    partDaughterPdg[curPart].push_back(  211);
    curPart++;

    partDaughterPdg[curPart].push_back(  310); //Ds- -> K0 K0 pi-
    partDaughterPdg[curPart].push_back(  310);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  310); //Ds+ -> K0 K+ pi+ pi-
    partDaughterPdg[curPart].push_back(  321);
    partDaughterPdg[curPart].push_back(  211);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back(  310); //Ds- -> K0 K- pi+ pi-
    partDaughterPdg[curPart].push_back( -321);
    partDaughterPdg[curPart].push_back(  211);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  321); //Ds+ -> K+ pi+ pi-
    partDaughterPdg[curPart].push_back(  211);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back( -321); //Ds- -> K- pi+ pi-
    partDaughterPdg[curPart].push_back(  211);
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
    
    partDaughterPdg[curPart].push_back( 2212); //Lambdac -> p K0s
    partDaughterPdg[curPart].push_back(  310);
    curPart++;

    partDaughterPdg[curPart].push_back(-2212); //Lambdac_bar -> p_bar K0s
    partDaughterPdg[curPart].push_back(  310);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 2212); //Lambdac -> p K0s pi+ pi-
    partDaughterPdg[curPart].push_back(  310);
    partDaughterPdg[curPart].push_back(  211);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back(-2212); //Lambdac_bar -> p_bar K0s pi+ pi-
    partDaughterPdg[curPart].push_back(  310);
    partDaughterPdg[curPart].push_back(  211);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 3122); //Lambdac -> Lambda pi+
    partDaughterPdg[curPart].push_back(  211);
    curPart++;

    partDaughterPdg[curPart].push_back(-3122); //Lambdac_bar -> Lambda_bar pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 3122); //Lambdac -> Lambda 2pi+ pi-
    partDaughterPdg[curPart].push_back(  211);
    partDaughterPdg[curPart].push_back(  211);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back(-3122); //Lambdac_bar -> Lambda_bar 2pi- pi+
    partDaughterPdg[curPart].push_back( -211);
    partDaughterPdg[curPart].push_back(  211);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;  
    
    partDaughterPdg[curPart].push_back( 3312); //Xic0 -> Xi- 2pi+ pi-
    partDaughterPdg[curPart].push_back(  211);
    partDaughterPdg[curPart].push_back(  211);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(-3312); //Xic0_bar -> Xi+ 2pi- pi+
    partDaughterPdg[curPart].push_back(  211);
    partDaughterPdg[curPart].push_back( -211);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 2212); //Lambdac -> p pi+ pi-
    partDaughterPdg[curPart].push_back(  211);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back(-2212); //Lambdac_bar -> p_bar pi+ pi-
    partDaughterPdg[curPart].push_back(  211);
    partDaughterPdg[curPart].push_back( -211);
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
    
    partDaughterPdg[curPart].push_back(  429); //D*+ -> D04 pi+
    partDaughterPdg[curPart].push_back(  211);
    curPart++;
    
    partDaughterPdg[curPart].push_back( -429); //D*- -> D04_bar pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back(  428); //D*0 -> D04 pi0
    partDaughterPdg[curPart].push_back(  111);
    curPart++;
    
    partDaughterPdg[curPart].push_back(   11); //B -> e+ e-
    partDaughterPdg[curPart].push_back(  -11);
    curPart++;
    
    partDaughterPdg[curPart].push_back(   13); //B -> mu+ mu-
    partDaughterPdg[curPart].push_back(  -13);
    curPart++;
    
    partDaughterPdg[curPart].push_back( -421); //B+ -> D0_bar pi+
    partDaughterPdg[curPart].push_back(  211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  421); //B- -> D0 pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back( -421); //B+ -> D0_bar K+
    partDaughterPdg[curPart].push_back(  321);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  421); //B- -> D0 K-
    partDaughterPdg[curPart].push_back( -321);
    curPart++;

    partDaughterPdg[curPart].push_back( -411); //B0 -> D- pi+
    partDaughterPdg[curPart].push_back(  211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  411); //B0_bar -> D+ pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back( -411); //B0 -> D0_bar K+
    partDaughterPdg[curPart].push_back(  321);
    curPart++;
    
    partDaughterPdg[curPart].push_back(  411); //B0_bar -> D0 K-
    partDaughterPdg[curPart].push_back( -321);
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

    partDaughterPdg[curPart].push_back( 1000010020); //H3Lambda -> d p pi-
    partDaughterPdg[curPart].push_back( 2212);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back(-1000010020); //H3Lambda_bar -> d- p- pi+
    partDaughterPdg[curPart].push_back(-2212);
    partDaughterPdg[curPart].push_back(  211);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 1000020040); //H4Lambda -> He4+ pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back(-1000020040); //H4Lambda_bar -> He4- pi+
    partDaughterPdg[curPart].push_back(  211);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 1000010030); //H4Lambda -> t p pi-
    partDaughterPdg[curPart].push_back( 2212);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(-1000010030); //H4Lambda_bar -> t- p- pi+
    partDaughterPdg[curPart].push_back(-2212);
    partDaughterPdg[curPart].push_back(  211);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 1000010020); //H4Lambda -> d d pi-
    partDaughterPdg[curPart].push_back( 1000010020);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back(-1000010020); //H4Lambda_bar -> d- d- pi+
    partDaughterPdg[curPart].push_back(-1000010020);
    partDaughterPdg[curPart].push_back(  211);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 1000010030); //H5Lambda -> t d pi-
    partDaughterPdg[curPart].push_back( 1000010020);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back(-1000010030); //H5Lambda_bar -> -t -d pi+
    partDaughterPdg[curPart].push_back(-1000010020);
    partDaughterPdg[curPart].push_back(  211);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000020060); //H6Lambda -> He6+ pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back(-1000020060); //H6Lambda_bar -> He6- pi-
    partDaughterPdg[curPart].push_back(  211);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 1000010030); //H6Lambda -> t t pi-
    partDaughterPdg[curPart].push_back( 1000010030);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back(-1000010030); //H6Lambda -> t- t- pi+
    partDaughterPdg[curPart].push_back(-1000010030);
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
    
    partDaughterPdg[curPart].push_back( 1000020030); //He5Lambda -> He3+ d pi-
    partDaughterPdg[curPart].push_back( 1000010020);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back(-1000020030); //He5Lambda_bar -> He3- d- pi+
    partDaughterPdg[curPart].push_back(-1000010020);
    partDaughterPdg[curPart].push_back(  211);
    curPart++;    

    partDaughterPdg[curPart].push_back( 1000030060); //He6Lambda -> Li6+ pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(-1000030060); //He6Lambda_bar -> Li6- pi+
    partDaughterPdg[curPart].push_back(  211);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 1000020030); //He6Lambda -> He3+ t pi-
    partDaughterPdg[curPart].push_back( 1000010030);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(-1000020030); //He6Lambda_bar -> He3- t- pi+
    partDaughterPdg[curPart].push_back(-1000010030);
    partDaughterPdg[curPart].push_back(  211);
    curPart++;    
    
    partDaughterPdg[curPart].push_back( 1000020040); //He6Lambda -> He4+ d pi-
    partDaughterPdg[curPart].push_back( 1000010020);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back(-1000020040); //He6Lambda_bar -> He4- d- pi+
    partDaughterPdg[curPart].push_back(-1000010020);
    partDaughterPdg[curPart].push_back(  211);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000030070); //He7Lambda -> Li7+ pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;  

    partDaughterPdg[curPart].push_back(-1000030070); //He7Lambda_bar -> Li7- pi+
    partDaughterPdg[curPart].push_back(  211);
    curPart++;  

    partDaughterPdg[curPart].push_back( 1000020040); //He7Lambda -> He4+ t pi-
    partDaughterPdg[curPart].push_back( 1000010030);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;  

    partDaughterPdg[curPart].push_back(-1000020040); //He7Lambda_bar -> He4- t- pi+
    partDaughterPdg[curPart].push_back(-1000010030);
    partDaughterPdg[curPart].push_back(  211);
    curPart++;  

    partDaughterPdg[curPart].push_back( 1000020030); //Li6Lambda -> He3+ He3+ pi-
    partDaughterPdg[curPart].push_back( 1000020030);
    partDaughterPdg[curPart].push_back( -211);
    curPart++; 
    
    partDaughterPdg[curPart].push_back(-1000020030); //Li6Lambda_bar -> He3- He3- pi+
    partDaughterPdg[curPart].push_back(-1000020030);
    partDaughterPdg[curPart].push_back(  211);
    curPart++;     
    
    partDaughterPdg[curPart].push_back( 1000040070); //Li7Lambda -> Be7+ pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back(-1000040070); //Li7Lambda_bar -> Be7- pi+
    partDaughterPdg[curPart].push_back(  211);
    curPart++;
    
    partDaughterPdg[curPart].push_back( 1000020030); //Li7Lambda -> He3+ He4+ pi-
    partDaughterPdg[curPart].push_back( 1000020040);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;
    
    partDaughterPdg[curPart].push_back(-1000020030); //Li7Lambda_bar -> He3- He4- pi+
    partDaughterPdg[curPart].push_back(-1000020040);
    partDaughterPdg[curPart].push_back(  211);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000020040); //Li8Lambda -> He4+ He4+ pi-
    partDaughterPdg[curPart].push_back( 1000020040);
    partDaughterPdg[curPart].push_back( -211);
    curPart++; 
    
    partDaughterPdg[curPart].push_back(-1000020040); //Li8Lambda_bar -> He4- He4- pi+
    partDaughterPdg[curPart].push_back(-1000020040);
    partDaughterPdg[curPart].push_back(  211);
    curPart++;

    partDaughterPdg[curPart].push_back( 3004); //LLn -> H3Lambda pi-
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back( 3005); //LLnn -> H4Lambda pi-
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
    
    partDaughterPdg[curPart].push_back( 2212); //H2L -> p p pi-
    partDaughterPdg[curPart].push_back( 2212);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;    

    partDaughterPdg[curPart].push_back(-2212); //H2L_bar -> p- p- pi+
    partDaughterPdg[curPart].push_back(-2212);
    partDaughterPdg[curPart].push_back(  211);
    curPart++;    
    
    partDaughterPdg[curPart].push_back( 2212); //He3L -> p p p pi-
    partDaughterPdg[curPart].push_back( 2212);
    partDaughterPdg[curPart].push_back( 2212);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;    

    partDaughterPdg[curPart].push_back(-2212); //He3L_bar -> p- p- p- pi+
    partDaughterPdg[curPart].push_back(-2212);
    partDaughterPdg[curPart].push_back(-2212);
    partDaughterPdg[curPart].push_back(  211);
    curPart++;  

    partDaughterPdg[curPart].push_back( 2212); //H2L* -> p Lambda
    partDaughterPdg[curPart].push_back( 3122);
    curPart++;

    partDaughterPdg[curPart].push_back(-2212); //H2L* -> p- Lambda_bar
    partDaughterPdg[curPart].push_back(-3122);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000010020); //H3L* -> d+ Lambda
    partDaughterPdg[curPart].push_back( 3122);
    curPart++;

    partDaughterPdg[curPart].push_back(-1000010020); //H3L*b -> d- Lambda b
    partDaughterPdg[curPart].push_back(-3122);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000010030); //H4L* -> t+ Lambda
    partDaughterPdg[curPart].push_back( 3122);
    curPart++;

    partDaughterPdg[curPart].push_back(-1000010030); //H4L*b -> t- Lambda b
    partDaughterPdg[curPart].push_back(-3122);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000020030); //He4L* -> He3+ Lambda
    partDaughterPdg[curPart].push_back( 3122);
    curPart++;

    partDaughterPdg[curPart].push_back(-1000020030); //He4L*b -> He3- Lambda b
    partDaughterPdg[curPart].push_back(-3122);
    curPart++;

    partDaughterPdg[curPart].push_back( 3004); //He4L* -> H3L p
    partDaughterPdg[curPart].push_back( 2212);
    curPart++;

    partDaughterPdg[curPart].push_back(-3004); //He4L*b -> H3Lb p-
    partDaughterPdg[curPart].push_back(-2212);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000020040); //He5L* -> He4+ Lambda
    partDaughterPdg[curPart].push_back( 3122);
    curPart++;

    partDaughterPdg[curPart].push_back(-1000020040); //He5L*b -> He4- Lambda b
    partDaughterPdg[curPart].push_back(-3122);
    curPart++;

    partDaughterPdg[curPart].push_back( 3005); //He5L* -> H4L p
    partDaughterPdg[curPart].push_back( 2212);
    curPart++;

    partDaughterPdg[curPart].push_back(-3005); //He5L*b -> H4Lb p-
    partDaughterPdg[curPart].push_back(-2212);
    curPart++;

    partDaughterPdg[curPart].push_back(1000010020); //H2Sp -> d pi+
    partDaughterPdg[curPart].push_back(211);
    curPart++;

    partDaughterPdg[curPart].push_back(1000010030); //H3Sp -> t pi+
    partDaughterPdg[curPart].push_back(211);
    curPart++;

    partDaughterPdg[curPart].push_back(1000020030); //He3Sp -> He3 pi+
    partDaughterPdg[curPart].push_back(211);
    curPart++;

    partDaughterPdg[curPart].push_back(1000020040); //He4Sp -> He4 pi+
    partDaughterPdg[curPart].push_back(211);
    curPart++;

    partDaughterPdg[curPart].push_back(1000020060); //He6Sp -> He6 pi+
    partDaughterPdg[curPart].push_back(211);
    curPart++;

    partDaughterPdg[curPart].push_back(1000030060); //Li6Sp -> Li6 pi+
    partDaughterPdg[curPart].push_back(211);
    curPart++;

    partDaughterPdg[curPart].push_back(1000030070); //Li7Sp -> Li7 pi+
    partDaughterPdg[curPart].push_back(211);
    curPart++;

    partDaughterPdg[curPart].push_back(1000040070); //Be7Sp -> Be7 pi+
    partDaughterPdg[curPart].push_back(211);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000020030); //Li5Lambda -> He3+ p+ p+ pi-
    partDaughterPdg[curPart].push_back( 2212);
    partDaughterPdg[curPart].push_back( 2212);
    partDaughterPdg[curPart].push_back( -211);
    curPart++;

    partDaughterPdg[curPart].push_back( 1000020040); //Li6Lambda -> He4+ p+ p+ pi-
    partDaughterPdg[curPart].push_back( 2212);
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
    
    partDaughterPdg[curPart].push_back( 1000020030); // He3L+ -> He3 + Pi0
    partDaughterPdg[curPart].push_back(    7700111); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(    7003029); // Pi0 <- He3L+ - He3 
    partDaughterPdg[curPart].push_back( 1000020030); //
    curPart++;

    partDaughterPdg[curPart].push_back(-1000020030); // He3L- -> He3- + Pi0
    partDaughterPdg[curPart].push_back(   -7700111); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(   -7003029); // Pi0 <- He3L- - He3- 
    partDaughterPdg[curPart].push_back(-1000020030); //
    curPart++;

    partDaughterPdg[curPart].push_back( 1000020040); // He4L+ -> He4 + Pi0
    partDaughterPdg[curPart].push_back(    7800111); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(    7003006); // Pi0 <- He4L+ - He4
    partDaughterPdg[curPart].push_back( 1000020040); //
    curPart++;

    partDaughterPdg[curPart].push_back(-1000020040); // He4L- -> He4- + Pi0
    partDaughterPdg[curPart].push_back(   -7800111); //
    curPart++;
    
    partDaughterPdg[curPart].push_back(   -7003006); // Pi0 <- He4L- - He4- 
    partDaughterPdg[curPart].push_back(-1000020040); //
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
      partMassSigma[iP] = particleInfo[iP].MassSigma();
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

  virtual ~KFPartEfficiencies(){}

 /** \brief Returns index of the decay with a given PDG code in the scheme of the KF Particle Finder. If it is not present there - returns "-1". */
  int GetParticleIndex(int pdg)
  {
    std::map<int, int>::iterator it;
    it=fPdgToIndex.find(pdg);
    if(it != fPdgToIndex.end()) return it->second;
    else return -1;
  }

  /** \brief Returns the map between PDG codes and index of the decay in the scheme of the KF Particle Finder. */
  std::map<int,int> GetPdgToIndexMap() const { return fPdgToIndex; } 
  
  void AddCounter(std::string shortname, std::string name)
  {
    /** Adds a counter with the name defined by "name" to all counter
     ** objects. For easiness of operation with counters, a shortname is assigned
     ** to each of them and the corresponding entry in the map indices is done.
     ** \param[in] shortname - a short name of the counter for fast and easy access to its index
     ** \param[in] name - name of the counter which is added to each counter object.
     **/
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

  /** \brief Operator to add efficiency table from object "a" to the current object. Returns the current object after addition. */
  KFPartEfficiencies& operator+=(KFPartEfficiencies& a){
    mc1 += a.mc1; mc2 += a.mc2; mc3 += a.mc3; reco += a.reco;
    ghost += a.ghost; bg += a.bg; clone += a.clone;
    return *this;
  };
  
  /** \brief Function to calculate efficiency after all counters are set. If the counters are modified the function should be called again. */
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
    /** Increases counters by one, if the corresponding boolean variable is "true".
     ** \param[in] isReco - "true" if particle is reconstructed
     ** \param[in] nClones - number of double reconstructed particles for the given MC particle,
     ** will be added to the "clone" counters
     ** \param[in] isMC1 - "true" if particle is reconstructable in 4pi, mc1 is increased
     ** \param[in] isMC2 - "true" if all daughters are reconstructable, mc2 is increased
     ** \param[in] isMC3 - "true" if all daughters are reconstructed, mc3 is increased
     ** \param[in] name  - "shortname" of the set of counters, which should be increased
     **/
    const int index = indices[name];
    
    if(isMC1) mc1.counters[index]++;
    if(isMC2) mc2.counters[index]++;
    if(isMC3) mc3.counters[index]++;
    
    if(isReco) reco.counters[index]++;
    if(nClones > 0)
      clone.counters[index] += nClones;
  };

  void IncReco(bool isGhost, bool isBg, std::string name)
  {
    /** Increases counters by one, if the corresponding boolean variable is "true".
     ** \param[in] isGhost - "true" if ghost is added
     ** \param[in] isBg - "true" if physics background is added
     ** \param[in] name  - "shortname" of the set of counters, which should be increased
     **/
    const int index = indices[name];

    if (isGhost) ghost.     counters[index]++;
    if (isBg)    bg.counters[index]++;
  };

  /** \brief Prints the efficiency table on the screen. */
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
  
  float GetTotal4piEfficiency(int iDecay) { return ratio_reco1.counters[3*iDecay]; }  ///< Returns efficiency in 4pi for decay "iDecay".
  float GetTotalKFPEfficiency(int iDecay) { return ratio_reco3.counters[3*iDecay]; }  ///< Returns efficiency of KF Particle Finder method (cuts) for decay "iDecay".
  float GetPrimary4piEfficiency(int iDecay) { return ratio_reco1.counters[3*iDecay+1]; } ///< Returns efficiency in 4pi for decay "iDecay" for primary particles.
  float GetPrimaryKFPEfficiency(int iDecay) { return ratio_reco3.counters[3*iDecay+1]; } ///< Returns efficiency of KF Particle Finder method (cuts) for decay "iDecay" for primary particles.
  float GetSecondary4piEfficiency(int iDecay) { return ratio_reco1.counters[3*iDecay+2]; } ///< Returns efficiency in 4pi for decay "iDecay" for secondary particles.
  float GetSecondaryKFPEfficiency(int iDecay) { return ratio_reco3.counters[3*iDecay+2]; } ///< Returns efficiency of KF Particle Finder method (cuts) for decay "iDecay" for secondary particles.
  
  /** \brief Operator to write efficiencies to file. */
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
  /** \brief Operator to read efficiencies from file. */
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
  /** \brief Adds efficiency from the file with the name defined by "fileName" to the current objects. */
  void AddFromFile(std::string fileName)
  {
    std::fstream file(fileName.data(),std::fstream::in);
    file >> *this;
  }
  
  int GetNDaughters(int iParticle) const { return partDaughterPdg[iParticle].size(); } ///< Returns number of daughter particles for the decay with index "iParticle".
  /** \brief Returns the PDG code of the daughter "iDaughter" from the decay with index "iParticle". */
  int GetDaughterPDG(int iParticle, int iDaughter) const { return partDaughterPdg[iParticle][iDaughter]; }
  
  float GetMass(int iParticle) const { return partMass[iParticle]; } ///< Returns the table mass of the decay with index "iParticle".
  float GetMassSigma(int iParticle) const { return partMassSigma[iParticle]; } ///< Returns expected width of the mass peak of the decay with index "iParticle".

  static const int nParticles = 316;                     ///< Number of particles.
  static const int fFirstHypernucleusIndex = 159;        ///< Index of the first hypernuclei in the list.
  static const int fLastHypernucleusIndex = 236;         ///< Index of the last hypernuclei in the list.
  static const int fFirstMissingMassParticleIndex = 237; ///< Index of the first decay reconstructed by the missing mass method.
  static const int fLastMissingMassParticleIndex = 280;  ///< Index of the last decay reconstructed by the missing mass method.
  static const int fFirstStableParticleIndex = 281;      ///< Index of the first stable particle in the list.
  static const int fLastStableParticleIndex = 306;       ///< Index of the last stable particle in the list.
  
  int partPDG[nParticles];                        ///< Array of PDG codes assigned to the decays.
  std::string partName[nParticles];               ///< Array of names of the decay in the file with histograms.
  std::string partTitle[nParticles];              ///< Array of names of the decay in the output table with efficiency.
  std::vector<std::vector<int> > partDaughterPdg; ///< Array with vectors of daughter particles for each decay.
  float partMHistoMin[nParticles];                ///< Array with lower boundary in the mass histograms for each decay.
  float partMHistoMax[nParticles];                ///< Array with upper boundary in the mass histograms for each decay.
  int partMaxMult[nParticles];                    ///< Array with upper boundary in the multiplicity histograms of each decay.
  float partMass[nParticles];                     ///< Array with table masses of each decay.
  float partLifeTime[nParticles];                 ///< Array with lifetimes in seconds of each decay.
  int partCharge[nParticles];                     ///< Array with charge of each particle specie in units of the elementary charge.
  float partMassSigma[nParticles];                ///< Array with expected width of mass peaks used for the side band method.

  
 private:
  std::vector<std::string> names;     ///< Names of the counters. The same for all counters objects.
  std::map<std::string, int> indices; ///< Map between the counter index and its short name.

  std::map<int, int> fPdgToIndex;     ///< The map between PDG code assigned to the decay and index in the decay list.

  KFMCCounter<double> ratio_reco1;    ///< Efficiency in 4 pi for all decays.
  KFMCCounter<double> ratio_reco2;    ///< Efficiency normalised on the particles with all daughters reconstructable for all decays.
  KFMCCounter<double> ratio_reco3;    ///< Efficiency normalised on the particles with all daughters reconstructed for all decays.

  KFMCCounter<int> mc1;               ///< Counters of the Monte Carlo particles of all species.
  KFMCCounter<int> mc2;               ///< Counters of the Monte Carlo particles with all daughters reconstructable for all species.
  KFMCCounter<int> mc3;               ///< Counters of the Monte Carlo particles with all daughters found for all species.

  KFMCCounter<int> reco;              ///< Counters of the reconstructed particles for all species.

  KFMCCounter<double> ratio_ghost;    ///< Ratio of the ghost candidates to the total number of candidates for all species.
  KFMCCounter<double> ratio_bg;       ///< Ratio of the physics background candidates to the total number of candidates for all species.
  KFMCCounter<double> ratio_clone;    ///< Ratio of double reconstructed particles to the total number of signal candidates for all species.

  KFMCCounter<int> ghost;             ///< Counters of the ghost candidates for all species.
  KFMCCounter<int> bg;                ///< Counters of the physics background candidates for all species.
  KFMCCounter<int> clone;             ///< Counters of the double reconstructed particles for all species.
  
#ifndef KFParticleStandalone
  ClassDef( KFPartEfficiencies, 1 )
#endif
};

#endif
