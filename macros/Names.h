#ifndef __NAMES_H__
#define __NAMES_H__
enum { NHYPS = 18, NHypTypes = NHYPS/2, NTpcRSParts = 22};
const Char_t *NamesF[NHYPS]    = {"electron","antiproton","kaon-","pion-","muon-","dbar","tbar","He3Bar","alphabar",
				  "positron","proton"    ,"kaon+","pion+","muon+","deuteron"   ,"triton"   ,"He3"    ,"alpha"};
const Char_t *Names[NHYPS]     = {"e-","pbar","K-","pi-","mu-","dbar","tbar","He3Bar","alphabar",
				  "e+","p"   ,"K+","pi+","mu+","d"   ,"t"   ,"He3"    ,"alpha"};
const Char_t *HistNames[NHYPS] = {"eNzB","protonNzB","kaonNzB","piNzB","muNzB","deuteronNzB","tritonNzB","He3NzB","alphaNzB",
				  "ePzB","protonPzB","kaonPzB","piPzB","muPzB","deuteronPzB","tritonPzB","HePzB","alphaPzB"};
const Char_t *HistNames70[NHYPS] = {"eN70B","protonN70B","kaonN70B","piN70B","muN70B","deuteronN70B","tritonN70B","He3N70B","alphaN70B",
				    "eP70B","protonP70B","kaonP70B","piP70B","muP70B","deuteronP70B","tritonP70B","He3P70B","alphaP70B"};
const Char_t *HistNameP[NHYPS] = {"eNzB","protonNzB","kaonNzB","piNzB","muNzB","deuteronNzB","tritonNzB","He3NzB","alphaNzB",
				  "ePzB","protonPzB","kaonPzB","piPzB","muPzB","deuteronPzB","tritonPzB","He3PzB","alphaPzB"};
const Double_t Masses[NHYPS] = {0.51099907e-3,0.93827231,0.493677,0.13956995,0.1056584,1.875613,2.80925, 2.80923,3.727417,
				0.51099907e-3,0.93827231,0.493677,0.13956995,0.1056584,1.875613,2.80925, 2.80923,3.727417};
const Int_t GEANTiD[NHYPS]    = { 3, 15, 12,  9, 6, 53, 50046, 50049, 50047, // GEANT part Id
				  2, 14, 11,  8, 5, 45,    46,    49,   47};
#if 0
const Char_t *NamesT[NHYPS]    = {"electron","antiproton","kaon","pion","muon","dbar",    "tbar",  "He3Bar","alphabar",
				  "positron","proton"    ,"kaon","pion","muon","deuteron","triton","He3"   ,"alpha"};
const Int_t ChargeT[NHYPS]     = {       -1,          -1,    -1,    -1,   -1,        -1,       -1,  -2,  -2,
					  1,           1,     1,     1,    1,         1,        1,   2,   2};
#endif
struct TpcRSPart_t {
  const Char_t *name;
  Double_t      mass;
  Int_t       charge;
  Int_t          pdg;
  const Char_t *pdgname;
};
TpcRSPart_t TpcRSPart[NTpcRSParts] = {
  {"muon+",               0.1056584,  1,         -13, "mu+"}, 	  
  {"muon-",    	          0.1056584, -1,          13, "mu-"},	  
  {"pion+",    	         0.13956995,  1,         211, "pi+"},	  
  {"pion-",    	         0.13956995, -1,        -211, "pi-"},	  
  {"electron-",       0.51099907e-3, -1,          11, "e-"},	  
  {"electron+",       0.51099907e-3,  1,         -11, "e+"},	  
  {"kaon+",                0.493677,  1,         321, "K+"},	  
  {"kaon-",    	           0.493677, -1,        -321, "K-"},	  
  {"proton+",            0.93827231,  1,        2212, "proton"}, 	  
  {"proton-",  		 0.93827231, -1,       -2212, "antiproton"},
  {"deuteron",        1.87561294257,  1,  1000010020, "DEUTERON"}, 
  {"triton",          2.80892113298,  1,  1000010030, "TRITON"}, 
  {"He3",             2.80839160743,  2,  1000020030, "HE3"}, 
  {"alpha",            3.7273794066,  2,  1000020040, "ALPHA"}, 
  {"HE6",                 5.6055375,  2,  1000020060, "HE6"}, 
  {"Li5",  		  4.6676161,  3,  1000030050, "Li5"},
  {"Li6",  		  5.6015181,  3,  1000030060, "Li6"},
  {"Li7",  		  6.5338336,  3,  1000030070, "Li7"},
  {"Be7",                 6.5341844,  4,  1000040070, "Be7"}, 
  {"Be9",  9.012182201*0.9314943228,  4,  1000040090, "Be9"}, 
  {"Be10",10.013533818*0.9314943228,  4,  1000040100, "Be10"}, 
  {"B11", 11.0216577497*0.9314943228, 5,  1000050110, "B11"}
};
#if 0
enum PidParticle {
  kPidElectron ,
  kPidProton   ,
  kPidKaon     ,
  kPidPion     ,
  kPidMuon     ,
  kPidDeuteron ,
  kPidTriton   ,
  kPidHe3      ,
  kPidAlpha    ,
  KPidParticles
};
#else
#include "StPidParticleDefinition.h"
#endif
const Char_t *PidNames[KPidParticles] = {"e","proton","kaon","pi","mu","deuteron","Triton","He3","Alpha"};
#endif
