// Interface to Magboltz (version 9)

#ifndef G_MAGBOLTZ_INTERFACE
#define G_MAGBOLTZ_INTERFACE

#ifndef __CINT__

namespace Garfield {

namespace Magboltz {

extern "C" {

// Magboltz COMMON blocks

// Magnetic field
extern struct {
  double eovb;
  double wb;
  double btheta, bmag;
} bfld_;

extern struct {
  long long nGas;
  long long nStep;
  long long nAniso;
  double efinal;
  double estep;
  double akt;
  double ary;
  double tempc;
  double torr;
  long long ipen;
} inpt_;

extern struct {
  double tmax;
  double small;
  double api;
  double estart;
  double theta, phi;
  double rstart;
  double efield;
  long long nmax;
} setp_;

// Physical constants
extern struct {
  double echarg;
  double emass;
  double amu;
  double pir2;
} cnsts_;

// Definition of the gas mixture
extern struct {
  long long ngasn[6];
} gasn_;
extern struct {
  double an1, an2, an3, an4, an5, an6, an;
  double frac[6];
} ratio_;

// Calculation results
// Drift velocity
extern struct {
  double wx, wy, wz;
} vel_;
extern struct {
  double dwx, dwy, dwz;
} velerr_;

// Diffusion
extern struct {
  double difxx, difyy, difzz;
  double difyz, difxy, difxz;
} diflab_;
extern struct {
  double dxxer, dyyer, dzzer;
  double dyzer, dxyer, dxzer;
} diferb_;
extern struct {
  double difln, diftr;
} difvel_;
extern struct {
  double dfler, dfter;
} diferl_;

// Townsend and attachment coefficient
extern struct {
  double alpha, att;
} ctowns_;
extern struct {
  double alper, atter;
} ctwner_;
extern struct {
  double ralpha, ralper;
  double tofene, tofener, tofwv, tofwver;
  double tofdl, tofdler, tofdt, tofdter;
  double tofwr, tofwrer;
  double rattof, ratofer;
} tofout_;

void gasmix_(long long* ngs, double* q, double* qin, long long* nin, double* e,
             double* ei, char* name, double* virl, double* eb, double* peqel,
             double* peqin, double* penfra, long long* kel, long long* kin,
             double* qion, double* peqion, double* eion, long long* nion,
             long long* nc0, double* ec0, double* wk, double* efl, 
             long long* ng1, double* eg1, long long* ng2, double* eg2,  
             char scrpt[260][50]);

void setup1_();
void setupt1_();

void mixer_();
void mixert_();

void elimit_(long long* ielow);
void elimitb_(long long* ielow);
void elimitc_(long long* ielow);
void elimitt_(long long* ielow);
void elimitbt_(long long* ielow);
void elimitct_(long long* ielow);

void monte_();
void montea_();
void monteb_();
void montec_();
void montet_();
void monteat_();
void montebt_();
void montect_();

void alpcalc_();
void alpclca_();
void alpclcb_();
void alpclcc_();
void alpcalct_();
void alpclcat_();
void alpclcbt_();
void alpclcct_();

void prnter_();
void output_();
void output2_();
void outputt_();
void output2t_();
}
}
}
#endif
#endif
