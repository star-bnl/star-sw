// $Id: StFtpcMagboltz2.hh,v 1.1 2000/12/20 08:44:03 jcs Exp $
//
// $Log: StFtpcMagboltz2.hh,v $
// Revision 1.1  2000/12/20 08:44:03  jcs
// Replace pam/ftpc/fmg with maker
//
//
// Header file for StFtpcMagboltz2
// f2c of magboltz 2

#ifndef STAR_StFtpcMagboltz2
#define STAR_StFtpcMagboltz2

#include "TBenchmark.h"


class StFtpcMagboltz2
{

private:
  int c__1;
  double c_b524;
  double c_b1350;
  double c_b1360;
  double c_b1361;
  TBenchmark *mBench;

  /* Common Block Declarations */
  
  struct {
    double eovb, wb, btheta, bmag;
} bfld_1;

struct {
    int nout, itmax, i2type, ngas, nstep, nstep1;
    double conv, efinal, estep, akt, ary, tempc, torr;
    int idbug, isfb;
    double conalp, alpnew, alpold, alpnax, alpnay, alpnaz, alpha, alpoax, 
	    alpoay, alpoaz;
    int nitalp, idlong, lhigh;
} inpt_1;

struct {
    double tmax, small, api, estart, theta, phi, tcfmax[10], rstart, 
	    efield;
    int nmax;
} setp_1;

struct {
    double an1, an2, an3, an4, frac1, frac2, frac3, frac4, an;
} ratio_1;

struct {
    int ngas1, ngas2, ngas3, ngas4;
} gasn_1;

struct {
    double qelm[2002], qsum[2002], qion[8008]	/* was [4][2002] */, 
	    qin1[40040]	/* was [20][2002] */, qin2[40040]	/* was [20][
	    2002] */, qin3[40040]	/* was [20][2002] */, qin4[40040]	
	    /* was [20][2002] */, qsatt[2002];
} mix1_1;

union {
    struct {
	double e[2002], eroot[2002], qtot[2002], qrel[2002], qinel[2002], 
		qel[2002];
    } _1;
    struct {
	double es[2002], eroot[2002], qtot[2002], qrel[2002], qinel[2002],
		 qel[2002];
    } _2;
} mix2_;

#define mix2_1 (mix2_._1)
#define mix2_2 (mix2_._2)

struct {
    int nin1, nin2, nin3, nin4, lion[4], lin1[20], lin2[20], lin3[20], 
	    lin4[20];
    double alion[4], alin1[20], alin2[20], alin3[20], alin4[20];
} mix3_1;

struct {
    double const1, const2, const3, const4, const5;
} cnsts1_1;

union {
    struct {
	double cf[128000], ein[64], tcf[2000];
	int iarry[64];
	double rgas[64];
	int ipn[64], iplast;
    } _1;
    struct {
	double cf[128000], ein[64], tcf[2000];
	int iarry[64];
	double rgas[64];
	int ipn[64], last;
    } _2;
} large_;

#define large_1 (large_._1)
#define large_2 (large_._2)

struct {
    double pel[8008]	/* was [4][2002] */, pin[16016]	/* was [8][
	    2002] */;
    int kel[4], index[64], niso;
} anis_1;

struct {
    double van1, van2, van3, van4, van;
} mratio_1;

struct {
    char name1[15], name2[15], name3[15], name4[15];
} names_1;

struct {
    double echarg, emass, amu, pir2;
} cnsts_1;

struct {
    double erfint[25], con;
    int ithrm;
} thrm_1;

struct {
    double time[300];
    int icoll[20];
    double spec[2000], tmax1, ave, xid, x, y, z__, st;
    int nnull;
} outpt_1;

struct {
    double wx, wy, wz;
} vel_1;

struct {
    double difxx, difyy, difzz, difyz, difxy, difxz;
} diflab_1;

struct {
    double difln, diftr;
} difvel_1;

struct {
    double simf[2002];
} sint_1;

  int min(int i, int j) {if(i>j) return j;
			 else return i;}
  double min(double i, double j) {if(i>j) return j;
			       else return i;}
  int max(int i, int j) {if(i<j) return j;
			 else return i;}
  double max(double i, double j) {if(i<j) return j;
			       else return i;}
  
public:
  StFtpcMagboltz2();
  ~StFtpcMagboltz2();
  int StFtpcMagboltz2::magboltz_(float *e_magni__, float *b_magni__,
				 float *b_ang__, float *press, float *p_ar__, 
				 float *p_co2__, float *p_ne__, 
				 float *p_he__, float *temper,
				 float *vdr, float *psiang, float *efin);
  int StFtpcMagboltz2::mixer_();
  int StFtpcMagboltz2::gasmix_(int *ngs, int *niso, double *q, 
			       double *qin, int *nin, double *e, 
			       double *ei, char *name__, double *virial, 
			       double *peqel, double *peqin, int *kel, 
			       int *kin, int name_len);
  int StFtpcMagboltz2::setup_(float *e_magni__,
			      float *b_magni__, float *b_ang__,
			      float *press, float *p_ar__,
			      float *p_co2__, float *p_ne__,
			      float *p_he__, float *temper);
  int StFtpcMagboltz2::prnter_();
  int StFtpcMagboltz2::monte_();
  int StFtpcMagboltz2::output_();
  int StFtpcMagboltz2::montea_();
  int StFtpcMagboltz2::monteb_();
  int StFtpcMagboltz2::montec_();
  int StFtpcMagboltz2::elimit_(int *ielow);
  int StFtpcMagboltz2::elimitb_(int *ielow);
  int StFtpcMagboltz2::elimitc_(int *ielow);
  int StFtpcMagboltz2::gas2_(double *q, double *qin, int *nin, 
			     double *e, double *ein, char *name__, 
			     double *virial, int *monte, int name_len);
  int StFtpcMagboltz2::gas3_(double *q, double *qin, int *nin, 
			     double *e, double *ein, char *name__, 
			     double *virial, int *monte, int name_len);
  int StFtpcMagboltz2::gas5_(double *q, double *qin, int *nin, 
			     double *e, double *ein, char *name__, 
			     double *virial, int *monte, int name_len);
  int StFtpcMagboltz2::gas12_(double *q, double *qin, int *nin, 
			      double *e, double *ein, char *name__, 
			      double *virial, int *monte, int name_len);


};
#endif








