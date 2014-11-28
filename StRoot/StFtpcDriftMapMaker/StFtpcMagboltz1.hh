// $Id: StFtpcMagboltz1.hh,v 1.2 2001/04/25 17:47:12 perev Exp $
//
// $Log: StFtpcMagboltz1.hh,v $
// Revision 1.2  2001/04/25 17:47:12  perev
// HPcorrs
//
// Revision 1.1  2000/12/20 08:44:02  jcs
// Replace pam/ftpc/fmg with maker
//
//
// Header file for StFtpcMagboltz1
// f2c of magboltz 1

#ifndef STAR_StFtpcMagboltz1
#define STAR_StFtpcMagboltz1



class StFtpcMagboltz1
{

private:
  int c__1;
  int c__4;
  int c__0;
  int c__3;
  double c_b12;
  
  /* Common Block Declarations */
  
  struct {
    float e_magnitude__, b_magnitude__, b_angle__, pressure, perc_ar__, 
      perc_co2__, perc_ne__, perc_he__, temperature;
  } callpars_1;
  
  struct {
    double f[2002], df[2002], df0[2002];
  } f0c_1;
  
  struct {
    double f1[2002], df1[2002];
  } f1c_1;
  
  struct {
    double h1[2002], dh1[2002];
  } h1c_1;
  
  struct {
    double g[2002], dg[2002], dg0[2002];
  } g0c_1;
  
  struct {
    double g1[2002], dg1[2002];
  } g1c_1;
  
  struct {
    double g2[2002], dg2[2002];
  } g2c_1;
  
  struct {
    int nout, itmax, i2type, ngas, nstep, nstep1;
    double conv, efinal, estep, akt, ary, tempc, torr;
    int idbug, isfb;
    double conalp, alpnew, alpold, alpnax, alpnay, alpnaz, alpha, alpoax, 
      alpoay, alpoaz;
    int nitalp, idlong, lhigh;
  } inpt_1;
  
  struct {
    double f2[2002], df2[2002];
  } f2c_1;
  
  struct {
    double velz, sig_long__, sig_tranx__, sig_trany__, angle, amk_emag__, 
      amk_setemag__, emean, btheta_mk__, bmag_mk__, emaximum;
  } mk_1;
  
  struct {
    double an1, an2, an3, an4, frac1, frac2, frac3, frac4, an;
  } ratio_1;
  
  struct {
    char ngas1[15], ngas2[15], ngas3[15], ngas4[15];
  } gasn_1;
  
  struct {
    double qef[2002], denom[2002], cod2[2002], sod2[2002], scd[2002], sod[
									  2002], btheta, bmag, wb, emag, eovm, qeeef[2002], qeef[2002], 
      qfemag[2002], ef[2002], qe[2002];
  } mag_1;
  
  struct {
    double echarg, emass, amu, pir2;
  } cnsts_1;
  
  struct {
    double qelm[2002], qsum[2002], qion[8008]       /* was [4][2002] */, 
      qin1[48048] /* was [24][2002] */, qin2[48048]       /* was [24][
		     2002] */, qin3[48048]       /* was [24][2002] */, qin4[48048]
    /* was [24][2002] */, qsatt[2002];
  } mix1_1;
  
  struct {
    double e[2002], eroot[2002], qtot[2002], qrel[2002], qinel[2002], qel[
									  2002];
  } mix2_1;
  
  struct {
    int nin1, nin2, nin3, nin4, lion[4], lin1[24], lin2[24], lin3[24], 
      lin4[24];
    double alion[4], alin1[24], alin2[24], alin3[24], alin4[24];
  } mix3_1;
  
  struct {
    int n2ro1, n2ro2, n2ro3, n2ro4, l2ro1[3], l2ro2[3], l2ro3[3], l2ro4[3]
    ;
    double al2ro1[3], al2ro2[3], al2ro3[3], al2ro4[3], q2ro1[12012] /* 
								       was [2][3][2002] */, q2ro2[12012]   /* was [2][3][2002] */, q2ro3[
																	 12012]      /* was [2][3][2002] */, q2ro4[12012]    /* was [2][3][
																			2002] */;
  } mix4_1;
  
  struct {
    double simf[2002];
  } sint_1;
  
  struct {
    char name1[15], name2[15], name3[15], name4[15];
  } names_1;
  
  struct {
    double f3[2002], df3[2002];
  } f3c_1;
  
  
public:
  StFtpcMagboltz1();
  ~StFtpcMagboltz1();
  int magboltz_(float *e_magni__, float *b_magni__,
				 float *b_ang__, float *press, float *p_ar__, 
				 float *p_co2__, float *p_ne__, 
				 float *p_he__, float *temper,
				 float *vdr, float *psiang, float *efin);
  int bfield_(int *nstep1);
  int f0calc_(int *iback, int *icon, double *anew, int *l);
  int fncalc_(int *lmax);
  int g0calc_(int *icon, double *gfinal, 
			       double *eg0sum, int *lmax);

  int gas1_(double *q, double *qin, double *q2ro, int *nin,
			     int *n2ro, double *e, double *ein, 
			     double *e2ro, char *name__, 
			     double *virial, int *monte, int name_len);
  int gas2_(double *q, double *qin, double *q2ro, int *nin,
			     int *n2ro, double *e, double *ein, double *e2ro,
			     char *name__, 
			     double *virial, int *monte, int name_len);
  int gas3_(double *q, double *qin, double *q2ro, int *nin,
			     int *n2ro, double *e, double *ein, double *e2ro, 
			     char *name__, 
			     double *virial, int *monte, int name_len);
  int gas4_(double *q, double *qin, double *q2ro, int *nin,
			     int *n2ro, double *e, double *ein, double *e2ro, 
			     char *name__, 
			     double *virial, int *monte, int name_len);
  int h1calc_(int *l, double *dhfnal, double *dxx, 
			       double *dhfrst);
  int mixer_();
  int nalpha_();
  int output_(int *n);
  int prnter_();
  int setup_(int *last);
  int simp_(double *sum);
  int stepph_(int *l);
  int steppr_(int *itype, int *lmax);
  int type2_(double *s2sum, int *i__, int *nstep1);
  int type2g_(double *s2sum, int *i__, int *nstep1);
  
};
#endif








