
// $Id: StFtpcMagboltz1.cc,v 1.2 2003/09/02 17:58:15 perev Exp $
//
// $Log: StFtpcMagboltz1.cc,v $
// Revision 1.2  2003/09/02 17:58:15  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.1  2000/12/20 08:44:02  jcs
// Replace pam/ftpc/fmg with maker
//

#include "StFtpcMagboltz1.hh"
#include <math.h>
#include <stdio.h>

StFtpcMagboltz1::StFtpcMagboltz1()
{
  /* Table of constant values */
c__1 = 1;
c__4 = 4;
c__0 = 0;
c__3 = 3;
c_b12 = 10.;
}

StFtpcMagboltz1::~StFtpcMagboltz1()
{

}

int StFtpcMagboltz1::magboltz_(float *e_magni__, float *b_magni__,
float *b_ang__, float *press, float *p_ar__, 
	float *p_co2__, float *p_ne__, float *p_he__, float *temper,
float *vdr, float *psiang, float *efin)
{
    /* System generated locals */
    int i__1;
    double d__1;

    /* Local variables */
    static double aold, anew;
    static int last, itot, iout, j, l, m;
    static double aconv, anorm;
    static int ist;


/* --------------------------------------------------------------------- 
*/
/*  FILL CALLPARS COMMON BLOCK WITH CALLUP PARAMETERS */
    callpars_1.e_magnitude__ = *e_magni__;
    printf("setting e_magnitude to %f\n", callpars_1.e_magnitude__);
    callpars_1.b_magnitude__ = *b_magni__;
    callpars_1.b_angle__ = *b_ang__;
    callpars_1.pressure = *press;
    callpars_1.perc_ar__ = *p_ar__;
    callpars_1.perc_co2__ = *p_co2__;
    callpars_1.perc_ne__ = *p_ne__;
    callpars_1.perc_he__ = *p_he__;
    callpars_1.temperature = *temper;
    inpt_1.efinal = *efin;
    last = 0;
    setup_(&last);
L777:
    mixer_();
    prnter_();
    bfield_(&inpt_1.nstep1);

/*  BACKWARD PROLONGATION AND GAUSS-SEIDEL ITERATION */

    i__1 = inpt_1.lhigh;
    for (l = 1; l <= i__1; ++l) {
	aold = (float)1.;
	itot = 0;
	iout = 0;
	f0calc_(&c__0, &c__0, &anew, &l);
	goto L100;
L5:
	f0calc_(&c__1, &c__0, &anew, &l);
L100:
	++itot;
	++iout;
	anorm = (d__1 = (float)1. - anew / aold, fabs(d__1));
	printf("NEW SUM = %f OLD SUM = %f FRACTIONAL DIFFERENCE = %f ITERATION NUMBER = %d\n", anew, aold, anorm, itot);
	aold = anew;
	if (iout == inpt_1.nout) {
	    output_(&c__0);
	}
	if (iout == inpt_1.nout) {
	    iout = 0;
	}
	if (itot > inpt_1.itmax) {
	    goto L20;
	}
	if (anorm > inpt_1.conv) {
	    goto L5;
	}
	stepph_(&l);
	if (inpt_1.idlong == 1) {
	    steppr_(&c__0, &l);
	}
	output_(&l);
	if (inpt_1.i2type == 1) {
	    goto L50;
	}
	if (inpt_1.nitalp == 1 && inpt_1.alpha != (float)0.) {
	    goto L30;
	}
	goto L60;

/*   INCLUDE COLLISIONS OF 2ND KIND */

L50:
	itot = 0;
	iout = 0;
L10:
	f0calc_(&c__1, &inpt_1.i2type, &anew, &l);
	stepph_(&l);
	++iout;
	++itot;
	anorm = (d__1 = (float)1. - anew / aold, fabs(d__1));
	printf("NEW SUM = %f OLD SUM = %f FRACTIONAL DIFFERENCE = %f ITERATION NUMBER = %d\n", anew, aold, anorm, itot);
	aold = anew;
	if (iout == inpt_1.nout) {
	    output_(&c__0);
	}
	if (iout == inpt_1.nout) {
	    iout = 0;
	}
	if (itot > inpt_1.itmax) {
	    goto L20;
	}
	if (anorm >= inpt_1.conv) {
	    goto L10;
	}
	if (inpt_1.idlong == 1) {
	    steppr_(&inpt_1.i2type, &l);
	}
	output_(&c__3);
	goto L60;

/*  INCLUDE NET IONISATION AND ATTACHMENT TO ALL ORDERS */

L30:
	itot = 0;
	ist = 0;
L40:
	f0calc_(&c__1, &inpt_1.i2type, &anew, &l);
	stepph_(&l);
	++itot;
	anorm = (d__1 = (float)1. - anew / aold, fabs(d__1));
	if (anorm < inpt_1.conv) {
	  printf("NEW SUM = %f OLD SUM = %f FRACTIONAL DIFFERENCE = %f ITERATION NUMBER = %d\n", anew, aold, anorm, itot);
	}
	aold = anew;
	if (itot > inpt_1.itmax) {
	    goto L20;
	}
	if (anorm >= inpt_1.conv) {
	    goto L40;
	}

/*     ITERATE ON NET IONISATION */

	nalpha_();
	++ist;
	if (inpt_1.alpold == (float)0.) {
	    inpt_1.alpold = 1e-10;
	}
	if (inpt_1.alpnew < (float)700.) {
	    goto L876;
	}
	if (ist == 1) {
	    inpt_1.alpnew *= (float).5;
	}
	if (ist == 1) {
	    inpt_1.alpnax *= (float).5;
	}
	if (ist == 1) {
	    inpt_1.alpnay *= (float).5;
	}
	if (ist == 1) {
	    inpt_1.alpnaz *= (float).5;
	}
L876:
	aconv = (d__1 = (float)1. - inpt_1.alpnew / inpt_1.alpold,
fabs(d__1));
	if (aconv >= inpt_1.conalp) {
	    goto L40;
	}
	if (inpt_1.idlong == 1) {
	    steppr_(&inpt_1.i2type, &l);
	}
	output_(&c__4);

/*   CALCULATE HIGHER MULTIPOLE TERM IN F-DISTRIBUTION TO FIRST ORDER 
*/

/* REDUCED OUTPUT */
L60:
	if (l == 2) {
	    goto L2;
	}
	if (l == 1) {
	    goto L3000;
	}
/* REDUCED OUTPUT */
	fncalc_(&l);
	stepph_(&l);
	if (inpt_1.idlong == 1) {
	    steppr_(&inpt_1.i2type, &l);
	}
	m = l + 4;
/* MK      CALL OUTPUT(M) */
	for (j = 1; j <= 2002; ++j) {
	    f2c_1.f2[j - 1] = (float)0.;
/* L433: */
	    f2c_1.df2[j - 1] = (float)0.;
	}

L3000:
	;
    }
    goto L2;
L20:
    printf("NUMBER OF ITERATIONS GREATER THAN ALLOWED = %d\n", itot);
/* ********** MK new 99-routine ********** */
L2:
    if (f0c_1.f[inpt_1.nstep - 1] > (float)1e-7) {
	printf("Jump 1: %f\n", f0c_1.f[inpt_1.nstep - 1]);
	inpt_1.efinal *= (float)1.1;
/* increase by 10% */
	last = 20;
	setup_(&last);
	last = 0;
	goto L777;
    } else if (f0c_1.f[inpt_1.nstep - 1] < (float)1e-10) {
	printf("Jump 1: %f\n", f0c_1.f[inpt_1.nstep - 1]);
	inpt_1.efinal *= (float).9;
/* decrease by 10% */
	last = 20;
	setup_(&last);
	last = 0;
	goto L777;
    } else {
	printf("********************\n");
	d__1 = mk_1.velz / (float)1e6;
	printf("Drift velocity == %f\n", d__1);
	printf("Longitudinal diff = %f\n",mk_1.sig_long__);
	printf("Transvers_xx diff = %f\n", mk_1.sig_tranx__);
	printf("Transvers_yy diff = %f\n", mk_1.sig_trany__);
	printf("Lorentz angle  == %f\n",mk_1.angle);
	printf("Drift Field =    %f\n", mk_1.amk_emag__);
        printf("********************\n");
	*vdr = mk_1.velz / (float)1e6;
	*psiang = mk_1.angle;
	*efin = inpt_1.efinal;
    }
    return 0;
} /* magboltz_ */


int StFtpcMagboltz1::bfield_(int *nstep1)
{
    /* System generated locals */
    int i__1;
    double d__1, d__2;

    /* Local variables */
    static double bcos, bsin, emag2;
    static int j;
    static double en, pi, atheta, ef2, sincos, cos2, sin2;

    pi =(double) acos(-1.);
    atheta = mag_1.btheta * pi / 180.;
    bsin = sin(atheta);
    bcos = cos(atheta);

/*   REMOVE ROUNDING ERRORS IN SIN AND COS AT 90 DEGREES. */

    if (mag_1.btheta == (float)90.) {
	bsin = (float)1.;
    }
    if (mag_1.btheta == (float)90.) {
	bcos = (float)0.;
    }
    sin2 = bsin * bsin;
    cos2 = bcos * bcos;
    sincos = bsin * bcos;
    emag2 = mag_1.emag * mag_1.emag;
    i__1 = *nstep1 + 1;
    for (j = 1; j <= i__1; ++j) {
	en = mix2_1.e[j - 1];
	if (en == (float)0.) {
	    en = (float)1e-4;
	}
/* Computing 2nd power */
	d__1 = mag_1.bmag;
/* Computing 2nd power */
	d__2 = mix2_1.qtot[j - 1];
	mag_1.denom[j - 1] = cnsts_1.echarg * (d__1 * d__1) / (cnsts_1.emass *
		 2e6 * en * (d__2 * d__2));
	mag_1.sod[j - 1] = ::sqrt(mag_1.denom[j - 1]) * bsin;
	mag_1.cod2[j - 1] = mag_1.denom[j - 1] * cos2;
	mag_1.sod2[j - 1] = mag_1.denom[j - 1] * sin2;
	mag_1.scd[j - 1] = mag_1.denom[j - 1] * sincos;
	mag_1.cod2[j - 1] += (float)1.;
	mag_1.sod2[j - 1] += (float)1.;
	mag_1.denom[j - 1] += (float)1.;
	mag_1.sod[j - 1] /= mag_1.denom[j - 1];
	mag_1.sod2[j - 1] /= mag_1.denom[j - 1];
	mag_1.cod2[j - 1] /= mag_1.denom[j - 1];
	mag_1.scd[j - 1] /= mag_1.denom[j - 1];
	ef2 = emag2 * cos2 + emag2 * sin2 / mag_1.denom[j - 1];
	mag_1.ef[j - 1] = ef2 / mag_1.emag;
	mag_1.qe[j - 1] = mag_1.ef[j - 1] / mix2_1.qtot[j - 1];
	mag_1.qef[j - 1] = en / (mix2_1.qtot[j - 1] * (float)3.);
	mag_1.qfemag[j - 1] = mag_1.qef[j - 1] * mag_1.emag;
	mag_1.qeef[j - 1] = mag_1.qef[j - 1] * mag_1.ef[j - 1];
/* L100: */
	mag_1.qeeef[j - 1] = ef2 * mag_1.qef[j - 1];
    }
    return 0;
} /* bfield_ */

int StFtpcMagboltz1::f0calc_(int *iback, int *icon, double *anew, int *l)
{
    /* System generated locals */
    int i__1, i__2;


    /* Local variables */
    static int mion;
    static double asum;
    static double fmus, ssum, f1sum, f2sum;
    static int i__, j, m;
    static double s2sum, z__, f2sum1, f2sum2, fnsum, fnssm1, fnssm2, ai, 
	    sfb, f2t1, f2t2, ext, sum;
    static int min1, min2, min3, min4;




/* STARTING VALUES */


    printf("starting f0calc\n");
    if (*iback == 1) {
	goto L10;
    }
    if (*l != 1) {
	goto L10;
    }
    f0c_1.df[inpt_1.nstep + 1] = (float)0.;
    f0c_1.f[inpt_1.nstep1 - 1] = 1e-7;
L10:
    ssum = (float)0.;
    f2sum = (float)0.;
    fnssm1 = (float)0.;
    fnssm2 = (float)0.;
    z__ = (float)0.;
    i__1 = inpt_1.nstep;
    for (m = 1; m <= i__1; ++m) {
	i__ = inpt_1.nstep - m + 2;
	ai = (double) i__;
	i__2 = inpt_1.ngas;
	for (j = 1; j <= i__2; ++j) {
	    mion = mix3_1.lion[j - 1] + i__;
	    if (mion >= inpt_1.nstep1) {
		goto L60;
	    }
	    z__ = z__ + f0c_1.f[mion - 1] * mix1_1.qion[j + (mion << 2) - 5] /
		     ai + (f0c_1.f[mion] * mix1_1.qion[j + ((mion + 1) << 2) - 
		    5] - f0c_1.f[mion - 1] * mix1_1.qion[j + (mion << 2) - 5])
		     * mix3_1.alion[j - 1] / ai;
L60:
	    ;
	}

	asum = f0c_1.f[i__ - 1] * (mag_1.qef[i__ - 1] * inpt_1.alpnaz * 
		inpt_1.alpnew + mag_1.qeef[i__ - 1] * inpt_1.alpnew * 
		f0c_1.df[i__ - 1]);
	sum = z__ - f0c_1.f[i__ - 1] * mix1_1.qsum[i__ - 1] + asum;

	if (mix3_1.nin1 == 0) {
	    goto L610;
	}
	i__2 = mix3_1.nin1;
	for (j = 1; j <= i__2; ++j) {
	    min1 = mix3_1.lin1[j - 1] + i__;
	    if (min1 >= inpt_1.nstep1) {
		goto L61;
	    }
	    sum = sum + f0c_1.f[min1 - 1] * mix1_1.qin1[j + min1 * 24 - 25] + 
		    (f0c_1.f[min1] * mix1_1.qin1[j + (min1 + 1) * 24 - 25] - 
		    f0c_1.f[min1 - 1] * mix1_1.qin1[j + min1 * 24 - 25]) * 
		    mix3_1.alin1[j - 1];
L61:
	    ;
	}
L610:
	if (mix3_1.nin2 == 0) {
	    goto L620;
	}
	i__2 = mix3_1.nin2;
	for (j = 1; j <= i__2; ++j) {
	    min2 = mix3_1.lin2[j - 1] + i__;
	    if (min2 >= inpt_1.nstep1) {
		goto L62;
	    }
	    sum = sum + f0c_1.f[min2 - 1] * mix1_1.qin2[j + min2 * 24 - 25] + 
		    (f0c_1.f[min2] * mix1_1.qin2[j + (min2 + 1) * 24 - 25] - 
		    f0c_1.f[min2 - 1] * mix1_1.qin2[j + min2 * 24 - 25]) * 
		    mix3_1.alin2[j - 1];
L62:
	    ;
	}
L620:
	if (mix3_1.nin3 == 0) {
	    goto L630;
	}
	i__2 = mix3_1.nin3;
	for (j = 1; j <= i__2; ++j) {
	    min3 = mix3_1.lin3[j - 1] + i__;
	    if (min3 >= inpt_1.nstep1) {
		goto L63;
	    }
	    sum = sum + f0c_1.f[min3 - 1] * mix1_1.qin3[j + min3 * 24 - 25] + 
		    (f0c_1.f[min3] * mix1_1.qin3[j + (min3 + 1) * 24 - 25] - 
		    f0c_1.f[min3 - 1] * mix1_1.qin3[j + min3 * 24 - 25]) * 
		    mix3_1.alin3[j - 1];
L63:
	    ;
	}
L630:
	if (mix3_1.nin4 == 0) {
	    goto L640;
	}
	i__2 = mix3_1.nin4;
	for (j = 1; j <= i__2; ++j) {
	    min4 = mix3_1.lin4[j - 1] + i__;
	    if (min4 >= inpt_1.nstep1) {
		goto L64;
	    }
	    sum = sum + f0c_1.f[min4 - 1] * mix1_1.qin4[j + min4 * 24 - 25] + 
		    (f0c_1.f[min4] * mix1_1.qin4[j + (min4 + 1) * 24 - 25] - 
		    f0c_1.f[min4 - 1] * mix1_1.qin4[j + min4 * 24 - 25]) * 
		    mix3_1.alin4[j - 1];
L64:
	    ;
	}
L640:

/*   COLLISIONS OF 2ND KIND */

	if (*icon != 1) {
	    goto L90;
	}
	type2_(&s2sum, &i__, &inpt_1.nstep1);
	sum += s2sum;
L90:
	fnsum = sum - asum;
	fnsum *= inpt_1.estep;
	fnssm1 += fnsum;
	sum *= inpt_1.estep;
	ssum += sum;
	f2sum1 = inpt_1.alpnaz * (float).4 * inpt_1.alpnew * f2c_1.f2[i__ - 1]
		 * mag_1.qef[j - 1] * inpt_1.estep;
	f2sum2 = inpt_1.alpnew * (float).4 * mag_1.qeef[i__ - 1] * (f2c_1.df2[
		i__ - 1] + f2c_1.f2[i__ - 1] * (float)1.5 / mix2_1.e[i__ - 1])
		 * inpt_1.estep;
	f2sum = f2sum + f2sum1 + f2sum2;
	f2t1 = mag_1.qeeef[i__ - 1] * (float).4 * (f2c_1.df2[i__ - 1] + 
		f2c_1.f2[i__ - 1] * (float)1.5 / mix2_1.e[i__ - 1]);
	f2t2 = mag_1.qfemag[i__ - 1] * (float).4 * inpt_1.alpnaz * f2c_1.f2[
		i__ - 1];
	if (inpt_1.isfb == 1) {
	    sfb = mag_1.qeeef[i__ - 1] * f0c_1.f[i__ - 1] + ((mix1_1.qelm[i__ 
		    - 1] + mag_1.qfemag[i__ - 1] * inpt_1.alpnaz) * f0c_1.f[
		    i__ - 1] - ssum) * inpt_1.akt;
	}
	if (inpt_1.isfb == 0) {
	    sfb = mag_1.qeeef[i__ - 1] * f0c_1.f[i__ - 1] + mix1_1.qelm[i__ - 
		    1] * f0c_1.f[i__ - 1] * inpt_1.akt;
	}
	f0c_1.df[i__ - 1] = (ssum - f0c_1.f[i__ - 1] * (mix1_1.qelm[i__ - 1] 
		+ mag_1.qfemag[i__ - 1] * inpt_1.alpnaz) + f2sum - f2t1 - 
		f2t2) / sfb;

	ext = mix1_1.qelm[i__ - 1] * f0c_1.f[i__ - 1] + mix1_1.qelm[i__ - 1] *
		 f0c_1.f[i__ - 1] * f0c_1.df[i__ - 1] * inpt_1.akt;
	f1c_1.f1[i__ - 1] = (fnssm2 + ext - fnssm1) * (float)3. / (mag_1.emag 
		* mix2_1.e[i__ - 1]);
	f1sum = inpt_1.alpnew * (mix2_1.e[i__ - 1] * (float)2. * f1c_1.f1[i__ 
		- 1] - mix2_1.e[i__] * f1c_1.f1[i__]) * inpt_1.estep / (float)
		3.;
	fnssm2 += f1sum;
	if (i__ >= inpt_1.nstep1 - 2) {
	    goto L623;
	}
	f1c_1.df1[i__ - 1] = (f1c_1.f1[i__ + 1] - f1c_1.f1[i__ - 1]) / 
		inpt_1.estep - f1c_1.df1[i__ + 1];
	f1c_1.df1[i__] = (f1c_1.f1[i__ + 1] - f1c_1.f1[i__ - 1]) / (
		inpt_1.estep * (float)2.);
	goto L624;
L623:
	if (i__ == inpt_1.nstep1) {
	    f1c_1.df1[i__ - 1] = -f1c_1.f1[inpt_1.nstep1 - 1] / (inpt_1.estep 
		    * (float)2.);
	}
	if (i__ == inpt_1.nstep1 - 1) {
	    f1c_1.df1[i__ - 1] = (f1c_1.f1[inpt_1.nstep1 - 1] - f1c_1.f1[
		    inpt_1.nstep1 - 2]) / inpt_1.estep;
	}
	if (i__ == inpt_1.nstep1 - 2) {
	    f1c_1.df1[i__ - 1] = (f1c_1.f1[inpt_1.nstep1 - 2] - f1c_1.f1[
		    inpt_1.nstep1 - 3]) / inpt_1.estep;
	}

L624:
	if (*l == 1) {
	    goto L629;
	}
	f2c_1.f2[i__ - 1] = mag_1.qe[i__ - 1] * (float)-2. * (f1c_1.df1[i__ - 
		1] - f1c_1.f1[i__ - 1] / (mix2_1.e[i__ - 1] * (float)2.)) / (
		float)3. - inpt_1.alpnaz * (float)2. * f1c_1.f1[i__ - 1] / (
		mix2_1.qtot[i__ - 1] * (float)3.);
	f2c_1.df2[i__ - 1] = f0c_1.df[i__ - 1] * (float)-2.5 * f0c_1.f[i__ - 
		1] - inpt_1.alpnaz * (float)2.5 * f0c_1.f[i__ - 1] / mag_1.ef[
		i__ - 1] - f2c_1.f2[i__ - 1] * (float)1.5 / mix2_1.e[i__ - 1] 
		- inpt_1.alpnaz * f2c_1.f2[i__ - 1] / mag_1.ef[i__ - 1] - 
		f1c_1.f1[i__ - 1] * (float)2.5 / mag_1.qe[i__ - 1];
	f2c_1.df2[inpt_1.nstep1] = f2c_1.df2[inpt_1.nstep1 - 1];
	f2c_1.f2[i__ - 2] = f2c_1.f2[i__ - 1] - inpt_1.estep * (f2c_1.df2[i__ 
		- 1] * (float)1.5 - f2c_1.df2[i__] * (float).5);
	f2c_1.df2[i__ - 2] = f2c_1.df2[i__ - 1] * (float)2. - f2c_1.df2[i__];
	goto L296;
L629:
	if (*iback == 1) {
	    goto L1000;
	}


/*    BACKWARD PROLONGATION */


L296:
	f0c_1.f[i__ - 2] = f0c_1.f[i__ - 1] * exp(-(f0c_1.df[i__ - 1] * (
		float)3. - f0c_1.df[i__]) * (float).5 * inpt_1.estep);
L1000:
	;
    }
    f0c_1.df[0] = (float)0.;


/*    GAUSS-SEIDEL ITERATION */


    f0c_1.f[1] = exp((f0c_1.df[0] + f0c_1.df[1]) * (float).5 * inpt_1.estep);
    f0c_1.f[0] = f0c_1.f[1] * exp(-(f0c_1.df[1] * (float)3. - f0c_1.df[2]) * (
	    float).5 * inpt_1.estep);
    f0c_1.f[2] = exp(inpt_1.estep * (f0c_1.df[0] + f0c_1.df[1] * (float)4. + 
	    f0c_1.df[2]) / (float)3.);
    i__1 = inpt_1.nstep1;
    for (i__ = 5; i__ <= i__1; i__ += 2) {
/* L300: */
	f0c_1.f[i__ - 1] = f0c_1.f[i__ - 3] * exp(inpt_1.estep * (f0c_1.df[
		i__ - 3] + f0c_1.df[i__ - 2] * (float)4. + f0c_1.df[i__ - 1]) 
		/ (float)3.);
    }
    i__1 = inpt_1.nstep;
    for (i__ = 4; i__ <= i__1; i__ += 2) {
/* L310: */
	f0c_1.f[i__ - 1] = f0c_1.f[i__ - 3] * exp(inpt_1.estep * (f0c_1.df[
		i__ - 3] + f0c_1.df[i__ - 2] * (float)4. + f0c_1.df[i__ - 1]) 
		/ (float)3.);
    }



/*    NORMALIZATION OF PROBABILITY DISTRIBUTION */


    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L400: */
	sint_1.simf[j - 1] = f0c_1.f[j - 1] * mix2_1.eroot[j - 1];
    }
    simp_(anew);
    fmus = (float)1. / *anew;
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
	f0c_1.f[j - 1] *= fmus;
/* L410: */
	f0c_1.df0[j - 1] = f0c_1.df[j - 1] * f0c_1.f[j - 1];
    }

    printf("done with f0calc %f\n", f0c_1.f[inpt_1.nstep - 1]);
    return 0;

} /* f0calc_ */

int StFtpcMagboltz1::fncalc_(int *lmax)
{
    /* System generated locals */
    int i__1;

    /* Local variables */
    static int j;

    if (*lmax == 2) {
	goto L300;
    }
    f2c_1.f2[inpt_1.nstep1] = (float)0.;
    i__1 = inpt_1.nstep1;
    for (j = 2; j <= i__1; ++j) {
/* L100: */
	f2c_1.f2[j - 1] = -(mag_1.ef[j - 1] * (float)2. * (mix2_1.e[j - 1] * 
		f1c_1.df1[j - 1] - f1c_1.f1[j - 1] / (float)2.) / (
		mix2_1.qtot[j - 1] * (float)3. * mix2_1.e[j - 1]) + f1c_1.f1[
		j - 1] * (float)2. * inpt_1.alpnaz / (mix2_1.qtot[j - 1] * (
		float)3.));
    }
    f2c_1.f2[0] = (float)0.;
    i__1 = inpt_1.nstep;
    for (j = 2; j <= i__1; ++j) {
/* L150: */
	f2c_1.df2[j - 1] = (f2c_1.f2[j] - f2c_1.f2[j - 2]) / (inpt_1.estep * (
		float)2.);
    }
    f2c_1.df2[inpt_1.nstep1 - 1] = (f2c_1.f2[inpt_1.nstep1 - 1] - f2c_1.f2[
	    inpt_1.nstep - 1]) / inpt_1.estep;
    f2c_1.df2[0] = f2c_1.f2[1] / inpt_1.estep;
    return 0;
L300:
    f3c_1.f3[inpt_1.nstep1] = (float)0.;
    i__1 = inpt_1.nstep1;
    for (j = 2; j <= i__1; ++j) {
/* L310: */
	f3c_1.f3[j - 1] = -mag_1.qe[j - 1] * (float).6 * (f2c_1.df2[j - 1] - 
		f2c_1.f2[j - 1] / mix2_1.e[j - 1]) - inpt_1.alpnaz * (float)
		.6 * f2c_1.f2[j - 1] / mix2_1.qtot[j - 1];
    }
    f3c_1.f3[0] = (float)0.;
    i__1 = inpt_1.nstep;
    for (j = 2; j <= i__1; ++j) {
/* L350: */
	f3c_1.df3[j - 1] = (f3c_1.f3[j] - f3c_1.f3[j - 2]) / (inpt_1.estep * (
		float)2.);
    }
    f3c_1.df3[inpt_1.nstep1 - 1] = (f3c_1.f3[inpt_1.nstep1 - 1] - f3c_1.f3[
	    inpt_1.nstep - 1]) / inpt_1.estep;
    f3c_1.df3[0] = f3c_1.f3[1] / inpt_1.estep;
    return 0;
} /* fncalc_ */

int StFtpcMagboltz1::g0calc_(int *icon, double *gfinal, double *eg0sum, int *lmax)
{
    /* System generated locals */
    int i__1, i__2;

    /* Local variables */
    static double anew;
    static int mion;
    static double asum, bsum, ssum, g1sum;
    static int i__, j;
    static double s2sum;
    static int m;
    static double z__, gssum, gnsum, ai;
    static double gnssum, vel, ext, sum;
    static int min1, min2, min3, min4;
    static double sum1, sum2, sum3, sum4;

    inpt_1.alpnew = (float)0.;
    inpt_1.alpnaz = (float)0.;
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L10: */
	sint_1.simf[j - 1] = mix2_1.e[j - 1] * f1c_1.f1[j - 1];
    }
    simp_(&sum);
    vel = sum * mag_1.eovm / (float)3.;

/* STARTING VALUES */

    g0c_1.g[inpt_1.nstep1 - 1] = *gfinal;
    g2c_1.g2[inpt_1.nstep1 - 1] = (float)0.;
    g2c_1.dg2[inpt_1.nstep1 - 1] = (float)0.;
    ssum = (float)0.;
    sum4 = (float)0.;
    gnssum = (float)0.;
    gssum = (float)0.;
    z__ = (float)0.;
    i__1 = inpt_1.nstep;
    for (m = 1; m <= i__1; ++m) {
	i__ = inpt_1.nstep - m + 2;
	ai = (double) i__;
	i__2 = inpt_1.ngas;
	for (j = 1; j <= i__2; ++j) {
	    mion = mix3_1.lion[j - 1] + i__;
	    if (mion >= inpt_1.nstep1) {
		goto L60;
	    }
	    z__ = z__ + g0c_1.g[mion - 1] * mix1_1.qion[j + (mion << 2) - 5] /
		     ai + (g0c_1.g[mion] * mix1_1.qion[j + ((mion + 1) << 2) - 
		    5] - g0c_1.g[mion - 1] * mix1_1.qion[j + (mion << 2) - 5])
		     * mix3_1.alion[j - 1] / ai;
L60:
	    ;
	}

	asum = g0c_1.g[i__ - 1] * (mag_1.qef[i__ - 1] * inpt_1.alpnaz * 
		inpt_1.alpnew + mag_1.qeef[i__ - 1] * inpt_1.alpnew * 
		g0c_1.dg[i__ - 1]);
	sum = z__ - g0c_1.g[i__ - 1] * mix1_1.qsum[i__ - 1] + asum;
	if (*lmax == 1) {
	    goto L601;
	}
	bsum = inpt_1.alpnew * (inpt_1.alpnaz * mag_1.qef[i__ - 1] * g2c_1.g2[
		i__ - 1] + mag_1.qeef[i__ - 1] * (g2c_1.dg2[i__ - 1] + 
		g2c_1.g2[i__ - 1] * (float)1.5 / mix2_1.e[i__ - 1]));
	sum += bsum * (float).4;

L601:
	if (mix3_1.nin1 == 0) {
	    goto L610;
	}
	i__2 = mix3_1.nin1;
	for (j = 1; j <= i__2; ++j) {
	    min1 = mix3_1.lin1[j - 1] + i__;
	    if (min1 >= inpt_1.nstep1) {
		goto L61;
	    }
	    sum = sum + g0c_1.g[min1 - 1] * mix1_1.qin1[j + min1 * 24 - 25] + 
		    (g0c_1.g[min1] * mix1_1.qin1[j + (min1 + 1) * 24 - 25] - 
		    g0c_1.g[min1 - 1] * mix1_1.qin1[j + min1 * 24 - 25]) * 
		    mix3_1.alin1[j - 1];
L61:
	    ;
	}
L610:
	if (mix3_1.nin2 == 0) {
	    goto L620;
	}
	i__2 = mix3_1.nin2;
	for (j = 1; j <= i__2; ++j) {
	    min2 = mix3_1.lin2[j - 1] + i__;
	    if (min2 >= inpt_1.nstep1) {
		goto L62;
	    }
	    sum = sum + g0c_1.g[min2 - 1] * mix1_1.qin2[j + min2 * 24 - 25] + 
		    (g0c_1.g[min2] * mix1_1.qin2[j + (min2 + 1) * 24 - 25] - 
		    g0c_1.g[min2 - 1] * mix1_1.qin2[j + min2 * 24 - 25]) * 
		    mix3_1.alin2[j - 1];
L62:
	    ;
	}
L620:
	if (mix3_1.nin3 == 0) {
	    goto L630;
	}
	i__2 = mix3_1.nin3;
	for (j = 1; j <= i__2; ++j) {
	    min3 = mix3_1.lin3[j - 1] + i__;
	    if (min3 >= inpt_1.nstep1) {
		goto L63;
	    }
	    sum = sum + g0c_1.g[min3 - 1] * mix1_1.qin3[j + min3 * 24 - 25] + 
		    (g0c_1.g[min3] * mix1_1.qin3[j + (min3 + 1) * 24 - 25] - 
		    g0c_1.g[min3 - 1] * mix1_1.qin3[j + min3 * 24 - 25]) * 
		    mix3_1.alin3[j - 1];
L63:
	    ;
	}
L630:
	if (mix3_1.nin4 == 0) {
	    goto L640;
	}
	i__2 = mix3_1.nin4;
	for (j = 1; j <= i__2; ++j) {
	    min4 = mix3_1.lin4[j - 1] + i__;
	    if (min4 >= inpt_1.nstep1) {
		goto L64;
	    }
	    sum = sum + g0c_1.g[min4 - 1] * mix1_1.qin4[j + min4 * 24 - 25] + 
		    (g0c_1.g[min4] * mix1_1.qin4[j + (min4 + 1) * 24 - 25] - 
		    g0c_1.g[min4 - 1] * mix1_1.qin4[j + min4 * 24 - 25]) * 
		    mix3_1.alin4[j - 1];
L64:
	    ;
	}
L640:
	sum1 = -mag_1.qef[i__ - 1] * inpt_1.alpnew * (f0c_1.f[i__ - 1] + 
		f2c_1.f2[i__ - 1] * (float).4) + inpt_1.alpnew * mix2_1.eroot[
		i__ - 1] * vel * f1c_1.f1[i__ - 1] / (mag_1.eovm * (float)3. *
		 mix2_1.qtot[i__ - 1]) + mix2_1.e[i__ - 1] * f1c_1.f1[i__ - 1]
		 / (float)3. - mix2_1.eroot[i__ - 1] * vel * f0c_1.f[i__ - 1] 
		/ mag_1.eovm;
	gnsum = sum - asum;
	if (*lmax == 1) {
	    goto L641;
	}
	gnsum -= bsum * (float).4;
L641:
	gnsum = gnsum + mix2_1.e[i__ - 1] * f1c_1.f1[i__ - 1] / (float)3. - 
		mix2_1.eroot[i__ - 1] * vel * f0c_1.f[i__ - 1] / mag_1.eovm;
	sum += sum1;

/*   COLLISIONS OF 2ND KIND */

	if (*icon != 1) {
	    goto L90;
	}
	type2g_(&s2sum, &i__, &inpt_1.nstep1);
	gnsum += s2sum;
	sum += s2sum;
L90:
	sum *= inpt_1.estep;
	gnsum *= inpt_1.estep;
	gnssum += gnsum;
	ssum += sum;
	sum2 = mag_1.qfemag[i__ - 1] * (f0c_1.f[i__ - 1] + f2c_1.f2[i__ - 1] *
		 (float).4 - vel * f1c_1.f1[i__ - 1] / (mag_1.eovm * 
		mix2_1.eroot[i__ - 1]));
	sum3 = -g0c_1.g[i__ - 1] * (mix1_1.qelm[i__ - 1] + mag_1.qfemag[i__ - 
		1] * inpt_1.alpnaz);
	if (*lmax == 1) {
	    goto L901;
	}
	sum4 = (g2c_1.g2[i__ - 1] * inpt_1.alpnaz * mag_1.qfemag[i__ - 1] + 
		mag_1.qeeef[i__ - 1] * (g2c_1.dg2[i__ - 1] + g2c_1.g2[i__ - 1]
		 * (float)1.5 / mix2_1.e[i__ - 1])) * (float).4;
L901:
	g0c_1.dg0[i__ - 1] = (ssum + sum2 + sum3 - sum4) / (mag_1.qeeef[i__ - 
		1] + mix1_1.qelm[i__ - 1] * inpt_1.akt);

	ext = g0c_1.g[i__ - 1] * mix1_1.qelm[i__ - 1] + mix1_1.qelm[i__ - 1] *
		 g0c_1.dg0[i__ - 1] * inpt_1.akt;
	g1c_1.g1[i__ - 1] = (ext + gssum - gnssum) * (float)3. / (mag_1.emag *
		 mix2_1.e[i__ - 1]);
	g1sum = inpt_1.alpnew * (mix2_1.e[i__ - 1] * (float)2. * g1c_1.g1[i__ 
		- 1] - mix2_1.e[i__] * g1c_1.g1[i__]) * inpt_1.estep / (float)
		3.;
	gssum += g1sum;
	if (i__ >= inpt_1.nstep1 - 2) {
	    goto L623;
	}
	g1c_1.dg1[i__ - 1] = (g1c_1.g1[i__ + 1] - g1c_1.g1[i__ - 1]) / 
		inpt_1.estep - g1c_1.dg1[i__ + 1];
	g1c_1.dg1[i__] = (g1c_1.g1[i__ + 1] - g1c_1.g1[i__ - 1]) / (
		inpt_1.estep * (float)2.);
	goto L624;
L623:
	g1c_1.dg1[i__ - 1] = (g1c_1.g1[i__] - g1c_1.g1[i__ - 1]) / 
		inpt_1.estep;
	if (i__ == inpt_1.nstep1) {
	    g1c_1.dg1[i__ - 1] = -g1c_1.g1[i__ - 1] / (inpt_1.estep * (float)
		    2.);
	}

L624:
	if (*lmax == 1) {
	    goto L903;
	}

/* PN, 07.10.99: make continuation readable: */
	g2c_1.g2[i__ - 1] = f1c_1.f1[i__ - 1] * (float)2. / (float)3. - vel * 
		f2c_1.f2[i__ - 1] / (mix2_1.eroot[i__ - 1] * mag_1.eovm) - 
		mag_1.ef[i__ - 1] * (float)2. * (g1c_1.dg1[i__ - 1] - 
		g1c_1.g1[i__ - 1] / (mix2_1.e[i__ - 1] * (float)2.)) / (float)
		3. - inpt_1.alpnaz * (float)2. * g1c_1.g1[i__ - 1] / (float)
		3. + f3c_1.f3[i__ - 1] * (float)3. / (float)7.;
	g2c_1.g2[i__ - 1] /= mix2_1.qtot[i__ - 1];
	g2c_1.dg2[i__ - 1] = g0c_1.dg0[i__ - 1] * (float)-2.5 - g2c_1.g2[i__ 
		- 1] * (float)1.5 / mix2_1.e[i__ - 1] - inpt_1.alpnaz * (
		float)2.5 * (g0c_1.g[i__ - 1] + g2c_1.g2[i__ - 1] * (float).4)
		 / mag_1.ef[i__ - 1] - g1c_1.g1[i__ - 1] * (float)2.5 / 
		mag_1.qe[i__ - 1] + (f0c_1.f[i__ - 1] + f2c_1.f2[i__ - 1] * (
		float).4) * (float)2.5 / mag_1.ef[i__ - 1] - vel * (float)2.5 
		* f1c_1.f1[i__ - 1] / (mix2_1.eroot[i__ - 1] * mag_1.eovm * 
		mag_1.ef[i__ - 1]);
	g2c_1.dg2[inpt_1.nstep1] = g2c_1.dg2[inpt_1.nstep1 - 1];
	g2c_1.g2[i__ - 2] = g2c_1.g2[i__ - 1] - inpt_1.estep * (g2c_1.dg2[i__ 
		- 1] * (float)1.5 - g2c_1.dg2[i__] * (float).5);
	g2c_1.dg2[i__ - 2] = g2c_1.dg2[i__ - 1] * (float)2. - g2c_1.dg2[i__];
L903:
	g0c_1.g[i__ - 2] = g0c_1.g[i__ - 1] - inpt_1.estep * (g0c_1.dg0[i__ - 
		1] * (float)1.5 - g0c_1.dg0[i__] * (float).5);
	g0c_1.dg[i__ - 1] = g0c_1.dg0[i__ - 1] / g0c_1.g[i__ - 1];
/* L1000: */
    }
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L400: */
	sint_1.simf[j - 1] = g0c_1.g[j - 1] * mix2_1.eroot[j - 1];
    }
    simp_(&anew);
    *eg0sum = anew;
    return 0;

} /* g0calc_ */

/* Argon gas */
int StFtpcMagboltz1::gas1_(double *q, double *qin, double *q2ro, int *nin,
int *n2ro, double *e, double *ein, double *e2ro, char *name__, 
	double *virial, int *monte, int name_len)
{
    /* Initialized data */

    static double xen[34] = { 1.,1.2,1.5,1.7,2.,2.5,3.,4.,4.9,5.,6.,6.67,
	    7.,8.,8.71,9.,10.,11.,12.,13.,13.6,14.,15.,16.,16.5,18.,20.,30.,
	    30.6,50.,54.4,100.,400.,1e3 };
    static double yxsec[34] = { 1.39,1.66,2.05,2.33,2.7,3.43,4.15,5.65,
	    7.26,7.46,9.32,10.6,11.3,13.1,14.1,14.4,15.4,15.8,15.8,15.4,15.1,
	    14.8,14.1,13.2,13.,11.4,10.2,6.13,6.01,4.17,3.97,2.71,1.3,1. };
    static double xeni[54] = { 15.7,16.,16.5,17.,17.5,18.,18.5,19.,19.5,
	    20.,20.5,21.,21.5,22.,22.5,23.,23.5,24.,24.5,25.,25.5,26.,28.,30.,
	    32.,34.,36.,38.,40.,50.,60.,70.,80.,90.,100.,110.,120.,130.,140.,
	    150.,160.,180.,200.,250.,300.,350.,400.,450.,500.,600.,700.,800.,
	    900.,1e3 };
    static double yxeni[54] = { -.2,.306,.825,1.126,1.326,1.468,1.577,
	    1.663,1.737,1.797,1.853,1.896,1.933,1.97,1.997,2.024,2.048,2.071,
	    2.094,2.115,2.132,2.148,2.204,2.256,2.293,2.325,2.351,2.368,2.379,
	    2.404,2.424,2.443,2.454,2.456,2.455,2.452,2.448,2.441,2.436,2.429,
	    2.419,2.401,2.379,2.337,2.296,2.258,2.225,2.19,2.164,2.115,2.065,
	    2.027,1.994,1.961 };
    static double xin[15] = { 11.55,13.,13.2,13.4,14.,16.,20.,30.,40.,50.,
	    80.,100.,200.,500.,1e3 };
    static double yxsin[15] = { 0.,.057,.075,.072,.096,.17,.18,.21,.24,
	    .28,.22,.17,.11,.06,.04 };
    static double yxpin[15] = { 0.,0.,.01,.03,.06,.17,.35,.45,.44,.41,.3,
	    .27,.18,.09,.05 };
    static double yxdin[15] = { 0.,0.,0.,0.,0.,.05,.11,.22,.26,.28,.35,
	    .35,.26,.13,.08 };

    /* System generated locals */
    int i__1, i__2;
    double d__1;

    /* Local variables */
    static double apol;
    static int lmax;
    static double sumi, a, b;
    static int i__, j, ndata;
    static double e1, aa, dd, ff, ak, en;
    static int nidata, nxdata;
    static double an0, an1, an2, api, sum;

    /* Parameter adjustments */
    --e2ro;
    --ein;
    --e;
    q2ro -= 9;
    qin -= 25;
    q -= 7;

    /* Function Body */
    sprintf(name__, " ARGON  1988   ");
/* ---------------------------------------------------------------- */
/*  MULTI-TERM CROSS-SECTION. */
/*  FOR PURE ARGON: */
/*  ACCURACY OF DERIVED VELOCITY AND DIFFUSION COEFFICIENTS 0.5% BELOW */
/*  3000VOLTS . BELOW 20000VOLTS ACCURACY 1.0%. IONISATION COEFFICIENT */
/*  AND DRIFT VELOCITY ACCURACY BETTER THAN 10% BELOW 500,000VOLTS. */
/* ----------------------------------------------------------------- */

/*  PARAMETERS OF PHASE SHIFT ANALYSIS. */

    apol = (float)11.08;
    lmax = 100;
    aa = (float)-1.488;
    dd = (float)65.4;
    ff = (float)-84.3;
    e1 = (float).883;
    api = (float)3.141592654;

    *nin = 3;
    *n2ro = 0;
    e[1] = (float)0.;
    e[2] = cnsts_1.emass * (float)2. / (cnsts_1.amu * (float)39.948);
    e[3] = (float)15.7;
    e[4] = (float)0.;
    e[5] = (float)0.;
    e[6] = (float)0.;
    ein[1] = (float)11.55;
    ein[2] = (float)13.;
    ein[3] = (float)14.;
    en = -inpt_1.estep;
    if (*monte == 1) {
	en = -inpt_1.estep / (float)2.;
    }
    i__1 = inpt_1.nstep1 + 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	en += inpt_1.estep;
	if (en > (float)1.) {
	    goto L100;
	}
	if (en == (float)0.) {
	    q[i__ * 6 + 2] = (float)7.79e-16;
	}
	if (en == (float)0.) {
	    goto L200;
	}
	ak = ::sqrt(en / inpt_1.ary);
	an0 = -aa * ak * (apol * (float)4. / (float)3. * ak * ak * ::log(ak) + (
		float)1.) - api * apol / (float)3. * ak * ak + dd * ak * ak * 
		ak + ff * ak * ak * ak * ak;
	an1 = api / (float)15. * apol * ak * ak * ((float)1. - ::sqrt(en / e1));
	an2 = api * apol * ak * ak / (float)105.;
	an0 = atan(an0);
	an1 = atan(an1);
	an2 = atan(an2);
/* Computing 2nd power */
	d__1 = sin(an0 - an1);
	sum = d__1 * d__1;
/* Computing 2nd power */
	d__1 = sin(an1 - an2);
	sum += d__1 * d__1 * (float)2.;
	i__2 = lmax - 1;
	for (j = 2; j <= i__2; ++j) {
	    sumi = (float)6. / ((j * (float)2. + (float)5.) * (j * (float)2. 
		    + (float)3.) * (j * (float)2. + (float)1.) * (j * (float)
		    2. - (float)1.));
/* Computing 2nd power */
	    d__1 = sin(atan(api * apol * ak * ak * sumi));
	    sum += (j + (float)1.) * (d__1 * d__1);
/* L10: */
	}
	q[i__ * 6 + 2] = sum * (float)4. * cnsts_1.pir2 / (ak * ak);
	goto L200;
L100:
	ndata = 34;
	i__2 = ndata;
	for (j = 2; j <= i__2; ++j) {
	    if (en <= xen[j - 1]) {
		goto L160;
	    }
/* L150: */
	}
	j = ndata;
L160:
	a = (yxsec[j - 1] - yxsec[j - 2]) / (xen[j - 1] - xen[j - 2]);
	b = (xen[j - 2] * yxsec[j - 1] - xen[j - 1] * yxsec[j - 2]) / (xen[j 
		- 2] - xen[j - 1]);
	q[i__ * 6 + 2] = (a * en + b) * (float)1e-16;
L200:
	q[i__ * 6 + 3] = (float)0.;
	if (en <= e[3]) {
	    goto L230;
	}
	nidata = 54;
	i__2 = nidata;
	for (j = 2; j <= i__2; ++j) {
	    if (en <= xeni[j - 1]) {
		goto L220;
	    }
/* L210: */
	}
	j = nidata;
L220:
	a = (yxeni[j - 1] - yxeni[j - 2]) / (xeni[j - 1] - xeni[j - 2]);
	b = (xeni[j - 2] * yxeni[j - 1] - xeni[j - 1] * yxeni[j - 2]) / (xeni[
		j - 2] - xeni[j - 1]);
	d__1 = a * en + b;
	q[i__ * 6 + 3] = ::pow(c_b12, d__1) * (float)1e-18;
L230:
	q[i__ * 6 + 4] = (float)0.;
	q[i__ * 6 + 5] = (float)0.;
	q[i__ * 6 + 6] = (float)0.;

	qin[i__ * 24 + 1] = (float)0.;
	qin[i__ * 24 + 2] = (float)0.;
	qin[i__ * 24 + 3] = (float)0.;
	if (en <= ein[1]) {
	    goto L400;
	}
	nxdata = 15;
	i__2 = nxdata;
	for (j = 2; j <= i__2; ++j) {
	    if (en <= xin[j - 1]) {
		goto L320;
	    }
/* L310: */
	}
	j = nxdata;
L320:
	a = (yxsin[j - 1] - yxsin[j - 2]) / (xin[j - 1] - xin[j - 2]);
	b = (xin[j - 2] * yxsin[j - 1] - xin[j - 1] * yxsin[j - 2]) / (xin[j 
		- 2] - xin[j - 1]);
	qin[i__ * 24 + 1] = (a * en + b) * (float)1e-16;
	if (en <= ein[2]) {
	    goto L400;
	}
	a = (yxpin[j - 1] - yxpin[j - 2]) / (xin[j - 1] - xin[j - 2]);
	b = (xin[j - 2] * yxpin[j - 1] - xin[j - 1] * yxpin[j - 2]) / (xin[j 
		- 2] - xin[j - 1]);
	qin[i__ * 24 + 2] = (a * en + b) * (float)1e-16;
	if (en <= ein[3]) {
	    goto L400;
	}
	a = (yxdin[j - 1] - yxdin[j - 2]) / (xin[j - 1] - xin[j - 2]);
	b = (xin[j - 2] * yxdin[j - 1] - xin[j - 1] * yxdin[j - 2]) / (xin[j 
		- 2] - xin[j - 1]);
	qin[i__ * 24 + 3] = (a * en + b) * (float)1e-16;
L400:
	q[i__ * 6 + 1] = q[i__ * 6 + 2] + q[i__ * 6 + 3] + qin[i__ * 24 + 1] 
		+ qin[i__ * 24 + 2] + qin[i__ * 24 + 3];
/* L900: */
    }
/* SAVE COMPUTE TIME */
    if (inpt_1.efinal <= ein[3]) {
	*nin = 2;
    }
    if (inpt_1.efinal <= ein[2]) {
	*nin = 1;
    }
    if (inpt_1.efinal <= ein[1]) {
	*nin = 0;
    }

    return 0;
} /* gas1_ */


/*  CO2 */
int StFtpcMagboltz1::gas2_(double *q, double *qin, double *q2ro, int *nin,
int *n2ro, double *e, double *ein, double *e2ro, char *name__, 
	double *virial, int *monte, int name_len)
{
    /* Initialized data */

    static double xmom[54] = { 0.,.001,.002,.003,.005,.007,.0085,.01,.015,
	    .02,.03,.04,.05,.07,.1,.12,.15,.17,.2,.25,.3,.35,.4,.5,.7,1.,1.2,
	    1.3,1.5,1.7,1.9,2.1,2.2,2.5,2.8,3.,3.3,3.6,4.,4.5,5.,6.,7.,8.,10.,
	    12.,15.,17.,20.,25.,30.,50.,75.,100. };
    static double yvib4[24] = { 0.,0.,.95,1.7,1.85,2.,2.15,2.2,2.15,2.,
	    1.85,1.55,1.23,1.,.76,.64,.49,.44,.41,.48,.26,.135,.1,0. };
    static double xvib5[12] = { 0.,.339,1.5,1.95,2.5,3.,3.56,4.1,4.5,5.06,
	    6.,100. };
    static double yvib5[12] = { 0.,0.,0.,.07,.2,.41,.66,.34,.155,0.,0.,0. 
	    };
    static double xvib6[12] = { 0.,.422,1.5,1.95,2.5,3.,3.56,4.1,4.5,5.06,
	    6.,100. };
    static double yvib6[12] = { 0.,0.,0.,0.,0.,.105,.225,.1,0.,0.,0.,0. };
    static double xvib7[12] = { 0.,.505,1.5,1.95,2.5,3.,3.56,4.1,4.5,5.06,
	    6.,100. };
    static double yvib7[12] = { 0.,0.,0.,0.,0.,.156,.33,.156,0.,0.,0.,0. }
	    ;
    static double xexc1[7] = { 0.,2.5,3.,3.6,4.1,4.5,100. };
    static double yexc1[7] = { 0.,0.,.18,.25,.18,0.,0. };
    static double xatt[12] = { 0.,3.85,4.3,4.5,5.1,6.6,7.2,8.2,8.4,8.9,
	    9.7,100. };
    static double ymom[54] = { 600.,540.,380.,307.,237.,200.,182.,170.,
	    138.,120.,97.,85.,76.,63.,50.,44.,39.,34.,29.,24.,18.,15.,13.,10.,
	    7.1,5.2,4.8,4.7,4.65,4.65,4.85,5.05,5.2,6.4,7.6,9.,11.5,14.,15.2,
	    14.8,12.7,10.,10.,10.8,12.1,13.1,14.4,15.,15.8,16.,15.8,12.6,9.5,
	    8. };
    static double yatt[12] = { 0.,0.,.0014,.0014,0.,0.,7e-4,.0045,.0042,
	    .001,0.,0. };
    static double xexc2[6] = { 0.,7.,8.,8.5,11.,100. };
    static double yexc2[6] = { 0.,0.,.6,.6,0.,0. };
    static double xexc3[10] = { 0.,10.5,12.,12.7,13.5,15.,17.,20.,40.,
	    100. };
    static double yexc3[10] = { 0.,0.,.69,.73,.78,.88,1.04,1.24,3.6,6.3 };
    static double xion[12] = { 0.,13.3,14.5,15.,16.,18.,20.,30.,40.,50.,
	    70.,100. };
    static double yion[12] = { 0.,0.,.06,.104,.188,.359,.532,1.63,2.28,
	    2.79,3.43,3.79 };
    static double xvib1[38] = { 0.,.083,.0844,.0862,.0932,.1035,.1208,
	    .1382,.1726,.207,.275,.345,.5,.7,.9,1.1,1.4,1.6,1.8,2.3,2.6,3.,
	    3.2,3.4,3.6,3.8,4.,4.2,4.6,5.1,5.5,6.,7.,8.,10.,20.,50.,100. };
    static double yvib1[38] = { 0.,0.,.85,1.16,1.85,2.3,2.6,2.68,2.54,2.2,
	    1.72,1.43,1.08,.8,.66,.57,.45,.42,.44,.7,.93,1.34,1.58,1.75,1.8,
	    1.79,1.7,1.52,1.05,.57,.51,.5,.48,.45,.2,0.,0.,0. };
    static double xvib2[28] = { 0.,.167,.172,.18,.2,.25,.5,1.,1.5,1.9,2.,
	    2.25,2.5,3.,3.2,3.4,3.55,3.7,3.9,4.1,4.5,4.9,5.2,6.,8.,10.,20.,
	    100. };
    static double yvib2[28] = { 0.,0.,.3,.33,.35,.325,.117,.05,.04,.06,
	    .08,.2,.4,1.28,1.57,1.77,1.78,1.75,1.6,1.28,.88,.39,.33,.27,.25,
	    .21,0.,0. };
    static double xvib3[12] = { 0.,.252,1.5,1.95,2.5,3.,3.56,4.1,4.5,5.06,
	    6.,100. };
    static double yvib3[12] = { 0.,0.,0.,0.,0.,.32,.54,.34,.16,.044,0.,0. 
	    };
    static double xvib4[24] = { 0.,.291,.3,.31,.32,.33,.35,.38,.4,.45,.5,
	    .6,.8,1.,1.5,2.,3.,4.5,6.,8.,10.,25.,30.,100. };

    /* System generated locals */
    int i__1, i__2;

    /* Local variables */
    static int nion, nmom, natt, nexc1, nvib1, nvib2, nvib3, nvib4, nvib5,
	     nvib6, nvib7, nexc2, nexc3, i__, j;
    static double a, b, en;

    /* Parameter adjustments */
    --e2ro;
    --ein;
    --e;
    q2ro -= 9;
    qin -= 25;
    q -= 7;

    /* Function Body */

    sprintf(name__, "C02 TEST  # 2  ");
    *nin = 10;
    *n2ro = 0;
    nmom = 54;
    nvib1 = 38;
    nvib2 = 28;
    nvib3 = 12;
    nvib4 = 24;
    nvib5 = 12;
    nvib6 = 12;
    nvib7 = 12;
    nexc1 = 7;
    natt = 12;
    nexc2 = 6;
    nexc3 = 10;
    nion = 12;
    e[1] = (float)0.;
    e[2] = cnsts_1.emass * (float)2. / (cnsts_1.amu * (float)44.0098);
    e[3] = (float)13.3;
    e[4] = (float)3.85;
    e[5] = (float)0.;
    e[6] = (float)0.;
    ein[1] = (float).083;
    ein[2] = (float).167;
    ein[3] = (float).252;
    ein[4] = (float).291;
    ein[5] = (float).339;
    ein[6] = (float).422;
    ein[7] = (float).505;
    ein[8] = (float)2.5;
    ein[9] = (float)7.;
    ein[10] = (float)10.5;
    en = -inpt_1.estep;
    if (*monte == 1) {
	en = -inpt_1.estep / (float)2.;
    }
    i__1 = inpt_1.nstep1 + 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	en += inpt_1.estep;

	i__2 = nmom;
	for (j = 2; j <= i__2; ++j) {
	    if (en <= xmom[j - 1]) {
		goto L150;
	    }
/* L100: */
	}
	j = nmom;
L150:
	a = (ymom[j - 1] - ymom[j - 2]) / (xmom[j - 1] - xmom[j - 2]);
	b = (xmom[j - 2] * ymom[j - 1] - xmom[j - 1] * ymom[j - 2]) / (xmom[j 
		- 2] - xmom[j - 1]);
	q[i__ * 6 + 2] = (a * en + b) * (float)1e-16;

	qin[i__ * 24 + 1] = (float)0.;
	if (en <= ein[1]) {
	    goto L260;
	}
	i__2 = nvib1;
	for (j = 2; j <= i__2; ++j) {
	    if (en <= xvib1[j - 1]) {
		goto L250;
	    }
/* L200: */
	}
	j = nvib1;
L250:
	a = (yvib1[j - 1] - yvib1[j - 2]) / (xvib1[j - 1] - xvib1[j - 2]);
	b = (xvib1[j - 2] * yvib1[j - 1] - xvib1[j - 1] * yvib1[j - 2]) / (
		xvib1[j - 2] - xvib1[j - 1]);
	qin[i__ * 24 + 1] = (a * en + b) * (float)1e-16;

L260:
	qin[i__ * 24 + 2] = (float)0.;
	if (en <= ein[2]) {
	    goto L360;
	}
	i__2 = nvib2;
	for (j = 2; j <= i__2; ++j) {
	    if (en <= xvib2[j - 1]) {
		goto L350;
	    }
/* L300: */
	}
	j = nvib2;
L350:
	a = (yvib2[j - 1] - yvib2[j - 2]) / (xvib2[j - 1] - xvib2[j - 2]);
	b = (xvib2[j - 2] * yvib2[j - 1] - xvib2[j - 1] * yvib2[j - 2]) / (
		xvib2[j - 2] - xvib2[j - 1]);
	qin[i__ * 24 + 2] = (a * en + b) * (float)1e-16;

L360:
	qin[i__ * 24 + 3] = (float)0.;
	if (en <= ein[3]) {
	    goto L460;
	}
	i__2 = nvib3;
	for (j = 2; j <= i__2; ++j) {
	    if (en <= xvib3[j - 1]) {
		goto L450;
	    }
/* L400: */
	}
	j = nvib3;
L450:
	a = (yvib3[j - 1] - yvib3[j - 2]) / (xvib3[j - 1] - xvib3[j - 2]);
	b = (xvib3[j - 2] * yvib3[j - 1] - xvib3[j - 1] * yvib3[j - 2]) / (
		xvib3[j - 2] - xvib3[j - 1]);
	qin[i__ * 24 + 3] = (a * en + b) * (float)1e-16;

L460:
	qin[i__ * 24 + 4] = (float)0.;
	if (en <= ein[4]) {
	    goto L560;
	}
	i__2 = nvib4;
	for (j = 2; j <= i__2; ++j) {
	    if (en <= xvib4[j - 1]) {
		goto L550;
	    }
/* L500: */
	}
	j = nvib4;
L550:
	a = (yvib4[j - 1] - yvib4[j - 2]) / (xvib4[j - 1] - xvib4[j - 2]);
	b = (xvib4[j - 2] * yvib4[j - 1] - xvib4[j - 1] * yvib4[j - 2]) / (
		xvib4[j - 2] - xvib4[j - 1]);
	qin[i__ * 24 + 4] = (a * en + b) * (float)1e-16;

L560:
	qin[i__ * 24 + 5] = (float)0.;
	if (en <= ein[5]) {
	    goto L660;
	}
	i__2 = nvib5;
	for (j = 2; j <= i__2; ++j) {
	    if (en <= xvib5[j - 1]) {
		goto L650;
	    }
/* L600: */
	}
	j = nvib5;
L650:
	a = (yvib5[j - 1] - yvib5[j - 2]) / (xvib5[j - 1] - xvib5[j - 2]);
	b = (xvib5[j - 2] * yvib5[j - 1] - xvib5[j - 1] * yvib5[j - 2]) / (
		xvib5[j - 2] - xvib5[j - 1]);
	qin[i__ * 24 + 5] = (a * en + b) * (float)1e-16;

L660:
	qin[i__ * 24 + 6] = (float)0.;
	if (en <= ein[6]) {
	    goto L760;
	}
	i__2 = nvib6;
	for (j = 2; j <= i__2; ++j) {
	    if (en <= xvib6[j - 1]) {
		goto L750;
	    }
/* L700: */
	}
	j = nvib6;
L750:
	a = (yvib6[j - 1] - yvib6[j - 2]) / (xvib6[j - 1] - xvib6[j - 2]);
	b = (xvib6[j - 2] * yvib6[j - 1] - xvib6[j - 1] * yvib6[j - 2]) / (
		xvib6[j - 2] - xvib6[j - 1]);
	qin[i__ * 24 + 6] = (a * en + b) * (float)1e-16;

L760:
	qin[i__ * 24 + 7] = (float)0.;
	if (en <= ein[7]) {
	    goto L860;
	}
	i__2 = nvib7;
	for (j = 2; j <= i__2; ++j) {
	    if (en <= xvib7[j - 1]) {
		goto L850;
	    }
/* L800: */
	}
	j = nvib7;
L850:
	a = (yvib7[j - 1] - yvib7[j - 2]) / (xvib7[j - 1] - xvib7[j - 2]);
	b = (xvib7[j - 2] * yvib7[j - 1] - xvib7[j - 1] * yvib7[j - 2]) / (
		xvib7[j - 2] - xvib7[j - 1]);
	qin[i__ * 24 + 7] = (a * en + b) * (float)1e-16;

L860:
	qin[i__ * 24 + 8] = (float)0.;
	if (en <= ein[8]) {
	    goto L960;
	}
	i__2 = nexc1;
	for (j = 2; j <= i__2; ++j) {
	    if (en <= xexc1[j - 1]) {
		goto L950;
	    }
/* L900: */
	}
	j = nexc1;
L950:
	a = (yexc1[j - 1] - yexc1[j - 2]) / (xexc1[j - 1] - xexc1[j - 2]);
	b = (xexc1[j - 2] * yexc1[j - 1] - xexc1[j - 1] * yexc1[j - 2]) / (
		xexc1[j - 2] - xexc1[j - 1]);
	qin[i__ * 24 + 8] = (a * en + b) * (float)1e-16;

L960:
	q[i__ * 6 + 4] = (float)0.;
	if (en <= e[4]) {
	    goto L1060;
	}
	i__2 = natt;
	for (j = 2; j <= i__2; ++j) {
	    if (en <= xatt[j - 1]) {
		goto L1050;
	    }
/* L1000: */
	}
	j = natt;
L1050:
	a = (yatt[j - 1] - yatt[j - 2]) / (xatt[j - 1] - xatt[j - 2]);
	b = (xatt[j - 2] * yatt[j - 1] - xatt[j - 1] * yatt[j - 2]) / (xatt[j 
		- 2] - xatt[j - 1]);
	q[i__ * 6 + 4] = (a * en + b) * (float)1e-16;

L1060:
	qin[i__ * 24 + 9] = (float)0.;
	if (en <= ein[9]) {
	    goto L1160;
	}
	i__2 = nexc2;
	for (j = 2; j <= i__2; ++j) {
	    if (en <= xexc2[j - 1]) {
		goto L1150;
	    }
/* L1100: */
	}
	j = nexc2;
L1150:
	a = (yexc2[j - 1] - yexc2[j - 2]) / (xexc2[j - 1] - xexc2[j - 2]);
	b = (xexc2[j - 2] * yexc2[j - 1] - xexc2[j - 1] * yexc2[j - 2]) / (
		xexc2[j - 2] - xexc2[j - 1]);
	qin[i__ * 24 + 9] = (a * en + b) * (float)1e-16;

L1160:
	qin[i__ * 24 + 10] = (float)0.;
	if (en <= ein[10]) {
	    goto L1260;
	}
	i__2 = nexc3;
	for (j = 2; j <= i__2; ++j) {
	    if (en <= xexc3[j - 1]) {
		goto L1250;
	    }
/* L1200: */
	}
	j = nexc3;
L1250:
	a = (yexc3[j - 1] - yexc3[j - 2]) / (xexc3[j - 1] - xexc3[j - 2]);
	b = (xexc3[j - 2] * yexc3[j - 1] - xexc3[j - 1] * yexc3[j - 2]) / (
		xexc3[j - 2] - xexc3[j - 1]);
	qin[i__ * 24 + 10] = (a * en + b) * (float)1e-16;

L1260:
	q[i__ * 6 + 3] = (float)0.;
	if (en <= e[3]) {
	    goto L1360;
	}
	i__2 = nion;
	for (j = 2; j <= i__2; ++j) {
	    if (en <= xion[j - 1]) {
		goto L1350;
	    }
/* L1300: */
	}
	j = nion;
L1350:
	a = (yion[j - 1] - yion[j - 2]) / (xion[j - 1] - xion[j - 2]);
	b = (xion[j - 2] * yion[j - 1] - xion[j - 1] * yion[j - 2]) / (xion[j 
		- 2] - xion[j - 1]);
	q[i__ * 6 + 3] = (a * en + b) * (float)1e-16;

L1360:
	q[i__ * 6 + 1] = q[i__ * 6 + 2] + q[i__ * 6 + 3] + q[i__ * 6 + 4];
	q[i__ * 6 + 2] = q[i__ * 6 + 2] - qin[i__ * 24 + 1] - qin[i__ * 24 + 
		2] - qin[i__ * 24 + 3] - qin[i__ * 24 + 4] - qin[i__ * 24 + 5]
		 - qin[i__ * 24 + 6] - qin[i__ * 24 + 7] - qin[i__ * 24 + 8] 
		- qin[i__ * 24 + 9] - qin[i__ * 24 + 10];
	q[i__ * 6 + 5] = (float)0.;
	q[i__ * 6 + 6] = (float)0.;
/* L9000: */
    }

/*     SAVE ON COMPUTING TIME */

    if (inpt_1.efinal < ein[10]) {
	*nin = 9;
    }
    if (inpt_1.efinal < ein[9]) {
	*nin = 8;
    }
    if (inpt_1.efinal < ein[8]) {
	*nin = 7;
    }
    if (inpt_1.efinal < ein[7]) {
	*nin = 6;
    }
    if (inpt_1.efinal < ein[6]) {
	*nin = 5;
    }
    if (inpt_1.efinal < ein[5]) {
	*nin = 4;
    }
    if (inpt_1.efinal < ein[4]) {
	*nin = 3;
    }
    if (inpt_1.efinal < ein[3]) {
	*nin = 2;
    }
    if (inpt_1.efinal < ein[2]) {
	*nin = 1;
    }
    if (inpt_1.efinal < ein[1]) {
	*nin = 0;
    }
    return 0;
} /* gas2_ */

/* Ne */
int StFtpcMagboltz1::gas3_(double *q, double *qin, double *q2ro, int *nin,
int *n2ro, double *e, double *ein, double *e2ro, char *name__, 
	double *virial, int *monte, int name_len)
{
    /* Initialized data */

    static double xen[37] = { 1.,1.2,1.5,1.8,2.,2.5,3.,4.,5.,6.,7.,8.,
	    8.71,9.,10.,11.,13.6,15.,16.5,19.6,20.,30.,30.6,50.,54.4,65.,77.,
	    100.,130.,150.,170.,200.,300.,400.,600.,800.,1e3 };
    static double yxsec[37] = { 1.62,1.69,1.75,1.79,1.82,1.86,1.91,1.98,
	    2.07,2.14,2.21,2.29,2.35,2.37,2.44,2.51,2.66,2.71,2.76,2.83,2.84,
	    2.83,2.82,2.45,2.36,2.18,1.97,1.56,1.22,1.05,.91,.76,.52,.42,.33,
	    .27,.25 };
    static double xion[35] = { 21.56,22.,22.5,23.,23.5,24.,24.5,25.,25.5,
	    26.,27.,28.,29.,30.,32.,34.,36.,40.,45.,50.,55.,60.,70.,80.,90.,
	    100.,120.,140.,175.,200.,300.,400.,600.,800.,1e3 };
    static double yion[35] = { 0.,.0033,.0089,.0146,.02,.026,.032,.038,
	    .044,.05,.063,.076,.089,.102,.128,.154,.179,.228,.282,.338,.391,
	    .435,.514,.58,.63,.67,.72,.76,.78,.78,.72,.63,.53,.44,.39 };
    static double xexc[35] = { 16.615,16.78,16.97,17.3,18.4,18.7,18.8,
	    19.8,20.,21.,22.,24.,26.,28.,30.,35.,40.,50.,60.,70.,80.,90.,100.,
	    120.,150.,200.,250.,300.,400.,500.,600.,700.,800.,900.,1e3 };
    static double yexc[35] = { 0.,.0034,.0185,.012,.0181,.0349,.028,.05,
	    .0523,.0732,.0923,.123,.143,.162,.176,.195,.205,.207,.203,.195,
	    .187,.179,.171,.157,.138,.117,.102,.091,.075,.064,.057,.051,.047,
	    .043,.04 };

    /* System generated locals */
    int i__1, i__2;
    double d__1;

    /* Local variables */
    static double apol;
    static int nexc, lmax, nion;
    static double sumi, a, b;
    static int i__, j, ndata;
    static double a1, b1, a2, aa, dd, ff, ak, en, an0, an1, an2, api, sum;

    /* Parameter adjustments */
    --e2ro;
    --ein;
    --e;
    q2ro -= 9;
    qin -= 25;
    q -= 7;

    /* Function Body */

    sprintf(name__, " NEON    92    ");
    *nin = 1;
    *n2ro = 0;
    ndata = 37;
    nion = 35;
    nexc = 35;
    e[1] = (float)0.;
    e[2] = cnsts_1.emass * (float)2. / (cnsts_1.amu * (float)20.179);
    e[3] = (float)21.56;
    e[4] = (float)0.;
    e[5] = (float)0.;
    e[6] = (float)0.;
    ein[1] = (float)16.615;
    apol = (float)2.672;
    lmax = 100;
    aa = (float).2135;
    dd = (float)3.86;
    ff = (float)-2.656;
    a1 = (float)1.846;
    b1 = (float)3.29;
    a2 = (float)-.037;
    api = (float)3.141592654;
    en = -inpt_1.estep;
    if (*monte == 1) {
	en = -inpt_1.estep / (float)2.;
    }
    i__1 = inpt_1.nstep1 + 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	en += inpt_1.estep;
	if (en > (float)1.) {
	    goto L100;
	}
	if (en == (float)0.) {
	    q[i__ * 6 + 2] = (float)1.61e-17;
	}
	if (en == (float)0.) {
	    goto L200;
	}
	ak = ::sqrt(en / inpt_1.ary);
	an0 = -aa * ak * (apol * (float)4. / (float)3. * ak * ak * ::log(ak) + (
		float)1.) - api * apol / (float)3. * ak * ak + dd * ak * ak * 
		ak + ff * ak * ak * ak * ak;
	an1 = (ak * (float).56 * ak - a1 * ak * ak * ak) / (b1 * ak * ak + (
		float)1.);
	an2 = ak * (float).08 * ak - a2 * ak * ak * ak * ak * ak;
/* Computing 2nd power */
	d__1 = sin(an0 - an1);
	sum = d__1 * d__1;
/* Computing 2nd power */
	d__1 = sin(an1 - an2);
	sum += d__1 * d__1 * (float)2.;
	i__2 = lmax - 1;
	for (j = 2; j <= i__2; ++j) {
	    sumi = (float)6. / ((j * (float)2. + (float)5.) * (j * (float)2. 
		    + (float)3.) * (j * (float)2. + (float)1.) * (j * (float)
		    2. - (float)1.));
/* Computing 2nd power */
	    d__1 = sin(api * apol * ak * ak * sumi);
	    sum += (j + (float)1.) * (d__1 * d__1);
/* L10: */
	}
	q[i__ * 6 + 2] = sum * (float)4. * cnsts_1.pir2 / (ak * ak);
	goto L200;
L100:
	i__2 = ndata;
	for (j = 2; j <= i__2; ++j) {
	    if (en <= xen[j - 1]) {
		goto L160;
	    }
/* L150: */
	}
	j = ndata;
L160:
	a = (yxsec[j - 1] - yxsec[j - 2]) / (xen[j - 1] - xen[j - 2]);
	b = (xen[j - 2] * yxsec[j - 1] - xen[j - 1] * yxsec[j - 2]) / (xen[j 
		- 2] - xen[j - 1]);
	q[i__ * 6 + 2] = (a * en + b) * (float)1e-16;
L200:
	q[i__ * 6 + 3] = (float)0.;
	if (en <= e[3]) {
	    goto L230;
	}
	i__2 = nion;
	for (j = 2; j <= i__2; ++j) {
	    if (en <= xion[j - 1]) {
		goto L220;
	    }
/* L210: */
	}
	j = nion;
L220:
	a = (yion[j - 1] - yion[j - 2]) / (xion[j - 1] - xion[j - 2]);
	b = (xion[j - 2] * yion[j - 1] - xion[j - 1] * yion[j - 2]) / (xion[j 
		- 2] - xion[j - 1]);
	q[i__ * 6 + 3] = (a * en + b) * (float)1e-16;
L230:
	q[i__ * 6 + 4] = (float)0.;
	q[i__ * 6 + 5] = (float)0.;
	q[i__ * 6 + 6] = (float)0.;

	qin[i__ * 24 + 1] = (float)0.;
	if (en <= ein[1]) {
	    goto L370;
	}
	i__2 = nexc;
	for (j = 2; j <= i__2; ++j) {
	    if (en <= xexc[j - 1]) {
		goto L360;
	    }
/* L350: */
	}
	j = nexc;
L360:
	a = (yexc[j - 1] - yexc[j - 2]) / (xexc[j - 1] - xexc[j - 2]);
	b = (xexc[j - 2] * yexc[j - 1] - xexc[j - 1] * yexc[j - 2]) / (xexc[j 
		- 2] - xexc[j - 1]);
	qin[i__ * 24 + 1] = (a * en + b) * (float)1e-16;
L370:
	q[i__ * 6 + 1] = q[i__ * 6 + 2] + q[i__ * 6 + 3] + qin[i__ * 24 + 1];
/* L900: */
    }
    if (inpt_1.efinal < ein[1]) {
	*nin = 0;
    }
    return 0;
} /* gas3_ */
/*  He */
int StFtpcMagboltz1::gas4_(double *q, double *qin, double *q2ro, int *nin,
int *n2ro, double *e, double *ein, double *e2ro, char *name__, 
	double *virial, int *monte, int name_len)
{
    /* Initialized data */

    static double xen[63] = { 0.,.008,.009,.01,.013,.017,.02,.025,.03,.04,
	    .05,.06,.07,.08,.09,.1,.12,.15,.18,.2,.25,.3,.4,.5,.6,.7,.8,.9,1.,
	    1.2,1.5,1.8,2.,2.5,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.6,16.5,18.,
	    20.,25.,30.,40.,50.,60.,70.,75.,80.,90.,100.,150.,200.,400.,600.,
	    1e3 };
    static double yxsec[63] = { 4.9,5.18,5.19,5.21,5.26,5.31,5.35,5.41,
	    5.46,5.54,5.62,5.68,5.74,5.79,5.83,5.86,5.94,6.04,6.12,6.16,6.27,
	    6.35,6.49,6.59,6.66,6.73,6.77,6.82,6.85,6.91,6.96,6.98,6.99,6.96,
	    6.89,6.62,6.31,6.,5.68,5.35,5.03,4.72,4.44,4.15,3.83,3.25,2.99,
	    2.58,1.95,1.51,.98,.7,.5,.4,.34,.31,.25,.21,.104,.063,.02,.01,
	    .0035 };
    static double xion[28] = { 24.59,25.,25.5,26.,26.5,27.,28.,29.,30.,
	    32.,34.,36.,38.,40.,45.,50.,55.,60.,70.,80.,100.,120.,150.,175.,
	    200.,300.,500.,1e3 };
    static double yion[28] = { 0.,.0052,.0113,.0175,.0236,.03,.043,.055,
	    .067,.092,.114,.135,.155,.172,.21,.243,.271,.29,.321,.344,.366,
	    .373,.37,.359,.347,.297,.224,.141 };
    static double xexc[21] = { 19.82,20.,25.,30.,40.,50.,60.,70.,75.,80.,
	    90.,100.,150.,200.,300.,400.,500.,600.,700.,800.,1e3 };
    static double yexc[21] = { 0.,.03,.1,.163,.187,.191,.205,.194,.192,
	    .189,.187,.185,.165,.146,.121,.099,.087,.076,.068,.061,.051 };

    /* System generated locals */
    int i__1, i__2;

    /* Local variables */
    static int nexc, nion;
    static double a, b;
    static int i__, j, ndata;
    static double en;

    /* Parameter adjustments */
    --e2ro;
    --ein;
    --e;
    q2ro -= 9;
    qin -= 25;
    q -= 7;

    /* Function Body */

    sprintf(name__, "  HELIUM4      ");
/* -------------------------------------------------------------------- */
/*  HELIUM 4 BEST KNOWN GAS USED AS STANDARD ACCURACY BETTER THAN 0.2% */
/*  AT ALL FIELDS. */
/* -------------------------------------------------------------------- */
    *nin = 1;
    *n2ro = 0;
    ndata = 63;
    nion = 28;
    nexc = 21;
    e[1] = (float)0.;
    e[2] = cnsts_1.emass * (float)2. / (cnsts_1.amu * (float)4.0026);
    e[3] = (float)24.59;
    e[4] = (float)0.;
    e[5] = (float)0.;
    e[6] = (float)0.;
    ein[1] = (float)19.82;
    en = -inpt_1.estep;
    if (*monte == 1) {
	en = -inpt_1.estep / (float)2.;
    }
    i__1 = inpt_1.nstep1 + 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	en += inpt_1.estep;
	i__2 = ndata;
	for (j = 2; j <= i__2; ++j) {
	    if (en <= xen[j - 1]) {
		goto L20;
	    }
/* L10: */
	}
	j = ndata;
L20:
	a = (yxsec[j - 1] - yxsec[j - 2]) / (xen[j - 1] - xen[j - 2]);
	b = (xen[j - 2] * yxsec[j - 1] - xen[j - 1] * yxsec[j - 2]) / (xen[j 
		- 2] - xen[j - 1]);
	q[i__ * 6 + 2] = (a * en + b) * (float)1e-16;

	q[i__ * 6 + 3] = (float)0.;
	if (en <= e[3]) {
	    goto L200;
	}
	i__2 = nion;
	for (j = 2; j <= i__2; ++j) {
	    if (en <= xion[j - 1]) {
		goto L120;
	    }
/* L110: */
	}
	j = nion;
L120:
	a = (yion[j - 1] - yion[j - 2]) / (xion[j - 1] - xion[j - 2]);
	b = (xion[j - 2] * yion[j - 1] - xion[j - 1] * yion[j - 2]) / (xion[j 
		- 2] - xion[j - 1]);
	q[i__ * 6 + 3] = (a * en + b) * (float)1e-16;

L200:
	q[i__ * 6 + 4] = (float)0.;
	q[i__ * 6 + 5] = (float)0.;
	q[i__ * 6 + 6] = (float)0.;

	qin[i__ * 24 + 1] = (float)0.;
	if (en <= ein[1]) {
	    goto L600;
	}
	i__2 = nexc;
	for (j = 2; j <= i__2; ++j) {
	    if (en <= xexc[j - 1]) {
		goto L520;
	    }
/* L510: */
	}
	j = nexc;
L520:
	a = (yexc[j - 1] - yexc[j - 2]) / (xexc[j - 1] - xexc[j - 2]);
	b = (xexc[j - 2] * yexc[j - 1] - xexc[j - 1] * yexc[j - 2]) / (xexc[j 
		- 2] - xexc[j - 1]);
	qin[i__ * 24 + 1] = (a * en + b) * (float)1e-16;
	qin[i__ * 24 + 1] = qin[i__ * 24 + 1];
L600:

	q[i__ * 6 + 1] = q[i__ * 6 + 2] + qin[i__ * 24 + 1] + q[i__ * 6 + 3];
/* L900: */
    }
/*  SAVE COMPUTE TIME */
    if (inpt_1.efinal <= ein[1]) {
	*nin = 0;
    }

    return 0;
} /* gas4_ */

int StFtpcMagboltz1::h1calc_(int *l, double *dhfnal, double *dxx, double *dhfrst)
{
    /* System generated locals */
    int i__1;

    /* Local variables */
    static double ssum0;
    static int i__, j, m;
    static double termb, termt, sum, sum0, sum1, sum2, sum3, sum4, sum5;

    if (*l == 2) {
	goto L300;
    }
    i__1 = inpt_1.nstep1;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L1000: */
	h1c_1.h1[i__ - 1] = (f0c_1.f[i__ - 1] - f2c_1.f2[i__ - 1] * (float).2)
		 / mix2_1.qtot[i__ - 1];
    }
    i__1 = inpt_1.nstep1;
    for (j = 2; j <= i__1; ++j) {
/* L200: */
	h1c_1.dh1[j - 1] = (h1c_1.h1[j] - h1c_1.h1[j - 2]) / (inpt_1.estep * (
		float)2.);
    }
    h1c_1.dh1[0] = h1c_1.h1[0] / inpt_1.estep;
    return 0;
L300:
    ssum0 = (float)0.;
    for (j = 1; j <= 2002; ++j) {
	h1c_1.dh1[j - 1] = (float)0.;
/* L344: */
	h1c_1.h1[j - 1] = (float)0.;
    }
    h1c_1.h1[inpt_1.nstep1 - 1] = (float)0.;
    h1c_1.dh1[inpt_1.nstep1 - 1] = -(*dhfnal);
    h1c_1.h1[inpt_1.nstep - 1] = -inpt_1.estep * h1c_1.dh1[inpt_1.nstep1 - 1];
    h1c_1.dh1[inpt_1.nstep - 1] = h1c_1.dh1[inpt_1.nstep1 - 1];
    i__1 = inpt_1.nstep - 1;
    for (m = 1; m <= i__1; ++m) {
	i__ = inpt_1.nstep - m + 1;
	sum0 = h1c_1.h1[i__ - 1];
	sum1 = (f0c_1.f[i__ - 1] - f2c_1.f2[i__ - 1] * (float).2) / 
		mix2_1.qtot[i__ - 1];
	sum2 = mag_1.qe[i__ - 1] * (float).3 * f1c_1.f1[i__ - 1] / (mix2_1.e[
		i__ - 1] * mix2_1.qtot[i__ - 1]);
	sum3 = mag_1.qe[i__ - 1] * (float).3 * mag_1.qe[i__ - 1] * h1c_1.dh1[
		i__ - 1] / mix2_1.e[i__ - 1];
	sum4 = mag_1.qe[i__ - 1] * (float).15 * mag_1.qe[i__ - 1] * h1c_1.h1[
		i__ - 1] / (mix2_1.e[i__ - 1] * mix2_1.e[i__ - 1]);
	sum5 = mag_1.qe[i__ - 1] * (float)1.8 * f3c_1.f3[i__ - 1] / (mix2_1.e[
		i__ - 1] * mix2_1.qtot[i__ - 1] * (float)14.);
	sum = sum1 - sum0 - sum2 + sum3 - sum4 + sum5;
	sum *= inpt_1.estep;
	ssum0 += sum;
	termt = mag_1.qe[i__ - 1] * (float).2 * (f1c_1.f1[i__ - 1] * (float)
		1. / mix2_1.qtot[i__ - 1] + mag_1.qe[i__ - 1] * h1c_1.h1[i__ 
		- 1] / (mix2_1.e[i__ - 1] * (float)2.) - f3c_1.f3[i__ - 1] * (
		float)3. / (mix2_1.qtot[i__ - 1] * (float)7.));
	termb = mag_1.qe[i__ - 1] * (float).2 * mag_1.qe[i__ - 1];
	h1c_1.dh1[i__ - 1] = (ssum0 + termt) / termb;
	h1c_1.dh1[i__ - 1] -= *dhfnal;
	h1c_1.h1[i__ - 2] = h1c_1.h1[i__ - 1] - inpt_1.estep * (h1c_1.dh1[i__ 
		- 1] * (float)1.5 - h1c_1.dh1[i__] * (float).5);
	h1c_1.dh1[i__ - 2] = h1c_1.dh1[i__ - 1] * (float)2. - h1c_1.dh1[i__];
/* L500: */
    }
    *dhfrst = h1c_1.dh1[0];
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L125: */
	sint_1.simf[j - 1] = mix2_1.e[j - 1] * h1c_1.h1[j - 1];
    }
    simp_(&sum);
    *dxx = sum * mag_1.eovm / (float)3.;
    return 0;
} /* h1calc_ */

int StFtpcMagboltz1::mixer_()
{
    /* System generated locals */
    int i__1, i__2;

    /* Local variables */
    static double a2ro1, aion, a2ro2, e2ro1[3], e2ro2[3], e2ro3[3], e2ro4[
	    3], eion[4], qatt[8008]	/* was [4][2002] */, a2ro3, a2ro4;
    static int i__, j, k;
    static double e1[6], e2[6], e3[6], e4[6], qdrot[2002], q1[12012]	
	    /* was [6][2002] */, q2[12012]	/* was [6][2002] */, q3[12012]
	    	/* was [6][2002] */, q4[12012]	/* was [6][2002] */, qqrot[
	    2002], eg, en, ei1[24], ei2[24], ei3[24], ei4[24], virial1, 
	    virial2, virial3, virial4, ain1, ain2, ain3;
    static double ain4;




/*  --------------------------------------------------------------------- 
*/

/*     SUBROUTINE MIXER FILLS ARRAYS USED IN SUBROUTINE  F0CALC */
/*     CAN HAVE A MIXTURE OF UP TO 4 GASES */


/*  --------------------------------------------------------------------- 
*/

    mix3_1.nin1 = 0;
    mix3_1.nin2 = 0;
    mix3_1.nin3 = 0;
    mix3_1.nin4 = 0;
    mix4_1.n2ro1 = 0;
    mix4_1.n2ro2 = 0;
    mix4_1.n2ro3 = 0;
    mix4_1.n2ro4 = 0;
    sprintf(names_1.name1, "000000000000000");
    sprintf(names_1.name2, "000000000000000");
    sprintf(names_1.name3, "000000000000000");
    sprintf(names_1.name4, "000000000000000");
    for (j = 1; j <= 6; ++j) {
	for (i__ = 1; i__ <= 2002; ++i__) {
	    q1[j + i__ * 6 - 7] = (float)0.;
	    q2[j + i__ * 6 - 7] = (float)0.;
	    q3[j + i__ * 6 - 7] = (float)0.;
	    q4[j + i__ * 6 - 7] = (float)0.;
/* L10: */
	}
	e1[j - 1] = (float)0.;
	e2[j - 1] = (float)0.;
	e3[j - 1] = (float)0.;
/* L20: */
	e4[j - 1] = (float)0.;
    }

/*   CALL GAS CROSS-SECTIONS */

    gas1_(q1, mix1_1.qin1, mix4_1.q2ro1, &mix3_1.nin1, &mix4_1.n2ro1, e1, ei1,
	     e2ro1, names_1.name1, &virial1, &c__0, 15L);
    if (inpt_1.ngas == 1) {
	goto L100;
    }
    gas2_(q2, mix1_1.qin2, mix4_1.q2ro2, &mix3_1.nin2, &mix4_1.n2ro2, e2, ei2,
	     e2ro2, names_1.name2, &virial2, &c__0, 15L);
    if (inpt_1.ngas == 2) {
	goto L100;
    }
    gas3_(q3, mix1_1.qin3, mix4_1.q2ro3, &mix3_1.nin3, &mix4_1.n2ro3, e3, ei3,
	     e2ro3, names_1.name3, &virial3, &c__0, 15L);
    if (inpt_1.ngas == 3) {
	goto L100;
    }
    gas4_(q4, mix1_1.qin4, mix4_1.q2ro4, &mix3_1.nin4, &mix4_1.n2ro4, e4, ei4,
	     e2ro4, names_1.name4, &virial4, &c__0, 15L);
L100:

/* ------------------------------------------------------------------- */
/* CORRECTION FOR NUMBER DENSITY DUE TO SECOND VIRIAL COEFFICIENT TO */
/* BE ENTERED HERE (NOT YET IMPLEMENTED) . IMPORTANT FOR HIGH PRESSURE. */
/* -------------------------------------------------------------------- */

    i__1 = inpt_1.nstep1 + 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	mix2_1.qtot[i__ - 1] = ratio_1.an1 * q1[i__ * 6 - 6] + ratio_1.an2 * 
		q2[i__ * 6 - 6] + ratio_1.an3 * q3[i__ * 6 - 6] + ratio_1.an4 
		* q4[i__ * 6 - 6];
	mix2_1.qel[i__ - 1] = ratio_1.an1 * q1[i__ * 6 - 5] + ratio_1.an2 * 
		q2[i__ * 6 - 5] + ratio_1.an3 * q3[i__ * 6 - 5] + ratio_1.an4 
		* q4[i__ * 6 - 5];
	mix1_1.qelm[i__ - 1] = ratio_1.an1 * q1[i__ * 6 - 5] * e1[1] + 
		ratio_1.an2 * q2[i__ * 6 - 5] * e2[1] + ratio_1.an3 * q3[i__ *
		 6 - 5] * e3[1] + ratio_1.an4 * q4[i__ * 6 - 5] * e4[1];

	qqrot[i__ - 1] = ratio_1.an1 * q1[i__ * 6 - 2] * e1[4] + ratio_1.an2 *
		 q2[i__ * 6 - 2] * e2[4] + ratio_1.an3 * q3[i__ * 6 - 2] * e3[
		4] + ratio_1.an4 * q4[i__ * 6 - 2] * e4[4];
	qqrot[i__ - 1] *= (float)4.;

	en = mix2_1.e[i__ - 1] * (float)2.;
	if (mix2_1.e[i__ - 1] == (float)0.) {
	    en = (float)1e-8;
	}
	qdrot[i__ - 1] = (float)0.;
	if (e1[5] == (float)0.) {
	    goto L170;
	}
	qdrot[i__ - 1] = ratio_1.an1 * q1[i__ * 6 - 1] * (float)2. * e1[5] * 
		inpt_1.ary * ::log(en / ::sqrt(e1[5] * inpt_1.akt));
L170:
	if (e2[5] == (float)0.) {
	    goto L171;
	}
	qdrot[i__ - 1] += ratio_1.an2 * q2[i__ * 6 - 1] * (float)2. * e2[5] * 
		inpt_1.ary * ::log(en / ::sqrt(e2[5] * inpt_1.akt));
L171:
	if (e3[5] == (float)0.) {
	    goto L172;
	}
	qdrot[i__ - 1] += ratio_1.an3 * q3[i__ * 6 - 1] * (float)2. * e3[5] * 
		inpt_1.ary * ::log(en / ::sqrt(e3[5] * inpt_1.akt));
L172:
	if (e4[5] == (float)0.) {
	    goto L173;
	}
	qdrot[i__ - 1] += ratio_1.an4 * q4[i__ * 6 - 1] * (float)2. * e4[5] * 
		inpt_1.ary * ::log(en / ::sqrt(e4[5] * inpt_1.akt));

L173:
	mix1_1.qelm[i__ - 1] = mix1_1.qelm[i__ - 1] * mix2_1.e[i__ - 1] * 
		mix2_1.e[i__ - 1] + qqrot[i__ - 1] * mix2_1.e[i__ - 1] + 
		qdrot[i__ - 1];

	mix1_1.qion[(i__ << 2) - 4] = q1[i__ * 6 - 4] * ratio_1.an1 * 
		mix2_1.e[i__ - 1];
	mix1_1.qion[(i__ << 2) - 3] = q2[i__ * 6 - 4] * ratio_1.an2 * 
		mix2_1.e[i__ - 1];
	mix1_1.qion[(i__ << 2) - 2] = q3[i__ * 6 - 4] * ratio_1.an3 * 
		mix2_1.e[i__ - 1];
	mix1_1.qion[(i__ << 2) - 1] = q4[i__ * 6 - 4] * ratio_1.an4 * 
		mix2_1.e[i__ - 1];
	qatt[(i__ << 2) - 4] = q1[i__ * 6 - 3] * ratio_1.an1 * mix2_1.e[i__ - 
		1];
	qatt[(i__ << 2) - 3] = q2[i__ * 6 - 3] * ratio_1.an2 * mix2_1.e[i__ - 
		1];
	qatt[(i__ << 2) - 2] = q3[i__ * 6 - 3] * ratio_1.an3 * mix2_1.e[i__ - 
		1];
	qatt[(i__ << 2) - 1] = q4[i__ * 6 - 3] * ratio_1.an4 * mix2_1.e[i__ - 
		1];

	if (mix3_1.nin1 == 0) {
	    goto L1810;
	}
	i__2 = mix3_1.nin1;
	for (j = 1; j <= i__2; ++j) {
/* L181: */
	    mix1_1.qin1[j + i__ * 24 - 25] = mix1_1.qin1[j + i__ * 24 - 25] * 
		    ratio_1.an1 * mix2_1.e[i__ - 1];
	}
L1810:
	if (mix3_1.nin2 == 0) {
	    goto L1820;
	}
	i__2 = mix3_1.nin2;
	for (j = 1; j <= i__2; ++j) {
/* L182: */
	    mix1_1.qin2[j + i__ * 24 - 25] = mix1_1.qin2[j + i__ * 24 - 25] * 
		    ratio_1.an2 * mix2_1.e[i__ - 1];
	}
L1820:
	if (mix3_1.nin3 == 0) {
	    goto L1830;
	}
	i__2 = mix3_1.nin3;
	for (j = 1; j <= i__2; ++j) {
/* L183: */
	    mix1_1.qin3[j + i__ * 24 - 25] = mix1_1.qin3[j + i__ * 24 - 25] * 
		    ratio_1.an3 * mix2_1.e[i__ - 1];
	}
L1830:
	if (mix3_1.nin4 == 0) {
	    goto L1840;
	}
	i__2 = mix3_1.nin4;
	for (j = 1; j <= i__2; ++j) {
/* L184: */
	    mix1_1.qin4[j + i__ * 24 - 25] = mix1_1.qin4[j + i__ * 24 - 25] * 
		    ratio_1.an4 * mix2_1.e[i__ - 1];
	}

L1840:
	for (k = 1; k <= 2; ++k) {
	    if (mix4_1.n2ro1 == 0) {
		goto L1910;
	    }
	    i__2 = mix4_1.n2ro1;
	    for (j = 1; j <= i__2; ++j) {
/* L191: */
		mix4_1.q2ro1[k + ((j + i__ * 3) << 1) - 9] = mix4_1.q2ro1[k + (
			(j + i__ * 3) << 1) - 9] * ratio_1.an1 * mix2_1.e[i__ - 
			1];
	    }
L1910:
	    if (mix4_1.n2ro2 == 0) {
		goto L1920;
	    }
	    i__2 = mix4_1.n2ro2;
	    for (j = 1; j <= i__2; ++j) {
/* L192: */
		mix4_1.q2ro2[k + ((j + i__ * 3) << 1) - 9] = mix4_1.q2ro2[k + (
			(j + i__ * 3) << 1) - 9] * ratio_1.an2 * mix2_1.e[i__ - 
			1];
	    }
L1920:
	    if (mix4_1.n2ro3 == 0) {
		goto L1930;
	    }
	    i__2 = mix4_1.n2ro3;
	    for (j = 1; j <= i__2; ++j) {
/* L193: */
		mix4_1.q2ro3[k + ((j + i__ * 3) << 1) - 9] = mix4_1.q2ro3[k + (
			(j + i__ * 3) << 1) - 9] * ratio_1.an3 * mix2_1.e[i__ - 
			1];
	    }
L1930:
	    if (mix4_1.n2ro4 == 0) {
		goto L195;
	    }
	    i__2 = mix4_1.n2ro4;
	    for (j = 1; j <= i__2; ++j) {
/* L194: */
		mix4_1.q2ro4[k + ((j + i__ * 3) << 1) - 9] = mix4_1.q2ro4[k + (
			(j + i__ * 3) << 1) - 9] * ratio_1.an4 * mix2_1.e[i__ - 
			1];
	    }
L195:
	    ;
	}


	mix2_1.qrel[i__ - 1] = (float)0.;
	mix1_1.qsatt[i__ - 1] = (float)0.;
	mix1_1.qsum[i__ - 1] = (float)0.;
	i__2 = inpt_1.ngas;
	for (j = 1; j <= i__2; ++j) {
	    mix1_1.qsum[i__ - 1] = mix1_1.qsum[i__ - 1] + mix1_1.qion[j + (
		    i__ << 2) - 5] + qatt[j + (i__ << 2) - 5];
	    mix1_1.qsatt[i__ - 1] += qatt[j + (i__ << 2) - 5];
/* L90: */
	    mix2_1.qrel[i__ - 1] = mix2_1.qrel[i__ - 1] + mix1_1.qion[j + (
		    i__ << 2) - 5] - qatt[j + (i__ << 2) - 5];
	}

	if (mix3_1.nin1 == 0) {
	    goto L910;
	}
	i__2 = mix3_1.nin1;
	for (j = 1; j <= i__2; ++j) {
/* L91: */
	    mix1_1.qsum[i__ - 1] += mix1_1.qin1[j + i__ * 24 - 25];
	}
L910:
	if (mix3_1.nin2 == 0) {
	    goto L920;
	}
	i__2 = mix3_1.nin2;
	for (j = 1; j <= i__2; ++j) {
/* L92: */
	    mix1_1.qsum[i__ - 1] += mix1_1.qin2[j + i__ * 24 - 25];
	}
L920:
	if (mix3_1.nin3 == 0) {
	    goto L930;
	}
	i__2 = mix3_1.nin3;
	for (j = 1; j <= i__2; ++j) {
/* L93: */
	    mix1_1.qsum[i__ - 1] += mix1_1.qin3[j + i__ * 24 - 25];
	}
L930:
	if (mix3_1.nin4 == 0) {
	    goto L940;
	}
	i__2 = mix3_1.nin4;
	for (j = 1; j <= i__2; ++j) {
/* L94: */
	    mix1_1.qsum[i__ - 1] += mix1_1.qin4[j + i__ * 24 - 25];
	}
L940:
	eg = mix2_1.e[i__ - 1];
	if (mix2_1.e[i__ - 1] == (float)0.) {
	    eg = (float)1e-7;
	}
	mix2_1.qinel[i__ - 1] = mix1_1.qsum[i__ - 1] / eg;

/* L200: */
    }

    eion[0] = e1[2];
    eion[1] = e2[2];
    eion[2] = e3[2];
    eion[3] = e4[2];
    i__1 = inpt_1.ngas;
    for (j = 1; j <= i__1; ++j) {
	aion = eion[j - 1] / inpt_1.estep;
	mix3_1.lion[j - 1] = (int) aion;
/* L300: */
	mix3_1.alion[j - 1] = aion - mix3_1.lion[j - 1];
    }

    if (mix3_1.nin1 == 0) {
	goto L4000;
    }
    i__1 = mix3_1.nin1;
    for (j = 1; j <= i__1; ++j) {
	ain1 = ei1[j - 1] / inpt_1.estep;
	mix3_1.lin1[j - 1] = (int) ain1;
/* L400: */
	mix3_1.alin1[j - 1] = ain1 - mix3_1.lin1[j - 1];
    }
L4000:
    if (mix3_1.nin2 == 0) {
	goto L4100;
    }
    i__1 = mix3_1.nin2;
    for (j = 1; j <= i__1; ++j) {
	ain2 = ei2[j - 1] / inpt_1.estep;
	mix3_1.lin2[j - 1] = (int) ain2;
/* L410: */
	mix3_1.alin2[j - 1] = ain2 - mix3_1.lin2[j - 1];
    }
L4100:
    if (mix3_1.nin3 == 0) {
	goto L4200;
    }
    i__1 = mix3_1.nin3;
    for (j = 1; j <= i__1; ++j) {
	ain3 = ei3[j - 1] / inpt_1.estep;
	mix3_1.lin3[j - 1] = (int) ain3;
/* L420: */
	mix3_1.alin3[j - 1] = ain3 - mix3_1.lin3[j - 1];
    }
L4200:
    if (mix3_1.nin4 == 0) {
	goto L4300;
    }
    i__1 = mix3_1.nin4;
    for (j = 1; j <= i__1; ++j) {
	ain4 = ei4[j - 1] / inpt_1.estep;
	mix3_1.lin4[j - 1] = (int) ain4;
/* L430: */
	mix3_1.alin4[j - 1] = ain4 - mix3_1.lin4[j - 1];
    }
L4300:

    if (mix4_1.n2ro1 == 0) {
	goto L5000;
    }
    i__1 = mix4_1.n2ro1;
    for (j = 1; j <= i__1; ++j) {
	a2ro1 = e2ro1[j - 1] / inpt_1.estep;
	mix4_1.l2ro1[j - 1] = (int) a2ro1;
/* L500: */
	mix4_1.al2ro1[j - 1] = a2ro1 - mix4_1.l2ro1[j - 1];
    }
L5000:
    if (mix4_1.n2ro2 == 0) {
	goto L5100;
    }
    i__1 = mix4_1.n2ro2;
    for (j = 1; j <= i__1; ++j) {
	a2ro2 = e2ro2[j - 1] / inpt_1.estep;
	mix4_1.l2ro2[j - 1] = (int) a2ro2;
/* L510: */
	mix4_1.al2ro2[j - 1] = a2ro2 - mix4_1.l2ro2[j - 1];
    }
L5100:
    if (mix4_1.n2ro3 == 0) {
	goto L5200;
    }
    i__1 = mix4_1.n2ro3;
    for (j = 1; j <= i__1; ++j) {
	a2ro3 = e2ro3[j - 1] / inpt_1.estep;
	mix4_1.l2ro3[j - 1] = (int) a2ro3;
/* L520: */
	mix4_1.al2ro3[j - 1] = a2ro3 - mix4_1.l2ro3[j - 1];
    }
L5200:
    if (mix4_1.n2ro4 == 0) {
	goto L5300;
    }
    i__1 = mix4_1.n2ro4;
    for (j = 1; j <= i__1; ++j) {
	a2ro4 = e2ro4[j - 1] / inpt_1.estep;
	mix4_1.l2ro4[j - 1] = (int) a2ro4;
/* L530: */
	mix4_1.al2ro4[j - 1] = a2ro4 - mix4_1.l2ro4[j - 1];
    }
L5300:
    return 0;
} /* mixer_ */

int StFtpcMagboltz1::nalpha_()
{
    /* System generated locals */
    int i__1;
    double d__1;

    /* Local variables */
    static double velx, vely, velz, vtot, velx1, vely1, velz1;
    static int j;
    static double vtot1, ratei, cjk, dss, dxx, sum, dyy, dzz;


    inpt_1.alpold = inpt_1.alpnew;
    inpt_1.alpoax = inpt_1.alpnax;
    inpt_1.alpoay = inpt_1.alpnay;
    inpt_1.alpoaz = inpt_1.alpnaz;
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L50: */
	sint_1.simf[j - 1] = mix2_1.e[j - 1] * f0c_1.f[j - 1] / (mag_1.denom[
		j - 1] * mix2_1.qtot[j - 1]);
    }
    simp_(&sum);
    dxx = sum * mag_1.eovm / (float)3.;
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L60: */
	sint_1.simf[j - 1] = mix2_1.e[j - 1] * f0c_1.f[j - 1] * mag_1.sod2[j 
		- 1] / mix2_1.qtot[j - 1];
    }
    simp_(&sum);
    dyy = sum * mag_1.eovm / (float)3.;
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L70: */
	sint_1.simf[j - 1] = mix2_1.e[j - 1] * f0c_1.f[j - 1] * mag_1.cod2[j 
		- 1] / mix2_1.qtot[j - 1];
    }
    simp_(&sum);
    dzz = sum * mag_1.eovm / (float)3.;
    dss = (dzz + dyy + dxx) / (float)3.;
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L100: */
	sint_1.simf[j - 1] = mag_1.cod2[j - 1] * mag_1.qfemag[j - 1] * 
		f0c_1.df[j - 1] * f0c_1.f[j - 1];
    }
    simp_(&sum);
    velz1 = -sum * mag_1.eovm;
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L200: */
	sint_1.simf[j - 1] = mag_1.scd[j - 1] * mag_1.qfemag[j - 1] * 
		f0c_1.df[j - 1] * f0c_1.f[j - 1];
    }
    simp_(&sum);
    vely1 = -sum * mag_1.eovm;
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L300: */
	sint_1.simf[j - 1] = mag_1.sod[j - 1] * mag_1.qfemag[j - 1] * 
		f0c_1.df[j - 1] * f0c_1.f[j - 1];
    }
    simp_(&sum);
    velx1 = -sum * mag_1.eovm;
    vtot1 = ::sqrt(velx1 * velx1 + vely1 * vely1 + velz1 * velz1);
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L10: */
	sint_1.simf[j - 1] = mix2_1.qrel[j - 1] * f0c_1.f[j - 1];
    }
    simp_(&sum);
    ratei = sum * mag_1.eovm;
/* Computing 2nd power */
    d__1 = vtot1 / (dss * (float)2.);
    cjk = d__1 * d__1 - ratei / dss;
    if (cjk < (float)0.) {
	goto L15;
    }
    inpt_1.alpnew = vtot1 / (dss * (float)2.) - ::sqrt(cjk);
    goto L16;
L15:
    inpt_1.alpnew = ratei / vtot1;
L16:
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L20: */
	sint_1.simf[j - 1] = mag_1.cod2[j - 1] * (mag_1.qfemag[j - 1] * 
		f0c_1.df[j - 1] - mag_1.qef[j - 1] * inpt_1.alpnew) * f0c_1.f[
		j - 1];
    }
    simp_(&sum);
    velz = -sum * mag_1.eovm;
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L30: */
	sint_1.simf[j - 1] = mag_1.scd[j - 1] * (mag_1.qfemag[j - 1] * 
		f0c_1.df[j - 1] - mag_1.qef[j - 1] * inpt_1.alpnew) * f0c_1.f[
		j - 1];
    }
    simp_(&sum);
    vely = -sum * mag_1.eovm;
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L40: */
	sint_1.simf[j - 1] = mag_1.sod[j - 1] * (mag_1.qfemag[j - 1] * 
		f0c_1.df[j - 1] - mag_1.qef[j - 1] * inpt_1.alpnew) * f0c_1.f[
		j - 1];
    }
    simp_(&sum);
    velx = -sum * mag_1.eovm;
    vtot = ::sqrt(velx * velx + vely * vely + velz * velz);
    if (cjk < (float)0.) {
      printf("WARNING NALPHA DID NOT CONVERGE (SUBROUTINE NALPHA)\n");
    }
    inpt_1.alpnax = velx1 * inpt_1.alpnew / vtot1;
    inpt_1.alpnay = vely1 * inpt_1.alpnew / vtot1;
    inpt_1.alpnaz = velz1 * inpt_1.alpnew / vtot1;
    printf("NEW ALPHA =%f / CM.   OLD ALPHA =%f /CM.        VELZ =%f VTOT =%f    CM./SEC.\n",inpt_1.alpnew,inpt_1.alpold,velz,vtot);
    return 0;
} /* nalpha_ */


int StFtpcMagboltz1::output_(int *n)
{

    /* System generated locals */
    int i__1;
    double d__1;

    /* Local variables */
    static double faci;
    static double velx, vely, vtot, velx1, vely1, velz1;
    static int j;
    static double vtot1, vtot2, fudge, ratei, dovmb, rteel, vtot12, dl, 
	    ri, wm, select, erootf[2002], colrte, rteinl, ratatt, alpatt, 
	    dlovmb, fac, cjk, dss, dxx, sum, dyy, dyz, dzz, dxz;



    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L3: */
	sint_1.simf[j - 1] = mag_1.cod2[j - 1] * (mag_1.qfemag[j - 1] * 
		f0c_1.df[j - 1] * f0c_1.f[j - 1] + mag_1.emag * (float).4 * (
		mix2_1.e[j - 1] * f2c_1.df2[j - 1] + f2c_1.f2[j - 1] * (float)
		1.5) / (mix2_1.qtot[j - 1] * (float)3.));
    }
    simp_(&sum);
    velz1 = -sum * mag_1.eovm;
    if (*n == 0 && inpt_1.i2type == 1) {
      printf("COLLISIONS OF 2ND KIND INCLUDED.\n");
    }
    if (*n == 0) {
      printf("INTERMEDIATE VALUE FOR DRIFT VELOCITY = %f\n",velz1); 
    }
    if (*n == 0) {
	return 0;
    }
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L5: */
	sint_1.simf[j - 1] = mix2_1.e[j - 1] * f0c_1.f[j - 1] / (mag_1.denom[
		j - 1] * mix2_1.qtot[j - 1]);
    }
    simp_(&sum);
    dxx = sum * mag_1.eovm / (float)3.;
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L6: */
	sint_1.simf[j - 1] = mix2_1.e[j - 1] * f0c_1.f[j - 1] * mag_1.sod2[j 
		- 1] / mix2_1.qtot[j - 1];
    }
    simp_(&sum);
    dyy = sum * mag_1.eovm / (float)3.;
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L7: */
	sint_1.simf[j - 1] = mix2_1.e[j - 1] * f0c_1.f[j - 1] * mag_1.cod2[j 
		- 1] / mix2_1.qtot[j - 1];
    }
    simp_(&sum);
    dzz = sum * mag_1.eovm / (float)3.;
    dss = (dzz + dyy + dxx) / (float)3.;
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L10: */
	sint_1.simf[j - 1] = mix2_1.qrel[j - 1] * f0c_1.f[j - 1];
    }
    simp_(&sum);
    ratei = sum * mag_1.eovm;
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L11: */
	sint_1.simf[j - 1] = mag_1.cod2[j - 1] * (mag_1.qfemag[j - 1] * 
		f0c_1.df[j - 1] * f0c_1.f[j - 1] + mag_1.emag * (float).4 * (
		mix2_1.e[j - 1] * f2c_1.df2[j - 1] + f2c_1.f2[j - 1] * (float)
		1.5) / (mix2_1.qtot[j - 1] * (float)3.));
    }
    simp_(&sum);
    velz1 = -sum * mag_1.eovm;
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L12: */
	sint_1.simf[j - 1] = mag_1.scd[j - 1] * (mag_1.qfemag[j - 1] * 
		f0c_1.df[j - 1] * f0c_1.f[j - 1] + mag_1.emag * (float).4 * (
		mix2_1.e[j - 1] * f2c_1.df2[j - 1] + f2c_1.f2[j - 1] * (float)
		1.5) / (mix2_1.qtot[j - 1] * (float)3.));
    }
    simp_(&sum);
    vely1 = -sum * mag_1.eovm;
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L13: */
	sint_1.simf[j - 1] = mag_1.sod[j - 1] * (mag_1.qfemag[j - 1] * 
		f0c_1.df[j - 1] * f0c_1.f[j - 1] + mag_1.emag * (float).4 * (
		mix2_1.e[j - 1] * f2c_1.df2[j - 1] + f2c_1.f2[j - 1] * (float)
		1.5) / (mix2_1.qtot[j - 1] * (float)3.));
    }
    simp_(&sum);
    velx1 = -sum * mag_1.eovm;
    vtot12 = velx1 * velx1 + vely1 * vely1 + velz1 * velz1;
    vtot1 = ::sqrt(vtot12);
/* Computing 2nd power */
    d__1 = vtot1 / (dss * (float)2.);
    cjk = d__1 * d__1 - ratei / dss;
    if (cjk < (float)0.) {
      printf("WARNING ALPHA DID NOT CONVERGE USED ALPHA =  RATE/VEL0CITY\n");
    }
    if (cjk < (float)0. || ratei == (float)0.) {
	goto L15;
    }
    inpt_1.alpha = vtot1 / (dss * (float)2.) - ::sqrt(cjk);
    goto L16;
L15:
    inpt_1.alpha = ratei / vtot1;
L16:
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L20: */
	sint_1.simf[j - 1] = mag_1.cod2[j - 1] * (mag_1.qfemag[j - 1] * 
		f0c_1.df[j - 1] * f0c_1.f[j - 1] + mag_1.emag * (float).4 * (
		mix2_1.e[j - 1] * f2c_1.df2[j - 1] + f2c_1.f2[j - 1] * (float)
		1.5) / (mix2_1.qtot[j - 1] * (float)3.) - mag_1.qef[j - 1] * 
		inpt_1.alpha * (f0c_1.f[j - 1] + f2c_1.f2[j - 1] * (float).4))
		;
    }
    simp_(&sum);
    mk_1.velz = -sum * mag_1.eovm;
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L30: */
	sint_1.simf[j - 1] = mag_1.scd[j - 1] * (mag_1.qfemag[j - 1] * 
		f0c_1.df[j - 1] * f0c_1.f[j - 1] + mag_1.emag * (float).4 * (
		mix2_1.e[j - 1] * f2c_1.df2[j - 1] + f2c_1.f2[j - 1] * (float)
		1.5) / (mix2_1.qtot[j - 1] * (float)3.) - mag_1.qef[j - 1] * 
		inpt_1.alpha * (f0c_1.f[j - 1] + f2c_1.f2[j - 1] * (float).4))
		;
    }
    simp_(&sum);
    vely = -sum * mag_1.eovm;
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L40: */
	sint_1.simf[j - 1] = mag_1.sod[j - 1] * (mag_1.qfemag[j - 1] * 
		f0c_1.df[j - 1] * f0c_1.f[j - 1] + mag_1.emag * (float).4 * (
		mix2_1.e[j - 1] * f2c_1.df2[j - 1] + f2c_1.f2[j - 1] * (float)
		1.5) / (mix2_1.qtot[j - 1] * (float)3.) - mag_1.qef[j - 1] * 
		inpt_1.alpha * (f0c_1.f[j - 1] + f2c_1.f2[j - 1] * (float).4))
		;
    }
    simp_(&sum);
    velx = -sum * mag_1.eovm;
    vtot2 = velx * velx + vely * vely + mk_1.velz * mk_1.velz;
    vtot = ::sqrt(vtot2);
    ri = inpt_1.alpha * vtot1;
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L50: */
	sint_1.simf[j - 1] = mix2_1.e[j - 1] * h1c_1.h1[j - 1] / mag_1.denom[
		j - 1];
    }
    simp_(&sum);
    dxx = sum * mag_1.eovm / (float)3.;
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L60: */
	sint_1.simf[j - 1] = mix2_1.e[j - 1] * h1c_1.h1[j - 1] * mag_1.sod2[j 
		- 1];
    }
    simp_(&sum);
    dyy = sum * mag_1.eovm / (float)3.;
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L70: */
	sint_1.simf[j - 1] = mix2_1.e[j - 1] * h1c_1.h1[j - 1] * mag_1.cod2[j 
		- 1];
    }
    simp_(&sum);
    dzz = sum * mag_1.eovm / (float)3.;
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L80: */
	sint_1.simf[j - 1] = mix2_1.e[j - 1] * h1c_1.h1[j - 1] * (float)2. * 
		mag_1.scd[j - 1];
    }
    simp_(&sum);
    dyz = sum * mag_1.eovm / (float)3.;
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L90: */
	sint_1.simf[j - 1] = mix2_1.e[j - 1] * h1c_1.h1[j - 1] * (float)2. * 
		mag_1.sod[j - 1];
    }
    simp_(&sum);
    dxz = sum * mag_1.eovm / (float)3.;
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L100: */
	sint_1.simf[j - 1] = mix2_1.e[j - 1] * mix2_1.eroot[j - 1] * f0c_1.f[
		j - 1];
    }
    simp_(&sum);
    mk_1.emean = sum;
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L110: */
	sint_1.simf[j - 1] = mix2_1.eroot[j - 1] * f0c_1.f[j - 1] / 
		mag_1.denom[j - 1];
    }
    simp_(&sum);
    fac = sum;
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L120: */
	sint_1.simf[j - 1] = mix2_1.qtot[j - 1] * mix2_1.e[j - 1] * f0c_1.f[j 
		- 1];
    }
    simp_(&sum);
    colrte = sum * mag_1.eovm;
/* Computing 2nd power */
    d__1 = mag_1.wb / colrte;
    faci = (float)1. / (d__1 * d__1 + (float)1.);
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L130: */
	erootf[j - 1] = mix2_1.eroot[j - 1] * f0c_1.f[j - 1];
    }
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L140: */
	sint_1.simf[j - 1] = mix2_1.qinel[j - 1] * mix2_1.e[j - 1] * f0c_1.f[
		j - 1];
    }
    simp_(&sum);
    rteinl = sum * mag_1.eovm;
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L150: */
	sint_1.simf[j - 1] = mix2_1.qel[j - 1] * mix2_1.e[j - 1] * f0c_1.f[j 
		- 1];
    }
    simp_(&sum);
    rteel = sum * mag_1.eovm;
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L160: */
	sint_1.simf[j - 1] = g1c_1.g1[j - 1] * mix2_1.e[j - 1];
    }
    simp_(&sum);
    dl = sum * mag_1.eovm / (float)3.;

    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L161: */
	sint_1.simf[j - 1] = f0c_1.f[j - 1] * (mix1_1.qin1[j * 24 - 22] + 
		mix1_1.qin1[j * 24 - 23]);
    }
    simp_(&sum);
    select = sum * mag_1.eovm;
    printf("SELECTED INELASTIC FREQUENCY= %f\n", select);
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L170: */
	sint_1.simf[j - 1] = f0c_1.f[j - 1] * mix1_1.qsatt[j - 1];
    }
    simp_(&sum);
    ratatt = sum * mag_1.eovm;
    alpatt = ratatt / vtot1;
    wm = (float)0.;
    if (mag_1.bmag > (float)0.) {
	wm = velx * mag_1.emag * (float)1e5 / (mk_1.velz * mag_1.bmag);
    }
    dovmb = (dzz + dyy + dxx) * mag_1.emag / (vtot1 * (float)3.);
    dlovmb = dl * mag_1.emag / vtot1;
    mk_1.angle = atan(velx / mk_1.velz) * (float)180. / acos(-1.);
    printf("-----------------------------------------------------------------------------------------------------------------------");
    if (*n == 1) {
      printf("FINAL VALUES WITHOUT COLLISIONS OF 2ND KIND.   LORENTZ SOLUTION (L=1).\n");
    }
    if (*n == 2) {
      printf("FINAL VALUES WITH L=2 TERM FULLY INCLUDED IN CALCULATION.\n");
    }
    if (*n == 3) {
      printf("FINAL VALUES WITH COLLISIONS OF 2ND KIND. \n");
    }
    if (*n == 4) {
      printf("FINAL VALUES CONVERGED ON ALPHA TO ALL ORDERS. \n");
    }
    if (*n == 5) {
      printf("FINAL VALUES WITH  HIGHER MULTIPOLE. (L=2 TERM)     HIGHER TERM ONLY TO FIRST ORDER .\n");
    }
    if (*n == 6) {
      printf("FINAL VALUES WITH  HIGHER MULTIPOLE. (L=3 TERM)     HIGHER TERM ONLY TO FIRST ORDER .\n");
    }
    printf("N.B. LORENTZ ANGLE = VELX/VELZ  =%f DEGREES.\n", mk_1.angle);
    printf("VELZ =%f VELY =%f VELX =%f VTOT =%f     CM./SEC.  ( WITH IONISATION )\n",mk_1.velz,vely,velx,vtot);
    printf("VELZ1 =%f VELY1 =%f VELX1 =%f VTOT1 =%f    CM./SEC.  ( WITHOUT IONISATION )\n",velz1,vely1,velx1,vtot1);
    printf(" DZZ =%f DYY =%f DXX =%f DYZ =%f DXZ =%f   CM.**2/SEC.",dzz,dyy,dxx,dyz,dxz);
    printf("MEAN ELECTRON ENERGY =%f EV.     DIFFUSIONMOBILITY =%f EV.   =  CHARACTERISTIC ENERGY.\n",mk_1.emean,dovmb);
    printf("NET ( IONISATION - ATTACHMENT ) RATE =%f / CM.    =%f /SEC.  ATTACHMENT RATE =%f / CM.\n",inpt_1.alpha,ratei,alpatt);
    printf("CYCLOTRON FREQUENCY =%f RADS./SEC.          TOTAL COLLISION FREQUENCY =%f /SEC.           1/(1+(CYCLOTRON FREQ./COLL.FREQ.)**2) = K FACTOR =%f TRUE K FACTOR =%f\n",mag_1.wb,colrte,faci,fac);
    printf("MAGNETIC DRIFT VELOCITY =%f CM/SEC.  (ONLY VALID FOR E AND B AT 90 DEGREES TO EACH OTHER.)\n",wm);
    printf("INELASTIC COLLISION FREQUENCY =%f /SEC.          ELASTIC COLLISION FREQUENCY =%f /SEC.\n",rteinl,rteel);
    printf(" F0 =%f AT FINAL ENERGY OF %f EV.\n",f0c_1.f[inpt_1.nstep1 - 1],inpt_1.efinal);
    printf("LONGITUDINAL DIFFUSION =%f CM.**2/SEC.     = %f EV. \n",dl,dlovmb);
/* cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc */
/* convert long. diff. to um/::sqrt(cm) by W.Gong */
/* also applying the correction */
    fudge = dl / dzz;
    mk_1.sig_long__ = ::sqrt(dl * (float)2. * (float)1e6 / mk_1.velz) * (float)
	    10.;
    mk_1.sig_tranx__ = ::sqrt(dxx * (float)2. * (float)1e6 / mk_1.velz) * (
	    float)10.;
    mk_1.sig_trany__ = ::sqrt(dyy * (float)2. * (float)1e6 / mk_1.velz) * (
	    float)10.;
/* L9921: */
    mk_1.btheta_mk__ = mag_1.btheta;
    mk_1.bmag_mk__ = mag_1.bmag;
    printf("PULSED TOWNSEND OR TOF IONISATION RATE=%f /SEC.\n",ri);
/* cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc */
/* write out the relevant quantities into fort.10 by W.Gong */
/* not needed for STAR version, output to table */
/*     s_wsfe(&io___58); */
/*     do_fio(&c__1, (char *)&mag_1.emag, (int)sizeof(double)); */
/*     d__1 = mk_1.velz / (float)1e6; */
/*     do_fio(&c__1, (char *)&d__1, (int)sizeof(double)); */
/*     do_fio(&c__1, (char *)&mk_1.sig_long__, (int)sizeof(double)); */
/*     do_fio(&c__1, (char *)&mk_1.sig_tranx__, (int)sizeof(double)); */
/*     do_fio(&c__1, (char *)&mk_1.sig_trany__, (int)sizeof(double)); */
/*     do_fio(&c__1, (char *)&mk_1.angle, (int)sizeof(double)); */
/*     do_fio(&c__1, (char *)&mk_1.emean, (int)sizeof(double)); */
/*     do_fio(&c__1, (char *)&mag_1.bmag, (int)sizeof(double)); */
/*     do_fio(&c__1, (char *)&mag_1.btheta, (int)sizeof(double)); */
/*     do_fio(&c__1, (char *)&f0c_1.f[inpt_1.nstep1 - 1], (int)sizeof( */
/* 	    double)); */
/*     do_fio(&c__1, (char *)&inpt_1.efinal, (int)sizeof(double)); */
/*     e_wsfe(); */
/* cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc */
/* ********** MK ********** */
    mk_1.amk_emag__ = mag_1.emag;
    printf("MK-EMAG %f\n", mk_1.amk_emag__);
/* ************************ */
    if (*n < 5) {
	return 0;
    }
    return 0;
} /* output_ */


int StFtpcMagboltz1::prnter_()
{
    printf("BOLTZMAN SOLUTION FOR MIXTURE OF %d GASES.\n------------------------------------------------------\n",inpt_1.ngas);
    printf(" GASES REQUESTED = %s %s %s %s\n",gasn_1.ngas1,gasn_1.ngas2,gasn_1.ngas3,gasn_1.ngas4);
    printf(" GASES USED = %s %s %s %s\n",names_1.name1,names_1.name2,names_1.name3,names_1.name4);
    printf(" PERCENTAGE USED = %f %f %f %f\n",ratio_1.frac1,ratio_1.frac2,ratio_1.frac3,ratio_1.frac4);
    printf("GAS TEMPERATURE =%f DEGREES CENTIGRADE.           GAS PRESSURE = %f TORR.\n",inpt_1.tempc,inpt_1.torr);
    printf(" INTEGRATION FROM 0.0 TO %f EV.  IN %d STEPS.  CONVERGENCE ERROR LESS THAN %f\n",inpt_1.efinal,inpt_1.nstep,inpt_1.conv);
    if (inpt_1.i2type == 1) {
        printf("COLLISIONS OF SECOND TYPE INCLUDED.\n");
    }
    if (inpt_1.idlong == 1) {
	goto L71;
    }
    printf("PRINTOUT EVERY %d ITERATIONS.   MAXIMUM NUMBER OF ITERATIONS ALLOWED =%d\nLONGITUDINAL DIFF. NOT CALCULATED.\n",inpt_1.nout,inpt_1.itmax);
    goto L74;
L71:
    printf("PRINTOUT EVERY %d ITERATIONS.   MAXIMUM NUMBER OF ITERATIONS ALLOWED =%d\nLONGITUDINAL DIFFUSION CALCULATED.\n",inpt_1.nout,inpt_1.itmax);
L74:
    printf("  ELECTRIC FIELD =%f VOLTS/CM.   MAGNETIC FIELD =%f KGAUSS. \nANGLE BETWEEN MAGNETIC AND ELECTRIC FIELD = %f DEGREES.\n",mag_1.emag,mag_1.bmag,mag_1.btheta);
    if (inpt_1.isfb == 0) {
        printf(" STANDARD PARAMETERISATION OF KT TERM.\n");
    }
    if (inpt_1.isfb == 1) {
        printf("  INELASTIC LEVELS IN KT TERM.\n");
    }
    printf(" N.B. MULTIPLY DIFFUSION COEFFICIENTS BELOW BY :- DZZ=DZZ*FUDGE,  DYZ=DYZ*SQRT(FUDGE),  DXZ=DXZ*SQRT(FUDGE). USE (FUDGE) FACTOR = RATIO OF LONGITUDINAL/TRANSVERSE DIFFUSION COEFFICIENTS IN ZERO MAGNETIC FIELD.\n");
    return 0;
} /* prnter_ */

int StFtpcMagboltz1::setup_(int *last)
{
    /* Local variables */
    static double corr;
    static int i__, j;
    static double boltz, aj, alosch, awb;


/*   PHYSICAL CONSTANTS  1987 UPDATE OF TAYLOR AND COHEN */
/*   NEW VALUE OF BOLTZ 1988 */

    inpt_1.ary = (float)13.6056981;
    cnsts_1.pir2 = 8.79735669e-17;
    cnsts_1.echarg = 1.60217733e-19;
    cnsts_1.emass = 9.1093897e-31;
    cnsts_1.amu = 1.6605402e-27;
    boltz = 8.617343e-5;
    awb = 17588196200.;
    alosch = 2.686763e19;
    mag_1.eovm = ::sqrt(cnsts_1.echarg * (float)2. / cnsts_1.emass) * (float)
	    100.;


/*      READ IN OUTPUT CONTROL AND INTEGRATION DATA */


/*   INTEGRATE TO 0.1% ACCURACY */
    inpt_1.conv = (float)5e-4;
    inpt_1.nstep = 2000;
/*   CALCULATE ALPHA TO 1% ACCURACY */
    inpt_1.conalp = (float).01;
    inpt_1.nout = 10;
    inpt_1.itmax = 1200;
    inpt_1.isfb = 0;
    inpt_1.idbug = 0;
/* ********** hh ********** */
    if (*last < 10) {
	inpt_1.ngas = 4;
	inpt_1.i2type = 0;
	inpt_1.nitalp = 0;
	inpt_1.idlong = 1;
	inpt_1.lhigh = 1;
    }
/* ********** hh ********** */
    printf("************************\n");
    printf("Here comes the final E: %f\n", inpt_1.efinal);
    printf("************************\n");
    if (inpt_1.ngas == 0) {
	goto L99;
    }
    inpt_1.nstep1 = inpt_1.nstep + 1;
    inpt_1.estep = inpt_1.efinal / inpt_1.nstep;
    for (i__ = 1; i__ <= 2002; ++i__) {
	j = i__ - 1;
	aj = (double) j;
	mix2_1.e[i__ - 1] = aj * inpt_1.estep;
/* L10: */
	mix2_1.eroot[i__ - 1] = ::sqrt(mix2_1.e[i__ - 1]);
    }
    mix2_1.eroot[0] = 1e-10;

/*   GAS PARAMETERS */

/* ********** hh ********** */
    sprintf(gasn_1.ngas1, "Ar");
    sprintf(gasn_1.ngas1, "CO2");
    sprintf(gasn_1.ngas1, "Ne");
    sprintf(gasn_1.ngas1, "He");
    ratio_1.frac1 = callpars_1.perc_ar__;
    ratio_1.frac2 = callpars_1.perc_co2__;
    ratio_1.frac3 = callpars_1.perc_ne__;
    ratio_1.frac4 = callpars_1.perc_he__;
    inpt_1.tempc = callpars_1.temperature;
    inpt_1.torr = callpars_1.pressure;
/* ********** hh ********** */
    corr = inpt_1.torr * (float)2.7316 / ((inpt_1.tempc + (float)273.16) * (
	    float)760.);
    ratio_1.an1 = ratio_1.frac1 * corr * alosch;
    ratio_1.an2 = ratio_1.frac2 * corr * alosch;
    ratio_1.an3 = ratio_1.frac3 * corr * alosch;
    ratio_1.an4 = ratio_1.frac4 * corr * alosch;
    inpt_1.akt = (inpt_1.tempc + (float)273.16) * boltz;
    ratio_1.an = corr * (float)100. * alosch;

/*   FIELD VALUES */

    mag_1.emag = callpars_1.e_magnitude__;
    mag_1.bmag = callpars_1.b_magnitude__;
    mag_1.btheta = callpars_1.b_angle__;
/*   HH end *************************** */
    *last = 0;
/* ********** MK ********** */
    mag_1.wb = awb * mag_1.bmag;
    for (j = 1; j <= 2002; ++j) {
	f1c_1.f1[j - 1] = (float)0.;
	f1c_1.df1[j - 1] = (float)0.;
	f2c_1.f2[j - 1] = (float)0.;
	f2c_1.df2[j - 1] = (float)0.;
	f3c_1.f3[j - 1] = (float)0.;
	f3c_1.df3[j - 1] = (float)0.;
	h1c_1.h1[j - 1] = (float)0.;
	h1c_1.dh1[j - 1] = (float)0.;
	g2c_1.g2[j - 1] = (float)0.;
	g2c_1.dg2[j - 1] = (float)0.;
	g1c_1.g1[j - 1] = (float)0.;
	g1c_1.dg1[j - 1] = (float)0.;
	g0c_1.g[j - 1] = (float)0.;
	g0c_1.dg0[j - 1] = (float)0.;
	g0c_1.dg[j - 1] = (float)0.;
	f0c_1.f[j - 1] = (float)0.;
	f0c_1.df0[j - 1] = (float)0.;
/* L6: */
	f0c_1.df[j - 1] = (float)0.;
    }
    inpt_1.alpnew = (float)0.;
    inpt_1.alpold = (float)0.;
    inpt_1.alpnax = (float)0.;
    inpt_1.alpnay = (float)0.;
    inpt_1.alpnaz = (float)0.;
    inpt_1.alpoax = (float)0.;
    inpt_1.alpoay = (float)0.;
    inpt_1.alpoaz = (float)0.;
    inpt_1.alpha = (float)0.;
    return 0;
L99:
    *last = 1;
    return 0;
} /* setup_ */

int StFtpcMagboltz1::simp_(double *sum)
{
    /* System generated locals */
    int i__1;

    /* Local variables */
    static double aodd, even;
    static int i__, n0;

    aodd = (float)0.;
    even = (float)0.;
    i__1 = inpt_1.nstep;
    for (i__ = 2; i__ <= i__1; i__ += 2) {
/* L11: */
	aodd += sint_1.simf[i__ - 1];
    }
    n0 = inpt_1.nstep - 1;
    i__1 = n0;
    for (i__ = 3; i__ <= i__1; i__ += 2) {
/* L12: */
	even += sint_1.simf[i__ - 1];
    }
    *sum = inpt_1.estep * (sint_1.simf[0] + sint_1.simf[inpt_1.nstep] + aodd *
	     (float)4. + even * (float)2.) / (float)3.;
    return 0;
} /* simp_ */
int StFtpcMagboltz1::stepph_(int *l)
{
    /* System generated locals */
    double d__1;
    int c__2=2;
    /* Local variables */
    static double frac;
    static int k;
    static double accur, dxold;
    static int kstep;
    static double dhfrs1, dhfrs2, h01, dhfnal, dhlast, dhstep, dhfrst;
    static int kprint;
    static double dum, dxx;

    if (*l == 2) {
	goto L21;
    }
    h1calc_(&c__1, &dum, &dum, &dum);
    return 0;
/*  ACCURACY 3.0% */
L21:
    accur = (float).02;
    dxold = (float)0.;
    dhlast = (float)0.;
    kprint = 0;
    kstep = 0;
/*   INITIALLY USE WIDE STEPS */
    dhstep = (float)100.;
    dhfnal = (float)1.0000000000000011e-10;
L146:
    h1calc_(&c__2, &dhfnal, &dxx, &dhfrst);
L10:
    h01 = dhfnal;
    dhfrs1 = dhfrst;
    ++kstep;
    if (kstep > 9) {
	goto L160;
    }
    dhfnal *= dhstep;
    h1calc_(&c__2, &dhfnal, &dxx, &dhfrst);
    dhfrs2 = dhfrst;
    if (dhfrs1 / dhfrs2 <= (float)0.) {
	goto L20;
    }
    if (kstep > 1) {
	goto L10;
    }
    if (fabs(dhfrs1) > fabs(dhfrs2)) {
	goto L10;
    }
    dhstep = (float)1. / dhstep;
    goto L10;
/* SMALLER STEPS CLOSER TO MINIMUM */
L20:
    if (dhstep > (float)1.) {
	goto L55;
    }
L25:
    dhstep = h01 * (float).5;
L30:
    dhfnal = h01;
    dhfnal -= dhstep;
    ++kstep;
    h1calc_(&c__2, &dhfnal, &dxx, &dhfrst);
    if (dhlast == dhfrst) {
	goto L60;
    }
    dhlast = dhfrst;
    dhfrs2 = dhfrst;
    frac = (d__1 = (dxx - dxold) / dxx, fabs(d__1));
    if (frac < accur) {
	goto L70;
    }
    dxold = dxx;
/* L46: */
    if (dhfrs1 / dhfrs2 <= (float)0.) {
	goto L50;
    }
    h01 = dhfnal;
    dhfrs1 = dhfrst;
L50:
    dhstep *= (float).5;
    goto L30;
L55:
    h01 = dhfnal;
    dhfrs1 = dhfrs2;
    goto L25;
L60:
    printf(" WARNING TRANSVERSE DIFFUSION DID NOT CONVERGE IN %d ITERATIONS . LAST VALUE DXX=%f\n REMEDY:INCREASE COMPUTER PRECISION.  IF FAIL AT FLOAT*8 THEN REDUCE UPPER INTEGRATION ENERGY LIMIT.\n",kstep,dxx);
    for (k = 1; k <= 2002; ++k) {
/* L91: */
	h1c_1.h1[k - 1] = (float)0.;
    }
    return 0;
L70:
    printf("TRANSVERSE DIFFUSION CONVERGED AFTER %d ITERATIONS.\n",kstep);
    return 0;
L160:
    if (dhfnal < (float)0.) {
	goto L876;
    }
    dhfnal = (float)-1.0000000000000011e-10;
    dhstep = (float)100.;
    kstep = 0;
    goto L146;
L876:
    printf(" WARNING  TRANSVERSE  DIFFUSION DID NOT APPROACH CONVGENCE.\n");
    return 0;
} /* stepph_ */

int StFtpcMagboltz1::steppr_(int *itype, int *lmax)
{

    /* System generated locals */
    int i__1;
    double d__1;

    /* Local variables */
    static double frac;
    static int j, k;
    static double accur, dlold, gstep;
    static int kstep;
    static double eg0sum, egsum1, egsum2, g01, dl, gfinal, sum;

/*  ACCURACY 0.5% */
    accur = (float).003;
    dlold = (float)0.;
    kstep = 0;
/*   INITIALLY USE WIDE STEPS */
    gstep = (float).01;
    gfinal = (float).010000000000000002;
    g0calc_(&c__0, &gfinal, &eg0sum, lmax);
L10:
    g01 = gfinal;
    egsum1 = eg0sum;
    ++kstep;
    if (kstep > 10) {
	goto L876;
    }
    gfinal *= gstep;
    g0calc_(&c__0, &gfinal, &eg0sum, lmax);
    egsum2 = eg0sum;
    if (egsum1 / egsum2 <= (float)0.) {
	goto L25;
    }
    goto L10;
/* SMALLER STEPS CLOSER TO MINIMUM */
L25:
    gstep = g01;
L30:
    gstep *= (float).5;
    gfinal = g01;
    gfinal -= gstep;
    ++kstep;
    g0calc_(itype, &gfinal, &eg0sum, lmax);
    if (kstep > 40) {
	goto L60;
    }
    i__1 = inpt_1.nstep1;
    for (j = 1; j <= i__1; ++j) {
/* L161: */
	sint_1.simf[j - 1] = g1c_1.g1[j - 1] * mix2_1.e[j - 1];
    }
    simp_(&sum);
    dl = sum * mag_1.eovm / (float)3.;
    egsum2 = eg0sum;
    frac = (d__1 = (dl - dlold) / dl, fabs(d__1));
    if (frac < accur) {
	goto L70;
    }
    dlold = dl;
    if (egsum1 / egsum2 <= (float)0.) {
	goto L30;
    }
    g01 = gfinal;
    egsum1 = eg0sum;
    goto L30;
L60:
    printf(" WARNING LONGITUDINAL DIFFUSION DID NOT CONVERGE IN       %d ITERATIONS . LAST VALUE DL =%f\n",kstep,dl);
    for (k = 1; k <= 2002; ++k) {
/* L823: */
	g1c_1.g1[k - 1] = (float)0.;
    }
    return 0;
L70:
    printf("LONGITUDINAL DIFFUSION CONVERGED AFTER %d ITERATIONS.\n",kstep);
    return 0;
L876:
    printf(" WARNING LONGITUDINAL DIFFUSION DID NOT APPROACH CONVERGENCE.\n");
    return 0;
} /* steppr_ */


int StFtpcMagboltz1::type2_(double *s2sum, int *i__, int *nstep1)
{
    /* System generated locals */
    int i__1;

    /* Local variables */
    static double s1sum;
    static int j, ldn, lup;


    s1sum = (float)0.;
    *s2sum = (float)0.;
    if (mix4_1.n2ro1 == 0) {
	goto L1000;
    }
    i__1 = mix4_1.n2ro1;
    for (j = 1; j <= i__1; ++j) {
	s1sum += f0c_1.f[*i__ - 1] * (mix4_1.q2ro1[((j + *i__ * 3) << 1) - 8] + 
		mix4_1.q2ro1[((j + *i__ * 3) << 1) - 7]);
	lup = *i__ + mix4_1.l2ro1[j - 1];
	if (lup >= *nstep1) {
	    goto L50;
	}
	*s2sum = *s2sum + f0c_1.f[lup - 1] * mix4_1.q2ro1[((j + lup * 3) << 1) 
		- 8] + (f0c_1.f[lup] * mix4_1.q2ro1[((j + (lup + 1) * 3) << 1) 
		- 8] - f0c_1.f[lup - 1] * mix4_1.q2ro1[((j + lup * 3) << 1) - 8]
		) * mix4_1.al2ro1[j - 1];
L50:
	;
    }
    i__1 = mix4_1.n2ro1;
    for (j = 2; j <= i__1; ++j) {
	ldn = *i__ - mix4_1.l2ro1[j - 2];
	if (ldn < 1) {
	    goto L100;
	}
	*s2sum = *s2sum + f0c_1.f[ldn - 1] * mix4_1.q2ro1[((j + ldn * 3) << 1) 
		- 7] + (f0c_1.f[ldn] * mix4_1.q2ro1[((j + (ldn + 1) * 3) << 1) 
		- 7] - f0c_1.f[ldn - 1] * mix4_1.q2ro1[((j + ldn * 3) << 1) - 7]
		) * mix4_1.al2ro1[j - 1];
L100:
	;
    }
L1000:
    if (mix4_1.n2ro2 == 0) {
	goto L2001;
    }
    i__1 = mix4_1.n2ro2;
    for (j = 1; j <= i__1; ++j) {
	s1sum += f0c_1.f[*i__ - 1] * (mix4_1.q2ro2[((j + *i__ * 3) << 1) - 8] + 
		mix4_1.q2ro2[((j + *i__ * 3) << 1) - 7]);
	lup = *i__ + mix4_1.l2ro2[j - 1];
	if (lup >= *nstep1) {
	    goto L150;
	}
	*s2sum = *s2sum + f0c_1.f[lup - 1] * mix4_1.q2ro2[((j + lup * 3) << 1) 
		- 8] + (f0c_1.f[lup] * mix4_1.q2ro2[((j + (lup + 1) * 3) << 1) 
		- 8] - f0c_1.f[lup - 1] * mix4_1.q2ro2[((j + lup * 3) << 1) - 8]
		) * mix4_1.al2ro2[j - 1];
L150:
	;
    }
    i__1 = mix4_1.n2ro2;
    for (j = 2; j <= i__1; ++j) {
	ldn = *i__ - mix4_1.l2ro2[j - 2];
	if (ldn < 1) {
	    goto L200;
	}
	*s2sum = *s2sum + f0c_1.f[ldn - 1] * mix4_1.q2ro2[((j + ldn * 3) << 1) 
		- 7] + (f0c_1.f[ldn] * mix4_1.q2ro2[((j + (ldn + 1) * 3) << 1) 
		- 7] - f0c_1.f[ldn - 1] * mix4_1.q2ro2[((j + ldn * 3) << 1) - 7]
		) * mix4_1.al2ro2[j - 1];
L200:
	;
    }
L2001:
    if (mix4_1.n2ro3 == 0) {
	goto L3000;
    }
    i__1 = mix4_1.n2ro3;
    for (j = 1; j <= i__1; ++j) {
	s1sum += f0c_1.f[*i__ - 1] * (mix4_1.q2ro3[((j + *i__ * 3) << 1) - 8] + 
		mix4_1.q2ro3[((j + *i__ * 3) << 1) - 7]);
	lup = *i__ + mix4_1.l2ro3[j - 1];
	if (lup >= *nstep1) {
	    goto L250;
	}
	*s2sum = *s2sum + f0c_1.f[lup - 1] * mix4_1.q2ro3[((j + lup * 3) << 1) 
		- 8] + (f0c_1.f[lup] * mix4_1.q2ro3[((j + (lup + 1) * 3) << 1) 
		- 8] - f0c_1.f[lup - 1] * mix4_1.q2ro3[((j + lup * 3) << 1) - 8]
		) * mix4_1.al2ro3[j - 1];
L250:
	;
    }
    i__1 = mix4_1.n2ro3;
    for (j = 2; j <= i__1; ++j) {
	ldn = *i__ - mix4_1.l2ro3[j - 2];
	if (ldn < 1) {
	    goto L300;
	}
	*s2sum = *s2sum + f0c_1.f[ldn - 1] * mix4_1.q2ro3[((j + ldn * 3) << 1) 
		- 7] + (f0c_1.f[ldn] * mix4_1.q2ro3[((j + (ldn + 1) * 3) << 1) 
		- 7] - f0c_1.f[ldn - 1] * mix4_1.q2ro3[((j + ldn * 3) << 1) - 7]
		) * mix4_1.al2ro3[j - 1];
L300:
	;
    }
L3000:
    if (mix4_1.n2ro4 == 0) {
	goto L4000;
    }
    i__1 = mix4_1.n2ro4;
    for (j = 1; j <= i__1; ++j) {
	s1sum += f0c_1.f[*i__ - 1] * (mix4_1.q2ro4[((j + *i__ * 3) << 1) - 8] + 
		mix4_1.q2ro4[((j + *i__ * 3) << 1) - 7]);
	lup = *i__ + mix4_1.l2ro4[j - 1];
	if (lup >= *nstep1) {
	    goto L350;
	}
	*s2sum = *s2sum + f0c_1.f[lup - 1] * mix4_1.q2ro4[((j + lup * 3) << 1) 
		- 8] + (f0c_1.f[lup] * mix4_1.q2ro4[((j + (lup + 1) * 3) << 1) 
		- 8] - f0c_1.f[lup - 1] * mix4_1.q2ro4[((j + lup * 3) << 1) - 8]
		) * mix4_1.al2ro4[j - 1];
L350:
	;
    }
    i__1 = mix4_1.n2ro4;
    for (j = 2; j <= i__1; ++j) {
	ldn = *i__ - mix4_1.l2ro4[j - 2];
	if (ldn < 1) {
	    goto L400;
	}
	*s2sum = *s2sum + f0c_1.f[ldn - 1] * mix4_1.q2ro4[((j + ldn * 3) << 1) 
		- 7] + (f0c_1.f[ldn] * mix4_1.q2ro4[((j + (ldn + 1) * 3) << 1) 
		- 7] - f0c_1.f[ldn - 1] * mix4_1.q2ro4[((j + ldn * 3) << 1) - 7]
		) * mix4_1.al2ro4[j - 1];
L400:
	;
    }
L4000:
    *s2sum -= s1sum;
    return 0;
} /* type2_ */

int StFtpcMagboltz1::type2g_(double *s2sum, int *i__, int *nstep1)
{
    /* System generated locals */
    int i__1;

    /* Local variables */
    static double s1sum;
    static int j, ldn, lup;


    s1sum = (float)0.;
    *s2sum = (float)0.;
    if (mix4_1.n2ro1 == 0) {
	goto L1000;
    }
    i__1 = mix4_1.n2ro1;
    for (j = 1; j <= i__1; ++j) {
	s1sum += g0c_1.g[*i__ - 1] * (mix4_1.q2ro1[((j + *i__ * 3) << 1) - 8] + 
		mix4_1.q2ro1[((j + *i__ * 3) << 1) - 7]);
	lup = *i__ + mix4_1.l2ro1[j - 1];
	if (lup >= *nstep1) {
	    goto L50;
	}
	*s2sum = *s2sum + g0c_1.g[lup - 1] * mix4_1.q2ro1[((j + lup * 3) << 1) 
		- 8] + (g0c_1.g[lup] * mix4_1.q2ro1[((j + (lup + 1) * 3) << 1) 
		- 8] - g0c_1.g[lup - 1] * mix4_1.q2ro1[((j + lup * 3) << 1) - 8]
		) * mix4_1.al2ro1[j - 1];
L50:
	;
    }
    i__1 = mix4_1.n2ro1;
    for (j = 2; j <= i__1; ++j) {
	ldn = *i__ - mix4_1.l2ro1[j - 2];
	if (ldn < 1) {
	    goto L100;
	}
	*s2sum = *s2sum + g0c_1.g[ldn - 1] * mix4_1.q2ro1[((j + ldn * 3) << 1) 
		- 7] + (g0c_1.g[ldn] * mix4_1.q2ro1[((j + (ldn + 1) * 3) << 1) 
		- 7] - g0c_1.g[ldn - 1] * mix4_1.q2ro1[((j + ldn * 3) << 1) - 7]
		) * mix4_1.al2ro1[j - 1];
L100:
	;
    }
L1000:
    if (mix4_1.n2ro2 == 0) {
	goto L2001;
    }
    i__1 = mix4_1.n2ro2;
    for (j = 1; j <= i__1; ++j) {
	s1sum += g0c_1.g[*i__ - 1] * (mix4_1.q2ro2[((j + *i__ * 3) << 1) - 8] + 
		mix4_1.q2ro2[((j + *i__ * 3) << 1) - 7]);
	lup = *i__ + mix4_1.l2ro2[j - 1];
	if (lup >= *nstep1) {
	    goto L150;
	}
	*s2sum = *s2sum + g0c_1.g[lup - 1] * mix4_1.q2ro2[((j + lup * 3) << 1) 
		- 8] + (g0c_1.g[lup] * mix4_1.q2ro2[((j + (lup + 1) * 3) << 1) 
		- 8] - g0c_1.g[lup - 1] * mix4_1.q2ro2[((j + lup * 3) << 1) - 8]
		) * mix4_1.al2ro2[j - 1];
L150:
	;
    }
    i__1 = mix4_1.n2ro2;
    for (j = 2; j <= i__1; ++j) {
	ldn = *i__ - mix4_1.l2ro2[j - 2];
	if (ldn < 1) {
	    goto L200;
	}
	*s2sum = *s2sum + g0c_1.g[ldn - 1] * mix4_1.q2ro2[((j + ldn * 3) << 1) 
		- 7] + (g0c_1.g[ldn] * mix4_1.q2ro2[((j + (ldn + 1) * 3 )<< 1) 
		- 7] - g0c_1.g[ldn - 1] * mix4_1.q2ro2[((j + ldn * 3) << 1) - 7]
		) * mix4_1.al2ro2[j - 1];
L200:
	;
    }
L2001:
    if (mix4_1.n2ro3 == 0) {
	goto L3000;
    }
    i__1 = mix4_1.n2ro3;
    for (j = 1; j <= i__1; ++j) {
	s1sum += g0c_1.g[*i__ - 1] * (mix4_1.q2ro3[((j + *i__ * 3) << 1) - 8] + 
		mix4_1.q2ro3[((j + *i__ * 3) << 1) - 7]);
	lup = *i__ + mix4_1.l2ro3[j - 1];
	if (lup >= *nstep1) {
	    goto L250;
	}
	*s2sum = *s2sum + g0c_1.g[lup - 1] * mix4_1.q2ro3[((j + lup * 3) << 1) 
		- 8] + (g0c_1.g[lup] * mix4_1.q2ro3[((j + (lup + 1) * 3) << 1) 
		- 8] - g0c_1.g[lup - 1] * mix4_1.q2ro3[((j + lup * 3) << 1) - 8]
		) * mix4_1.al2ro3[j - 1];
L250:
	;
    }
    i__1 = mix4_1.n2ro3;
    for (j = 2; j <= i__1; ++j) {
	ldn = *i__ - mix4_1.l2ro3[j - 2];
	if (ldn < 1) {
	    goto L300;
	}
	*s2sum = *s2sum + g0c_1.g[ldn - 1] * mix4_1.q2ro3[((j + ldn * 3) << 1) 
		- 7] + (g0c_1.g[ldn] * mix4_1.q2ro3[((j + (ldn + 1) * 3) << 1) 
		- 7] - g0c_1.g[ldn - 1] * mix4_1.q2ro3[((j + ldn * 3) << 1) - 7]
		) * mix4_1.al2ro3[j - 1];
L300:
	;
    }
L3000:
    if (mix4_1.n2ro4 == 0) {
	goto L4000;
    }
    i__1 = mix4_1.n2ro4;
    for (j = 1; j <= i__1; ++j) {
	s1sum += g0c_1.g[*i__ - 1] * (mix4_1.q2ro4[((j + *i__ * 3) << 1) - 8] + 
		mix4_1.q2ro4[((j + *i__ * 3) << 1) - 7]);
	lup = *i__ + mix4_1.l2ro4[j - 1];
	if (lup >= *nstep1) {
	    goto L350;
	}
	*s2sum = *s2sum + g0c_1.g[lup - 1] * mix4_1.q2ro4[((j + lup * 3) << 1) 
		- 8] + (g0c_1.g[lup] * mix4_1.q2ro4[((j + (lup + 1) * 3) << 1) 
		- 8] - g0c_1.g[lup - 1] * mix4_1.q2ro4[((j + lup * 3) << 1) - 8]
		) * mix4_1.al2ro4[j - 1];
L350:
	;
    }
    i__1 = mix4_1.n2ro4;
    for (j = 2; j <= i__1; ++j) {
	ldn = *i__ - mix4_1.l2ro4[j - 2];
	if (ldn < 1) {
	    goto L400;
	}
	*s2sum = *s2sum + g0c_1.g[ldn - 1] * mix4_1.q2ro4[((j + ldn * 3) << 1) 
		- 7] + (g0c_1.g[ldn] * mix4_1.q2ro4[((j + (ldn + 1) * 3) << 1) 
		- 7] - g0c_1.g[ldn - 1] * mix4_1.q2ro4[((j + ldn * 3) << 1) - 7]
		) * mix4_1.al2ro4[j - 1];
L400:
	;
    }
L4000:
    *s2sum -= s1sum;
    return 0;
} /* type2g_ */















