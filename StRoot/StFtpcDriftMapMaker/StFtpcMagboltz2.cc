// $Id: StFtpcMagboltz2.cc,v 1.3 2003/04/30 20:37:15 perev Exp $
//
// $Log: StFtpcMagboltz2.cc,v $
// Revision 1.3  2003/04/30 20:37:15  perev
// Warnings cleanup. Modified lines marked VP
//
// Revision 1.2  2002/03/05 16:59:15  jcs
// force double to long for HepRandom
//
// Revision 1.1  2000/12/20 08:44:02  jcs
// Replace pam/ftpc/fmg with maker
//

#include "StFtpcMagboltz2.hh"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "Random.h"

// random number engines from StarClassLibrary
#include "RandFlat.h"

StFtpcMagboltz2::StFtpcMagboltz2()
{
  /* Table of constant values */
  c__1 = 1;
  c_b524 = 10.;
  c_b1350 = -1.118;
  c_b1360 = 1.522;
  c_b1361 = -2.821;
  mBench = new TBenchmark();
}

StFtpcMagboltz2::~StFtpcMagboltz2()
{
  delete mBench;
}

/*  PROGRAM MAGBOLTZ 2  CALCULATES DRIFT AND DIFFUSION IN E AND B FIELDS. */
/*  VERSION 3.1 MAR  20000 */
/*  ------------------------------------------------------------------- */
/*  MAR 2000 INCREASED MAX ARRAY SIZE FOR NUMBER OF LEVELS FROM 32 TO 64 */
/*           ALSO CHANGED MIXER ROUTINE TO INCREASE NUMBER OF GASSES */
/*           IN DATA BASE FROM 30 TO 60 */
/*  --------------------------------------------------------------------- */
/*  PROGRAM USES SAME DATA BASE AS THE BOLTZMANN CALCULATION . */
/*  THE PROGRAM IS ONLY LIMITED IN ACCURACY BY THE STATISTICAL PRECISION */
/*  OF THE RESULTS IT IS EASY TO OBTAIN A STATISTICAL PRECISION */
/*  OF BETTER THAN 0.1% ON THE DRIFT VELOCITY AND 1% ON THE */
/*  DIFFUSION COEFICIENTS IN MOST COUNTING GAS MIXTURES IN ABOUT */
/*  10 SECONDS OF COMPUTING TIME ON A PC , ALPHA OR WORKSTATION. */
/*  THE STATISTICAL PRECISION VARIES AS THE SQUARE ROOT OF THE COMPUTING */
/*  TIME. */
/*  THE CODE USES ONLY THE ORIGINAL ELECTRON IN THE IONISATION COLLISIONS */
/*  AND THEREFORE SHOULD ONLY BE COMPARED WITH THE BOLTZMANN CODE WITH */
/*  NALPHA=0. */
/*  THE MONTE CARLO FOR THE CASE WHERE THE GENERATED IONISATION ELECTRONS */
/*  ARE INCLUDED CAN BE OBTAINED FROM THE AUTHOR. (SFB@HEP.PH.LIV.AC.UK) */
/* ---------------------------------------------------------------------- */
/*  IHE MAGNETIC FIELD CAN BE AT ANY ANGLE WITH RESPECT TO THE ELECTRIC */
/*  FIELD. */
/*  THE PROGRAM ALLOWS ANISOTROPIC ELASTIC AND INELASTIC SCATTERING : */
/*  REF : NIM A 421 (1999) 234-240 */
/*   THE GAS DATA BASE LIST BELOW SHOWS THOSE X-SECTIONS WHICH CONTAIN */
/*   ANISOTROPIC SCATTERING DATA. */
/* -------------------------------------------------------------------- */
/*  GEOMETRY: */
/* -------------- */
/*            THE ELECTRIC FIELD IS TAKEN ALONG THE Z-AXIS AND THE */
/*   MAGNETIC FIELD IS TAKEN IN THE Z-X PLANE AT AN ANGLE, BTHETA , */
/*   TO THE ELECTRIC FIELD. */

/*  THE RESULTS OF THE CALCULATION ARE LOADED INTO COMMON BLOCKS: */
/*  COMMON/VEL/WX,WY,WZ */
/*  COMMON/DIFLAB/DIFXX,DIFYY,DIFZZ,DIFYZ,DIFXY,DIFXZ */
/*  COMMON/DIFVEL/DIFLN,DIFTR */

/*    WX,WY,WZ ARE THE DRIFT VELOCITY VECTORS */
/*    DIFXX,DIFYY,DIFZZ,DIFYZ,DIFXY,DIFXZ ARE THE VALUES OF THE DIFFUSION */
/*    TENSOR IN THE CARTESIAN COORDINATE SYSTEM. */
/*    ------------------------------- */
/*    NOTE : OFF-DIAGONAL ELEMENTS ARE DEFINED SO THAT THE COEFFICIENTS */
/*    ARE EQUAL :  DIFXY=DIFYX   ,   DIFXZ=DIFZX  AND DIFYZ=DIFZY . */
/*    ----------------------------- */
/*    DIFLN,DIFTR,DIFXX ARE THE DIFFUSION COEFFICIENTS IN THE COORDINATE */
/*    SYSTEM ALIGNED ALONG THE DRIFT DIRECTION (IT IS ONLY CALCULATED */
/*    FOR THE CASE WHERE THE MAGNETIC FIELD IS AT 90 DEGREES TO EFIELD). */
/*    IF THERE IS NO MAGNETIC FIELD THE VALUES DIFLN AND DIFTR */
/*     REPRESENT THE LONGITUDINAL AND TRANSVERSE DIFFUSION. */

/*    OUTPUT UNITS IN ARRAYS : VELOCITY :CM/SEC */
/*                           DIFFUSION: CM**2/SEC */

/* --------------------------- */
/*   INPUT CARDS : */
/* ---------------------------------------------------------- */
/*  FIRST CARD: 2I10,F10.5  :  NGAS,NMAX,EFINAL */
/*  NGAS:  NUMBER OF GASES IN MIXTURE */
/*  NMAX: NUMBER OF REAL AND NULL COLLISIONS (MULTIPLE OF 960000 ) */
/*  USE NMAX =BETWEEN 1 OR 2 FOR INELASTIC GAS TO OBTAIN 1% ACCURACY */
/*      NAMX = ABOVE 10 FOR BETTER THAN 0.5% ACCURACY. */
/*      NMAX = AT LEAST 10 FOR PURE ELASTIC GASES LIKE ARGON */
/*  EFINAL= UPPER LIMIT OF THE ELECTRON ENERGY IN ELECTRON VOLTS. */
/*  EFINAL= 0.0 (PROGRAM AUTOMATICALLY CALCULATES UPPER INTEGRATION */
/*                 ENERGY LIMIT) */
/* ------------------------------------------------------------- */
/*  SECOND CARD : 4I5   : NGAS1 , NGAS2, NGAS3 , NGAS4 */
/*       NGAS1,ETC :  GAS NUMBER IDENTIFIERS (BETWEEN 1 AND 28) */
/*                   SEE GAS LIST BELOW FOR IDENTIFYING NUMBERS. */

/* ------------------------------------------------------------- */
/* THIRD CARD: 6F10.4  : FRAC1,FRAC2,FRAC3,FRAC4,TEMP,TORR */
/*  FRAC1,ETC : PERCENTAGE FRACTION OF GAS1,ETC */
/*  TEMP : TEMPERATURE OF GAS IN CENTIGRADE */
/*  TORR :  PRESSURE OF GAS IN TORR */
/* ------------------------------------------------------------ */
/* FOURTH CARD : 6F10.3  : EFIELD,BMAG,BTHETA */
/*  EFIELD : ELECTRIC FIELD IN VOLTS/ CM. */
/*   BMAG  : MAGNITUDE OF THE MAGNETIC FIELD IN KILOGAUSS */
/*  BTHETA : ANGLE BETWEEN THE ELECTRIC AND MAGNETIC FIELDS IN DEGREES. */
/* ----------------------------------------------------------------------- */
/* CARD 4*N+1 USES NGAS=0 TO TERMINATE CORRECTLY */
/* -------------------------------------------------------------------- */
/* DATA BASE: */

/* GAS NUMBER:                                           STAR RATING: */
/* ----------------------------------------------------------------- */
/* GAS1  :  CF4             (1999) (ANISOTROPIC SCATTERING ONLY)    5* */
/* GAS2  :  ARGON           (1997)                                  5* */
/* GAS3  :  HELIUM 4        (1997)                                  5* */
/* GAS4  :  HELIUM 3        (1992)                                  5* */
/* GAS5  :  NEON            (1992)                                  5* */
/* GAS6  :  KRYPTON         (1989)                                  4* */
/* GAS7  :  XENON           (1989)                                  4* */
/* GAS8  :  METHANE         (1994)                                  5* */
/* GAS9  :  ETHANE          (1999)                                  5* */
/* GAS10 :  PROPANE         (1999)                                  4* */
/* GAS11 :  ISOBUTANE       (1999)                                  3* */
/* GAS12 :  CO2             (1997)                                  5* */
/* GAS13 :  NEO-PENTANE     (1995)  C(CH3)4                         3* */
/* GAS14 :  H20             (1998)                                  3* */
/* GAS15 :  OXYGEN          (1990)  3-BODY ATTACHMENT INCLUDED      4* */
/* GAS16 :  NITROGEN         (      PITCHFORD AND PHELPS )          4* */
/* GAS17 :  NITRIC OXIDE    (1995)  ATTACHING GAS                   4* */
/* GAS18 :  NITROUS OXIDE   (1995)  ATTACHING GAS                   4* */
/* GAS19 :  ETHENE          (1999)   C2H4                           4* */
/* GAS20 :  ACETYLENE       (1992)   C2H2                           3* */
/* GAS21 :  HYDROGEN        (1998)                                  5* */
/* GAS22 :  DEUTERIUM       (1998)                                  5* */
/* GAS23 :  CARBON MONOXIDE (1998)                                  5* */
/* GAS24 :  METHYLAL        (1988)                                  2* */
/* GAS25 :  DME             (1998)                                  4* */
/* GAS26 :  REID STEP MODEL         (ANISOTROPIC VERSION) */
/* GAS27 :  MAXWELL MODEL */
/* GAS28 :  REID RAMP MODEL */
/* GAS29 :  C2F6             (1999)   (ANISOTROPIC )                4* */
/* GAS30 :  SF6   N.B.  DO NOT USE HIGH PERCENTAGE                  3* */
/* GAS31 :  NH3 AMMONIA        (1999)                               3* */
/* GAS32 :  C3H6   PROPENE     (1999)                               4* */
/* GAS33 :  C3H6  CYCLOPROPANE (1999)                               4* */
/* GAS34 :  CH3OH METHANOL     (1999)                               2* */
/* GAS35 :  C2H5OH ETHANOL     (1999)                               3* */
/* GAS36 :  C3H7OH ISO PROPANOL(1999)                               2* */
/*   GAS 37-60 : DUMMY ROUTINES */
/* ------------------------------------------------------------------ */

int StFtpcMagboltz2::magboltz_(float *e_magni__, float *b_magni__,
			       float *b_ang__, float *press, 
			       float *p_ar__, float *p_co2__,
			       float *p_ne__, float *p_he__, float *temper,
			       float *vdr, float *psiang, float *efin)
{
    /* Local variables */
    static int ielow;

    printf("~~~~~~~magboltz started\n");
    setup_(e_magni__, b_magni__, b_ang__, press, p_ar__,
	   p_co2__, p_ne__, p_he__, temper);
    if (inpt_1.efinal > 0.) {
	goto L3;
    }
/* CALCULATE EFINAL (START AT 0.5 EV.) */
    inpt_1.efinal = .5;
L2:
    mixer_();
/* LOOP TO CALCULATE EFINAL */
    if (bfld_1.bmag == 0. || bfld_1.btheta == 0. || bfld_1.btheta == 180.) {
	elimit_(&ielow);
    }
    if (bfld_1.bmag != 0.) {
	if (bfld_1.btheta == 90.) {
	    elimitb_(&ielow);
	}
	if (bfld_1.btheta > 0. && bfld_1.btheta < 90. || bfld_1.btheta > 90. 
		&& bfld_1.btheta < 180.) {
	    elimitc_(&ielow);
	}
    }
    
    if (ielow == 1) {
	inpt_1.efinal *= sqrt(2.);
	setp_1.estart = inpt_1.efinal / 20.;
	goto L2;
    }
    printf("EFINAL = %f\n", inpt_1.efinal);
    goto L4;
L3:
    mixer_();
L4:
    prnter_();
    if (bfld_1.bmag == 0.) {
	monte_();
    }
    if (bfld_1.bmag != 0.) {
      if (bfld_1.btheta == 0. || bfld_1.btheta == 180.) {
	montea_();
      } else if (bfld_1.btheta == 90.) {
	monteb_();
      } else {
	montec_();
      }
    }
    output_();

    *vdr=vel_1.wz/(double)1e6;
    *psiang = atan(vel_1.wy / vel_1.wz) * 180. / acos(-1.);
    
    printf("~~~~~~~magboltz done\n");
    return 1;
} /* magboltz */

int StFtpcMagboltz2::mixer_()
{
    /* System generated locals */
    int i__1, i__2;

    /* Local variables */
    static int kpin;
    static double elow, qatt[8008]	/* was [4][2002] */;
    static int jlow;
    static double rgas1, rgas2, rgas3, rgas4;
    static int i__, j, l;
    static double ehalf, e1[6], e2[6], e3[6], e4[6], f2, q1[12012]	/* 
	    was [6][2002] */, q2[12012]	/* was [6][2002] */, q3[12012]	/* 
	    was [6][2002] */, q4[12012]	/* was [6][2002] */, peqel1[2002], 
	    peqel2[2002], peqel3[2002], peqel4[2002], peqin1[4004]	/* 
	    was [2][2002] */, peqin2[4004]	/* was [2][2002] */, peqin3[
	    4004]	/* was [2][2002] */, peqin4[4004]	/* was [2][
	    2002] */, aj;
    static int ie, if__;
    static double bp;
    static int np;
    static double ei1[20], ei2[20], ei3[20], ei4[20], virial1, virial2, 
	    virial3, virial4, ehi;
    static int jhi, kel1, kel2, kel3, kel4, kin1[2], kin2[2], kin3[2], 
	    kin4[2];

/*  --------------------------------------------------------------------- 
*/

/*     SUBROUTINE MIXER FILLS ARRAYS OF COLLISION FREQUENCY */
/*     CAN HAVE A MIXTURE OF UP TO 4 GASES */


/*  --------------------------------------------------------------------- 
*/

    anis_1.niso = 0;
    kel1 = 0;
    kel2 = 0;
    kel3 = 0;
    kel4 = 0;
    mix3_1.nin1 = 0;
    mix3_1.nin2 = 0;
    mix3_1.nin3 = 0;
    mix3_1.nin4 = 0;
    sprintf(names_1.name1, "000000000000000");
    sprintf(names_1.name2, "000000000000000");
    sprintf(names_1.name3, "000000000000000");
    sprintf(names_1.name4, "000000000000000");
    for (j = 1; j <= 6; ++j) {
	for (i__ = 1; i__ <= 2002; ++i__) {
	    q1[j + i__ * 6 - 7] = 0.;
	    q2[j + i__ * 6 - 7] = 0.;
	    q3[j + i__ * 6 - 7] = 0.;
	    q4[j + i__ * 6 - 7] = 0.;
	}
	e1[j - 1] = 0.;
	e2[j - 1] = 0.;
	e3[j - 1] = 0.;
/* L2: */
	e4[j - 1] = 0.;
    }
    inpt_1.estep = inpt_1.efinal / inpt_1.nstep;
    ehalf = inpt_1.estep / 2.;
    mix2_1.e[0] = ehalf;
    for (i__ = 2; i__ <= 2002; ++i__) {
	aj = (double) (i__ - 1);
	mix2_1.e[i__ - 1] = ehalf + inpt_1.estep * aj;
/* L3: */
	mix2_1.eroot[i__ - 1] = sqrt(mix2_1.e[i__ - 1]);
    }
    mix2_1.eroot[0] = sqrt(ehalf);
    for (i__ = 1; i__ <= 2; ++i__) {
	kin1[i__ - 1] = 0;
	kin2[i__ - 1] = 0;
	kin3[i__ - 1] = 0;
/* L4: */
	kin4[i__ - 1] = 0;
    }
    for (i__ = 1; i__ <= 4; ++i__) {
/* L5: */
	anis_1.kel[i__ - 1] = 0;
    }
    for (i__ = 1; i__ <= 64; ++i__) {
/* L6: */
	anis_1.index[i__ - 1] = 0;
    }

/*   CALL GAS CROSS-SECTIONS */
    gasmix_(&gasn_1.ngas1, &anis_1.niso, q1, mix1_1.qin1, &mix3_1.nin1, e1, 
	    ei1, names_1.name1, &virial1, peqel1, peqin1, &kel1, kin1, 15L);
    if (inpt_1.ngas == 1) {
	goto L200;
    }
    gasmix_(&gasn_1.ngas2, &anis_1.niso, q2, mix1_1.qin2, &mix3_1.nin2, e2, 
	    ei2, names_1.name2, &virial2, peqel2, peqin2, &kel2, kin2, 15L);
    if (inpt_1.ngas == 2) {
	goto L200;
    }
    gasmix_(&gasn_1.ngas3, &anis_1.niso, q3, mix1_1.qin3, &mix3_1.nin3, e3, 
	    ei3, names_1.name3, &virial3, peqel3, peqin3, &kel3, kin3, 15L);
    if (inpt_1.ngas == 3) {
	goto L200;
    }
    gasmix_(&gasn_1.ngas4, &anis_1.niso, q4, mix1_1.qin4, &mix3_1.nin4, e4, 
	    ei4, names_1.name4, &virial4, peqel4, peqin4, &kel4, kin4, 15L);
L200:
/* --------------------------------------------------------------- */
/*  CORRECTION OF NUMBER DENSITY DUE TO VIRIAL COEFFICIENT */
/*  CAN BE PROGRAMMED HERE NOT YET IMPLEMENTED. */
/* ----------------------------------------------------------------- */
/* ----------------------------------------------------------------- */
/*     CALCULATION OF COLLISION FREQUENCIES FOR AN ARRAY OF */
/*     ELECTRON ENERGIES IN THE RANGE ZERO TO EFINAL */

/*     L=5*N-4    ELASTIC NTH GAS */
/*     L=5*N-3    IONISATION NTH GAS */
/*     L=5*N-2    ATTACHMENT NTH GAS */
/*     L=5*N-1    INELASTIC NTH GAS */
/*     L=5*N      SUPERELASTIC NTH GAS */
/* --------------------------------------------------------------- */
    anis_1.kel[0] = kel1;
    anis_1.kel[1] = kel2;
    anis_1.kel[2] = kel3;
    anis_1.kel[3] = kel4;
    for (ie = 1; ie <= 2000; ++ie) {
	kpin = 0;
	anis_1.pin[(ie << 3) - 8] = 0.;
	anis_1.pin[(ie << 3) - 7] = 0.;
	anis_1.pel[(ie << 2) - 4] = 0.;
	np = 1;
	l = 1;
	large_1.cf[ie + np * 2000 - 2001] = q1[ie * 6 - 5] * mratio_1.van1 * 
		1e15;
	if (kel1 == 1) {
	    anis_1.pel[(ie << 2) - 4] = peqel1[ie - 1];
	}
	rgas1 = e1[1] / 2. + 1.;
	large_1.rgas[np - 1] = rgas1;
	large_1.ein[np - 1] = 0.;
	large_1.ipn[np - 1] = 0;
	large_1.iarry[np - 1] = l;
	if (inpt_1.efinal < e1[2]) {
	    goto L230;
	}
	l = 2;
	++np;
	large_1.cf[ie + np * 2000 - 2001] = q1[ie * 6 - 4] * mratio_1.van1 * 
		1e15;
	large_1.rgas[np - 1] = rgas1;
	large_1.ein[np - 1] = e1[2] / rgas1;
	large_1.ipn[np - 1] = 1;
	large_1.iarry[np - 1] = l;
L230:
	if (inpt_1.efinal < e1[3]) {
	    goto L240;
	}
	l = 3;
	++np;
	large_1.cf[ie + np * 2000 - 2001] = q1[ie * 6 - 3] * mratio_1.van1 * 
		1e15;
	large_1.rgas[np - 1] = rgas1;
	large_1.ein[np - 1] = 0.;
	large_1.ipn[np - 1] = 0;
	large_1.iarry[np - 1] = l;
L240:
	if (mix3_1.nin1 == 0) {
	    goto L260;
	}
	i__1 = mix3_1.nin1;
	for (j = 1; j <= i__1; ++j) {
	    l = 4;
	    ++np;
	    large_1.cf[ie + np * 2000 - 2001] = mix1_1.qin1[j + ie * 20 - 21] 
		    * mratio_1.van1 * 1e15;
	    if (kin1[0] == j) {
		++kpin;
		anis_1.pin[kpin + (ie << 3) - 9] = peqin1[(ie << 1) - 2];
		anis_1.index[np - 1] = kpin;
	    }
	    if (kin1[1] == j) {
		++kpin;
		anis_1.pin[kpin + (ie << 3) - 9] = peqin1[(ie << 1) - 1];
		anis_1.index[np - 1] = kpin;
	    }
	    large_1.rgas[np - 1] = rgas1;
	    large_1.ein[np - 1] = ei1[j - 1] / rgas1;
	    if (ei1[j - 1] < 0.) {
		l = 5;
	    }
	    large_1.ipn[np - 1] = 0;
/* L250: */
	    large_1.iarry[np - 1] = l;
	}
L260:
	if (inpt_1.ngas == 1) {
	    goto L600;
	}
	anis_1.pin[(ie << 3) - 6] = 0.;
	anis_1.pin[(ie << 3) - 5] = 0.;
	anis_1.pel[(ie << 2) - 3] = 0.;
	++np;
	l = 6;
	large_1.cf[ie + np * 2000 - 2001] = q2[ie * 6 - 5] * mratio_1.van2 * 
		1e15;
	if (kel2 == 1) {
	    anis_1.pel[(ie << 2) - 3] = peqel2[ie - 1];
	}
	rgas2 = e2[1] / 2. + 1.;
	large_1.rgas[np - 1] = rgas2;
	large_1.ein[np - 1] = 0.;
	large_1.ipn[np - 1] = 0;
	large_1.iarry[np - 1] = l;
	if (inpt_1.efinal < e2[2]) {
	    goto L330;
	}
	l = 7;
	++np;
	large_1.cf[ie + np * 2000 - 2001] = q2[ie * 6 - 4] * mratio_1.van2 * 
		1e15;
	large_1.rgas[np - 1] = rgas2;
	large_1.ein[np - 1] = e2[2] / rgas2;
	large_1.ipn[np - 1] = 1;
	large_1.iarry[np - 1] = l;
L330:
	if (inpt_1.efinal < e2[3]) {
	    goto L340;
	}
	l = 8;
	++np;
	large_1.cf[ie + np * 2000 - 2001] = q2[ie * 6 - 3] * mratio_1.van2 * 
		1e15;
	large_1.rgas[np - 1] = rgas2;
	large_1.ein[np - 1] = 0.;
	large_1.ipn[np - 1] = 0;
	large_1.iarry[np - 1] = l;
L340:
	if (mix3_1.nin2 == 0) {
	    goto L360;
	}
	i__1 = mix3_1.nin2;
	for (j = 1; j <= i__1; ++j) {
	    l = 9;
	    ++np;
	    large_1.cf[ie + np * 2000 - 2001] = mix1_1.qin2[j + ie * 20 - 21] 
		    * mratio_1.van2 * 1e15;
	    if (kin2[0] == j) {
		++kpin;
		anis_1.pin[kpin + (ie << 3) - 9] = peqin2[(ie << 1) - 2];
		anis_1.index[np - 1] = kpin;
	    }
	    if (kin2[1] == j) {
		++kpin;
		anis_1.pin[kpin + (ie << 3) - 9] = peqin2[(ie << 1) - 1];
		anis_1.index[np - 1] = kpin;
	    }
	    large_1.rgas[np - 1] = rgas2;
	    large_1.ein[np - 1] = ei2[j - 1] / rgas2;
	    if (ei2[j - 1] < 0.) {
		l = 10;
	    }
	    large_1.ipn[np - 1] = 0;
/* L350: */
	    large_1.iarry[np - 1] = l;
	}
L360:
	if (inpt_1.ngas == 2) {
	    goto L600;
	}
	anis_1.pin[(ie << 3) - 4] = 0.;
	anis_1.pin[(ie << 3) - 3] = 0.;
	anis_1.pel[(ie << 2) - 2] = 0.;
	++np;
	l = 11;
	large_1.cf[ie + np * 2000 - 2001] = q3[ie * 6 - 5] * mratio_1.van3 * 
		1e15;
	if (kel3 == 1) {
	    anis_1.pel[(ie << 2) - 2] = peqel3[ie - 1];
	}
	rgas3 = e3[1] / 2. + 1.;
	large_1.rgas[np - 1] = rgas3;
	large_1.ein[np - 1] = 0.;
	large_1.ipn[np - 1] = 0;
	large_1.iarry[np - 1] = l;
	if (inpt_1.efinal < e3[2]) {
	    goto L430;
	}
	l = 12;
	++np;
	large_1.cf[ie + np * 2000 - 2001] = q3[ie * 6 - 4] * mratio_1.van3 * 
		1e15;
	large_1.rgas[np - 1] = rgas3;
	large_1.ein[np - 1] = e3[2] / rgas3;
	large_1.ipn[np - 1] = 1;
	large_1.iarry[np - 1] = l;
L430:
	if (inpt_1.efinal < e3[3]) {
	    goto L440;
	}
	l = 13;
	++np;
	large_1.cf[ie + np * 2000 - 2001] = q3[ie * 6 - 3] * mratio_1.van3 * 
		1e15;
	large_1.rgas[np - 1] = rgas3;
	large_1.ein[np - 1] = 0.;
	large_1.ipn[np - 1] = 0;
	large_1.iarry[np - 1] = l;
L440:
	if (mix3_1.nin3 == 0) {
	    goto L460;
	}
	i__1 = mix3_1.nin3;
	for (j = 1; j <= i__1; ++j) {
	    l = 14;
	    ++np;
	    large_1.cf[ie + np * 2000 - 2001] = mix1_1.qin3[j + ie * 20 - 21] 
		    * mratio_1.van3 * 1e15;
	    if (kin3[0] == j) {
		++kpin;
		anis_1.pin[kpin + (ie << 3) - 9] = peqin3[(ie << 1) - 2];
		anis_1.index[np - 1] = kpin;
	    }
	    if (kin3[1] == j) {
		++kpin;
		anis_1.pin[kpin + (ie << 3) - 9] = peqin3[(ie << 1) - 1];
		anis_1.index[np - 1] = kpin;
	    }
	    large_1.rgas[np - 1] = rgas3;
	    large_1.ein[np - 1] = ei3[j - 1] / rgas3;
	    if (ei3[j - 1] < 0.) {
		l = 15;
	    }
	    large_1.ipn[np - 1] = 0;
/* L450: */
	    large_1.iarry[np - 1] = l;
	}
L460:
	if (inpt_1.ngas == 3) {
	    goto L600;
	}
	anis_1.pin[(ie << 3) - 2] = 0.;
	anis_1.pin[(ie << 3) - 1] = 0.;
	anis_1.pel[(ie << 2) - 1] = 0.;
	++np;
	l = 16;
	large_1.cf[ie + np * 2000 - 2001] = q4[ie * 6 - 5] * mratio_1.van4 * 
		1e15;
	if (kel4 == 1) {
	    anis_1.pel[(ie << 2) - 1] = peqel4[ie - 1];
	}
	rgas4 = e4[1] / 2. + 1.;
	large_1.rgas[np - 1] = rgas4;
	large_1.ein[np - 1] = 0.;
	large_1.ipn[np - 1] = 0;
	large_1.iarry[np - 1] = l;
	if (inpt_1.efinal < e4[2]) {
	    goto L530;
	}
	l = 17;
	++np;
	large_1.cf[ie + np * 2000 - 2001] = q4[ie * 6 - 4] * mratio_1.van4 * 
		1e15;
	large_1.rgas[np - 1] = rgas4;
	large_1.ein[np - 1] = e4[2] / rgas4;
	large_1.ipn[np - 1] = 1;
	large_1.iarry[np - 1] = l;
L530:
	if (inpt_1.efinal < e4[3]) {
	    goto L540;
	}
	l = 18;
	++np;
	large_1.cf[ie + np * 2000 - 2001] = q4[ie * 6 - 3] * mratio_1.van4 * 
		1e15;
	large_1.rgas[np - 1] = rgas4;
	large_1.ein[np - 1] = 0.;
	large_1.ipn[np - 1] = 0;
	large_1.iarry[np - 1] = l;
L540:
	if (mix3_1.nin4 == 0) {
	    goto L560;
	}
	i__1 = mix3_1.nin4;
	for (j = 1; j <= i__1; ++j) {
	    l = 19;
	    ++np;
	    large_1.cf[ie + np * 2000 - 2001] = mix1_1.qin4[j + ie * 20 - 21] 
		    * mratio_1.van4 * 1e15;
	    if (kin4[0] == j) {
		++kpin;
		anis_1.pin[kpin + (ie << 3) - 9] = peqin4[(ie << 1) - 2];
		anis_1.index[np - 1] = kpin;
	    }
	    if (kin4[1] == j) {
		++kpin;
		anis_1.pin[kpin + (ie << 3) - 9] = peqin4[(ie << 1) - 1];
		anis_1.index[np - 1] = kpin;
	    }
	    large_1.rgas[np - 1] = rgas4;
	    large_1.ein[np - 1] = ei4[j - 1] / rgas4;
	    if (ei4[j - 1] < 0.) {
		l = 20;
	    }
	    large_1.ipn[np - 1] = 0;
/* L550: */
	    large_1.iarry[np - 1] = l;
	}
L560:

L600:
	large_1.iplast = np;
/* ---------------------------------------------------------------- */
/*   CAN INCREASE ARRAY SIZE UP TO 132 IF MORE COMPLEX MIXTURES USED. 
*/
/* ------------------------------------------------------------------ 
*/
	if (large_1.iplast > 64) {
	  printf("WARNING TOO MANY LEVELS IN CALCULATION. \nCAN INCREASE THE ARRAY SIZES FROM  64 UP TO 132 MAXIMUM");
	  return 0;
	}
/* ------------------------------------------------------------------
-- */
/*     CALCULATION OF TOTAL COLLISION FREQUENCY */
/* ------------------------------------------------------------------
--- */
	large_1.tcf[ie - 1] = 0.;
	i__1 = large_1.iplast;
	for (if__ = 1; if__ <= i__1; ++if__) {
	  large_1.tcf[ie - 1] += large_1.cf[ie + if__ * 2000 - 2001];
	  if (large_1.cf[ie + if__ * 2000 - 2001] < 0.) {
	    printf("WARNING NEGATIVE COLLISION FEQUENCY = %12.3e IE =%i IF =%i IARRY=%i EIN=%7.4f\n",large_1.cf[ie + if__ * 2000 - 2001], ie, if__, large_1.iarry[if__ - 1], large_1.ein[if__ - 1]);
	  }
	}
	i__1 = large_1.iplast;
	for (if__ = 1; if__ <= i__1; ++if__) {
	    if (large_1.tcf[ie - 1] == 0.) {
		goto L615;
	    }
	    large_1.cf[ie + if__ * 2000 - 2001] /= large_1.tcf[ie - 1];
	    goto L620;
L615:
	    large_1.cf[ie + if__ * 2000 - 2001] = 0.;
L620:
	    ;
	}
	i__1 = large_1.iplast;
	for (if__ = 2; if__ <= i__1; ++if__) {
	    large_1.cf[ie + if__ * 2000 - 2001] += large_1.cf[ie + (if__ - 1) 
		    * 2000 - 2001];
/* L630: */
	}
	large_1.tcf[ie - 1] *= mix2_1.eroot[ie - 1];
/* L700: */
    }
/* ------------------------------------------------------------------- */
/*   CALCULATE NULL COLLISION FREQUENCY */
/* ------------------------------------------------------------------- */
    bp = setp_1.efield * setp_1.efield * cnsts1_1.const1;
    f2 = setp_1.efield * cnsts1_1.const3;
    elow = setp_1.tmax * (setp_1.tmax * bp - f2 * sqrt(inpt_1.efinal * .5)) / 
	    inpt_1.estep - 1.;
    if(elow>setp_1.small)
      elow=setp_1.small;
    ehi = setp_1.tmax * (setp_1.tmax * bp + f2 * sqrt(inpt_1.efinal * .5)) / 
	    inpt_1.estep + 1.;
    if (ehi > (float)1e4) {
	ehi = (float)1e4;
    }
    for (i__ = 1; i__ <= 10; ++i__) {
	jlow = 2000 - (11 - i__) * 200 + 1 + (int) elow;
	jhi = 2000 - (10 - i__) * 200 + (int) ehi;
	if(jlow<1)
	  j=1;
	if(jhi>2000)
	  jhi=2000;
	i__1 = jhi;
	for (j = jlow; j <= i__1; ++j) {
	    if (large_1.tcf[j - 1] >= setp_1.tcfmax[i__ - 1]) {
		setp_1.tcfmax[i__ - 1] = large_1.tcf[j - 1];
	    }
/* L800: */
	}
/* L810: */
    }
/* ------------------------------------------------------------------- */
/*   CROSS SECTION DATA FOR INTEGRALS IN SUBROUTINE OUTPUT */
/* --------------------------------------------------------------------- 
*/
    i__1 = inpt_1.nstep1 + 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	mix2_1.qtot[i__ - 1] = ratio_1.an1 * q1[i__ * 6 - 6] + ratio_1.an2 * 
		q2[i__ * 6 - 6] + ratio_1.an3 * q3[i__ * 6 - 6] + ratio_1.an4 
		* q4[i__ * 6 - 6];
	mix2_1.qel[i__ - 1] = ratio_1.an1 * q1[i__ * 6 - 5] + ratio_1.an2 * 
		q2[i__ * 6 - 5] + ratio_1.an3 * q3[i__ * 6 - 5] + ratio_1.an4 
		* q4[i__ * 6 - 5];

	mix1_1.qion[(i__ << 2) - 4] = q1[i__ * 6 - 4] * ratio_1.an1;
	mix1_1.qion[(i__ << 2) - 3] = q2[i__ * 6 - 4] * ratio_1.an2;
	mix1_1.qion[(i__ << 2) - 2] = q3[i__ * 6 - 4] * ratio_1.an3;
	mix1_1.qion[(i__ << 2) - 1] = q4[i__ * 6 - 4] * ratio_1.an4;
	qatt[(i__ << 2) - 4] = q1[i__ * 6 - 3] * ratio_1.an1;
	qatt[(i__ << 2) - 3] = q2[i__ * 6 - 3] * ratio_1.an2;
	qatt[(i__ << 2) - 2] = q3[i__ * 6 - 3] * ratio_1.an3;
	qatt[(i__ << 2) - 1] = q4[i__ * 6 - 3] * ratio_1.an4;

	if (mix3_1.nin1 == 0) {
	    goto L820;
	}
	i__2 = mix3_1.nin1;
	for (j = 1; j <= i__2; ++j) {
/* L815: */
	    mix1_1.qin1[j + i__ * 20 - 21] *= ratio_1.an1;
	}
L820:
	if (mix3_1.nin2 == 0) {
	    goto L830;
	}
	i__2 = mix3_1.nin2;
	for (j = 1; j <= i__2; ++j) {
/* L825: */
	    mix1_1.qin2[j + i__ * 20 - 21] *= ratio_1.an2;
	}
L830:
	if (mix3_1.nin3 == 0) {
	    goto L840;
	}
	i__2 = mix3_1.nin3;
	for (j = 1; j <= i__2; ++j) {
/* L835: */
	    mix1_1.qin3[j + i__ * 20 - 21] *= ratio_1.an3;
	}
L840:
	if (mix3_1.nin4 == 0) {
	    goto L850;
	}
	i__2 = mix3_1.nin4;
	for (j = 1; j <= i__2; ++j) {
/* L845: */
	    mix1_1.qin4[j + i__ * 20 - 21] *= ratio_1.an4;
	}

L850:
	mix2_1.qrel[i__ - 1] = 0.;
	mix1_1.qsatt[i__ - 1] = 0.;
	mix1_1.qsum[i__ - 1] = 0.;
	i__2 = inpt_1.ngas;
	for (j = 1; j <= i__2; ++j) {
	    mix1_1.qsum[i__ - 1] = mix1_1.qsum[i__ - 1] + mix1_1.qion[j + (
		    i__ << 2) - 5] + qatt[j + (i__ << 2) - 5];
	    mix1_1.qsatt[i__ - 1] += qatt[j + (i__ << 2) - 5];
/* L855: */
	    mix2_1.qrel[i__ - 1] = mix2_1.qrel[i__ - 1] + mix1_1.qion[j + (
		    i__ << 2) - 5] - qatt[j + (i__ << 2) - 5];
	}

	if (mix3_1.nin1 == 0) {
	    goto L865;
	}
	i__2 = mix3_1.nin1;
	for (j = 1; j <= i__2; ++j) {
/* L860: */
	    mix1_1.qsum[i__ - 1] += mix1_1.qin1[j + i__ * 20 - 21];
	}
L865:
	if (mix3_1.nin2 == 0) {
	    goto L875;
	}
	i__2 = mix3_1.nin2;
	for (j = 1; j <= i__2; ++j) {
/* L870: */
	    mix1_1.qsum[i__ - 1] += mix1_1.qin2[j + i__ * 20 - 21];
	}
L875:
	if (mix3_1.nin3 == 0) {
	    goto L885;
	}
	i__2 = mix3_1.nin3;
	for (j = 1; j <= i__2; ++j) {
/* L880: */
	    mix1_1.qsum[i__ - 1] += mix1_1.qin3[j + i__ * 20 - 21];
	}
L885:
	if (mix3_1.nin4 == 0) {
	    goto L895;
	}
	i__2 = mix3_1.nin4;
	for (j = 1; j <= i__2; ++j) {
/* L890: */
	    mix1_1.qsum[i__ - 1] += mix1_1.qin4[j + i__ * 20 - 21];
	}
L895:

/* L900: */
	;
    }

    return 0;
} /* mixer_ */

int StFtpcMagboltz2::gasmix_(int *ngs, int *niso, double *q, 
			     double *qin, int *nin, double *e, 
			     double *ei, char *name__, double *virial, 
			     double *peqel, double *peqin, int *kel, 
			     int *kin, int name_len)
{
    static int mn;

    /* Parameter adjustments */
    --kin;
    peqin -= 3;
    --peqel;
    --ei;
    --e;
    qin -= 21;
    q -= 7;

    /* Function Body */
    mn = 1;
    switch ((int)*ngs) {
	case 1:  goto L1;
	case 2:  goto L2;
	case 3:  goto L3;
	case 4:  goto L4;
	case 5:  goto L5;
	case 6:  goto L6;
	case 7:  goto L7;
	case 8:  goto L8;
	case 9:  goto L9;
	case 10:  goto L10;
	case 11:  goto L11;
	case 12:  goto L12;
    }
L1:
    return 0;
L2:
    gas2_(&q[7], &qin[21], nin, &e[1], &ei[1], name__, virial, &mn, 15L);
    return 0;
L3:
    gas3_(&q[7], &qin[21], nin, &e[1], &ei[1], name__, virial, &mn, 15L);
    return 0;
L4:
    return 0;
L5:
    gas5_(&q[7], &qin[21], nin, &e[1], &ei[1], name__, virial, &mn, 15L);
    return 0;
L6:
    return 0;
L7:
    return 0;
L8:
    return 0;
L9:
    return 0;
L10:
    return 0;
L11:
    return 0;
L12:
    gas12_(&q[7], &qin[21], nin, &e[1], &ei[1], name__, virial, &mn, 15L);
    return 0;
} /* gasmix_ */

int StFtpcMagboltz2::setup_(float *e_magni__,
			    float *b_magni__, float *b_ang__,
			    float *press, float *p_ar__,
			    float *p_co2__, float *p_ne__,
			    float *p_he__, float *temper)
{
    /* Local variables */
    static double eovm, corr;
    static int j, k;
    static double boltz, atmos;
    static int nscale;
    static double alosch, abzero, boltzj, awb, totfrac;

/* INTEGRALS OF ERROR FUNCTION */
/*     DATA ERFINT/.112462916,.222702589,.328626759,.428392355, */
/*    /.520499878,.603856091,.677801194,.742100965,.796908212,.842700793, 
*/
/*    /.880205070,.910313978,.934007945,.952285120,.966105146,.976348383, 
*/
/*    /.983790459,.989090502,.992790429,.995322265,.997020533,.998137154, 
*/
/*    /.998856823,.999311486,.999593048/ */

/*   NEW UPDATE OF CONSTANTS 1998 */

    for (k = 1; k <= 25; ++k) {
/* L10: */
	thrm_1.erfint[k - 1] = 0.;
    }
    setp_1.api = acos(-1.);
    inpt_1.ary = (float)13.60569172;
    cnsts_1.pir2 = 8.79735534e-17;
    cnsts_1.echarg = 1.602176462e-19;
    cnsts_1.emass = 9.10938188e-31;
    cnsts_1.amu = 1.66053873e-27;
    boltz = 8.617342e-5;
    boltzj = 1.3806503e-23;
    awb = 17588201740.;
    alosch = 2.6867775e19;
    eovm = sqrt(cnsts_1.echarg * 2. / cnsts_1.emass) * 100.;
    abzero = 273.15;
    atmos = 760.;
    cnsts1_1.const1 = awb / 2. * 1e-19;
    cnsts1_1.const2 = cnsts1_1.const1 * .01;
    cnsts1_1.const3 = sqrt(awb * .2) * 1e-9;
    cnsts1_1.const4 = cnsts1_1.const3 * alosch * 1e-15;
    cnsts1_1.const5 = cnsts1_1.const3 / 2.;

/*      READ IN OUTPUT CONTROL AND INTEGRATION DATA */

    inpt_1.ngas = 4;
    setp_1.nmax = 10;
    // always calculate upper limit, *efin is start value for magboltz1
    inpt_1.efinal = 0.0;

/*   GAS IDENTIFIERS */

    gasn_1.ngas1=2; // Argon
    gasn_1.ngas2=12; // CO2
    gasn_1.ngas3=5; // Neon
    gasn_1.ngas4=3; // Helium4

/*      GAS PARAMETERS */

    ratio_1.frac1=*p_ar__;
    ratio_1.frac2=*p_co2__;
    ratio_1.frac3=*p_ne__;
    ratio_1.frac4=*p_he__;

    inpt_1.tempc = *temper;
    inpt_1.torr = *press;

    if(*p_he__==0.0)
      {
	inpt_1.ngas--;
	if(*p_ne__==0.0)
	  inpt_1.ngas--;
      }
/*      FIELD VALUES */

    setp_1.efield=*e_magni__;
    bfld_1.bmag=*b_magni__;
    bfld_1.btheta=*b_ang__;

/*   CHECK INPUT */
    totfrac = 0.;
    if (inpt_1.ngas == 0 || inpt_1.ngas > 4) {
	goto L999;
    }
    if (gasn_1.ngas1 == 0 || ratio_1.frac1 == 0.) {
	goto L999;
    }
    totfrac = ratio_1.frac1;
    if (inpt_1.ngas == 1) {
	goto L1;
    }
    if (gasn_1.ngas2 == 0 || ratio_1.frac2 == 0.) {
	goto L999;
    }
    totfrac += ratio_1.frac2;
    if (inpt_1.ngas == 2) {
	goto L1;
    }
    if (gasn_1.ngas3 == 0 || ratio_1.frac3 == 0.) {
	goto L999;
    }
    totfrac += ratio_1.frac3;
    if (inpt_1.ngas == 3) {
	goto L1;
    }
    if (gasn_1.ngas4 == 0 || ratio_1.frac4 == 0.) {
	goto L999;
    }
    totfrac += ratio_1.frac4;
L1:
    if (totfrac != 100.) {
	goto L999;
    }
    vel_1.wx = 0.;
    vel_1.wy = 0.;
    vel_1.wz = 0.;
    diflab_1.difxx = 0.;
    diflab_1.difyy = 0.;
    diflab_1.difzz = 0.;
    diflab_1.difyz = 0.;
    diflab_1.difxy = 0.;
    diflab_1.difxz = 0.;
    difvel_1.difln = 0.;
    difvel_1.diftr = 0.;
    setp_1.tmax = 100.;
    nscale = 960000;
    inpt_1.nstep = 2000;
    inpt_1.nout = 10;
    setp_1.theta = .785;
    setp_1.phi = .1;
    for (j = 1; j <= 300; ++j) {
/* L65: */
	outpt_1.time[j - 1] = 0.;
    }
    for (k = 1; k <= 20; ++k) {
/* L70: */
	outpt_1.icoll[k - 1] = 0;
    }
    for (k = 1; k <= 2000; ++k) {
/* L100: */
	outpt_1.spec[k - 1] = 0.;
    }
    for (k = 1; k <= 10; ++k) {
/* L101: */
	setp_1.tcfmax[k - 1] = 0.;
    }
    inpt_1.alpha = 0.;
    setp_1.rstart = .666;
    setp_1.estart = inpt_1.efinal / 20.;
    thrm_1.ithrm = 0;
    setp_1.nmax *= nscale;
    inpt_1.nstep1 = inpt_1.nstep + 1;

    corr = abzero * inpt_1.torr / (atmos * (abzero + inpt_1.tempc) * 100.);
    inpt_1.akt = (abzero + inpt_1.tempc) * boltz;
    ratio_1.an1 = ratio_1.frac1 * corr * alosch;
    ratio_1.an2 = ratio_1.frac2 * corr * alosch;
    ratio_1.an3 = ratio_1.frac3 * corr * alosch;
    ratio_1.an4 = ratio_1.frac4 * corr * alosch;
    ratio_1.an = corr * 100. * alosch;
    mratio_1.van1 = ratio_1.frac1 * corr * cnsts1_1.const4;
    mratio_1.van2 = ratio_1.frac2 * corr * cnsts1_1.const4;
    mratio_1.van3 = ratio_1.frac3 * corr * cnsts1_1.const4;
    mratio_1.van4 = ratio_1.frac4 * corr * cnsts1_1.const4;
    mratio_1.van = corr * 100. * cnsts1_1.const4;
/* CALCULATE THERMAL VELOCITY DISTRIBUTION INTEGRALS */
    thrm_1.con = 1e-13 / sqrt(cnsts_1.amu / (boltzj * 2. * (inpt_1.tempc + 
	    abzero)));
/* N.B.  LOADED ERROR FUNCTION INTEGRALS IN DATA ARRAY . */

/*  RADIANS PER PICOSECOND */
    bfld_1.wb = awb * bfld_1.bmag * 1e-12;
/*   METRES PER PICOSECOND */
    if (bfld_1.bmag == 0.) {
	return 0;
    }
    bfld_1.eovb = setp_1.efield * 1e-9 / bfld_1.bmag;
    return 0;
L999:
    printf("ERROR IN GAS INPUT : NGAS=%i NGAS1=%i NGAAS2=%i NGAS3=%i NGAS4=%i\nFRAC1=%8.3f, FRAC2=%8.3f, FRAC3=%8.3f, FRAC4=%8.3f\n", inpt_1.ngas, gasn_1.ngas1, gasn_1.ngas2, gasn_1.ngas3, gasn_1.ngas4, ratio_1.frac1, ratio_1.frac2, ratio_1.frac3, ratio_1.frac4);

    return 0;
} /* setup_ */

int StFtpcMagboltz2::prnter_()
{
    printf("PROGRAM MAGBOLTZ 2\n");
    printf("MONTE CARLO SOLUTION FOR MIXTURE OF %i GASES.\n------------------------------------------------------\n", inpt_1.ngas);
    printf("    GASES  USED  = %s %s %s %s\n", names_1.name1, names_1.name2, names_1.name3, names_1.name4);
    printf(" PERCENTAGE USED = %f %f %f %f\n", ratio_1.frac1, ratio_1.frac2, ratio_1.frac3, ratio_1.frac4);
    printf(" GAS TEMPERATURE = %6.1f DEGREES CENTIGRADE.   GAS PRESSURE = %7.1f TORR.\n",inpt_1.tempc, inpt_1.torr);
    printf(" INTEGRATION FROM 0.0 TO %8.2f EV.  IN %i STEPS. \n", inpt_1.efinal, inpt_1.nstep);
    printf("  ELECTRIC FIELD =%12.4f VOLTS/CM.\n  MAGNETIC FIELD =%11.4f KILOGAUSS.\n  ANGLE BETWEEN ELECTRIC AND MAGNETIC FIELD =%10.3f DEGREES.\n  CYCLOTRON FREQ. =%12.3e RADIANS/PICOSECOND\n", setp_1.efield, bfld_1.bmag, bfld_1.btheta, bfld_1.wb);
    printf(" INITIAL ELECTRON ENERGY =%8.3f EV.   RANDOM NUMBER STARTER =%9.5f\n", setp_1.estart, setp_1.rstart);
    printf("  TOTAL NUMBER OF REAL AND NULL COLLISIONS =%i\n", setp_1.nmax);
    printf("  NULL COLLISION FREQUENCY AT 10 EQUALLY SPACED ENERGY INTERVALS (*10**12/SEC) %10.3e %10.3e %10.3e %10.3e %10.3e %10.3e %10.3e %10.3e %10.3e %10.3e\n", setp_1.tcfmax[0], setp_1.tcfmax[1], setp_1.tcfmax[2], setp_1.tcfmax[3], setp_1.tcfmax[4], setp_1.tcfmax[5], setp_1.tcfmax[6], setp_1.tcfmax[7], setp_1.tcfmax[8], setp_1.tcfmax[9]);
    printf("  REAL COLLISION FREQUENCY AT 10 EQUALLY SPACED ENERGY INTERVALS (*10**12/SEC) %10.3e %10.3e %10.3e %10.3e %10.3e %10.3e %10.3e %10.3e %10.3e %10.3e\n", large_1.tcf[100- 1], large_1.tcf[300- 1], large_1.tcf[500- 1], large_1.tcf[700- 1], large_1.tcf[900- 1], large_1.tcf[1100- 1], large_1.tcf[1300- 1], large_1.tcf[1500- 1], large_1.tcf[1700- 1], large_1.tcf[1900- 1]);
    return 0;
} /* prnter_ */

int StFtpcMagboltz2::monte_()
{
    /* System generated locals */
    int i__1, i__2;
    double d__1;

    /* Local variables */
    static double sdif, arat;
    static int ncol;
    static long rdum;
    static double csqd, tlim, argz, vtot, dftp1, dftp2;
    static double sume2, a, b, d__, e;
    static int i__;
    static double q, t, u;
    static int icans[64];
    static double tdash;
    static int intem, ncolm;
    static double e1, f1, f2, f4;
    static int j1, j2;
    static double r1, t2, r5, r2, s1, extra, r9, s2, r3, f3, r4, f8, f9, 
	    f6, f5, s3, theta0, s4, r6, cx1del, cy1del, cz1del, const9;
    static int id;
    static double ap, bp;
    static int ie;
    static double const6, const7, ei, r31;
    static int ij, it;
    static double deltae;
    static int iprint, j2m;
    static double cx1, cy1, cz1, sumxsq, sumysq, sumzsq, ano;
    static int ipt;
    static double sto[10000], xst[10000], yst[10000], zst[10000], arg1, 
	    dcx1, dcy1, dcz1, dcx2, dcy2, dcz2, phi0, cor1, std1, std2;

/* ------------------------------------------------------------------- */
/*   CALCULATES COLLISION EVENTS AND UPDATES DIFFUSION AND VELOCITY. */
/*   USED WITH MAGNETIC FIELD B =0.0   ELECTRIC FIELD IN Z DIRECTION. */
/* ------------------------------------------------------------------- */
/*  SET ANISOTROPY CONTROL FOR ELASTIC COLLISIONS */
    for (i__ = 1; i__ <= 64; ++i__) {
	icans[i__ - 1] = 0;
	if (large_2.iarry[i__ - 1] == 1 && anis_1.kel[0] == 1) {
	    icans[i__ - 1] = 1;
	}
	if (large_2.iarry[i__ - 1] == 6 && anis_1.kel[1] == 1) {
	    icans[i__ - 1] = 1;
	}
	if (large_2.iarry[i__ - 1] == 11 && anis_1.kel[2] == 1) {
	    icans[i__ - 1] = 1;
	}
/* L1: */
	if (large_2.iarry[i__ - 1] == 16 && anis_1.kel[3] == 1) {
	    icans[i__ - 1] = 1;
	}
    }
    vel_1.wx = 0.;
    vel_1.wy = 0.;
    outpt_1.x = 0.;
    outpt_1.y = 0.;
    outpt_1.z__ = 0.;
    outpt_1.st = 0.;
    std1 = 0.;
    std2 = 0.;
    sume2 = 0.;
    sumzsq = 0.;
    sumxsq = 0.;
    sumysq = 0.;
    setp_1.small = 1e-20;
    outpt_1.tmax1 = 0.;

    rdum = (long) (10000000000.0*setp_1.rstart);
    HepRandom quasiRandom(rdum);

    e1 = setp_1.estart;
    const9 = cnsts1_1.const3 * .01;
    arat = cnsts_1.emass / cnsts_1.amu;
    intem = 10;
    inpt_1.itmax = 60;
    id = 0;
    ncol = 0;
    outpt_1.nnull = 0;
/*  NUMBER OF COLLISIONS FOR DE-CORRELATION */
    ncolm = 9998;
    iprint = 0;
    tdash = 0.;

/*     INITIAL DIRECTION COSINES */

    dcz1 = cos(setp_1.theta);
    dcx1 = sin(setp_1.theta) * cos(setp_1.phi);
    dcy1 = sin(setp_1.theta) * sin(setp_1.phi);

    bp = setp_1.efield * setp_1.efield * cnsts1_1.const1;
    f1 = setp_1.efield * cnsts1_1.const2;
    f2 = setp_1.efield * cnsts1_1.const3;
    f4 = setp_1.api * 2.;
    deltae = inpt_1.efinal / (double) intem;
    j2m = setp_1.nmax / inpt_1.itmax;
/* MAIN LOOP */
    i__1 = inpt_1.itmax;
    for (j1 = 1; j1 <= i__1; ++j1) {
	i__2 = j2m;
	for (j2 = 1; j2 <= i__2; ++j2) {
	    r1 = quasiRandom.flat();
	    i__ = (int) (e1 / deltae) + 1;
	    if(i__>10)
	      i__=10;
	    tlim = setp_1.tcfmax[i__ - 1];
	    t = -log(r1) / tlim + tdash;
	    tdash = t;
	    ap = dcz1 * f2 * sqrt(e1);
	    e = e1 + (ap + bp * t) * t;
	    ie = (int) (e / inpt_1.estep) + 1;
	    ie = min(ie,2000);
	    if (large_2.tcf[ie - 1] <= tlim) {
		goto L122;
	    }
	    tdash += log(r1) / tlim;
	    setp_1.tcfmax[i__ - 1] *= 1.05;
/*     WRITE(6,996) */
/* 996  FORMAT(/,5X,' WARNING NULL COLLISION TIME INCREASED',/) */
	    goto L133;

/*     TEST FOR REAL OR NULL COLLISION */

L122:
	    r5 = quasiRandom.flat();
	    tlim = large_2.tcf[ie - 1] / tlim;
	    if (r5 <= tlim) {
		goto L137;
	    }
	    ++outpt_1.nnull;
	    goto L133;

/*  CALCULATE DIRECTION COSINES AND POSITIONS AT INSTANT BEFORE CO
LLISION */
/*    ALSO UPDATE DIFFUSION  AND ENERGY CALCULATIONS. */
L137:
	    t2 = t * t;
	    if (t >= outpt_1.tmax1) {
		outpt_1.tmax1 = t;
	    }
	    tdash = 0.;
	    const6 = sqrt(e1 / e);
	    dcx2 = dcx1 * const6;
	    dcy2 = dcy1 * const6;
	    dcz2 = dcz1 * const6 + setp_1.efield * t * cnsts1_1.const5 / sqrt(
		    e);
	    a = ap * t;
	    b = bp * t2;
	    sume2 += t * (e1 + a / 2. + b / 3.);
	    const7 = const9 * sqrt(e1);
	    a = t * const7;
	    ++ncol;
	    cx1 = dcx1 * const7;
	    cy1 = dcy1 * const7;
	    cz1 = dcz1 * const7;
	    outpt_1.x += dcx1 * a;
	    outpt_1.y += dcy1 * a;
	    outpt_1.z__ = outpt_1.z__ + dcz1 * a + t2 * f1;
	    outpt_1.st += t;
	    it = (int) (t + (float)1.);
	    it = min(it,300);
	    outpt_1.time[it - 1] += 1.;
	    outpt_1.spec[ie - 1] += 1.;
	    vel_1.wz = outpt_1.z__ / outpt_1.st;
	    if (id == 0) {
		goto L121;
	    }
	    std2 += t;
	    sdif = outpt_1.st - sto[ncol - 1];
/* Computing 2nd power */
	    d__1 = outpt_1.x - xst[ncol - 1];
	    sumxsq += d__1 * d__1 * t / sdif;
/* Computing 2nd power */
	    d__1 = outpt_1.y - yst[ncol - 1];
	    sumysq += d__1 * d__1 * t / sdif;
	    if (j1 < 6) {
		goto L121;
	    }
	    std1 += t;
/* Computing 2nd power */
	    d__1 = outpt_1.z__ - zst[ncol - 1] - vel_1.wz * sdif;
	    sumzsq += d__1 * d__1 * t / sdif;
L121:
	    xst[ncol - 1] = outpt_1.x;
	    yst[ncol - 1] = outpt_1.y;
	    zst[ncol - 1] = outpt_1.z__;
	    sto[ncol - 1] = outpt_1.st;
	    if (ncol < ncolm) {
		goto L135;
	    }
	    ++id;
	    outpt_1.xid = (double) id;
	    ncol = 0;
/* --------------------------------------------------------------
------- */
/*     DETERMINATION OF REAL COLLISION TYPE */
/* --------------------------------------------------------------
------- */
L135:
	    r2 = quasiRandom.flat();
	    i__ = 0;
L140:
	    ++i__;
	    if (large_2.cf[ie + i__ * 2000 - 2001] < r2) {
		goto L140;
	    }
	    s1 = large_2.rgas[i__ - 1];
	    ei = large_2.ein[i__ - 1];
	    if (large_2.ipn[i__ - 1] == 0) {
		goto L666;
	    }
/*  USE FLAT DISTRIBUTION OF  ELECTRON ENERGY BETWEEN E-EION AND 0
.0 EV */
/*  SAME AS IN BOLTZMANN */
	    r9 = quasiRandom.flat();
	    extra = r9 * (e - ei);
	    ei = extra + ei;

/*  GENERATE SCATTERING ANGLES AND UPDATE  LABORATORY COSINES AFTE
R */
/*   COLLISION ALSO UPDATE ENERGY OF ELECTRON. */

L666:
	    ipt = large_2.iarry[i__ - 1];
	    ++outpt_1.icoll[ipt - 1];
	    if (e < ei) {
/*      WRITE(6,994) E,EI,J2 */
/* 994  FORMAT(2X,' WARNING ENERGY =',F8.3,' LESS THAN ENERGY 
LOSS EI=',F8 */
/*    /.3,' AT ITER=',I12,' DUE TO BINNING ERROR') */
/*  FIX ENERGY LOSS SMALLER THAN INCIDENT ENERGY IF ERROR OCCU
RS */
		ei = e - 1e-4;
	    }
	    s2 = s1 * s1 / (s1 - 1.);
/* ANISOTROPY ANGLE CONTROL */
	    if (anis_1.niso == 0) {
		goto L55;
	    }
/* ELASTIC ANISOTROPY */
	    if (icans[i__ - 1] == 1) {
		r31 = quasiRandom.flat();
		r3 = quasiRandom.flat();
		f3 = r3;
		if (ipt == 1 && r31 > anis_1.pel[(ie << 2) - 4]) {
		    f3 = -f3;
		}
		if (ipt == 6 && r31 > anis_1.pel[(ie << 2) - 3]) {
		    f3 = -f3;
		}
		if (ipt == 11 && r31 > anis_1.pel[(ie << 2) - 2]) {
		    f3 = -f3;
		}
		if (ipt == 16 && r31 > anis_1.pel[(ie << 2) - 1]) {
		    f3 = -f3;
		}
/* INELASTIC ANISOTROPY */
	    } else if (anis_1.index[i__ - 1] != 0) {
		r31 = quasiRandom.flat();
		r3 = quasiRandom.flat();
		f3 = r3;
		if (r31 > anis_1.pin[anis_1.index[i__ - 1] + (ie << 3) - 9]) {
		    f3 = -f3;
		}
	    } else {
/* ISOTROPIC */
		r3 = quasiRandom.flat();
		f3 = 1. - r3 * 2.;
	    }
	    goto L56;
L55:
	    r3 = quasiRandom.flat();
	    f3 = 1. - r3 * 2.;
L56:
	    theta0 = acos(f3);
	    r4 = quasiRandom.flat();
	    phi0 = f4 * r4;
	    f8 = sin(phi0);
	    f9 = cos(phi0);
	    arg1 = 1. - s1 * ei / e;
	    if(arg1<setp_1.small)
	      arg1=setp_1.small;
	    d__ = 1. - f3 * sqrt(arg1);
	    e1 = e * (1. - ei / (s1 * e) - d__ * 2. / s2);
	    if(e1<setp_1.small)
	      e1=setp_1.small;
	    q = sqrt(e / e1 * arg1) / s1;
	    if(q>1.0)
	      q=1.0;
	    setp_1.theta = asin(q * sin(theta0));
	    f6 = cos(setp_1.theta);
	    u = (s1 - 1.) * (s1 - 1.) / arg1;
	    csqd = f3 * f3;
	    if (f3 < 0. && csqd > u) {
		f6 *= -1.;
	    }
	    f5 = sin(setp_1.theta);
	    if(dcz2>1.0)
	      dcz2=1.0;	    
	    argz = sqrt(dcx2 * dcx2 + dcy2 * dcy2);
	    if (argz == 0.) {
/*      WRITE(6,9232) ITER,ID,E1 */
/* 9232  FORMAT(3X,'WARNING ARGZ= 0.0 AT ITER =',I10,' ID =',I
10,' E1=',E1 */
/*    /2.3) */
		dcz1 = f6;
		dcx1 = f9 * f5;
		dcy1 = f8 * f5;
		goto L130;
	    }
	    dcz1 = dcz2 * f6 + argz * f5 * f8;
	    dcy1 = dcy2 * f6 + f5 / argz * (dcx2 * f9 - dcy2 * dcz2 * f8);
	    dcx1 = dcx2 * f6 - f5 / argz * (dcy2 * f9 + dcx2 * dcz2 * f8);
L130:
	    if (thrm_1.ithrm != 0) {
/* CALCULATE VELOCITY CHANGE DUE TO MOLECULAR MOTION IN GAS. 
*/
		s3 = s1 - 1.;
		s4 = 1. / s3;
		ano = s4 * arat;
		cor1 = 1. / sqrt(ano);
		r5 = quasiRandom.flat();
		r6 = r5 * 2. - 1.;
		r5 = fabs(r6);
		for (ij = 1; ij <= 25; ++ij) {
/* L260: */
		    if (r5 < thrm_1.erfint[ij - 1]) {
			goto L261;
		    }
		}
		ij = 26;
L261:
		cx1del = thrm_1.con * ((double) ij - .5) * cor1;
		if (r6 < 0.) {
		    cx1del = -cx1del;
		}
		r5 = quasiRandom.flat();
		r6 = r5 * 2. - 1.;
		r5 = fabs(r6);
		for (ij = 1; ij <= 25; ++ij) {
/* L262: */
		    if (r5 < thrm_1.erfint[ij - 1]) {
			goto L263;
		    }
		}
		ij = 26;
L263:
		cy1del = thrm_1.con * ((double) ij - .5) * cor1;
		if (r6 < 0.) {
		    cy1del = -cy1del;
		}
		r5 = quasiRandom.flat();
		r6 = r5 * 2. - 1.;
		r5 = fabs(r6);
		for (ij = 1; ij <= 25; ++ij) {
/* L264: */
		    if (r5 < thrm_1.erfint[ij - 1]) {
			goto L265;
		    }
		}
		ij = 26;
L265:
		cz1del = thrm_1.con * ((double) ij - .5) * cor1;
		if (r6 < 0.) {
		    cz1del = -cz1del;
		}
		vtot = const9 * sqrt(e);
		cx1del = vtot * dcx2 * s3 + cx1del;
		cy1del = vtot * dcy2 * s3 + cy1del;
		cz1del = vtot * dcz2 * s3 + cz1del;
		vtot = const9 * sqrt(e1);
		cx1 = vtot * dcx1 + cx1del;
		cy1 = vtot * dcy1 + cy1del;
		cz1 = vtot * dcz1 + cz1del;
		vtot = sqrt(cx1 * cx1 + cy1 * cy1 + cz1 * cz1);
		dcx1 = cx1 / vtot;
		dcy1 = cy1 / vtot;
		dcz1 = cz1 / vtot;
		e1 = vtot * vtot / (const9 * const9);
	    }
L133:
	    ;
	}
/*   ------------------------------------------ */
	++iprint;
	if (j1 == 10) {
	  printf("    VEL      POS        TIME       ENERGY     COUNT        DIFTR(X)  DIFTR(Y)  DIFLN\n");
	}
	if (iprint == 10) {
	    vel_1.wz *= 1e8;
	    outpt_1.ave = sume2 / outpt_1.st;
	    if (std1 != 0.) {
		difvel_1.difln = sumzsq * 5e15 / std1;
	    }
	    if (std2 != 0.) {
		dftp1 = sumxsq * 5e15 / std2;
	    }
	    if (std2 != 0.) {
		dftp2 = sumysq * 5e15 / std2;
	    }
	    difvel_1.diftr = (dftp1 + dftp2) / 2.;
	    diflab_1.difxx = dftp1;
	    diflab_1.difyy = dftp2;
	    diflab_1.difzz = difvel_1.difln;
	    printf("%8.3f %10.3e %10.3e %9.4f  %i   %8.1f   %8.1f   %8.1f\n", vel_1.wz, outpt_1.z__, outpt_1.st, outpt_1.ave, id, dftp1, dftp2, difvel_1.difln);
	    iprint = 0;
	}
/* LOOP */
/* L210: */
    }
/*  CONVERT CM/SEC */
    vel_1.wz *= 1e6;
    diflab_1.difyz = 0.;
    diflab_1.difxy = 0.;
    diflab_1.difxz = 0.;
    return 0;
} /* monte_ */

int StFtpcMagboltz2::output_()
{
    /* System generated locals */
    int i__1;

    /* Local variables */
    static int nela;
    static double ener, dlmn, freq, dtmn, eplt, wmnx, wmny, wmnz;
    static int i__, k, nreal;
    static double attch;
    static int ninel, ilast;
    static double specs[20], specn;
    static int j1, j2;
    static double delion, freqel, dtovmb, dlovmb, freqin, delatt, freatt, 
	    erratt, freion, errion, smspec, freine1, freine2, freine3, 
	    freine4, freqel1, freqel2, freqel3, freqel4, freion1, freion2, 
	    freatt1, freatt2, freatt3, freatt4, freion3, freion4, freqsp1, 
	    freqsp2, freqsp3, freqsp4;

    printf("------------------------------------------------------------------------------------------------------------------\n");
    printf("------------------------------------------------------------------------------------------------------------------\n");
    nreal = setp_1.nmax - outpt_1.nnull;
    printf("CALCULATED MAX. COLLISION TIME =%7.2f PICOSECONDS.\nNUMBER OF NULL COLLISIONS =%i9,\n NUMBER OF REAL COLLISIONS =%i\n", outpt_1.tmax1, outpt_1.nnull, nreal);
    printf("NUMBER OF COLLISIONS IN LAST ENERGY BIN =%6.1f\n", outpt_1.spec[1999]);
    wmnz = vel_1.wz * 1e-5;
    wmny = vel_1.wy * 1e-5;
    wmnx = vel_1.wx * 1e-5;
    printf("Z DRIFT VELOCITY =%11.4e  CM./SEC.    =%11.4e MICRONS/NANOSECOND\nY DRIFT VELOCITY =%11.4e  CM./SEC.    =%11.4e MICRONS/NANOSECOND\nX DRIFT VELOCITY =%11.4e  CM./SEC.    =%11.4e MICRONS/NANOSECOND\n", vel_1.wz, wmnz, vel_1.wy, wmny, vel_1.wx, wmnx);
    if (bfld_1.bmag > 0. && (bfld_1.btheta > 0. && bfld_1.btheta < 180.)) {
	goto L800;
    }
    dtovmb = difvel_1.diftr * setp_1.efield / vel_1.wz;
    dtmn = sqrt(difvel_1.diftr * 2. / vel_1.wz) * 1e4;
    printf("TRANSVERSE DIFFUSION   =%10.3e CM.**2/SEC.\n     = %9.3f EV.      =%6.1f MICRONS/CENTIMETER**0.5\n", difvel_1.diftr, dtovmb, dtmn);
    dlovmb = difvel_1.difln * setp_1.efield / vel_1.wz;
    dlmn = sqrt(difvel_1.difln * 2. / vel_1.wz) * 1e4;
    printf("LONGITUDINAL DIFFUSION =%10.3e CM.**2/SEC.\n     = %9.3f EV.      =%6.1f MICRONS/CENTIMETER**0.5\n", difvel_1.difln, dlovmb, dlmn);
    goto L900;
L800:
    printf(" DIFFUSION IN CM**2/SEC.");
    printf(" DIFFUSION TENSOR DIFXX =%10.3f DIFYY =%10.3f  DIFZZ =%10.3f\n    DIFYZ =%10.3f DIFXY =%10.3f  DIFXZ =%10.3f\n", diflab_1.difxx, diflab_1.difyy, diflab_1.difzz, diflab_1.difyz, diflab_1.difxy, diflab_1.difxz);
    if (bfld_1.btheta == (float)90.) {
    printf(" DIFFUSION LONGITUDINAL =%10.3f DIFFUSION TRANSVERSE =%10.3f\n DIFFUSION TRANSVERSE (OUT OF EB PLANE) =%10.3f\n", difvel_1.difln, difvel_1.diftr, diflab_1.difxx);
    }
L900:
    printf("MEAN ELECTRON ENERGY =%9.4f EV.\n", outpt_1.ave);
    freq = nreal / outpt_1.st;
    ninel = outpt_1.icoll[1] + outpt_1.icoll[2] + outpt_1.icoll[3] + 
	    outpt_1.icoll[4] + outpt_1.icoll[6] + outpt_1.icoll[7] + 
	    outpt_1.icoll[8] + outpt_1.icoll[9] + outpt_1.icoll[11] + 
	    outpt_1.icoll[12] + outpt_1.icoll[13] + outpt_1.icoll[14] + 
	    outpt_1.icoll[16] + outpt_1.icoll[17] + outpt_1.icoll[18] + 
	    outpt_1.icoll[19];
    freqin = ninel / outpt_1.st;
    nela = outpt_1.icoll[0] + outpt_1.icoll[5] + outpt_1.icoll[10] + 
	    outpt_1.icoll[15];
    freqel = nela / outpt_1.st;
    printf("TOTAL COLL. FREQ. =%9.3e   INELASTIC COLL. FREQ. =%9.3e\n   ELASTIC COLL. FREQ.=%9.3e (*10**12)/SEC.\n", freq, freqin, freqel);
    printf("------------------------------------------------------------------------------------------------------------------\n");
    ilast = (int) outpt_1.tmax1 + 1;
    if (ilast > 300) {
	ilast = 300;
    }
    printf("DISTRIBUTION OF COLLISION TIMES IN 1 PICOSECOND BINS\n");
    i__1 = ilast;
    for (i__ = 1; i__ <= i__1; ++i__) {
        printf("                                                  %10.1f\n", outpt_1.time[i__ - 1]);
    }
    printf("------------------------------------------------------------------------------------------------------------------\n");
    printf("COLLISION FREQUENCIES SORTED ACCORDING TO GAS AND TYPE OF COLLISION IN UNITS OF 10**12/SEC.\n%s   %s   %s   %s\n           ", names_1.name1, names_1.name2, names_1.name3, names_1.name4);
    freqel1 = outpt_1.icoll[0] / outpt_1.st;
    freqel2 = outpt_1.icoll[5] / outpt_1.st;
    freqel3 = outpt_1.icoll[10] / outpt_1.st;
    freqel4 = outpt_1.icoll[15] / outpt_1.st;
    printf("ELASTIC     =%10.3e %10.3e %10.3e %10.3e\n", freqel1, freqel2, freqel3, freqel4);
    freqsp1 = outpt_1.icoll[4] / outpt_1.st;
    freqsp2 = outpt_1.icoll[9] / outpt_1.st;
    freqsp3 = outpt_1.icoll[14] / outpt_1.st;
    freqsp4 = outpt_1.icoll[19] / outpt_1.st;
    printf("SUPERELASTIC=%10.3e %10.3e %10.3e %10.3e\n", freqsp1, freqsp2, freqsp3, freqsp4);
    freine1 = outpt_1.icoll[3] / outpt_1.st;
    freine2 = outpt_1.icoll[8] / outpt_1.st;
    freine3 = outpt_1.icoll[13] / outpt_1.st;
    freine4 = outpt_1.icoll[18] / outpt_1.st;
    printf(" INELASTIC  =%10.3e %10.3e %10.3e %10.3e\n", freine1, freine2, freine3, freine4);
    freatt1 = outpt_1.icoll[2] / outpt_1.st;
    freatt2 = outpt_1.icoll[7] / outpt_1.st;
    freatt3 = outpt_1.icoll[12] / outpt_1.st;
    freatt4 = outpt_1.icoll[17] / outpt_1.st;
    printf(" ATTACHMENT =%10.3e %10.3e %10.3e %10.3e\n", freatt1, freatt2, freatt3, freatt4);
    freion1 = outpt_1.icoll[1] / outpt_1.st;
    freion2 = outpt_1.icoll[6] / outpt_1.st;
    freion3 = outpt_1.icoll[11] / outpt_1.st;
    freion4 = outpt_1.icoll[16] / outpt_1.st;
    printf(" IONISATION =%10.3e %10.3e %10.3e %10.3e\n", freion1, freion2, freion3, freion4);
    printf("------------------------------------------------------------------------------------------------------------------\n");
    delatt = 0.;
    freatt = freatt1 + freatt2 + freatt3 + freatt4;
    if (freatt == 0.) {
	goto L222;
    }
    delatt = sqrt(freatt * outpt_1.st) / outpt_1.st;
    erratt = delatt * 100. / freatt;
L222:
    attch = freatt / vel_1.wz * 1e12;
    delion = 0.;
    freion = freion1 + freion2 + freion3 + freion4;
    if (freion == 0.) {
	goto L224;
    }
    delion = sqrt(freion * outpt_1.st) / outpt_1.st;
    errion = delion * 100. / freion;
L224:
    inpt_1.alpha = freion / vel_1.wz * 1e12;
    printf(" IONISATION RATE /CM.=%10.3e +/-%6.1f PERCENT.    ATTACHMENT RATE /CM.=%10.3e +/-%6.1f PERCENT.\n      NOTE THAT GENERATED ELECTRONS NOT YET INCLUDED\n", inpt_1.alpha, errion, attch, erratt);
    printf("      NORMALISED ENERGY DISTRIBUTION\n");
    j1 = 0;
    j2 = 0;
    smspec = 0.;
    specn = (double) nreal;
    for (k = 1; k <= 2000; ++k) {
	outpt_1.spec[k - 1] /= specn;
	++j1;
	smspec += outpt_1.spec[k - 1];
	if (j1 < 100) {
	    goto L350;
	}
	++j2;
	specs[j2 - 1] = smspec;
	smspec = 0.;
	j1 = 0;
L350:
	;
    }
    eplt = inpt_1.efinal / 20.;
    for (i__ = 1; i__ <= 20; ++i__) {
	ener = eplt * ((double) i__ - .5);
        printf("       E=%7.3f      SPEC=%10.3e\n", ener, specs[i__ - 1]);
/* L420: */
    }
/*     ESTP10=ESTEP*20.0D0 */
/*     WRITE(6,997) ESTP10 */
/* 997 FORMAT(5(/),20X,'PROBABILITY DISTRIBUTION SPECTRUM IN STEPS OF',F7 
*/
/*    /.4,' ELECTRON VOLTS',/,20X,'-------------------------------------- 
*/
/*    /-------------------------------') */
/*     K=-199 */
/*     DO 300 J=1,10 */
/*     K=K+200 */
/*     K1=K+199 */
/*     IF(K1.GT.NSTEP1+200) GO TO 300 */
/*     IF(K1.GT.2000) K1=2000 */
/*     WRITE(6,999) (SPEC(L),L=K,K1,20) */
/* 300 CONTINUE */
    return 0;
} /* output_ */

int StFtpcMagboltz2::montea_()
{
    /* System generated locals */
    int i__1, i__2;
    double d__1;

    /* Local variables */
    static double sdif;
    static int ncol;
    static double csqd, tlim, argz, vtot, dftp1, dftp2; static long rdum;
    static double sume2, a, b, d__, e;
    static int i__;
    static double q, t, u;
    static int icans[64];
    static double tdash;
    static int ncolm, intem;
    static double e1, f1, f2, f4;
    static int j1, j2;
    static double r1, coswt, sinwt, t2, r5, r2, s1, extra, r9, s2, r3, f3,
	     r4, f8, f9, f6, f5, theta0, const9;
    static int id;
    static double ap, bp;
    static int ie;
    static double const6, const7, ei, r31, dx, dy, deltae;
    static int it, iprint, j2m;
    static double cx1, cy1, cz1, cx2, cy2, sumxsq, sumysq, sumzsq, wbt;
    static int ipt;
    static double sto[10000], xst[10000], yst[10000], zst[10000], arg1, 
	    dcx1, dcy1, dcz1, dcx2, dcy2, dcz2, phi0, std1, std2;

/* ------------------------------------------------------------------- */
/*   CALCULATES COLLISION EVENTS AND UPDATES DIFFUSION AND VELOCITY. */
/*   USED WITH MAGNETIC FIELD , B , PARALLEL TO ELECTRIC FIELD IN THE */
/*   Z DIRECTION. */
/* ------------------------------------------------------------------- */
/*  SET ELASTIC ANISOTROPY CONTROL */
    for (i__ = 1; i__ <= 64; ++i__) {
	icans[i__ - 1] = 0;
	if (large_2.iarry[i__ - 1] == 1 && anis_1.kel[0] == 1) {
	    icans[i__ - 1] = 1;
	}
	if (large_2.iarry[i__ - 1] == 6 && anis_1.kel[1] == 1) {
	    icans[i__ - 1] = 1;
	}
	if (large_2.iarry[i__ - 1] == 11 && anis_1.kel[2] == 1) {
	    icans[i__ - 1] = 1;
	}
/* L1: */
	if (large_2.iarry[i__ - 1] == 16 && anis_1.kel[3] == 1) {
	    icans[i__ - 1] = 1;
	}
    }
    vel_1.wx = 0.;
    vel_1.wy = 0.;
    outpt_1.x = 0.;
    outpt_1.y = 0.;
    outpt_1.z__ = 0.;
    outpt_1.st = 0.;
    std1 = 0.;
    std2 = 0.;
    sume2 = 0.;
    sumxsq = 0.;
    sumysq = 0.;
    sumzsq = 0.;
    setp_1.small = 1e-20;
    outpt_1.tmax1 = 0.;
    rdum = (long)setp_1.rstart;
    HepRandom quasiRandom(rdum);

    e1 = setp_1.estart;
    const9 = cnsts1_1.const3 * .01;
    intem = 10;
    inpt_1.itmax = 60;
    id = 0;
    ncol = 0;
    outpt_1.nnull = 0;
/*  NUMBER OD COLLISIONS FOR DE-CORRELATION */
    ncolm = 9998;
    iprint = 0;
    tdash = 0.;

/*     INITIAL DIRECTION COSINES */

    dcz1 = cos(setp_1.theta);
    dcx1 = sin(setp_1.theta) * cos(setp_1.phi);
    dcy1 = sin(setp_1.theta) * sin(setp_1.phi);
/* INITIAL VELOCITY */
    vtot = const9 * sqrt(e1);
    cx1 = dcx1 * vtot;
    cy1 = dcy1 * vtot;
    cz1 = dcz1 * vtot;
    bp = setp_1.efield * setp_1.efield * cnsts1_1.const1;
    f1 = setp_1.efield * cnsts1_1.const2;
    f2 = setp_1.efield * cnsts1_1.const3;
    f4 = setp_1.api * 2.;
    deltae = inpt_1.efinal / (double) intem;
    j2m = setp_1.nmax / inpt_1.itmax;
/* MAIN LOOP */
    i__1 = inpt_1.itmax;
    for (j1 = 1; j1 <= i__1; ++j1) {
	i__2 = j2m;
	for (j2 = 1; j2 <= i__2; ++j2) {
	    r1 = quasiRandom.flat();
	    i__ = (int) (e1 / deltae) + 1;
	    i__ = min(i__,10);
	    tlim = setp_1.tcfmax[i__ - 1];
	    t = -log(r1) / tlim + tdash;
	    tdash = t;
	    ap = dcz1 * f2 * sqrt(e1);
	    e = e1 + (ap + bp * t) * t;
	    ie = (int) (e / inpt_1.estep) + 1;
	    ie = min(ie,2000);
	    if (large_2.tcf[ie - 1] <= tlim) {
		goto L122;
	    }
	    tdash += log(r1) / tlim;
	    setp_1.tcfmax[i__ - 1] *= 1.05;
/*     WRITE(6,996) */
/* 996  FORMAT(/,5X,' WARNING NULL COLLISION TIME INCREASED',/) */
	    goto L133;

/*     TEST FOR REAL OR NULL COLLISION */

L122:
	    r5 = quasiRandom.flat();
	    tlim = large_2.tcf[ie - 1] / tlim;
	    if (r5 <= tlim) {
		goto L137;
	    }
	    ++outpt_1.nnull;
	    goto L133;

/*  CALCULATE DIRECTION COSINES AND POSITIONS AT INSTANT BEFORE CO
LLISION */
/*    ALSO UPDATE DIFFUSION  AND ENERGY CALCULATIONS. */
L137:
	    t2 = t * t;
	    if (t >= outpt_1.tmax1) {
		outpt_1.tmax1 = t;
	    }
	    tdash = 0.;
	    wbt = bfld_1.wb * t;
	    coswt = cos(wbt);
	    sinwt = sin(wbt);
	    const6 = sqrt(e1 / e);
	    cx2 = cx1 * coswt - cy1 * sinwt;
	    cy2 = cy1 * coswt + cx1 * sinwt;
	    vtot = const9 * sqrt(e);
	    dcx2 = cx2 / vtot;
	    dcy2 = cy2 / vtot;
	    dcz2 = dcz1 * const6 + setp_1.efield * t * cnsts1_1.const5 / sqrt(
		    e);
	    a = ap * t;
	    b = bp * t2;
	    sume2 += t * (e1 + a / 2. + b / 3.);
	    const7 = const9 * sqrt(e1);
	    a = t * const7;
	    ++ncol;
	    dx = (cx1 * sinwt - cy1 * (1. - coswt)) / bfld_1.wb;
	    outpt_1.x += dx;
	    dy = (cy1 * sinwt + cx1 * (1. - coswt)) / bfld_1.wb;
	    outpt_1.y += dy;
	    outpt_1.z__ = outpt_1.z__ + dcz1 * a + t2 * f1;
	    outpt_1.st += t;
	    it = (int) (t + 1.);
	    it = min(it,300);
	    outpt_1.time[it - 1] += 1.;
	    outpt_1.spec[ie - 1] += 1.;
	    vel_1.wz = outpt_1.z__ / outpt_1.st;
	    if (id == 0) {
		goto L121;
	    }
	    std2 += t;
	    sdif = outpt_1.st - sto[ncol - 1];
/* Computing 2nd power */
	    d__1 = outpt_1.x - xst[ncol - 1];
	    sumxsq += d__1 * d__1 * t / sdif;
/* Computing 2nd power */
	    d__1 = outpt_1.y - yst[ncol - 1];
	    sumysq += d__1 * d__1 * t / sdif;
	    if (j1 < 6) {
		goto L121;
	    }
	    std1 += t;
/* Computing 2nd power */
	    d__1 = outpt_1.z__ - zst[ncol - 1] - vel_1.wz * sdif;
	    sumzsq += d__1 * d__1 * t / sdif;
L121:
	    xst[ncol - 1] = outpt_1.x;
	    yst[ncol - 1] = outpt_1.y;
	    zst[ncol - 1] = outpt_1.z__;
	    sto[ncol - 1] = outpt_1.st;
	    if (ncol < ncolm) {
		goto L135;
	    }
	    ++id;
	    outpt_1.xid = (double) id;
	    ncol = 0;
/* --------------------------------------------------------------
------- */
/*     DETERMINATION OF REAL COLLISION TYPE */
/* --------------------------------------------------------------
------- */
L135:
	    r2 = quasiRandom.flat();
	    i__ = 0;
L140:
	    ++i__;
	    if (large_2.cf[ie + i__ * 2000 - 2001] < r2) {
		goto L140;
	    }
	    s1 = large_2.rgas[i__ - 1];
	    ei = large_2.ein[i__ - 1];
	    if (large_2.ipn[i__ - 1] == 0) {
		goto L666;
	    }
/*  USE FLAT DISTRIBUTION OF  ELECTRON ENERGY BETWEEN E-EION AND 0
.0 EV */
/*  SAME AS IN BOLTZMANN */
	    r9 = quasiRandom.flat();
	    extra = r9 * (e - ei);
	    ei = extra + ei;

/*  GENERATE SCATTERING ANGLES AND UPDATE  LABORATORY COSINES AFTE
R */
/*   COLLISION ALSO UPDATE ENERGY OF ELECTRON. */

L666:
	    ipt = large_2.iarry[i__ - 1];
	    ++outpt_1.icoll[ipt - 1];
	    if (e < ei) {
/*      WRITE(6,994) E,EI,J2 */
/* 994  FORMAT(2X,' WARNING ENERGY =',F8.3,' LESS THAN ENERGY 
LOSS EI=',F8 */
/*    /.3,' AT ITER=',I12,' DUE TO BINNING ERROR') */
/*  FIX ENERGY LOSS SMALLER THAN INCIDENT ENERGY IF ERROR OCCU
RS */
		ei = e - 1e-4;
	    }
	    s2 = s1 * s1 / (s1 - 1.);
/* ANISOTROPY ANGLE CONTROL */
	    if (anis_1.niso == 0) {
		goto L55;
	    }
/* ELASTIC ANISOTROPY */
	    if (icans[i__ - 1] == 1) {
		r31 = quasiRandom.flat();
		r3 = quasiRandom.flat();
		f3 = r3;
		if (ipt == 1 && r31 > anis_1.pel[(ie << 2) - 4]) {
		    f3 = -f3;
		}
		if (ipt == 6 && r31 > anis_1.pel[(ie << 2) - 3]) {
		    f3 = -f3;
		}
		if (ipt == 11 && r31 > anis_1.pel[(ie << 2) - 2]) {
		    f3 = -f3;
		}
		if (ipt == 16 && r31 > anis_1.pel[(ie << 2) - 1]) {
		    f3 = -f3;
		}
/* INELASTIC ANISOTROPY */
	    } else if (anis_1.index[i__ - 1] != 0) {
		r31 = quasiRandom.flat();
		r3 = quasiRandom.flat();
		f3 = r3;
		if (r31 > anis_1.pin[anis_1.index[i__ - 1] + (ie << 3) - 9]) {
		    f3 = -f3;
		}
	    } else {
/* ISOTROPIC */
		r3 = quasiRandom.flat();
		f3 = 1. - r3 * 2.;
	    }
	    goto L56;
L55:
	    r3 = quasiRandom.flat();
	    f3 = 1. - r3 * 2.;
L56:
	    theta0 = acos(f3);
	    r4 = quasiRandom.flat();
	    phi0 = f4 * r4;
	    f8 = sin(phi0);
	    f9 = cos(phi0);
	    arg1 = 1. - s1 * ei / e;
	    if(arg1<setp_1.small)
	      arg1=setp_1.small;
	    d__ = 1. - f3 * sqrt(arg1);
	    e1 = e * (1. - ei / (s1 * e) - d__ * 2. / s2);
	    if(e1<setp_1.small)
	      e1=setp_1.small;
	    q = sqrt(e / e1 * arg1) / s1;
	    if(q>1.)
	      q=1.;
	    setp_1.theta = asin(q * sin(theta0));
	    f6 = cos(setp_1.theta);
	    u = (s1 - 1.) * (s1 - 1.) / arg1;
	    csqd = f3 * f3;
	    if (f3 < 0. && csqd > u) {
		f6 *= -1.;
	    }
	    f5 = sin(setp_1.theta);
	    if(dcz2<1.)
	      dcz2=1.;
	    vtot = const9 * sqrt(e1);
	    argz = sqrt(dcx2 * dcx2 + dcy2 * dcy2);
	    if (argz == 0.) {
/*      WRITE(6,9232) ITER,ID,E1 */
/* 9232  FORMAT(3X,'WARNING ARGZ= 0.0 AT ITER =',I10,' ID =',I
10,' E1=',E1 */
/*    /2.3) */
		dcz1 = f6;
		dcx1 = f9 * f5;
		dcy1 = f8 * f5;
		goto L130;
	    }
	    dcz1 = dcz2 * f6 + argz * f5 * f8;
	    dcy1 = dcy2 * f6 + f5 / argz * (dcx2 * f9 - dcy2 * dcz2 * f8);
	    dcx1 = dcx2 * f6 - f5 / argz * (dcy2 * f9 + dcx2 * dcz2 * f8);
/* CALCULATE VELOCITY VECTORS AFTER COLLISION */
L130:
	    cx1 = dcx1 * vtot;
	    cy1 = dcy1 * vtot;
	    cz1 = dcz1 * vtot;
L133:
	    ;
	}
/*   ------------------------------------------ */
	++iprint;
	if (j1 == 10) {
	  printf("    VEL      POS        TIME       ENERGY     COUNT        DIFTR(X)  DIFTR(Y)  DIFLN\n");
	}
	if (iprint == 10) {
	    vel_1.wz *= 1e8;
	    outpt_1.ave = sume2 / outpt_1.st;
	    if (std1 != 0.) {
		difvel_1.difln = sumzsq * 5e15 / std1;
	    }
	    if (std2 != 0.) {
		dftp1 = sumxsq * 5e15 / std2;
	    }
	    if (std2 != 0.) {
		dftp2 = sumysq * 5e15 / std2;
	    }
	    difvel_1.diftr = (dftp1 + dftp2) / 2.;
	    diflab_1.difxx = dftp1;
	    diflab_1.difyy = dftp2;
	    diflab_1.difzz = difvel_1.difln;
	    printf(" %8.3f %10.3e %10.3e %9.4f  %8i   %8.1f %8.1f %8.1f\n", vel_1.wz, outpt_1.z__, outpt_1.st, outpt_1.ave, id, dftp1, dftp2, difvel_1.difln);
	    iprint = 0;
	}
/* LOOP */
/* L210: */
    }
/* CONVERT TO CM/SEC */
    vel_1.wz *= 1e6;
    diflab_1.difxy = 0.;
    diflab_1.difyz = 0.;
    diflab_1.difxz = 0.;
    return 0;
} /* montea_ */

int StFtpcMagboltz2::monteb_()
{
    /* System generated locals */
    int i__1, i__2;
    double d__1, d__2;

    /* Local variables */
    static double ebar;
    static int ncol;
    static double delt, csqd, tlim, argz, vtot; static long rdum; 
    static double sume2, d__, e;
    static int i__;
    static double q, t, u;
    static int icans[64];
    static double tdash;
    static int intem, ncolm;
    static double e1, f4;
    static int j1, j2;
    static double coswt, r1, sinwt, sumdx, t2, r5, a2, b2, c2, r2, s1, r9,
	     extra, s2, r3, f3, r4, f8, f9, theta0, f6, sumyz, f5;
    static int id;
    static double const9;
    static int ie;
    static double ei;
    static int ik;
    static double r31;
    static int it;
    static double deltae, dz, dl2;
    static int iprint, j2m;
    static double dt2, cx1, cy1, cz1, cx2, cy2, cz2, sumlsq, sumtsq, 
	    sumxsq, sumysq, sumzsq, wbt;
    static int ipt;
    static double sto[10000], ef100, xst[10000], yst[10000], zst[10000], 
	    arg1, dcx1, dcy1, dcz1, dcx2, dcy2, dcz2, phi0, std1, std2;

/* ------------------------------------------------------------------- */
/*   CALCULATES COLLISION EVENTS AND UPDATES DIFFUSION AND VELOCITY. */
/*   SUBROUTINE HANDLES MAGNETIC FIELD AND ELECTRIC FIELD */
/*   BFIELD ALONG X-AXIS EFIELD ALONG Z-AXIS (90 DEGREES). */
/* ------------------------------------------------------------------- */
/*  SET ANISOTROPY CONTROL FOR ELASTIC COLLISIONS */
    for (i__ = 1; i__ <= 64; ++i__) {
	icans[i__ - 1] = 0;
	if (large_2.iarry[i__ - 1] == 1 && anis_1.kel[0] == 1) {
	    icans[i__ - 1] = 1;
	}
	if (large_2.iarry[i__ - 1] == 6 && anis_1.kel[1] == 1) {
	    icans[i__ - 1] = 1;
	}
	if (large_2.iarry[i__ - 1] == 11 && anis_1.kel[2] == 1) {
	    icans[i__ - 1] = 1;
	}
/* L1: */
	if (large_2.iarry[i__ - 1] == 16 && anis_1.kel[3] == 1) {
	    icans[i__ - 1] = 1;
	}
    }
    vel_1.wx = 0.;
    outpt_1.x = 0.;
    outpt_1.y = 0.;
    outpt_1.z__ = 0.;
    outpt_1.st = 0.;
    std1 = 0.;
    std2 = 0.;
    sume2 = 0.;
    sumxsq = 0.;
    sumysq = 0.;
    sumzsq = 0.;
    sumyz = 0.;
    sumlsq = 0.;
    sumtsq = 0.;
    setp_1.small = 1e-20;
    outpt_1.tmax1 = 0.;
    ef100 = setp_1.efield * 100.;
    rdum = (long)setp_1.rstart;
    HepRandom quasiRandom(rdum);

    e1 = setp_1.estart;
    intem = 10;
    inpt_1.itmax = 60;
    id = 0;
    ncol = 0;
    outpt_1.nnull = 0;
/*  NUMBER OF COLLISIONS FOR DE-CORRELATION */
    ncolm = 9998;
    iprint = 0;
    tdash = 0.;
    const9 = cnsts1_1.const3 * .01;

/*     INITIAL DIRECTION COSINES */

    dcz1 = cos(setp_1.theta);
    dcx1 = sin(setp_1.theta) * cos(setp_1.phi);
    dcy1 = sin(setp_1.theta) * sin(setp_1.phi);
/*     INITIAL VELOCITY */
    vtot = const9 * sqrt(e1);
    cx1 = dcx1 * vtot;
    cy1 = dcy1 * vtot;
    cz1 = dcz1 * vtot;
    f4 = setp_1.api * 2.;
    deltae = inpt_1.efinal / (double) intem;
    j2m = setp_1.nmax / inpt_1.itmax;
/* MAIN LOOP */
    i__1 = inpt_1.itmax;
    for (j1 = 1; j1 <= i__1; ++j1) {
	i__2 = j2m;
	for (j2 = 1; j2 <= i__2; ++j2) {
	    r1 = quasiRandom.flat();
	    i__ = (int) (e1 / deltae) + 1;
	    if(i__>10)
	      i__=10;
	    tlim = setp_1.tcfmax[i__ - 1];
	    t = -log(r1) / tlim + tdash;
	    tdash = t;
	    wbt = bfld_1.wb * t;
	    coswt = cos(wbt);
	    sinwt = sin(wbt);
	    dz = (cz1 * sinwt + (bfld_1.eovb - cy1) * (1. - coswt)) / 
		    bfld_1.wb;
	    e = e1 + dz * ef100;
/*     IF(E.LT.0.0) WRITE(6,983) J2,DZ,E1,COSWT,SINWT,WBT,CY1 */
/* 983  FORMAT(2X,' J2=',I12,' DZ=',E12.3,' E1=',E12.3,' COSWT=',E
12.3 */
/*    /,' SINWT=',E12.3,' WBT=',E12.3,' CY1=',E12.3) */
	    ie = (int) (e / inpt_1.estep) + 1;
	    ie = min(ie,2000);
	    if (large_2.tcf[ie - 1] <= tlim) {
		goto L122;
	    }
	    tdash += log(r1) / tlim;
	    setp_1.tcfmax[i__ - 1] *= 1.05;

/*     WRITE(6,996) */
/* 996  FORMAT(/,5X,' WARNING NULL COLLISION TIME INCREASED',/) */
	    goto L133;

/*     TEST FOR REAL OR NULL COLLISION */

L122:
	    r5 = quasiRandom.flat();
	    tlim = large_2.tcf[ie - 1] / tlim;
	    if (r5 <= tlim) {
		goto L137;
	    }
	    ++outpt_1.nnull;
	    goto L133;

/*  CALCULATE DIRECTION COSINES AND POSITIONS AT INSTANT BEFORE CO
LLISION */
/*    ALSO UPDATE DIFFUSION  AND ENERGY CALCULATIONS. */
L137:
	    t2 = t * t;
	    if (t >= outpt_1.tmax1) {
		outpt_1.tmax1 = t;
	    }
	    tdash = 0.;
/*  CALC VELOCITY */
	    cx2 = cx1;
	    cy2 = (cy1 - bfld_1.eovb) * coswt + cz1 * sinwt + bfld_1.eovb;
	    cz2 = cz1 * coswt - (cy1 - bfld_1.eovb) * sinwt;
/* CALC DIRECTION COSINE */
	    vtot = sqrt(cx2 * cx2 + cy2 * cy2 + cz2 * cz2);
	    dcx2 = cx2 / vtot;
	    dcy2 = cy2 / vtot;
	    dcz2 = cz2 / vtot;
	    ++ncol;
/* CALC NEW POSITION */
	    outpt_1.x += cx1 * t;
	    outpt_1.y = outpt_1.y + bfld_1.eovb * t + ((cy1 - bfld_1.eovb) * 
		    sinwt + cz1 * (1. - coswt)) / bfld_1.wb;
	    outpt_1.z__ += dz;
	    outpt_1.st += t;
	    it = (int) (t + 1.);
	    it = min(it,300);
	    outpt_1.time[it - 1] += 1.;
	    outpt_1.spec[ie - 1] += 1.;
	    vel_1.wz = outpt_1.z__ / outpt_1.st;
	    vel_1.wy = outpt_1.y / outpt_1.st;
	    sumdx += cx1 * cx1 * t2;
	    if (id == 0) {
		goto L121;
	    }
	    std2 += t;
	    delt = outpt_1.st - sto[ncol - 1];
/* Computing 2nd power */
	    d__1 = outpt_1.x - xst[ncol - 1];
	    sumxsq += d__1 * d__1 * t / delt;
	    if (j1 < 6) {
		goto L121;
	    }
	    std1 += t;
	    delt = outpt_1.st - sto[ncol - 1];
/* Computing 2nd power */
	    d__1 = outpt_1.z__ - zst[ncol - 1] - vel_1.wz * delt;
	    sumzsq += d__1 * d__1 * t / delt;
/* Computing 2nd power */
	    d__1 = outpt_1.y - yst[ncol - 1] - vel_1.wy * delt;
	    sumysq += d__1 * d__1 * t / delt;
	    sumyz += (outpt_1.z__ - zst[ncol - 1] - vel_1.wz * delt) * (
		    outpt_1.y - yst[ncol - 1] - vel_1.wy * delt) * t / delt;
/* Computing 2nd power */
	    d__1 = vel_1.wz * delt;
/* Computing 2nd power */
	    d__2 = vel_1.wy * delt;
	    a2 = d__1 * d__1 + d__2 * d__2;
/* Computing 2nd power */
	    d__1 = outpt_1.z__ - vel_1.wz * delt - zst[ncol - 1];
/* Computing 2nd power */
	    d__2 = outpt_1.y - vel_1.wy * delt - yst[ncol - 1];
	    b2 = d__1 * d__1 + d__2 * d__2;
/* Computing 2nd power */
	    d__1 = outpt_1.z__ - zst[ncol - 1];
/* Computing 2nd power */
	    d__2 = outpt_1.y - yst[ncol - 1];
	    c2 = d__1 * d__1 + d__2 * d__2;
/* Computing 2nd power */
	    d__1 = a2 + b2 - c2;
	    dl2 = d__1 * d__1 / (a2 * 4.);
	    dt2 = b2 - dl2;
	    sumlsq += dl2 * t / delt;
	    sumtsq += dt2 * t / delt;
L121:
	    xst[ncol - 1] = outpt_1.x;
	    yst[ncol - 1] = outpt_1.y;
	    zst[ncol - 1] = outpt_1.z__;
	    sto[ncol - 1] = outpt_1.st;
	    if (ncol < ncolm) {
		goto L135;
	    }
	    ++id;
	    outpt_1.xid = (double) id;
	    ncol = 0;
/* --------------------------------------------------------------
------- */
/*     DETERMINATION OF REAL COLLISION TYPE */
/* --------------------------------------------------------------
------- */
L135:
	    r2 = quasiRandom.flat();
	    i__ = 0;
L140:
	    ++i__;
	    if (large_2.cf[ie + i__ * 2000 - 2001] < r2) {
		goto L140;
	    }
	    s1 = large_2.rgas[i__ - 1];
	    ei = large_2.ein[i__ - 1];
	    if (large_2.ipn[i__ - 1] == 0) {
		goto L666;
	    }
/*  USE FLAT DISTRIBUTION OF  ELECTRON ENERGY BETWEEN E-EION AND 0
.0 EV */
/*  SAME AS IN BOLTZMANN */
	    r9 = quasiRandom.flat();
	    extra = r9 * (e - ei);
	    ei = extra + ei;

/*  GENERATE SCATTERING ANGLES AND UPDATE  LABORATORY COSINES AFTE
R */
/*   COLLISION ALSO UPDATE ENERGY OF ELECTRON. */

L666:
	    ipt = large_2.iarry[i__ - 1];
	    ++outpt_1.icoll[ipt - 1];
	    if (e < ei) {
/*      WRITE(6,994) E,EI,J2 */
/* 994  FORMAT(2X,' WARNING ENERGY =',F8.3,' LESS THAN ENERGY 
LOSS EI=',F8 */
/*    /.3,' AT ITER=',I12,' DUE TO BINNING ERROR') */
/*  FIX ENERGY LOSS SMALLER THAN INCIDENT ENERGY IF ERROR OCCU
RS */
		ei = e - 1e-4;
	    }
	    s2 = s1 * s1 / (s1 - 1.);
/* ANISOTROPY ANGLE CONTROL */
	    if (anis_1.niso == 0) {
		goto L55;
	    }
/*  ELASTIC ANISOTROPY */
	    if (icans[i__ - 1] == 1) {
		r31 = quasiRandom.flat();
		r3 = quasiRandom.flat();
		f3 = r3;
		if (ipt == 1 && r31 > anis_1.pel[(ie << 2) - 4]) {
		    f3 = -f3;
		}
		if (ipt == 6 && r31 > anis_1.pel[(ie << 2) - 3]) {
		    f3 = -f3;
		}
		if (ipt == 11 && r31 > anis_1.pel[(ie << 2) - 2]) {
		    f3 = -f3;
		}
		if (ipt == 16 && r31 > anis_1.pel[(ie << 2) - 1]) {
		    f3 = -f3;
		}
/* INELASTIC ANISOTROPY */
	    } else if (anis_1.index[i__ - 1] != 0) {
		r31 = quasiRandom.flat();
		r3 = quasiRandom.flat();
		f3 = r3;
		if (r31 > anis_1.pin[anis_1.index[i__ - 1] + (ie << 3) - 9]) {
		    f3 = -f3;
		}
	    } else {
/* ISOTROPIC */
		r3 = quasiRandom.flat();
		f3 = 1. - r3 * 2.;
	    }
	    goto L56;
L55:
	    r3 = quasiRandom.flat();
	    f3 = 1. - r3 * 2.;
L56:
	    theta0 = acos(f3);
	    r4 = quasiRandom.flat();
	    phi0 = f4 * r4;
	    f8 = sin(phi0);
	    f9 = cos(phi0);
	    arg1 = 1. - s1 * ei / e;
	    arg1 = max(arg1,setp_1.small);
	    d__ = 1. - f3 * sqrt(arg1);
	    e1 = e * (1. - ei / (s1 * e) - d__ * 2. / s2);
	    e1 = max(e1,setp_1.small);
	    q = sqrt(e / e1 * arg1) / s1;
	    q = min(q,1.);
	    setp_1.theta = asin(q * sin(theta0));
	    f6 = cos(setp_1.theta);
	    u = (s1 - 1.) * (s1 - 1.) / arg1;
	    csqd = f3 * f3;
	    if (f3 < 0. && csqd > u) {
		f6 *= -1.;
	    }
	    f5 = sin(setp_1.theta);
	    dcz2 = min(dcz2,1.);
	    vtot = const9 * sqrt(e1);
	    argz = sqrt(dcx2 * dcx2 + dcy2 * dcy2);
	    if (argz == 0.) {
/*      WRITE(6,9232) ITER,ID,E1 */
/* 9232  FORMAT(3X,'WARNING ARGZ= 0.0 AT ITER =',I10,' ID =',I
10,' E1=',E1 */
/*    /2.3) */
		dcz1 = f6;
		dcx1 = f9 * f5;
		dcy1 = f8 * f5;
		goto L130;
	    }
	    dcz1 = dcz2 * f6 + argz * f5 * f8;
	    dcy1 = dcy2 * f6 + f5 / argz * (dcx2 * f9 - dcy2 * dcz2 * f8);
	    dcx1 = dcx2 * f6 - f5 / argz * (dcy2 * f9 + dcx2 * dcz2 * f8);
/* CALCULATE VELOCITY VECTORS AFTER COLLISION */
L130:
	    cx1 = dcx1 * vtot;
	    cy1 = dcy1 * vtot;
	    cz1 = dcz1 * vtot;
L133:
	    ;
	}
/*   ------------------------------------------ */
	++iprint;
	if (j1 == 10) {
	  printf("   VELZ    VELY    POS     TIME     ENERGY    COUNT        DIFXX   DIFYY    DIFZZ    DIFYZ    DIFLONG  DIFTRANS\n");
	}
	if (iprint == 10) {
	    vel_1.wz *= 1e8;
	    vel_1.wy *= 1e8;
	    if (std2 != 0.) {
		diflab_1.difxx = sumxsq * 5e15 / std2;
	    }
	    if (std1 != 0.) {
		diflab_1.difyy = sumysq * 5e15 / std1;
	    }
	    if (std1 != 0.) {
		diflab_1.difzz = sumzsq * 5e15 / std1;
	    }
	    if (std1 != 0.) {
		diflab_1.difyz = sumyz * -5e15 / std1;
	    }
	    if (std1 != 0.) {
		difvel_1.difln = sumlsq * 5e15 / std1;
	    }
	    if (std1 != 0.) {
		difvel_1.diftr = sumtsq * 5e15 / std1;
	    }
	    ebar = 0.;
	    for (ik = 1; ik <= 2000; ++ik) {
/* L300: */
		ebar += mix2_2.es[ik - 1] * outpt_1.spec[ik - 1] / 
			large_2.tcf[ik - 1];
	    }
	    outpt_1.ave = ebar / outpt_1.st;
	    printf("%7.3f %7.3f %8.2e %8.2e  %7.4f  %6i %8.1f %8.1f %8.1f %8.1f %8.1f %8.1f\n", vel_1.wz, vel_1.wy, outpt_1.z__, outpt_1.st, outpt_1.ave, id, diflab_1.difxx, diflab_1.difyy, diflab_1.difzz, diflab_1.difyz, difvel_1.difln, difvel_1.diftr);
	    iprint = 0;
	}
/*  LOOP */
/* L210: */
    }
/* CONVERT TO CM/SEC */
    vel_1.wz *= 1e6;
    vel_1.wy *= 1e6;
    diflab_1.difxz = 0.;
    diflab_1.difxy = 0.;
    return 0;
} /* monteb_ */

int StFtpcMagboltz2::montec_()
{
  char benchname[20]="montec";
  mBench->Reset();
  mBench->Start(benchname);
  /* System generated locals */
  int i__1;
  double d__1, d__2;
  
  /* Local variables */
  double ebar;
  int ncol;
  double delt, csqd, tlim, argz, vtot, efx100, efz100; static long rdum;
  double sume2, d__, e;
  int i__;
  double q, t, u;
  int icans[64];
  double tdash;
  int ncolm, intem;
  double f1, e1, f4;
  int j1, j2;
  double r1, coswt, sinwt, t2, r5, a2, b2, c2, r2, s1;
  double s2, f3, r4, f8, f9, sumxy, sumxz, sumyz, theta0, f6, f5;
  int id;
  double const9;
  int ie;
  double ei;
  int ik;
  double r31, dx;
  int it;
  double deltae, dz, rtheta, dl2, difxxr;
  int iprint;
  double difyyr, difxyr, difzzr;
  int j2m;
  double dt2, difyzr, cx1, cy1, cz1, cx2, cy2, cz2, difxzr;
  double  sumlsq, sumtsq, sumxsq, sumysq, sumzsq, rcs, wbt;
  int ipt;
  double rsn, sto[10000], xst[10000], yst[10000], zst[10000], 
    wyr, wzr, wxr, arg1, dcx1, dcy1, dcz1, dcx2, dcy2, dcz2, phi0, 
    std1, std2;


/* ------------------------------------------------------------------- */
/*   CALCULATES COLLISION EVENTS AND UPDATES DIFFUSION AND VELOCITY. */
/*   SUBROUTINE SOLVES MOTION IN COORDINATE SYSTEM WITH BFIELD */
/*   ALIGNED ALONG X AXIS AND ELECTRIC FIELD AT AN ANGLE BTHETA IN */
/*   THE X-Z PLANE.  THE VELOCITY VECTORS AND DIFFUSION ARE THEN */
/*   ROTATED INTO THE STANDARD COORDINATE FRAME WITH THE ELECTRIC- */
/*   FIELD ALONG Z-AXIS AND THE  BFIELD AT AN ANGLE BTHETA TO THE */
/*   ELECTRIC FIELD IN THE X-Z PLANE. */
/* ------------------------------------------------------------------- */
/*  SET ANISOTROPY CONTROL FOR ELASTIC COLLISIONS */
  for (i__ = 0; i__ < 64; ++i__) {
    icans[i__] = 0;
    if (large_2.iarry[i__] == 1 && anis_1.kel[0] == 1) {
      icans[i__] = 1;
    }
    if (large_2.iarry[i__] == 6 && anis_1.kel[1] == 1) {
      icans[i__] = 1;
    }
    if (large_2.iarry[i__] == 11 && anis_1.kel[2] == 1) {
      icans[i__] = 1;
    }
    if (large_2.iarry[i__] == 16 && anis_1.kel[3] == 1) {
      icans[i__] = 1;
    }
  }
  outpt_1.x = 0.;
  outpt_1.y = 0.;
  outpt_1.z__ = 0.;
  outpt_1.st = 0.;
  std2 = 0.;
  std1 = 0.;
  sume2 = 0.;
  sumxsq = 0.;
  sumysq = 0.;
  sumzsq = 0.;
  sumyz = 0.;
  sumxy = 0.;
  sumxz = 0.;
  sumlsq = 0.;
  sumtsq = 0.;
  setp_1.small = 1e-20;
  outpt_1.tmax1 = 0.;
  /* CALC ROTATION MATRIX ANGLES */
  rcs = cos((bfld_1.btheta - 90.) * setp_1.api / 180.);
  rsn = sin((bfld_1.btheta - 90.) * setp_1.api / 180.);
  
  rtheta = bfld_1.btheta * setp_1.api / 180.;
  efz100 = setp_1.efield * 100. * sin(rtheta);
  efx100 = setp_1.efield * 100. * cos(rtheta);
  f1 = setp_1.efield * cnsts1_1.const2 * cos(rtheta);
  bfld_1.eovb *= sin(rtheta);
  rdum = (long)setp_1.rstart;
  HepRandom quasiRandom(rdum);
  
  e1 = setp_1.estart;
  intem = 10;
  inpt_1.itmax = 60;
  id = 0;
  ncol = -1;
  outpt_1.nnull = 0;
  ncolm = 9997;
  iprint = 0;
  tdash = 0.;
  const9 = cnsts1_1.const3 * .01;
  
  /*     INITIAL DIRECTION COSINES */
  
  dcz1 = cos(setp_1.theta);
  dcx1 = sin(setp_1.theta) * cos(setp_1.phi);
  dcy1 = sin(setp_1.theta) * sin(setp_1.phi);
  /*     INITIAL VELOCITY */
  vtot = const9 * sqrt(e1);
  cx1 = dcx1 * vtot;
  cy1 = dcy1 * vtot;
  cz1 = dcz1 * vtot;
  f4 = setp_1.api * 2.;
  deltae = inpt_1.efinal / 10.0;
  j2m = setp_1.nmax / inpt_1.itmax;
  printf("   VELZ    VELY    VELX    POS     TIME     ENERGY    COUNT   DIFTXX   DIFYY    DIFZZ    DIFYZ    DIFXZ    DIFXY\n");

  // additional variables for speed optimization
  double invDeltae = 1. / deltae;
  double invWb = 1. / bfld_1.wb;
  double invEstep = 1. / inpt_1.estep;
  double invSt = 1. / outpt_1.st;
  double ivtot, invDelt, invStd1;
  double esTcf[2000], invTcfmax[10], invRgas[64];
  for (i__ = 0; i__ < 2000; ++i__) 
    {
	esTcf[i__]= mix2_2.es[i__] / large_2.tcf[i__];
    }
  for (i__ = 0; i__ < 10; ++i__) 
    {
	invTcfmax[i__]= 1 / setp_1.tcfmax[i__];
    }
  for (i__ = 0; i__ < 64; ++i__) 
    {
	invRgas[i__]= 1 / large_2.rgas[i__];
    }
  /* MAIN LOOP */
  i__1 = inpt_1.itmax;
  for (j1 = 1; j1 <= i__1; ++j1) {
    for (j2 = 1; j2 <= j2m; ++j2) {
      r1 = log(quasiRandom.flat());
      i__ = (int) (e1 * invDeltae);
      if(i__>9)
	i__=9;
      tlim = setp_1.tcfmax[i__];
      double invTlim=invTcfmax[i__];
      t = -r1 * invTlim + tdash;
      t2=t*t;
      tdash = t;
      wbt = bfld_1.wb * t;
      coswt = cos(wbt);
      sinwt = sin(wbt);
      dz = (cz1 * sinwt + (bfld_1.eovb - cy1) * (1. - coswt)) * invWb;
      dx = cx1 * t + f1 * t2;
      e = e1 + dz * efz100 + dx * efx100;
      ie = (int) (e * invEstep);
      if(ie>1999)
	ie=1999;
      if (large_2.tcf[ie] > tlim) 
	{
	  tdash += r1 * invTlim;
	  setp_1.tcfmax[i__] *= 1.05;
	} 
      else
	{
	  /*     TEST FOR REAL OR NULL COLLISION */
	  r5 = quasiRandom.flat();
	  tlim = large_2.tcf[ie] * invTlim;
	  if (r5 > tlim) 
	    ++outpt_1.nnull;
	  else
	    {
	      /*  CALCULATE DIRECTION COSINES AND POSITIONS AT INSTANT BEFORE COLLISION */
	      /*    ALSO UPDATE DIFFUSION  AND ENERGY CALCULATIONS. */
	      if (t >= outpt_1.tmax1) {
		outpt_1.tmax1 = t;
	      }
	      tdash = 0.;
	      /*  CALC VELOCITY */
	      cx2 = cx1 + f1 * t;
	      cy2 = (cy1 - bfld_1.eovb) * coswt + cz1 * sinwt + bfld_1.eovb;
	      cz2 = cz1 * coswt - (cy1 - bfld_1.eovb) * sinwt;
	      /* CALC DIRECTION COSINE */
	      ivtot = 1/sqrt(cx2 * cx2 + cy2 * cy2 + cz2 * cz2);
	      dcx2 = cx2 * ivtot;
	      dcy2 = cy2 * ivtot;
	      dcz2 = cz2 * ivtot;
	      ++ncol;
	      /* CALC NEW POSITION */
	      outpt_1.x += dx;
	      outpt_1.y = outpt_1.y + bfld_1.eovb * t 
		+ ((cy1 - bfld_1.eovb) * sinwt + cz1 * (1. - coswt)) *invWb;
	      outpt_1.z__ += dz;
	      outpt_1.st += t;
	      invSt=1/outpt_1.st;
	      it = (int) t;
	      if(it>299)
		it=299;
	      outpt_1.time[it] += 1.;
	      outpt_1.spec[ie] += 1.;
	      vel_1.wz = outpt_1.z__ * invSt;
	      vel_1.wy = outpt_1.y * invSt;
	      vel_1.wx = outpt_1.x * invSt;
	      if (id != 0 && j1 >= 6) 
		{
		  std1 += t;
		  delt = outpt_1.st - sto[ncol];
		  invDelt=1/delt;
		  /* Computing 2nd power */
		  double zdiff = outpt_1.z__ - zst[ncol] - vel_1.wz * delt;
		  double ydiff = outpt_1.y - yst[ncol] - vel_1.wy * delt;
		  double xdiff = outpt_1.x - xst[ncol] - vel_1.wx * delt;
		  sumzsq += zdiff * zdiff * t * invDelt;
		  sumysq += ydiff * ydiff * t * invDelt;
		  sumxsq += xdiff * xdiff * t * invDelt;
		  sumyz += zdiff  * ydiff * t * invDelt;
		  sumxy += xdiff * ydiff * t * invDelt;
		  sumxz += xdiff * zdiff * t * invDelt;
		  /* Computing 2nd power */
		  d__1 = vel_1.wz * delt;
		  /* Computing 2nd power */
		  d__2 = vel_1.wy * delt;
		  a2 = d__1 * d__1 + d__2 * d__2;
		  /* Computing 2nd power */
		  b2 = zdiff * zdiff + ydiff * ydiff;
		  /* Computing 2nd power */
		  d__1 = outpt_1.z__ - zst[ncol];
		  /* Computing 2nd power */
		  d__2 = outpt_1.y - yst[ncol];
		  c2 = d__1 * d__1 + d__2 * d__2;
		  /* Computing 2nd power */
		  d__1 = a2 + b2 - c2;
		  dl2 = d__1 * d__1 / (a2 * 4.);
		  dt2 = b2 - dl2;
		  sumlsq += dl2 * t * invDelt;
		  sumtsq += dt2 * t * invDelt;
		}
	      xst[ncol] = outpt_1.x;
	      yst[ncol] = outpt_1.y;
	      zst[ncol] = outpt_1.z__;
	      sto[ncol] = outpt_1.st;
	      if (ncol >= ncolm) 
		{
		  ++id;
		  outpt_1.xid = (double) id;
		  ncol = -1;
		}
	      /* --------------------------------------------------------------------- */
	      /*     DETERMINATION OF REAL COLLISION TYPE */
	      /* --------------------------------------------------------------------- */
	      r2 = quasiRandom.flat();
	      i__=0;
	      while(large_2.cf[ie + i__ * 2000] < r2)
		i__++;
	      s1 = large_2.rgas[i__];
	      double invS1=invRgas[i__];
	      ei = large_2.ein[i__];
	      if (large_2.ipn[i__] != 0) 
		{
		  /*  USE FLAT DISTRIBUTION OF  ELECTRON ENERGY BETWEEN E-EION AND 0.0 EV */
		  /*  SAME AS IN BOLTZMANN */
		  ei = quasiRandom.flat()*(e - ei) + ei;
		  
		  /*  GENERATE SCATTERING ANGLES AND UPDATE  LABORATORY COSINES AFTER */
		  /*   COLLISION ALSO UPDATE ENERGY OF ELECTRON. */
		}
	      ipt = large_2.iarry[i__];
	      outpt_1.icoll[ipt - 1]++;
	      if (e < ei) {
		ei = e - 1e-4;
	      }
	      s2 = s1 * s1 / (s1 - 1.);
	      /* ANISOTROPY ANGLE CONTROL */
	      if (anis_1.niso != 0) 
		{
		  /* ELASTIC ANISOTROPY */
		  if (icans[i__] == 1) {
		    r31 = quasiRandom.flat();
		    f3 = quasiRandom.flat();
		    if (ipt == 1 && r31 > anis_1.pel[((ie+1) << 2) - 4]) {
		      f3 = -f3;
		    }
		    else if (ipt == 6 && r31 > anis_1.pel[((ie+1) << 2) - 3]) {
		      f3 = -f3;
		    }
		    else if (ipt == 11 && r31 > anis_1.pel[((ie+1) << 2) - 2]) {
		      f3 = -f3;
		    }
		    else if (ipt == 16 && r31 > anis_1.pel[((ie+1) << 2) - 1]) {
		      f3 = -f3;
		    }
		    /*  INELASTIC ANISOTROPY */
		  } else if (anis_1.index[i__] != 0) {
		    r31 = quasiRandom.flat();
		    f3 = quasiRandom.flat();
		    if (r31 > anis_1.pin[anis_1.index[i__] + ((ie+1) << 3) - 9]) {
		      f3 = -f3;
		    }
		  } else {
		    /* ISOTROPIC */
		    f3 = 1. - quasiRandom.flat() * 2.;
		  }
		}
	      else
		{
		  f3 = 1. - quasiRandom.flat() * 2.;
		}
	      theta0 = acos(f3);
	      r4 = quasiRandom.flat();
	      phi0 = f4 * r4;
	      f8 = sin(phi0);
	      f9 = cos(phi0);
	      double invE=1/e;
	      arg1 = 1. - s1 * ei *invE;
	      if(arg1<setp_1.small)
		arg1=setp_1.small;
	      d__ = 1. - f3 * sqrt(arg1);
	      e1 = e * (1. - ei * invS1 * invE - d__ * 2. / s2);
	      if(e1<setp_1.small)
		e1=setp_1.small;
	      q = sqrt(e / e1 * arg1) * invS1;
	      if(q>1.)
		q=1.;
	      f5=q * sin(theta0);
	      setp_1.theta = asin(f5);
	      f6 = cos(setp_1.theta);
	      u = (s1 - 1.) * (s1 - 1.) / arg1;
	      csqd = f3 * f3;
	      if (f3 < 0. && csqd > u) {
		f6 *= -1.;
	      }
	      if(dcz2>1.)
		dcz2=1.;
	      vtot = const9 * sqrt(e1);
	      argz = sqrt(dcx2 * dcx2 + dcy2 * dcy2);
	      if (argz == 0.) {
		dcz1 = f6;
		dcx1 = f9 * f5;
		dcy1 = f8 * f5;
	      }
	      else {
		dcz1 = dcz2 * f6 + argz * f5 * f8;
		dcy1 = dcy2 * f6 + f5 / argz * (dcx2 * f9 - dcy2 * dcz2 * f8);
		dcx1 = dcx2 * f6 - f5 / argz * (dcy2 * f9 + dcx2 * dcz2 * f8);
	      }
	      /* CALCULATE VELOCITY VECTORS AFTER COLLISION */
	      cx1 = dcx1 * vtot;
	      cy1 = dcy1 * vtot;
	      cz1 = dcz1 * vtot;
	    }
	}
    }
    /*   ------------------------------------------ */
    ++iprint;
    if (iprint == 10) 
      {
	vel_1.wz *= 1e8;
	vel_1.wy *= 1e8;
	vel_1.wx *= 1e8;
	if (std1 != 0.) 
	  {
	    invStd1=1/std1;
	    diflab_1.difxx = sumxsq * 5e15 * invStd1;
	    diflab_1.difyy = sumysq * 5e15 * invStd1;
	    diflab_1.difzz = sumzsq * 5e15 * invStd1;
	    diflab_1.difyz = sumyz * 5e15 * invStd1;
	    diflab_1.difxz = sumxz * 5e15 * invStd1;
	    diflab_1.difxy = sumxy * 5e15 * invStd1;
	  }
	ebar = 0.;
	for (ik = 0; ik < 2000; ++ik) 
	  {
	    ebar += esTcf[ik]* outpt_1.spec[ik]; 
	  }
	outpt_1.ave = ebar *invSt;
	/* CALCULATE  ROTATED VECTORS AND TENSOR . */
	wzr = vel_1.wz * rcs - vel_1.wx * rsn;
	wyr = vel_1.wy;
	wxr = vel_1.wz * rsn + vel_1.wx * rcs;
	difxxr = diflab_1.difxx * rcs * rcs + diflab_1.difzz * rsn * rsn 
	  + rcs * 2. * rsn * diflab_1.difxz;
	difyyr = diflab_1.difyy;
	difzzr = diflab_1.difxx * rsn * rsn + diflab_1.difzz * rcs * rcs 
	  - rcs * 2. * rsn * diflab_1.difxz;
	difxyr = rcs * diflab_1.difxy + rsn * diflab_1.difyz;
	difyzr = rsn * diflab_1.difxy - rcs * diflab_1.difyz;
	difxzr = (rcs * rcs - rsn * rsn) * diflab_1.difxz - rsn * rcs 
	  * (diflab_1.difxx - diflab_1.difzz);
	/* OUTPUT ROTATED VECTORS AND TENSOR */
	printf("%7.3f %7.3f %7.3f %8.2e %8.2e  %7.4f  %6i %8.1f %8.1f %8.1f %8.1f %8.1f %8.1f\n", wzr, wyr, wxr, outpt_1.z__, outpt_1.st, outpt_1.ave, id, difxxr, difyyr, difzzr, difyzr, difxzr, difxyr);
	iprint = 0;
      }
    /* LOOP */
  }
  /* LOAD ROTATED VALUES INTO ARRAYS */
  vel_1.wz = wzr;
  vel_1.wx = wxr;
  vel_1.wy = wyr;
  diflab_1.difxx = difxxr;
  diflab_1.difyy = difyyr;
  diflab_1.difzz = difzzr;
  diflab_1.difyz = difyzr;
  diflab_1.difxz = difxzr;
  diflab_1.difxy = difxyr;
  /* CONVERT TO CM/SEC. */
  vel_1.wz *= 1e6;
  vel_1.wy *= 1e6;
  vel_1.wx *= 1e6;
  mBench->Show(benchname);
  return 0;
} /* montec_ */

int StFtpcMagboltz2::elimit_(int *ielow)
{
    /* System generated locals */
    int i__1;

    /* Local variables */
    static double csqd, tlim, argz; static long rdum;
    static double d__, e;
    static int i__;
    static double q, t, u;
    static int icans[64];
    static double tdash;
    static int isamp, intem;
    static double extra, e1, f1, f2, f4;
    static int j1;
    static double f3, f8, f9, f6, f5, r1, r2, s1, s2, r5, r3, r4, r9, 
	    theta0;
    static int ie;
    static double const6, ap, bp, ei, r31, deltae;
    static int j2m, ipt;
    static double arg1, dcx1, dcy1, dcz1, dcx2, dcy2, dcz2, phi0;

/* ------------------------------------------------------------------- */
/*   CALCULATES COLLISION EVENTS AND TESTS TO FIND IF THE UPPER ENERGY */
/*   LIMIT FOR THE ELECTRON ENERGY IS EXCEEDED. */
/*    IF ENERGY LIMIT IS OK       IELOW = 0 */
/*    IF ENERGY LIMIT IS EXCEEDED IELOW = 1 */
/*   THE TEST IS CARRIED OUT FOR A SAMPLE OF COLLISIONS THAT ARE */
/*   SMALLER THAN THE FULL SAMPLE BY A FACTOR OF 1/ISAMP */

/*   USED WITH MAGNETIC FIELD B =0.0   ELECTRIC FIELD IN Z DIRECTION. */
/* ------------------------------------------------------------------- */
    isamp = 30;
    for (i__ = 1; i__ <= 64; ++i__) {
	icans[i__ - 1] = 0;
	if (large_2.iarry[i__ - 1] == 1 && anis_1.kel[0] == 1) {
	    icans[i__ - 1] = 1;
	}
	if (large_2.iarry[i__ - 1] == 6 && anis_1.kel[1] == 1) {
	    icans[i__ - 1] = 1;
	}
	if (large_2.iarry[i__ - 1] == 11 && anis_1.kel[2] == 1) {
	    icans[i__ - 1] = 1;
	}
/* L1: */
	if (large_2.iarry[i__ - 1] == 16 && anis_1.kel[3] == 1) {
	    icans[i__ - 1] = 1;
	}
    }
    setp_1.small = 1e-20;
    rdum = (long)setp_1.rstart;
    HepRandom quasiRandom(rdum);

    e1 = setp_1.estart;
    intem = 10;
    tdash = 0.;

/*     INITIAL DIRECTION COSINES */

    dcz1 = cos(setp_1.theta);
    dcx1 = sin(setp_1.theta) * cos(setp_1.phi);
    dcy1 = sin(setp_1.theta) * sin(setp_1.phi);

    bp = setp_1.efield * setp_1.efield * cnsts1_1.const1;
    f1 = setp_1.efield * cnsts1_1.const2;
    f2 = setp_1.efield * cnsts1_1.const3;
    f4 = setp_1.api * 2.;
    deltae = inpt_1.efinal / (double) intem;
    j2m = setp_1.nmax / isamp;
/* MAIN LOOP */
    i__1 = j2m;
    for (j1 = 1; j1 <= i__1; ++j1) {
	r1 = quasiRandom.flat();
	i__ = (int) (e1 / deltae) + 1;
	i__ = min(i__,10);
	tlim = setp_1.tcfmax[i__ - 1];
	t = -log(r1) / tlim + tdash;
	tdash = t;
	ap = dcz1 * f2 * sqrt(e1);
	e = e1 + (ap + bp * t) * t;
	ie = (int) (e / inpt_1.estep) + 1;
	ie = min(ie,2000);
	if (large_2.tcf[ie - 1] <= tlim) {
	    goto L122;
	}
	tdash += log(r1) / tlim;
	setp_1.tcfmax[i__ - 1] *= 1.05;
	goto L210;

/*     TEST FOR REAL OR NULL COLLISION */

L122:
	r5 = quasiRandom.flat();
	tlim = large_2.tcf[ie - 1] / tlim;
	if (r5 <= tlim) {
	    goto L137;
	}
	goto L210;

/*  CALCULATE DIRECTION COSINES AT INSTANT BEFORE COLLISION */

L137:
	if (ie == 2000) {
/* ELECTRON ENERGY OUT OF RANGE */
	    *ielow = 1;
	    return 0;
	}
	tdash = 0.;
	const6 = sqrt(e1 / e);
	dcx2 = dcx1 * const6;
	dcy2 = dcy1 * const6;
	dcz2 = dcz1 * const6 + setp_1.efield * t * cnsts1_1.const5 / sqrt(e);
/* ------------------------------------------------------------------
--- */
/*     DETERMINATION OF REAL COLLISION TYPE */
/* ------------------------------------------------------------------
--- */
/* L135: */
	r2 = quasiRandom.flat();
	i__ = 0;
L140:
	++i__;
	if (large_2.cf[ie + i__ * 2000 - 2001] < r2) {
	    goto L140;
	}
	s1 = large_2.rgas[i__ - 1];
	ei = large_2.ein[i__ - 1];
	if (large_2.ipn[i__ - 1] == 0) {
	    goto L666;
	}
	r9 = quasiRandom.flat();
	extra = r9 * (e - ei);
	ei = extra + ei;

/*  GENERATE SCATTERING ANGLES AND UPDATE  LABORATORY COSINES AFTER */
/*   COLLISION ALSO UPDATE ENERGY OF ELECTRON. */

L666:
	ipt = large_2.iarry[i__ - 1];
	if (e < ei) {
	    ei = e - 1e-4;
	}
	s2 = s1 * s1 / (s1 - 1.);
	if (anis_1.niso == 0) {
	    goto L55;
	}
	if (icans[i__ - 1] == 1) {
	    r31 = quasiRandom.flat();
	    r3 = quasiRandom.flat();
	    f3 = r3;
	    if (ipt == 1 && r31 > anis_1.pel[(ie << 2) - 4]) {
		f3 = -f3;
	    }
	    if (ipt == 6 && r31 > anis_1.pel[(ie << 2) - 3]) {
		f3 = -f3;
	    }
	    if (ipt == 11 && r31 > anis_1.pel[(ie << 2) - 2]) {
		f3 = -f3;
	    }
	    if (ipt == 16 && r31 > anis_1.pel[(ie << 2) - 1]) {
		f3 = -f3;
	    }
	} else if (anis_1.index[i__ - 1] != 0) {
	    r31 = quasiRandom.flat();
	    r3 = quasiRandom.flat();
	    f3 = r3;
	    if (r31 > anis_1.pin[anis_1.index[i__ - 1] + (ie << 3) - 9]) {
		f3 = -f3;
	    }
	} else {
	    r3 = quasiRandom.flat();
	    f3 = 1. - r3 * 2.;
	}
	goto L56;
L55:
	r3 = quasiRandom.flat();
	f3 = 1. - r3 * 2.;
L56:
	theta0 = acos(f3);
	r4 = quasiRandom.flat();
	phi0 = f4 * r4;
	f8 = sin(phi0);
	f9 = cos(phi0);
	arg1 = 1. - s1 * ei / e;
	arg1 = max(arg1,setp_1.small);
	d__ = 1. - f3 * sqrt(arg1);
	e1 = e * (1. - ei / (s1 * e) - d__ * 2. / s2);
	e1 = max(e1,setp_1.small);
	q = sqrt(e / e1 * arg1) / s1;
	q = min(q,1.);
	setp_1.theta = asin(q * sin(theta0));
	f6 = cos(setp_1.theta);
	u = (s1 - 1.) * (s1 - 1.) / arg1;
	csqd = f3 * f3;
	if (f3 < 0. && csqd > u) {
	    f6 *= -1.;
	}
	f5 = sin(setp_1.theta);
	dcz2 = min(dcz2,1.);
	argz = sqrt(dcx2 * dcx2 + dcy2 * dcy2);
	if (argz == 0.) {
	    dcz1 = f6;
	    dcx1 = f9 * f5;
	    dcy1 = f8 * f5;
	    goto L210;
	}
	dcz1 = dcz2 * f6 + argz * f5 * f8;
	dcy1 = dcy2 * f6 + f5 / argz * (dcx2 * f9 - dcy2 * dcz2 * f8);
	dcx1 = dcx2 * f6 - f5 / argz * (dcy2 * f9 + dcx2 * dcz2 * f8);
/* LOOP */
L210:
	;
    }
    *ielow = 0;
    return 0;
} /* elimit_ */

int StFtpcMagboltz2::elimitb_(int *ielow)
{
    /* System generated locals */
    int i__1;

    /* Local variables */
    static double csqd, tlim, argz, vtot; static long rdum;
    static double d__, e;
    static int i__;
    static double q, t, u;
    static int icans[64];
    static double tdash;
    static int isamp, intem;
    static double extra, e1, f4;
    static int j1;
    static double f3, f8, coswt, f9, f6, f5, r1, sinwt, r2, s1, r5, s2, 
	    r3, r4, r9, theta0, const9;
    static int ie;
    static double ei, r31, deltae, dz;
    static int j2m;
    static double cx1, cy1, cz1, cx2, cy2, cz2, wbt;
    static int ipt;
    static double ef100, arg1, dcx1, dcy1, dcz1, dcx2, dcy2, dcz2, phi0;

/* ------------------------------------------------------------------- */
/*   CALCULATES COLLISION EVENTS AND TESTS TO FIND IF THE UPPER ENERGY */
/*   LIMIT FOR THE ELECTRON ENERGY IS EXCEEDED. */
/*    IF ENERGY LIMIT IS OK       IELOW = 0 */
/*    IF ENERGY LIMIT IS EXCEEDED IELOW = 1 */
/*   THE TEST IS CARRIED OUT FOR A SAMPLE OF COLLISIONS THAT ARE */
/*   SMALLER THAN THE FULL SAMPLE BY A FACTOR OF 1/ISAMP */

/*   USED WITH MAGNETIC FIELD B AT 90 DEGREES TO ELECTRIC FIELD */
/* ------------------------------------------------------------------- */
    isamp = 30;
    for (i__ = 1; i__ <= 64; ++i__) {
	icans[i__ - 1] = 0;
	if (large_2.iarry[i__ - 1] == 1 && anis_1.kel[0] == 1) {
	    icans[i__ - 1] = 1;
	}
	if (large_2.iarry[i__ - 1] == 6 && anis_1.kel[1] == 1) {
	    icans[i__ - 1] = 1;
	}
	if (large_2.iarry[i__ - 1] == 11 && anis_1.kel[2] == 1) {
	    icans[i__ - 1] = 1;
	}
/* L1: */
	if (large_2.iarry[i__ - 1] == 16 && anis_1.kel[3] == 1) {
	    icans[i__ - 1] = 1;
	}
    }
    setp_1.small = 1e-20;
    ef100 = setp_1.efield * 100.;
    rdum = (long)setp_1.rstart;
    HepRandom quasiRandom(rdum);

    e1 = setp_1.estart;
    intem = 10;
    tdash = 0.;
    const9 = cnsts1_1.const3 * .01;

/*     INITIAL DIRECTION COSINES */

    dcz1 = cos(setp_1.theta);
    dcx1 = sin(setp_1.theta) * cos(setp_1.phi);
    dcy1 = sin(setp_1.theta) * sin(setp_1.phi);

    vtot = const9 * sqrt(e1);
    cx1 = dcx1 * vtot;
    cy1 = dcy1 * vtot;
    cz1 = dcz1 * vtot;
    f4 = setp_1.api * 2.;
    deltae = inpt_1.efinal / (double) intem;
    j2m = setp_1.nmax / isamp;
/* MAIN LOOP */
    i__1 = j2m;
    for (j1 = 1; j1 <= i__1; ++j1) {
	r1 = quasiRandom.flat();
	i__ = (int) (e1 / deltae) + 1;
	i__ = min(i__,10);
	tlim = setp_1.tcfmax[i__ - 1];
	t = -log(r1) / tlim + tdash;
	tdash = t;
	wbt = bfld_1.wb * t;
	coswt = cos(wbt);
	sinwt = sin(wbt);
	dz = (cz1 * sinwt + (bfld_1.eovb - cy1) * (1. - coswt)) / bfld_1.wb;
	e = e1 + dz * ef100;
	ie = (int) (e / inpt_1.estep) + 1;
	ie = min(ie,2000);
	if (large_2.tcf[ie - 1] <= tlim) {
	    goto L122;
	}
	tdash += log(r1) / tlim;
	setp_1.tcfmax[i__ - 1] *= 1.05;
	goto L210;

/*     TEST FOR REAL OR NULL COLLISION */

L122:
	r5 = quasiRandom.flat();
	tlim = large_2.tcf[ie - 1] / tlim;
	if (r5 <= tlim) {
	    goto L137;
	}
	goto L210;

/*  CALCULATE DIRECTION COSINES AT INSTANT BEFORE COLLISION */

L137:
	if (ie == 2000) {
/* ELECTRON ENERGY OUT OF RANGE */
	    *ielow = 1;
	    return 0;
	}
	tdash = 0.;
	cx2 = cx1;
	cy2 = (cy1 - bfld_1.eovb) * coswt + cz1 * sinwt + bfld_1.eovb;
	cz2 = cz1 * coswt - (cy1 - bfld_1.eovb) * sinwt;
	vtot = sqrt(cx2 * cx2 + cy2 * cy2 + cz2 * cz2);
	dcx2 = cx2 / vtot;
	dcy2 = cy2 / vtot;
	dcz2 = cz2 / vtot;
/* ------------------------------------------------------------------
--- */
/*     DETERMINATION OF REAL COLLISION TYPE */
/* ------------------------------------------------------------------
--- */
/* L135: */
	r2 = quasiRandom.flat();
	i__ = 0;
L140:
	++i__;
	if (large_2.cf[ie + i__ * 2000 - 2001] < r2) {
	    goto L140;
	}
	s1 = large_2.rgas[i__ - 1];
	ei = large_2.ein[i__ - 1];
	if (large_2.ipn[i__ - 1] == 0) {
	    goto L666;
	}
	r9 = quasiRandom.flat();
	extra = r9 * (e - ei);
	ei = extra + ei;

/*  GENERATE SCATTERING ANGLES AND UPDATE  LABORATORY COSINES AFTER */
/*   COLLISION ALSO UPDATE ENERGY OF ELECTRON. */

L666:
	ipt = large_2.iarry[i__ - 1];
	if (e < ei) {
	    ei = e - 1e-4;
	}
	s2 = s1 * s1 / (s1 - 1.);
	if (anis_1.niso == 0) {
	    goto L55;
	}
	if (icans[i__ - 1] == 1) {
	    r31 = quasiRandom.flat();
	    r3 = quasiRandom.flat();
	    f3 = r3;
	    if (ipt == 1 && r31 > anis_1.pel[(ie << 2) - 4]) {
		f3 = -f3;
	    }
	    if (ipt == 6 && r31 > anis_1.pel[(ie << 2) - 3]) {
		f3 = -f3;
	    }
	    if (ipt == 11 && r31 > anis_1.pel[(ie << 2) - 2]) {
		f3 = -f3;
	    }
	    if (ipt == 16 && r31 > anis_1.pel[(ie << 2) - 1]) {
		f3 = -f3;
	    }
	} else if (anis_1.index[i__ - 1] != 0) {
	    r31 = quasiRandom.flat();
	    r3 = quasiRandom.flat();
	    f3 = r3;
	    if (r31 > anis_1.pin[anis_1.index[i__ - 1] + (ie << 3) - 9]) {
		f3 = -f3;
	    }
	} else {
	    r3 = quasiRandom.flat();
	    f3 = 1. - r3 * 2.;
	}
	goto L56;
L55:
	r3 = quasiRandom.flat();
	f3 = 1. - r3 * 2.;
L56:
	theta0 = acos(f3);
	r4 = quasiRandom.flat();
	phi0 = f4 * r4;
	f8 = sin(phi0);
	f9 = cos(phi0);
	arg1 = 1. - s1 * ei / e;
	arg1 = max(arg1,setp_1.small);
	d__ = 1. - f3 * sqrt(arg1);
	e1 = e * (1. - ei / (s1 * e) - d__ * 2. / s2);
	e1 = max(e1,setp_1.small);
	q = sqrt(e / e1 * arg1) / s1;
	q = min(q,1.);
	setp_1.theta = asin(q * sin(theta0));
	f6 = cos(setp_1.theta);
	u = (s1 - 1.) * (s1 - 1.) / arg1;
	csqd = f3 * f3;
	if (f3 < 0. && csqd > u) {
	    f6 *= -1.;
	}
	f5 = sin(setp_1.theta);
	dcz2 = min(dcz2,1.);
	vtot = const9 * sqrt(e1);
	argz = sqrt(dcx2 * dcx2 + dcy2 * dcy2);
	if (argz == 0.) {
	    dcz1 = f6;
	    dcx1 = f9 * f5;
	    dcy1 = f8 * f5;
	    goto L130;
	}
	dcz1 = dcz2 * f6 + argz * f5 * f8;
	dcy1 = dcy2 * f6 + f5 / argz * (dcx2 * f9 - dcy2 * dcz2 * f8);
	dcx1 = dcx2 * f6 - f5 / argz * (dcy2 * f9 + dcx2 * dcz2 * f8);
L130:
	cx1 = dcx1 * vtot;
	cy1 = dcy1 * vtot;
	cz1 = dcz1 * vtot;
/* LOOP */
L210:
	;
    }
    *ielow = 0;
    return 0;
} /* elimitb_ */

int StFtpcMagboltz2::elimitc_(int *ielow)
{
    /* System generated locals */
    int i__1;

    /* Local variables */
    static double csqd, tlim, argz, vtot, efx100, efz100, eovb1; static long rdum;
    static double d__, e;
    static int i__;
    static double q, t, u;
    static int icans[64];
    static double tdash;
    static int isamp, intem;
    static double extra, f1, e1, f4;
    static int j1;
    static double f3, f8, coswt, f9, f6, f5, r1, sinwt, r2, s1, r5, s2, 
	    r3, r4, r9, theta0, const9;
    static int ie;
    static double ei, r31, dx, deltae, dz, rtheta;
    static int j2m;
    static double cx1, cy1, cz1, cx2, cy2, cz2, wbt;
    static int ipt;
    static double arg1, dcx1, dcy1, dcz1, dcx2, dcy2, dcz2, phi0;

/* ------------------------------------------------------------------- */
/*   CALCULATES COLLISION EVENTS AND TESTS TO FIND IF THE UPPER ENERGY */
/*   LIMIT FOR THE ELECTRON ENERGY IS EXCEEDED. */
/*    IF ENERGY LIMIT IS OK       IELOW = 0 */
/*    IF ENERGY LIMIT IS EXCEEDED IELOW = 1 */
/*   THE TEST IS CARRIED OUT FOR A SAMPLE OF COLLISIONS THAT ARE */
/*   SMALLER THAN THE FULL SAMPLE BY A FACTOR OF 1/ISAMP */

/*   USED WITH MAGNETIC FIELD B AT ANGLES BETWEEN 0 AND 90 DEGREES TO */
/*   THE ELECTRIC FIELD. */
/* ------------------------------------------------------------------- */
    isamp = 30;
    for (i__ = 1; i__ <= 64; ++i__) {
	icans[i__ - 1] = 0;
	if (large_2.iarry[i__ - 1] == 1 && anis_1.kel[0] == 1) {
	    icans[i__ - 1] = 1;
	}
	if (large_2.iarry[i__ - 1] == 6 && anis_1.kel[1] == 1) {
	    icans[i__ - 1] = 1;
	}
	if (large_2.iarry[i__ - 1] == 11 && anis_1.kel[2] == 1) {
	    icans[i__ - 1] = 1;
	}
/* L1: */
	if (large_2.iarry[i__ - 1] == 16 && anis_1.kel[3] == 1) {
	    icans[i__ - 1] = 1;
	}
    }
    setp_1.small = 1e-20;
    rtheta = bfld_1.btheta * setp_1.api / 180.;
    efz100 = setp_1.efield * 100. * sin(rtheta);
    efx100 = setp_1.efield * 100. * cos(rtheta);
    f1 = setp_1.efield * cnsts1_1.const2 * cos(rtheta);
    eovb1 = bfld_1.eovb * sin(rtheta);
    rdum = (long)setp_1.rstart;
    HepRandom quasiRandom(rdum);

    e1 = setp_1.estart;
    intem = 10;
    tdash = 0.;
    const9 = cnsts1_1.const3 * .01;

/*     INITIAL DIRECTION COSINES */

    dcz1 = cos(setp_1.theta);
    dcx1 = sin(setp_1.theta) * cos(setp_1.phi);
    dcy1 = sin(setp_1.theta) * sin(setp_1.phi);

    vtot = const9 * sqrt(e1);
    cx1 = dcx1 * vtot;
    cy1 = dcy1 * vtot;
    cz1 = dcz1 * vtot;
    f4 = setp_1.api * 2.;
    deltae = inpt_1.efinal / (double) intem;
    j2m = setp_1.nmax / isamp;
/* MAIN LOOP */
    i__1 = j2m;
    for (j1 = 1; j1 <= i__1; ++j1) {
	r1 = quasiRandom.flat();
	i__ = (int) (e1 / deltae) + 1;
	i__ = min(i__,10);
	tlim = setp_1.tcfmax[i__ - 1];
	t = -log(r1) / tlim + tdash;
	tdash = t;
	wbt = bfld_1.wb * t;
	coswt = cos(wbt);
	sinwt = sin(wbt);
	dz = (cz1 * sinwt + (eovb1 - cy1) * (1. - coswt)) / bfld_1.wb;
	dx = cx1 * t + f1 * t * t;
	e = e1 + dz * efz100 + dx * efx100;
	ie = (int) (e / inpt_1.estep) + 1;
	ie = min(ie,2000);
	if (large_2.tcf[ie - 1] <= tlim) {
	    goto L122;
	}
	tdash += log(r1) / tlim;
	setp_1.tcfmax[i__ - 1] *= 1.05;
	goto L210;

/*     TEST FOR REAL OR NULL COLLISION */

L122:
	r5 = quasiRandom.flat();
	tlim = large_2.tcf[ie - 1] / tlim;
	if (r5 <= tlim) {
	    goto L137;
	}
	goto L210;

/*  CALCULATE DIRECTION COSINES AT INSTANT BEFORE COLLISION */

L137:
	if (ie == 2000) {
/* ELECTRON ENERGY OUT OF RANGE */
	    *ielow = 1;
	    return 0;
	}
	tdash = 0.;
	cx2 = cx1 + f1 * t;
	cy2 = (cy1 - eovb1) * coswt + cz1 * sinwt + eovb1;
	cz2 = cz1 * coswt - (cy1 - eovb1) * sinwt;
	vtot = sqrt(cx2 * cx2 + cy2 * cy2 + cz2 * cz2);
	dcx2 = cx2 / vtot;
	dcy2 = cy2 / vtot;
	dcz2 = cz2 / vtot;
/* ------------------------------------------------------------------
--- */
/*     DETERMINATION OF REAL COLLISION TYPE */
/* ------------------------------------------------------------------
--- */
/* L135: */
	r2 = quasiRandom.flat();
	i__ = 0;
L140:
	++i__;
	if (large_2.cf[ie + i__ * 2000 - 2001] < r2) {
	    goto L140;
	}
	s1 = large_2.rgas[i__ - 1];
	ei = large_2.ein[i__ - 1];
	if (large_2.ipn[i__ - 1] == 0) {
	    goto L666;
	}
	r9 = quasiRandom.flat();
	extra = r9 * (e - ei);
	ei = extra + ei;

/*  GENERATE SCATTERING ANGLES AND UPDATE  LABORATORY COSINES AFTER */
/*   COLLISION ALSO UPDATE ENERGY OF ELECTRON. */

L666:
	ipt = large_2.iarry[i__ - 1];
	if (e < ei) {
	    ei = e - 1e-4;
	}
	s2 = s1 * s1 / (s1 - 1.);
	if (anis_1.niso == 0) {
	    goto L55;
	}
	if (icans[i__ - 1] == 1) {
	    r31 = quasiRandom.flat();
	    r3 = quasiRandom.flat();
	    f3 = r3;
	    if (ipt == 1 && r31 > anis_1.pel[(ie << 2) - 4]) {
		f3 = -f3;
	    }
	    if (ipt == 6 && r31 > anis_1.pel[(ie << 2) - 3]) {
		f3 = -f3;
	    }
	    if (ipt == 11 && r31 > anis_1.pel[(ie << 2) - 2]) {
		f3 = -f3;
	    }
	    if (ipt == 16 && r31 > anis_1.pel[(ie << 2) - 1]) {
		f3 = -f3;
	    }
	} else if (anis_1.index[i__ - 1] != 0) {
	    r31 = quasiRandom.flat();
	    r3 = quasiRandom.flat();
	    f3 = r3;
	    if (r31 > anis_1.pin[anis_1.index[i__ - 1] + (ie << 3) - 9]) {
		f3 = -f3;
	    }
	} else {
	    r3 = quasiRandom.flat();
	    f3 = 1. - r3 * 2.;
	}
	goto L56;
L55:
	r3 = quasiRandom.flat();
	f3 = 1. - r3 * 2.;
L56:
	theta0 = acos(f3);
	r4 = quasiRandom.flat();
	phi0 = f4 * r4;
	f8 = sin(phi0);
	f9 = cos(phi0);
	arg1 = 1. - s1 * ei / e;
	arg1 = max(arg1,setp_1.small);
	d__ = 1. - f3 * sqrt(arg1);
	e1 = e * (1. - ei / (s1 * e) - d__ * 2. / s2);
	e1 = max(e1,setp_1.small);
	q = sqrt(e / e1 * arg1) / s1;
	q = min(q,1.);
	setp_1.theta = asin(q * sin(theta0));
	f6 = cos(setp_1.theta);
	u = (s1 - 1.) * (s1 - 1.) / arg1;
	csqd = f3 * f3;
	if (f3 < 0. && csqd > u) {
	    f6 *= -1.;
	}
	f5 = sin(setp_1.theta);
	dcz2 = min(dcz2,1.);
	vtot = const9 * sqrt(e1);
	argz = sqrt(dcx2 * dcx2 + dcy2 * dcy2);
	if (argz == 0.) {
	    dcz1 = f6;
	    dcx1 = f9 * f5;
	    dcy1 = f8 * f5;
	    goto L130;
	}
	dcz1 = dcz2 * f6 + argz * f5 * f8;
	dcy1 = dcy2 * f6 + f5 / argz * (dcx2 * f9 - dcy2 * dcz2 * f8);
	dcx1 = dcx2 * f6 - f5 / argz * (dcy2 * f9 + dcx2 * dcz2 * f8);
L130:
	cx1 = dcx1 * vtot;
	cy1 = dcy1 * vtot;
	cz1 = dcz1 * vtot;
/* LOOP */
L210:
	;
    }
    *ielow = 0;
    return 0;
} /* elimitc_ */


int StFtpcMagboltz2::gas2_(double *q, double *qin, int *nin, 
			   double *e, double *ein, char *name__, 
			   double *virial, int *monte, int name_len)
{
    /* Initialized data */

    static double xen[44] = { 1.,1.2,1.5,1.7,2.,2.5,3.,4.,4.9,5.,6.,6.67,
	    7.,8.,8.71,9.,10.,11.,12.,13.,13.6,14.,15.,16.,16.5,18.,20.,25.,
	    30.,40.,50.,60.,70.,80.,100.,150.,200.,400.,1e3,2e3,4e3,1e4,2e4,
	    1e5 };
    static double yxsec[44] = { 1.3913,1.66,2.05,2.33,2.7,3.43,4.15,5.65,
	    7.26,7.46,9.32,10.6,11.3,13.1,14.1,14.4,15.4,15.8,15.8,15.4,15.1,
	    14.8,14.1,13.2,13.,11.4,10.2,7.8,6.25,4.45,3.5,2.8,2.2,2.,1.45,.9,
	    .63,.28,.6,.2,.1,.0048,.0018,9e-5 };
    static double xeni[76] = { 15.7,16.,16.5,17.,17.5,18.,18.5,19.,19.5,
	    20.,20.5,21.,21.5,22.,22.5,23.,23.5,24.,24.5,25.,25.5,26.,28.,30.,
	    32.,34.,36.,38.,40.,45.,50.,55.,60.,65.,70.,75.,80.,85.,90.,95.,
	    100.,110.,120.,130.,140.,150.,160.,180.,200.,250.,300.,350.,400.,
	    450.,500.,600.,700.,800.,900.,1e3,1200.,1400.,1600.,1800.,2e3,
	    2500.,3e3,3500.,4e3,5e3,6e3,8e3,1e4,1.4e4,2e4,1e5 };
    static double yxeni[76] = { -.2,.306,.825,1.126,1.326,1.468,1.577,
	    1.663,1.737,1.797,1.853,1.896,1.933,1.97,1.997,2.024,2.048,2.071,
	    2.094,2.115,2.132,2.148,2.204,2.256,2.293,2.325,2.351,2.368,2.379,
	    2.396,2.404,2.414,2.424,2.436,2.443,2.45,2.454,2.455,2.456,2.456,
	    2.455,2.452,2.448,2.441,2.436,2.429,2.419,2.401,2.379,2.337,2.296,
	    2.258,2.225,2.19,2.164,2.115,2.065,2.027,1.994,1.961,1.892,1.844,
	    1.811,1.767,1.727,1.656,1.591,1.538,1.486,1.413,1.349,1.242,1.166,
	    1.05,.923,.224 };
    static double xin[26] = { 11.55,13.,13.2,13.4,14.,16.,20.,30.,40.,50.,
	    60.,80.,100.,150.,200.,300.,500.,700.,1e3,1400.,2e3,4e3,6e3,1e4,
	    2e4,1e5 };
    static double yxsin[26] = { 0.,.069,.09,.087,.115,.205,.22,.25,.29,
	    .34,.31,.265,.24,.18,.15,.115,.08,.063,.047,.036,.028,.016,.0115,
	    .007,.0036,7.2e-4 };
    static double yxpin[26] = { 0.,0.,.012,.036,.072,.205,.42,.54,.53,.5,
	    .46,.39,.34,.26,.21,.165,.11,.083,.06,.046,.035,.02,.014,.009,
	    .0042,9e-4 };
    static double yxdin[26] = { 0.,0.,0.,0.,0.,.067,.15,.29,.35,.39,.41,
	    .47,.47,.44,.37,.285,.19,.15,.11,.081,.061,.035,.0245,.016,.008,
	    .0016 };

    /* System generated locals */
    int i__1, i__2;
    double d__1;

    /* Local variables */
    static double apol;
    static int lmax;
    static double sumi, a, b;
    static int i__, j, ndata;
    static double a1, aa, dd, ff, ak, en;
    static int nidata, nxdata;
    static double ak2, ak3, ak4, an0, an1, an2, api, sum;

    /* Parameter adjustments */
    --ein;
    --e;
    qin -= 21;
    q -= 7;

    /* Function Body */
    sprintf(name__, " ARGON  1997   ");
/* ---------------------------------------------------------------- */
/*  MULTI-TERM CROSS-SECTION. */
/*  FOR PURE ARGON: */
/*  ACCURACY OF DERIVED VELOCITY AND DIFFUSION COEFFICIENTS 0.5% BELOW */
/*  3000VOLTS . BELOW 20000VOLTS ACCURACY 1.0%. IONISATION COEFFICIENT */
/*  AND DRIFT VELOCITY ACCURACY BETTER THAN  5% BELOW 1,000,000 VOLTS */
/* ----------------------------------------------------------------- */

/*  PARAMETERS OF PHASE SHIFT ANALYSIS. */

    apol = (float)11.08;
    lmax = 100;
    aa = (float)-1.459;
    dd = (float)68.93;
    ff = (float)-97.;
    a1 = (float)8.69;
    api = (float)3.141592654;

    *nin = 3;
    ndata = 44;
    nidata = 76;
    nxdata = 26;
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
	ak = sqrt(en / inpt_1.ary);
	ak2 = ak * ak;
	ak3 = ak2 * ak;
	ak4 = ak3 * ak;
	an0 = -aa * ak * (apol * (float)4. / (float)3. * ak2 * log(ak) + (
		float)1.) - api * apol / (float)3. * ak2 + dd * ak3 + ff * 
		ak4;
	an1 = api / (float)15. * apol * ak2 - a1 * ak3;
	an2 = api * apol * ak2 / (float)105.;
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
	    d__1 = sin(atan(api * apol * ak2 * sumi));
	    sum += (j + (float)1.) * (d__1 * d__1);
/* L10: */
	}
	q[i__ * 6 + 2] = sum * (float)4. * cnsts_1.pir2 / ak2;
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
	q[i__ * 6 + 3] = pow(c_b524, d__1) * (float)1e-18;
L230:
	q[i__ * 6 + 4] = (float)0.;
	q[i__ * 6 + 5] = (float)0.;
	q[i__ * 6 + 6] = (float)0.;

	qin[i__ * 20 + 1] = (float)0.;
	qin[i__ * 20 + 2] = (float)0.;
	qin[i__ * 20 + 3] = (float)0.;
	if (en <= ein[1]) {
	    goto L400;
	}
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
	qin[i__ * 20 + 1] = (a * en + b) * (float)1e-16;
	if (en <= ein[2]) {
	    goto L400;
	}
	a = (yxpin[j - 1] - yxpin[j - 2]) / (xin[j - 1] - xin[j - 2]);
	b = (xin[j - 2] * yxpin[j - 1] - xin[j - 1] * yxpin[j - 2]) / (xin[j 
		- 2] - xin[j - 1]);
	qin[i__ * 20 + 2] = (a * en + b) * (float)1e-16;
	if (en <= ein[3]) {
	    goto L400;
	}
	a = (yxdin[j - 1] - yxdin[j - 2]) / (xin[j - 1] - xin[j - 2]);
	b = (xin[j - 2] * yxdin[j - 1] - xin[j - 1] * yxdin[j - 2]) / (xin[j 
		- 2] - xin[j - 1]);
	qin[i__ * 20 + 3] = (a * en + b) * (float)1e-16;
L400:
	q[i__ * 6 + 1] = q[i__ * 6 + 2] + q[i__ * 6 + 3] + qin[i__ * 20 + 1] 
		+ qin[i__ * 20 + 2] + qin[i__ * 20 + 3];
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
} /* gas2_ */

int StFtpcMagboltz2::gas3_(double *q, double *qin, int *nin, 
			   double *e, double *ein, char *name__, 
			   double *virial, int *monte, int name_len)
{
    /* Initialized data */

    static double xen[67] = { 0.,.008,.009,.01,.013,.017,.02,.025,.03,.04,
	    .05,.06,.07,.08,.09,.1,.12,.15,.18,.2,.25,.3,.4,.5,.6,.7,.8,.9,1.,
	    1.2,1.5,1.8,2.,2.5,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.6,16.5,18.,
	    20.,25.,30.,40.,50.,60.,70.,75.,80.,90.,100.,150.,200.,400.,600.,
	    1e3,2e3,1e4,2e4,1e5 };
    static double yxsec[67] = { 4.9,5.18,5.19,5.21,5.26,5.31,5.35,5.41,
	    5.46,5.54,5.62,5.68,5.74,5.79,5.83,5.86,5.94,6.04,6.12,6.16,6.27,
	    6.35,6.49,6.59,6.66,6.73,6.77,6.82,6.85,6.91,6.96,6.98,6.99,6.96,
	    6.89,6.62,6.31,6.,5.68,5.35,5.03,4.72,4.44,4.15,3.83,3.25,2.99,
	    2.58,2.,1.6,1.06,.77,.57,.46,.4,.37,.3,.26,.132,.081,.024,.012,
	    .0048,.0014,8e-5,2e-5,1.2e-6 };
    static double xion[48] = { 24.587,25.,25.5,26.,26.5,27.,28.,29.,30.,
	    32.,34.,36.,38.,40.,45.,50.,55.,60.,70.,80.,100.,120.,150.,175.,
	    200.,250.,300.,400.,500.,600.,700.,800.,900.,1e3,1200.,1400.,
	    1600.,1800.,2e3,2500.,3e3,4e3,5e3,6e3,8e3,1e4,2e4,1e5 };
    static double yion[48] = { 0.,.0051,.0111,.0172,.0232,.029,.042,.054,
	    .066,.091,.112,.133,.153,.169,.207,.239,.267,.286,.316,.339,.361,
	    .367,.364,.354,.342,.316,.293,.253,.221,.197,.177,.163,.148,.138,
	    .119,.103,.095,.086,.078,.065,.055,.044,.036,.032,.025,.021,.0117,
	    .004 };
    static double xexc[25] = { 19.82,20.,20.2,20.5,20.6,20.8,21.,21.3,22.,
	    25.,30.,40.,50.,60.,70.,80.,90.,100.,150.,200.,400.,1e3,1e4,2e4,
	    1e5 };
    static double yexc[25] = { 0.,.047,.053,.035,.029,.043,.042,.041,.046,
	    .075,.071,.054,.038,.026,.017,.013,.0094,.0075,.0022,9.4e-4,
	    1.2e-4,8e-6,8e-9,1e-9,3e-10 };
    static double xexs[34] = { 20.61,20.9,21.,21.5,22.,22.5,25.,28.,30.,
	    35.,40.,45.,50.,60.,70.,80.,90.,100.,150.,200.,300.,400.,500.,
	    600.,800.,1e3,1500.,2e3,3e3,4e3,6e3,1e4,2e4,1e5 };
    static double yexs[34] = { 0.,.025,.022,.0265,.0315,.036,.065,.082,
	    .092,.115,.133,.148,.155,.175,.177,.178,.178,.177,.163,.148,.121,
	    .099,.086,.075,.061,.051,.038,.03,.022,.017,.013,.0088,.0052,
	    .0018 };

    /* System generated locals */
    int i__1, i__2;

    /* Local variables */
    static int nexc, nion, nexs;
    static double a, b;
    static int i__, j, ndata;
    static double en;

    /* Parameter adjustments */
    --ein;
    --e;
    qin -= 21;
    q -= 7;

    /* Function Body */

/* DECOMMENT TO INCLUDE ANISOTROPIC SCATTERING FOR DELTA CALCULATION */
/*    /1.58,1.26,1.00,0.85,0.79,0.74,0.66,0.57,0.35,0.24, */
/*    /.095,.049,.018,.005,.00018,.00005,.00001/ */
/* ---------------------------------------------------------------- */
/* TRIPLET EXCITATION */
/*  SINGLET EXCITATION */
    sprintf(name__, "  HELIUM4 97   ");
/* -------------------------------------------------------------------- */
/*  HELIUM 4 BEST KNOWN GAS USED AS STANDARD ACCURACY BETTER THAN 0.2% */
/*  AT ALL FIELDS. */
/*  UPDATED 1992 TO INCLUDE 20KEV RANGE ALSO ELASTIC ANISOTROPIC */
/*   INCLUDED AS OPTION */
/* -------------------------------------------------------------------- */
    *nin = 2;
    ndata = 67;
    nion = 48;
    nexc = 25;
    nexs = 34;
    e[1] = (float)0.;
    e[2] = cnsts_1.emass * (float)2. / (cnsts_1.amu * (float)4.0026);
    e[3] = (float)24.587;
    e[4] = (float)0.;
    e[5] = (float)0.;
    e[6] = (float)0.;
    ein[1] = (float)19.82;
    ein[2] = (float)20.61;
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

	qin[i__ * 20 + 1] = (float)0.;
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
	qin[i__ * 20 + 1] = (a * en + b) * (float)1e-16;
L600:

	qin[i__ * 20 + 2] = (float)0.;
	if (en <= ein[2]) {
	    goto L700;
	}
	i__2 = nexs;
	for (j = 2; j <= i__2; ++j) {
	    if (en <= xexs[j - 1]) {
		goto L620;
	    }
/* L610: */
	}
	j = nexs;
L620:
	a = (yexs[j - 1] - yexs[j - 2]) / (xexs[j - 1] - xexs[j - 2]);
	b = (xexs[j - 2] * yexs[j - 1] - xexs[j - 1] * yexs[j - 2]) / (xexs[j 
		- 2] - xexs[j - 1]);
	qin[i__ * 20 + 2] = (a * en + b) * (float)1e-16;
L700:

	q[i__ * 6 + 1] = q[i__ * 6 + 2] + q[i__ * 6 + 3] + qin[i__ * 20 + 1] 
		+ qin[i__ * 20 + 2];
/* L900: */
    }
/*  SAVE COMPUTE TIME */
    if (inpt_1.efinal <= ein[2]) {
	*nin = 1;
    }
    if (inpt_1.efinal <= ein[1]) {
	*nin = 0;
    }

    return 0;
} /* gas3_ */


int StFtpcMagboltz2::gas5_(double *q, double *qin, int *nin, 
			   double *e, double *ein, char *name__, 
			   double *virial, int *monte, int name_len)
{
    /* Initialized data */

    static double xen[43] = { 1.,1.2,1.5,1.8,2.,2.5,3.,4.,5.,6.,7.,8.,
	    8.71,9.,10.,11.,13.6,15.,16.5,19.6,20.,30.,40.,50.,60.,70.,77.,
	    100.,130.,150.,170.,200.,300.,400.,600.,800.,1e3,2e3,4e3,1e4,2e4,
	    4e4,1e5 };
    static double yxsec[43] = { 1.62,1.69,1.75,1.79,1.82,1.86,1.91,1.98,
	    2.07,2.14,2.21,2.29,2.35,2.37,2.44,2.51,2.66,2.71,2.76,2.83,2.84,
	    2.84,2.78,2.58,2.3,2.12,2.03,1.53,1.21,1.03,.9,.756,.52,.42,.33,
	    .27,.25,.13,.075,.034,.019,.011,.005 };
    static double xion[68] = { 21.56,22.,22.5,23.,23.5,24.,24.5,25.,25.5,
	    26.,27.,28.,29.,30.,32.,34.,36.,40.,45.,50.,55.,60.,65.,70.,75.,
	    80.,90.,100.,110.,120.,140.,150.,175.,200.,250.,300.,350.,400.,
	    500.,600.,700.,800.,900.,1e3,1200.,1400.,1600.,1800.,2e3,2500.,
	    3e3,3500.,4e3,4500.,5e3,5500.,6e3,7e3,8e3,9e3,1e4,1.2e4,1.4e4,
	    1.6e4,1.8e4,2e4,5e4,1e5 };
    static double yion[68] = { 0.,.0033,.0089,.0146,.02,.026,.032,.038,
	    .044,.05,.063,.076,.089,.102,.128,.154,.179,.228,.282,.338,.391,
	    .435,.477,.514,.547,.577,.628,.667,.7,.725,.757,.772,.781,.781,
	    .757,.722,.686,.628,.586,.528,.484,.444,.413,.386,.333,.301,.273,
	    .248,.23,.195,.168,.149,.133,.122,.113,.104,.0976,.086,.0772,
	    .0706,.0649,.0563,.0495,.0444,.0406,.0373,.0183,.0109 };
    static double xexc[50] = { 16.615,16.78,16.97,17.3,18.4,18.7,18.8,
	    19.8,20.,21.,22.,24.,26.,28.,30.,35.,40.,50.,60.,70.,80.,90.,100.,
	    120.,150.,200.,250.,300.,400.,500.,600.,700.,800.,900.,1e3,1200.,
	    1500.,2e3,2500.,3e3,4e3,5e3,6e3,7e3,8e3,9e3,1e4,2e4,5e4,1e5 };
    static double yexc[50] = { 0.,.0034,.0185,.012,.0181,.0349,.028,.05,
	    .0523,.0732,.0923,.123,.143,.162,.176,.195,.205,.207,.203,.195,
	    .187,.179,.171,.157,.138,.117,.102,.091,.075,.064,.057,.051,.047,
	    .043,.04,.035,.03,.024,.019,.017,.014,.0118,.0103,.0092,.0083,
	    .0075,.007,.0041,.002,.0012 };

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
    --ein;
    --e;
    qin -= 21;
    q -= 7;

    /* Function Body */
/*  CAN USE INSTEAD OF MOMENTUM X-SECT FOR DELTA CALC. */
/*    /2.83,2.84,2.84,2.84,2.84.2.84,2.72,2.57,2.22,1.93, */
/*    /1.74,1.63,1.45,1.12,0.90,0.72,0.59,0.49,0.28,.156, */
/*    /.070,.039,.022,.010/ */

    sprintf(name__, " NEON    92    ");
    *nin = 1;
    ndata = 43;
    nion = 68;
    nexc = 50;
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
	ak = sqrt(en / inpt_1.ary);
	an0 = -aa * ak * (apol * (float)4. / (float)3. * ak * ak * log(ak) + (
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

	qin[i__ * 20 + 1] = (float)0.;
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
	qin[i__ * 20 + 1] = (a * en + b) * (float)1e-16;
L370:
	q[i__ * 6 + 1] = q[i__ * 6 + 2] + q[i__ * 6 + 3] + qin[i__ * 20 + 1];
/* L900: */
    }
    if (inpt_1.efinal < ein[1]) {
	*nin = 0;
    }
    return 0;
} /* gas5_ */


int StFtpcMagboltz2::gas12_(double *q, double *qin, int *nin, 
			    double *e, double *ein, char *name__, 
			    double *virial, int *monte, int name_len)
{
    /* Initialized data */

    static double xmom[64] = { 0.,.001,.002,.003,.005,.007,.0085,.01,.015,
	    .02,.03,.04,.05,.07,.1,.12,.15,.17,.2,.25,.3,.35,.4,.5,.7,1.,1.2,
	    1.3,1.5,1.7,1.9,2.1,2.2,2.5,2.8,3.,3.3,3.6,4.,4.5,5.,5.5,6.,7.,8.,
	    10.,12.,15.,17.,20.,25.,30.,50.,75.,100.,200.,400.,600.,1e3,2e3,
	    4e3,1e4,2e4,1e5 };
    static double ymom[64] = { 600.,540.,380.,307.,237.,200.,182.,170.,
	    138.,120.,97.,85.,76.,63.,50.,44.,36.,32.,27.,20.,15.,12.4,10.5,
	    8.,5.7,4.2,3.7,3.5,3.3,3.2,3.3,3.5,3.6,4.,4.4,4.7,5.2,5.8,6.,5.5,
	    5.1,5.,5.2,6.1,7.3,8.8,10.,11.,11.,10.7,10.,9.1,6.2,4.,3.,.697,
	    .288,.158,.09,.042,.02,.0077,.0038,.001 };
    static double xvib1[39] = { .083,.0844,.0862,.0932,.1035,.121,.138,
	    .1726,.2,.25,.35,.5,.7,.9,1.1,1.4,1.6,1.9,2.6,3.1,3.5,3.7,3.9,4.1,
	    4.3,4.5,4.7,5.1,5.6,6.1,6.5,7.5,8.5,10.5,20.,50.,100.,1e3,1e5 };
    static double yvib1[39] = { 0.,.85,1.16,1.85,2.3,2.6,2.68,2.4,2.,1.55,
	    1.13,.86,.68,.57,.51,.45,.42,.44,.7,1.32,2.64,3.15,3.5,3.56,3.52,
	    3.35,2.74,1.85,.8,.61,.55,.48,.45,.2,.05,.01,.001,1e-4,0. };
    static double xvib2[29] = { .167,.172,.18,.2,.25,.5,1.,1.5,2.,2.2,2.5,
	    2.9,3.4,3.6,3.9,4.05,4.2,4.4,4.6,5.1,5.5,5.7,6.5,8.5,10.5,20.,
	    100.,1e3,1e5 };
    static double yvib2[29] = { 0.,.3,.33,.35,.325,.117,.05,.04,.06,.08,
	    .2,.57,2.53,3.1,3.5,3.52,3.45,3.16,2.3,1.58,.71,.6,.37,.25,.21,
	    .02,.001,1e-4,0. };
    static double xvib3[13] = { .252,1.5,1.95,2.5,3.5,4.06,4.6,5.1,5.56,
	    6.,100.,1e3,1e5 };
    static double yvib3[13] = { 0.,0.,0.,0.,.63,1.06,.61,.29,.066,.001,
	    1e-4,1e-5,0. };
    static double xvib4[25] = { .291,.3,.31,.32,.33,.35,.38,.4,.45,.5,.6,
	    .8,1.,1.5,2.,3.,4.5,6.,8.,10.,25.,30.,100.,1e3,1e5 };
    static double yvib4[25] = { 0.,.76,1.36,1.58,1.67,1.73,1.82,1.83,1.78,
	    1.67,1.46,1.17,1.,.76,.64,.49,.44,.41,.48,.26,.135,.1,.001,1e-4,
	    0. };
    static double xvib5[13] = { .339,1.5,2.3,2.9,3.4,4.06,4.6,5.1,5.66,6.,
	    100.,1e3,1e5 };
    static double yvib5[13] = { 0.,0.,.125,.36,.81,1.3,.61,.278,.01,.001,
	    1e-4,0.,0. };
    static double xvib6[13] = { .422,1.5,1.95,2.5,3.4,4.06,4.6,5.1,5.56,
	    6.,100.,1e3,1e5 };
    static double yvib6[13] = { 0.,0.,0.,0.,.21,.444,.18,0.,0.,0.,0.,0.,
	    0. };
    static double xvib7[13] = { .505,1.5,1.95,2.5,3.4,4.06,4.6,5.1,5.56,
	    6.,100.,1e3,1e5 };
    static double yvib7[13] = { 0.,0.,0.,0.,.31,.59,.28,0.,0.,0.,0.,0.,0. 
	    };
    static double xexc1[8] = { 2.5,3.4,4.1,4.6,5.,100.,1e3,1e5 };
    static double yexc1[8] = { 0.,.35,.49,.32,0.,0.,0.,0. };
    static double xatt[29] = { 3.85,4.,4.2,4.4,4.6,4.8,5.,5.2,5.4,6.3,6.6,
	    6.9,7.2,7.4,7.6,7.8,8.,8.2,8.4,8.6,8.8,9.,9.2,9.5,9.8,10.,100.,
	    1e3,1e5 };
    static double yatt[29] = { 0.,5e-4,.0014,.0014,.001,6e-4,3e-4,1e-4,
	    1e-4,1e-4,1e-4,2e-4,8e-4,.0018,.0027,.0036,.0042,.0041,.0034,.002,
	    .0012,4e-4,3e-4,2e-4,1e-4,1e-4,1e-5,1e-6,0. };
    static double xexc2[7] = { 7.,8.,8.5,11.,100.,1e3,1e5 };
    static double yexc2[7] = { 0.,.5,.5,0.,0.,0.,0. };
    static double xexc3[23] = { 10.5,12.,13.,14.,15.,17.,20.,25.,30.,40.,
	    60.,80.,100.,150.,200.,400.,600.,1e3,2e3,4e3,1e4,2e4,1e5 };
    static double yexc3[23] = { 0.,.76,.83,.9,.97,1.14,1.4,1.95,2.54,3.6,
	    4.8,5.6,6.3,6.6,6.,3.2,2.15,1.35,.75,.4,.18,.09,.022 };
    static double xion[50] = { 13.3,14.5,15.,16.,18.,19.,20.,21.,22.,24.,
	    26.,28.,30.,32.,34.,36.,38.,40.,45.,50.,55.,60.,65.,70.,80.,90.,
	    100.,110.,130.,140.,160.,180.,200.,250.,300.,400.,500.,600.,700.,
	    800.,900.,1e3,1500.,2e3,4e3,7e3,1e4,2e4,4e4,1e5 };
    static double yion[50] = { 0.,.06,.104,.188,.359,.46,.532,.622,.729,
	    .95,1.21,1.45,1.63,1.78,1.92,2.04,2.15,2.28,2.56,2.79,2.98,3.16,
	    3.31,3.43,3.61,3.73,3.8,3.83,3.83,3.8,3.71,3.62,3.52,3.26,3.03,
	    2.61,2.31,2.06,1.86,1.69,1.58,1.51,1.15,.9,.5,.29,.21,.11,.063,
	    .029 };

    /* System generated locals */
    int i__1, i__2;

    /* Local variables */
    static int nion, nmom, natt, nexc1, nvib1, nvib2, nvib3, nvib4, nvib5,
	     nvib6, nvib7, nexc2, nexc3, i__, j;
    static double a, b, en;

    /* Parameter adjustments */
    --ein;
    --e;
    qin -= 21;
    q -= 7;

    /* Function Body */
/* ----------------------------------------------------- */
/* NAKAMURAS ORIGINAL LOW ENERGY X-SECTION IS MODIFIED */
/* BELOW 0.17 EV TO BETTER FIT ELFORDS DATA: */
/*     DATA YMOM/600.,578.,407.,328.,254.,214.,195.,182.,148.,128., */
/*    /104.,91.0,81.0,67.0,53.5,46.0,37.0,32.0,27.0,20.0, */
/*   TO USE NAKAMURAS X-SECTION DECOMMENT THE ABOVE TWO LINES */
/*   AND COMMENT THE TWO LINES BELOW. */
/* ------------------------------------------------------- */
    sprintf(name__, "C02 NAKAMURA   ");
    *nin = 10;
    nmom = 64;
    nvib1 = 39;
    nvib2 = 29;
    nvib3 = 13;
    nvib4 = 25;
    nvib5 = 13;
    nvib6 = 13;
    nvib7 = 13;
    nexc1 = 8;
    natt = 29;
    nexc2 = 7;
    nexc3 = 23;
    nion = 50;
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
	q[i__ * 6 + 2] = (a * en + b) * 1e-16;

	qin[i__ * 20 + 1] = (float)0.;
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
	qin[i__ * 20 + 1] = (a * en + b) * 1e-16;

L260:
	qin[i__ * 20 + 2] = (float)0.;
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
	qin[i__ * 20 + 2] = (a * en + b) * 1e-16;

L360:
	qin[i__ * 20 + 3] = (float)0.;
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
	qin[i__ * 20 + 3] = (a * en + b) * 1e-16;

L460:
	qin[i__ * 20 + 4] = (float)0.;
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
	qin[i__ * 20 + 4] = (a * en + b) * 1e-16;

L560:
	qin[i__ * 20 + 5] = (float)0.;
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
	qin[i__ * 20 + 5] = (a * en + b) * 1e-16;

L660:
	qin[i__ * 20 + 6] = (float)0.;
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
	qin[i__ * 20 + 6] = (a * en + b) * 1e-16;

L760:
	qin[i__ * 20 + 7] = (float)0.;
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
	qin[i__ * 20 + 7] = (a * en + b) * 1e-16;

L860:
	qin[i__ * 20 + 8] = (float)0.;
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
	qin[i__ * 20 + 8] = (a * en + b) * 1e-16;

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
	q[i__ * 6 + 4] = (a * en + b) * 1e-16;

L1060:
	qin[i__ * 20 + 9] = (float)0.;
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
	qin[i__ * 20 + 9] = (a * en + b) * 1e-16;

L1160:
	qin[i__ * 20 + 10] = (float)0.;
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
	qin[i__ * 20 + 10] = (a * en + b) * 1e-16;

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
	q[i__ * 6 + 3] = (a * en + b) * 1e-16;

L1360:
	q[i__ * 6 + 5] = (float)0.;
	q[i__ * 6 + 6] = (float)0.;
	q[i__ * 6 + 1] = q[i__ * 6 + 2] + q[i__ * 6 + 3] + q[i__ * 6 + 4] + 
		qin[i__ * 20 + 1] + qin[i__ * 20 + 2] + qin[i__ * 20 + 3] + 
		qin[i__ * 20 + 4] + qin[i__ * 20 + 5] + qin[i__ * 20 + 6] + 
		qin[i__ * 20 + 7] + qin[i__ * 20 + 8] + qin[i__ * 20 + 9] + 
		qin[i__ * 20 + 10];
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
} /* gas12_ */

