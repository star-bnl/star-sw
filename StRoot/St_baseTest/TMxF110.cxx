//*-- Author :    Valery Fine(fine@bnl.gov)   25/09/99  
// tmxm.F -- translated by f2c (version 19970219).
//
// The test ("micky") of the set of methods to work with the plain 
// matrix / vector "derived" from  
// http://wwwinfo.cern.ch/asdoc/shortwrupsdir/f110/top.html 
//
// $Id: TMxF110.cxx,v 1.2 1999/09/26 23:37:22 fine Exp $
// $Log: TMxF110.cxx,v $
// Revision 1.2  1999/09/26 23:37:22  fine
// test F112 package has been introduced
//
// Revision 1.1  1999/09/26 19:35:13  fine
// Micky test has been rearranged
//
// Revision 1.1  1999/09/24 17:15:37  fine
// type fixed
//
// Revision 1.1  1999/09/23 18:33:11  fine
// test system for RMath class has been introduced
//
//

#include "StMicky.h"
#include "RMath.h"
#include <iostream.h>

//____________________________________________________________________________________
void StMicky::Tmxm()
{
   // Load "base" library

    /* Initialized data */

    float ac[6]   = { 0.,1.,2.,3.,4.,5. };
    float bc[8]   = { 10.,20.,30.,40.,50.,60.,70.,80. };
    float c1c[12] = { 50.,60.,70.,80.,170.,220.,270.,320.,290., 380.,470.,560. };
    float c2c[9]  = { 3.,4.,5.,9.,14.,19.,15.,24.,33. };
    float d__[12] = { 2.,4.,6.,8.,102.,104.,106.,108.,202.,204.,206.,208. };
    struct {
	int e_1;
	char e_2[4];
	int e_3;
	char e_4[4];
	} equiv_30 = { 1000, {'X', 'M', 'P', 'Y'}, 0, {'3', '2', '4', ' '} };

#define tinf ((int *)&equiv_30)

    static struct {
	char e_1[4];
	int e_2;
	} equiv_31 = { {'X', 'M', 'A', 'D'}, 0 };

#define tinfa (*(int *)&equiv_31)

    static struct {
	char e_1[4];
	int e_2;
	} equiv_32 = { {'X', 'M', 'U', 'B'}, 0 };

#define tinfu (*(int *)&equiv_32)

    static struct {
	char e_1[4];
	int e_2;
	} equiv_33 = { {'M', 'P', 'Y', '1'}, 0 };

#define tinfy1 (*(int *)&equiv_33)

    static struct {
	char e_1[4];
	int e_2;
	} equiv_34 = { {'M', 'A', 'D', '1'}, 0 };

#define tinfa1 (*(int *)&equiv_34)

    static struct {
	char e_1[4];
	int e_2;
	} equiv_35 = { {'M', 'U', 'B', '1'}, 0 };

#define tinfu1 (*(int *)&equiv_35)

    static struct {
	char e_1[4];
	int e_2;
	} equiv_36 = { {'M', 'P', 'Y', '2'}, 0 };

#define tinfy2 (*(int *)&equiv_36)

    static struct {
	char e_1[4];
	int e_2;
	} equiv_37 = { {'M', 'A', 'D', '2'}, 0 };

#define tinfa2 (*(int *)&equiv_37)

    static struct {
	char e_1[4];
	int e_2;
	} equiv_38 = { {'M', 'U', 'B', '2'}, 0 };

#define tinfu2 (*(int *)&equiv_38)

    static struct {
	char e_1[4];
	int e_2;
	} equiv_39 = { {'M', 'P', 'Y', '3'}, 0 };

#define tinfy3 (*(int *)&equiv_39)

    static struct {
	char e_1[4];
	int e_2;
	} equiv_40 = { {'M', 'A', 'D', '3'}, 0 };

#define tinfa3 (*(int *)&equiv_40)

    static struct {
	char e_1[4];
	int e_2;
	} equiv_41 = { {'M', 'U', 'B', '3'}, 0 };

#define tinfu3 (*(int *)&equiv_41)

    static int j;
    static int ntimes;

    /* Local variables */
#define amin   ((float *)&_BLNK__1 + 1399)
#define dmin__ ((float *)&_BLNK__1 + 1699)
#define amin2  ((float *)&_BLNK__1 + 1599)
#define aplus  ((float *)&_BLNK__1 + 1299)
	    
#define aplus1 ((float *)&_BLNK__1 + 1499)
#define ia     ((int *)&_BLNK__1)
#define ib     ((int *)&_BLNK__1 + 1000)

#define act    ((float *)&_BLNK__1 + 1099)
#define bct    ((float *)&_BLNK__1 + 1199)
#define zer    ((float *)&_BLNK__1 + 1000)

//_______________________________________________________
    param_1.zerlev = param_1.zerov[1];
    RMath::uzero(zer, 1, 12);
    RMath::mxtrp(bc, bct, 2, 4);
    RMath::mxtrp(ac, act, 3, 2);
/*        TEST FOR MXMPY-1-2-3 */
    StMicky::Newguy("MXMPY-1-2-3.", "TMXM    ");
    RMath::mxmpy(ac, bc, _BLNK__1.a, 3, 2, 4);
    StMicky::Mverif(1, _BLNK__1.a, c1c, 12);

    RMath::mxmpy1(ac, bct, _BLNK__1.a, 3, 2, 4);
    StMicky::Mverif(11, _BLNK__1.a, c1c, 12);

    RMath::mxmpy2(act, bc, _BLNK__1.a, 3, 2, 4);
    StMicky::Mverif(21, _BLNK__1.a, c1c, 12);

    RMath::mxmpy3(act, bct, _BLNK__1.a, 3, 2, 4);
    StMicky::Mverif(31, _BLNK__1.a, c1c, 12);

    RMath::mxmpy(ac, bc, _BLNK__1.a, 3, 0, 4);
    StMicky::Mverif(10, _BLNK__1.a, zer, 12);

    RMath::mxmpy1(ac, bct, _BLNK__1.a, 3, 0, 4);
    StMicky::Mverif(110, _BLNK__1.a, zer, 12);

    RMath::mxmpy2(act, bc, _BLNK__1.a, 3, 0, 4);
    StMicky::Mverif(210, _BLNK__1.a, zer, 12);

    RMath::mxmpy3(act, bct, _BLNK__1.a, 3, 0, 4);
    StMicky::Mverif(310, _BLNK__1.a, zer, 12);

    RMath::mxmpy(ac, ac, _BLNK__1.a, 3, 2, 3);
    StMicky::Mverif(12, _BLNK__1.a, c2c, 9);
/* --      TIMING */
    if (param_1.itimes == 0) {
	goto L100;
    }
    ntimes = param_1.itimes * tinf[0];
    tinf[0] = ntimes;
    StMicky::Timed(&param_1.timerd);
    for (j = 1; j <= ntimes; ++j) 
	RMath::mxmpy(ac, bc, _BLNK__1.a, 3, 2, 4);
    
    StMicky::Timing(tinf);
    tinf[1] = tinfy1;
    StMicky::Timed(&param_1.timerd);
    for (j = 1; j <= ntimes; ++j) 
	RMath::mxmpy1(ac, bct, _BLNK__1.a, 3, 2, 4);    

    StMicky::Timing(tinf);
    tinf[1] = tinfy2;
    StMicky::Timed(&param_1.timerd);
    for (j = 1; j <= ntimes; ++j) 
	RMath::mxmpy2(act, bc, _BLNK__1.a, 3, 2, 4);
    
    StMicky::Timing(tinf);
    tinf[1] = tinfy3;
    StMicky::Timed(&param_1.timerd);
    for (j = 1; j <= ntimes; ++j) 
	RMath::mxmpy3(act, bct, _BLNK__1.a, 3, 2, 4);
    
    StMicky::Timing(tinf);
/*        TEST FOR MXMAD-12-3- */
L100:
    StMicky::Newguy("MXMAD-1-2-3.", "TMXM    ");
    RMath::ucopy(c1c, _BLNK__1.a, 12);
    RMath::vadd(_BLNK__1.a, d__, aplus, 12);
    RMath::vsub(_BLNK__1.a, d__, amin, 12);
    RMath::mxmpy(ac, ac, _BLNK__1.a, 3, 2, 3);
    RMath::vadd(_BLNK__1.a, d__, aplus1, 9);
    RMath::vsub(_BLNK__1.a, d__, amin2, 9);

    RMath::ucopy(d__, _BLNK__1.a, 12);    
    RMath::mxmad(ac, bc, _BLNK__1.a, 3, 2, 4);
    StMicky::Mverif(1, _BLNK__1.a, aplus, 12);

    RMath::ucopy(d__, _BLNK__1.a, 12);
    RMath::mxmad1(ac, bct, _BLNK__1.a, 3, 2, 4);
    StMicky::Mverif(11, _BLNK__1.a, aplus, 12);

    RMath::ucopy(d__, _BLNK__1.a, 12);
    RMath::mxmad2(act, bc, _BLNK__1.a, 3, 2, 4);
    StMicky::Mverif(21, _BLNK__1.a, aplus, 12);

    RMath::ucopy(d__, _BLNK__1.a, 12);
    RMath::mxmad3(act, bct, _BLNK__1.a, 3, 2, 4);
    StMicky::Mverif(31, _BLNK__1.a, aplus, 12);

    RMath::ucopy(d__, _BLNK__1.a, 12);
    RMath::mxmad(ac, bc, _BLNK__1.a, 3, 0, 4);
    StMicky::Mverif(10, _BLNK__1.a, d__, 12);

    RMath::mxmad1(ac, bct, _BLNK__1.a, 3, 0, 4);
    StMicky::Mverif(110, _BLNK__1.a, d__, 12);

    RMath::mxmad2(act, bc, _BLNK__1.a, 3, 0, 4);
    StMicky::Mverif(210, _BLNK__1.a, d__, 12);

    RMath::mxmad3(act, bct, _BLNK__1.a, 3, 0, 4);
    StMicky::Mverif(310, _BLNK__1.a, d__, 12);

    RMath::ucopy(d__, _BLNK__1.a, 9);
    RMath::mxmad(ac, ac, _BLNK__1.a, 3, 2, 3);
    StMicky::Mverif(12, _BLNK__1.a, aplus1, 9);

    RMath::ucopy(d__, _BLNK__1.a, 9);
/* --      TIMING */
    if (param_1.itimes == 0) {
	goto L200;
    }
    tinf[1] = tinfa;
    StMicky::Timed(&param_1.timerd);
    for (j = 1; j <= ntimes; ++j) {
	RMath::ucopy(d__, _BLNK__1.a, 12);
	RMath::mxmad(ac, bc, _BLNK__1.a, 3, 2, 4);
    }

    StMicky::Timing(tinf);
    tinf[1] = tinfa1;
    StMicky::Timed(&param_1.timerd);

    for (j = 1; j <= ntimes; ++j) {
	RMath::ucopy(d__, _BLNK__1.a, 12);
	RMath::mxmad1(ac, bct, _BLNK__1.a, 3, 2, 4);
    }

    StMicky::Timing(tinf);
    tinf[1] = tinfa2;
    StMicky::Timed(&param_1.timerd);

    for (j = 1; j <= ntimes; ++j) {
	RMath::ucopy(d__, _BLNK__1.a, 12);
	RMath::mxmad2(act, bc, _BLNK__1.a, 3, 2, 4);
    }

    StMicky::Timing(tinf);
    tinf[1] = tinfa3;
    StMicky::Timed(&param_1.timerd);

    for (j = 1; j <= ntimes; ++j) {
	RMath::ucopy(d__, _BLNK__1.a, 12);
	RMath::mxmad3(act, bct, _BLNK__1.a, 3, 2, 4);
    }

    StMicky::Timing(tinf);

/*        TEST FOR MXMUB-1-2-3 */
L200:
    StMicky::Newguy("MXMUB-1-2-3.", "TMXM    ");
    RMath::vcopyn(d__, dmin__, 12);
    RMath::ucopy(d__, _BLNK__1.a, 12);
    RMath::mxmub(ac, bc, _BLNK__1.a, 3, 2, 4);
    StMicky::Mverif(1, _BLNK__1.a, amin, 12);

    RMath::ucopy(d__, _BLNK__1.a, 12);
    RMath::mxmub1(ac, bct, _BLNK__1.a, 3, 2, 4);
    StMicky::Mverif(11, _BLNK__1.a, amin, 12);

    RMath::ucopy(d__, _BLNK__1.a, 12);
    RMath::mxmub2(act, bc, _BLNK__1.a, 3, 2, 4);
    StMicky::Mverif(21, _BLNK__1.a, amin, 12);

    RMath::ucopy(d__, _BLNK__1.a, 12);
    RMath::mxmub3(act, bct, _BLNK__1.a, 3, 2, 4);
    StMicky::Mverif(31, _BLNK__1.a, amin, 12);

    RMath::ucopy(d__, _BLNK__1.a, 12);
    RMath::mxmub(ac, bc, _BLNK__1.a, 3, 0, 4);
    StMicky::Mverif(10, _BLNK__1.a, dmin__, 12);

    RMath::ucopy(d__, _BLNK__1.a, 12);
    RMath::mxmub1(ac, bct, _BLNK__1.a, 3, 0, 4);
    StMicky::Mverif(110, _BLNK__1.a, dmin__, 12);

    RMath::ucopy(d__, _BLNK__1.a, 12);
    RMath::mxmub2(act, bc, _BLNK__1.a, 3, 0, 4);
    StMicky::Mverif(210, _BLNK__1.a, dmin__, 12);

    RMath::ucopy(d__, _BLNK__1.a, 12);
    RMath::mxmub3(act, bct, _BLNK__1.a, 3, 0, 4);
    StMicky::Mverif(310, _BLNK__1.a, dmin__, 12);

    RMath::ucopy(d__, _BLNK__1.a, 9);
    RMath::mxmub(ac, ac, _BLNK__1.a, 3, 2, 3);
    StMicky::Mverif(12, _BLNK__1.a, amin2, 9);

    if (param_1.itimes == 0) goto L300;

    tinf[1] = tinfu;
    StMicky::Timed(&param_1.timerd);

    for (j = 1; j <= ntimes; ++j) {
	RMath::ucopy(d__, _BLNK__1.a, 12);
	RMath::mxmub(ac, bc, _BLNK__1.a, 3, 2, 4);
    }

    StMicky::Timing(tinf);
    tinf[1] = tinfu1;
    StMicky::Timed(&param_1.timerd);

    for (j = 1; j <= ntimes; ++j) {
	RMath::ucopy(d__, _BLNK__1.a, 12);
	RMath::mxmub1(ac, bct, _BLNK__1.a, 3, 2, 4);
    }

    StMicky::Timing(tinf);
    tinf[1] = tinfu2;
    StMicky::Timed(&param_1.timerd);

    for (j = 1; j <= ntimes; ++j) {
	RMath::ucopy(d__, _BLNK__1.a, 12);
	RMath::mxmub2(act, bc, _BLNK__1.a, 3, 2, 4);
    }

    StMicky::Timing(tinf);
    tinf[1] = tinfu3;
    StMicky::Timed(&param_1.timerd);

    for (j = 1; j <= ntimes; ++j) {
	RMath::ucopy(d__, _BLNK__1.a, 12);
	RMath::mxmub3(act, bct, _BLNK__1.a, 3, 2, 4);
    }

    StMicky::Timing(tinf);

/* --                TEST FOR MXMLRT - MXMLTR */
L300:
    StMicky::Newguy("MXMLRT - MXMLTR.", "TMXM    ");
    RMath::mxmpy(ac, c2c, _BLNK__1.a, 2, 3, 3);
    RMath::mxmpy1(_BLNK__1.a, ac, _BLNK__1.b, 2, 3, 2);
    RMath::mxmpy1(c2c, ac, &_BLNK__1.a[100], 3, 3, 2);
    RMath::mxmpy(ac, &_BLNK__1.a[100], &_BLNK__1.b[100], 2, 3, 2);
    StMicky::Mverif(0, _BLNK__1.b, &_BLNK__1.b[100], 4);

    RMath::mxmlrt(ac, c2c, _BLNK__1.a, 2, 3);
    StMicky::Mverif(1, _BLNK__1.a, _BLNK__1.b, 4);

    RMath::mxmpy2(c1c, c2c, _BLNK__1.a, 4, 3, 3);
    RMath::mxmpy(_BLNK__1.a, c1c, _BLNK__1.b, 4, 3, 4);
    RMath::mxmpy(c2c, c1c, &_BLNK__1.a[100], 3, 3, 4);
    RMath::mxmpy2(c1c, &_BLNK__1.a[100], &_BLNK__1.b[100], 4, 3, 4);
    StMicky::Mverif(10, _BLNK__1.b, &_BLNK__1.b[100], 16);

    RMath::mxmltr(c1c, c2c, _BLNK__1.a, 4, 3);
    StMicky::Mverif(11, _BLNK__1.a, _BLNK__1.b, 16);

} /* tmxm_ */
