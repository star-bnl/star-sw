//*-- Author :    Valery Fine(fine@bnl.gov)   25/09/99  
// tmxm.F -- translated by f2c (version 19970219).
//
// The test ("micky") of the set of methods to work with the plain 
// matrix / vector "derived" from  
// http://wwwinfo.cern.ch/asdoc/shortwrupsdir/f110/top.html 
//
// $Id: TMxF110.cxx,v 1.6 2006/12/08 17:51:14 fine Exp $
// $Log: TMxF110.cxx,v $
// Revision 1.6  2006/12/08 17:51:14  fine
// prepare the test are to move to ROOT CVS
//
// Revision 1.5  2003/09/02 17:59:24  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.4  2000/03/26 03:16:05  fine
// Adjusted to ROOT 2.24
//
// Revision 1.3  1999/09/28 19:54:57  fine
// RMath has been renamed to StCL
//
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
// test system for StCL class has been introduced
//
//

#include "StMicky.h"
#if ROOT_VERSION_CODE < ROOT_VERSION(5,13,0)
#include "TCL.h"
#else
#include "TCernLib.h"
#endif
#include "Stiostream.h"

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

#ifndef tinf
#define tinf ((int *)&equiv_30)
#endif
   
   static struct {
	char e_1[4];
	int e_2;
	} equiv_31 = { {'X', 'M', 'A', 'D'}, 0 };
   
   
#ifndef tinfa
#define tinfa (*(int *)&equiv_31)
#endif 

      static struct {
	char e_1[4];
	int e_2;
	} equiv_32 = { {'X', 'M', 'U', 'B'}, 0 };
   
#ifndef tinfu
#define tinfu (*(int *)&equiv_32)
#endif

   static struct {
	char e_1[4];
	int e_2;
	} equiv_33 = { {'M', 'P', 'Y', '1'}, 0 };
   
#ifndef tinfy1
#define tinfy1 (*(int *)&equiv_33)
#endif

   static struct {
	char e_1[4];
	int e_2;
	} equiv_34 = { {'M', 'A', 'D', '1'}, 0 };
   
#ifndef tinfa1
#define tinfa1 (*(int *)&equiv_34)
#endif

   static struct {
	char e_1[4];
	int e_2;
	} equiv_35 = { {'M', 'U', 'B', '1'}, 0 };

#ifndef tinfu1
#define tinfu1 (*(int *)&equiv_35)
#endif

    static struct {
	char e_1[4];
	int e_2;
	} equiv_36 = { {'M', 'P', 'Y', '2'}, 0 };

#ifndef tinfy2
#define tinfy2 (*(int *)&equiv_36)
#endif


    static struct {
	char e_1[4];
	int e_2;
	} equiv_37 = { {'M', 'A', 'D', '2'}, 0 };

#ifndef tinfa2
#define tinfa2 (*(int *)&equiv_37)
#endif

    static struct {
	char e_1[4];
	int e_2;
	} equiv_38 = { {'M', 'U', 'B', '2'}, 0 };

#ifndef tinfu2
#define tinfu2 (*(int *)&equiv_38)
#endif
   
   static struct {
	char e_1[4];
	int e_2;
	} equiv_39 = { {'M', 'P', 'Y', '3'}, 0 };

#ifndef tinfy3
#define tinfy3 (*(int *)&equiv_39)
#endif

    static struct {
	char e_1[4];
	int e_2;
	} equiv_40 = { {'M', 'A', 'D', '3'}, 0 };

#ifndef tinfa3
#define tinfa3 (*(int *)&equiv_40)
#endif

    static struct {
	char e_1[4];
	int e_2;
	} equiv_41 = { {'M', 'U', 'B', '3'}, 0 };

#ifndef tinfu3
#define tinfu3 (*(int *)&equiv_41)
#endif

    static int j;
    static int ntimes;

    /* Local variables */
#ifndef  _LOCALMICKYVARS_
#define  _LOCALMICKYVARS_
    
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

#endif

//_______________________________________________________
    param_1.zerlev = param_1.zerov[1];
    TCL::vzero(zer,12);
    TCL::mxtrp(bc, bct, 2, 4);
    TCL::mxtrp(ac, act, 3, 2);
/*        TEST FOR MXMPY-1-2-3 */
    Newguy("MXMPY-1-2-3.", "TMXM    ");
    TCL::mxmpy(ac, bc, _BLNK__1.a, 3, 2, 4);
    Mverif(1, _BLNK__1.a, c1c, 12);

    TCL::mxmpy1(ac, bct, _BLNK__1.a, 3, 2, 4);
    Mverif(11, _BLNK__1.a, c1c, 12);

    TCL::mxmpy2(act, bc, _BLNK__1.a, 3, 2, 4);
    Mverif(21, _BLNK__1.a, c1c, 12);

    TCL::mxmpy3(act, bct, _BLNK__1.a, 3, 2, 4);
    Mverif(31, _BLNK__1.a, c1c, 12);

    TCL::mxmpy(ac, bc, _BLNK__1.a, 3, 0, 4);
    Mverif(10, _BLNK__1.a, zer, 12);

    TCL::mxmpy1(ac, bct, _BLNK__1.a, 3, 0, 4);
    Mverif(110, _BLNK__1.a, zer, 12);

    TCL::mxmpy2(act, bc, _BLNK__1.a, 3, 0, 4);
    Mverif(210, _BLNK__1.a, zer, 12);

    TCL::mxmpy3(act, bct, _BLNK__1.a, 3, 0, 4);
    Mverif(310, _BLNK__1.a, zer, 12);

    TCL::mxmpy(ac, ac, _BLNK__1.a, 3, 2, 3);
    Mverif(12, _BLNK__1.a, c2c, 9);
/* --      TIMING */
    if (param_1.itimes == 0) {
	goto L100;
    }
    ntimes = param_1.itimes * tinf[0];
    tinf[0] = ntimes;
    Timed(&param_1.timerd);
    for (j = 1; j <= ntimes; ++j) 
	TCL::mxmpy(ac, bc, _BLNK__1.a, 3, 2, 4);
    
    Timing(tinf);
    tinf[1] = tinfy1;
    Timed(&param_1.timerd);
    for (j = 1; j <= ntimes; ++j) 
	TCL::mxmpy1(ac, bct, _BLNK__1.a, 3, 2, 4);    

    Timing(tinf);
    tinf[1] = tinfy2;
    Timed(&param_1.timerd);
    for (j = 1; j <= ntimes; ++j) 
	TCL::mxmpy2(act, bc, _BLNK__1.a, 3, 2, 4);
    
    Timing(tinf);
    tinf[1] = tinfy3;
    Timed(&param_1.timerd);
    for (j = 1; j <= ntimes; ++j) 
	TCL::mxmpy3(act, bct, _BLNK__1.a, 3, 2, 4);
    
    Timing(tinf);
/*        TEST FOR MXMAD-12-3- */
L100:
    Newguy("MXMAD-1-2-3.", "TMXM    ");
    TCL::ucopy(c1c, _BLNK__1.a, 12);
    TCL::vadd(_BLNK__1.a, d__, aplus, 12);
    TCL::vsub(_BLNK__1.a, d__, amin, 12);
    TCL::mxmpy(ac, ac, _BLNK__1.a, 3, 2, 3);
    TCL::vadd(_BLNK__1.a, d__, aplus1, 9);
    TCL::vsub(_BLNK__1.a, d__, amin2, 9);

    TCL::ucopy(d__, _BLNK__1.a, 12);    
    TCL::mxmad(ac, bc, _BLNK__1.a, 3, 2, 4);
    Mverif(1, _BLNK__1.a, aplus, 12);

    TCL::ucopy(d__, _BLNK__1.a, 12);
    TCL::mxmad1(ac, bct, _BLNK__1.a, 3, 2, 4);
    Mverif(11, _BLNK__1.a, aplus, 12);

    TCL::ucopy(d__, _BLNK__1.a, 12);
    TCL::mxmad2(act, bc, _BLNK__1.a, 3, 2, 4);
    Mverif(21, _BLNK__1.a, aplus, 12);

    TCL::ucopy(d__, _BLNK__1.a, 12);
    TCL::mxmad3(act, bct, _BLNK__1.a, 3, 2, 4);
    Mverif(31, _BLNK__1.a, aplus, 12);

    TCL::ucopy(d__, _BLNK__1.a, 12);
    TCL::mxmad(ac, bc, _BLNK__1.a, 3, 0, 4);
    Mverif(10, _BLNK__1.a, d__, 12);

    TCL::mxmad1(ac, bct, _BLNK__1.a, 3, 0, 4);
    Mverif(110, _BLNK__1.a, d__, 12);

    TCL::mxmad2(act, bc, _BLNK__1.a, 3, 0, 4);
    Mverif(210, _BLNK__1.a, d__, 12);

    TCL::mxmad3(act, bct, _BLNK__1.a, 3, 0, 4);
    Mverif(310, _BLNK__1.a, d__, 12);

    TCL::ucopy(d__, _BLNK__1.a, 9);
    TCL::mxmad(ac, ac, _BLNK__1.a, 3, 2, 3);
    Mverif(12, _BLNK__1.a, aplus1, 9);

    TCL::ucopy(d__, _BLNK__1.a, 9);
/* --      TIMING */
    if (param_1.itimes == 0) {
	goto L200;
    }
    tinf[1] = tinfa;
    Timed(&param_1.timerd);
    for (j = 1; j <= ntimes; ++j) {
	TCL::ucopy(d__, _BLNK__1.a, 12);
	TCL::mxmad(ac, bc, _BLNK__1.a, 3, 2, 4);
    }

    Timing(tinf);
    tinf[1] = tinfa1;
    Timed(&param_1.timerd);

    for (j = 1; j <= ntimes; ++j) {
	TCL::ucopy(d__, _BLNK__1.a, 12);
	TCL::mxmad1(ac, bct, _BLNK__1.a, 3, 2, 4);
    }

    Timing(tinf);
    tinf[1] = tinfa2;
    Timed(&param_1.timerd);

    for (j = 1; j <= ntimes; ++j) {
	TCL::ucopy(d__, _BLNK__1.a, 12);
	TCL::mxmad2(act, bc, _BLNK__1.a, 3, 2, 4);
    }

    Timing(tinf);
    tinf[1] = tinfa3;
    Timed(&param_1.timerd);

    for (j = 1; j <= ntimes; ++j) {
	TCL::ucopy(d__, _BLNK__1.a, 12);
	TCL::mxmad3(act, bct, _BLNK__1.a, 3, 2, 4);
    }

    Timing(tinf);

/*        TEST FOR MXMUB-1-2-3 */
L200:
    Newguy("MXMUB-1-2-3.", "TMXM    ");
    TCL::vcopyn(d__, dmin__, 12);
    TCL::ucopy(d__, _BLNK__1.a, 12);
    TCL::mxmub(ac, bc, _BLNK__1.a, 3, 2, 4);
    Mverif(1, _BLNK__1.a, amin, 12);

    TCL::ucopy(d__, _BLNK__1.a, 12);
    TCL::mxmub1(ac, bct, _BLNK__1.a, 3, 2, 4);
    Mverif(11, _BLNK__1.a, amin, 12);

    TCL::ucopy(d__, _BLNK__1.a, 12);
    TCL::mxmub2(act, bc, _BLNK__1.a, 3, 2, 4);
    Mverif(21, _BLNK__1.a, amin, 12);

    TCL::ucopy(d__, _BLNK__1.a, 12);
    TCL::mxmub3(act, bct, _BLNK__1.a, 3, 2, 4);
    Mverif(31, _BLNK__1.a, amin, 12);

    TCL::ucopy(d__, _BLNK__1.a, 12);
    TCL::mxmub(ac, bc, _BLNK__1.a, 3, 0, 4);
    Mverif(10, _BLNK__1.a, dmin__, 12);

    TCL::ucopy(d__, _BLNK__1.a, 12);
    TCL::mxmub1(ac, bct, _BLNK__1.a, 3, 0, 4);
    Mverif(110, _BLNK__1.a, dmin__, 12);

    TCL::ucopy(d__, _BLNK__1.a, 12);
    TCL::mxmub2(act, bc, _BLNK__1.a, 3, 0, 4);
    Mverif(210, _BLNK__1.a, dmin__, 12);

    TCL::ucopy(d__, _BLNK__1.a, 12);
    TCL::mxmub3(act, bct, _BLNK__1.a, 3, 0, 4);
    Mverif(310, _BLNK__1.a, dmin__, 12);

    TCL::ucopy(d__, _BLNK__1.a, 9);
    TCL::mxmub(ac, ac, _BLNK__1.a, 3, 2, 3);
    Mverif(12, _BLNK__1.a, amin2, 9);

    if (param_1.itimes == 0) goto L300;

    tinf[1] = tinfu;
    Timed(&param_1.timerd);

    for (j = 1; j <= ntimes; ++j) {
	TCL::ucopy(d__, _BLNK__1.a, 12);
	TCL::mxmub(ac, bc, _BLNK__1.a, 3, 2, 4);
    }

    Timing(tinf);
    tinf[1] = tinfu1;
    Timed(&param_1.timerd);

    for (j = 1; j <= ntimes; ++j) {
	TCL::ucopy(d__, _BLNK__1.a, 12);
	TCL::mxmub1(ac, bct, _BLNK__1.a, 3, 2, 4);
    }

    Timing(tinf);
    tinf[1] = tinfu2;
    Timed(&param_1.timerd);

    for (j = 1; j <= ntimes; ++j) {
	TCL::ucopy(d__, _BLNK__1.a, 12);
	TCL::mxmub2(act, bc, _BLNK__1.a, 3, 2, 4);
    }

    Timing(tinf);
    tinf[1] = tinfu3;
    Timed(&param_1.timerd);

    for (j = 1; j <= ntimes; ++j) {
	TCL::ucopy(d__, _BLNK__1.a, 12);
	TCL::mxmub3(act, bct, _BLNK__1.a, 3, 2, 4);
    }

    Timing(tinf);

/* --                TEST FOR MXMLRT - MXMLTR */
L300:
    Newguy("MXMLRT - MXMLTR.", "TMXM    ");
    TCL::mxmpy(ac, c2c, _BLNK__1.a, 2, 3, 3);
    TCL::mxmpy1(_BLNK__1.a, ac, _BLNK__1.b, 2, 3, 2);
    TCL::mxmpy1(c2c, ac, &_BLNK__1.a[100], 3, 3, 2);
    TCL::mxmpy(ac, &_BLNK__1.a[100], &_BLNK__1.b[100], 2, 3, 2);
    Mverif(0, _BLNK__1.b, &_BLNK__1.b[100], 4);

    TCL::mxmlrt(ac, c2c, _BLNK__1.a, 2, 3);
    Mverif(1, _BLNK__1.a, _BLNK__1.b, 4);

    TCL::mxmpy2(c1c, c2c, _BLNK__1.a, 4, 3, 3);
    TCL::mxmpy(_BLNK__1.a, c1c, _BLNK__1.b, 4, 3, 4);
    TCL::mxmpy(c2c, c1c, &_BLNK__1.a[100], 3, 3, 4);
    TCL::mxmpy2(c1c, &_BLNK__1.a[100], &_BLNK__1.b[100], 4, 3, 4);
    Mverif(10, _BLNK__1.b, &_BLNK__1.b[100], 16);

    TCL::mxmltr(c1c, c2c, _BLNK__1.a, 4, 3);
    Mverif(11, _BLNK__1.a, _BLNK__1.b, 16);

} /* tmxm_ */
#undef tinf
#undef tinfa
#undef tinfu
#undef tinfy1
#undef tinfa1
#undef tinfu1
#undef tinfy2
#undef tinfa2
#undef tinfu2
#undef tinfy3
#undef tinfa3
#undef tinfu3

