//*-- Author :    Valery Fine(fine@bnl.gov)   25/09/99  
//
// The test ("micky") of the set of methods to work with the plain 
// matrix / vector "derived" from  
// http://wwwinfo.cern.ch/asdoc/shortwrupsdir/f112/top.html 
//
// $Id: TTrF112.cxx,v 1.6 2006/12/08 17:51:15 fine Exp $
// $Log: TTrF112.cxx,v $
// Revision 1.6  2006/12/08 17:51:15  fine
// prepare the test are to move to ROOT CVS
//
// Revision 1.5  2003/09/02 17:59:25  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.4  2000/03/26 03:16:05  fine
// Adjusted to ROOT 2.24
//
// Revision 1.3  1999/12/07 22:33:11  fine
// Clean up to remove compilation warnings
//
// Revision 1.2  1999/09/28 19:54:57  fine
// RMath has been renamed to TCL
//
// Revision 1.1  1999/09/27 00:13:46  fine
//  test F112 has been fixed for Sun
//

#include "StMicky.h"
#if ROOT_VERSION_CODE < ROOT_VERSION(5,13,0)
#  include "TCL.h"
#else
#  include "TCernLib.h"
#endif
#include <Stiostream.h>

//____________________________________________________________________________________
int StMicky::ttrinv()
{
// ttrinv.F -- translated by f2c (version 19970219).
    /* Initialized data */

    float ac[10] = { 1.,0.,0.,2.,0.,13.,4.,0.,23.,77. };
    float cc[10] = { 0.2, -8.0, 4.0, 0.0, 0.0, 0.0, 4.2, -2.0, 0.0,1.0 };
    float qc[10] = { 5.0, 10.0, .25, 0.0, 0.0, 0.0,-1.0,  0.5, 0.0,1.0 };
    float rc[10] = { 1.45679,0.,0.,-.191358,0.,.1882716,-.0185185,0.,-.0462963,.02777778 };
    struct {
	int e_1;
	char e_2[4];
	int e_3;
	char e_4[4];
	} equiv_11 = { 1000, {'I', 'N', 'V', ' '}, 1, {'4', '*', '4', ' '} };

#define tinf ((int *)&equiv_11)

    static struct {
	char e_1[4];
	int e_2;
	} equiv_12 = { {'S', 'I', 'N', 'V'}, 0 };

#define tinfs2 (*(int *)&equiv_12)

    static struct {
	char e_1[4];
	int e_2;
	} equiv_13 = { {'4', '*', '4', ' '}, 0 };

#define tinfs4 (*(int *)&equiv_13)


    /* Local variables */
    static int j;


    Newguy("TRINV -TRSINV.", "TTRINV  ");
    param_1.zerlev = param_1.zerov[1];
    TCL::trinv(cc, _BLNK__1.a, 4);
    Mverif(11, _BLNK__1.a, qc, 10);

    TCL::ucopy(cc, _BLNK__1.a, 10);
    TCL::trinv(_BLNK__1.a, _BLNK__1.a, 4);
    Mverif(12, _BLNK__1.a, qc, 10);

    param_1.zerlev = param_1.zerov[3];
    TCL::trsinv(ac, _BLNK__1.a, 4);
    Mverif(21, _BLNK__1.a, rc, 10);

    TCL::ucopy(ac, _BLNK__1.a, 10);
    TCL::trsinv(_BLNK__1.a, _BLNK__1.a, 4);
    Mverif(22, _BLNK__1.a, rc, 10);
/* - - - - - - - - - - - - - - - - - - - -- -  - -- - - - - - - - - - - - 
*/
    if (param_1.itimes == 0)	return 0;
    
    Timed(&param_1.timerd);
    for (j = 1; j <= 1000; ++j) 
	TCL::trinv(cc, _BLNK__1.a, 4);

    Timing(tinf);
    tinf[1] = tinfs2;
    tinf[3] = tinfs4;
    Timed(&param_1.timerd);
    for (j = 1; j <= 1000; ++j) 
	TCL::trsinv(ac, _BLNK__1.a, 4);
    Timing(tinf);
    return 0;
} /* trinv */

#undef tinfs4
#undef tinfs2
#undef tinf


//____________________________________________________________________________________
/* Subroutine */ int StMicky::ttrla()
{
// ttrla.F -- translated by f2c (version 19970219).
    /* Initialized data */

  float xc[10] = { 77.,62.,69.,17., 23.,10.,52.,45.,11.,38. };
  float yc[12] = { 4.,29.,0.,69., 6.,24.,0.,74.,5.,16.,0., 42. };
  float ac[10] = { 1.,0.,0.,2.,0.,13.,4.,0.,23.,77. };
  float asc[12] = { 30.,44.,0.,69.,34.,49.,0.,74.,17.,26., 0.,42. };
  float sac[12] = { 18.,23.,19.,27.,37.,28.,0.,0.,0.,43.,64.,44. };
  float satc[12] = { 30.,34.,17.,44.,49.,26.,0.,0.,0.,69.,74.,42. };
  float atsc[12] = { 18.,27.,0.,43.,23.,37.,0.,64.,19.,28.,0.,44. };
  float atsac[6] = { 239.,331.,447.,248.,345.,257. };
  float qsqc[10] = { 1265.,1594.,2009.,0.,0.,0.,1940.,2446.,0.,2980. };
  float upckc[16] = { 1.,2.,4.,7.,2.,3.,5.,8.,4.,5.,6.,9.,7.,8.,9.,10. };
  float pckc[10] = { 1.,2.,3.,4.,5.,6.,7.,8.,9.,10. };
  struct {
	int e_1;
	char e_2[4];
	int e_3;
	char e_4[4];
	} equiv_25 = { 1000, {' ', ' ', ' ', ' '}, 1, {' ', ' ', ' ', ' '} };

#define tinf ((int *)&equiv_25)

  struct {
	char e_1[60];
	int e_2;
	} equiv_26 = { {'L', 'A', ' ', ' ', 'L', 'T', 'A', ' ', 'A', 'L', ' ',
		 ' ', 'A', 'L', 'T', ' ', 'A', 'A', 'T', ' ', 'A', 'T', 'A', 
		' ', 'A', 'S', 'A', 'T', 'A', 'T', 'S', 'A', 'Q', 'S', 'Q', 
		' ', 'S', 'A', ' ', ' ', 'A', 'S', ' ', ' ', 'S', 'A', 'T', 
		' ', 'A', 'T', 'S', ' ', 'P', 'C', 'K', ' ', 'U', 'P', 'C', 
		'K'}, 0 };

#define tinf2 ((int *)&equiv_26)

  struct {
	char e_1[60];
	int e_2;
	} equiv_27 = { {'4', '4', '3', ' ', '4', '4', '3', ' ', '3', '4', '4',
		 ' ', '3', '4', '4', ' ', '4', '*', '3', ' ', '4', '*', '3', 
		' ', '3', '4', '4', '3', '3', '4', '4', '3', '4', '4', '4', 
		'4', '4', '4', '3', ' ', '3', '4', '4', ' ', '4', '4', '3', 
		' ', '3', '4', '4', ' ', '4', '*', '4', ' ', '4', '*', '4', 
		' '}, 0 };

#define tinf4 ((int *)&equiv_27)

  float dc[10] = { 1.,2.,3.,0.,0.,0.,4.,5.,0.,6. };
  float ec[12] = { 4.,7.,3.,3.,6.,4.,0.,5.,5.,2.,1.,2. };
  float sc[6] = { 1384.,1712.,2201.,858.,1075.,538. };
  float tc[12] = { 18.,23.,19.,19.,23.,22.,0.,0.,0.,12.,6.,12. };
  float uc[12] = { 30.,36.,0.,18.,34.,37.,0.,30.,17.,16.,0.,12. };
  float vc[12] = { 4.,7.,3.,17.,32.,18.,0.,0.,0.,43.,64.,44. };
  float wc[10] = { 74.,66.,61.,50.,50.,50.,21.,20.,15.,9. };

    /* Local variables */
  int j;
  Newguy("TRLA-TRLTA-TRAL-TRALT.", "TTRLA   ");
    param_1.zerlev = param_1.zerov[1];
    TCL::trla(dc, ec, _BLNK__1.a, 4, 3);
    Mverif(111, _BLNK__1.a, vc, 12);

    TCL::ucopy(ec, _BLNK__1.a, 12);
    TCL::trla(dc, _BLNK__1.a, _BLNK__1.a, 4, 3);
    Mverif(112, _BLNK__1.a, vc, 12);

    TCL::trlta(dc, ec, _BLNK__1.a, 4, 3);
    Mverif(121, _BLNK__1.a, tc, 12);
    TCL::ucopy(ec, _BLNK__1.a, 12);
    TCL::trlta(dc, _BLNK__1.a, _BLNK__1.a, 4, 3);
    Mverif(122, _BLNK__1.a, tc, 12);

    TCL::tral(ec, dc, _BLNK__1.a, 3, 4);
    Mverif(131, _BLNK__1.a, uc, 12);
    TCL::ucopy(ec, _BLNK__1.a, 12);
    TCL::tral(_BLNK__1.a, dc, _BLNK__1.a, 3, 4);
    Mverif(132, _BLNK__1.a, uc, 12);

    TCL::tralt(ec, dc, _BLNK__1.a, 3, 4);
    Mverif(141, _BLNK__1.a, yc, 12);
    TCL::ucopy(ec, _BLNK__1.a, 12);
    TCL::tralt(_BLNK__1.a, dc, _BLNK__1.a, 3, 4);
    Mverif(142, _BLNK__1.a, yc, 12);
/* -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - 
*/
    if (param_1.itimes == 0) 	goto L180;
    
    tinf[1] = tinf2[0];
    tinf[3] = tinf4[0];
    Timed(&param_1.timerd);
    for (j = 1; j <= 1000; ++j) 
	 TCL::trla(dc, ec, _BLNK__1.a, 4, 3);

    Timing(tinf);
    tinf[1] = tinf2[1];
    tinf[3] = tinf4[1];
    Timed(&param_1.timerd);
    for (j = 1; j <= 1000; ++j) 
	 TCL::trlta(dc, ec, _BLNK__1.a, 4, 3);

    Timing(tinf);
    tinf[1] = tinf2[2];
    tinf[3] = tinf4[2];
    Timed(&param_1.timerd);
    for (j = 1; j <= 1000; ++j) 
	 TCL::tral(ec, dc, _BLNK__1.a, 3, 4);

    Timing(tinf);
    tinf[1] = tinf2[3];
    tinf[3] = tinf4[3];
    Timed(&param_1.timerd);

    for (j = 1; j <= 1000; ++j) 
	 TCL::tralt(ec, dc, _BLNK__1.a, 3, 4);

    Timing(tinf);
/* ---------------------------------------------------------------------- 
*/
L180:
    Newguy("TRAAT-TRATA.", "TTRLA   ");
    TCL::traat(ec, _BLNK__1.a, 4, 3);
    Mverif(211, _BLNK__1.a, wc, 10);

    TCL::trata(ec, _BLNK__1.a, 4, 3);
    Mverif(221, _BLNK__1.a, xc, 10);
/* -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - 
*/
    if (param_1.itimes == 0) 	goto L280;

    tinf[1] = tinf2[4];
    tinf[3] = tinf4[4];
    Timed(&param_1.timerd);

    for (j = 1; j <= 1000; ++j) 
	 TCL::traat(ec, _BLNK__1.a, 4, 3);
    
    Timing(tinf);
    tinf[1] = tinf2[5];
    tinf[3] = tinf4[5];
    Timed(&param_1.timerd);

    for (j = 1; j <= 1000; ++j) 
	 TCL::trata(ec, _BLNK__1.a, 4, 3);
    
    Timing(tinf);
/* ---------------------------------------------------------------------- 
*/
L280:
    Newguy("TRASAT-TRATSA-TRQSQ.", "TTRLA   ");
    TCL::trasat(ec, ac, _BLNK__1.a, 3, 4);
    Mverif(311, _BLNK__1.a, sc, 6);

    TCL::tratsa(ec, dc, _BLNK__1.a, 3, 4);
    Mverif(321, _BLNK__1.a, atsac, 6);
    TCL::trqsq(dc, ac, _BLNK__1.a, 4);
    Mverif(331, _BLNK__1.a, qsqc, 10);
/* -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - 
*/
    if (param_1.itimes == 0) 	goto L380;

    tinf[1] = tinf2[6];
    tinf[3] = tinf4[6];
    Timed(&param_1.timerd);

    for (j = 1; j <= 1000; ++j) 
	 TCL::trasat(ac, ec, _BLNK__1.a, 4, 3);

    Timing(tinf);
    tinf[1] = tinf2[7];
    tinf[3] = tinf4[7];
    Timed(&param_1.timerd);

    for (j = 1; j <= 1000; ++j) 
	 TCL::tratsa(ec, ac, _BLNK__1.a, 3, 4);
    
    Timing(tinf);
    tinf[1] = tinf2[8];
    tinf[3] = tinf4[8];
    Timed(&param_1.timerd);
    for (j = 1; j <= 1000; ++j) 
	 TCL::trqsq(dc, ac, _BLNK__1.a, 4);

    Timing(tinf);
/* ---------------------------------------------------------------------- 
*/
L380:
    Newguy("TRSA-TRAS-TRSAT-TRATS.", "TTRLA   ");
    TCL::trsa(dc, ec, _BLNK__1.a, 4, 3);
    Mverif(411, _BLNK__1.a, sac, 12);

    TCL::tras(ec, dc, _BLNK__1.a, 3, 4);
    Mverif(421, _BLNK__1.a, asc, 12);

    TCL::trsat(dc, ec, _BLNK__1.a, 4, 3);
    Mverif(431, _BLNK__1.a, satc, 12);

    TCL::trats(ec, dc, _BLNK__1.a, 3, 4);
    Mverif(441, _BLNK__1.a, atsc, 12);
/* -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - 
*/
    if (param_1.itimes == 0) 	goto L480;
    tinf[1] = tinf2[9];
    tinf[3] = tinf4[9];
    Timed(&param_1.timerd);

    for (j = 1; j <= 1000; ++j) 
	 TCL::trsa(dc, ec, _BLNK__1.a, 4, 3);

    Timing(tinf);
    tinf[1] = tinf2[10];
    tinf[3] = tinf4[10];
    Timed(&param_1.timerd);

    for (j = 1; j <= 1000; ++j) 
	 TCL::tras(ec, dc, _BLNK__1.a, 3, 4);
    Timing(tinf);
    tinf[1] = tinf2[11];
    tinf[3] = tinf4[11];
    Timed(&param_1.timerd);
    for (j = 1; j <= 1000; ++j) 
	 TCL::trsat(dc, ec, _BLNK__1.a, 4, 3);
    
    Timing(tinf);
    tinf[1] = tinf2[12];
    tinf[3] = tinf4[12];
    Timed(&param_1.timerd);
    for (j = 1; j <= 1000; ++j) 
	 TCL::trats(ec, dc, _BLNK__1.a, 3, 4);
    Timing(tinf);
/* ---------------------------------------------------------------------- 
*/
L480:
    Newguy("TRPCK-TRUPCK.", "TTRLA   ");
    TCL::trpck(upckc, _BLNK__1.a, 4);
    Mverif(511, _BLNK__1.a, pckc, 10);

    TCL::trupck(pckc, _BLNK__1.b, 4);
    Mverif(521, _BLNK__1.b, upckc, 16);

    TCL::trpck(_BLNK__1.b, _BLNK__1.b, 4);
    Mverif(512, _BLNK__1.b, pckc, 10);
    TCL::trupck(_BLNK__1.a, _BLNK__1.a, 4);
    Mverif(522, _BLNK__1.a, upckc, 16);
/* -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - 
*/
    if (param_1.itimes == 0) 	goto L580;    
    tinf[1] = tinf2[13];
    tinf[3] = tinf4[13];
    Timed(&param_1.timerd);
    for (j = 1; j <= 1000; ++j) 
	 TCL::trpck(_BLNK__1.a, _BLNK__1.b, 4);

    Timing(tinf);
    tinf[3] = tinf4[14];
    tinf[1] = tinf2[14];
    Timed(&param_1.timerd);
    for (j = 1; j <= 1000; ++j) 
	 TCL::trupck(_BLNK__1.a, _BLNK__1.b, 4);
 
    Timing(tinf);
L580:
    return 0;
} /* trla */

#undef tinf4
#undef tinf2
#undef tinf


//____________________________________________________________________________________
// ttrcho.F -- translated by f2c (version 19970219).
int StMicky::ttrcho()
{
    /* Initialized data */

  struct {
	char e_1[4];
	int e_2;
	} equiv_14 = { {'S', 'M', 'U', 'L'}, 0 };

#define tinfm2 (*(int *)&equiv_14)

  struct {
	char e_1[4];
	int e_2;
	} equiv_15 = { {'4', '*', '4', ' '}, 0 };

#define tinfm4 (*(int *)&equiv_15)

#if 0
  struct {
	char e_1[4];
	float e_2;
	} equiv_16 = { {'S', 'M', 'L', 'U'}, 0. };
#endif

#define tinfl2 (*(float *)&equiv_16)

  float ac[10] = { 1.,0.,0.,2.,0.,13.,4.,0.,23.,77. };
  float bc[10] = { 21.,26.,34.,0.,0.,0.,24.,30.,0.,36. };
  float dc[10] = { 1.,2.,3.,0.,0.,0.,4.,5.,0.,6. };
  float pc[10] = { 1.,0.,0.,2.,0.,3.,4.,0.,5.,6. };
  float ec[10] = { 1.,2.,13.,0.,0.,0.,4.,23.,0.,77. };
  struct {
	int e_1;
	char e_2[4];
	int e_3;
	char e_4[4];
	} equiv_17 = { 1000, {'C', 'H', 'L', 'U'}, 1, {'4', '*', '4', ' '} };

#define tinf ((int *)&equiv_17)

  struct {
	char e_1[4];
	int e_2;
	} equiv_18 = { {'C', 'H', 'U', 'L'}, 0 };

#define tinft2 (*(int *)&equiv_18)


    /* Local variables */
  int j;
    Newguy("TRCHLU-TRCHUL.", "TTRCHO  ");
    param_1.zerlev = param_1.zerov[2];
    TCL::trchlu(ac, _BLNK__1.a, 4);
    Mverif(11, _BLNK__1.a, pc, 10);

    TCL::ucopy(ac, _BLNK__1.a, 10);
    TCL::trchlu(_BLNK__1.a, _BLNK__1.a, 4);
    Mverif(12, _BLNK__1.a, pc, 10);

    TCL::trchul(bc, _BLNK__1.a, 4);
    Mverif(21, _BLNK__1.a, dc, 10);
    TCL::ucopy(bc, _BLNK__1.a, 10);
    TCL::trchul(_BLNK__1.a, _BLNK__1.a, 4);
    Mverif(22, _BLNK__1.a, dc, 10);
/* - - - - - - - - - - - - - - - - - - - -- -  - -- - - - - - - - - - - - 
*/
    if (param_1.itimes == 0) 	goto L120;
    Timed(&param_1.timerd);
    for (j = 1; j <= 1000; ++j)
	 TCL::trchlu(ac, _BLNK__1.a, 4);

    Timing(tinf);
    tinf[1] = tinft2;
    Timed(&param_1.timerd);
    for (j = 1; j <= 1000; ++j) 
	 TCL::trchul(bc, _BLNK__1.a, 4);

    Timing(tinf);
/* -----------------------------------------------------------------------
 */
L120:
    Newguy("TRSMUL-TRSMLU.", "TTRCHO  ");
    param_1.zerlev = param_1.zerov[1];
    TCL::trsmul(dc, _BLNK__1.a, 4);
    Mverif(11, _BLNK__1.a, bc, 10);

    TCL::ucopy(dc, _BLNK__1.a, 10);
    TCL::trsmul(_BLNK__1.a, _BLNK__1.a, 4);
    Mverif(12, _BLNK__1.a, bc, 10);

    TCL::trsmlu(dc, _BLNK__1.a, 4);
    Mverif(22, _BLNK__1.a, ec, 10);

    TCL::ucopy(dc, _BLNK__1.a, 10);
    TCL::trsmlu(_BLNK__1.a, _BLNK__1.a, 4);
    Mverif(21, _BLNK__1.a, ec, 10);
/* - - - - - - - - - - - - - - - - - - - -- -  - -- - - - - - - - - - - - 
*/
    if (param_1.itimes == 0) 	return 0;
    tinf[1] = tinfm2;
    tinf[3] = tinfm4;
    Timed(&param_1.timerd);
    for (j = 1; j <= 1000; ++j) 
	TCL::trsmul(dc, _BLNK__1.a, 4);

    Timing(tinf);
//    tinf[1] = tinfl2;
    Timed(&param_1.timerd);
    for (j = 1; j <= 1000; ++j) 
	TCL::trsmlu(dc, _BLNK__1.a, 4);

    Timing(tinf);
    return 0;
} /* ttrcho_ */

