//*-- Author :    Valery Fine(fine@bnl.gov)   25/09/99  
//
// minit.F -- translated by f2c (version 19970219).
//
// The test ("micky") of the set of methods to work with the plain 
// matrix / vector "derived" from  
// http://wwwinfo.cern.ch/asdoc/shortwrupsdir/f110/top.html 
//
// $Id: minit.cxx,v 1.1 1999/09/23 18:33:09 fine Exp $
// $Log: minit.cxx,v $
// Revision 1.1  1999/09/23 18:33:09  fine
// test system for RMath class has been introduced
//
//

#include <iostream.h>

/* Common Block Declarations */

extern struct {
    int iqbitw, iqchaw, itb, nlines, itimes;
    float timerd;
    int iflgu, lungu;
    float zergu, zerov[5], zerlev;
    int loglev, nfaipr, neachp, nfailt, nfail, ntest, mtestv[20];
} param_;

#define param_1 param_

extern struct {
    float a[1000], b[1010];
    int ibcd[47], intg[100];
} _BLNK__;

#define _BLNK__1 _BLNK__

//__________________________________________________________________
/* Subroutine */ void minit_()
{
    /* Initialized data */

    static struct {
	char e_1[188];
	int e_2;
	} equiv_7 = { {' ', ' ', ' ', ' ', '0', ' ', ' ', ' ', '1', ' ', ' ', 
		' ', '2', ' ', ' ', ' ', '3', ' ', ' ', ' ', '4', ' ', ' ', 
		' ', '5', ' ', ' ', ' ', '6', ' ', ' ', ' ', '7', ' ', ' ', 
		' ', '8', ' ', ' ', ' ', '9', ' ', ' ', ' ', 'A', ' ', ' ', 
		' ', 'B', ' ', ' ', ' ', 'C', ' ', ' ', ' ', 'D', ' ', ' ', 
		' ', 'E', ' ', ' ', ' ', 'F', ' ', ' ', ' ', 'G', ' ', ' ', 
		' ', 'H', ' ', ' ', ' ', 'I', ' ', ' ', ' ', 'J', ' ', ' ', 
		' ', 'K', ' ', ' ', ' ', 'L', ' ', ' ', ' ', 'M', ' ', ' ', 
		' ', 'N', ' ', ' ', ' ', 'O', ' ', ' ', ' ', 'P', ' ', ' ', 
		' ', 'Q', ' ', ' ', ' ', 'R', ' ', ' ', ' ', 'S', ' ', ' ', 
		' ', 'T', ' ', ' ', ' ', 'U', ' ', ' ', ' ', 'V', ' ', ' ', 
		' ', 'W', ' ', ' ', ' ', 'X', ' ', ' ', ' ', 'Y', ' ', ' ', 
		' ', 'Z', ' ', ' ', ' ', '+', ' ', ' ', ' ', '-', ' ', ' ', 
		' ', '=', ' ', ' ', ' ', '*', ' ', ' ', ' ', '/', ' ', ' ', 
		' ', '(', ' ', ' ', ' ', ')', ' ', ' ', ' ', '.', ' ', ' ', 
		' ', ',', ' ', ' ', ' ', '\'', ' ', ' ', ' '}, 0 };

#define mbcd ((int *)&equiv_7)




    /* Local variables */
    static int j;
#define ia ((int *)&_BLNK__1)
#define ib ((int *)&_BLNK__1 + 1000)

    param_1.iqbitw = 32;
    param_1.iqchaw = 4;
    param_1.itb = 6;
    param_1.itimes = 0;
    for (j = 1; j <= 47; ++j)   _BLNK__1.ibcd[j - 1] = mbcd[j - 1];
    for (j = 1; j <= 100; ++j) 	_BLNK__1.intg[j - 1] = j;

/* ----     ZEROV(1-5)  CONTAIN THE LEVELS OF PRECISION EXPECTED AFTER */
/* -        (1) NO, (2) LITTLE, (3) SOME, (4) FAIR, (5) MUCH FLOATING CALC
 */
    param_1.zerov[0] = (float)1e-10;
    param_1.zerov[1] = (float)1e-6;
    param_1.zerov[2] = (float)1e-5;
    param_1.zerov[3] = (float)1e-4;
    param_1.zerov[4] = (float).001;
    param_1.zergu = param_1.zerov[0];
    param_1.zerlev = param_1.zerov[0];
    param_1.nfaipr = 4;
    param_1.neachp = 0;
    param_1.nfailt = 0;
    param_1.nfail = 0;
    param_1.ntest = 0;
    /* Format strings */
    cout << " MICKY executing." << endl;
    cout << "   MICKY    2.71  950712  9.30" << endl;


//    kerngt_(&param_1.itb);
} /* minit_ */

#undef ib
#undef ia
#undef mbcd


