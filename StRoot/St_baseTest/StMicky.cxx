//*-- Author :    Valery Fine(fine@bnl.gov)   25/09/99  
//
// mverif.F -- translated by f2c (version 19970219).
//
// The test ("micky") of the set of methods to work with the plain 
// matrix / vector "derived" from  
// http://wwwinfo.cern.ch/asdoc/shortwrupsdir/f110/top.html 
//
// $Id: StMicky.cxx,v 1.2 1999/09/28 19:54:56 fine Exp $
// $Log: StMicky.cxx,v $
// Revision 1.2  1999/09/28 19:54:56  fine
// RMath has been renamed to StCL
//
// Revision 1.1  1999/09/26 19:35:12  fine
// Micky test has been rearranged
//
// Revision 1.1  1999/09/23 18:33:10  fine
// test system for StCL class has been introduced
//
//

#include <iostream.h>
#include "StMicky.h"

inline double dabs(double d){ return d>0?d: -d;}
inline int min(int n1, int n2){ return n1 < n2 ? n1 : n2;}

//___________________________________________________________________________
StMicky::StMicky(){ Minit(); }

//___________________________________________________________________________
void StMicky::Prtest(){
  if (param_1.ntest <= 0 ) return;
  if (param_1.nfail)  cout << " TESTS FAIL ";
  else                cout << " Tests pass ";
  for (int i =0;i<=param_1.ntest;i++) cout << param_1.mtestv[i] << "    ";
  cout << endl;
  param_1.ntest = 0;
};

//__________________________________________________________________
void StMicky::Minit()
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
    int j;

    ia = (int *)(&(_BLNK__1.a));
    ib = (int *)(&(_BLNK__1.b));

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


//___________________________________________________________________________
void StMicky::Mverif(int ntt, float *have, float *amust, int nn)
{
    /* System generated locals */
    float r__1;

    /* Local variables */
    float diff, test[200], zeru;
    int j, nhave, ntuse, n1, n2;

    int jj;
    float sum;


/* -----------------------------------------------------------------------
 */
    /* Parameter adjustments */
    --amust;
    --have;

    /* Function Body */
    ntuse = ntt;
    nhave = nn;
    if (nhave >= 201) goto L91; 

    zeru = param_1.zerlev;
    if (param_1.ntest >= 10) StMicky::Prtest();    

/* ----              Verify */
    for (jj = 1; jj <= nhave; ++jj) {
	j = jj;
	diff = (r__1 = have[j] - amust[j], dabs(r__1));
	if (diff < zeru)    continue;
	sum = (r__1 = have[j] + amust[j], dabs(r__1));
	if (sum < (float)2.)   goto L41;
	if (diff * (float)2. / sum > zeru)  goto L41;
    }
/* ----              VERIFY OK. */
    if (param_1.nfail) {
      StMicky::Prtest();
      param_1.nfail = 0;
    }
L39:
    ++param_1.ntest;
    param_1.mtestv[param_1.ntest - 1] = ntuse;
    if (param_1.neachp == 0) return ;
    StMicky::Prtest();
    return ;
/* ----              VERIFY FAILURE */
L41:
    jj = j;
    ++param_1.nfailt;
    if (param_1.nfail == 0)             goto L47;
    if (param_1.nfail < param_1.nfaipr) goto L48;
    ++param_1.nfail;
    goto L39;
L47:
    StMicky::Prtest();
L48:
    cout << " TEST " << ntuse << " FAILED, Dump follows" << endl;
    ++param_1.nfail;
    n1 = jj;
L51:
/* Computing MIN */
    n2 = min(n1 + 4,nhave);
    cout << n1 << " MUST ";
    for (j = n1; j <= n2; ++j) {
//        cout << hex << amust[j] << "     ";
        cout <<  amust[j] << "     ";
        if ((j-n1)%5 == 0) cout << endl;
    }    
    cout << endl;

    cout << "          HAVE ";
    for (j = n1; j <= n2; ++j) {
//        cout << hex << have[j] << "     ";
        cout << have[j] << "     ";
        if ((j-n1)%5 == 0) cout << endl;
    }
    cout << endl;

    n1 += 5;
    if (n1 <= nhave) goto L51;

    if (n1 - 1 != nhave) cout << endl;

/* --                PRINT VECTORS IN FLOATING FORMAT */

    cout << "           Floating point values,   ZERLEV= "<< zeru << endl;

    for (j = jj; j <=  nhave; ++j) {
	diff = (r__1 = have[j] - amust[j], dabs(r__1));
	sum = (r__1 = have[j] + amust[j], dabs(r__1));
	test[j - 1] = (float)0.;
	if (diff < zeru) continue;
	test[j - 1] = diff;
	if (sum < (float)2.) continue;
	test[j - 1] = test[j - 1] * (float)2. / sum;
    }
    n1 = jj;
L60:
/* Computing MIN */
    n2 = min(n1 + 4,nhave);
    cout << n1 << "  MUST ";
    for (j = n1; j <= n2; ++j) {
        cout << amust[j] << "     ";
        if ((j-n1)%5 == 0) cout << endl;
    }
    cout << endl;

    cout << "          HAVE ";
    for (j = n1; j <= n2; ++j) {
        cout << have[j] << "     ";
        if ((j-n1)%5 == 0) cout << endl;
    }
    cout << endl;

    cout << "          TEST ";
    for (j = n1; j <= n2; ++j) {
        cout << test[j - 1] << "     ";
        if ((j-n1)%5 == 0) cout << endl;
    }
    cout << endl;

    n1 += 5;
    if (n1 <= nhave) goto L60;

    cout << endl;
    return;
L91:
    StMicky::Prtest();
    cout << endl << " VERIFF VECTOR FOR TEST" << ntuse 
         << "  MORE THAN 200 WORDS, namely " << nhave << endl;
    return;


} /* mverif_ */

//____________________________________________________________________________________
void  StMicky::Newguy(const char *t1, const char *t2) {
 Prtest();
 param_1.nfail = 0;
 cout << " ---------------------------------------------------------------" << endl <<
         " Routing " << t2 << " testing " << t1 << endl;
}

