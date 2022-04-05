//*-- Author :    Valery Fine(fine@bnl.gov)   25/09/99  
//
// mverif.F -- translated by f2c (version 19970219).
//
// The test ("micky") of the set of methods to work with the plain 
// matrix / vector "derived" from  
// http://wwwinfo.cern.ch/asdoc/shortwrupsdir/f110/top.html 
//
// $Id: StMicky.cxx,v 1.7 2006/12/08 17:51:12 fine Exp $
// $Log: StMicky.cxx,v $
// Revision 1.7  2006/12/08 17:51:12  fine
// prepare the test are to move to ROOT CVS
//
// Revision 1.6  2003/09/02 17:59:24  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.5  1999/12/07 22:33:10  fine
// Clean up to remove compilation warnings
//
// Revision 1.4  1999/10/02 21:58:07  fine
// double precision
//
// Revision 1.3  1999/09/30 15:50:20  fine
// ClassDef has been introduced
//
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

#include <Stiostream.h>
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

    da = (double *)(&(_BLNK__1.a));
    db = (double *)(&(_BLNK__1.b));

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
    if (nhave >= 201) {
      Prtest();
      cout << endl << " VERIFF VECTOR FOR TEST" << ntuse 
           << "  MORE THAN 200 WORDS, namely " << nhave << endl;
      return;
    }

    zeru = param_1.zerlev;
    if (param_1.ntest >= 10) StMicky::Prtest();    

/* ----              Verify */
    for (jj = 1; jj <= nhave; ++jj) {
	j = jj;
	diff = (r__1 = have[j] - amust[j], dabs(r__1));
	if (diff < zeru)    continue;
	sum = (r__1 = have[j] + amust[j], dabs(r__1));

	if (sum < 2. || (diff * (float)2. / sum > zeru))  goto L41;
    }
/* ----              VERIFY OK. */
    if (param_1.nfail) {
      Prtest();
      param_1.nfail = 0;
    }

    ++param_1.ntest;
    param_1.mtestv[param_1.ntest - 1] = ntuse;
    if (param_1.neachp == 0) return ;
    Prtest();
    return ;
/* ----              VERIFY FAILURE */
L41:
    jj = j;
    ++param_1.nfailt;
    if (param_1.nfail == 0)                   Prtest();
    else if (param_1.nfail > param_1.nfaipr)  ++param_1.nfail;

    cout << " TEST " << ntuse << " FAILED, Dump follows" << endl;
    ++param_1.nfail;
    n1 = jj;
    do {
       // Computing MIN */
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
    } while  (n1 <= nhave);

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

    do {
      // Computing MIN 
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
    } while (n1 <= nhave);

    cout << endl;
    return;
} /* mverif_ */

//____________________________________________________________________________________
void  StMicky::Newguy(const char *t1, const char *t2) {
 Prtest();
 param_1.nfail = 0;
 cout << " ---------------------------------------------------------------" << endl <<
         " Routing " << t2 << " testing " << t1 << endl;
}

