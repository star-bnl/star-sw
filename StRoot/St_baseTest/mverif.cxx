//*-- Author :    Valery Fine(fine@bnl.gov)   25/09/99  
//
// mverif.F -- translated by f2c (version 19970219).
//
// The test ("micky") of the set of methods to work with the plain 
// matrix / vector "derived" from  
// http://wwwinfo.cern.ch/asdoc/shortwrupsdir/f110/top.html 
//
// $Id: mverif.cxx,v 1.1 1999/09/23 18:33:10 fine Exp $
// $Log: mverif.cxx,v $
// Revision 1.1  1999/09/23 18:33:10  fine
// test system for RMath class has been introduced
//
//

#include <iostream.h>

void timing_(){ cout << "timimg" << endl;}
void timed_(){ cout << "Timed" << endl;}
inline double dabs(double d){ return d>0?d: -d;}
inline int min(int n1, int n2){ return n1 < n2 ? n1 : n2;}
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

//___________________________________________________________________________
void prtest_(){
  if (param_1.ntest <= 0 ) return;
  if (param_1.nfail)  cout << " TESTS FAIL ";
  else                cout << " Tests pass ";
  for (int i =0;i<=param_1.ntest;i++) cout << param_1.mtestv[i] << "    ";
  cout << endl;
  param_1.ntest = 0;
};


//___________________________________________________________________________
void mverif_(int ntt, float *have, float *amust, int nn)
{
    /* System generated locals */
    float r__1;

    /* Local variables */
    static float diff, test[200], zeru;
    static int j, nhave, ntuse, n1, n2;
#define ia ((int *)&_BLNK__1)
#define ib ((int *)&_BLNK__1 + 1000)
    static int jj;
    static float sum;


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
    if (param_1.ntest >= 10) prtest_();    

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
      prtest_();
      param_1.nfail = 0;
    }
L39:
    ++param_1.ntest;
    param_1.mtestv[param_1.ntest - 1] = ntuse;
    if (param_1.neachp == 0) return ;
    prtest_();
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
    prtest_();
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
    prtest_();
    cout << endl << " VERIFF VECTOR FOR TEST" << ntuse 
         << "  MORE THAN 200 WORDS, namely " << nhave << endl;
    return;


} /* mverif_ */
