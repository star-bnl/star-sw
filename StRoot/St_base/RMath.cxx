//*-- Author :    Valery Fine(fine@bnl.gov)   25/09/99  
//
// The set of methods to work with the plain matrix / vector
// "derived" from  http://wwwinfo.cern.ch/asdoc/shortwrupsdir/f110/top.html 
// "derived" from  http://wwwinfo.cern.ch/asdoc/shortwrupsdir/f112/top.html 
//
// $Id: RMath.cxx,v 1.6 1999/09/27 00:14:46 fine Exp $
// $Log: RMath.cxx,v $
// Revision 1.6  1999/09/27 00:14:46  fine
// some bugs for double have been fixed
//
// Revision 1.5  1999/09/26 23:37:50  fine
// Bug fixes for F112
//
// Revision 1.4  1999/09/26 02:48:50  fine
// F112 CERNLIB package (TR matrix) has been added. No micky test yet
//
// Revision 1.3  1999/09/23 18:32:11  fine
// double prec for float matrices was introduced
//
//
#include "RMath.h"
#include "TMath.h"

// http://wwwinfo.cern.ch/asdoc/shortwrupsdir/f110/top.html 

ClassImp(RMath)
//___________________________________________________________________________
void RMath::mxmad_0_(int n_, float *a, float *b, float *c, int i, int j, int k)
{
////////////////////////////////////////////////////////////////////////////////////////////////////////
/* begin_html 
<table border>
<tr><td align=left>Author(s): TC  <td align=left>Library: KERNLIB 
<tr><td align=left>Submitter: C. Letertre  <td align=left>Submitted: 01.08.1969  
<tr><td align=left>Language: Fortran  <td align=left>Revised: 07.03.1989  
</table>
<BR>
<!-- KEY VALUE="tc manipulation matrix row-wise " -->
<P>
<DIV ALIGN=CENTER><P ALIGN=CENTER> <IMG WIDTH=555 HEIGHT=96 ALIGN=BOTTOM ALT="tex2html_wrap271" SRC="img1.gif"  > </P></DIV>
The routines of <TT>MXPACK</TT> compute the product of two matrices or the
product of their transposed matrices and may add or subtract to the
resultant matrix a third one, add or subtract one matrix from another,
or transfer a matrix, its negative, or a multiple of it, transpose a
given matrix, build up a unit matrix, multiply a matrix by a diagonal
(from left or from right) and may add the result to another matrix,
add to square matrix the multiple of a diagonal matrix, compute the
products  <IMG WIDTH=79 HEIGHT=12 ALIGN=BOTTOM ALT="tex2html_wrap_inline191" SRC="img2.gif"  >  ( <IMG WIDTH=16 HEIGHT=12 ALIGN=BOTTOM ALT="tex2html_wrap_inline193" SRC="img3.gif"  >  denotes the transpose of
 <IMG WIDTH=12 HEIGHT=11 ALIGN=BOTTOM ALT="tex2html_wrap_inline195" SRC="img4.gif"  > ) and  <IMG WIDTH=79 HEIGHT=12 ALIGN=BOTTOM ALT="tex2html_wrap_inline197" SRC="img5.gif"  > . It is assumed that matrices are
stored <B>row-wise without gaps</B>, contrary to the Fortran convention.
<P>
<p><b>Structure:</b><p>
<TT>SUBROUTINE</TT> subprograms  <BR> 
User Entry Names:
<TABLE COLS=8>
<COL ALIGN=LEFT><COL ALIGN=LEFT><COL ALIGN=LEFT><COL ALIGN=LEFT><COL ALIGN=LEFT><COL ALIGN=LEFT><COL ALIGN=LEFT><COL ALIGN=LEFT>
<TR><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP>
<A NAME=MXMAD><TT>MXMAD</TT></A>,  </TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP> <A NAME=MXMAD1><TT>MXMAD1</TT></A>, </TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP> <A NAME=MXMAD2><TT>MXMAD2</TT></A>, </TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP> <A NAME=MXMAD3><TT>MXMAD3</TT></A>, </TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP>
<A NAME=MXMPY><TT>MXMPY</TT></A>,  </TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP> <A NAME=MXMPY1><TT>MXMPY1</TT></A>, </TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP> <A NAME=MXMPY2><TT>MXMPY2</TT></A>, </TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP> <A NAME=MXMPY3><TT>MXMPY3</TT></A>, </TD></TR>
<TR><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP> 
<A NAME=MXMUB><TT>MXMUB</TT></A>,  </TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP> <A NAME=MXMUB1><TT>MXMUB1</TT></A>, </TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP> <A NAME=MXMUB2><TT>MXMUB2</TT></A>, </TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP> <A NAME=MXMUB3><TT>MXMUB3</TT></A>, </TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP>
<A NAME=MXTRP><TT>MXTRP</TT></A>,  </TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP> <A NAME=MXUTY><TT>MXUTY</TT></A>,  </TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP> <A NAME=MXMLRT><TT>MXMLRT</TT></A>, </TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP> <A NAME=MXMLTR><TT>MXMLTR</TT></A>
</TD></TR>
</TABLE>
<P>
<p><b>Usage:</b><p>
<B>Matrix Multiplication</B> <BR>
<TABLE COLS=4>
<COL ALIGN=CENTER><COL ALIGN=LEFT><COL ALIGN=CENTER><COL ALIGN=LEFT>
<TR><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP>
<TT>CALL MXMPY(A,B,C,NI,NJ,NK)</TT> </TD><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP>  <IMG WIDTH=132 HEIGHT=25 ALIGN=MIDDLE ALT="tex2html_wrap_inline199" SRC="img6.gif"  >  </TD></TR>
<TR><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP> 
<TT>CALL MXMPY1(A,Q,C,NI,NJ,NK)</TT> </TD><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP>  <IMG WIDTH=67 HEIGHT=24 ALIGN=MIDDLE ALT="tex2html_wrap_inline201" SRC="img7.gif"  >  \
( <IMG WIDTH=12 HEIGHT=22 ALIGN=MIDDLE ALT="tex2html_wrap_inline203" SRC="img8.gif"  >  is  <IMG WIDTH=52 HEIGHT=18 ALIGN=MIDDLE ALT="tex2html_wrap_inline205" SRC="img9.gif"  > ) </TD></TR>
<TR><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP> 
<TT>CALL MXMPY2(P,B,C,NI,NJ,NK)</TT> </TD><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP>  <IMG WIDTH=66 HEIGHT=13 ALIGN=BOTTOM ALT="tex2html_wrap_inline207" SRC="img10.gif"  >  \
( <IMG WIDTH=10 HEIGHT=11 ALIGN=BOTTOM ALT="tex2html_wrap_inline209" SRC="img11.gif"  >  is  <IMG WIDTH=52 HEIGHT=18 ALIGN=MIDDLE ALT="tex2html_wrap_inline211" SRC="img12.gif"  > ) </TD></TR>
<TR><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP> 
<TT>CALL MXMPY3(P,Q,C,NI,NJ,NK)</TT> </TD><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP>  <IMG WIDTH=71 HEIGHT=24 ALIGN=MIDDLE ALT="tex2html_wrap_inline213" SRC="img13.gif"  >  </TD></TR>
</TABLE>
 <BR>
If  <IMG WIDTH=46 HEIGHT=10 ALIGN=BOTTOM ALT="tex2html_wrap_inline215" SRC="img14.gif"  > ,  <IMG WIDTH=11 HEIGHT=11 ALIGN=BOTTOM ALT="tex2html_wrap_inline217" SRC="img15.gif"  >  will be filled with zeros. <BR>
<B>Matrix Multiplication and Addition</B> <BR>
<TABLE COLS=4>
<COL ALIGN=CENTER><COL ALIGN=LEFT><COL ALIGN=CENTER><COL ALIGN=LEFT>
<TR><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP>
<TT>CALL MXMAD(A,B,C,NI,NJ,NK)</TT> </TD><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP>
 <IMG WIDTH=188 HEIGHT=25 ALIGN=MIDDLE ALT="tex2html_wrap_inline219" SRC="img16.gif"  >  </TD></TR>
<TR><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP> 
<TT>CALL MXMAD1(A,Q,C,NI,NJ,NK)</TT> </TD><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP>  <IMG WIDTH=100 HEIGHT=24 ALIGN=MIDDLE ALT="tex2html_wrap_inline221" SRC="img17.gif"  >  </TD></TR>
<TR><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP> 
<TT>CALL MXMAD2(P,B,C,NI,NJ,NK)</TT> </TD><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP>  <IMG WIDTH=98 HEIGHT=26 ALIGN=MIDDLE ALT="tex2html_wrap_inline223" SRC="img18.gif"  >  </TD></TR>
<TR><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP> 
<TT>CALL MXMAD3(P,Q,C,NI,NJ,NK)</TT> </TD><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP>  <IMG WIDTH=104 HEIGHT=24 ALIGN=MIDDLE ALT="tex2html_wrap_inline225" SRC="img19.gif"  >  </TD></TR>
</TABLE>
 <BR>
If  <IMG WIDTH=46 HEIGHT=10 ALIGN=BOTTOM ALT="tex2html_wrap_inline227" SRC="img20.gif"  > ,  <IMG WIDTH=11 HEIGHT=11 ALIGN=BOTTOM ALT="tex2html_wrap_inline229" SRC="img21.gif"  >  will not be changed. <BR>
<P>
<B>Matrix Multiplication and Subtraction</B> <BR>
<TABLE COLS=4>
<COL ALIGN=CENTER><COL ALIGN=LEFT><COL ALIGN=CENTER><COL ALIGN=LEFT>
<TR><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP>
<TT>CALL MXMUB(A,B,C,NI,NJ,NK)</TT> </TD><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP>
 <IMG WIDTH=188 HEIGHT=25 ALIGN=MIDDLE ALT="tex2html_wrap_inline231" SRC="img22.gif"  >  </TD></TR>
<TR><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP> 
<TT>CALL MXMUB1(A,Q,C,NI,NJ,NK)</TT> </TD><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP>  <IMG WIDTH=100 HEIGHT=24 ALIGN=MIDDLE ALT="tex2html_wrap_inline233" SRC="img23.gif"  >  </TD></TR>
<TR><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP> 
<TT>CALL MXMUB2(P,B,C,NI,NJ,NK)</TT> </TD><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP>  <IMG WIDTH=98 HEIGHT=24 ALIGN=MIDDLE ALT="tex2html_wrap_inline235" SRC="img24.gif"  >  </TD></TR>
<TR><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP> 
<TT>CALL MXMUB3(P,Q,C,NI,NJ,NK)</TT> </TD><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP>  <IMG WIDTH=104 HEIGHT=24 ALIGN=MIDDLE ALT="tex2html_wrap_inline237" SRC="img25.gif"  >  </TD></TR>
</TABLE>
 <BR>
If  <IMG WIDTH=46 HEIGHT=10 ALIGN=BOTTOM ALT="tex2html_wrap_inline239" SRC="img26.gif"  > ,  <IMG WIDTH=11 HEIGHT=11 ALIGN=BOTTOM ALT="tex2html_wrap_inline241" SRC="img27.gif"  >  will be replaced by  <IMG WIDTH=23 HEIGHT=20 ALIGN=MIDDLE ALT="tex2html_wrap_inline243" SRC="img28.gif"  > .
<BR>
<B>Matrix Transposition</B> <BR>
<TABLE COLS=4>
<COL ALIGN=CENTER><COL ALIGN=LEFT><COL ALIGN=CENTER><COL ALIGN=LEFT>
<TR><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP>
<TT>CALL MXTRP(A,B,NI,NJ)</TT> </TD><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP>  <IMG WIDTH=94 HEIGHT=25 ALIGN=MIDDLE ALT="tex2html_wrap_inline245" SRC="img29.gif"  >  </TD></TR>
</TABLE>
 <BR>
<B>Unity Matrix</B> <BR>
<TABLE COLS=4>
<COL ALIGN=CENTER><COL ALIGN=LEFT><COL ALIGN=CENTER><COL ALIGN=LEFT>
<TR><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP>
<TT>CALL MXUTY(A,NI)</TT> </TD><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP>  <IMG WIDTH=193 HEIGHT=25 ALIGN=MIDDLE ALT="tex2html_wrap_inline247" SRC="img30.gif"  >  </TD></TR>
</TABLE>
 <BR>
<B>Matrix Multiplication</B> <BR>
<TABLE COLS=4>
<COL ALIGN=CENTER><COL ALIGN=LEFT><COL ALIGN=CENTER><COL ALIGN=LEFT>
<TR><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP>
<TT>CALL MXMLRT(A,B,X,M,N)</TT> </TD><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP>
 <IMG WIDTH=294 HEIGHT=25 ALIGN=MIDDLE ALT="tex2html_wrap_inline249" SRC="img31.gif"  >  </TD></TR>
<TR><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP> 
<TT>CALL MXMLTR(A,B,X,N,M)</TT> </TD><TD VALIGN=BASELINE ALIGN=CENTER NOWRAP></TD><TD VALIGN=BASELINE ALIGN=LEFT NOWRAP>
 <IMG WIDTH=294 HEIGHT=25 ALIGN=MIDDLE ALT="tex2html_wrap_inline251" SRC="img32.gif"  >  </TD></TR>
</TABLE>
<P>
<p><b>Notes:</b><p>
In the formulae above,  <IMG WIDTH=34 HEIGHT=25 ALIGN=MIDDLE ALT="tex2html_wrap_inline253" SRC="img33.gif"  >  <I>etc</I> denotes the ensemble of
elements of the matrix  <IMG WIDTH=12 HEIGHT=11 ALIGN=BOTTOM ALT="tex2html_wrap_inline255" SRC="img34.gif"  >  <I>etc</I>
with the row index <I>i</I> and the column index <I>j</I>.
The Fortran variables <TT>NI</TT>, <TT>NJ</TT> and <TT>NK</TT> specify the
dimensions associated with the indices <I>i</I>,<I>j</I> and <I>k</I>. If
<TT>DIMENSION A(NJ,NI)</TT> reserves space for the
matrix  <IMG WIDTH=12 HEIGHT=11 ALIGN=BOTTOM ALT="tex2html_wrap_inline265" SRC="img35.gif"  > , then the element  <IMG WIDTH=22 HEIGHT=23 ALIGN=MIDDLE ALT="tex2html_wrap_inline267" SRC="img36.gif"  >  is contained in
<TT>A(J,I)</TT>.
<BR>  <IMG WIDTH=6 HEIGHT=7 ALIGN=BOTTOM ALT="tex2html_wrap_inline269" SRC="img37.gif"  > 
<BR><HR>
<P><ADDRESS>
Michel Goossens
Wed Jun  5 04:52:43 METDST 1996
</ADDRESS>
</BODY>
</HTML>
end_html 
*/
////////////////////////////////////////////////////////////////////////////////////////////////////////
    /* Local variables */
    int l, m, n, ia, ic, ib, ja, jb, iia, iib, ioa, iob;

/* CERN PROGLIB# F110    MXMAD           .VERSION KERNFOR  1.0   650809 */
/* ORIG. 01/01/64 RKB */

    /* Parameter adjustments */
    --a;  --b;  --c;
    /* Function Body */
//                      MXMAD MXMAD1 MXMAD2 MXMAD3 MXMPY MXMPY1 MXMPY2 MXMPY3 MXMUB MXMUB1 MXMUB2 MXMUB3
//  const int iandj1[] = {21,   22,    23,    24,   11,    12,    13,    14,    31,   32,   33,    34 };
    const int iandj1[] = {2,    2 ,    2 ,    2 ,   1 ,    1 ,    1 ,    1 ,    3 ,   3 ,   3 ,    3  };
    const int iandj2[] = { 1,    2,     3,     4,    1,     2,     3,     4,     1,    2,    3,     4 };
    int n1 = iandj1[n_];
    int n2 = iandj2[n_];
    if (i == 0 || k == 0) return;
    
    switch (n2) {
	case 1: iia = 1; ioa = j; iib = k; iob = 1; break;
        case 2: iia = 1; ioa = j; iib = 1; iob = j; break;
	case 3: iia = i; ioa = 1; iib = k; iob = 1; break;
        case 4: iia = i; ioa = 1; iib = 1; iob = j; break;
    };

    ia = 1; ic = 1;
    double cic;
    for (l = 1; l <= i; ++l) {
	ib = 1;
	for (m = 1; m <= k; ++m,++ic) {
	    switch (n1) {
		case 1:  c[ic] = (float)0.; break;
		case 3:  c[ic] = -c[ic];    break;
	    };
	    if (j == 0) continue;
	    ja = ia; jb = ib;
            cic = double(c[ic]);
	    for (n = 1; n <= j; ++n, ja+=iia, jb+=iib) 
		cic += a[ja] * b[jb];
            c[ic] = float(cic);
	    ib += iob;
	}
	ia += ioa;
    }
    return;
} /* mxmad_ */

//___________________________________________________________________________
void RMath::mxmad_0_(int n_, double *a, double *b, double *c, int i, int j, int k)
{
    /* Local variables */
    int l, m, n, ia, ic, ib, ja, jb, iia, iib, ioa, iob;

    /* Parameter adjustments */
    --a;  --b;  --c;
    /* Function Body */
//                      MXMAD MXMAD1 MXMAD2 MXMAD3 MXMPY MXMPY1 MXMPY2 MXMPY3 MXMUB MXMUB1 MXMUB2 MXMUB3
//  const int iandj1[] = {21,   22,    23,    24,   11,    12,    13,    14,    31,   32,   33,    34 };
    const int iandj1[] = {2,    2 ,    2 ,    2 ,   1 ,    1 ,    1 ,    1 ,    3 ,   3 ,   3 ,    3  };
    const int iandj2[] = { 1,    2,     3,     4,    1,     2,     3,     4,     1,    2,    3,     4 };
    int n1 = iandj1[n_];
    int n2 = iandj2[n_];
    if (i == 0 || k == 0) return;
    
    switch (n2) {
	case 1: iia = 1; ioa = j; iib = k; iob = 1; break;
        case 2: iia = 1; ioa = j; iib = 1; iob = j; break;
	case 3: iia = i; ioa = 1; iib = k; iob = 1; break;
        case 4: iia = i; ioa = 1; iib = 1; iob = j; break;
    };

    ia = 1; ic = 1;
    for (l = 1; l <= i; ++l) {
	ib = 1;
	for (m = 1; m <= k; ++m,++ic) {
	    switch (n1) {
		case 1:  c[ic] = (double)0.; break;
		case 3:  c[ic] = -c[ic];    break;
	    };
	    if (j == 0) continue;
	    ja = ia; jb = ib;
	    for (n = 1; n <= j; ++n, ja+=iia, jb+=iib) 
		c[ic] += a[ja] * b[jb];
	    ib += iob;
	}
	ia += ioa;
    }
    return;
} /* mxmad_ */

//___________________________________________________________________________
void RMath::mxmlrt_0_(int n__, float *a, float *b, float *c, int ni,int nj)
{
  /* Local variables */
  float x;
  int ia, ib, ic, ja, kc, ii, jj, kj, ki, ia1, ib1, ic1, ja1;

/* CERN PROGLIB# F110    MXMLRT          .VERSION KERNFOR  2.00  720707 */
/* ORIG. 01/01/64 RKB */


/* --      ENTRY MXMLRT */

/* --                C = A(I,J) X B(J,J) X A*(J,I) */
/* --                A* STANDS FOR A-TRANSPOSED */

/*        CALL MXMLRT (A,B,C,NI,NJ)     IS EQUIVALENT TO */
/*             CALL MXMPY (A,B,X,NI,NJ,NJ) */
/*             CALL MXMPY1 (X,A,C,NI,NJ,NI) */

/*        OR   CALL MXMPY1 (B,A,Y,NJ,NJ,NI) */
/*             CALL MXMPY (A,Y,C,NI,NJ,NI) */

    /* Parameter adjustments to use indeces from "1" */
  --a;  --b;  --c;

  /* Function Body */
  int ipa = 1;
  int jpa = nj;
  if (n__ == 1) { ipa = ni;  jpa = 1; }

/* --                C = A*(I,J) X B(J,J) X A(J,I) */

/*        CALL MXMLTR (A,B,C,NI,NJ)     IS EQUIVALENT TO */
/*             CALL MXMPY2 (A,B,X,NI,NJ,NJ) */
/*             CALL MXMPY (X,A,C,NI,NJ,NI) */

/*        OR   CALL MXMPY (B,A,Y,NJ,NJ,NI) */
/*             CALL MXMPY2 (A,Y,C,NI,NJ,NI) */

  if (ni <= 0 || nj <= 0) return;
    
  ic1 = 1;  ia1 = 1;
  for (ii = 1; ii <= ni; ++ii, ic1+=ni, ia1+=jpa) {
    ic = ic1;
    for (kc = 1; kc <= ni; ++kc,ic++) c[ic] = (float)0.;
    ib1 = 1;  ja1 = 1;
    for (jj = 1; jj <= nj; ++jj,++ib1,ja1 += ipa) {
      ib = ib1;  ia = ia1;
      x = (float)0.;
      for (kj = 1;kj <= nj;++kj,ia+=ipa,ib += nj) 
		x += a[ia] * b[ib];	    
      ja = ja1;  ic = ic1;
      for (ki = 1; ki <= ni; ++ki,++ic,ja += jpa) 
		c[ic] += x * a[ja];	    
    }
  }
  return;
} /* mxmlrt_ */

//___________________________________________________________________________
void RMath::mxmlrt_0_(int n__, double *a, double *b, double *c, int ni,int nj)
{
  /* Local variables */
  double x;
  int ia, ib, ic, ja, kc, ii, jj, kj, ki, ia1, ib1, ic1, ja1;

    /* Parameter adjustments to use indeces from "1" */
  --a;  --b;  --c;

  /* Function Body */
  int ipa = 1;  int jpa = nj;
  if (n__ == 1) { ipa = ni;  jpa = 1; }

  if (ni <= 0 || nj <= 0) 	return;
    
  ic1 = 1;  ia1 = 1;
  for (ii = 1; ii <= ni; ++ii, ic1+=ni, ia1+=jpa) {
    ic = ic1;
    for (kc = 1; kc <= ni; ++kc,ic++) c[ic] = (double)0.;
    ib1 = 1; ja1 = 1;
    for (jj = 1; jj <= nj; ++jj,++ib1,ja1 += ipa) {
      ib = ib1;  ia = ia1;
      x = (double)0.;
      for (kj = 1;kj <= nj;++kj,ia+=ipa,ib += nj) 
		x += a[ia] * b[ib];	    
      ja = ja1; ic = ic1;
      for (ki = 1; ki <= ni; ++ki,++ic,ja += jpa) 
		c[ic] += x * a[ja];	    
    }
  }
  return;
} /* mxmlrt_ */


//___________________________________________________________________________
void RMath::mxtrp(float *a, float *b, int i, int j)
{
// CERN PROGLIB# F110    MXTRP           .VERSION KERNFOR  1.0   650809 
// ORIG. 01/01/64 RKB 

  /* Parameter adjustments */
  --b;  --a;
  if (i == 0 || j == 0) return;

  int ib = 1;
  for (int k = 1; k <= j; ++k) 
  {
    int ia = k;
    for (int l = 1; l <= i; ++l,ia += j,++ib) b[ib] = a[ia];
  }
} /* mxtrp */

//___________________________________________________________________________
void RMath::mxtrp(double *a, double *b, int i, int j)
{
// CERN PROGLIB# F110    MXTRP           .VERSION KERNFOR  1.0   650809 
// ORIG. 01/01/64 RKB 

  /* Parameter adjustments */
  --b;  --a;
  /* Function Body */
  if (i == 0 || j == 0) return;

  int ib = 1;
  for (int k = 1; k <= j; ++k) 
  {
    int ia = k;
    for (int l = 1; l <= i; ++l,ia += j,++ib) b[ib] = a[ia];
  }
} /* mxtrp */


//___________________________________________________________________________
//___________________________________________________________________________
//
//            TRPACK
//___________________________________________________________________________
//___________________________________________________________________________

//____________________________________________________________
float *RMath::traat(float *a, float *s, int m, int n)
{
//
// CERN PROGLIB# F112    TRAAT           .VERSION KERNFOR  4.15  861204 
// ORIG. 18/12/74 WH */
// traat.F -- translated by f2c (version 19970219).
//
   float *res = 0;
   /* Local variables */
   int ipiv, i, j, ipivn, ia, is, iat;
   double sum;

   // Parameter adjustments 
   --s;    --a;

   // Function Body 
   ia = 0;   is = 0;

   for (i = 1; i <= m; ++i) {
     ipiv = ia;
     ipivn = ipiv + n;
     iat = 0;
     for (j = 1; j <= i; ++j) {
       ia = ipiv;
       sum = (float)0.;
       do {
          ++ia;  ++iat;
           sum += a[ia] * a[iat]; }
       while (ia < ipivn);
       ++is;
       s[is] = sum;
     }
   }
   res  = s;
   return res;
} /* traat_ */

//____________________________________________________________
float *RMath::tral(float *a, float *u, float *b, int m, int n)
{
  // tral.F -- translated by f2c (version 19970219).

    /* Local variables */
     int indu, i, j, k, ia, ib, iu;
     double sum;


/* CERN PROGLIB# F112    TRAL            .VERSION KERNFOR  4.15  861204 */
/* ORIG. 18/12/74 WH */


    /* Parameter adjustments */
    --b;    --u;    --a;

    /* Function Body */
    ib = 1;

    for (i = 1; i <= m; ++i) {
	indu = 0;

	for (j = 1; j <= n; ++j) {
	    indu += j;
	    ia = ib;
	    iu = indu;
	    sum = 0.;

	    for (k = j; k <= n; ++k) {
		sum += a[ia] * u[iu];
		++ia;
/* L30: */
		iu += k;
	    }
	    b[ib] = sum;
/* L40: */
	    ++ib;
	}
    }

    return 0;
} /* tral_ */

//____________________________________________________________
// tralt.F -- translated by f2c (version 19970219).
float *RMath::tralt(float *a, float *u, float *b, int m, int n)
{
    /* System generated locals */

    /* Local variables */
     int indu, j, k, ia, ib, iu;
     double sum;

/* CERN PROGLIB# F112    TRALT           .VERSION KERNFOR  4.15  861204 */
/* ORIG. 18/12/74 WH */

    /* Parameter adjustments */
    --b;    --u;    --a;

    /* Function Body */
    ib = m * n;
    indu = (n * n + n) / 2;

    do {
      iu = indu;
      for (j = 1; j <= n; ++j) {
 	ia = ib;
	sum = (float)0.;

	for (k = j; k <= n; ++k) {
          sum += a[ia] * u[iu];
          --ia;
          --iu;
	}
	b[ib] = sum;
	--ib;
     } 
    } while (ib > 0);

    ++b; // restore b;
    return b;
} /* tralt_ */

//____________________________________________________________
// tras.F -- translated by f2c (version 19970219).

float *RMath::tras(float *a, float *s, float *b, int m, int n)
{
    /* Local variables */
     int inds, i__, j, k, ia, ib, is;
     double sum;


/* CERN PROGLIB# F112    TRAS            .VERSION KERNFOR  4.15  861204 */
/* ORIG. 18/12/74 WH */


    /* Parameter adjustments */
    --b;    --s;    --a;

    /* Function Body */
    ib = 0;
    inds = 0;
    i__ = 0;

    do {
      inds += i__;
      ia = 0;
      ib = i__ + 1;

      for (j = 1; j <= m; ++j) {
	is = inds;
	sum = 0.;
	k = 0;

L10:
	if (k > i__) 	    goto L20;
	++is;
	goto L30;
L20:
	is += k;
L30:
	++ia;
	sum += a[ia] * s[is];
	++k;
	if (k < n) 	    goto L10;

	b[ib] = sum;
/* L40: */
	ib += n;
      }
      ++i__;
      } while (i__ < n);

    ++b;
    return b;
} /* tras_ */

//____________________________________________________________
// trasat.F -- translated by f2c (version 19970219).

float *RMath::trasat(float *a, float *s, float *r__, int m, int n)
{
     int imax, i__, k;
     int ia, mn, ir, is, iaa, ind;
     double sum;

/* CERN PROGLIB# F112    TRASAT          .VERSION KERNFOR  4.15  861204 */
/* ORIG. 18/12/74 WH */

    /* Parameter adjustments */
    --r__;    --s;    --a;

    /* Function Body */
    imax = (m * m + m) / 2;
    vzero(&r__[1], imax);
    mn = m * n;
    ind = 0;
    i__ = 0;

    do {
      ind += i__;
      ia = 0;
      ir = 0;

      do {
        is = ind;
        sum = 0.;
        k = 0;

        do {
          if (k > i__) is += k;
          else         ++is;
          ++ia;
          sum += s[is] * a[ia];
          ++k;
        } while (k < n);
        iaa = i__ + 1;
        do {
          ++ir;
          r__[ir] += sum * a[iaa];
          iaa += n;
        } while (iaa <= ia);
      } while (ia < mn);

      ++i__;
    } while (i__ < n);

    return 0;
} /* trasat_ */

//____________________________________________________________
float *RMath::trata(float *a, float *r__, int m, int n)
{
// trata.F -- translated by f2c (version 19970219).
// CERN PROGLIB# F112    TRATA           .VERSION KERNFOR  4.15  861204 */
// ORIG. 18/12/74 WH */

    /* Local variables */
     int i__, j, ia, mn, ir, iat;
     double sum;

    /* Parameter adjustments */
    --r__;    --a;

    /* Function Body */
    mn = m * n;
    ir = 0;

    for (i__ = 1; i__ <= m; ++i__) {

	for (j = 1; j <= i__; ++j) {
	    ia = i__;
	    iat = j;

	    sum = (float)0.;
            do {
	      sum += a[ia] * a[iat];
	      ia +=  m;
	      iat += m;
	    } while  (ia <= mn);
	    ++ir;
	    r__[ir] = sum;
	}
    }

    return 0;
} /* trata_ */

//____________________________________________________________
// trats.F -- translated by f2c (version 19970219).
float *RMath::trats(float *a, float *s, float *b, int m, int n)
{
    /* Local variables */
     int inds, i__, j, k, ia, ib, is;
     double sum;

/* CERN PROGLIB# F112    TRATS           .VERSION KERNFOR  4.15  861204 */
/* ORIG. 18/12/74 WH */

    /* Parameter adjustments */
    --b;    --s;    --a;

    /* Function Body */
    ib = 0;    inds = 0;    i__ = 0;

    do {
      inds += i__;
      ib = i__ + 1;

      for (j = 1; j <= m; ++j) {
	ia = j;
	is = inds;
	sum = (float)0.;
	k = 0;

        do {
	  if (k > i__) is += k;
          else         ++is;
	  sum += a[ia] * s[is];
	  ia += m;
	  ++k;
	} while (k < n);

	b[ib] = sum;
	ib += n;
      }
      ++i__;
    } while (i__ < n);

    return 0;
} /* trats_ */

//____________________________________________________________
// tratsa.F -- translated by f2c (version 19970219).
/* Subroutine */float *RMath::tratsa(float *a, float *s, float *r__, int m, int n)
{

    /* Local variables */
     int imax, i__, j, k;
     int ia, mn, ir, is, iaa, ind;
     double sum;

/* CERN PROGLIB# F112    TRATSA          .VERSION KERNFOR  4.15  861204 */
/* ORIG. 18/12/74 WH */


    /* Parameter adjustments */
    --r__;    --s;    --a;

    /* Function Body */
    imax = (m * m + m) / 2;
    vzero(&r__[1], imax);
    mn = m * n;
    ind = 0;
    i__ = 0;

    do {
      ind += i__;
      ir = 0;

      for (j = 1; j <= m; ++j) {
	is = ind;
	ia = j;
	sum = (float)0.;
	k = 0;

       do {
	  if (k > i__) is += k;
	  else         ++is;
	  sum += s[is] * a[ia];
	  ia += m;
	  ++k;
	} while  (k < n);
	iaa = i__ * m;

	for (k = 1; k <= j; ++k) {
	    ++iaa;
	    ++ir;
	    r__[ir] += sum * a[iaa];
	}
      }
      ++i__;
    } while (i__ < n);

    return 0;
} /* tratsa_ */

//____________________________________________________________
// trchlu.F -- translated by f2c (version 19970219).
float *RMath::trchlu(float *a, float *b, int n)
{
    /* Local variables */
     int ipiv, kpiv, i__, j;
     double r__, dc;
     int id, kd;
     double sum;


/* CERN PROGLIB# F112    TRCHLU          .VERSION KERNFOR  4.16  870601 */
/* ORIG. 18/12/74 W.HART */


    /* Parameter adjustments */
    --b;    --a;

    /* Function Body */
    ipiv = 0;

    i__ = 0;

    do {
      ++i__;
      ipiv += i__;
      kpiv = ipiv;
      r__ = a[ipiv];

      for (j = i__; j <= n; ++j) {
	sum = 0.;
	if (i__ == 1) 	    goto L40;
	if (r__ == 0.)      goto L42;
	id = ipiv - i__ + 1;
	kd = kpiv - i__ + 1;

        do {
	  sum += b[kd] * b[id];
	  ++kd;	  ++id;
	} while (id < ipiv);

L40:
	sum = a[kpiv] - sum;
L42:
	if (j != i__) b[kpiv] = sum * r__;
        else {
	  dc = TMath::Sqrt(sum);
	  b[kpiv] = dc;
	  if (r__ > 0.)  r__ = (float)1. / dc;
        }
	kpiv += j;
      }

    } while  (i__ < n);

    return 0;
} /* trchlu_ */

//____________________________________________________________
// trchul.F -- translated by f2c (version 19970219).
/* Subroutine */float *RMath::trchul(float *a, float *b, int n)
{
    /* Local variables */
     int ipiv, kpiv, i__;
     double r__;
     int nstep;
     double dc;
     int id, kd;
     double sum;


/* CERN PROGLIB# F112    TRCHUL          .VERSION KERNFOR  4.16  870601 */
/* ORIG. 18/12/74 WH */


    /* Parameter adjustments */
    --b;    --a;

    /* Function Body */
    kpiv = (n * n + n) / 2;

    i__ = n;
    do {
      ipiv = kpiv;
      r__ = a[ipiv];

      do {
        sum = 0.;
        if (i__ == n) 	        goto L40;
        if (r__ == (float)0.) 	goto L42;
        id = ipiv;
        kd = kpiv;
        nstep = i__;

        do {
          kd += nstep;
          id += nstep;
          ++nstep;
          sum += b[id] * b[kd];
        } while  (nstep < n);

L40:
        sum = a[kpiv] - sum;
L42:
        if (kpiv < ipiv) b[kpiv] = sum * r__;
        else {
          dc = TMath::Sqrt(sum);
          b[kpiv] = dc;
          if (r__ > (float)0.) 	r__ = (float)1. / dc;
        }
        --kpiv;
      } while (kpiv > ipiv - i__);

      --i__;
    } while  (i__ > 0);

    return 0;
} /* trchul_ */

//____________________________________________________________
/* Subroutine */float *RMath::trinv(float *t, float *s, int n)
{
// trinv.F -- translated by f2c (version 19970219).
// CERN PROGLIB# F112    TRINV           .VERSION KERNFOR  4.15  861204 */
// ORIG. 18/12/74 WH */

     int lhor, ipiv, lver, j;
     double sum = 0;
     double r__ = 0;
     int mx, ndstep, ind;


    /* Parameter adjustments */
    --s;    --t;

    /* Function Body */
    mx = (n * n + n) / 2;
    ipiv = mx;

    int i = n;
    do {
      r__ = 0.;
      if (t[ipiv] > 0.) r__ = 1. / t[ipiv];
      s[ipiv] = r__;
      ndstep = n;
      ind = mx - n + i;

      while (ind != ipiv) {
        sum = 0.;
        if (r__ != 0.) {
          lhor = ipiv;
          lver = ind;
          j = i;

          do {
            lhor += j;
            ++lver;
            sum += t[lhor] * s[lver];
            ++j;
          } while  (lhor < ind);
        }
        s[ind] = -sum * r__;
        --ndstep;
        ind -= ndstep;
      }

      ipiv -= i;
      --i;
    } while (i > 0);

    return 0;
} /* trinv_ */

//____________________________________________________________
// trla.F -- translated by f2c (version 19970219).
/* Subroutine */float *RMath::trla(float *u, float *a, float *b, int m, int n)
{
     int ipiv, ia, ib, iu;
     double sum;

/* CERN PROGLIB# F112    TRLA            .VERSION KERNFOR  4.15  861204 */
/* ORIG. 18/12/74 WH */


    /* Parameter adjustments */
    --b;    --a;    --u;

    /* Function Body */
    ib = m * n;
    ipiv = (m * m + m) / 2;

    do {
      do {
        ia = ib;
        iu = ipiv;
  
        sum = (float)0.;
        do {
          sum += a[ia] * u[iu];
          --iu;
          ia -= n;
        } while (ia > 0);
  
        b[ib] = sum;
        --ib;
      } while (ia > 1 - n);
  
      ipiv = iu;
    } while (iu > 0);

    return 0;
} /* trla_ */

//____________________________________________________________
/* trlta.F -- translated by f2c (version 19970219).
// Subroutine */float *RMath::trlta(float *u, float *a, float *b, int m, int n)
{
     int ipiv, mxpn, i__, nstep, ia, ib, iu, mx;
     double sum;

/* CERN PROGLIB# F112    TRLTA           .VERSION KERNFOR  4.15  861204 */
/* ORIG. 18/12/74 WH */


    /* Parameter adjustments */
    --b;    --a;    --u;

    /* Function Body */
    ipiv = 0;
    mx = m * n;
    mxpn = mx + n;
    ib = 0;

    i__ = 0;
    do {
      ++i__;
      ipiv += i__;
  
      do {
        iu = ipiv;
        nstep = i__;
        ++ib;
        ia = ib;
    
        sum = 0.;
        do {
          sum += a[ia] * u[iu];
          ia += n;
          iu += nstep;
          ++nstep;
        } while (ia <= mx);
    
        b[ib] = sum;
      } while (ia < mxpn);
  
    } while (i__ < m);

    return 0;
} /* trlta_ */

//____________________________________________________________
/* Subroutine */float *RMath::trpck(float *s, float *u, int n)
{
 // trpck.F -- translated by f2c (version 19970219).
 // CERN PROGLIB# F112    TRPCK           .VERSION KERNFOR  2.08  741218 */
 // ORIG. 18/12/74 WH */
  int i__, ia, ind, ipiv;
 
   /* Parameter adjustments */
    --u;    --s;

    /* Function Body */
    ia = 0;
    ind = 0;
    ipiv = 0;

    for (i__ = 1; i__ <= n; ++i__) {
      ipiv += i__;
      do {
        ++ia;
        ++ind;
        u[ind] = s[ia];
      } while (ind < ipiv);
      ia = ia + n - i__;
    }

    return 0;
} /* trpck_ */

//____________________________________________________________
float *RMath::trqsq(float *q, float *s, float *r__, int m)
{
// trqsq.F -- translated by f2c (version 19970219).
// CERN PROGLIB# F112    TRQSQ           .VERSION KERNFOR  4.15  861204 */
// ORIG. 18/12/74 WH */

     int indq, inds, imax, i__, j, k, l;
     int iq, ir, is, iqq;
     double sum;

    /* Parameter adjustments */
    --r__;    --s;    --q;

    /* Function Body */
    imax = (m * m + m) / 2;
    vzero(&r__[1], imax);
    inds = 0;
    i__ = 0;

    do {
      inds += i__;
      ir = 0;
      indq = 0;
      j = 0;
  
      do {
        indq += j;
        is = inds;
        iq = indq;
        sum = (float)0.;
        k = 0;
    
        do {
          if (k > i__) 	is += k;
          else          ++is;
      
          if (k > j) 	iq += k;
          else        ++iq;
      
          sum += s[is] * q[iq];
          ++k;
        } while (k < m);
        iqq = inds;
        l = 0;
    
        do {
          ++ir;
          if (l > i__) 	iqq += l;
          else          ++iqq;
          r__[ir] += q[iqq] * sum;
          ++l;
        } while (l <= j);
        ++j;
      } while (j < m);
      ++i__;
    } while (i__ < m);

    return 0;
} /* trqsq_ */

//____________________________________________________________
float *RMath::trsa(float *s, float *a, float *b, int m, int n)
{
// trsa.F -- translated by f2c (version 19970219).
// CERN PROGLIB# F112    TRSA            .VERSION KERNFOR  4.15  861204 */
// ORIG. 18/12/74 WH */
    /* Local variables */
     int inds, i__, j, k, ia, ib, is;
     double sum;

  /* Parameter adjustments */
    --b;    --a;    --s;

    /* Function Body */
    inds = 0;
    ib = 0;
    i__ = 0;

    do {
      inds += i__;
  
      for (j = 1; j <= n; ++j) {
  	    ia = j;
  	    is = inds;
  	    sum = (float)0.;
  	    k = 0;
  
        do {
  	      if (k > i__) is += k;
    	    else         ++is;
        	sum += s[is] * a[ia];
    	    ia += n;
    	    ++k;
  	    } while (k < m);
      	++ib;
  	    b[ib] = sum;
      }
      ++i__;
    } while (i__ < m);

    return 0;
} /* trsa_ */

//____________________________________________________________
/* Subroutine */float *RMath::trsinv(float *g, float *gi, int n)
{
// trsinv.F -- translated by f2c (version 19970219).
// CERN PROGLIB# F112    TRSINV          .VERSION KERNFOR  2.08  741218 
// ORIG. 18/12/74 WH */

    /* Function Body */
    trchlu(g, gi, n);
    trinv(gi, gi, n);
    trsmul(gi, gi, n);

    return 0;
} /* trsinv_ */

//____________________________________________________________
/* Subroutine */float *RMath::trsmlu(float *u, float *s, int n)
{
// trsmlu.F -- translated by f2c (version 19970219).
// CERN PROGLIB# F112    TRSMLU          .VERSION KERNFOR  4.15  861204 */
// ORIG. 18/12/74 WH */

    /* Local variables */
     int lhor, lver, i__, k, l, ind;
     double sum;

    /* Parameter adjustments */
    --s;    --u;

    /* Function Body */
    ind = (n * n + n) / 2;

    for (i__ = 1; i__ <= n; ++i__) {
	    lver = ind;

	    for (k = i__; k <= n; ++k,--ind) {
	      lhor = ind;    sum = 0.;
  	    for (l = k; l <= n; ++l,--lver,--lhor) 
      		sum += u[lver] * u[lhor];
	      s[ind] = sum;
    	}
    }

    return 0;
} /* trsmlu_ */

//____________________________________________________________
/* Subroutine */float *RMath::trsmul(float *g, float *gi, int n)
{
  // trsmul.F -- translated by f2c (version 19970219).
  // CERN PROGLIB# F112    TRSMUL          .VERSION KERNFOR  4.15  861204 */
  // ORIG. 18/12/74 WH */

    /* Local variables */
     int lhor, lver, lpiv, i__, j, k, ind;
     double sum;

    /* Parameter adjustments */
    --gi;    --g;

    /* Function Body */
    ind = 1;
    lpiv = 0;
    for (i__ = 1; i__ <= n; ++i__) {
    	lpiv += i__;
    	for (j = 1; j <= i__; ++j,++ind) {
	      lver = lpiv;
	      lhor = ind;
	      sum = 0.;
	      for (k = i__; k <= n; lhor += k,lver += k,++k) 
		      sum += g[lver] * g[lhor];	      
	      gi[ind] = sum;
    	}
    }

    return 0;
} /* trsmul_ */

//____________________________________________________________
/* Subroutine */float *RMath::trupck(float *u, float *s, int m)
{
// trupck.F -- translated by f2c (version 19970219).
// CERN PROGLIB# F112    TRUPCK          .VERSION KERNFOR  2.08  741218 
// ORIG. 18/12/74 WH 


    int i__, im, is, iu, iv, ih, m2;

    /* Parameter adjustments */
    --s;    --u;

    /* Function Body */
    m2 = m * m;
    is = m2;
    iu = (m2 + m) / 2;
    i__ = m - 1;

    do {
      im = i__ * m;
      do {
        s[is] = u[iu];
        --is;
        --iu;
      } while (is > im);
      is = is - m + i__;
      --i__;
    } while (i__ >= 0);

    is = 1;
    do {
      iv = is;
      ih = is;
      while (1) {
        iv += m;
        ++ih;
        if (iv > m2) 	break;
        s[ih] = s[iv];
      }
      is = is + m + 1;
    } while (is < m2);

    return 0;
} /* trupck_ */

//____________________________________________________________
/* trsat.F -- translated by f2c (version 19970219).
// Subroutine */ float *RMath::trsat(float *s, float *a, float *b, int m, int n)
{

    /* Local variables */
     int inds, i__, j, k, ia, ib, mn, is;
     double sum;


/* CERN PROGLIB# F112    TRSAT           .VERSION KERNFOR  4.15  861204 */
/* ORIG. 18/12/74 WH */


    /* Parameter adjustments */
    --b;    --a;    --s;

    /* Function Body */
    mn = m * n;
    inds = 0;
    ib = 0;
    i__ = 0;

    do {
      inds += i__;
      ia = 0;
  
      for (j = 1; j <= n; ++j) {
      	is = inds;
      	sum = 0.;
      	k = 0;
  
        do {
          if (k > i__) is += k;
          else         ++is;
          ++ia;
          sum += s[is] * a[ia];
          ++k;
      	} while (k < m);
      	++ib;
      	b[ib] = sum;
      }
      ++i__;
    } while (i__ < m);

    return 0;
} /* trsat_ */

// ------  double 

//____________________________________________________________
double *RMath::traat(double *a, double *s, int m, int n)
{
//
// CERN PROGLIB# F112    TRAAT           .VERSION KERNFOR  4.15  861204 
// ORIG. 18/12/74 WH */
// traat.F -- translated by f2c (version 19970219).
//
   double *res = 0;
   /* Local variables */
   int ipiv, i, j, ipivn, ia, is, iat;
   double sum;

   // Parameter adjustments 
   --s;    --a;

   // Function Body 
   ia = 0;   is = 0;

   for (i = 1; i <= m; ++i) {
     ipiv = ia;
     ipivn = ipiv + n;
     iat = 0;
     for (j = 1; j <= i; ++j) {
       ia = ipiv;
       sum = (double)0.;
       do {
          ++ia;  ++iat;
           sum += a[ia] * a[iat]; }
       while (ia < ipivn);
       ++is;
       s[is] = sum;
     }
   }
   res  = s;
   return res;
} /* traat_ */

//____________________________________________________________
double *RMath::tral(double *a, double *u, double *b, int m, int n)
{
  // tral.F -- translated by f2c (version 19970219).

    /* Local variables */
     int indu, i, j, k, ia, ib, iu;
     double sum;


/* CERN PROGLIB# F112    TRAL            .VERSION KERNFOR  4.15  861204 */
/* ORIG. 18/12/74 WH */


    /* Parameter adjustments */
    --b;    --u;    --a;

    /* Function Body */
    ib = 1;

    for (i = 1; i <= m; ++i) {
	indu = 0;

	for (j = 1; j <= n; ++j) {
	    indu += j;
	    ia = ib;
	    iu = indu;
	    sum = (double)0.;

	    for (k = j; k <= n; ++k) {
		sum += a[ia] * u[iu];
		++ia;
		iu += k;
	    }
	    b[ib] = sum;
	    ++ib;
	}
    }

    return 0;
} /* tral_ */

//____________________________________________________________
// tralt.F -- translated by f2c (version 19970219).
double *RMath::tralt(double *a, double *u, double *b, int m, int n)
{
    /* Local variables */
     int indu, j, k, ia, ib, iu;
     double sum;

/* CERN PROGLIB# F112    TRALT           .VERSION KERNFOR  4.15  861204 */
/* ORIG. 18/12/74 WH */

    /* Parameter adjustments */
    --b;    --u;    --a;

    /* Function Body */
    ib = m * n;
    indu = (n * n + n) / 2;

    do {
      iu = indu;
      for (j = 1; j <= n; ++j) {
 	ia = ib;
	sum = (double)0.;

	for (k = j; k <= n; ++k) {
          sum += a[ia] * u[iu];
          --ia;
          --iu;
	}
	b[ib] = sum;
	--ib;
     } 
    } while (ib > 0);

    ++b; // restore b;
    return b;
} /* tralt_ */

//____________________________________________________________
// tras.F -- translated by f2c (version 19970219).

double *RMath::tras(double *a, double *s, double *b, int m, int n)
{

    /* Local variables */
     int inds, i__, j, k, ia, ib, is;
     double sum;


/* CERN PROGLIB# F112    TRAS            .VERSION KERNFOR  4.15  861204 */
/* ORIG. 18/12/74 WH */


    /* Parameter adjustments */
    --b;    --s;    --a;

    /* Function Body */
    ib = 0;
    inds = 0;
    i__ = 0;

    do {
      inds += i__;
      ia = 0;
      ib = i__ + 1;

      for (j = 1; j <= m; ++j) {
	is = inds;
	sum = 0.;
	k = 0;

L10:
	if (k > i__) 	    goto L20;
	++is;
	goto L30;
L20:
	is += k;
L30:
	++ia;
	sum += a[ia] * s[is];
	++k;
	if (k < n) 	    goto L10;

	b[ib] = sum;
/* L40: */
	ib += n;
      }
      ++i__;
      } while (i__ < n);

    ++b;
    return b;
} /* tras_ */

//____________________________________________________________
// trasat.F -- translated by f2c (version 19970219).

double *RMath::trasat(double *a, double *s, double *r__, int m, int n)
{
     int imax, i__, k;
     int ia, mn, ir, is, iaa, ind;
     double sum;

/* CERN PROGLIB# F112    TRASAT          .VERSION KERNFOR  4.15  861204 */
/* ORIG. 18/12/74 WH */

    /* Parameter adjustments */
    --r__;    --s;    --a;

    /* Function Body */
    imax = (m * m + m) / 2;
    vzero(&r__[1], imax);
    mn = m * n;
    ind = 0;
    i__ = 0;

    do {
      ind += i__;
      ia = 0;
      ir = 0;

      do {
        is = ind;
        sum = 0.;
        k = 0;

        do {
          if (k > i__) is += k;
          else         ++is;
          ++ia;
          sum += s[is] * a[ia];
          ++k;
        } while (k < n);
        iaa = i__ + 1;
        do {
          ++ir;
          r__[ir] += sum * a[iaa];
          iaa += n;
        } while (iaa <= ia);
      } while (ia < mn);

      ++i__;
    } while (i__ < n);

    return 0;
} /* trasat_ */

//____________________________________________________________
// trata.F -- translated by f2c (version 19970219).
double *RMath::trata(double *a, double *r__, int m, int n)
{

    /* Local variables */
     int i__, j, ia, mn, ir, iat;
     double sum;


/* CERN PROGLIB# F112    TRATA           .VERSION KERNFOR  4.15  861204 */
/* ORIG. 18/12/74 WH */


    /* Parameter adjustments */
    --r__;    --a;

    /* Function Body */
    mn = m * n;
    ir = 0;

    for (i__ = 1; i__ <= m; ++i__) {

	for (j = 1; j <= i__; ++j) {
	    ia = i__;
	    iat = j;

	    sum = (double)0.;
            do {
	      sum += a[ia] * a[iat];
	      ia +=  m;
	      iat += m;
	    } while  (ia <= mn);
	    ++ir;
	    r__[ir] = sum;
	}
    }

    return 0;
} /* trata_ */

//____________________________________________________________
// trats.F -- translated by f2c (version 19970219).
double *RMath::trats(double *a, double *s, double *b, int m, int n)
{
    /* Local variables */
     int inds, i__, j, k, ia, ib, is;
     double sum;


/* CERN PROGLIB# F112    TRATS           .VERSION KERNFOR  4.15  861204 */
/* ORIG. 18/12/74 WH */

    /* Parameter adjustments */
    --b;    --s;    --a;

    /* Function Body */
    ib = 0;    inds = 0;    i__ = 0;

    do {
      inds += i__;
      ib = i__ + 1;

      for (j = 1; j <= m; ++j) {
	ia = j;
	is = inds;
	sum = (double)0.;
	k = 0;

        do {
	  if (k > i__) is += k;
          else         ++is;
	  sum += a[ia] * s[is];
	  ia += m;
	  ++k;
	} while (k < n);

	b[ib] = sum;
	ib += n;
      }
      ++i__;
    } while (i__ < n);

    return 0;
} /* trats_ */

//____________________________________________________________
// tratsa.F -- translated by f2c (version 19970219).
/* Subroutine */double *RMath::tratsa(double *a, double *s, double *r__, int m, int n)
{
    /* Local variables */
     int imax, i__, j, k;
     int ia, mn, ir, is, iaa, ind;
     double sum;

/* CERN PROGLIB# F112    TRATSA          .VERSION KERNFOR  4.15  861204 */
/* ORIG. 18/12/74 WH */


    /* Parameter adjustments */
    --r__;    --s;    --a;

    /* Function Body */
    imax = (m * m + m) / 2;
    vzero(&r__[1], imax);
    mn = m * n;
    ind = 0;
    i__ = 0;

    do {
      ind += i__;
      ir = 0;

      for (j = 1; j <= m; ++j) {
	is = ind;
	ia = j;
	sum = (double)0.;
	k = 0;

       do {
	  if (k > i__) is += k;
	  else         ++is;
	  sum += s[is] * a[ia];
	  ia += m;
	  ++k;
	} while  (k < n);
	iaa = i__ * m;

	for (k = 1; k <= j; ++k) {
	    ++iaa;
	    ++ir;
	    r__[ir] += sum * a[iaa];
	}
      }
      ++i__;
    } while (i__ < n);

    return 0;
} /* tratsa_ */

//____________________________________________________________
double *RMath::trchlu(double *a, double *b, int n)
{
// trchlu.F -- translated by f2c (version 19970219).
    /* Local variables */
     int ipiv, kpiv, i__, j;
     double r__, dc;
     int id, kd;
     double sum;


/* CERN PROGLIB# F112    TRCHLU          .VERSION KERNFOR  4.16  870601 */
/* ORIG. 18/12/74 W.HART */


    /* Parameter adjustments */
    --b;    --a;

    /* Function Body */
    ipiv = 0;

    i__ = 0;

    do {
      ++i__;
      ipiv += i__;
      kpiv = ipiv;
      r__ = a[ipiv];

      for (j = i__; j <= n; ++j) {
	sum = 0.;
	if (i__ == 1) 	    goto L40;
	if (r__ == 0.)      goto L42;
	id = ipiv - i__ + 1;
	kd = kpiv - i__ + 1;

        do {
	  sum += b[kd] * b[id];
	  ++kd;	  ++id;
	} while (id < ipiv);

L40:
	sum = a[kpiv] - sum;
L42:
	if (j != i__) b[kpiv] = sum * r__;
        else {
	  dc = TMath::Sqrt(sum);
	  b[kpiv] = dc;
	  if (r__ > 0.)  r__ = (double)1. / dc;
        }
	kpiv += j;
      }

    } while  (i__ < n);

    return 0;
} /* trchlu_ */

//____________________________________________________________
// trchul.F -- translated by f2c (version 19970219).
double *RMath::trchul(double *a, double *b, int n)
{
    /* Local variables */
     int ipiv, kpiv, i__;
     double r__;
     int nstep;
     double dc;
     int id, kd;
     double sum;


/* CERN PROGLIB# F112    TRCHUL          .VERSION KERNFOR  4.16  870601 */
/* ORIG. 18/12/74 WH */


    /* Parameter adjustments */
    --b;    --a;

    /* Function Body */
    kpiv = (n * n + n) / 2;

    i__ = n;
    do {
      ipiv = kpiv;
      r__ = a[ipiv];

      do {
        sum = 0.;
        if (i__ == n) 	        goto L40;
        if (r__ == (double)0.) 	goto L42;
        id = ipiv;
        kd = kpiv;
        nstep = i__;

        do {
          kd += nstep;
          id += nstep;
          ++nstep;
          sum += b[id] * b[kd];
        } while  (nstep < n);

L40:
        sum = a[kpiv] - sum;
L42:
        if (kpiv < ipiv) b[kpiv] = sum * r__;
        else {
          dc = TMath::Sqrt(sum);
          b[kpiv] = dc;
          if (r__ > (double)0.) 	r__ = (double)1. / dc;
        }
        --kpiv;
      } while (kpiv > ipiv - i__);

      --i__;
    } while  (i__ > 0);

    return 0;
} /* trchul_ */

//____________________________________________________________
double *RMath::trinv(double *t, double *s, int n)
{
// trinv.F -- translated by f2c (version 19970219).
// CERN PROGLIB# F112    TRINV           .VERSION KERNFOR  4.15  861204 */
// ORIG. 18/12/74 WH */
//
     int lhor, ipiv, lver,  j;
     double r__;
     int mx, ndstep, ind;
     double sum;

    /* Parameter adjustments */
    --s;    --t;

    /* Function Body */
    mx = (n * n + n) / 2;
    ipiv = mx;

    int i = n;
    do {
      r__ = 0.;
      if (t[ipiv] > 0.)  r__ = (double)1. / t[ipiv];
      s[ipiv] = r__;
      ndstep = n;
      ind = mx - n + i;

      while (ind != ipiv) {
        sum = 0.;
        if (r__ != 0.) {
          lhor = ipiv;
          lver = ind;
          j = i;

          do {
            lhor += j;
            ++lver;
            sum += t[lhor] * s[lver];
            ++j;
          } while  (lhor < ind);
        }
        s[ind] = -sum * r__;
        --ndstep;
        ind -= ndstep;
      }

      ipiv -= i;
      --i;
    } while (i > 0);

    return 0;
} /* trinv_ */

//____________________________________________________________
/* Subroutine */double *RMath::trla(double *u, double *a, double *b, int m, int n)
{
//
// trla.F -- translated by f2c (version 19970219).
// CERN PROGLIB# F112    TRLA            .VERSION KERNFOR  4.15  861204 */
// ORIG. 18/12/74 WH */
//
     int ipiv, ia, ib, iu;
     double sum;

  /* Parameter adjustments */
    --b;    --a;    --u;

    /* Function Body */
    ib = m * n;
    ipiv = (m * m + m) / 2;

    do {
      do {
        ia = ib;
        iu = ipiv;
  
        sum = 0.;
        do {
          sum += a[ia] * u[iu];
          --iu;
          ia -= n;
        } while (ia > 0);
  
        b[ib] = sum;
        --ib;
      } while (ia > 1 - n);
  
      ipiv = iu;
    } while (iu > 0);

    return 0;
} /* trla_ */

//____________________________________________________________
double *RMath::trlta(double *u, double *a, double *b, int m, int n)
{
// trlta.F -- translated by f2c (version 19970219).
// CERN PROGLIB# F112    TRLTA           .VERSION KERNFOR  4.15  861204 
// ORIG. 18/12/74 WH 

     int ipiv, mxpn, i__, nstep, ia, ib, iu, mx;
     double sum;

    /* Parameter adjustments */
    --b;    --a;    --u;

    /* Function Body */
    ipiv = 0;
    mx = m * n;
    mxpn = mx + n;
    ib = 0;

    i__ = 0;
    do {
      ++i__;
      ipiv += i__;
  
      do {
        iu = ipiv;
        nstep = i__;
        ++ib;
        ia = ib;
    
        sum = 0.;
        do {
          sum += a[ia] * u[iu];
          ia += n;
          iu += nstep;
          ++nstep;
        } while (ia <= mx);
    
        b[ib] = sum;
      } while (ia < mxpn);
  
    } while (i__ < m);

    return 0;
} /* trlta_ */

//____________________________________________________________
/* Subroutine */double *RMath::trpck(double *s, double *u, int n)
{
 // trpck.F -- translated by f2c (version 19970219).
 // CERN PROGLIB# F112    TRPCK           .VERSION KERNFOR  2.08  741218 */
 // ORIG. 18/12/74 WH */
  int i__, ia, ind, ipiv;
 
   /* Parameter adjustments */
    --u;    --s;

    /* Function Body */
    ia = 0;
    ind = 0;
    ipiv = 0;

    for (i__ = 1; i__ <= n; ++i__) {
      ipiv += i__;
      do {
        ++ia;
        ++ind;
        u[ind] = s[ia];
    	} while (ind < ipiv);
  	  ia = ia + n - i__;
    }

    return 0;
} /* trpck_ */

//____________________________________________________________
double *RMath::trqsq(double *q, double *s, double *r__, int m)
{
// trqsq.F -- translated by f2c (version 19970219).
// CERN PROGLIB# F112    TRQSQ           .VERSION KERNFOR  4.15  861204 */
// ORIG. 18/12/74 WH */

     int indq, inds, imax, i__, j, k, l;
     int iq, ir, is, iqq;
     double sum;

    /* Parameter adjustments */
    --r__;    --s;    --q;

    /* Function Body */
    imax = (m * m + m) / 2;
    vzero(&r__[1], imax);
    inds = 0;
    i__ = 0;

    do {
      inds += i__;
      ir = 0;
      indq = 0;
      j = 0;
  
      do {
        indq += j;
        is = inds;
        iq = indq;
        sum = 0.;
        k = 0;
    
        do {
          if (k > i__) 	is += k;
          else          ++is;
      
          if (k > j) 	iq += k;
          else        ++iq;
      
          sum += s[is] * q[iq];
          ++k;
        } while (k < m);
        iqq = inds;
        l = 0;
    
        do {
          ++ir;
          if (l > i__) 	iqq += l;
          else          ++iqq;
          r__[ir] += q[iqq] * sum;
          ++l;
        } while (l <= j);
        ++j;
      } while (j < m);
      ++i__;
    } while (i__ < m);

    return 0;
} /* trqsq_ */

//____________________________________________________________
double *RMath::trsa(double *s, double *a, double *b, int m, int n)
{
// trsa.F -- translated by f2c (version 19970219).
// CERN PROGLIB# F112    TRSA            .VERSION KERNFOR  4.15  861204 */
// ORIG. 18/12/74 WH */
    /* Local variables */
     int inds, i__, j, k, ia, ib, is;
     double sum;

  /* Parameter adjustments */
    --b;    --a;    --s;

    /* Function Body */
    inds = 0;
    ib = 0;
    i__ = 0;

    do {
      inds += i__;
  
      for (j = 1; j <= n; ++j) {
  	    ia = j;
  	    is = inds;
  	    sum = 0.;
  	    k = 0;
  
        do {
  	      if (k > i__) is += k;
    	    else         ++is;
        	sum += s[is] * a[ia];
    	    ia += n;
    	    ++k;
  	    } while (k < m);
      	++ib;
  	    b[ib] = sum;
      }
      ++i__;
    } while (i__ < m);

    return 0;
} /* trsa_ */

//____________________________________________________________
/* Subroutine */double *RMath::trsinv(double *g, double *gi, int n)
{
// trsinv.F -- translated by f2c (version 19970219).
// CERN PROGLIB# F112    TRSINV          .VERSION KERNFOR  2.08  741218 
// ORIG. 18/12/74 WH */

    /* Function Body */
    trchlu(g, gi, n);
    trinv(gi, gi, n);
    trsmul(gi, gi, n);

    return 0;
} /* trsinv_ */

//____________________________________________________________
/* Subroutine */double *RMath::trsmlu(double *u, double *s, int n)
{
// trsmlu.F -- translated by f2c (version 19970219).
// CERN PROGLIB# F112    TRSMLU          .VERSION KERNFOR  4.15  861204 */
// ORIG. 18/12/74 WH */

    /* Local variables */
     int lhor, lver, i__, k, l, ind;
     double sum;

    /* Parameter adjustments */
    --s;    --u;

    /* Function Body */
    ind = (n * n + n) / 2;

    for (i__ = 1; i__ <= n; ++i__) {
	    lver = ind;

	    for (k = i__; k <= n; ++k,--ind) {
	      lhor = ind;    sum = 0.;
  	    for (l = k; l <= n; ++l,--lver,--lhor) 
      		sum += u[lver] * u[lhor];
	      s[ind] = sum;
    	}
    }

    return 0;
} /* trsmlu_ */

//____________________________________________________________
/* Subroutine */double *RMath::trsmul(double *g, double *gi, int n)
{
  // trsmul.F -- translated by f2c (version 19970219).
  // CERN PROGLIB# F112    TRSMUL          .VERSION KERNFOR  4.15  861204 */
  // ORIG. 18/12/74 WH */

    /* Local variables */
     int lhor, lver, lpiv, i__, j, k, ind;
     double sum;

    /* Parameter adjustments */
    --gi;    --g;

    /* Function Body */
    ind = 1;
    lpiv = 0;
    for (i__ = 1; i__ <= n; ++i__) {
    	lpiv += i__;
    	for (j = 1; j <= i__; ++j,++ind) {
	      lver = lpiv;
	      lhor = ind;
	      sum = 0.;
	      for (k = i__; k <= n;lhor += k,lver += k,++k) 
		      sum += g[lver] * g[lhor];	      
	      gi[ind] = sum;
    	}
    }

    return 0;
} /* trsmul_ */

//____________________________________________________________
/* Subroutine */double *RMath::trupck(double *u, double *s, int m)
{
// trupck.F -- translated by f2c (version 19970219).
// CERN PROGLIB# F112    TRUPCK          .VERSION KERNFOR  2.08  741218 
// ORIG. 18/12/74 WH 


    int i__, im, is, iu, iv, ih, m2;

    /* Parameter adjustments */
    --s;    --u;

    /* Function Body */
    m2 = m * m;
    is = m2;
    iu = (m2 + m) / 2;
    i__ = m - 1;

    do {
      im = i__ * m;
      do {
        s[is] = u[iu];
        --is;
        --iu;
      } while (is > im);
      is = is - m + i__;
      --i__;
    } while (i__ >= 0);

    is = 1;
    do {
      iv = is;
      ih = is;
      while (1) {
        iv += m;
        ++ih;
        if (iv > m2) 	break;
        s[ih] = s[iv];
      }
      is = is + m + 1;
    } while (is < m2);

    return 0;
} /* trupck_ */

//____________________________________________________________
double *RMath::trsat(double *s, double *a, double *b, int m, int n)
{
// trsat.F -- translated by f2c (version 19970219)
// CERN PROGLIB# F112    TRSAT           .VERSION KERNFOR  4.15  861204 
// ORIG. 18/12/74 WH 

    /* Local variables */
     int inds, i__, j, k, ia, ib, mn, is;
     double sum;

    /* Parameter adjustments */
    --b;    --a;    --s;

    /* Function Body */
    mn = m * n;
    inds = 0;
    ib = 0;
    i__ = 0;

    do {
      inds += i__;
      ia = 0;
  
      for (j = 1; j <= n; ++j) {
      	is = inds;
      	sum = 0.;
      	k = 0;
  
        do {
        	if (k > i__) is += k;
        	else         ++is;
        	++ia;
        	sum += s[is] * a[ia];
        	++k;
      	} while (k < m);
      	++ib;
      	b[ib] = sum;
      }
      ++i__;
    } while (i__ < m);

    return 0;
} /* trsat_ */
