//*-- Author :    Valery Fine(fine@bnl.gov)   25/09/99  
//
// The set of methods to work with the plain matrix / vector
// "derived" from  http://wwwinfo.cern.ch/asdoc/shortwrupsdir/f110/top.html 
// "derived" from  http://wwwinfo.cern.ch/asdoc/shortwrupsdir/f112/top.html 
//
// $Id: StCL.cxx,v 1.2 1999/09/29 00:31:49 fine Exp $
// $Log: StCL.cxx,v $
// Revision 1.2  1999/09/29 00:31:49  fine
// RMath class has been repleaced with StCL one
//
// Revision 1.1  1999/09/28 19:45:10  fine
// RMath class has been renamed to StCL - STAR CERN Library
//
// Revision 1.7  1999/09/27 23:45:42  fine
// Several methods to calculate errors were introduced
//
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
#include "StCL.h"
#include "TMath.h"

// http://wwwinfo.cern.ch/asdoc/shortwrupsdir/f110/top.html 

////////////////////////////////////////////////////////////////////////////////////////////////////////
/* begin_html 
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
<!-- saved from url=(0056)http://wwwinfo.cern.ch/asdoc/shortwrupsdir/f112/top.html -->
<!--Converted with LaTeX2HTML 96.1-e (April 9, 1996) by Nikos Drakos (nikos@cbl.leeds.ac.uk), CBLU, University of Leeds --><HTML><HEAD><TITLE>Manipulation of Triangular and Symmetric Matrices</TITLE>
<META content="text/html; charset=windows-1251" http-equiv=Content-Type>
<META content="Manipulation of Triangular and Symmetric Matrices " 
name=description>
<META content=top name=keywords>
<META content=document name=resource-type>
<META content=global name=distribution>
<META content="MSHTML 5.00.2314.1000" name=GENERATOR></HEAD>
<BODY lang=EN>
<P>
<H2>F112: Manipulation of Triangular and Symmetric Matrices </H2>
<TABLE border=1>
  <TBODY>
  <TR>
    <TD align=left>Author(s): W. Hart 
    <TD align=left>Library: KERNLIB 
  <TR>
    <TD align=left>Submitter: 
    <TD align=left>Submitted: 01.01.1975 
  <TR>
    <TD align=left>Language: Fortran 
    <TD align=left>Revised: 12.12.1986 </TR></TBODY></TABLE><BR><!-- KEY VALUE="operation symmetric triangular matrix " -->
<P>At CERN, matrices are often stored row-wise (TC-convention); furthermore, 
symmetric matrices are stored packed as the lower left triangular part only, 
i.e., the <I>I</I>th diagonal element is found in position 
<I>I</I>(<I>I</I>+1)/2. The <TT>TR</TT>-package performs many of the frequently 
required operations associated with such matrices without resorting to expanding 
into the unpacked square form. In all the following routines an <IMG 
align=middle alt=tex2html_wrap_inline295 height=20 src="gif/trpack_files/img1.gif" 
width=52> <I>symmetric</I> matrix is taken to be stored in the packed form with 
<I>M</I>(<I>M</I>+1)/2 elements. 
<P>Some of these operations produce and require the manipulation of <I>lower 
triangular</I> matrices which have all elements zero above the leading diagonal. 
These are also stored in the packed form with all the zeros dropped; therefore, 
care has to be taken in the interpretation of a packed matrix as to whether it 
represents a symmetric or lower triangular array. To facilitate this distinction 
in the Write-up, the following nomenclature has been adopted: 
<DL compact>
  <DT>A,B,C 
  <DD>unpacked rectangular matrices (row-wise storage) 
  <DT>Q,R,S,T 
  <DD>packed symmetric matrices 
  <DT>V,W 
  <DD>packed lower triangular matrices </DD></DL>On 32-bit machines the 
calculations are performed internally in double-precision mode. 
<P>
<P><B>Structure:</B>
<P><TT>SUBROUTINE</TT> subprograms <BR>User Entry Names: 
<TABLE cols=8>
  <COLGROUP>
  <COL align=left>
  <COL align=left>
  <COL align=left>
  <COL align=left>
  <COL align=left>
  <COL align=left>
  <COL align=left>
  <COL align=left>
  <TBODY>
  <TR>
    <TD align=left noWrap vAlign=baseline><A name=TRCHUL><TT>TRCHUL</TT></A>, 
    </TD>
    <TD align=left noWrap vAlign=baseline><A name=TRCHLU><TT>TRCHLU</TT></A>, 
    </TD>
    <TD align=left noWrap vAlign=baseline><A name=TRSMUL><TT>TRSMUL</TT></A>, 
    </TD>
    <TD align=left noWrap vAlign=baseline><A name=TRSMLU><TT>TRSMLU</TT></A>, 
    </TD>
    <TD align=left noWrap vAlign=baseline><A name=TRINV><TT>TRINV</TT></A>, 
</TD>
    <TD align=left noWrap vAlign=baseline><A name=TRSINV><TT>TRSINV</TT></A>, 
    </TD>
    <TD align=left noWrap vAlign=baseline><A name=TRLA><TT>TRLA</TT></A>, </TD>
    <TD align=left noWrap vAlign=baseline><A name=TRLTA><TT>TRLTA</TT></A>, 
  </TD></TR>
  <TR>
    <TD align=left noWrap vAlign=baseline><A name=TRAL><TT>TRAL</TT></A>, </TD>
    <TD align=left noWrap vAlign=baseline><A name=TRALT><TT>TRALT</TT></A>, 
</TD>
    <TD align=left noWrap vAlign=baseline><A name=TRSA><TT>TRSA</TT></A>, </TD>
    <TD align=left noWrap vAlign=baseline><A name=TRAS><TT>TRAS</TT></A>, </TD>
    <TD align=left noWrap vAlign=baseline><A name=TRSAT><TT>TRSAT</TT></A>, 
</TD>
    <TD align=left noWrap vAlign=baseline><A name=TRATS><TT>TRATS</TT></A>, 
</TD>
    <TD align=left noWrap vAlign=baseline><A name=TRAAT><TT>TRAAT</TT></A>, 
</TD>
    <TD align=left noWrap vAlign=baseline><A name=TRATA><TT>TRATA</TT></A>, 
  </TD></TR>
  <TR>
    <TD align=left noWrap vAlign=baseline><A name=TRASAT><TT>TRASAT</TT></A>, 
    </TD>
    <TD align=left noWrap vAlign=baseline><A name=TRATSA><TT>TRATSA</TT></A>, 
    </TD>
    <TD align=left noWrap vAlign=baseline><A name=TRQSQ><TT>TRQSQ</TT></A>, 
</TD>
    <TD align=left noWrap vAlign=baseline><A name=TRPCK><TT>TRPCK</TT></A>, 
</TD>
    <TD align=left noWrap vAlign=baseline><A name=TRUPCK><TT>TRUPCK</TT></A> 
    </TD>
    <TD align=left noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline></TD></TR></TBODY></TABLE>
<P>
<P><B>Usage:</B>
<P><B>Choleski Decomposition</B> <BR>
<TABLE cols=4>
  <COLGROUP>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <TBODY>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRCHUL(S,W,M)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline299 height=12 src="gif/trpack_files/img2.gif" 
      width=72> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRCHLU(S,V,M)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline301 height=12 src="gif/trpack_files/img3.gif" 
      width=61> </TD></TR></TBODY></TABLE><BR><TT>S</TT> is an <IMG align=middle 
alt=tex2html_wrap_inline303 height=18 src="gif/trpack_files/img4.gif" width=36> 
<I>positive semi-definite</I> symmetric matrix (e.g., error or weight matrix) 
and the routines calculate the complementary lower triangular Choleski factors. 
It is allowed to overwrite <TT>S</TT> by <TT>W</TT> or <TT>V</TT>. 
<P><B>Symmetric Multiplication of Lower Triangular Matrices</B> <BR>
<TABLE cols=6>
  <COLGROUP>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <TBODY>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRSMUL(W,S,M)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline307 height=12 src="gif/trpack_files/img6.gif" 
      width=42> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline305 height=9 src="gif/trpack_files/img5.gif" width=14> 
    </TD>
    <TD align=left noWrap vAlign=baseline><B>S</B> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRSMLU(W,R,M)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline309 height=12 src="gif/trpack_files/img7.gif" 
      width=41> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline305 height=9 src="gif/trpack_files/img5.gif" width=14> 
    </TD>
    <TD align=left noWrap vAlign=baseline><B>R</B> 
</TD></TR></TBODY></TABLE><BR><TT>W</TT> is an <IMG align=middle 
alt=tex2html_wrap_inline311 height=18 src="gif/trpack_files/img8.gif" width=36> 
lower triangular matrix and <TT>S</TT>, <TT>R</TT> the two symmetric products of 
the multiplication of <TT>W</TT> by its transpose. It is allowed to overwrite 
<TT>W</TT> by either <TT>S</TT> or <TT>R</TT>. 
<P><B>Lower Triangular Matrix Inversion</B> <BR>
<TABLE cols=6>
  <COLGROUP>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <TBODY>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRINV(W,V,M)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline315 height=13 src="gif/trpack_files/img10.gif" 
      width=35> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline313 height=9 src="gif/trpack_files/img9.gif" width=14> 
    </TD>
    <TD align=left noWrap vAlign=baseline><B>V</B> 
</TD></TR></TBODY></TABLE><BR><TT>W</TT> is an <IMG align=middle 
alt=tex2html_wrap_inline317 height=18 src="gif/trpack_files/img11.gif" width=36> 
lower triangular matrix which is inverted into <TT>V</TT> (the inverse of a 
lower triangular matrix is lower triangular). <TT>W</TT> <I>may have rows and 
columns of zeros</I> as produced by the Choleski decomposition of a weight 
matrix with unmeasured variables. It is allowed to overwrite <TT>W</TT> by 
<TT>V</TT>. 
<P><B>Symmetric Matrix Inversion</B> <BR>
<TABLE cols=6>
  <COLGROUP>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <TBODY>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRSINV(S,R,M)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline321 height=13 src="gif/trpack_files/img13.gif" 
      width=25> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline319 height=9 src="gif/trpack_files/img12.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>R</B> 
</TD></TR></TBODY></TABLE><BR><TT>S</TT> is an <IMG align=middle 
alt=tex2html_wrap_inline323 height=18 src="gif/trpack_files/img14.gif" width=36> 
<I>positive semi-definite</I> symmetric matrix which is inverted into <TT>R</TT> 
(also stored packed). It is permissible to overwrite <TT>S</TT> by <TT>R</TT>. 
<P><B>Triangular - Rectangular Multiplication</B> <BR>
<TABLE cols=6>
  <COLGROUP>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <TBODY>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRLA (W,A,B,M,N)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline327 height=11 src="gif/trpack_files/img16.gif" 
      width=30> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline325 height=9 src="gif/trpack_files/img15.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>B</B> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRLTA(W,A,B,M,N)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline329 height=12 src="gif/trpack_files/img17.gif" 
      width=37> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline325 height=9 src="gif/trpack_files/img15.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>B</B> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRAL (A,V,B,M,N)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><B>AV</B> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline325 height=9 src="gif/trpack_files/img15.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>B</B> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRALT(A,V,B,M,N)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline331 height=12 src="gif/trpack_files/img18.gif" 
      width=28> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline325 height=9 src="gif/trpack_files/img15.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>B</B> 
</TD></TR></TBODY></TABLE><BR><TT>A</TT> and <TT>B</TT> are <IMG align=middle 
alt=tex2html_wrap_inline333 height=18 src="gif/trpack_files/img19.gif" width=36> 
rectangular matrices, <TT>W</TT> is an <IMG align=middle 
alt=tex2html_wrap_inline335 height=18 src="gif/trpack_files/img20.gif" width=36> 
lower triangular matrix, and <TT>V</TT> is an <IMG align=middle 
alt=tex2html_wrap_inline337 height=18 src="gif/trpack_files/img21.gif" width=36> 
lower triangular matrix. In each call it is allowed to overwrite <TT>A</TT> by 
<TT>B</TT>. 
<P><B>Symmetric - Rectangular Multiplication</B> <BR>
<TABLE cols=6>
  <COLGROUP>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <TBODY>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRSA (S,A,C,M,N)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><B>SA</B> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline339 height=9 src="gif/trpack_files/img22.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>C</B> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRAS (A,R,C,M,N)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><B>AR</B> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline339 height=9 src="gif/trpack_files/img22.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>C</B> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRSAT(S,B,C,M,N)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline341 height=12 src="gif/trpack_files/img23.gif" 
      width=25> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline339 height=9 src="gif/trpack_files/img22.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>C</B> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRATS(B,R,C,M,N)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline343 height=12 src="gif/trpack_files/img24.gif" 
      width=31> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline339 height=9 src="gif/trpack_files/img22.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>C</B> 
</TD></TR></TBODY></TABLE><BR><TT>A</TT> and <TT>C</TT> are <IMG align=middle 
alt=tex2html_wrap_inline345 height=18 src="gif/trpack_files/img25.gif" width=36> 
rectangular matrices, <TT>B</TT> is an <IMG align=middle 
alt=tex2html_wrap_inline347 height=18 src="gif/trpack_files/img26.gif" width=36> 
matrix, <TT>S</TT> is an <IMG align=middle alt=tex2html_wrap_inline349 height=18 
src="gif/trpack_files/img27.gif" width=36> symmetrix matrix, and <TT>R</TT> is an 
<IMG align=middle alt=tex2html_wrap_inline351 height=18 
src="gif/trpack_files/img28.gif" width=36> symmetric matrix. It is <I>not</I> 
allowed to overwrite <TT>A</TT> or <TT>B</TT> by the product matrix <TT>C</TT>. 
<P><B>Symmetric Multiplication of Rectangular Matrices</B> <BR>
<TABLE cols=6>
  <COLGROUP>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <TBODY>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRAAT(A,S,M,N)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline355 height=12 src="gif/trpack_files/img30.gif" 
      width=30> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline353 height=9 src="gif/trpack_files/img29.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>S</B> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRATA(B,R,M,N)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline357 height=12 src="gif/trpack_files/img31.gif" 
      width=29> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline353 height=9 src="gif/trpack_files/img29.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>R</B> 
</TD></TR></TBODY></TABLE><BR><TT>A</TT> is an <IMG align=middle 
alt=tex2html_wrap_inline359 height=18 src="gif/trpack_files/img32.gif" width=36> 
matrix, <TT>B</TT> is an <IMG align=middle alt=tex2html_wrap_inline361 height=18 
src="gif/trpack_files/img33.gif" width=36> matrix, <TT>S</TT> is an <IMG 
align=middle alt=tex2html_wrap_inline363 height=18 src="gif/trpack_files/img34.gif" 
width=36> symmetric matrix, and <TT>R</TT> is an <IMG align=middle 
alt=tex2html_wrap_inline365 height=18 src="gif/trpack_files/img35.gif" width=36> 
symmetric matrix. No overwriting is allowed. 
<P><B>Transformation of Symmetric Matrix </B><BR>
<TABLE cols=6>
  <COLGROUP>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <TBODY>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRASAT(A,S,R,M,N)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline369 height=12 src="gif/trpack_files/img37.gif" 
      width=40> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline367 height=9 src="gif/trpack_files/img36.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>R</B> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRATSA(B,S,R,M,N)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline371 height=12 src="gif/trpack_files/img38.gif" 
      width=39> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline367 height=9 src="gif/trpack_files/img36.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>R</B> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRQSQ (Q,T,R,M)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><B>QTQ</B> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline367 height=9 src="gif/trpack_files/img36.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>R</B> 
</TD></TR></TBODY></TABLE><BR><TT>A</TT> is an <IMG align=middle 
alt=tex2html_wrap_inline373 height=18 src="gif/trpack_files/img39.gif" width=36> 
matrix, <TT>B</TT> is an <IMG align=middle alt=tex2html_wrap_inline375 height=18 
src="gif/trpack_files/img40.gif" width=36> matrix, <TT>S</TT> is an <IMG 
align=middle alt=tex2html_wrap_inline377 height=18 src="gif/trpack_files/img41.gif" 
width=36> symmetric matrix, and <TT>R</TT>, <TT>Q</TT>, <TT>T</TT> are <IMG 
align=middle alt=tex2html_wrap_inline379 height=18 src="gif/trpack_files/img42.gif" 
width=36> symmetric matrices. No overwriting is allowed. 
<P><B>Packing and Unpacking a Symmetric Matrix</B> <BR>
<TABLE cols=6>
  <COLGROUP>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <TBODY>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRPCK (A,S,M)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><B>A</B> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline381 height=9 src="gif/trpack_files/img43.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>S</B> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRUPCK(S,A,M)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><B>S</B> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline381 height=9 src="gif/trpack_files/img43.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>A</B> 
</TD></TR></TBODY></TABLE><BR><TT>A</TT> is an <IMG align=middle 
alt=tex2html_wrap_inline383 height=18 src="gif/trpack_files/img44.gif" width=36> 
unpacked symmetric matrix (all <IMG align=bottom alt=tex2html_wrap_inline385 
height=13 src="gif/trpack_files/img45.gif" width=15> elements) and <TT>S</TT> is the 
same matrix stored packed. Overwriting is allowed for both <TT>TRPCK</TT> and 
<TT>TRUPCK</TT>. <BR><IMG align=bottom alt=tex2html_wrap_inline387 height=7 
src="gif/trpack_files/img46.gif" width=6> <BR>
<HR>
<P>
<ADDRESS>Michel Goossens Wed Jun 5 05:00:56 METDST 1996 </ADDRESS></BODY></HTML>
end_html 
*/
////////////////////////////////////////////////////////////////////////////////////////////////////////

ClassImp(StCL)
//___________________________________________________________________________
void StCL::mxmad_0_(int n_, float *a, float *b, float *c, int i, int j, int k)
{
////////////////////////////////////////////////////////////////////////////////////////////////////////
/* begin_html 
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
<!-- saved from url=(0056)http://wwwinfo.cern.ch/asdoc/shortwrupsdir/f110/top.html -->
<!--Converted with LaTeX2HTML 96.1-e (April 9, 1996) by Nikos Drakos (nikos@cbl.leeds.ac.uk), CBLU, University of Leeds --><HTML><HEAD><TITLE>TC Matrix Manipulation Package</TITLE>
<META content="text/html; charset=windows-1251" http-equiv=Content-Type>
<META content="TC Matrix Manipulation Package" name=description>
<META content=top name=keywords>
<META content=document name=resource-type>
<META content=global name=distribution>
<META content="MSHTML 5.00.2314.1000" name=GENERATOR></HEAD>
<BODY lang=EN>
<P>
<H2>F110: TC Matrix Manipulation Package</H2>
<TABLE border=1>
  <TBODY>
  <TR>
    <TD align=left>Author(s): TC 
    <TD align=left>Library: KERNLIB 
  <TR>
    <TD align=left>Submitter: C. Letertre 
    <TD align=left>Submitted: 01.08.1969 
  <TR>
    <TD align=left>Language: Fortran 
    <TD align=left>Revised: 07.03.1989 </TR></TBODY></TABLE><BR><!-- KEY VALUE="tc manipulation matrix row-wise " -->
<P>
<DIV align=center>
<P align=center><IMG align=bottom alt=tex2html_wrap271 height=96 
src="gif/tcpack_files/img1.gif" width=555> </P></DIV>The routines of <TT>MXPACK</TT> 
compute the product of two matrices or the product of their transposed matrices 
and may add or subtract to the resultant matrix a third one, add or subtract one 
matrix from another, or transfer a matrix, its negative, or a multiple of it, 
transpose a given matrix, build up a unit matrix, multiply a matrix by a 
diagonal (from left or from right) and may add the result to another matrix, add 
to square matrix the multiple of a diagonal matrix, compute the products <IMG 
align=bottom alt=tex2html_wrap_inline191 height=12 src="gif/tcpack_files/img2.gif" 
width=79> ( <IMG align=bottom alt=tex2html_wrap_inline193 height=12 
src="gif/tcpack_files/img3.gif" width=16> denotes the transpose of <IMG align=bottom 
alt=tex2html_wrap_inline195 height=11 src="gif/tcpack_files/img4.gif" width=12> ) 
and <IMG align=bottom alt=tex2html_wrap_inline197 height=12 
src="gif/tcpack_files/img5.gif" width=79> . It is assumed that matrices are stored 
<B>row-wise without gaps</B>, contrary to the Fortran convention. 
<P>
<P><B>Structure:</B>
<P><TT>SUBROUTINE</TT> subprograms <BR>User Entry Names: 
<TABLE cols=8>
  <COLGROUP>
  <COL align=left>
  <COL align=left>
  <COL align=left>
  <COL align=left>
  <COL align=left>
  <COL align=left>
  <COL align=left>
  <COL align=left>
  <TBODY>
  <TR>
    <TD align=left noWrap vAlign=baseline><A name=MXMAD><TT>MXMAD</TT></A>, 
</TD>
    <TD align=left noWrap vAlign=baseline><A name=MXMAD1><TT>MXMAD1</TT></A>, 
    </TD>
    <TD align=left noWrap vAlign=baseline><A name=MXMAD2><TT>MXMAD2</TT></A>, 
    </TD>
    <TD align=left noWrap vAlign=baseline><A name=MXMAD3><TT>MXMAD3</TT></A>, 
    </TD>
    <TD align=left noWrap vAlign=baseline><A name=MXMPY><TT>MXMPY</TT></A>, 
</TD>
    <TD align=left noWrap vAlign=baseline><A name=MXMPY1><TT>MXMPY1</TT></A>, 
    </TD>
    <TD align=left noWrap vAlign=baseline><A name=MXMPY2><TT>MXMPY2</TT></A>, 
    </TD>
    <TD align=left noWrap vAlign=baseline><A name=MXMPY3><TT>MXMPY3</TT></A>, 
    </TD></TR>
  <TR>
    <TD align=left noWrap vAlign=baseline><A name=MXMUB><TT>MXMUB</TT></A>, 
</TD>
    <TD align=left noWrap vAlign=baseline><A name=MXMUB1><TT>MXMUB1</TT></A>, 
    </TD>
    <TD align=left noWrap vAlign=baseline><A name=MXMUB2><TT>MXMUB2</TT></A>, 
    </TD>
    <TD align=left noWrap vAlign=baseline><A name=MXMUB3><TT>MXMUB3</TT></A>, 
    </TD>
    <TD align=left noWrap vAlign=baseline><A name=MXTRP><TT>MXTRP</TT></A>, 
</TD>
    <TD align=left noWrap vAlign=baseline><A name=MXUTY><TT>MXUTY</TT></A>, 
</TD>
    <TD align=left noWrap vAlign=baseline><A name=MXMLRT><TT>MXMLRT</TT></A>, 
    </TD>
    <TD align=left noWrap vAlign=baseline><A name=MXMLTR><TT>MXMLTR</TT></A> 
    </TD></TR></TBODY></TABLE>
<P>
<P><B>Usage:</B>
<P><B>Matrix Multiplication</B> <BR>
<TABLE cols=4>
  <COLGROUP>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <TBODY>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL MXMPY(A,B,C,NI,NJ,NK)</TT> 
    </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=middle 
      alt=tex2html_wrap_inline199 height=25 src="gif/tcpack_files/img6.gif" 
      width=132> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL MXMPY1(A,Q,C,NI,NJ,NK)</TT> 
    </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=middle 
      alt=tex2html_wrap_inline201 height=24 src="gif/tcpack_files/img7.gif" 
      width=67> \ ( <IMG align=middle alt=tex2html_wrap_inline203 height=22 
      src="gif/tcpack_files/img8.gif" width=12> is <IMG align=middle 
      alt=tex2html_wrap_inline205 height=18 src="gif/tcpack_files/img9.gif" 
      width=52> ) </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL MXMPY2(P,B,C,NI,NJ,NK)</TT> 
    </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline207 height=13 src="gif/tcpack_files/img10.gif" 
      width=66> \ ( <IMG align=bottom alt=tex2html_wrap_inline209 height=11 
      src="gif/tcpack_files/img11.gif" width=10> is <IMG align=middle 
      alt=tex2html_wrap_inline211 height=18 src="gif/tcpack_files/img12.gif" 
      width=52> ) </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL MXMPY3(P,Q,C,NI,NJ,NK)</TT> 
    </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=middle 
      alt=tex2html_wrap_inline213 height=24 src="gif/tcpack_files/img13.gif" 
      width=71> </TD></TR></TBODY></TABLE><BR>If <IMG align=bottom 
alt=tex2html_wrap_inline215 height=10 src="gif/tcpack_files/img14.gif" width=46> , 
<IMG align=bottom alt=tex2html_wrap_inline217 height=11 
src="gif/tcpack_files/img15.gif" width=11> will be filled with zeros. <BR><B>Matrix 
Multiplication and Addition</B> <BR>
<TABLE cols=4>
  <COLGROUP>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <TBODY>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL MXMAD(A,B,C,NI,NJ,NK)</TT> 
    </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=middle 
      alt=tex2html_wrap_inline219 height=25 src="gif/tcpack_files/img16.gif" 
      width=188> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL MXMAD1(A,Q,C,NI,NJ,NK)</TT> 
    </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=middle 
      alt=tex2html_wrap_inline221 height=24 src="gif/tcpack_files/img17.gif" 
      width=100> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL MXMAD2(P,B,C,NI,NJ,NK)</TT> 
    </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=middle 
      alt=tex2html_wrap_inline223 height=26 src="gif/tcpack_files/img18.gif" 
      width=98> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL MXMAD3(P,Q,C,NI,NJ,NK)</TT> 
    </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=middle 
      alt=tex2html_wrap_inline225 height=24 src="gif/tcpack_files/img19.gif" 
      width=104> </TD></TR></TBODY></TABLE><BR>If <IMG align=bottom 
alt=tex2html_wrap_inline227 height=10 src="gif/tcpack_files/img20.gif" width=46> , 
<IMG align=bottom alt=tex2html_wrap_inline229 height=11 
src="gif/tcpack_files/img21.gif" width=11> will not be changed. <BR>
<P><B>Matrix Multiplication and Subtraction</B> <BR>
<TABLE cols=4>
  <COLGROUP>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <TBODY>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL MXMUB(A,B,C,NI,NJ,NK)</TT> 
    </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=middle 
      alt=tex2html_wrap_inline231 height=25 src="gif/tcpack_files/img22.gif" 
      width=188> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL MXMUB1(A,Q,C,NI,NJ,NK)</TT> 
    </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=middle 
      alt=tex2html_wrap_inline233 height=24 src="gif/tcpack_files/img23.gif" 
      width=100> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL MXMUB2(P,B,C,NI,NJ,NK)</TT> 
    </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=middle 
      alt=tex2html_wrap_inline235 height=24 src="gif/tcpack_files/img24.gif" 
      width=98> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL MXMUB3(P,Q,C,NI,NJ,NK)</TT> 
    </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=middle 
      alt=tex2html_wrap_inline237 height=24 src="gif/tcpack_files/img25.gif" 
      width=104> </TD></TR></TBODY></TABLE><BR>If <IMG align=bottom 
alt=tex2html_wrap_inline239 height=10 src="gif/tcpack_files/img26.gif" width=46> , 
<IMG align=bottom alt=tex2html_wrap_inline241 height=11 
src="gif/tcpack_files/img27.gif" width=11> will be replaced by <IMG align=middle 
alt=tex2html_wrap_inline243 height=20 src="gif/tcpack_files/img28.gif" width=23> . 
<BR><B>Matrix Transposition</B> <BR>
<TABLE cols=4>
  <COLGROUP>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <TBODY>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL MXTRP(A,B,NI,NJ)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=middle 
      alt=tex2html_wrap_inline245 height=25 src="gif/tcpack_files/img29.gif" 
      width=94> </TD></TR></TBODY></TABLE><BR><B>Unity Matrix</B> <BR>
<TABLE cols=4>
  <COLGROUP>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <TBODY>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL MXUTY(A,NI)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=middle 
      alt=tex2html_wrap_inline247 height=25 src="gif/tcpack_files/img30.gif" 
      width=193> </TD></TR></TBODY></TABLE><BR><B>Matrix Multiplication</B> <BR>
<TABLE cols=4>
  <COLGROUP>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <TBODY>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL MXMLRT(A,B,X,M,N)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=middle 
      alt=tex2html_wrap_inline249 height=25 src="gif/tcpack_files/img31.gif" 
      width=294> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL MXMLTR(A,B,X,N,M)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=middle 
      alt=tex2html_wrap_inline251 height=25 src="gif/tcpack_files/img32.gif" 
      width=294> </TD></TR></TBODY></TABLE>
<P>
<P><B>Notes:</B>
<P>In the formulae above, <IMG align=middle alt=tex2html_wrap_inline253 
height=25 src="gif/tcpack_files/img33.gif" width=34> <I>etc</I> denotes the ensemble 
of elements of the matrix <IMG align=bottom alt=tex2html_wrap_inline255 
height=11 src="gif/tcpack_files/img34.gif" width=12> <I>etc</I> with the row index 
<I>i</I> and the column index <I>j</I>. The Fortran variables <TT>NI</TT>, 
<TT>NJ</TT> and <TT>NK</TT> specify the dimensions associated with the indices 
<I>i</I>,<I>j</I> and <I>k</I>. If <TT>DIMENSION A(NJ,NI)</TT> reserves space 
for the matrix <IMG align=bottom alt=tex2html_wrap_inline265 height=11 
src="gif/tcpack_files/img35.gif" width=12> , then the element <IMG align=middle 
alt=tex2html_wrap_inline267 height=23 src="gif/tcpack_files/img36.gif" width=22> is 
contained in <TT>A(J,I)</TT>. <BR><IMG align=bottom alt=tex2html_wrap_inline269 
height=7 src="gif/tcpack_files/img37.gif" width=6> <BR>
<HR>

<P>
<ADDRESS>Michel Goossens Wed Jun 5 04:52:43 METDST 1996 </ADDRESS></BODY></HTML>
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
		    case 1:  c[ic] = 0.;     break;
		    case 3:  c[ic] = -c[ic]; break;
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

#define StCL_mxmad(n_,a,b,c,i,j,k)                       \
    /* Local variables */                                \
    int l, m, n, ia, ic, ib, ja, jb, iia, iib, ioa, iob; \
\
    /* Parameter adjustments */                          \
    --a;  --b;  --c;                                     \
    /* Function Body */                                  \
//                      MXMAD MXMAD1 MXMAD2 MXMAD3 MXMPY MXMPY1 MXMPY2 MXMPY3 MXMUB MXMUB1 MXMUB2 MXMUB3 \
//  const int iandj1[] = {21,   22,    23,    24,   11,    12,    13,    14,    31,   32,   33,    34 }; \
    const int iandj1[] = {2,    2 ,    2 ,    2 ,   1 ,    1 ,    1 ,    1 ,    3 ,   3 ,   3 ,    3  }; \
    const int iandj2[] = { 1,    2,     3,     4,    1,     2,     3,     4,     1,    2,    3,     4 }; \
    int n1 = iandj1[n_];                                  \
    int n2 = iandj2[n_];                                  \
    if (i == 0 || k == 0) return 0;                       \
    \
    switch (n2) {                                         \
      case 1: iia = 1; ioa = j; iib = k; iob = 1; break;  \
      case 2: iia = 1; ioa = j; iib = 1; iob = j; break;  \
      case 3: iia = i; ioa = 1; iib = k; iob = 1; break;  \
      case 4: iia = i; ioa = 1; iib = 1; iob = j; break;  \
    };                                                    \
  \
    ia = 1; ic = 1;                                       \
    for (l = 1; l <= i; ++l) {                            \
	    ib = 1;                                           \
	    for (m = 1; m <= k; ++m,++ic) {                   \
	      switch (n1) {                                   \
		      case 1:  c[ic] = 0.;      break;            \
		      case 3:  c[ic] = -c[ic];  break;            \
	      };                                              \
	      if (j == 0) continue;                           \
	      ja = ia; jb = ib;                               \
	      for (n = 1; n <= j; ++n, ja+=iia, jb+=iib)      \
		      c[ic] += a[ja] * b[jb];                     \
	      ib += iob;                                      \
	    }                                                 \
	    ia += ioa;                                        \
    }

//___________________________________________________________________________
double *StCL::mxmad_0_(int n_, double *a, double *b, double *c, int i, int j, int k)
{
   StCL_mxmad(n_,a,b,c,i,j,k)
   return c;
} /* mxmad_ */

//___________________________________________________________________________
void StCL::mxmlrt_0_(int n__, float *a, float *b, float *c, int ni,int nj)
{
 // Matrix Multiplication 
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/tcpack_files/img31.gif"> </P> End_Html // 
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/tcpack_files/img32.gif"> </P> End_Html // 
 // CERN PROGLIB# F110    MXMLRT          .VERSION KERNFOR  2.00  720707
 // ORIG. 01/01/64 RKB 

  /* Local variables */
  float x;
  int ia, ib, ic, ja, kc, ii, jj, kj, ki, ia1, ib1, ic1, ja1;



// --      ENTRY MXMLRT */
// --                C = A(I,J) X B(J,J) X A*(J,I) */
// --                A* STANDS FOR A-TRANSPOSED */
// Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/tcpack_files/img31.gif"> </P> End_Html // 
//             mxmlrt (A,B,C,NI,NJ)     IS EQUIVALENT TO */
//             CALL MXMPY (A,B,X,NI,NJ,NJ) */
//             CALL MXMPY1 (X,A,C,NI,NJ,NI) */

/*        OR   CALL MXMPY1 (B,A,Y,NJ,NJ,NI) */
/*             CALL MXMPY (A,Y,C,NI,NJ,NI) */

    /* Parameter adjustments to use indeces from "1" */
  --a;  --b;  --c;

  /* Function Body */
  int ipa = 1;
  int jpa = nj;
  if (n__ == 1) { ipa = ni;  jpa = 1; }

// --                C = A*(I,J) X B(J,J) X A(J,I) 
// Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/tcpack_files/img32.gif"> </P> End_Html // 

//        CALL MXMLTR (A,B,C,NI,NJ)     IS EQUIVALENT TO 
//             CALL MXMPY2 (A,B,X,NI,NJ,NJ)
//             CALL MXMPY (X,A,C,NI,NJ,NI) 

//        OR   CALL MXMPY (B,A,Y,NJ,NJ,NI) 
//             CALL MXMPY2 (A,Y,C,NI,NJ,NI)

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
void StCL::mxmlrt_0_(int n__, double *a, double *b, double *c, int ni,int nj)
{
 // Matrix Multiplication (double precision)
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/tcpack_files/img31.gif"> </P> End_Html // 
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/tcpack_files/img32.gif"> </P> End_Html // 
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
      x = 0.;
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
void StCL::mxtrp(float *a, float *b, int i, int j)
{
//
//  Matrix Transposition 
// Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/tcpack_files/img29.gif"> </P> End_Html // 
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
void StCL::mxtrp(double *a, double *b, int i, int j)
{
//  Matrix Transposition (double precision)
// Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/tcpack_files/img29.gif"> </P> End_Html // 
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
/*
begin_html
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
<!-- saved from url=(0056)http://wwwinfo.cern.ch/asdoc/shortwrupsdir/f112/top.html -->
<!--Converted with LaTeX2HTML 96.1-e (April 9, 1996) by Nikos Drakos (nikos@cbl.leeds.ac.uk), CBLU, University of Leeds --><HTML><HEAD><TITLE>Manipulation of Triangular and Symmetric Matrices</TITLE>
<META content="text/html; charset=windows-1251" http-equiv=Content-Type>
<META content="Manipulation of Triangular and Symmetric Matrices " 
name=description>
<META content=top name=keywords>
<META content=document name=resource-type>
<META content=global name=distribution>
<META content="MSHTML 5.00.2314.1000" name=GENERATOR></HEAD>
<BODY lang=EN>
<P>
<H2>F112: Manipulation of Triangular and Symmetric Matrices </H2>
<TABLE border=1>
  <TBODY>
  <TR>
    <TD align=left>Author(s): W. Hart 
    <TD align=left>Library: KERNLIB 
  <TR>
    <TD align=left>Submitter: 
    <TD align=left>Submitted: 01.01.1975 
  <TR>
    <TD align=left>Language: Fortran 
    <TD align=left>Revised: 12.12.1986 </TR></TBODY></TABLE><BR><!-- KEY VALUE="operation symmetric triangular matrix " -->
<P>At CERN, matrices are often stored row-wise (TC-convention); furthermore, 
symmetric matrices are stored packed as the lower left triangular part only, 
i.e., the <I>I</I>th diagonal element is found in position 
<I>I</I>(<I>I</I>+1)/2. The <TT>TR</TT>-package performs many of the frequently 
required operations associated with such matrices without resorting to expanding 
into the unpacked square form. In all the following routines an <IMG 
align=middle alt=tex2html_wrap_inline295 height=20 src="trpack_files/MxMItalic.gif" 
width=52> <I>symmetric</I> matrix is taken to be stored in the packed form with 
<I>M</I>(<I>M</I>+1)/2 elements. 
<P>Some of these operations produce and require the manipulation of <I>lower 
triangular</I> matrices which have all elements zero above the leading diagonal. 
These are also stored in the packed form with all the zeros dropped; therefore, 
care has to be taken in the interpretation of a packed matrix as to whether it 
represents a symmetric or lower triangular array. To facilitate this distinction 
in the Write-up, the following nomenclature has been adopted: 
<DL compact>
  <DT>A,B,C 
  <DD>unpacked rectangular matrices (row-wise storage) 
  <DT>Q,R,S,T 
  <DD>packed symmetric matrices 
  <DT>V,W 
  <DD>packed lower triangular matrices </DD></DL>On 32-bit machines the 
calculations are performed internally in double-precision mode. 
<P>
<P><B>Structure:</B>
<P><TT>SUBROUTINE</TT> subprograms <BR>User Entry Names: 
<TABLE cols=8>
  <COLGROUP>
  <COL align=left>
  <COL align=left>
  <COL align=left>
  <COL align=left>
  <COL align=left>
  <COL align=left>
  <COL align=left>
  <COL align=left>
  <TBODY>
  <TR>
    <TD align=left noWrap vAlign=baseline><A name=TRCHUL><TT>TRCHUL</TT></A>, 
    </TD>
    <TD align=left noWrap vAlign=baseline><A name=TRCHLU><TT>TRCHLU</TT></A>, 
    </TD>
    <TD align=left noWrap vAlign=baseline><A name=TRSMUL><TT>TRSMUL</TT></A>, 
    </TD>
    <TD align=left noWrap vAlign=baseline><A name=TRSMLU><TT>TRSMLU</TT></A>, 
    </TD>
    <TD align=left noWrap vAlign=baseline><A name=TRINV><TT>TRINV</TT></A>, 
</TD>
    <TD align=left noWrap vAlign=baseline><A name=TRSINV><TT>TRSINV</TT></A>, 
    </TD>
    <TD align=left noWrap vAlign=baseline><A name=TRLA><TT>TRLA</TT></A>, </TD>
    <TD align=left noWrap vAlign=baseline><A name=TRLTA><TT>TRLTA</TT></A>, 
  </TD></TR>
  <TR>
    <TD align=left noWrap vAlign=baseline><A name=TRAL><TT>TRAL</TT></A>, </TD>
    <TD align=left noWrap vAlign=baseline><A name=TRALT><TT>TRALT</TT></A>, 
</TD>
    <TD align=left noWrap vAlign=baseline><A name=TRSA><TT>TRSA</TT></A>, </TD>
    <TD align=left noWrap vAlign=baseline><A name=TRAS><TT>TRAS</TT></A>, </TD>
    <TD align=left noWrap vAlign=baseline><A name=TRSAT><TT>TRSAT</TT></A>, 
</TD>
    <TD align=left noWrap vAlign=baseline><A name=TRATS><TT>TRATS</TT></A>, 
</TD>
    <TD align=left noWrap vAlign=baseline><A name=TRAAT><TT>TRAAT</TT></A>, 
</TD>
    <TD align=left noWrap vAlign=baseline><A name=TRATA><TT>TRATA</TT></A>, 
  </TD></TR>
  <TR>
    <TD align=left noWrap vAlign=baseline><A name=TRASAT><TT>TRASAT</TT></A>, 
    </TD>
    <TD align=left noWrap vAlign=baseline><A name=TRATSA><TT>TRATSA</TT></A>, 
    </TD>
    <TD align=left noWrap vAlign=baseline><A name=TRQSQ><TT>TRQSQ</TT></A>, 
</TD>
    <TD align=left noWrap vAlign=baseline><A name=TRPCK><TT>TRPCK</TT></A>, 
</TD>
    <TD align=left noWrap vAlign=baseline><A name=TRUPCK><TT>TRUPCK</TT></A> 
    </TD>
    <TD align=left noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline></TD></TR></TBODY></TABLE>
<P>
<P><B>Usage:</B>
<P><B>Choleski Decomposition</B> <BR>
<TABLE cols=4>
  <COLGROUP>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <TBODY>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRCHUL(S,W,M)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline299 height=12 src="trpack_files/img2.gif" 
      width=72> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRCHLU(S,V,M)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline301 height=12 src="trpack_files/img3.gif" 
      width=61> </TD></TR></TBODY></TABLE><BR><TT>S</TT> is an <IMG align=middle 
alt=tex2html_wrap_inline303 height=18 src="trpack_files/MxM.gif" width=36> 
<I>positive semi-definite</I> symmetric matrix (e.g., error or weight matrix) 
and the routines calculate the complementary lower triangular Choleski factors. 
It is allowed to overwrite <TT>S</TT> by <TT>W</TT> or <TT>V</TT>. 
<P><B>Symmetric Multiplication of Lower Triangular Matrices</B> <BR>
<TABLE cols=6>
  <COLGROUP>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <TBODY>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRSMUL(W,S,M)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline307 height=12 src="trpack_files/img6.gif" 
      width=42> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline305 height=9 src="trpack_files/arrow.gif" width=14> 
    </TD>
    <TD align=left noWrap vAlign=baseline><B>S</B> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRSMLU(W,R,M)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline309 height=12 src="trpack_files/img7.gif" 
      width=41> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline305 height=9 src="trpack_files/arrow.gif" width=14> 
    </TD>
    <TD align=left noWrap vAlign=baseline><B>R</B> 
</TD></TR></TBODY></TABLE><BR><TT>W</TT> is an <IMG align=middle 
alt=tex2html_wrap_inline311 height=18 src="trpack_files/MxM.gif" width=36> 
lower triangular matrix and <TT>S</TT>, <TT>R</TT> the two symmetric products of 
the multiplication of <TT>W</TT> by its transpose. It is allowed to overwrite 
<TT>W</TT> by either <TT>S</TT> or <TT>R</TT>. 
<P><B>Lower Triangular Matrix Inversion</B> <BR>
<TABLE cols=6>
  <COLGROUP>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <TBODY>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRINV(W,V,M)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline315 height=13 src="trpack_files/img10.gif" 
      width=35> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline313 height=9 src="trpack_files/arrow.gif" width=14> 
    </TD>
    <TD align=left noWrap vAlign=baseline><B>V</B> 
</TD></TR></TBODY></TABLE><BR><TT>W</TT> is an <IMG align=middle 
alt=tex2html_wrap_inline317 height=18 src="trpack_files/MxM.gif" width=36> 
lower triangular matrix which is inverted into <TT>V</TT> (the inverse of a 
lower triangular matrix is lower triangular). <TT>W</TT> <I>may have rows and 
columns of zeros</I> as produced by the Choleski decomposition of a weight 
matrix with unmeasured variables. It is allowed to overwrite <TT>W</TT> by 
<TT>V</TT>. 
<P><B>Symmetric Matrix Inversion</B> <BR>
<TABLE cols=6>
  <COLGROUP>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <TBODY>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRSINV(S,R,M)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline321 height=13 src="trpack_files/img13.gif" 
      width=25> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline319 height=9 src="trpack_files/arrow.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>R</B> 
</TD></TR></TBODY></TABLE><BR><TT>S</TT> is an <IMG align=middle 
alt=tex2html_wrap_inline323 height=18 src="trpack_files/MxM.gif" width=36> 
<I>positive semi-definite</I> symmetric matrix which is inverted into <TT>R</TT> 
(also stored packed). It is permissible to overwrite <TT>S</TT> by <TT>R</TT>. 
<P><B>Triangular - Rectangular Multiplication</B> <BR>
<TABLE cols=6>
  <COLGROUP>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <TBODY>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRLA (W,A,B,M,N)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline327 height=11 src="trpack_files/img16.gif" 
      width=30> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline325 height=9 src="trpack_files/arrow.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>B</B> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRLTA(W,A,B,M,N)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline329 height=12 src="trpack_files/img17.gif" 
      width=37> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline325 height=9 src="trpack_files/arrow.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>B</B> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRAL (A,V,B,M,N)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><B>AV</B> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline325 height=9 src="trpack_files/arrow.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>B</B> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRALT(A,V,B,M,N)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline331 height=12 src="trpack_files/img18.gif" 
      width=28> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline325 height=9 src="trpack_files/arrow.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>B</B> 
</TD></TR></TBODY></TABLE><BR><TT>A</TT> and <TT>B</TT> are <IMG align=middle 
alt=tex2html_wrap_inline333 height=18 src="trpack_files/MxN.gif" width=36> 
rectangular matrices, <TT>W</TT> is an <IMG align=middle 
alt=tex2html_wrap_inline335 height=18 src="trpack_files/MxM.gif" width=36> 
lower triangular matrix, and <TT>V</TT> is an <IMG align=middle 
alt=tex2html_wrap_inline337 height=18 src="trpack_files/MxN.gif" width=36> 
lower triangular matrix. In each call it is allowed to overwrite <TT>A</TT> by 
<TT>B</TT>. 
<P><B>Symmetric - Rectangular Multiplication</B> <BR>
<TABLE cols=6>
  <COLGROUP>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <TBODY>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRSA (S,A,C,M,N)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><B>SA</B> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline339 height=9 src="trpack_files/arrow.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>C</B> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRAS (A,R,C,M,N)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><B>AR</B> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline339 height=9 src="trpack_files/arrow.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>C</B> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRSAT(S,B,C,M,N)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline341 height=12 src="trpack_files/img23.gif" 
      width=25> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline339 height=9 src="trpack_files/arrow.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>C</B> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRATS(B,R,C,M,N)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline343 height=12 src="trpack_files/img24.gif" 
      width=31> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline339 height=9 src="trpack_files/arrow.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>C</B> 
</TD></TR></TBODY></TABLE><BR><TT>A</TT> and <TT>C</TT> are <IMG align=middle 
alt=tex2html_wrap_inline345 height=18 src="trpack_files/MxN.gif" width=36> 
rectangular matrices, <TT>B</TT> is an <IMG align=middle 
alt=tex2html_wrap_inline347 height=18 src="trpack_files/NxM.gif" width=36> 
matrix, <TT>S</TT> is an <IMG align=middle alt=tex2html_wrap_inline349 height=18 
src="trpack_files/MxM.gif" width=36> symmetrix matrix, and <TT>R</TT> is an 
<IMG align=middle alt=tex2html_wrap_inline351 height=18 
src="trpack_files/NxN.gif" width=36> symmetric matrix. It is <I>not</I> 
allowed to overwrite <TT>A</TT> or <TT>B</TT> by the product matrix <TT>C</TT>. 
<P><B>Symmetric Multiplication of Rectangular Matrices</B> <BR>
<TABLE cols=6>
  <COLGROUP>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <TBODY>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRAAT(A,S,M,N)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline355 height=12 src="trpack_files/img30.gif" 
      width=30> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline353 height=9 src="trpack_files/arrow.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>S</B> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRATA(B,R,M,N)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline357 height=12 src="trpack_files/img31.gif" 
      width=29> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline353 height=9 src="trpack_files/arrow.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>R</B> 
</TD></TR></TBODY></TABLE><BR><TT>A</TT> is an <IMG align=middle 
alt=tex2html_wrap_inline359 height=18 src="trpack_files/img32.gif" width=36> 
matrix, <TT>B</TT> is an <IMG align=middle alt=tex2html_wrap_inline361 height=18 
src="trpack_files/NxM.gif" width=36> matrix, <TT>S</TT> is an <IMG 
align=middle alt=tex2html_wrap_inline363 height=18 src="trpack_files/MxM.gif" 
width=36> symmetric matrix, and <TT>R</TT> is an <IMG align=middle 
alt=tex2html_wrap_inline365 height=18 src="trpack_files/MxM.gif" width=36> 
symmetric matrix. No overwriting is allowed. 
<P><B>Transformation of Symmetric Matrix </B><BR>
<TABLE cols=6>
  <COLGROUP>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <TBODY>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRASAT(A,S,R,M,N)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline369 height=12 src="trpack_files/img37.gif" 
      width=40> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline367 height=9 src="trpack_files/arrow.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>R</B> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRATSA(B,S,R,M,N)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline371 height=12 src="trpack_files/img38.gif" 
      width=39> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline367 height=9 src="trpack_files/arrow.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>R</B> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRQSQ (Q,T,R,M)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><B>QTQ</B> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline367 height=9 src="trpack_files/arrow.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>R</B> 
</TD></TR></TBODY></TABLE><BR><TT>A</TT> is an <IMG align=middle 
alt=tex2html_wrap_inline373 height=18 src="trpack_files/MxN.gif" width=36> 
matrix, <TT>B</TT> is an <IMG align=middle alt=tex2html_wrap_inline375 height=18 
src="trpack_files/NxM.gif" width=36> matrix, <TT>S</TT> is an <IMG 
align=middle alt=tex2html_wrap_inline377 height=18 src="trpack_files/NxN.gif" 
width=36> symmetric matrix, and <TT>R</TT>, <TT>Q</TT>, <TT>T</TT> are <IMG 
align=middle alt=tex2html_wrap_inline379 height=18 src="trpack_files/MxM.gif" 
width=36> symmetric matrices. No overwriting is allowed. 
<P><B>Packing and Unpacking a Symmetric Matrix</B> <BR>
<TABLE cols=6>
  <COLGROUP>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <COL align=middle>
  <COL align=left>
  <TBODY>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRPCK (A,S,M)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><B>A</B> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline381 height=9 src="trpack_files/arrow.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>S</B> </TD></TR>
  <TR>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><TT>CALL TRUPCK(S,A,M)</TT> </TD>
    <TD align=middle noWrap vAlign=baseline></TD>
    <TD align=left noWrap vAlign=baseline><B>S</B> </TD>
    <TD align=middle noWrap vAlign=baseline><IMG align=bottom 
      alt=tex2html_wrap_inline381 height=9 src="trpack_files/arrow.gif" 
      width=14> </TD>
    <TD align=left noWrap vAlign=baseline><B>A</B> 
</TD></TR></TBODY></TABLE><BR><TT>A</TT> is an <IMG align=middle 
alt=tex2html_wrap_inline383 height=18 src="trpack_files/img44.gif" width=36> 
unpacked symmetric matrix (all <IMG align=bottom alt=tex2html_wrap_inline385 
height=13 src="trpack_files/img45.gif" width=15> elements) and <TT>S</TT> is the 
same matrix stored packed. Overwriting is allowed for both <TT>TRPCK</TT> and 
<TT>TRUPCK</TT>. <BR><IMG align=bottom alt=tex2html_wrap_inline387 height=7 
src="trpack_files/img46.gif" width=6> <BR>
<HR>

<P>
<ADDRESS>Michel Goossens Wed Jun 5 05:00:56 METDST 1996 </ADDRESS></BODY></HTML>
end_html
*/
//
//___________________________________________________________________________
//___________________________________________________________________________

//____________________________________________________________
float *StCL::traat(float *a, float *s, int m, int n)
{
//
// Symmetric Multiplication of Rectangular Matrices 
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
float *StCL::tral(float *a, float *u, float *b, int m, int n)
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
float *StCL::tralt(float *a, float *u, float *b, int m, int n)
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

float *StCL::tras(float *a, float *s, float *b, int m, int n)
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
float *StCL::trasat(float *a, float *s, float *r__, int m, int n)
{
  // trasat.F -- translated by f2c (version 19970219).
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
float *StCL::trata(float *a, float *r__, int m, int n)
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
float *StCL::trats(float *a, float *s, float *b, int m, int n)
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
/* Subroutine */float *StCL::tratsa(float *a, float *s, float *r__, int m, int n)
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
float *StCL::trchlu(float *a, float *b, int n)
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
/* Subroutine */float *StCL::trchul(float *a, float *b, int n)
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
/* Subroutine */float *StCL::trinv(float *t, float *s, int n)
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
/* Subroutine */float *StCL::trla(float *u, float *a, float *b, int m, int n)
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
// Subroutine */float *StCL::trlta(float *u, float *a, float *b, int m, int n)
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
/* Subroutine */float *StCL::trpck(float *s, float *u, int n)
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
float *StCL::trqsq(float *q, float *s, float *r__, int m)
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
float *StCL::trsa(float *s, float *a, float *b, int m, int n)
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
/* Subroutine */float *StCL::trsinv(float *g, float *gi, int n)
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
/* Subroutine */float *StCL::trsmlu(float *u, float *s, int n)
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
/* Subroutine */float *StCL::trsmul(float *g, float *gi, int n)
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
/* Subroutine */float *StCL::trupck(float *u, float *s, int m)
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
// Subroutine */ float *StCL::trsat(float *s, float *a, float *b, int m, int n)
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
double *StCL::traat(double *a, double *s, int m, int n)
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
double *StCL::tral(double *a, double *u, double *b, int m, int n)
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
double *StCL::tralt(double *a, double *u, double *b, int m, int n)
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

double *StCL::tras(double *a, double *s, double *b, int m, int n)
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

double *StCL::trasat(double *a, double *s, double *r__, int m, int n)
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
double *StCL::trata(double *a, double *r__, int m, int n)
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
double *StCL::trats(double *a, double *s, double *b, int m, int n)
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
/* Subroutine */double *StCL::tratsa(double *a, double *s, double *r__, int m, int n)
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
double *StCL::trchlu(double *a, double *b, int n)
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
double *StCL::trchul(double *a, double *b, int n)
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
double *StCL::trinv(double *t, double *s, int n)
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
/* Subroutine */double *StCL::trla(double *u, double *a, double *b, int m, int n)
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
double *StCL::trlta(double *u, double *a, double *b, int m, int n)
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
/* Subroutine */double *StCL::trpck(double *s, double *u, int n)
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
double *StCL::trqsq(double *q, double *s, double *r__, int m)
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
double *StCL::trsa(double *s, double *a, double *b, int m, int n)
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
/* Subroutine */double *StCL::trsinv(double *g, double *gi, int n)
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
/* Subroutine */double *StCL::trsmlu(double *u, double *s, int n)
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
/* Subroutine */double *StCL::trsmul(double *g, double *gi, int n)
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
/* Subroutine */double *StCL::trupck(double *u, double *s, int m)
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
double *StCL::trsat(double *s, double *a, double *b, int m, int n)
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
