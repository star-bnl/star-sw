#ifndef STAR_StCL
#define STAR_StCL
//*-- Author :    Valery Fine(fine@bnl.gov)   25/09/99  
//
// The set of methods to work with the plain matrix / vector
// "derived" from  http://wwwinfo.cern.ch/asdoc/shortwrupsdir/f110/top.html 
//
// $Id: StCL.h,v 1.2 1999/09/29 00:31:51 fine Exp $
// $Log: StCL.h,v $
// Revision 1.2  1999/09/29 00:31:51  fine
// RMath class has been repleaced with StCL one
//
// Revision 1.1  1999/09/28 19:45:10  fine
// RMath class has been renamed to StCL - STAR CERN Library
//
// Revision 1.6  1999/09/27 23:45:43  fine
// Several methods to calculate errors were introduced
//
// Revision 1.5  1999/09/26 02:48:50  fine
// F112 CERNLIB package (TR matrix) has been added. No micky test yet
//
// Revision 1.4  1999/09/23 18:32:11  fine
// double prec for float matrices was introduced
//
//

#include "Rtypes.h"
#include <string.h>

// http://wwwinfo.cern.ch/asdoc/shortwrupsdir/f110/top.html 

///////////////////////////////////////////////////////////////////////////////////////
//                                                                                   //
// The routines of MXPACK compute the product of two matrices or the product of      //
// their transposed matrices and may add or subtract to the resultant matrix         //
// a third one, add or subtract one matrix from another, or transfer a matrix,       //
// its negative, or a multiple of it, transpose a given matrix, build up a unit      //
// matrix, multiply a matrix by a diagonal (from left or from right) and may         //
// add the result to another matrix, add to square matrix the multiple of a diagonal //
// matrix, compute the products <IMG WIDTH=79 HEIGHT=12 ALIGN=BOTTOM ALT="tex2html_wrap_inline191" SRC="gif/mxpack_ABAt.gif"> (<IMG WIDTH=16 HEIGHT=12 ALIGN=BOTTOM ALT="tex2html_wrap_inline193" SRC="gif/mxpack_At.gif"> denotes the transpose of <IMG WIDTH=12 HEIGHT=11 ALIGN=BOTTOM ALT="tex2html_wrap_inline195" SRC="gif/mxpack_A.gif">) and <IMG WIDTH=79 HEIGHT=12 ALIGN=BOTTOM ALT="tex2html_wrap_inline197" SRC="gif/mxpack_AtBA.gif">.               //
// It is assumed that matrices are begin_html <B>row-wise without gaps</B> end_html without gaps.                     //
//                                                                                   //
///////////////////////////////////////////////////////////////////////////////////////

class StCL  {
  public:
    static float  *ucopy(const float  *a, float  *b, int n);
    static double *ucopy(const float  *a, double *b, int n);
    static float  *ucopy(const double *a, float  *b, int n);
    static double *ucopy(const double *a, double *b, int n);

    static float  *uzero(float *a,  int n1, int n2);
    static double *uzero(double *a, int n1, int n2);

    static float  *vzero(float *a,  int n2);
    static double *vzero(double *a, int n2);

    static float  *vadd(float *b, float *c,  float *a, int n);
    static double *vadd(double *b, double *c,  double *a, int n);

    static float  *vsub(float *a,   float *b,  float *x, int n);
    static double *vsub(double *a, double *b, double *x, int n);

    static float  *vcopyn(float *a,  float *x, int n);
    static double *vcopyn(double *a, double *x, int n);

    static void mxmad_0_(int n, float *a, float *b, float *c, int i, int j, int k);

    static void mxmad( float *a, float *b, float *c, int i, int j, int k);
    static void mxmad1(float *a, float *b, float *c, int i, int j, int k);
    static void mxmad2(float *a, float *b, float *c, int i, int j, int k);
    static void mxmad3(float *a, float *b, float *c, int i, int j, int k);
    static void mxmpy( float *a, float *b, float *c, int i, int j, int k);
    static void mxmpy1(float *a, float *b, float *c, int i, int j, int k);
    static void mxmpy2(float *a, float *b, float *c, int i, int j, int k);
    static void mxmpy3(float *a, float *b, float *c, int i, int j, int k);
    static void mxmub( float *a, float *b, float *c, int i, int j, int k);
    static void mxmub1(float *a, float *b, float *c, int i, int j, int k);
    static void mxmub2(float *a, float *b, float *c, int i, int j, int k);
    static void mxmub3(float *a, float *b, float *c, int i, int j, int k);

    static void mxmlrt_0_(int n__, float *a, float *b, float *c, int ni,int nj);
    static void mxmlrt(float *a, float *b, float *c, int ni, int nj);
    static void mxmltr(float *a, float *b, float *c, int ni, int nj);
    static void mxtrp(float *a, float *b, int i, int j);

    static double *mxmad_0_(int n, double *a, double *b, double *c, int i, int j, int k);

    static void mxmad (double *a, double *b, double *c, int i, int j, int k);
    static void mxmad1(double *a, double *b, double *c, int i, int j, int k);
    static void mxmad2(double *a, double *b, double *c, int i, int j, int k);
    static void mxmad3(double *a, double *b, double *c, int i, int j, int k);
    static void mxmpy (double *a, double *b, double *c, int i, int j, int k);
    static void mxmpy1(double *a, double *b, double *c, int i, int j, int k);
    static void mxmpy2(double *a, double *b, double *c, int i, int j, int k);
    static void mxmpy3(double *a, double *b, double *c, int i, int j, int k);
    static void mxmub (double *a, double *b, double *c, int i, int j, int k);
    static void mxmub1(double *a, double *b, double *c, int i, int j, int k);
    static void mxmub2(double *a, double *b, double *c, int i, int j, int k);
    static void mxmub3(double *a, double *b, double *c, int i, int j, int k);

    static void mxmlrt_0_(int n__, double *a, double *b, double *c, int ni,int nj);
    static void mxmlrt(double *a, double *b, double *c, int ni, int nj);
    static void mxmltr(double *a, double *b, double *c, int ni, int nj);
    static void mxtrp(double *a, double *b, int i, int j);

// * TR pack

       static float *traat(float *a, float *s, int m, int n);
       static float *tral(float *a, float *u, float *b, int m, int n);
       static float *tralt(float *a, float *u, float *b, int m, int n);
       static float *tras(float *a, float *s, float *b, int m, int n);
       static float *trasat(float *a, float *s, float *r__, int m, int n);
       static float *trata(float *a, float *r__, int m, int n);
       static float *trats(float *a, float *s, float *b, int m, int n);
       static float *tratsa(float *a, float *s, float *r__, int m, int n);
       static float *trchlu(float *a, float *b, int n);
       static float *trchul(float *a, float *b, int n);
       static float *trinv(float *t, float *s, int n);
       static float *trla(float *u, float *a, float *b, int m, int n);
       static float *trlta(float *u, float *a, float *b, int m, int n);
       static float *trpck(float *s, float *u, int n);
       static float *trqsq(float *q, float *s, float *r__, int m);
       static float *trsa(float *s, float *a, float *b, int m, int n);
       static float *trsinv(float *g, float *gi, int n);
       static float *trsmlu(float *u, float *s, int n);
       static float *trsmul(float *g, float *gi, int n);
       static float *trupck(float *u, float *s, int m);
       static float *trsat(float *s, float *a, float *b, int m, int n);

// ---   double version

       static double *traat (double *a, double *s, int m, int n);
       static double *tral  (double *a, double *u, double *b, int m, int n);
       static double *tralt (double *a, double *u, double *b, int m, int n);
       static double *tras  (double *a, double *s, double *b, int m, int n);
       static double *trasat(double *a, double *s, double *r__, int m, int n);
       static double *trata (double *a, double *r__, int m, int n);
       static double *trats (double *a, double *s, double *b, int m, int n);
       static double *tratsa(double *a, double *s, double *r__, int m, int n);
       static double *trchlu(double *a, double *b, int n);
       static double *trchul(double *a, double *b, int n);
       static double *trinv (double *t, double *s, int n);
       static double *trla  (double *u, double *a, double *b, int m, int n);
       static double *trlta (double *u, double *a, double *b, int m, int n);
       static double *trpck (double *s, double *u, int n);
       static double *trqsq (double *q, double *s, double *r__, int m);
       static double *trsa  (double *s, double *a, double *b, int m, int n);
       static double *trsinv(double *g, double *gi, int n);
       static double *trsmlu(double *u, double *s, int n);
       static double *trsmul(double *g, double *gi, int n);
       static double *trupck(double *u, double *s, int m);
       static double *trsat (double *s, double *a, double *b, int m, int n);

    ClassDef(StCL,0)  //Interface to matrix routines

};

//___________________________________________________________________________
inline void StCL::mxmad(float *a, float *b, float *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
  mxmad_0_(0, a, b, c, i, j, k);   }

//___________________________________________________________________________
inline void StCL:: mxmad1(float *a, float *q, float *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad1.gif"> </P> End_Html // 
 mxmad_0_(1, a, q, c, i, j, k);  }

//___________________________________________________________________________
inline void StCL::mxmad2(float *p, float *b, float *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad2.gif"> </P> End_Html // 
 mxmad_0_(2, p, b, c, i, j, k);  }

//___________________________________________________________________________
inline void StCL::mxmad3(float *p, float *q, float *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad3.gif"> </P> End_Html // 
 mxmad_0_(3, p, q, c, i, j, k);  }

//___________________________________________________________________________
inline void StCL::mxmpy(float *a, float *b, float *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmpy.gif"> </P> End_Html // 
  mxmad_0_(4, a, b, c, i, j, k); }

//___________________________________________________________________________
inline void StCL::mxmpy1(float *a, float *q, float *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmpy1.gif"> </P> End_Html // 
 mxmad_0_(5, a, q, c, i, j, k);  }

//___________________________________________________________________________
inline void StCL::mxmpy2(float *p, float *b, float *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmpy2.gif"> </P> End_Html // 
 mxmad_0_(6, p, b, c, i, j, k); }

//___________________________________________________________________________
inline void StCL::mxmpy3(float *p, float *q, float *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmpy3.gif"> </P> End_Html // 
 mxmad_0_(7, p, q, c, i, j, k); }

//___________________________________________________________________________
inline void StCL::mxmub(float *a, float *b, float *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmub.gif"> </P> End_Html // 
 mxmad_0_(8, a, b, c, i, j, k);  }

//___________________________________________________________________________
inline void StCL::mxmub1(float *a, float *q, float *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmub1.gif"> </P> End_Html // 
  mxmad_0_(9, a, q, c, i, j, k); }

//___________________________________________________________________________
inline void StCL::mxmub2(float *p, float *b, float *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmub2.gif"> </P> End_Html // 
  mxmad_0_(10, p, b, c, i, j, k); }

//___________________________________________________________________________
inline void StCL::mxmub3(float *p, float *q, float *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmub3.gif"> </P> End_Html // 
  mxmad_0_(11, p, q, c, i, j, k); }

//___________________________________________________________________________
inline void StCL::mxmlrt(float *a, float *b, float *x, int ni, int nj)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmlrt.gif"> </P> End_Html // 
  mxmlrt_0_(0, a, b, x, ni, nj); }

//___________________________________________________________________________
inline void StCL::mxmltr(float *a, float *b, float *x, int ni, int nj)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmltr.gif"> </P> End_Html // 
  mxmlrt_0_(1, a, b, x, ni, nj);   }


//--   double version --

//___________________________________________________________________________
inline void StCL::mxmad(double *a, double *b, double *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
  mxmad_0_(0, a, b, c, i, j, k);   }

//___________________________________________________________________________
inline void StCL:: mxmad1(double *a, double *b, double *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
 mxmad_0_(1, a, b, c, i, j, k);  }

//___________________________________________________________________________
inline void StCL::mxmad2(double *a, double *b, double *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
 mxmad_0_(2, a, b, c, i, j, k);  }

//___________________________________________________________________________
inline void StCL::mxmad3(double *a, double *b, double *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
 mxmad_0_(3, a, b, c, i, j, k);  }

//___________________________________________________________________________
inline void StCL::mxmpy(double *a, double *b, double *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
  mxmad_0_(4, a, b, c, i, j, k); }

//___________________________________________________________________________
inline void StCL::mxmpy1(double *a, double *b, double *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
 mxmad_0_(5, a, b, c, i, j, k);  }

//___________________________________________________________________________
inline void StCL::mxmpy2(double *a, double *b, double *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
 mxmad_0_(6, a, b, c, i, j, k); }

//___________________________________________________________________________
inline void StCL::mxmpy3(double *a, double *b, double *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
 mxmad_0_(7, a, b, c, i, j, k); }

//___________________________________________________________________________
inline void StCL::mxmub(double *a, double *b, double *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
 mxmad_0_(8, a, b, c, i, j, k);  }

//___________________________________________________________________________
inline void StCL::mxmub1(double *a, double *b, double *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
  mxmad_0_(9, a, b, c, i, j, k); }

//___________________________________________________________________________
inline void StCL::mxmub2(double *a, double *b, double *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
  mxmad_0_(10, a, b, c, i, j, k); }

//___________________________________________________________________________
inline void StCL::mxmub3(double *a, double *b, double *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
  mxmad_0_(11, a, b, c, i, j, k); }

//___________________________________________________________________________
inline void StCL::mxmlrt(double *a, double *b, double *c, int ni, int nj)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
 mxmlrt_0_(0, a, b, c, ni, nj); }

//___________________________________________________________________________
inline void StCL::mxmltr(double *a, double *b, double *c, int ni, int nj)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
 mxmlrt_0_(1, a, b, c, ni, nj);   }

// ----

//________________________________________________________
inline float *StCL::ucopy(const float *b, float *a, int n)
{ if (n <= 0) return 0; memcpy(a,b,n*sizeof(float)); return a;}

//________________________________________________________
inline float *StCL::ucopy(const double *b, float *a, int n)
{ 
  if (n <= 0) return 0; 
  for (int i=0;i<n;i++,a++,b++) *a = float(*b);
  return a;
}

//________________________________________________________
inline double *StCL::ucopy(const float *b, double *a, int n)
{ 
  if (n <= 0) return 0; 
  for (int i=0;i<n;i++,a++,b++) *a = double(*b);
  return a;
}

//________________________________________________________
inline double *StCL::ucopy(const double *b, double *a, int n)
{ if (n <= 0) return 0; memcpy(a,b,n*sizeof(double)); return a;}

//________________________________________________________
inline float *StCL::vadd(float *b, float *c,  float *a, int n)
{ 
  if (n <= 0)  return 0;
  for (int i=0;i<n;i++) a[i] = b[i] + c[i];
  return a;
}

//________________________________________________________
inline double *StCL::vadd(double *b, double *c,  double *a, int n)
{ 
  if (n <= 0)  return 0;
  for (int i=0;i<n;i++) a[i] = b[i] + c[i];
  return a;
}

//________________________________________________________
inline float *StCL::vsub(float *a, float *b, float *x, int n)
{ 
  if (n <= 0) return 0;
  for (int i=0;i<n;i++) x[i] = a[i]-b[i];
  return x;  
}

//________________________________________________________
inline double *StCL::vsub(double *a, double *b, double *x, int n)
{ 
  if (n <= 0) return 0;
  for (int i=0;i<n;i++) x[i] = a[i]-b[i];
  return x;  
}

//________________________________________________________
inline float *StCL::vcopyn(float *a, float *x, int n)
{ 
  if (n <= 0) return 0;
  for (int i=0;i<n;i++) x[i] = -a[i];
  return x;
}
//________________________________________________________
inline double *StCL::vcopyn(double *a, double *x, int n)
{ 
  if (n <= 0) return 0;
  for (int i=0;i<n;i++) x[i] = -a[i];
  return x;
}

//________________________________________________________
inline float *StCL::uzero(float *a, int n1, int n2)
{ 
  // Attention: n1, n2 is "Fortran-like index
  // namely the first element has index "1" 
  if (n2-n1 < 0) return 0;
  return (float *)memset(&a[n1-1],0,(n2-n1+1)*sizeof(float));
}

//________________________________________________________
inline double *StCL::uzero(double *a, int n1, int n2)
{ 
  // Attention: n1, n2 is "Fortran-like index
  // namely the first element has index "1" 
  if (n2-n1 < 0) return 0;
  return (double *)memset(&a[n1-1],0,(n2-n1+1)*sizeof(double));
}

//________________________________________________________
inline float *StCL::vzero(float *a, int n1)
{ 
  if (n1 <= 0) return 0;
  return (float *)memset(a,0,n1*sizeof(float));
}

//________________________________________________________
inline double *StCL::vzero(double *a, int n1)
{ 
  if (n1 <= 0) return 0;
  return (double *)memset(a,0,n1*sizeof(double));
}

#endif
