#ifndef STAR_StCL
#define STAR_StCL
//*-- Author :    Valery Fine(fine@bnl.gov)   25/09/99  
//
// The set of methods to work with the plain matrix / vector
// "derived" from  http://wwwinfo.cern.ch/asdoc/shortwrupsdir/f110/top.html 
//
// $Id: StCL.h,v 1.7 1999/11/18 14:35:20 fine Exp $
// $Log: StCL.h,v $
// Revision 1.7  1999/11/18 14:35:20  fine
// Class comment introduced
//
// Revision 1.6  1999/11/09 01:10:46  fine
// new method int *ucopy(const *int, ...) has been introduced
//
// Revision 1.5  1999/10/27 23:57:57  fine
// Clean up: const has been introduced instead of non-const
//
// Revision 1.4  1999/10/17 20:45:57  fine
// vadd methods added
//
// Revision 1.3  1999/09/29 02:30:49  fine
// Change return type from void to float/double
//
// Revision 1.2  1999/09/29 00:31:51  fine
// RMath class has been replaced with StCL one
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
    static int    *ucopy(const int    *a, int    *b, int n);
    static float  *ucopy(const float  *a, float  *b, int n);
    static double *ucopy(const float  *a, double *b, int n);
    static float  *ucopy(const double *a, float  *b, int n);
    static double *ucopy(const double *a, double *b, int n);

    static float  *uzero(float *a,  int n1, int n2);
    static double *uzero(double *a, int n1, int n2);

    static float  *vzero(float *a,  int n2);
    static double *vzero(double *a, int n2);

    static float  *vadd(const float *b,  const float *c,  float *a, int n);
    static double *vadd(const double *b, const double *c, double *a, int n);

    static float  *vadd(const float *b,  const double *c, float *a, int n);
    static double *vadd(const double *b, const float *c,  double *a, int n);

    static float  *vsub(const float *a,  const float *b,  float *x, int n);
    static double *vsub(const double *a, const double *b, double *x, int n);

    static float  *vcopyn(const float *a,  float *x, int n);
    static double *vcopyn(const double *a, double *x, int n);

    static float *mxmad_0_(int n, const float *a, const float *b, float *c, int i, int j, int k);

    static float *mxmad( const float *a, const float *b, float *c, int i, int j, int k);
    static float *mxmad1(const float *a, const float *b, float *c, int i, int j, int k);
    static float *mxmad2(const float *a, const float *b, float *c, int i, int j, int k);
    static float *mxmad3(const float *a, const float *b, float *c, int i, int j, int k);
    static float *mxmpy( const float *a, const float *b, float *c, int i, int j, int k);
    static float *mxmpy1(const float *a, const float *b, float *c, int i, int j, int k);
    static float *mxmpy2(const float *a, const float *b, float *c, int i, int j, int k);
    static float *mxmpy3(const float *a, const float *b, float *c, int i, int j, int k);
    static float *mxmub( const float *a, const float *b, float *c, int i, int j, int k);
    static float *mxmub1(const float *a, const float *b, float *c, int i, int j, int k);
    static float *mxmub2(const float *a, const float *b, float *c, int i, int j, int k);
    static float *mxmub3(const float *a, const float *b, float *c, int i, int j, int k);

    static float *mxmlrt_0_(int n__, const float *a, const float *b, float *c, int ni,int nj);
    static float *mxmlrt(const float *a, const float *b, float *c, int ni, int nj);
    static float *mxmltr(const float *a, const float *b, float *c, int ni, int nj);
    static float *mxtrp(const float *a, float *b, int i, int j);

    static double *mxmad_0_(int n, const double *a, const double *b, double *c, int i, int j, int k);

    static double *mxmad (const double *a, const double *b, double *c, int i, int j, int k);
    static double *mxmad1(const double *a, const double *b, double *c, int i, int j, int k);
    static double *mxmad2(const double *a, const double *b, double *c, int i, int j, int k);
    static double *mxmad3(const double *a, const double *b, double *c, int i, int j, int k);
    static double *mxmpy (const double *a, const double *b, double *c, int i, int j, int k);
    static double *mxmpy1(const double *a, const double *b, double *c, int i, int j, int k);
    static double *mxmpy2(const double *a, const double *b, double *c, int i, int j, int k);
    static double *mxmpy3(const double *a, const double *b, double *c, int i, int j, int k);
    static double *mxmub (const double *a, const double *b, double *c, int i, int j, int k);
    static double *mxmub1(const double *a, const double *b, double *c, int i, int j, int k);
    static double *mxmub2(const double *a, const double *b, double *c, int i, int j, int k);
    static double *mxmub3(const double *a, const double *b, double *c, int i, int j, int k);

    static double *mxmlrt_0_(int n__, const double *a, const double *b, double *c, int ni,int nj);
    static double *mxmlrt(const double *a, const double *b, double *c, int ni, int nj);
    static double *mxmltr(const double *a, const double *b, double *c, int ni, int nj);
    static double *mxtrp(const double *a, double *b, int i, int j);

// * TR pack

       static float *traat(float *a, float *s, int m, int n);
       static float *tral(float *a, float *u, float *b, int m, int n);
       static float *tralt(float *a, float *u, float *b, int m, int n);
       static float *tras(float *a, float *s, float *b, int m, int n);
       static float *trasat(float *a, float *s, float *r__, int m, int n);
       static float *trasat(double *a, float *s, float *r__, int m, int n);
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

    ClassDef(StCL,0)  //C++ replacement for CERNLIB matrix / triangle matrix packages: F110 and F112

};

//___________________________________________________________________________
inline float *StCL::mxmad(const float *a, const float *b, float *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
  return mxmad_0_(0, a, b, c, i, j, k);   }

//___________________________________________________________________________
inline float *StCL::mxmad1(const float *a, const float *q, float *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad1.gif"> </P> End_Html // 
 return mxmad_0_(1, a, q, c, i, j, k);  }

//___________________________________________________________________________
inline float *StCL::mxmad2(const float *p, const float *b, float *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad2.gif"> </P> End_Html // 
 return mxmad_0_(2, p, b, c, i, j, k);  }

//___________________________________________________________________________
inline float *StCL::mxmad3(const float *p, const float *q, float *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad3.gif"> </P> End_Html // 
 return mxmad_0_(3, p, q, c, i, j, k);  }

//___________________________________________________________________________
inline float *StCL::mxmpy(const float *a, const float *b, float *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmpy.gif"> </P> End_Html // 
  return mxmad_0_(4, a, b, c, i, j, k); }

//___________________________________________________________________________
inline float *StCL::mxmpy1(const float *a, const float *q, float *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmpy1.gif"> </P> End_Html // 
 return mxmad_0_(5, a, q, c, i, j, k);  }

//___________________________________________________________________________
inline float *StCL::mxmpy2(const float *p, const float *b, float *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmpy2.gif"> </P> End_Html // 
 return mxmad_0_(6, p, b, c, i, j, k); }

//___________________________________________________________________________
inline float *StCL::mxmpy3(const float *p, const float *q, float *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmpy3.gif"> </P> End_Html // 
 return mxmad_0_(7, p, q, c, i, j, k); }

//___________________________________________________________________________
inline float *StCL::mxmub(const float *a, const float *b, float *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmub.gif"> </P> End_Html // 
 return mxmad_0_(8, a, b, c, i, j, k);  }

//___________________________________________________________________________
inline float *StCL::mxmub1(const float *a, const float *q, float *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmub1.gif"> </P> End_Html // 
  return mxmad_0_(9, a, q, c, i, j, k); }

//___________________________________________________________________________
inline float *StCL::mxmub2(const float *p, const float *b, float *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmub2.gif"> </P> End_Html // 
  return mxmad_0_(10, p, b, c, i, j, k); }

//___________________________________________________________________________
inline float *StCL::mxmub3(const float *p, const float *q, float *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmub3.gif"> </P> End_Html // 
  return mxmad_0_(11, p, q, c, i, j, k); }

//___________________________________________________________________________
inline float *StCL::mxmlrt(const float *a, const float *b, float *x, int ni, int nj)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmlrt.gif"> </P> End_Html // 
  return mxmlrt_0_(0, a, b, x, ni, nj); }

//___________________________________________________________________________
inline float *StCL::mxmltr(const float *a, const float *b, float *x, int ni, int nj)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmltr.gif"> </P> End_Html // 
  return mxmlrt_0_(1, a, b, x, ni, nj);   }


//--   double version --

//___________________________________________________________________________
inline double *StCL::mxmad(const double *a, const double *b, double *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
  return mxmad_0_(0, a, b, c, i, j, k);   }

//___________________________________________________________________________
inline double *StCL:: mxmad1(const double *a, const double *b, double *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
 return mxmad_0_(1, a, b, c, i, j, k);  }

//___________________________________________________________________________
inline double *StCL::mxmad2(const double *a, const double *b, double *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
 return mxmad_0_(2, a, b, c, i, j, k);  }

//___________________________________________________________________________
inline double *StCL::mxmad3(const double *a, const double *b, double *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
 return mxmad_0_(3, a, b, c, i, j, k);  }

//___________________________________________________________________________
inline double *StCL::mxmpy(const double *a, const double *b, double *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
  return mxmad_0_(4, a, b, c, i, j, k); }

//___________________________________________________________________________
inline double *StCL::mxmpy1(const double *a, const double *b, double *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
 return mxmad_0_(5, a, b, c, i, j, k);  }

//___________________________________________________________________________
inline double *StCL::mxmpy2(const double *a, const double *b, double *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
 return mxmad_0_(6, a, b, c, i, j, k); }

//___________________________________________________________________________
inline double *StCL::mxmpy3(const double *a, const double *b, double *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
 return mxmad_0_(7, a, b, c, i, j, k); }

//___________________________________________________________________________
inline double *StCL::mxmub(const double *a, const double *b, double *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
 return mxmad_0_(8, a, b, c, i, j, k);  }

//___________________________________________________________________________
inline double *StCL::mxmub1(const double *a, const double *b, double *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
  return mxmad_0_(9, a, b, c, i, j, k); }

//___________________________________________________________________________
inline double *StCL::mxmub2(const double *a, const double *b, double *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
  return mxmad_0_(10, a, b, c, i, j, k); }

//___________________________________________________________________________
inline double *StCL::mxmub3(const double *a, const double *b, double *c, int i, int j, int k)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
  return mxmad_0_(11, a, b, c, i, j, k); }

//___________________________________________________________________________
inline double *StCL::mxmlrt(const double *a, const double *b, double *c, int ni, int nj)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
return  mxmlrt_0_(0, a, b, c, ni, nj); }

//___________________________________________________________________________
inline double *StCL::mxmltr(const double *a, const double *b, double *c, int ni, int nj)
{
 // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/mxpack_mxmad.gif"> </P> End_Html // 
return mxmlrt_0_(1, a, b, c, ni, nj);   }

// ----

//________________________________________________________
inline int  *StCL::ucopy(const int  *b, int  *a, int n)
{ if (n <= 0) return 0; memcpy(a,b,n*sizeof(int)); return a;}

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
inline float *StCL::vadd(const float *b, const float *c,  float *a, int n)
{ 
  if (n <= 0)  return 0;
  for (int i=0;i<n;i++) a[i] = b[i] + c[i];
  return a;
}

//________________________________________________________
inline double *StCL::vadd(const double *b, const double *c,  double *a, int n)
{ 
  if (n <= 0)  return 0;
  for (int i=0;i<n;i++) a[i] = b[i] + c[i];
  return a;
}

//________________________________________________________
inline float  *StCL::vadd(const float *b, const double *c,  float *a, int n)
{
  if (n <= 0)  return 0;
  for (int i=0;i<n;i++) a[i] = b[i] + c[i];
  return a;
}

//________________________________________________________
inline double *StCL::vadd(const double *b, const float *c,  double *a, int n)
{
  if (n <= 0)  return 0;
  for (int i=0;i<n;i++) a[i] = b[i] + c[i];
  return a;
}

//________________________________________________________
inline float *StCL::vsub(const float *a, const float *b, float *x, int n)
{ 
  if (n <= 0) return 0;
  for (int i=0;i<n;i++) x[i] = a[i]-b[i];
  return x;  
}

//________________________________________________________
inline double *StCL::vsub(const double *a, const double *b, double *x, int n)
{ 
  if (n <= 0) return 0;
  for (int i=0;i<n;i++) x[i] = a[i]-b[i];
  return x;  
}

//________________________________________________________
inline float *StCL::vcopyn(const float *a, float *x, int n)
{ 
  if (n <= 0) return 0;
  for (int i=0;i<n;i++) x[i] = -a[i];
  return x;
}
//________________________________________________________
inline double *StCL::vcopyn(const double *a, double *x, int n)
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
  return vzero(&a[n1-1],n2-n1+1);
}

//________________________________________________________
inline double *StCL::uzero(double *a, int n1, int n2)
{ 
  // Attention: n1, n2 is "Fortran-like index
  // namely the first element has index "1" 
  return vzero(&a[n1-1],n2-n1+1);
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
