#ifndef STAR_RMath
#define STAR_RMath

#include "Rtypes.h"
#include <string.h>

// http://wwwinfo.cern.ch/asdoc/shortwrupsdir/f110/top.html 

class RMath  {
  public:
    static float  *ucopy(float *a,  float *b, int n);
    static double *ucopy(double *a, double *b, int n);

    static float  *uzero(float *a,  int n1, int n2);
    static double *uzero(double *a, int n1, int n2);

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

    static void mxmad_0_(int n, double *a, double *b, double *c, int i, int j, int k);

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

    ClassDef(RMath,0)  //Interface to matrix routines

};

//___________________________________________________________________________
inline void RMath::mxmad(float *a, float *b, float *c, int i, int j, int k)
{  mxmad_0_(0, a, b, c, i, j, k);   }

//___________________________________________________________________________
inline void RMath:: mxmad1(float *a, float *b, float *c, int i, int j, int k)
{ mxmad_0_(1, a, b, c, i, j, k);  }

//___________________________________________________________________________
inline void RMath::mxmad2(float *a, float *b, float *c, int i, int j, int k)
{ mxmad_0_(2, a, b, c, i, j, k);  }

//___________________________________________________________________________
inline void RMath::mxmad3(float *a, float *b, float *c, int i, int j, int k)
{ mxmad_0_(3, a, b, c, i, j, k);  }

//___________________________________________________________________________
inline void RMath::mxmpy(float *a, float *b, float *c, int i, int j, int k)
{  mxmad_0_(4, a, b, c, i, j, k); }

//___________________________________________________________________________
inline void RMath::mxmpy1(float *a, float *b, float *c, int i, int j, int k)
{ mxmad_0_(5, a, b, c, i, j, k);  }

//___________________________________________________________________________
inline void RMath::mxmpy2(float *a, float *b, float *c, int i, int j, int k)
{ mxmad_0_(6, a, b, c, i, j, k); }

//___________________________________________________________________________
inline void RMath::mxmpy3(float *a, float *b, float *c, int i, int j, int k)
{ mxmad_0_(7, a, b, c, i, j, k); }

//___________________________________________________________________________
inline void RMath::mxmub(float *a, float *b, float *c, int i, int j, int k)
{ mxmad_0_(8, a, b, c, i, j, k);  }

//___________________________________________________________________________
inline void RMath::mxmub1(float *a, float *b, float *c, int i, int j, int k)
{  mxmad_0_(9, a, b, c, i, j, k); }

//___________________________________________________________________________
inline void RMath::mxmub2(float *a, float *b, float *c, int i, int j, int k)
{  mxmad_0_(10, a, b, c, i, j, k); }

//___________________________________________________________________________
inline void RMath::mxmub3(float *a, float *b, float *c, int i, int j, int k)
{  mxmad_0_(11, a, b, c, i, j, k); }

//___________________________________________________________________________
inline void RMath::mxmlrt(float *a, float *b, float *c, int ni, int nj)
{  mxmlrt_0_(0, a, b, c, ni, nj); }

//___________________________________________________________________________
inline void RMath::mxmltr(float *a, float *b, float *c, int ni, int nj)
{  mxmlrt_0_(1, a, b, c, ni, nj);   }


//--   double version --

//___________________________________________________________________________
inline void RMath::mxmad(double *a, double *b, double *c, int i, int j, int k)
{  mxmad_0_(0, a, b, c, i, j, k);   }

//___________________________________________________________________________
inline void RMath:: mxmad1(double *a, double *b, double *c, int i, int j, int k)
{ mxmad_0_(1, a, b, c, i, j, k);  }

//___________________________________________________________________________
inline void RMath::mxmad2(double *a, double *b, double *c, int i, int j, int k)
{ mxmad_0_(2, a, b, c, i, j, k);  }

//___________________________________________________________________________
inline void RMath::mxmad3(double *a, double *b, double *c, int i, int j, int k)
{ mxmad_0_(3, a, b, c, i, j, k);  }

//___________________________________________________________________________
inline void RMath::mxmpy(double *a, double *b, double *c, int i, int j, int k)
{  mxmad_0_(4, a, b, c, i, j, k); }

//___________________________________________________________________________
inline void RMath::mxmpy1(double *a, double *b, double *c, int i, int j, int k)
{ mxmad_0_(5, a, b, c, i, j, k);  }

//___________________________________________________________________________
inline void RMath::mxmpy2(double *a, double *b, double *c, int i, int j, int k)
{ mxmad_0_(6, a, b, c, i, j, k); }

//___________________________________________________________________________
inline void RMath::mxmpy3(double *a, double *b, double *c, int i, int j, int k)
{ mxmad_0_(7, a, b, c, i, j, k); }

//___________________________________________________________________________
inline void RMath::mxmub(double *a, double *b, double *c, int i, int j, int k)
{ mxmad_0_(8, a, b, c, i, j, k);  }

//___________________________________________________________________________
inline void RMath::mxmub1(double *a, double *b, double *c, int i, int j, int k)
{  mxmad_0_(9, a, b, c, i, j, k); }

//___________________________________________________________________________
inline void RMath::mxmub2(double *a, double *b, double *c, int i, int j, int k)
{  mxmad_0_(10, a, b, c, i, j, k); }

//___________________________________________________________________________
inline void RMath::mxmub3(double *a, double *b, double *c, int i, int j, int k)
{  mxmad_0_(11, a, b, c, i, j, k); }

//___________________________________________________________________________
inline void RMath::mxmlrt(double *a, double *b, double *c, int ni, int nj)
{ mxmlrt_0_(0, a, b, c, ni, nj); }

//___________________________________________________________________________
inline void RMath::mxmltr(double *a, double *b, double *c, int ni, int nj)
{ mxmlrt_0_(1, a, b, c, ni, nj);   }

// ----

//________________________________________________________
inline float *RMath::ucopy(float *b, float *a, int n)
{ if (n <= 0) return 0; memcpy(a,b,n*sizeof(float)); return a;}

//________________________________________________________
inline double *RMath::ucopy(double *b, double *a, int n)
{ if (n <= 0) return 0; memcpy(a,b,n*sizeof(double)); return a;}

//________________________________________________________
inline float *RMath::vadd(float *b, float *c,  float *a, int n)
{ 
  if (n <= 0)  return 0;
  for (int i=0;i<n;i++) a[i] = b[i] + c[i];
  return a;
}

//________________________________________________________
inline double *RMath::vadd(double *b, double *c,  double *a, int n)
{ 
  if (n <= 0)  return 0;
  for (int i=0;i<n;i++) a[i] = b[i] + c[i];
  return a;
}

//________________________________________________________
inline float *RMath::vsub(float *a, float *b, float *x, int n)
{ 
  if (n <= 0) return 0;
  for (int i=0;i<n;i++) x[i] = a[i]-b[i];
  return x;  
}

//________________________________________________________
inline double *RMath::vsub(double *a, double *b, double *x, int n)
{ 
  if (n <= 0) return 0;
  for (int i=0;i<n;i++) x[i] = a[i]-b[i];
  return x;  
}

//________________________________________________________
inline float *RMath::vcopyn(float *a, float *x, int n)
{ 
  if (n <= 0) return 0;
  for (int i=0;i<n;i++) x[i] = -a[i];
  return x;
}
//________________________________________________________
inline double *RMath::vcopyn(double *a, double *x, int n)
{ 
  if (n <= 0) return 0;
  for (int i=0;i<n;i++) x[i] = -a[i];
  return x;
}

//________________________________________________________
inline float *RMath::uzero(float *a, int n1, int n2)
{ 
  if (n2-n1 < 0) return 0;
  return (float *)memset(a,0,(n2-n1+1)*sizeof(float));
}

//________________________________________________________
inline double *RMath::uzero(double *a, int n1, int n2)
{ 
  if (n2-n1 < 0) return 0;
  return (double *)memset(a,0,(n2-n1+1)*sizeof(double));
}

#endif
