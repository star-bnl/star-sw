#ifndef STAR_RMath
#define STAR_RMath

#include "Rtypes.h"

// http://wwwinfo.cern.ch/asdoc/shortwrupsdir/f110/top.html 

class RMath  {
  public:
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

#endif
