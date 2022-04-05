#ifndef _numericalRecipes
#define _numericalRecipes

#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <math.h>

#define FREE_ARG char*

const float TINY = 1.0e-20;
const long NR_END = 1;

class numericalRecipes {
    public:

    numericalRecipes();
    virtual ~numericalRecipes();

    static float *vector(long nl, long nh);
    static float **matrix(long nrl, long nrh, long ncl, long nch);
    static float **convert_matrix(float *a, long nrl, long nrh, long ncl, long nch);
    static double *dvector(long nl, long nh);
    static double **dmatrix(long nrl, long nrh, long ncl, long nch);
    static int *ivector(long nl, long nh);
    static int **imatrix(long nrl, long nrh, long ncl, long nch);
    static float **submatrix(float **a, long oldrl, long oldrh, long oldcl, long oldch,
                      long newrl, long newcl);

    static void  invertMatrix( int n, double **matrix, double **inverse );
    static int   ludcmp(double **a, int n, int *indx, double *d);
    static void  lubksb(double **a, int n, int *indx, double b[]);
    static void covsrt(double **covar, int ma, int ia[], int mfit);
    static void gaussj(double **a, int n, double **b, int m);

    static void free_vector(float *v, long nl, long nh);
    static void free_dvector(double *v, long nl, long nh);
    static void free_ivector(int *v, long nl, long nh);
    static void free_matrix(float **m, long nrl, long nrh, long ncl, long nch);
    static void free_dmatrix(double **m, long nrl, long nrh, long ncl, long nch);
    static void free_imatrix(int **m, long nrl, long nrh, long ncl, long nch);
    static void free_submatrix(float **b, long nrl, long nrh, long ncl, long nch);
    static void free_convert_matrix(float **b, long nrl, long nrh, long ncl, long nch);
    static void nrerror(const char error_text[]);
};

#endif
