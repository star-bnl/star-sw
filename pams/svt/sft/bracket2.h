
#define NUM_POINTS 64

typedef struct _tagPoint
{
   float  x;
   float  y;
} POINT;

int bracket2 (float (*func) (double), float, float, 
              float *, float *, float *, float *, float *, float *); 

int quad_fit (double *x, double *y, double *dx, double *dy, 
              int n, double *a, double *da, double *chi2);

#define max(a,b) ((a>b)?(a):(b))
