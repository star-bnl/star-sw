/* External routines*/
#include "StarCallf77.h"
#define gufld F77_NAME(gufld,GUFLD)
extern "C" {
  void gufld(float *, float *);
  void prop_circle_param_(float *, float *, float *);
  void ev0_project_track_(float *, float *, float *, float *);
  void prop_update_track_param_(float *, float *, float *, float *, float *, float *);
  void prop_track_mom_(float *, float *);
  void prop_fine_approach_(float *, float *, float *, float (*)[3]);
  void prop_vzero_geom_(float *, float *, float *, float *, float *, float*, float *, long *);
  float prop_one_track( float * ,  float * , float * );
           }


