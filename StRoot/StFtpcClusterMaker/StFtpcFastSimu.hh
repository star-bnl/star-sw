#ifndef STAR_StFtpcFastSimu
#define STAR_StFtpcFastSimu
#include "g2t_ftp_hit.h"
#include "g2t_track.h"
#include "g2t_vertex.h"
#include "ffs_fspar.h"
#include "ffs_gaspar.h"
#include "ffs_gepoint.h"
#include "fcl_fppoint.h"

#define TRUE 1
#define FALSE 0
#define MXMROW 150000
#define SIZE 20
// #define sqr(x) ((x)*(x))

class HepRandom;

class StFtpcFastSimu
{
 private:
  float Va;
  float Vhm[4];
  float Tbm[4];
  float s_rad[4];
  float s_azi[4];
  float slong;
  float anode_width;
  float prf_wid;
  float shaper_wid;
  float ri;
  float ra;
  float padrows;
  float z1;
  float z2;
  float phimin;
  float phisec;
  float sector_phi_min;
  float sector_phi_max;
  int nrow[SIZE][MXMROW];
  int nrowmax[SIZE];
  double myModulo(double x1, double x2)
    {
      return x1-(double)(int)(x1/x2)*x2;
    }
 public:
  StFtpcFastSimu(G2T_FTP_HIT_ST* g2t_ftp_hit,
		 int *g2t_ftp_hit_nok,
		 G2T_TRACK_ST* g2t_track,
		 int *g2t_track_nok,
		 G2T_VERTEX_ST* g2t_vertex,
		 FFS_FSPAR_ST* ffs_fspar,
		 FFS_GASPAR_ST* ffs_gaspar,
		 FFS_GEPOINT_ST* ffs_gepoint,
		 int *ffs_gepoint_nok,
		 int ffs_gepoint_maxlen,
		 FCL_FPPOINT_ST* fcl_fppoint,
		 int *fcl_fppoint_nok,
		 int fcl_fppoint_maxlen);
  ~StFtpcFastSimu();
  int ffs_gen_padres(int *g2t_ftp_hit_nok, 
		     G2T_FTP_HIT_ST *g2t_ftp_hit, 
		     int *ffs_gepoint_nok,
		     int ffs_gepoint_maxlen,
		     FFS_GEPOINT_ST *ffs_gepoint,
		     int *fcl_fppoint_nok,
		     int fcl_fppoint_maxlen,
		     FCL_FPPOINT_ST *fcl_fppoint);
  int ffs_hit_rd(int *g2t_ftp_hit_nok,
		 G2T_FTP_HIT_ST *g2t_ftp_hit,
		 int *g2t_track_nok, 
		 G2T_TRACK_ST *g2t_track,
		 G2T_VERTEX_ST *g2t_vertex,
		 int *ffs_gepoint_nok,
		 int ffs_gepoint_maxlen,
		 FFS_GEPOINT_ST *ffs_gepoint,
		 int *fcl_fppoint_nok,
		 int fcl_fppoint_maxlen,
		 FCL_FPPOINT_ST *fcl_fppoint);
  int ffs_hit_smear(float phi, 
		    float xi, 
		    float yi, 
		    float zi, 
		    float *xo, 
		    float *yo, 
		    float *zo,
		    float st_dev_l_hit, 
		    float st_dev_tr_hit,
		    float *st_dev_z,
		    float *st_dev_x,
		    float *st_dev_y,  
		    HepRandom quasiRandom);
  int ffs_ini(FFS_FSPAR_ST *ffs_fspar,   
	      FFS_GASPAR_ST *ffs_gaspar);
  int ffs_merge_tagger(int *ffs_gepoint_nok,
		       int ffs_gepoint_maxlen,
		       FFS_GEPOINT_ST *ffs_gepoint,
		       int *fcl_fppoint_nok,
		       int fcl_fppoint_maxlen,
		       FCL_FPPOINT_ST *fcl_fppoint);
  int ffs_tag(int *ffs_gepoint_nok,
	      int ffs_gepoint_maxlen, 
	      FFS_GEPOINT_ST *ffs_gepoint,
	      int *fcl_fppoint_nok,
	      int fcl_fppoint_maxlen,
	      FCL_FPPOINT_ST *fcl_fppoin);
};

#endif
