// $Id: StFtpcFastSimu.hh,v 1.8 2000/09/18 14:26:49 hummler Exp $
//
// $Log: StFtpcFastSimu.hh,v $
// Revision 1.8  2000/09/18 14:26:49  hummler
// expand StFtpcParamReader to supply data for slow simulator as well
// introduce StFtpcGeantReader to separate g2t tables from simulator code
// implement StFtpcGeantReader in StFtpcFastSimu
//
// Revision 1.7  2000/08/03 14:39:00  hummler
// Create param reader to keep parameter tables away from cluster finder and
// fast simulator. StFtpcClusterFinder now knows nothing about tables anymore!
//
// Revision 1.6  2000/02/04 13:49:42  hummler
// upgrade ffs:
// -remove unused fspar table
// -make hit smearing gaussian with decent parameters and static rand engine
// -separate hit smearing from cluster width calculation
//
// Revision 1.5  2000/02/02 15:40:08  hummler
// make hit smearing gaussian instead of box-shaped
//
// Revision 1.4  2000/02/02 15:20:37  hummler
// correct acceptance at sector boundaries,
// take values from fcl_det
//
// Revision 1.3  2000/01/03 12:48:59  jcs
// Add CVS Id strings
//

#ifndef STAR_StFtpcFastSimu
#define STAR_StFtpcFastSimu
#include "ffs_gepoint.h"
#include "fcl_fppoint.h"

#define TRUE 1
#define FALSE 0
#define MXMROW 150000
#define SIZE 20

class RandGauss;
class StFtpcParamReader;
class StFtpcGeantReader;

class StFtpcFastSimu
{
 private:
  StFtpcParamReader *mParam;
  StFtpcGeantReader *mGeant;
  float Va;
  float Vhm[4];
  float Tbm[4];
  float s_rad[4];
  float s_azi[4];
  float err_rad[4];
  float err_azi[4];
  float slong;
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
  StFtpcFastSimu(FFS_GEPOINT_ST* ffs_gepoint,
		 int *ffs_gepoint_nok,
		 int ffs_gepoint_maxlen,
		 FCL_FPPOINT_ST* fcl_fppoint,
		 int *fcl_fppoint_nok,
		 int fcl_fppoint_maxlen,
		 StFtpcGeantReader *geantReader,
		 StFtpcParamReader *paramReader);
  ~StFtpcFastSimu();
  int ffs_gen_padres(int *ffs_gepoint_nok,
		     int ffs_gepoint_maxlen,
		     FFS_GEPOINT_ST *ffs_gepoint,
		     int *fcl_fppoint_nok,
		     int fcl_fppoint_maxlen,
		     FCL_FPPOINT_ST *fcl_fppoint);
  int ffs_hit_rd(int *ffs_gepoint_nok,
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
		    RandGauss *quasiRandom);
  int ffs_ini();
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
	      FCL_FPPOINT_ST *fcl_fppoint);
};

#endif
