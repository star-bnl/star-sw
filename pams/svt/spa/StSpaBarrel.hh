#ifndef STSPABARREL_HH
#define STSPABARREL_HH
# include <stdiostream.h>
# include <stdlib.h>
# include <math.h>
# include "spa_am.h"
# include "Random.hh"
# include "RanGauss.hh"
# include "StSpaWafer.hh"

class StSpaBarrel
{
 public:
  StSpaBarrel(sdm_geom_par_st  *geom_par, sdm_calib_par_st *cal_par);
  ~StSpaBarrel();

  void  setSpaParameters(sdm_geom_par_st  *geom_par);
  int   readStripFromTable(table_head_st *sls_strip_h, sls_strip_st *sls_strip);
  int   readNoiseFromTable(table_head_st *noise_h, sdm_calib_db_st *noise);
  int   readConditionDbFromTable(table_head_st *condition_h, sdm_condition_db_st *condition);
  int   writeStripToTable(table_head_st *out_strip_h, spa_strip_st *out_strip,
				 table_head_st *sls_strip_h, sls_strip_st *sls_strip);
  void  addNoiseToStrip(sls_ctrl_st *ctrl);
  void  doDaqSimulation(sls_ctrl_st *ctrl);

  StSpaWafer** mWafers;
  
 private:
  int       mSsdLayer;
  int       mNLadder;
  int       mNWaferPerLadder;
  int       mNStripPerSide;
  RanGauss *mGaussDistribution;

  int       idWaferToWaferNumb(int idWafer);
  int       waferNumbToIdWafer(int waferNumb);
  
};
#endif
