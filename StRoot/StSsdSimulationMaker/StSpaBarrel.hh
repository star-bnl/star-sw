#ifndef STSPABARREL_HH
#define STSPABARREL_HH
#include <stdlib.h>
#include <math.h>
#include "tables/St_sdm_geom_par_Table.h"
#include "tables/St_sls_ctrl_Table.h"
#include "tables/St_sdm_calib_par_Table.h"

#include "Random.hh"
#include "RanGauss.hh"
#include "StSpaWafer.hh"


class St_sls_strip;
class St_sdm_calib_db;
class St_sdm_condition_db;
class St_spa_strip;

class StSpaBarrel
{
 public:
  StSpaBarrel(sdm_geom_par_st  *geom_par, sdm_calib_par_st *cal_par);
  ~StSpaBarrel();

  void  setSpaParameters(sdm_geom_par_st  *geom_par);
  int   readStripFromTable(St_sls_strip *sls_strip);
  int   readNoiseFromTable(St_sdm_calib_db *sdm_noise);
  int   readConditionDbFromTable(St_sdm_condition_db *sdm_condition);
  int   writeStripToTable(St_spa_strip *spa_strip);
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
