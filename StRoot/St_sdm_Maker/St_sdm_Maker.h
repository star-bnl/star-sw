#ifndef STAR_St_sdm_Maker
#define STAR_St_sdm_Maker
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_spa_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

class TRandom;
class TString;
class TFile;

class St_sdm_condition_par;
class St_sdm_geom_par;
class St_sdm_calib_par;
class St_sdm_calib_db;
class St_sdm_condition_db;

class St_sdm_Maker : public StMaker
{
 private:
  Int_t                 mSsdLayer;
  Int_t                 mSsdTotLadder;
  Int_t                 mSsdTotWafer;
  Int_t                 mSsdTotPlane; 
  Int_t                 mSsdTotA128;
  Int_t                 mSsdTotStrip;
  St_sdm_condition_par *m_cond_par;//!
  St_sdm_geom_par      *m_geom_par;//!
  St_sdm_calib_par     *m_cal_par;//!
  //  St_sdm_calib_db      *m_noise;//!
  //  St_sdm_condition_db  *m_condition;//!
  TString               *m_DBPath;//!
  TString               *m_ParPath;//!
  //  TFile                 *m_CondDBFile;//!
  //  TFile                 *m_CalibDBFile;//!
  TRandom               *m_DBRandom;//!;
  void                  InitConditionPar();
  void                  InitGeomPar();
  void                  InitCalibPar();
  void                  BuildCalibDB();
  void                  BuildConditionDB();
  //  void                  WriteCalibDB();
  //  void                  WriteConditionDB();
  Bool_t                LoadConditionPar();
  Bool_t                LoadGeomPar();
  Bool_t                LoadCalibPar();
  Int_t                 ConvertStripId(Int_t st);
  Int_t                 WaferNumbToIdWafer(Int_t wafer_numb);
 public: 
  St_sdm_Maker(const char *name="sdm_strip");
  virtual       ~St_sdm_Maker();
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual void   PrintInfo();
  void           SetParamPath(Char_t *ParPath);
  void           SetDBPath(Char_t *DBPath);
  ClassDef(St_sdm_Maker,0)   //StAF chain virtual base class for Makers
};
#endif
    






