/*!
 * **********************************************************
 *  StPmdCalibrationConstMaker.h
 *
 * Author: Raghunath Sahoo, Institute of Physics; Bhubaneswar.
 *
 ************************************************************
 *
 * Description: Base class for PMD Calibration Constant Maker
 *
 ************************************************************
 * Initial version: 28th Aug 2003
 ************************************************************/
#ifndef STAR_StPmdCalibConstMaker
#define STAR_StPmdCalibConstMaker
#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "tables/St_pmdBrdMipCalib_Table.h"
#include "tables/St_pmdCalSummary_Table.h"
#include "TString.h" 
#include "StPmdUtil/StPmdGeom.h"
#include "StPmdUtil/StPmdDBUtil.h"
#define PMD_CELL_NEIGHBOUR 6
#define MIP_MIN_ENTRY 1000  // new entry
#define MIP_CH_MAX 100// check it

class StTrack;
class StEvent;

class StPmdCollection;
class StPmdDetector;
class StPmdCalibConstMaker: public StMaker{

 private:
  static Int_t neibx[PMD_CELL_NEIGHBOUR];
  static Int_t neiby[PMD_CELL_NEIGHBOUR];
  static Int_t imax[2*PMD_CRAMS_MAX];
  static Int_t jmax[2*PMD_CRAMS_MAX];
  int mDate;
  int mTime;  
  Bool_t  mSaveCalibToDB;
  Bool_t  mOptHist;
  Float_t mMipPeak[PMD_BOARD_MAX][PMD_BOARD_CH_MAX];
  Float_t mMipWidth[PMD_BOARD_MAX][PMD_BOARD_CH_MAX];
  
  StPmdGeom * mPmdGeom;
  StPmdDBUtil * mPmdDbUtil;
  void  InitMipParams();
  void  ClearHists();
  void  ClearMipArray();
  void  BookHistograms();   // Book hists for MIP
  void  GetIsoHit(StPmdDetector*, StPmdDetector*); // Find Isolated hits
 protected:

  //! booking Pmd cluster histograms
  TH1F *mMipEnergy[2*PMD_CRAMS_MAX][PMD_ROW_MAX][PMD_COL_MAX];     //! deposited energy of isolated cells
  Float_t normFactor[2*PMD_CRAMS_MAX][PMD_ROW_MAX][PMD_COL_MAX];   
  Float_t MPV_Entry[2*PMD_CRAMS_MAX][PMD_ROW_MAX][PMD_COL_MAX];   
 public: 
  StPmdCalibConstMaker(const char *name="PmdCalib"); 
  virtual       ~StPmdCalibConstMaker();///< Default destructor
  
  virtual Int_t Init(); ///< Init method
  virtual Int_t Make(); ///< Make mathod - process each event
  virtual Int_t FindMipParameters(); // Obtain MIP params from fitting
  virtual Int_t Finish();///< Finish method - save final numbers
  
  void    SaveCalibration();   //Save calibration constants to DB
  void    SetSaveCalibToDB(Bool_t a)     {   mSaveCalibToDB = a; }
  void    SetOptHist(Bool_t a)     {   mOptHist = a; }

  ClassDef(StPmdCalibConstMaker, 1) 
    };
    
#endif
    
















    
