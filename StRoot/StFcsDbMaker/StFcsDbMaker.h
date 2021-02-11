/***************************************************************************
 * $Id: StFcsDbMaker.h,v 1.18 2021/02/09 21:54:23 akio Exp $
 * \author: akio ogawa
 ***************************************************************************
 *
 * Description: FCS DB access Maker
 *
 ***************************************************************************
 *
 * $Log: StFcsDbMaker.h,v $
 * Revision 1.18  2021/02/09 21:54:23  akio
 * Using StEnumeration
 *
 * Revision 1.17  2021/02/05 17:23:25  akio
 * Adding access to STAR offline DB tables.
 * Adding getFromName/getDetFromName from David.
 *
 * Revision 1.16  2021/01/05 18:15:01  akio
 * added setPedestal()
 *
 * Revision 1.15  2020/12/30 20:17:55  akio
 * adding SC map access
 *
 * Revision 1.14  2020/09/03 19:43:20  akio
 * Updating SC map and adding patchpanel & cable color map
 *
 * Revision 1.13  2020/05/29 18:53:40  akio
 * Adding EPD as PRES maps, STAR coordinate for 4x4 trigger patch, renming map files to be used for DAQ as Tonko specifies
 *
 * Revision 1.12  2020/05/04 15:48:22  akio
 * adding input file for DAQ
 *
 * Revision 1.11  2019/10/23 13:34:38  akio
 * Adding getZDepth, and take out Ecal front space (for SiPM/Fee) from offsets
 * so that x/z offsets are now pointing to actual ecal tower front & near beam corner.
 *
 * Revision 1.10  2019/07/10 06:13:34  akio
 * Adding reading of gains from text files
 *
 * Revision 1.9  2019/07/03 16:18:49  akio
 * correcting a comment
 *
 * Revision 1.8  2019/06/27 16:10:32  akio
 * adding getLocalXYinCell
 *
 * Revision 1.7  2019/06/26 18:03:07  akio
 * change default to mRun19=0 (futture full FCS)
 *
 * Revision 1.6  2019/06/25 16:38:59  akio
 * Fixed y offset for run19
 * Added setting run# and time dependent (for preshower yoffset only for now)
 *
 * Revision 1.5  2019/06/21 17:28:55  akio
 * dealing with 5cm offsent when leakyHcal
 *
 * Revision 1.4  2019/06/07 18:16:55  akio
 * *** empty log message ***
 *
 * Revision 1.3  2019/03/13 20:46:19  akio
 * formatting
 *
 * Revision 1.2  2019/03/13 20:29:30  akio
 * update for run19
 *
 * Revision 1.1  2018/11/14 16:50:13  akio
 * FCS codes in offline/upgrade/akio
 *
 **************************************************************************/

#ifndef STFCSDBMAKER_H
#define STFCSDBMAKER_H

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "StEvent/StEnumerations.h"
#include "StThreeVectorD.hh"
#include "StLorentzVectorD.hh"

struct fcsDetectorPosition_st;
struct fcsEcalGain_st;
struct fcsHcalGain_st;
struct fcsPresGain_st;
struct fcsEcalGainCorr_st;
struct fcsHcalGainCorr_st;
struct fcsPresValley_st;

class StFcsHit;
class StFcsCluster;
class StFcsPoint;

class StFcsDbMaker : public StMaker {
 public: 
  StFcsDbMaker(const Char_t *name="fcsDb");
  virtual       ~StFcsDbMaker();
  virtual Int_t  Init();
  virtual Int_t  InitRun(Int_t runNumber);
  virtual Int_t  Make();
  virtual Int_t  Finish();
  virtual void   Clear(const Char_t *opt);
  
  void setDbAccess(Int_t v=1);  // enable(1) or disable(0) offline DB access
  void setRun(Int_t run);       // set run# 
  void setDebug(Int_t debug=1); // debug mode, 0 for minimal message, >0 for more debug messages
  void setRun19(Int_t v=1);     // set run19 geometry, otherwise final run21
  void setLeakyHcal(Int_t v=1); // set leaky Hcal

  //! Getting the whole DB table
  fcsDetectorPosition_st* fcsDetectorPosition();
  fcsEcalGain_st* fcsEcalGain();
  fcsHcalGain_st* fcsHcalGain();
  fcsPresGain_st* fcsPresGain();
  fcsEcalGainCorr_st* fcsEcalGainCorr();
  fcsHcalGainCorr_st* fcsHcalGainCorr();
  fcsPresValley_st* fcsPresValley();

  //! Utility functions related to FCS ChannelGeometry
  Int_t maxDetectorId() const;              //! 6
  Int_t detectorId(int eh, int ns) const;   //! Ecal North=0, Ecal South=1, Hcal North=2, Hcal South=3, Pres=4,5
  Int_t ecalHcalPres(Int_t det) const;      //! Ecal=0, Hcal=1, Pres=2
  Int_t northSouth(Int_t det) const;        //! north or south side
  Int_t nRow(Int_t det) const;              //! number of rows
  Int_t nColumn(Int_t det) const;           //! number of column
  Int_t maxId(Int_t det) const;             //! maximum number of id
  Int_t getRowNumber(Int_t det, Int_t id) const;       //! get the row number for the channel
  Int_t getColumnNumber(Int_t det, Int_t id) const ;   //! get the column number for the channel
  Int_t getId(Int_t det, Int_t row, Int_t col) const ; //! get the id from row/col
  Int_t getDepCh(Int_t dep, Int_t ch) const ; //! get the DEP/ch id
  void getName(Int_t det, Int_t id, char name[]) const;
  void getName(Int_t ehp, Int_t ns, Int_t dep, Int_t ch, char name[]) const;
  static void getFromName(const char name[], Int_t& det, Int_t& id);
  static Int_t getDetFromName(const std::string& detname);

  //! Utility functions related to DetectorPosition
  StThreeVectorD getDetectorOffset(Int_t det) const;  //! get the offset of the detector
  Float_t getDetectorAngle(Int_t det) const;  //! get the angle of the detector
  Float_t getXWidth(Int_t det) const; //! get the X width of the cell
  Float_t getYWidth(Int_t det) const; //! get the Y width of the cell
  
  Float_t getZDepth(Int_t det) const;       // z depth of active detector
  Float_t getShowerMaxZ(Int_t det) const;   // default z[cm] from front face for where it measure x/y (shower max) 
  
  //! getting XY in local cell coordinate
  void getLocalXYinCell(StFcsHit* hit, Float_t &x, Float_t &y) const; 
  void getLocalXYinCell(Int_t det, Int_t id, Float_t &x, Float_t &y) const;
  void getLocalXYinCell(Int_t det, Int_t col, Int_t row, Float_t &x, Float_t &y) const;

  //! get the STAR frame cooridnates from local XYZ [cm]
  StThreeVectorD getStarXYZ(Int_t det,Float_t FcsX, Float_t FcsY, Float_t FcsZ=-1.0, Float_t zVertex=0.0) const;
  //! get the STAR frame phi angle from from local X/Y [cm]
  Float_t getPhi(Int_t det,Float_t FcsX, Float_t FcsY, Float_t FcsZ=-1.0) const; 
  //! get the STAR frame pseudo rapidity from the vertex from local X/Y [cm]
  Float_t getEta(Int_t det,Float_t FcsX, Float_t FcsY, Float_t FcsZ=-1.0, Float_t zVertex=0.0) const; 
  
  // get the STAR frame cooridnates from other way
  StThreeVectorD getStarXYZfromColumnRow(Int_t det,Float_t col, Float_t row, Float_t FcsZ=-1.0) const; //from column/row[cell size unit]
  StThreeVectorD getStarXYZ(Int_t det, Int_t col, int row, Float_t FcsZ=-1.0) const;   //from column/row [cell unit]
  StThreeVectorD getStarXYZ(StFcsHit* hit, Float_t FcsZ=-1.0) const;                   //from StFcsHit
  StThreeVectorD getStarXYZ(Int_t det, Int_t id, Float_t FcsZ=-1.0) const;             //center of the cell
  
  // Get the STAR frame cooridnates for 4x4 sum
  StThreeVectorD getStarXYZ_4x4(Int_t det,Int_t col, Int_t row) const;

  //get get 4 vector assuing m=0 and taking beamline from DB
  StLorentzVectorD getLorentzVector(const StThreeVectorD& xyz, Float_t energy, Float_t zVertex=0.0);
  
  //! fcsGain/GainCorrection related
  Int_t   getZeroSuppression(Int_t det) const;
  Float_t getSamplingFraction(Int_t det) const;
  Float_t getGain(Int_t det, Int_t id) const;            //! get the gain for the channel for 16 timebin sum
  Float_t getGain(StFcsHit* hit) const;                  //! get the gain for the channel for 16 timebin sum
  Float_t getGain8(Int_t det, Int_t id) const;           //! get the gain for the channel for 8 timebin sum
  Float_t getGain8(StFcsHit* hit) const;                 //! get the gain for the channel for 8 timebin sum
  Float_t getGainCorrection(Int_t det, Int_t id) const;  //! get the gain correction for the channel    
  Float_t getGainCorrection(StFcsHit* hit) const;        //! get the gain correction for the channel    
  Float_t getPresValley(Int_t det, Int_t id) const;      //! get the pres valley position for cut
  Float_t getPresValley(StFcsHit* hit) const;            //! get the pres valley position for cut

  void forceUniformGain(float v)           {mForceUniformGain=v;          } //! force gain to be specified value               
  void forceUniformGainCorrection(float v) {mForceUniformGainCorrection=v;} //! force gaincorr to be specified value
  void readGainFromText(const char* file="fcsgain.txt");
  void readGainCorrFromText(const char* file="fcsgaincorr.txt");

  //ETGain
  Float_t getEtGain(Int_t det, Int_t id) const;
  void printEtGain();
  
  //! Fcs Map
  void getDepfromId(Int_t detectorId, Int_t id, Int_t &ehp, Int_t &ns, Int_t &crt, Int_t &slt, Int_t &dep, Int_t &ch) const;
  void getIdfromDep(Int_t ehp, Int_t ns, Int_t dep, Int_t ch, Int_t &detectorId, Int_t &id, Int_t &crt, Int_t &slt) const;
  int  getNDep(Int_t ehp, int ns) const;
  void getSCmap(Int_t det, Int_t id, 
		Int_t &ehp, Int_t &ns, Int_t &scdep, Int_t &branch, Int_t &fee_i2c, Int_t &sipm, 
		Int_t &pp, Int_t &jacket) const;
  
  void makeMap();
  void makePPMap();
  int  jacketColor(int ehp, int ns, int dep, int ch);
  void makeMap2019();
  void printMap();
  void printHeader(FILE* f, int flag, int csv);
  void printHeader2(FILE* f);
  void printHeader3(FILE* f);
  void printHeader4(FILE* f, int flag);

  //EPD as PRES
  void getIdfromEPD(Int_t pp, Int_t tt, Int_t &det, Int_t &id);
  void getEPDfromId(Int_t det, Int_t id, Int_t &pp, Int_t &tt);

  //! Pedestal
  float pedestal(Int_t ehp, Int_t ns, Int_t dep, Int_t ch);
  void setPedestal(Int_t ehp, Int_t ns, Int_t dep, Int_t ch, float ped);
  void readPedFromText(const char* file="fcsped.txt");
  
 private:
  Int_t   mDbAccess=0;                     //! enable(1) or disabe(0) DB access
  Int_t   mRun=0;                          //! run#
  Int_t   mDebug=0;                        //! >0 dump tables to text files    
  Int_t   mRun19=0;                        //!
  Int_t   mLeakyHcal=0;                    //!

  Float_t mForceUniformGain=-1.0;           //! -1 for fixed vale, 0 for DB/readtext, >0 for forcing a value
  Int_t   mReadGainFromText=0;              //!
  Float_t mForceUniformGainCorrection=-1.0; //! -1 for fixed vale, 0 for DB/readtext, >0 for forcing a value
  Int_t   mReadGainCorrectionFromText=0;    //!
 
  //DEP sorted ped/gain/corr
  Float_t mPed[kFcsEHP][kFcsNorthSouth][kFcsMaxDepBd][kFcsMaxDepCh];
  Float_t mGain[kFcsEHP][kFcsNorthSouth][kFcsMaxDepBd][kFcsMaxDepCh];
  Float_t mGainCorr[kFcsEHP][kFcsNorthSouth][kFcsMaxDepBd][kFcsMaxDepCh]; //Valley value for PRES
   
  //tables from DB
  fcsDetectorPosition_st* mFcsDetectorPosition=0;
  fcsEcalGain_st*         mFcsEcalGain=0;
  fcsHcalGain_st*         mFcsHcalGain=0;
  fcsPresGain_st*         mFcsPresGain=0;
  fcsEcalGainCorr_st*     mFcsEcalGainCorr=0;
  fcsHcalGainCorr_st*     mFcsHcalGainCorr=0;
  fcsPresValley_st*       mFcsPresValley=0;

  virtual const Char_t *GetCVS() const {static const Char_t cvs[]="Tag $Name:" __DATE__ " " __TIME__ ; return cvs;}
  ClassDef(StFcsDbMaker,3)   //StAF chain virtual base class for Makers        
};

#endif
  

