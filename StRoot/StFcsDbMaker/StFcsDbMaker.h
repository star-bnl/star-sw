/***************************************************************************
 * $Id: StFcsDbMaker.h,v 1.21 2021/02/24 22:56:19 akio Exp $
 * \author: akio ogawa
 ***************************************************************************
 *
 * Description: FCS DB access Maker
 *
 ***************************************************************************
 *
 * $Log: StFcsDbMaker.h,v $
 * Revision 1.21  2021/02/24 22:56:19  akio
 * Modified for STAR code review (Dmitry)
 *
 * Revision 1.20  2021/02/23 22:18:23  akio
 * Modified for STAr code review (Jason)
 *
 * Revision 1.19  2021/02/12 20:09:50  akio
 * Adding getIdfromSCmap()
 *
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
#include "tables/St_fcsDetectorPosition_Table.h"
#include "tables/St_fcsEcalGain_Table.h"
#include "tables/St_fcsHcalGain_Table.h"
#include "tables/St_fcsPresGain_Table.h"
#include "tables/St_fcsEcalGainCorr_Table.h"
#include "tables/St_fcsHcalGainCorr_Table.h"
#include "tables/St_fcsPresValley_Table.h"
#include "tables/St_vertexSeed_Table.h"
class StFcsHit;


class StFcsDbMaker : public StMaker {

public: 
  StFcsDbMaker(const Char_t *name="fcsDb");
  virtual       ~StFcsDbMaker();
  virtual Int_t  Init();
  virtual Int_t  InitRun(Int_t runNumber);
  virtual Int_t  Make();
  virtual Int_t  Finish();
  virtual void   Clear(const Char_t *opt);
 
  void setDebug(Int_t v=1) {SetDebug(v);}  //!backward compatibility 
  void setDbAccess(Int_t v=1);  //! enable(1) or disable(0) offline DB access
  void setRun(Int_t run);       //! set run# 
  void setRun19(Int_t v=1);     //! set run19 geometry, otherwise final run21
  void setLeakyHcal(Int_t v=1); //! set leaky Hcal

  //! Getting the whole DB table
  fcsDetectorPosition_st* fcsDetectorPosition(); //! get fcsDetectorPosition_st*
  fcsEcalGain_st* fcsEcalGain();                 //! get fcsEcalGain_st*
  fcsHcalGain_st* fcsHcalGain();                 //! get fcsHcalGain_st* 
  fcsPresGain_st* fcsPresGain();                 //! get fcsPresGain_st*
  fcsEcalGainCorr_st* fcsEcalGainCorr();         //! get fcsEcalGainCorr_st*
  fcsHcalGainCorr_st* fcsHcalGainCorr();         //! get fcsHcalGainCorr_st*
  fcsPresValley_st* fcsPresValley();             //! get fcsPresValley_st*

  //! Utility functions related to FCS ChannelGeometry
  Int_t maxDetectorId() const;              //! 6
  Int_t detectorId(int eh, int ns) const;   //! Ecal North=0, Ecal South=1, Hcal North=2, Hcal South=3, Pres=4/5
  Int_t ecalHcalPres(Int_t det) const;      //! Ecal=0, Hcal=1, Pres=2
  Int_t northSouth(Int_t det) const;        //! north or south side
  Int_t nRow(Int_t det) const;              //! number of rows
  Int_t nColumn(Int_t det) const;           //! number of column
  Int_t maxId(Int_t det) const;             //! maximum number of id
  Int_t getRowNumber(Int_t det, Int_t id) const;       //! get the row number for the channel
  Int_t getColumnNumber(Int_t det, Int_t id) const ;   //! get the column number for the channel
  Int_t getId(Int_t det, Int_t row, Int_t col) const ; //! get the id from row/col
  Int_t getDepCh(Int_t dep, Int_t ch) const ; //! get the DEP/ch id
  void getName(Int_t det, Int_t id, char name[]) const; //! Get Name of a channel
  void getName(Int_t ehp, Int_t ns, Int_t dep, Int_t ch, char name[]) const; //! Get Name of a channel 
  static void getFromName(const char name[], Int_t& det, Int_t& id); //! Get det/id from name
  static Int_t getDetFromName(const std::string& detname);  //! Get det from name

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
  
  //! get the STAR frame cooridnates from other way
  StThreeVectorD getStarXYZfromColumnRow(Int_t det,Float_t col, Float_t row, Float_t FcsZ=-1.0) const; //from column/row[cell size unit]
  StThreeVectorD getStarXYZ(Int_t det, Int_t col, int row, Float_t FcsZ=-1.0) const;   //from column/row [cell unit]
  StThreeVectorD getStarXYZ(StFcsHit* hit, Float_t FcsZ=-1.0) const;                   //from StFcsHit
  StThreeVectorD getStarXYZ(Int_t det, Int_t id, Float_t FcsZ=-1.0) const;             //center of the cell
  
  //! Get the STAR frame cooridnates for 4x4 sum
  StThreeVectorD getStarXYZ_4x4(Int_t det,Int_t col, Int_t row) const; 

  //! Get get 4 vector assuing m=0 and taking beamline from DB
  StLorentzVectorD getLorentzVector(const StThreeVectorD& xyz, Float_t energy, Float_t zVertex=0.0);
  
  //! fcsGain/GainCorrection related
  Int_t   getZeroSuppression(Int_t det) const;           //! get zero suppression threshold
  Float_t getSamplingFraction(Int_t det) const;          //! get sampling fraction
  Float_t getGain(Int_t det, Int_t id) const;            //! get the gain for the channel for 16 timebin sum
  Float_t getGain(StFcsHit* hit) const;                  //! get the gain for the channel for 16 timebin sum
  Float_t getGain8(Int_t det, Int_t id) const;           //! get the gain for the channel for 8 timebin sum
  Float_t getGain8(StFcsHit* hit) const;                 //! get the gain for the channel for 8 timebin sum
  Float_t getGainCorrection(Int_t det, Int_t id) const;  //! get the gain correction for the channel    
  Float_t getGainCorrection(StFcsHit* hit) const;        //! get the gain correction for the channel    
  Float_t getPresValley(Int_t det, Int_t id) const;      //! get the pres valley position for cut
  Float_t getPresValley(StFcsHit* hit) const;            //! get the pres valley position for cut

  enum GAINMODE { FIXED, DB, FORCED }; //! Gain mode switch
  void forceFixGain(float v)               {mGainMode=GAINMODE::FIXED;    }   //! fixed default gain
  void forceFixGainCorrection(float v)     {mGainCorrMode=GAINMODE::FIXED;}   //! fixed default gaincorr
  void forceUniformGain(float v)           {mGainMode=GAINMODE::FORCED;     mForceUniformGain=v;         }  //! force a specified value               
  void forceUniformGainCorrection(float v) {mGainCorrMode=GAINMODE::FORCED; mForceUniformGainCorrection=v;} //! force a specified value
  void readGainFromText(const char* file="fcsgain.txt");                      //! reading gain from text file
  void readGainCorrFromText(const char* file="fcsgaincorr.txt");              //! reading gaincorr from text file

  //ETGain
  Float_t getEtGain(Int_t det, Int_t id) const;  //! ET Gain
  void printEtGain();                            //! print ET gain
  
  //! Fcs Map
  void getDepfromId(Int_t detectorId, Int_t id, Int_t &ehp, Int_t &ns, Int_t &crt, Int_t &slt, Int_t &dep, Int_t &ch) const; //! Get DEP map
  void getIdfromDep(Int_t ehp, Int_t ns, Int_t dep, Int_t ch, Int_t &detectorId, Int_t &id, Int_t &crt, Int_t &slt) const;   //! Get Det map
  int  getNDep(Int_t ehp, int ns) const;  //! # of DEP
  void getSCmap(Int_t det, Int_t id,      
		Int_t &ehp, Int_t &ns, Int_t &scdep, Int_t &branch, Int_t &fee_i2c, Int_t &sipm, 
		Int_t &pp, Int_t &jacket) const; //! Get SC map
  void getIdfromSCmap(Int_t ehp, Int_t ns, Int_t scdep, Int_t branch, Int_t fee_i2c, Int_t sipm,
		      Int_t &det, Int_t &id) const;  //!Get Id from SC
  
  void makeMap(); //! Generate maps (this is the origin of the map)
  void makePPMap();  //! Generate Patchpanel map
  int  jacketColor(int ehp, int ns, int dep, int ch); //! cable jacket color
  void makeMap2019();  //! Generate map for run19
  void printMap();     //! Print maps
  void printHeader(FILE* f, int flag, int csv);  //! Map header
  void printHeader2(FILE* f);                    //! Map header 
  void printHeader3(FILE* f);                    //! Map header 
  void printHeader4(FILE* f, int flag);          //! Map header 

  //EPD as PRES
  void getIdfromEPD(Int_t pp, Int_t tt, Int_t &det, Int_t &id);  //! Get FCS's EPD map foom EPD mapping 
  void getEPDfromId(Int_t det, Int_t id, Int_t &pp, Int_t &tt);  //! Get EPD's EPD map from FCS mapping

  //! Pedestal
  float pedestal(Int_t ehp, Int_t ns, Int_t dep, Int_t ch);   //! get Pedestal
  void setPedestal(Int_t ehp, Int_t ns, Int_t dep, Int_t ch, float ped); //! setting pedestal
  void readPedFromText(const char* file="fcsped.txt"); //! reading pedestal from text
  
 private:
  Int_t   mDbAccess=1;                     //! enable(1) or disabe(0) DB access
  Int_t   mRun=0;                          //! run#
  Int_t   mDebug=0;                        //! >0 dump tables to text files    
  Int_t   mRun19=0;                        //! run19 flag
  Int_t   mLeakyHcal=0;                    //! LeakyHcal has different center positions

  GAINMODE mGainMode = GAINMODE::DB;        //! Gain mode selection 
  Float_t mForceUniformGain=-1.0;           //! forcing a value
  Int_t   mReadGainFromText=0;              //! flag for reading gain from text

  GAINMODE mGainCorrMode = GAINMODE::DB;    //! GainCorr mode selection 
  Float_t mForceUniformGainCorrection=-1.0; //! forcing a value
  Int_t   mReadGainCorrectionFromText=0;    //! flag for reading gaincorr from text 
 
  //DEP sorted ped/gain/corr
  Float_t mPed[kFcsEHP][kFcsNorthSouth][kFcsMaxDepBd][kFcsMaxDepCh]; //! Pedestal   
  Float_t mGain[kFcsEHP][kFcsNorthSouth][kFcsMaxDepBd][kFcsMaxDepCh]; //! Gain
  Float_t mGainCorr[kFcsEHP][kFcsNorthSouth][kFcsMaxDepBd][kFcsMaxDepCh]; //! GainCorr (Valley value for PRES) 
   
  //Beam line parameters
  double mVx=0.0;      //! beamline x offset
  double mVy=0.0;      //! beamline y offset 
  double mVdxdz=0.0;   //! beamline x slope 
  double mVdydz=0.0;   //! beamline y slope
  double mThetaX=0.0;  //! beamline x theta
  double mThetaY=0.0;  //! beamline y theta

  //tables from DB
  fcsDetectorPosition_st  mFcsDetectorPosition; 
  fcsEcalGain_st          mFcsEcalGain;
  fcsHcalGain_st          mFcsHcalGain;
  fcsPresGain_st          mFcsPresGain;
  fcsEcalGainCorr_st      mFcsEcalGainCorr;
  fcsHcalGainCorr_st      mFcsHcalGainCorr;
  fcsPresValley_st        mFcsPresValley;

  virtual const Char_t *GetCVS() const {static const Char_t cvs[]="Tag $Name:" __DATE__ " " __TIME__ ; return cvs;}
  ClassDef(StFcsDbMaker,3)   //StAF chain virtual base class for Makers        
};

#endif
  

