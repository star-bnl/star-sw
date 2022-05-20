/***************************************************************************
 * $Id: StFcsDb.h,v 1.3 2021/05/27 14:02:23 akio Exp $
 * \author: akio ogawa
 ***************************************************************************
 *
 * Description: FCS DB Utility
 *
 ***************************************************************************
 *
 * $Log: StFcsDb.h,v $
 * Revision 1.3  2021/05/27 14:02:23  akio
 * clean up Clear and fixGain/corr
 *
 * Revision 1.2  2021/04/09 15:11:18  akio
 * Adding projection of Hcal local position to Ecal local position
 *
 * Revision 1.1  2021/03/30 13:40:07  akio
 * FCS code after peer review and moved from $CVSROOT/offline/upgrades/akio
 *
 * Revision 1.22  2021/02/25 21:53:50  akio
 * Int_t -> int
 *
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

#ifndef STFCSDB_H
#define STFCSDB_H

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
class StFcsCluster;
class StFcsPoint;

class StFcsDb : public TDataSet {

public: 
  StFcsDb(const char *name="fcsDb");
  virtual       ~StFcsDb();
  int  Init();
  int  InitRun(int runNumber);
 
  void SetDebug(int v=1) {mDebug=v;}  //! debug level
  void setDebug(int v=1) {mDebug=v;}  //! debug level
  void setDbAccess(int v=1);  //! enable(1) or disable(0) offline DB access
  void setRun(int run);       //! set run# 
  void setRun19(int v=1);     //! set run19 geometry, otherwise final run21
  void setLeakyHcal(int v=1); //! set leaky Hcal

  //! Setting DB table
  void setFcsDetectorPosition(fcsDetectorPosition_st* t); //! set fcsDetectorPosition_st*
  void setFcsEcalGain(fcsEcalGain_st*);                   //! set fcsEcalGain_st*
  void setFcsHcalGain(fcsHcalGain_st*);                   //! set fcsHcalGain_st* 
  void setFcsPresGain(fcsPresGain_st*);                   //! set fcsPresGain_st*
  void setFcsEcalGainCorr(fcsEcalGainCorr_st*);           //! set fcsEcalGainCorr_st*
  void setFcsHcalGainCorr(fcsHcalGainCorr_st*);           //! set fcsHcalGainCorr_st*
  void setFcsPresValley(fcsPresValley_st*);               //! set fcsPresValley_st*

  //! Utility functions related to FCS ChannelGeometry
  int maxDetectorId() const;              //! 6
  int detectorId(int eh, int ns) const;   //! Ecal North=0, Ecal South=1, Hcal North=2, Hcal South=3, Pres=4/5
  int ecalHcalPres(int det) const;      //! Ecal=0, Hcal=1, Pres=2
  int northSouth(int det) const;        //! north or south side
  int nRow(int det) const;              //! number of rows
  int nColumn(int det) const;           //! number of column
  int maxId(int det) const;             //! maximum number of id
  int getRowNumber(int det, int id) const;       //! get the row number for the channel
  int getColumnNumber(int det, int id) const ;   //! get the column number for the channel
  int getId(int det, int row, int col) const ; //! get the id from row/col
  int getDepCh(int dep, int ch) const ; //! get the DEP/ch id
  void getName(int det, int id, char name[]); //! Get Name of a channel
  void getName(int ehp, int ns, int dep, int ch, char name[]); //! Get Name of a channel 
  static void getFromName(const char name[], int& det, int& id); //! Get det/id from name
  static int getDetFromName(const std::string& detname);  //! Get det from name

  //! Utility functions related to DetectorPosition
  StThreeVectorD getDetectorOffset(int det) const;  //! get the offset of the detector
  float getDetectorAngle(int det) const;  //! get the angle of the detector
  float getXWidth(int det) const; //! get the X width of the cell
  float getYWidth(int det) const; //! get the Y width of the cell
  
  float getZDepth(int det) const;       // z depth of active detector
  float getShowerMaxZ(int det) const;   // default z[cm] from front face for where it measure x/y (shower max) 
  
  //! getting XY in local cell coordinate
  void getLocalXYinCell(StFcsHit* hit, float &x, float &y) const; 
  void getLocalXYinCell(int det, int id, float &x, float &y) const;
  void getLocalXYinCell(int det, int col, int row, float &x, float &y) const;

  //! get the STAR frame cooridnates from local XYZ [cm]
  StThreeVectorD getStarXYZ(int det,float FcsX, float FcsY, float FcsZ=-1.0, float zVertex=0.0) const;
  //! get the STAR frame phi angle from from local X/Y [cm]
  float getPhi(int det,float FcsX, float FcsY, float FcsZ=-1.0) const; 
  //! get the STAR frame pseudo rapidity from the vertex from local X/Y [cm]
  float getEta(int det,float FcsX, float FcsY, float FcsZ=-1.0, float zVertex=0.0) const; 
  
  //! get the STAR frame cooridnates from other way
  StThreeVectorD getStarXYZfromColumnRow(int det,float col, float row, float FcsZ=-1.0) const; //from column/row[cell size unit]
  StThreeVectorD getStarXYZ(int det, int col, int row, float FcsZ=-1.0) const;   //from column/row [cell unit]
  StThreeVectorD getStarXYZ(const StFcsHit* hit, float FcsZ=-1.0) const;         //from StFcsHit
  StThreeVectorD getStarXYZ(const StFcsCluster* clu, float FcsZ=-1.0) const;     //from StFcsCluster
  StThreeVectorD getStarXYZ(int det, int id, float FcsZ=-1.0) const;             //center of the cell
  
  //! Get the STAR frame cooridnates for 4x4 sum
  StThreeVectorD getStarXYZ_4x4(int det,int col, int row) const; 

  //! Get get 4 vector assuing m=0 and taking beamline from DB
  StLorentzVectorD getLorentzVector(const StThreeVectorD& xyz, float energy, float zVertex=0.0);
  
  //! Project Hcal local x/y to Ecal local x/y [cm]
  //! See https://www.star.bnl.gov/protected/spin/akio/fcs/fcsProjection.pdf
  double getHcalProjectedToEcalX(int ns, double hcalLocalX, double zvtx=0.0);
  double getHcalProjectedToEcalY(int ns, double hcalLocalY, double zvtx=0.0);
  double getProjectedDistance(StFcsCluster* ecal, StFcsCluster* hcal, double zvtx=0.0);
  double getProjectedDistance(StFcsPoint*   ecal, StFcsCluster* hcal, double zvtx=0.0);

  //! fcsGain/GainCorrection related
  int   getZeroSuppression(int det) const;           //! get zero suppression threshold
  float getSamplingFraction(int det) const;          //! get sampling fraction
  float getGain(int det, int id) const;            //! get the gain for the channel for 16 timebin sum
  float getGain(StFcsHit* hit) const;                  //! get the gain for the channel for 16 timebin sum
  float getGain8(int det, int id) const;           //! get the gain for the channel for 8 timebin sum
  float getGain8(StFcsHit* hit) const;                 //! get the gain for the channel for 8 timebin sum
  float getGainCorrection(int det, int id) const;  //! get the gain correction for the channel    
  float getGainCorrection(StFcsHit* hit) const;        //! get the gain correction for the channel    
  float getPresValley(int det, int id) const;      //! get the pres valley position for cut
  float getPresValley(StFcsHit* hit) const;            //! get the pres valley position for cut

  enum GAINMODE { FIXED, DB, FORCED, TXT }; //! Gain mode switch
  void forceFixGain()                      {mGainMode=GAINMODE::FIXED;}       //! fixed default gain
  void forceFixGainCorrection()            {mGainCorrMode=GAINMODE::FIXED;}   //! fixed default gaincorr
  void forceUniformGain(float ecal, float hcal=0.0053, float pres=0.01){
    mGainMode=GAINMODE::FORCED;   //! force a specified value               
    mForceUniformGainEcal=ecal; 
    mForceUniformGainHcal=hcal; 
    mForceUniformGainPres=pres; 
  }  
  void forceUniformGainCorrection(float ecal, float hcal=1.0, float pres=0.5){
    mGainCorrMode=GAINMODE::FORCED; //! force a specified value
    mForceUniformGainCorrectionEcal=ecal;
    mForceUniformGainCorrectionHcal=hcal;
    mForceUniformGainCorrectionPres=pres;
  } 

  //! reading gain from text files
  void setReadGainFromText(const char* file="fcsgain.txt")         {strcpy(mGainFilename,file);     mGainMode=GAINMODE::TXT;}
  void setReadGainCorrFromText(const char* file="fcsgaincorr.txt") {strcpy(mGainCorrFilename,file); mGainCorrMode=GAINMODE::TXT;}

  //ETGain factor= 1(ET Match), 0(E Match), 0.5(halfway)
  float getEtGain(int det, int id, float factor=1.0) const;  //! ET gain
  void printEtGain();                            //! print ET gain
  
  //! Fcs Map
  void getDepfromId(int detectorId, int id, int &ehp, int &ns, int &crt, int &slt, int &dep, int &ch) const; //! Get DEP map
  void getIdfromDep(int ehp, int ns, int dep, int ch, int &detectorId, int &id, int &crt, int &slt) const;   //! Get Det map
  int  getNDep(int ehp, int ns) const;  //! # of DEP
  void getSCmap(int det, int id,      
		int &ehp, int &ns, int &scdep, int &branch, int &fee_i2c, int &sipm, 
		int &pp, int &jacket) const; //! Get SC map
  void getIdfromSCmap(int ehp, int ns, int scdep, int branch, int fee_i2c, int sipm,
		      int &det, int &id) const;  //!Get Id from SC
  
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
  void getIdfromEPD(int pp, int tt, int &det, int &id);  //! Get FCS's EPD map foom EPD mapping 
  void getEPDfromId(int det, int id, int &pp, int &tt);  //! Get EPD's EPD map from FCS mapping

  //! Pedestal
  float pedestal(int ehp, int ns, int dep, int ch);   //! get Pedestal
  void setPedestal(int ehp, int ns, int dep, int ch, float ped); //! setting pedestal
  void readPedFromText(const char* file="fcsped.txt"); //! reading pedestal from text
  
 private:
  int   mDbAccess=1;                     //! enable(1) or disabe(0) DB access
  int   mRun=0;                          //! run#
  int   mDebug=0;                        //! >0 dump tables to text files    
  int   mRun19=0;                        //! run19 flag
  int   mLeakyHcal=0;                    //! LeakyHcal has different center positions

  GAINMODE mGainMode = GAINMODE::DB;      //! Gain mode selection 
  float mForceUniformGainEcal=-1.0;       //! forcing a value
  float mForceUniformGainHcal=-1.0;       //! forcing a value
  float mForceUniformGainPres=-1.0;       //! forcing a value
  char  mGainFilename[256];               //! gain file name
  void readGainFromText();

  GAINMODE mGainCorrMode = GAINMODE::DB;      //! GainCorr mode selection 
  float mForceUniformGainCorrectionEcal=-1.0; //! forcing a value
  float mForceUniformGainCorrectionHcal=-1.0; //! forcing a value
  float mForceUniformGainCorrectionPres=-1.0; //! forcing a value
  char  mGainCorrFilename[256];               //! gaincorr filename
  void readGainCorrFromText();
 
  //DEP sorted ped/gain/corr
  float mPed[kFcsEHP][kFcsNorthSouth][kFcsMaxDepBd][kFcsMaxDepCh]; //! Pedestal   
  float mGain[kFcsEHP][kFcsNorthSouth][kFcsMaxDepBd][kFcsMaxDepCh]; //! Gain
  float mGainCorr[kFcsEHP][kFcsNorthSouth][kFcsMaxDepBd][kFcsMaxDepCh]; //! GainCorr (Valley value for PRES) 
   
  //Beam line parameters
  double mVx=0.0;      //! beamline x offset
  double mVy=0.0;      //! beamline y offset 
  double mVdxdz=0.0;   //! beamline x slope 
  double mVdydz=0.0;   //! beamline y slope
  double mThetaX=0.0;  //! beamline x theta
  double mThetaY=0.0;  //! beamline y theta

  //copy of tables from DB
  fcsDetectorPosition_st  mFcsDetectorPosition; 
  fcsEcalGain_st          mFcsEcalGain;
  fcsHcalGain_st          mFcsHcalGain;
  fcsPresGain_st          mFcsPresGain;
  fcsEcalGainCorr_st      mFcsEcalGainCorr;
  fcsHcalGainCorr_st      mFcsHcalGainCorr;
  fcsPresValley_st        mFcsPresValley;

  virtual const Char_t *GetCVS() const {static const Char_t cvs[]="Tag " __DATE__ " " __TIME__ ; return cvs;}
  ClassDef(StFcsDb,1)   //StAF chain virtual base class for Makers        
};

#endif
  

