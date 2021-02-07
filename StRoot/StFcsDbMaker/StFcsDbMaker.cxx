/***************************************************************************
 * $id: StFcsDbMaker.cxx,v 1.22 2020/12/17 21:01:04 akio Exp $
 * \author: akio ogawa
 ***************************************************************************
 *
 * Description: This maker is the interface between FCS and the STAR database
 *
 ***************************************************************************
 *
 * $Log: StFcsDbMaker.cxx,v $
 * Revision 1.27  2021/02/05 17:23:25  akio
 * Adding access to STAR offline DB tables.
 * Adding getFromName/getDetFromName from David.
 *
 * Revision 1.26  2021/01/05 18:15:01  akio
 * added setPedestal()
 *
 * Revision 1.25  2020/12/30 20:45:20  akio
 * fix format
 *
 * Revision 1.24  2020/12/30 20:34:38  akio
 * also modify getName for DEP
 *
 * Revision 1.23  2020/12/30 20:17:55  akio
 * adding SC map access
 *
 * Revision 1.22  2020/12/17 21:01:04  akio
 * fix slt problem in sc map
 *
 * Revision 1.21  2020/09/03 19:43:20  akio
 * Updating SC map and adding patchpanel & cable color map
 *
 * Revision 1.20  2020/07/24 17:23:31  akio
 * EPD mip value from 1.6MeV to 2.0MeV
 *
 * Revision 1.19  2020/05/29 18:53:40  akio
 * Adding EPD as PRES maps, STAR coordinate for 4x4 trigger patch, renming map files to be used for DAQ as Tonko specifies
 *
 * Revision 1.18  2020/05/04 15:49:39  akio
 * adding gain for EPD as PRES
 *
 * Revision 1.17  2020/05/04 15:48:22  akio
 * adding input file for DAQ
 *
 * Revision 1.16  2019/10/23 20:05:39  akio
 * bug fixed for getOffset for det=1
 *
 * Revision 1.15  2019/10/23 19:20:10  akio
 * fix det=0 bug for getGain/gaincorr
 *
 * Revision 1.14  2019/10/23 13:34:38  akio
 * Adding getZDepth, and take out Ecal front space (for SiPM/Fee) from offsets
 * so that x/z offsets are now pointing to actual ecal tower front & near beam corner.
 *
 * Revision 1.13  2019/08/01 18:36:06  akio
 * Bug fix which was causing id=0 to get gain=1.0 always
 *
 * Revision 1.12  2019/07/10 06:13:34  akio
 * Adding reading of gains from text files
 *
 * Revision 1.11  2019/07/08 15:53:28  akio
 * updating sampling fraction number by Ting & Liu's study
 *
 * Revision 1.10  2019/06/27 16:10:32  akio
 * adding getLocalXYinCell
 *
 * Revision 1.9  2019/06/25 16:38:59  akio
 * Fixed y offset for run19
 * Added setting run# and time dependent (for preshower yoffset only for now)
 *
 * Revision 1.8  2019/06/21 17:28:47  akio
 * dealing with 5cm offsent when leakyHcal
 *
 * Revision 1.7  2019/06/07 18:16:54  akio
 * *** empty log message ***
 *
 * Revision 1.6  2019/05/16 16:08:32  akio
 * going back to 2019 to full gepmetry
 *
 * Revision 1.5  2019/03/22 14:28:35  akio
 * adding map for 2019
 *
 * Revision 1.4  2019/03/13 20:46:19  akio
 * formatting
 *
 * Revision 1.3  2019/03/13 20:29:30  akio
 * update for run19
 *
 * Revision 1.2  2019/02/05 22:00:18  akio
 * fix NorthSouth()
 *
 * Revision 1.1  2018/11/14 16:50:13  akio
 * FCS codes in offline/upgrade/akio
 *
 *
 **************************************************************************/

#include "StFcsDbMaker.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StMessMgr.h"
#include "StEvent/StEnumerations.h"
#include "StEvent/StFcsHit.h"
#include "StEvent/StFcsCluster.h"
#include "StEvent/StFcsPoint.h"

#include "tables/St_fcsDetectorPosition_Table.h"
#include "tables/St_fcsEcalGain_Table.h"
#include "tables/St_fcsHcalGain_Table.h"
#include "tables/St_fcsPresGain_Table.h"
#include "tables/St_fcsEcalGainCorr_Table.h"
#include "tables/St_fcsHcalGainCorr_Table.h"
#include "tables/St_fcsPresValley_Table.h"
#include "tables/St_vertexSeed_Table.h"

#include <math.h>

//Gain factors 
Float_t mEtGain[kFcsNDet][kFcsMaxId];

//DEP map                                                                                                                         
Short_t mMap_ehp[kFcsNDet][kFcsMaxId];
Short_t mMap_ns [kFcsNDet][kFcsMaxId];
Short_t mMap_crt[kFcsNDet][kFcsMaxId];
Short_t mMap_slt[kFcsNDet][kFcsMaxId];
Short_t mMap_dep[kFcsNDet][kFcsMaxId];
Short_t mMap_ch [kFcsNDet][kFcsMaxId];
Short_t mMap_ppb[kFcsNDet][kFcsMaxId];
Short_t mMap_ppp[kFcsNDet][kFcsMaxId];
Short_t mMap_pph[kFcsNDet][kFcsMaxId];
Short_t mMap_wcol[kFcsNDet][kFcsMaxId];
Short_t mMap_jcol[kFcsNDet][kFcsMaxId];

//Reverse map                                                                                                                     
Short_t mRMap_det[kFcsEHP][kFcsNorthSouth][kFcsMaxDepBd][kFcsMaxDepCh];                                              
Short_t mRMap_id [kFcsEHP][kFcsNorthSouth][kFcsMaxDepBd][kFcsMaxDepCh];                                              
Short_t mRMap_crt[kFcsEHP][kFcsNorthSouth][kFcsMaxDepBd][kFcsMaxDepCh];                                              
Short_t mRMap_slt[kFcsEHP][kFcsNorthSouth][kFcsMaxDepBd][kFcsMaxDepCh];

//SC map
const unsigned short kFcsMaxBranch=2;
const unsigned short kFcsMaxAddr=16;
const unsigned short kFcsMaxSiPM=4;
Short_t mScMap_ehp[kFcsNDet][kFcsMaxId];
Short_t mScMap_ns[kFcsNDet][kFcsMaxId];
Short_t mScMap_dep[kFcsNDet][kFcsMaxId];
Short_t mScMap_bra[kFcsNDet][kFcsMaxId];
Short_t mScMap_add[kFcsNDet][kFcsMaxId];
Short_t mScMap_sipm[kFcsNDet][kFcsMaxId];
Short_t mScMap_pp[kFcsNDet][kFcsMaxId];
Short_t mScMap_j[kFcsNDet][kFcsMaxId];

//Reverse SC map
Short_t mRScMap_det[kFcsEHP][kFcsNorthSouth][kFcsMaxDepBd][kFcsMaxBranch][kFcsMaxAddr][kFcsMaxSiPM];
Short_t mRScMap_id[kFcsEHP][kFcsNorthSouth][kFcsMaxDepBd][kFcsMaxBranch][kFcsMaxAddr][kFcsMaxSiPM];

//PatchPanel Map
const short EPPMap[8][6][3]={ // {dep#,low_ch/high_ch,pwr&ctrl row#}
    {{20, 0, 1},{20, 1,-1},  //PPB1 P2,P3
     { 0, 0, 2},{ 0, 1,-1},  //PPB1 P4,P5
     { 1, 0, 3},{ 1, 1,-1}}, //PPB1 P6,P7
    {{ 2, 0,-2},{ 2, 1,-1},  //PPB2 P2,P3
     { 3, 0,-2},{ 3, 1,-1},  //PPB2 P4,P5
     { 4, 0,-2},{ 4, 1,-1}}, //PPB2 P6,P7
    {{22, 0, 4},{23, 0,-1},  //PPB3 P2,P3
     { 5, 0, 5},{ 5, 1,-1},  //PPB3 P4,P5
     { 6, 0, 6},{ 6, 1,-1}}, //PPB3 P6,P7
    {{ 7, 0, 7},{ 7, 1,-1},  //PPB4 P2,P3
     { 8, 0, 8},{ 8, 1,-1},  //PPB4 P4,P5
     { 9, 0, 9},{ 9, 1,-1}}, //PPB4 P6,P7
    {{10, 0,10},{10, 1,-1},  //PPB5 P2,P3
     {11, 0,11},{11, 1,-1},  //PPB5 P4,P5
     {12, 0,12},{12, 1,-1}}, //PPB5 P6,P7
    {{13, 0,13},{13, 1,-1},  //PPB6 P2,P3
     {14, 0,14},{14, 1,-1},  //PPB6 P4,P5
     {22, 1,15},{23, 1,-1}}, //PPB6 P6,P7
    {{15, 0,-2},{15, 1,-1},  //PPB7 P2,P3
     {16, 0,-2},{16, 1,-1},  //PPB7 P4,P5
     {17, 0,-2},{17, 1,-1}}, //PPB7 P6,P7
    {{18, 0,16},{18, 1,-1},  //PPB8 P2,P3
     {19, 0,17},{19, 1,-1},  //PPB8 P4,P5
     {21, 0,-1},{21, 1,-1}}  //PPB8 P6,P7
};
const short HPPMap[4][6][3]={ // {dep#,low_ch/high_ch,pwr&ctrl row#}
    {{ 6, 0, 1},{ 6, 1,-1},  //PPB1 P2,P3
     { 0, 0, 2},{ 1, 0,-1},  //PPB1 P4,P5
     {-1,-1,-1},{-1,-1,-1}}, //PPB1 P6,P7
    {{ 2, 0, 3},{ 0, 1,-1},  //PPB2 P2,P3
     { 1, 1, 4},{ 2, 1,-1},  //PPB2 P4,P5
     { 8, 0, 5},{-1,-1,-1}}, //PPB2 P6,P7
    {{ 8, 1, 6},{-1,-1,-1},  //PPB3 P2,P3
     { 3, 0, 7},{ 4, 0,-1},  //PPB3 P4,P5
     { 5, 0, 8},{ 3, 1,-1}}, //PPB3 P6,P7
    {{ 4, 1, 9},{ 5, 1,-1},  //PPB4 P2,P3
     { 7, 0,10},{ 7, 1,-1},  //PPB4 P4,P5
     {-1,-1,-1},{-1,-1,-1}}  //PPB4 P6,P7
};
Short_t	EMapPPB[24][2];
Short_t EMapPPP[24][2];
Short_t EMapSCDEP[17];
Short_t EMapSCBRA[17];
Short_t EMapSCPP[17];
Short_t EMapSCJ[17];
Short_t	HMapPPB[24][2];
Short_t HMapPPP[24][2];
Short_t HMapSCDEP[10];
Short_t HMapSCBRA[10];
Short_t HMapSCPP[10];
Short_t HMapSCJ[10];

const char* colW[4]={"Green ","Brown ","Orange","Blue  "};
const char* colJ[8]={"Blue  ","Orange","Violet","Black ",
	       "Yellow","Red   ","Grey  ","Blue  "};
float leng[8]={     6.5,     6.5,     5.0,    5.0,
      	            3.5,     3.5,     8.0,    8.0};
const char* colJH[8]={"Red   ","Grey  ","Orange","Yellow",
		      "Orange","Blue  ","Red   ","Yellow"};
float lengH[8]={    6.5,     5.0,     5.0,    5.0,
      	            6.5,     5.0,     5.0,    5.0};

ClassImp(StFcsDbMaker)

StFcsDbMaker::StFcsDbMaker(const Char_t *name) : StMaker(name) {}; 

StFcsDbMaker::~StFcsDbMaker() {}

Int_t StFcsDbMaker::Init(){
  LOG_DEBUG<<"StFcsDbMaker Init Start"<<endm; 
  if(mRun19==0){
    makeMap();
  }else{
    makeMap2019();
  }
  if(mDebug) {
    printMap();
    printEtGain();
  }
  return StMaker::Init();
}
Int_t StFcsDbMaker::Make(){LOG_DEBUG<<"StFcsDbMaker Make"<<endm; return kStOK;}
void StFcsDbMaker::Clear(const Char_t*){LOG_DEBUG<<"StFcsDbMaker Clear"<<endm; StMaker::Clear();}
Int_t StFcsDbMaker::Finish(){LOG_DEBUG<<"StFcsDbMaker Finish"<<endm; return kStOK;}

void StFcsDbMaker::setDbAccess(Int_t v) {mDbAccess =  v;}
void StFcsDbMaker::setRun(Int_t run) {mRun = run;}
void StFcsDbMaker::setDebug(Int_t debug) {mDebug = debug;}
void StFcsDbMaker::setRun19(Int_t v) {mRun19=v;}
void StFcsDbMaker::setLeakyHcal(Int_t v) {mLeakyHcal=v;}

Int_t StFcsDbMaker::InitRun(Int_t runNumber) {
    LOG_DEBUG << "StFcsDbMaker::InitRun - run = " << runNumber << endm;
    
    //! Accessing DBs
    if(mDbAccess>0) {
	St_db_Maker* dbmaker = (St_db_Maker*)GetMaker("db");
	if(dbmaker){
	    LOG_INFO << "StFcsDbMaker::InitRun - Date&time from St_db_Maker="<<dbmaker->GetDate()<<","<< dbmaker->GetTime() << endm;       
	}else{
	    LOG_ERROR << "StFcsDbMaker::InitRun - No St_db_Maker"<<endm; return kStFatal;
	}

	//Get to Geometry/fcs
	TDataSet *DBgeom = 0;    
	DBgeom = GetInputDB("Geometry/fcs");
	if(!DBgeom) {LOG_ERROR << "StFcsDbMaker::InitRun - No Geometry/fcs"<<endm; return kStFatal;}

	//Get to Calibrations/fcs
	TDataSet *DBcalib = 0;    
	DBcalib = GetInputDB("Calibrations/fcs");
	if(!DBcalib) {LOG_ERROR << "StFcsDbMaker::InitRun - No Calibration/fcs"<<endm; return kStFatal;}
	
	//!Getting DB tables 
	St_fcsDetectorPosition *dbFcsDetPos       = (St_fcsDetectorPosition*)DBgeom ->Find("fcsDetectorPosition");
	St_fcsEcalGain         *dbFcsEcalGain     = (St_fcsEcalGain*)        DBcalib->Find("fcsEcalGain");
	St_fcsHcalGain         *dbFcsHcalGain     = (St_fcsHcalGain*)        DBcalib->Find("fcsHcalGain");
	St_fcsPresGain         *dbFcsPresGain     = (St_fcsPresGain*)        DBcalib->Find("fcsPresGain");
	St_fcsEcalGainCorr     *dbFcsEcalGainCorr = (St_fcsEcalGainCorr*)    DBcalib->Find("fcsEcalGainCorr");
	St_fcsHcalGainCorr     *dbFcsHcalGainCorr = (St_fcsHcalGainCorr*)    DBcalib->Find("fcsHcalGainCorr");
	St_fcsPresValley       *dbFcsPresValley   = (St_fcsPresValley*)      DBcalib->Find("fcsPresValley");
	if(!dbFcsDetPos)       {LOG_ERROR << "StFcsDbMaker::InitRun - No Geometry/fcs/fcsDetectorPosition"<<endm;return kStFatal;}
	if(!dbFcsEcalGain)     {LOG_ERROR << "StFcsDbMaker::InitRun - No Calibration/fcs/fcsEcalGain"<<endm;     return kStFatal;}
	if(!dbFcsHcalGain)     {LOG_ERROR << "StFcsDbMaker::InitRun - No Calibration/fcs/fcsHcalGain"<<endm;     return kStFatal;}
	if(!dbFcsPresGain)     {LOG_ERROR << "StFcsDbMaker::InitRun - No Calibration/fcs/fcsPresGain"<<endm;     return kStFatal;}
	if(!dbFcsEcalGainCorr) {LOG_ERROR << "StFcsDbMaker::InitRun - No Calibration/fcs/fcsEcalGainCorr"<<endm; return kStFatal;}
	if(!dbFcsHcalGainCorr) {LOG_ERROR << "StFcsDbMaker::InitRun - No Calibration/fcs/fcsHcalGainCorr"<<endm; return kStFatal;}
	if(!dbFcsPresValley)   {LOG_ERROR << "StFcsDbMaker::InitRun - No Calibration/fcs/fcsPresValley"<<endm;   return kStFatal;}
	
	//storing DetectorPosition
	fcsDetectorPosition_st *tDetectorPosition = (fcsDetectorPosition_st*) dbFcsDetPos->GetTable();
	if(mFcsDetectorPosition) delete mFcsDetectorPosition;
	mFcsDetectorPosition = new fcsDetectorPosition_st;
	memcpy(mFcsDetectorPosition,tDetectorPosition,sizeof(fcsDetectorPosition_st));

	//storing pointer to the whole table
	mFcsEcalGain         = (fcsEcalGain_st*)         dbFcsEcalGain->GetTable();
	mFcsHcalGain         = (fcsHcalGain_st*)         dbFcsHcalGain->GetTable();
	mFcsPresGain         = (fcsPresGain_st*)         dbFcsPresGain->GetTable();
	mFcsEcalGainCorr     = (fcsEcalGainCorr_st*)     dbFcsEcalGainCorr->GetTable();
	mFcsHcalGainCorr     = (fcsHcalGainCorr_st*)     dbFcsHcalGainCorr->GetTable();
	mFcsPresValley       = (fcsPresValley_st*)       dbFcsPresValley->GetTable();
	
	//storing in DEP sorted table
	int ie=0, ih=0, ip=0, ehp, ns, crt, slt, dep, ch;
	for(Int_t ins=0; ins<kFcsNorthSouth; ins++){
	    int det=kFcsEcalNorthDetId+ins; 
	    for(Int_t id=0; id<maxId(det); id++){ 
		getDepfromId(det,id,ehp,ns,crt,slt,dep,ch);
		mGain[ehp][ns][dep][ch]=mFcsEcalGain[0].gain[ie]; 
		mGainCorr[ehp][ns][dep][ch]=mFcsEcalGainCorr[0].gaincorr[ie]; 
		ie++;
	    }
	    det=kFcsHcalNorthDetId+ins; 
	    for(Int_t id=0; id<maxId(det); id++){ 
		getDepfromId(det,id,ehp,ns,crt,slt,dep,ch);
		mGain[ehp][ns][dep][ch]=mFcsHcalGain[0].gain[ih]; 
		mGainCorr[ehp][ns][dep][ch]=mFcsHcalGainCorr[0].gaincorr[ie]; 
		ih++;
	    }
	    det=kFcsPresNorthDetId+ins; 
	    for(Int_t id=0; id<maxId(det); id++){ 
		getDepfromId(det,id,ehp,ns,crt,slt,dep,ch);
		mGain[ehp][ns][dep][ch]=mFcsPresGain[0].gain[ip]; 
		mGainCorr[ehp][ns][dep][ch]=mFcsPresValley[0].valley[ip]; 
		ip++;
	    }
	}
    }
    return kStOK;
}

Int_t StFcsDbMaker::maxDetectorId() const {return kFcsNDet;}

Int_t StFcsDbMaker::detectorId(int eh, int ns) const { 
    if(eh>=0 && eh<kFcsEHP && ns>=0 && ns<kFcsNorthSouth) return eh*2 + ns;
    return -1;
}

Int_t StFcsDbMaker::ecalHcalPres(Int_t det) const {
    if(det==0 || det==1) return 0;
    if(det==2 || det==3) return 1;
    if(det==4 || det==5) return 2;
    return -1;
}

Int_t StFcsDbMaker::northSouth(Int_t det) const{   
    return det%2;
}

Int_t StFcsDbMaker::nRow(Int_t det) const{ 
    int ehp=ecalHcalPres(det);
    if(mRun19==0){
      if     (ehp==0){return kFcsEcalNRow;}
      else if(ehp==1){return kFcsHcalNRow;}
      else if(ehp==2){return kFcsPresNRow;}
      return -1;
    }else{
      int ns = northSouth(det);
      if(ns==0) return 0;
      if     (ehp==0){return 8;}
      else if(ehp==1){return 4;}
      else if(ehp==2){return 9;}
      return -1;
    }
}

Int_t StFcsDbMaker::nColumn(Int_t det) const{ 
    int ehp=ecalHcalPres(det);
    if(mRun19==0){
      if     (ehp==0){return kFcsEcalNCol;}
      else if(ehp==1){return kFcsHcalNCol;}
      else if(ehp==2){return kFcsPresNCol;}
      return -1;
    }else{
      int ns = northSouth(det);
      if(ns==0) return 0;
      if     (ehp==0){return 8;}
      else if(ehp==1){return 4;}
      else if(ehp==2){return 1;}
      return -1;
    }
}

Int_t StFcsDbMaker::maxId(Int_t det) const{ 
    int ehp=ecalHcalPres(det);
    if(mRun19==0){
      if     (ehp==0){return kFcsEcalMaxId;}
      else if(ehp==1){return kFcsHcalMaxId;}
      else if(ehp==2){return kFcsPresMaxId;}
      return -1;
    }else{
      int ns = northSouth(det);
      if(ns==0) return 0;
      if     (ehp==0){return 64;}
      else if(ehp==1){return 16;}
      else if(ehp==2){return 9;}
      return -1;
    }
}

Int_t StFcsDbMaker::getRowNumber(Int_t det, Int_t id) const{ 
    if(id<0 || id>=maxId(det)) return -1;
    return id/nColumn(det) + 1;
}

Int_t StFcsDbMaker::getColumnNumber(Int_t det, Int_t id) const{ 
    if(id<0 || id>=maxId(det)) return -1;
    return id%nColumn(det) + 1;
}

Int_t StFcsDbMaker::getId(Int_t det, Int_t row, Int_t col) const{ 
    if(row<=0 || row>nRow(det) || nRow(det)<0) return -1;
    if(col<=0 || col>nColumn(det) || nRow(det)<0) return -1;    
    return col - 1 + nColumn(det)*(row-1);
}                                                                                        

Int_t StFcsDbMaker::getDepCh(Int_t dep, Int_t ch) const{ 
  return dep*kFcsMaxDepCh + ch;
}                                                                                        

void StFcsDbMaker::getName(Int_t det, Int_t id, char name[]) const{
  const char* nameDET[6]={"EN","ES","HN","HS","PN","PS"};
  int ehp,ns,crt,slt,dep,ch;
  int c=getColumnNumber(det,id);
  int r=getRowNumber(det,id);
  getDepfromId(det,id,ehp,ns,crt,slt,dep,ch);  
  int scehp,scns,scdep,br,i2c,sipm,pp,j;
  getSCmap(det,id,scehp,scns,scdep,br,i2c,sipm,pp,j);
  sprintf(name,"%2s%03d_r%02dc%02d_Dep%02dCh%02d_F%02d/%1d/%02d/%1d",
	  nameDET[det],id,r,c,dep,ch,scdep,br,i2c,sipm);
}

void StFcsDbMaker::getName(Int_t ehp, Int_t ns, Int_t dep, Int_t ch, char name[]) const{
  const char* nameDET[6]={"EN","ES","HN","HS","PN","PS"};
  int det,id,crt,slt;
  getIdfromDep(ehp,ns,dep,ch,det,id,crt,slt);
  // printf("%1d %1d %2d %2d : %1d %3d\n",ehp,ns,dep,ch,det,id);
  if(id==-1){
    det = detectorId(ehp, ns);
    sprintf(name,"%2s---_r--c--_Dep%02dCh%02d_F--/-/--/-",
	    nameDET[det],dep,ch);
  }else{
    int c=getColumnNumber(det,id);
    int r=getRowNumber(det,id);
    int scehp,scns,scdep,br,i2c,sipm,pp,j;
    getSCmap(det,id,scehp,scns,scdep,br,i2c,sipm,pp,j);
    sprintf(name,"%2s%03d_r%02dc%02d_Dep%02dCh%02d_F%02d/%1d/%02d/%1d",
	    nameDET[det],id,r,c,dep,ch,scdep,br,i2c,sipm);
  }
}

void StFcsDbMaker::getFromName(const char name[], Int_t &det, Int_t &id){
    char detname[5];
    int r,c,dep,ch,scdep,br,i2c,sipm;
    sscanf(name,"%2s%03d_r%02dc%02d_Dep%02dCh%02d_F%02d/%1d/%02d/%1d",
	   detname,&id,&r,&c,&dep,&ch,&scdep,&br,&i2c,&sipm);
    det = getDetFromName(detname);
    return;
}

Int_t StFcsDbMaker::getDetFromName(const std::string& detname){
    if     ( detname=="EN") {return 0;}
    else if( detname=="ES") {return 1;}
    else if( detname=="HN") {return 2;}
    else if( detname=="HS") {return 3;}
    else if( detname=="HN") {return 4;}
    else if( detname=="PN") {return 5;}
    else if( detname=="PS") {return 6;}
    else {
	LOG_ERROR << "ERROR:Invalid name for detector;Input:"<<detname<<endm;
	return -1;
    }
}

StThreeVectorD StFcsDbMaker::getDetectorOffset(Int_t det) const{ 
  if(mRun19==0){
      if(mDbAccess==0){ //no DB
	  double a = getDetectorAngle(det) / 180.0 * M_PI;
	  if(det==0){
	      double x = -16.69 - 13.9*sin(a);
	      double z = 710.16 + 13.9*cos(a);
	      return StThreeVectorD(x, 0.0, z);
	  }
	  if(det==1){
	      double x =  16.69 + 13.9*sin(a);
	      double z = 710.16 + 13.9*cos(a);
	      return StThreeVectorD(x, 0.0, z);
	  }
	  if(det==2) return StThreeVectorD(-18.87, 0.0, 782.63);
	  if(det==3) return StThreeVectorD( 18.87, 0.0, 782.63);
	  return StThreeVectorD(0.0, 0.0, 0.0);	  
      }else{ //from DB
	  if(det>=0 && det<4) 	  
	      return  StThreeVectorD(mFcsDetectorPosition->xoff[det], 
				     mFcsDetectorPosition->yoff[det],
				     mFcsDetectorPosition->zoff[det]);	  
	  return StThreeVectorD(0.0, 0.0, 0.0);
      }
  }else{ //run19
    const float bOffY=-(17.0*5.81);   //40in=101.6cm and 17*5.81=98.76 so I will leave this unchanged
    if(det==1) return StThreeVectorD( 25.25*2.54, bOffY + getYWidth(det)*nRow(det)/2.0, 710.16);
    if(det==3) return StThreeVectorD( 27.50*2.54, bOffY + getYWidth(det)*nRow(det)/2.0, 782.63);
    if(det==5){
      //this is before June5
      if(mRun<1105942) return StThreeVectorD( 14*2.54,    bOffY + 2.875 + 2.54 + getYWidth(det)*nRow(det)/2.0,            700.00);
      //this is afetr June5, moved up by 3.5inch for ecal cooling
      if(mRun<1105951) return StThreeVectorD( 14*2.54,    bOffY + 2.875 + 2.54 + getYWidth(det)*nRow(det)/2.0 + 3.5*2.54, 700.00);
      //after June19, added 4 inches due to cooling & mounted on STGC supoort structure
      return StThreeVectorD( 14*2.54,    bOffY + 2.875 + 2.54 + getYWidth(det)*nRow(det)/2.0 + 4.0*2.54, 700.00);
    }
      return  StThreeVectorD(0.0, 0.0, 0.0);
  }
}

Float_t StFcsDbMaker::getDetectorAngle(Int_t det) const{ 
    if(det==0) return  1.73;
    if(det==1) return  1.73;
    if(det==2) return  1.73;
    if(det==3) return  1.73;
    if(det==4) return  0.0;
    if(det==5) return  0.0;
    return 0.0;
}

Float_t StFcsDbMaker::getXWidth(Int_t det) const{ 
    if(det==0) return  5.542+0.05;
    if(det==1) return  5.542+0.05;
    if(det==2) return  10.00+0.02;
    if(det==3) return  10.00+0.02;
    if(det==4) return  85.0;
    if(det==5) return  85.0;
    return 0.0;
}

Float_t StFcsDbMaker::getYWidth(Int_t det) const{ 
    if(det==4) return  5.00+0.02;
    if(det==5) return  5.00+0.02;
    return getXWidth(det);
}

Float_t StFcsDbMaker::getZDepth(Int_t det) const{
    if(det==0 || det==1) {return 30.97;} //66*(0.4+0.01+0.01)+(66-1)*0.05
    if(det==2 || det==3) {return 84.24;} //36*2.34
    else                 {return 1.0;}
}

Float_t StFcsDbMaker::getShowerMaxZ(Int_t det) const{ 
    if(det==0 || det==1) return 15.0;
    if(det==2 || det==3) return 25.0;
    return 0.0;
}

//! Get xy of center of the cell in local cell coordinate
void StFcsDbMaker::getLocalXYinCell(StFcsHit* hit, Float_t &x, Float_t &y) const{
    getLocalXYinCell(hit->detectorId(),hit->id(),x,y);
}

void StFcsDbMaker::getLocalXYinCell(Int_t det, Int_t id, Float_t &x, Float_t &y) const{
    getLocalXYinCell(det,getColumnNumber(det,id),getRowNumber(det,id),x,y);
}

void StFcsDbMaker::getLocalXYinCell(Int_t det, Int_t col, Int_t row, Float_t &x, Float_t &y) const{    
    if(mLeakyHcal==1 && (det==kFcsHcalNorthDetId || det==kFcsHcalSouthDetId)){
	if(col==1){
	    x=float(col)-0.4;
	}else if(col==kFcsHcalNCol){
	    x=float(col)-0.6;
	}else{
	    x=float(col);
	}
    }else{
	x=float(col)-0.5;
    }
    y=float(row)-0.5;
}

//! get coordinates of center of 4x4 sums in STAR frame 
//! Ecal 4x4 : col goes 1-9 row goes 1-15
//! Hcal 4x4 : col goes 1-5 row goes 1-9
StThreeVectorD StFcsDbMaker::getStarXYZ_4x4(Int_t det,Int_t col, Int_t row) const{
    int c1=0, c2=0, r1=0, r2=0;
    if(det<=kFcsEcalSouthDetId){
	c1=(col-1)*2 + 2;
	c2=(col-1)*2 + 5;
	r1=(row-1)*2 + 2;
	r2=(row-1)*2 + 5;
    }else if(det<=kFcsHcalSouthDetId){
	c1=(col-1)*2 + 1;
	c2=(col-1)*2 + 4;
	r1=(row-1)*2 + 1;
	r2=(row-1)*2 + 4;
    }	     
    return (getStarXYZ(det,c1,r1)+getStarXYZ(det,c2,r2))/2.0;
}

//! get coordinates of center of the cell STAR frame from StFcsHit
StThreeVectorD StFcsDbMaker::getStarXYZ(StFcsHit* hit, Float_t FcsZ) const{ 
    return getStarXYZ(hit->detectorId(),hit->id(),FcsZ);
}

//! get coordinates of center of the cell in STAR frame from det/id
StThreeVectorD StFcsDbMaker::getStarXYZ(Int_t det, Int_t id, Float_t FcsZ) const{ 
    return getStarXYZ(det,getColumnNumber(det,id),getRowNumber(det,id),FcsZ);   
}

//! get coordinates of center of the cell STAR frame from det/row/column
StThreeVectorD StFcsDbMaker::getStarXYZ(Int_t det, Int_t col, Int_t row, Float_t FcsZ) const{ 
    float x,y;
    getLocalXYinCell(det,col,row,x,y);
    return getStarXYZfromColumnRow(det,x,y,FcsZ);
}

//! get coordinates in STAR frame from det/row/column grid space [unit is cell size in float]
StThreeVectorD StFcsDbMaker::getStarXYZfromColumnRow(Int_t det, Float_t col, Float_t row, Float_t FcsZ) const{ 
    return getStarXYZ(det,col*getXWidth(det),row*getYWidth(det),FcsZ); 
}

//! get coordinates in STAR frame from local XY (in row/column space [cm])
StThreeVectorD StFcsDbMaker::getStarXYZ(Int_t det, Float_t FcsX, Float_t FcsY, Float_t FcsZ, Float_t zVertex) const{ 
    if(FcsZ<0.0) FcsZ = getShowerMaxZ(det);
    double x = 0.0, y=0.0, z=0.0;
    StThreeVectorD off = getDetectorOffset(det);
    double a = getDetectorAngle(det) / 180.0 * M_PI;
    y = off.y() + (double(nRow(det)) / 2.0 * getYWidth(det)) - FcsY;
    //y = off.y() - FcsY;
    if(northSouth(det) == 0) {//! north side
	x = off.x() - FcsX * cos(a) - FcsZ * sin(a);
	z = off.z() + FcsZ * cos(a) - FcsX * sin(a);
    }else{ // south side
	x = off.x() + FcsX * cos(a) + FcsZ * sin(a);
	z = off.z() + FcsZ * cos(a) - FcsX * sin(a);
    }
    z -= zVertex;
    /*
    LOG_DEBUG << Form("getStarXYZ XOFF=%f YOFF=%f ZOFF=%f Angle=%f : x=%f y=%f z=%f",
		     off.x(),off.y(),off.z(),a,x,y,z) << endm;
    */
    return StThreeVectorD(x,y,z);
}

Float_t StFcsDbMaker::getPhi(Int_t det,Float_t FcsX, Float_t FcsY, Float_t FcsZ) const{  
    return (getStarXYZ(det,FcsX,FcsY,FcsZ)).phi();
}
 
Float_t StFcsDbMaker::getEta(Int_t det,Float_t FcsX, Float_t FcsY, Float_t FcsZ, Float_t zVertex)  const{  
    return (getStarXYZ(det,FcsX,FcsY,FcsZ,zVertex)).pseudoRapidity();
}

StLorentzVectorD StFcsDbMaker::getLorentzVector(const StThreeVectorD& xyz, Float_t energy, Float_t zVertex){
    // Calculate a 4 momentum from a direction/momentum vector and energy assuming zero mass i.e. E=p
    // Taking into account beamline offsets and angles from DB
    double Vx=0.0, Vy=0.0, Vdxdz=0.0, Vdydz=0.0;
    TDataSet* dbDataSet = GetChain()->GetDataBase("Calibrations/rhic/vertexSeed");
    if(dbDataSet){
        vertexSeed_st* vSeed = ((St_vertexSeed*) (dbDataSet->FindObject("vertexSeed")))->GetTable();
        if(vSeed){
            Vx    = vSeed->x0;
            Vy    = vSeed->y0;
            Vdxdz = vSeed->dxdz;
            Vdydz = vSeed->dydz;
        }
    }
    //Vdxdz = -0.01; //hack for debug
    //Vdydz = -0.01;
    double thetaX = TMath::ATan( Vdxdz );
    double thetaY = TMath::ATan( Vdydz );
    StThreeVectorD xyznew(xyz.x()-Vx,xyz.y()-Vy,xyz.z()-zVertex);
    xyznew.rotateX(+thetaY);
    xyznew.rotateY(-thetaX);
    double e=energy;
    StThreeVectorD mom3 = xyznew.unit() * e;
    LOG_DEBUG << Form("xyz     = %lf %lf %lf",xyz.x(), xyz.y(), xyz.z()) << endm;
    LOG_DEBUG << Form("xyz rot = %lf %lf %lf",xyznew.x(), xyznew.y(), xyznew.z()) << endm;
    LOG_DEBUG << Form("p       = %lf %lf %lf %lf",mom3.x(), mom3.y(), mom3.z(),e) << endm;
    return StLorentzVectorD(mom3, e);
}

Float_t StFcsDbMaker::getSamplingFraction(Int_t det) const{
    if(det==0 || det==1) return  0.2;
    if(det==2 || det==3) return  0.0145;
    if(det==4 || det==5) return  2.0; //2MeV for MIP
    return 0.0;
}

Int_t   StFcsDbMaker::getZeroSuppression(Int_t det) const {return 1;}

Float_t StFcsDbMaker::getGain(StFcsHit* hit) const  {
  return getGain(hit->detectorId(), hit->id());
}

Float_t StFcsDbMaker::getGain(Int_t det, Int_t id) const  {
    if(det>=0 && det<kFcsNDet && id>=0 && id<maxId(det)) {
	if(mForceUniformGain<0.0){ //default fixed value
	    if(det<=kFcsHcalSouthDetId) return 0.0053; //default 5.3MeV/ch
	    return 100;                                //100ch for MIP for Pres
	}else if(mForceUniformGain>0.0){ //force to uniform gain
	    return mForceUniformGain;
	}else{  // value from DB or readText
	    int ehp,ns,dep,ch,crt,slt;
	    getDepfromId(det,id,ehp,ns,crt,slt,dep,ch);
	    return mGain[ehp][ns][dep][ch];
	}
    }
    return 1.0;
}

Float_t StFcsDbMaker::getGainCorrection(StFcsHit* hit) const  {
    return getGainCorrection(hit->detectorId(), hit->id());
}

Float_t StFcsDbMaker::getGainCorrection(Int_t det, Int_t id) const  {
    if(det>=0 && det<kFcsNDet && id>=0 && id<maxId(det)) {
	if(mForceUniformGain<0.0){ //default fixed value
	    if(det<=kFcsHcalSouthDetId) return 1.0;    //default 
	    return 0.5;                                //0.5 MIP for Pres
	}else if(mForceUniformGainCorrection>0.0){ //force to uniform gaincorr
	    return mForceUniformGainCorrection;
	}else{  // value from DB or readText
	    int ehp,ns,dep,ch,crt,slt;
	    getDepfromId(det,id,ehp,ns,crt,slt,dep,ch);
	    return mGainCorr[ehp][ns][dep][ch];
	}
    }
    return 1.0;
}

Float_t StFcsDbMaker::getPresValley(StFcsHit* hit) const  {
    return getGainCorrection(hit->detectorId(), hit->id());
}

Float_t StFcsDbMaker::getPresValley(Int_t det, Int_t id) const  {
    return getGainCorrection(det,id);
}

Float_t StFcsDbMaker::getGain8(StFcsHit* hit) const  {
  return getGain8(hit->detectorId(), hit->id());
}

Float_t StFcsDbMaker::getGain8(Int_t det, Int_t id) const {
  return getGain(det,id)*0.0070/0.0053;
}

void StFcsDbMaker::getDepfromId(Int_t detectorId, Int_t id, Int_t &ehp, Int_t &ns, Int_t &crt, Int_t &slt, Int_t &dep, Int_t &ch) const{
    ehp=-1; ns=-1; crt=-1; slt=-1; dep=-1; ch=-1;
    if(detectorId<0 || detectorId>=kFcsNDet) return;
    if(id<0 || id>=kFcsMaxId) return;
    ehp = mMap_ehp[detectorId][id];
    ns  = mMap_ns [detectorId][id];
    crt = mMap_crt[detectorId][id];
    slt = mMap_slt[detectorId][id];
    dep = mMap_dep[detectorId][id];
    ch  = mMap_ch [detectorId][id];
    return;
}

void StFcsDbMaker::getIdfromDep(Int_t ehp, Int_t ns, Int_t dep, Int_t ch, Int_t &detectorId, Int_t &id, Int_t &crt, Int_t &slt) const{
    detectorId=6; id=4095; crt=0; slt=0;
    if(ehp<0 || ehp>=kFcsEHP) return;
    if(ns<0  || ns>=kFcsNorthSouth) return;
    if(dep<0 || dep>=kFcsMaxDepBd) return;
    if(ch<0  || ch>=kFcsMaxDepCh) return;
    detectorId = mRMap_det[ehp][ns][dep][ch];
    id         = mRMap_id [ehp][ns][dep][ch];
    crt        = mRMap_crt[ehp][ns][dep][ch];
    slt        = mRMap_slt[ehp][ns][dep][ch];
    return;
}

int StFcsDbMaker::getNDep(Int_t ehp, Int_t ns) const{
  if(mRun19==1){
    if(ns==0) return 0;
    switch(ehp){
    case 0: return 2;
    case 1: return 1;
    case 2: return 1;
    case 3: return 1;
    default: return 0;
    }
  }else{
    switch(ehp){
    case 0: return 24;
    case 1: return 9;
    case 2: return 6;
    case 3: return 3;
    default: return 0;
    }
  }
}

void StFcsDbMaker::getSCmap(Int_t det, Int_t id, 
			    Int_t &ehp, Int_t &ns, Int_t &scdep, Int_t &branch, Int_t &fee_i2c, Int_t &sipm, 
			    Int_t &pp, Int_t &jacket) const{
    ehp=-1; ns=-1; scdep=-1; branch=-1; fee_i2c=-1; sipm=-1, pp=-1; jacket=-1;
    if(det<0 || det>=kFcsNDet) return;
    if(id<0 || id>=kFcsMaxId) return;
    ehp=mScMap_ehp[det][id];
    ns=mScMap_ns[det][id];
    scdep=mScMap_dep[det][id];
    branch=mScMap_bra[det][id];
    fee_i2c= mScMap_add[det][id];
    sipm=mScMap_sipm[det][id];
    pp=mScMap_pp[det][id];
    jacket=mScMap_j[det][id];
    return;
}

int StFcsDbMaker::jacketColor(int ehp, int ns, int dep, int ch){
    // char* colJ[8]={"Blue  ","Orange","Violet","Yellow",
    //                "Green ","Red   ","Grey  ","Black "};
    // char* colJH[8]={"Red   ","Grey  ","Orange","Yellow",
    //   	       "Orange","Blue  ","Red   ","Yellow"};
    switch(ehp){
    case 0: 
	if(dep<=19) return dep%5;
	switch(dep){
	case 20: case 21:
	    if(ch<4) return 7;
	    return ch/4;
	case 22: return 6;
	case 23: return 5;
	default: return -1;
	}
    case 1:	
	switch(dep){
	case 0: case 3: return 0 + ns*4;
	case 1: case 4: return 1 + ns*4;
	case 2: case 5: return 2 + ns*4;
	case 6: case 7:
	    if(ch<8 ) return 0 + ns*4;
	    if(ch<16) return 1 + ns*4;
	    if(ch<24) return 2 + ns*4;
	    return -1;
	case 8: return 3 + ns*4;
	default: return -1;
	}
    default:
	return -1;
    }
}
    


void  StFcsDbMaker::makePPMap(){
    for(int b=1; b<=8; b++){
	for(int p=2; p<=7; p++){
	    short dep = EPPMap[b-1][p-2][0];
	    short lh  = EPPMap[b-1][p-2][1];
	    short scr = EPPMap[b-1][p-2][2];
	    if(dep>=0){
		EMapPPB[dep][lh]=b;
		EMapPPP[dep][lh]=p;
		if(scr>0){
		    EMapSCDEP[scr-1]=dep;
		    EMapSCBRA[scr-1]=lh;
		    EMapSCPP[scr-1]=b;
		    EMapSCJ[scr-1]=p/2;
		}
	    }
	}
    }
    for(int b=1; b<=4; b++){
	for(int p=2; p<=7; p++){
	    short dep = HPPMap[b-1][p-2][0];
	    short lh  = HPPMap[b-1][p-2][1];
	    short scr = HPPMap[b-1][p-2][2];
	    if(dep>=0){
		HMapPPB[dep][lh]=b;
		HMapPPP[dep][lh]=p;
		if(scr>0){
		    HMapSCDEP[scr-1]=dep;
		    HMapSCBRA[scr-1]=lh;
		    HMapSCPP[scr-1]=b;
		    HMapSCJ[scr-1]=p/2;
		}
	    }
	}
    }
}

void  StFcsDbMaker::makeMap(){
    makePPMap();
    int ehp,crt,slt,dep,ch,det,id;
    for(int det=0; det<kFcsNDet; det++){
      for(int id=0; id<kFcsMaxId; id++){
	mMap_ehp[det][id]=-1;
	mMap_ch[det][id]=-1;
	mMap_ppb[det][id]=-1;
	mMap_ppp[det][id]=-1;
	mMap_pph[det][id]=-1;
	mMap_wcol[det][id]=-1;
	mMap_jcol[det][id]=-1;
	mScMap_ehp[det][id]=-1;
	mScMap_pp[det][id]=-1;
	mScMap_j[det][id]=-1;
      }
    }
    for(int ehp=0; ehp<kFcsEHP; ehp++){
      for(int ns=0; ns<kFcsNorthSouth; ns++){
        for(int dep=0; dep<kFcsMaxDepBd; dep++){
	  for(int ch=0; ch<kFcsMaxDepCh; ch++){
	    mRMap_det[ehp][ns][dep][ch]=6;
	    mRMap_id[ehp][ns][dep][ch]=-1;
	  }
	  for(int bra=0; bra<kFcsMaxBranch; bra++){
	    for(int add=0; add<kFcsMaxAddr; add++){
	      for(int sipm=0; sipm<kFcsMaxSiPM; sipm++){
		mRScMap_det[ehp][ns][dep][bra][add][sipm]=-1;
	      }
	    }
	  }
	}
      }
    }
    
    //Ecal
    ehp=0;
    for(int ns=0; ns<2; ns++){
        id=0;
	det = ns;
        for(int row=1; row<=kFcsEcalNRow; row++){
            for(int col=1; col<=kFcsEcalNCol; col++){
                if     (row== 1){crt=1+ns*2; slt=16; dep=20; ch=col-1;} //top row    (ch0-21)  
                else if(row==34){crt=1+ns*2; slt=17; dep=21; ch=col-1;} //bottom row (ch0-21)
		else if(col== 1){crt=1+ns*2; slt=18; dep=22; ch=row-2;} //near side column (ch0-31)
                else if(col==22){crt=1+ns*2; slt=19; dep=23; ch=row-2;} //far side column (ch0-31)
                else{
                    crt=ns*4;
                    dep=(col-2)/4 + (row-2)/8*5;
		    slt=dep;
                    ch =(col-2)%4 + ((row-2)%8)*4;
                }
		mMap_ehp[det][id] = ehp; 
		mMap_ns [det][id] = ns; 
		mMap_crt[det][id] = crt; 
		mMap_slt[det][id] = slt; 
		mMap_dep[det][id] = dep; 
		mMap_ch [det][id]   =ch ; 
		mRMap_det[ehp][ns][dep][ch] = det;
		mRMap_id [ehp][ns][dep][ch] = id ;
		mRMap_crt[ehp][ns][dep][ch] = crt;
		mRMap_slt[ehp][ns][dep][ch] = slt;
		
		//cable
		int lh= ch/16;		
		int b = EMapPPB[dep][lh];
		int p = EMapPPP[dep][lh];
		int h = (ch%16)/4 + (p-2)*4 + 1;
		int w = ch%4;
		int j = jacketColor(ehp,ns,dep,ch);
		mMap_ppb[det][id] = b; 
		mMap_ppp[det][id] = p; 
		mMap_pph[det][id] = h;
		mMap_wcol[det][id] = w; 
		mMap_jcol[det][id] = j;

		//SC map
		int scr=(row-1)/2;
		unsigned short scdep=EMapSCDEP[scr];
		unsigned short bra=EMapSCBRA[scr];
		unsigned short pp=EMapSCPP[scr];
		unsigned short pj=EMapSCJ[scr];;
		unsigned short add=(col-1)/2;
		unsigned short sipm;
		if(ns==0){ 
		    sipm = (col-1)%2 + ((row-1)%2)*2;
		}else{       
		    sipm = col%2 + ((row-1)%2)*2;
		}
		mScMap_ehp[det][id]  = ehp;
		mScMap_ns[det][id]   = ns;
		mScMap_dep[det][id]  = scdep;
		mScMap_bra[det][id]  = bra;
		mScMap_add[det][id]  = add;
		mScMap_sipm[det][id] = sipm;
		mScMap_pp[det][id]   = pp;
		mScMap_j[det][id]    = pj;
		mRScMap_det[ehp][ns][scdep][bra][add][sipm]=det;
		mRScMap_id[ehp][ns][scdep][bra][add][sipm]=id;

		id++;
            }
        }
    }

    //Hcal
    ehp=1;
    for(int ns=0; ns<2; ns++){
	id=0;
	det = ns + 2;
	crt = 1+ns*2;
        for(int row=1; row<=kFcsHcalNRow; row++){
            for(int col=1; col<=kFcsHcalNCol; col++){
                if     (col==13){dep=8; ch=row-1;}                         //far side column (ch0-19)
                else if(row== 1){dep=6; ch=(col-1)%4 + ((col-1)/4)*8    ;} //top row (ch0,1,2,3, 8, 9...19)             
                else if(row== 2){dep=6; ch=(col-1)%4 + ((col-1)/4)*8 + 4;} //2nd row (ch4,5,6,7,12,13...23)            
                else if(row==19){dep=7; ch=(col-1)%4 + ((col-1)/4)*8    ;} //2nd bottom row             
                else if(row==20){dep=7; ch=(col-1)%4 + ((col-1)/4)*8 + 4;} //bottom row  
                else{
		    dep= (col-1)/4 + ((row-3)/8)*3;
                    ch = (col-1)%4 + ((row-3)%8)*4;
                }
		slt=dep;
		mMap_ehp[det][id] = ehp; 
		mMap_ns [det][id] = ns; 
		mMap_crt[det][id] = crt; 
		mMap_slt[det][id] = slt; 
		mMap_dep[det][id] = dep; 
		mMap_ch[det][id]  =ch ; 
		mRMap_det[ehp][ns][dep][ch] = det;
		mRMap_id [ehp][ns][dep][ch] = id ;
		mRMap_crt[ehp][ns][dep][ch] = crt;
		mRMap_slt[ehp][ns][dep][ch] = slt;

		//cable
		int lh= ch/16;		
		int b = HMapPPB[dep][lh];
		int p = HMapPPP[dep][lh];
		int h = (ch%16)/4 + (p-2)*4 + 1;
		int w = ch%4;
		int j = jacketColor(ehp,ns,dep,ch);
		mMap_ppb[det][id] = b; 
		mMap_ppp[det][id] = p; 
		mMap_pph[det][id] = h;
		mMap_wcol[det][id] = w; 
		mMap_jcol[det][id] = j;

		//SC map
		unsigned short feerow = (row-1)/2;
		unsigned short scdep=HMapSCDEP[feerow];
		unsigned short bra=HMapSCBRA[feerow];
		unsigned short pp=HMapSCPP[feerow];
		unsigned short pj=HMapSCJ[feerow];
		unsigned short add=col-1;
		unsigned short sipm;
		if(ns==0){ 
		    sipm = (row-1)%2;
		}else{       
		    sipm = row%2;
		}
		mScMap_ehp[det][id]  = ehp;
		mScMap_ns[det][id]   = ns;
		mScMap_dep[det][id]  = scdep;
		mScMap_bra[det][id]  = bra;
		mScMap_add[det][id]  = add;
		mScMap_pp[det][id]   = pp;
		mScMap_j[det][id]    = pj;
		mScMap_sipm[det][id] = sipm;
		mRScMap_det[ehp][ns][scdep][bra][add][sipm]=det;
		mRScMap_id[ehp][ns][scdep][bra][add][sipm]=id;
		id++;
            }
        }
    }

    //EPD west as PRES 
    //   EPD PP=  1 ~ 6 ==> ns=0 (north) and dep=0 (near top) to 5 (near bottom)
    //   EPD PP= 12 ~ 7 ==> ns=1 (south) and dep=0 (near top) to 5 (near bottom), odd/even reversed
    ehp=2;
    for(int ns=0; ns<2; ns++){
	det = ns + 4;
	crt = 1+ns*2;
	for(int dep=0; dep<6; dep++){
            for(int ch=0; ch<32; ch++){
		slt=dep+10;
		id=dep*32+ch;
		mMap_ehp[det][id] = ehp; 
		mMap_ns [det][id] = ns; 
		mMap_crt[det][id] = crt; 
		mMap_slt[det][id] = slt; 
		mMap_dep[det][id] = dep; 
		mMap_ch[det][id]  = ch ; 
		mRMap_det[ehp][ns][dep][ch] = det;
		mRMap_id [ehp][ns][dep][ch] = id ;
		mRMap_crt[ehp][ns][dep][ch] = crt;
		mRMap_slt[ehp][ns][dep][ch] = slt;
            }
        }
    }
}

void  StFcsDbMaker::makeMap2019(){
  int ehp,crt,slt,dep,ch,det,id,ns;
  crt=1;
  slt=0;
  ns=1;
  
  for(int det=0; det<kFcsNDet; det++){
    for(int id=0; id<kFcsMaxId; id++){
      mMap_ehp[det][id]=-1;
      mMap_ns[det][id]=-1;
      mMap_crt[det][id]=-1;
      mMap_slt[det][id]=-1;
      mMap_dep[det][id]=-1;
      mMap_ch[det][id]=-1;
    }
  }
  for(int ehp=0; ehp<kFcsEHP; ehp++){
    for(int ns=0; ns<kFcsNorthSouth; ns++){
      for(int dep=0; dep<kFcsMaxDepBd; dep++){
	for(int ch=0; ch<kFcsMaxDepCh; ch++){
	  mRMap_det[ehp][ns][dep][ch]=6;
	  mRMap_id[ehp][ns][dep][ch]=-1;
	  mRMap_crt[ehp][ns][dep][ch]=-1;
	  mRMap_slt[ehp][ns][dep][ch]=-1;
	}
      }
    }
  }
  
  //Ecal
  ehp=0;
  id=0;
  det = ns;	
  dep = 0;
  int c[2] = {0,0};
  for(int row=1; row<=8; row++){
    for(int col=1; col<=8; col++){
      if(col<=4) {dep=0;} 
      else       {dep=1;}
      mMap_ehp[det][id] = ehp; 
      mMap_ns [det][id] = ns; 
      mMap_crt[det][id] = crt; 
      mMap_slt[det][id] = slt; 
      mMap_dep[det][id] = dep; 
      mMap_ch [det][id] = c[dep] ;

      mRMap_det[ehp][ns][dep][c[dep]] = det;
      mRMap_id [ehp][ns][dep][c[dep]] = id ;
      mRMap_crt[ehp][ns][dep][c[dep]] = crt;
      mRMap_slt[ehp][ns][dep][c[dep]] = slt;
      id++;
      c[dep]++;
    }
  }
  
  //Hcal                                                                                                                          
  ehp=1;
  id=0;
  det = ns + 2;
  dep = 0;  //number for 2019/5/15 with OLD DEP16 before we install new DEP32
  ch = 0;
  for(int row=1; row<=4; row++){
    for(int col=1; col<=4; col++){
      mMap_ehp[det][id] = ehp; 
      mMap_ns [det][id] = ns; 
      mMap_crt[det][id] = crt; 
      mMap_slt[det][id] = slt; 
      mMap_dep[det][id] = dep; 
      mMap_ch[det][id]  =ch ; 
      mRMap_det[ehp][ns][dep][ch] = det;
      mRMap_id [ehp][ns][dep][ch] = id ;
      mRMap_crt[ehp][ns][dep][ch] = crt;
      mRMap_slt[ehp][ns][dep][ch] = slt;
      id++;
      ch++;
    }
  }
  
  //Pres
  ehp=2;
  id=0;
  det = ns + 4;
  dep = 0;
  ch = 8;
  for(int id=0; id<9; id++){
    mMap_ehp[det][id] = ehp; 
    mMap_ns [det][id] = ns; 
    mMap_crt[det][id] = crt; 
    mMap_slt[det][id] = slt; 
    mMap_dep[det][id] = dep; 
    mMap_ch[det][id]  = ch ; 
    mRMap_det[ehp][ns][dep][ch] = det;
    mRMap_id [ehp][ns][dep][ch] = id ;
    mRMap_crt[ehp][ns][dep][ch] = crt;
    mRMap_slt[ehp][ns][dep][ch] = slt;
    ch--;
  }
}

void StFcsDbMaker::getIdfromEPD(Int_t pp, Int_t tt, Int_t& det, Int_t &id){
    det=-1; 
    id=-1;
    int row,col;
    if(tt<0 || tt>=32) return;
    if(pp>=1 && pp<=6){ //north side
	det=4;
	row=(pp-1)*2 + (tt+1)%2 + 1;
	col=tt/2;
    }else if(pp>=7 && pp<=12){ //south side
	det=5;
	row=(12-pp)*2 + (tt)%2 + 1;
	col=tt/2;
    }
    id=(row-1)*16 + col;
}

void StFcsDbMaker::getEPDfromId(Int_t det, Int_t id, Int_t &pp, Int_t &tt){
    int row=id/16 + 1;
    int col=id%16 + 1;
    if(det==4){
	pp = (row-1)/2 + 1;
	tt = (col-1)*2 - (row-1)%2 + 1;
    }else if(det==5){
	pp = 12 - (row-1)/2;
	tt = (col-1)*2 + (row-1)%2;
    }
}

void StFcsDbMaker::printHeader(FILE* f, int flag=0, int csv=0){
  fprintf(f,"### Detector\n");
  fprintf(f,"#det : 0/1=ECal-N/S 2/3=Hcal-N/S  4/5=PRS-N/S\n");
  fprintf(f,"#id  : 0~747 for Ecal  0~259 for Hcal\n");
  fprintf(f,"#row : 1~34 for Ecal  1~20 for Hcal\n");
  fprintf(f,"#col : 1~22 for Ecal  1~13 for Hcal\n");
  fprintf(f,"### Readout\n");
  fprintf(f,"#ehp : 0=ECal  1=Hcal  2=PRS\n");
  fprintf(f,"#ns  : 0=north  1=south\n");
  fprintf(f,"#crt : 0=EcalNorth  1=MixNorth  2=Main  3=MixSouth  4=EcalSouth\n");
  fprintf(f,"#slt : 0~19\n");
  fprintf(f,"#dep : 0~24 for Ecal  0~7 for Hcal  0~3 for Pres\n");
  fprintf(f,"#ch  : 0~31 for all DEP\n");
  fprintf(f,"### Patchpanel and cable\n");  
  fprintf(f,"#FRow: FEEBd Row = (row-1)/2   [0-16 for Ecal  0-9  for Hcal]\n");
  fprintf(f,"#FCol: FEEBd Col = FEEBd Addr  [0-10 for Ecal  0-12 for Hcal]\n");
  fprintf(f,"#SiPM: FEEBd Ch#               [0-3  for Ecal  0-1  for Hcal]\n");
  fprintf(f,"#ppb : PatchPanel Board#       [1~8  for Ecal  1-4  for Hcal]\n");
  fprintf(f,"#ppp : PatchPanel MDR cable P# [2-7]\n");
  fprintf(f,"#pph : PatchPanel Header#      [1-24]\n");
  fprintf(f,"#jcol: Cable Jacket color\n");
  fprintf(f,"#wcol: Cable Wire color\n");  
  fprintf(f,"#length: Cable length\n");
  if(csv==0){
      if(flag==0){
	  fprintf(f,"#det id row col     ehp  ns crt slt dep  ch   Frow Fcol SiPM ppb ppp pph jcol   wcol  length\n");
      }else{
	  fprintf(f,"#ehp ns dep  ch   crt slt   det  id row col   Frow Fcol SiPM ppb ppp pph jcol   wcol  length\n");
      }
  }else{
      if(flag==0){
	  fprintf(f,"#det,id,row,col,ehp,ns,crt,slt,dep,ch,Frow,Fcol,SiPM,ppb,ppp,pph,jcol,wcol,length\n");
      }else{
	  fprintf(f,"#ehp,ns,dep,ch,crt,slt,det,id,row,col,Frow,Fcol,SiPM,ppb,ppp,pph,jcol,wcol,length\n");
      }
  }
}

void StFcsDbMaker::printHeader2(FILE* f){
    fprintf(f,"# Css-DDch\n");
    fprintf(f,"#  C  : crate 0~4 for EcalN,MixN,Main,MixS,EcalS\n");
    fprintf(f,"#  ss : slot (0~19)\n");
    fprintf(f,"#  DD : DEP bd# (0~24 for Ecal, 0~7 for Hcal\n");
    fprintf(f,"#  ch : DEP ch# (0~31 for all DEP)\n");
}

void StFcsDbMaker::printHeader3(FILE* f){
    fprintf(f,"# crt-slt : ns ehp dep : DNiii-RR/CC DNiii-RR/CC DNiii-RR/CC ...\n");
    fprintf(f,"#   D : \"E\"cal, \"H\"cal, \"P\"res\n");
    fprintf(f,"#   N : North or South\n");
    fprintf(f,"# iii : id\n");
    fprintf(f,"#  RR : Row\n");
    fprintf(f,"#  CC : Column\n");
}

void StFcsDbMaker::printHeader4(FILE* f, int flag=0){
  fprintf(f,"### Detector\n");
  fprintf(f,"#det : 0/1=ECal N/S, 2/3=Hcal N/S , 4/5=PRS N/S\n");
  fprintf(f,"#id  : 0~747 for Ecal, 0~259 for Hcal\n");
  fprintf(f,"#row : 1~34 for Ecal, 1~20 for Hcal\n");
  fprintf(f,"#col : 1~22 for Ecal, 1~13 for Hcal\n");
  fprintf(f,"### Slow Control\n");
  fprintf(f,"#ehp        : 0=ECal, 1=Hcal, 2=PRS\n");
  fprintf(f,"#ns         : 0=north, 1=south\n");
  fprintf(f,"#dep        : 0~24 for Ecal, 0~7 for Hcal, 0~3 for Pres\n");
  fprintf(f,"#branch     : 0~1\n");
  fprintf(f,"#switch addr: 0~15\n");
  fprintf(f,"#SiPM#      : 0~3 for Ecal, 0~1 for Hcal&Pres\n");
  fprintf(f,"### Patchpanel and cable\n");  
  fprintf(f,"#ppb : PatchPanel Board# (1~8 for ecal, 1-4 for hcal)\n");
  fprintf(f,"#J   : PatchPanel SC conection (J1~J3)\n");
  if(flag==0){
      fprintf(f,"#det id row col     ehp  ns dep bra add SiPM  ppb  J\n");
  }else{
      fprintf(f,"#ehp  ns dep bra add SiPM   det  id row col\n");
  }
}

void StFcsDbMaker::printMap(){
    int ehp,ns,crt,slt,dep,ch,det,id,row,col;
    
    FILE *f1  = fopen("fcsMap.txt","w");           printHeader(f1);
    FILE *f1c = fopen("fcsMap.csv","w");           printHeader(f1c,0,1);
    FILE *f1e = fopen("fcs_ecal_readout_map.csv","w"); printHeader(f1e);
    FILE *f1h = fopen("fcs_hcal_readout_map.csv","w"); printHeader(f1h);
    FILE *f1p = fopen("fcs_pres_readout_map.csv","w"); printHeader(f1p);    
    FILE *f2 = fopen("fcsMap2.txt","w");    printHeader2(f2);    
    FILE *f3 = fopen("fcsDepMap.txt","w");  printHeader(f3,1);    
    FILE *f3c= fopen("fcsDepMap.csv","w");  printHeader(f3c,1,1);    
    FILE *f4 = fopen("fcsDepMap2.txt","w"); printHeader3(f4);    
    FILE *f5  = fopen("fcsScMap.txt","w");    printHeader4(f5);
    FILE *f5e = fopen("fcs_ecal_sc_map.csv","w"); printHeader4(f5e);
    FILE *f5h = fopen("fcs_hcal_sc_map.csv","w"); printHeader4(f5h);
    FILE *f5p = fopen("fcs_pres_sc_map.csv","w"); printHeader4(f5p);
    FILE *f6  = fopen("fcsScRevMap.txt","w"); printHeader4(f6,1);

    FILE *f7  = fopen("fcsEpdMap.txt","w");    
    FILE *fpp = fopen("fcsPPMap.txt","w");    

    const char* EHP[3]={"Ecal","Hcal","Pres"};
    const char* CRT[5]={"EN","MN","MA","MS","ES"};
    const char* DET[6]={"EN","ES","HN","HS","PN","PS"};
    
    //Ecal
    for(ns=0; ns<2; ns++){
	det=ns;
	id=0;
	fprintf(f2,"Ecal NS=%1d\n",ns);
	for(row=1; row<=nRow(det); row++){
	    for(col=1; col<=nColumn(det); col++){
		if(mMap_ehp[det][id]>=0){
		    fprintf(f1,"%3d %3d %3d %3d     %3d %3d %3d %3d %3d %3d    %2d  %2d  %2d     %2d  P%1d H%02d %6s %6s %3.1f\n",
			    det,id,row,col,
			    mMap_ehp[det][id], 
			    mMap_ns [det][id], 
			    mMap_crt[det][id],
			    mMap_slt[det][id],
			    mMap_dep[det][id],
			    mMap_ch [det][id],
			    (row-1)/2,
			    mScMap_add[det][id],
			    mScMap_sipm[det][id],
			    mMap_ppb[det][id],
			    mMap_ppp[det][id],
			    mMap_pph[det][id],
			    colJ[mMap_jcol[det][id]],
			    colW[mMap_wcol[det][id]],
			    leng[mMap_jcol[det][id]]);
		    fprintf(f1c,"%3d,%3d,%3d,%3d,%3d,%3d,%3d,%3d,%3d,%3d,%2d,%2d,%2d,%2d,P%1d,H%02d,%6s,%6s,%3.1f\n",
			    det,id,row,col,
			    mMap_ehp[det][id], 
			    mMap_ns [det][id], 
			    mMap_crt[det][id],
			    mMap_slt[det][id],
			    mMap_dep[det][id],
			    mMap_ch [det][id],
			    (row-1)/2,
			    mScMap_add[det][id],
			    mScMap_sipm[det][id],
			    mMap_ppb[det][id],
			    mMap_ppp[det][id],
			    mMap_pph[det][id],
			    colJ[mMap_jcol[det][id]],
			    colW[mMap_wcol[det][id]],
			    leng[mMap_jcol[det][id]]);
		    fprintf(f1e,"%3d %3d %3d %3d     %3d %3d %3d %3d %3d %3d\n",
			    det,id,row,col,
			    mMap_ehp[det][id], 
			    mMap_ns [det][id], 
			    mMap_crt[det][id],
			    mMap_slt[det][id],
			    mMap_dep[det][id],
			    mMap_ch [det][id]); 
		    fprintf(f2,"%1d%02d-%02d%02d ",
			    mMap_crt[det][id],mMap_slt[det][id],
			    mMap_dep[det][id],mMap_ch[det][id]);
		}
		if(mScMap_ehp[det][id]>=0){
		    fprintf(f5,"%3d %3d %3d %3d     %3d %3d %3d %3d %3d %3d    %2d   J%1d\n",
                            det,id,row,col,
                            mScMap_ehp[det][id],
                            mScMap_ns [det][id],
                            mScMap_dep[det][id],
                            mScMap_bra[det][id],
                            mScMap_add[det][id],
                            mScMap_sipm[det][id],
                            mScMap_pp[det][id],
                            mScMap_j[det][id]);
		    fprintf(f5e,"%3d %3d %3d %3d     %3d %3d %3d %3d %3d %3d\n",
                            det,id,row,col,
                            mScMap_ehp[det][id],
                            mScMap_ns [det][id],
                            mScMap_dep[det][id],
                            mScMap_bra[det][id],
                            mScMap_add[det][id],
                            mScMap_sipm[det][id]);
		}
		id++;
	    }
	    fprintf(f2,"\n");
	}
    }
    
    //Hcal
    for(int ns=0; ns<2; ns++){
	det=ns+2;
	id=0;
	fprintf(f2,"Hcal NS=%1d\n",ns);
	for(row=1; row<=nRow(det); row++){
	    for(col=1; col<=nColumn(det); col++){
		if(mMap_ehp[det][id]>=0){
		    fprintf(f1,"%3d %3d %3d %3d     %3d %3d %3d %3d %3d %3d    %2d  %2d  %2d     %2d  P%1d H%02d %6s %6s %3.1f\n",
			    det,id,row,col,
			    mMap_ehp[det][id], 
			    mMap_ns[det][id], 
			    mMap_crt[det][id],
			    mMap_slt[det][id],
			    mMap_dep[det][id],
			    mMap_ch[det][id],
                            (row-1)/2,
                            mScMap_add[det][id],
                            mScMap_sipm[det][id],
			    mMap_ppb[det][id],
			    mMap_ppp[det][id],
			    mMap_pph[det][id],
			    colJH[mMap_jcol[det][id]],
			    colW[mMap_wcol[det][id]], 
			    lengH[mMap_jcol[det][id]]);
		    fprintf(f1c,"%3d,%3d,%3d,%3d,%3d,%3d,%3d,%3d,%3d,%3d,%2d,%2d,%2d,%2d,P%1d,H%02d,%6s,%6s,%3.1f\n",
			    det,id,row,col,
			    mMap_ehp[det][id], 
			    mMap_ns[det][id], 
			    mMap_crt[det][id],
			    mMap_slt[det][id],
			    mMap_dep[det][id],
			    mMap_ch[det][id],
                            (row-1)/2,
                            mScMap_add[det][id],
                            mScMap_sipm[det][id],
			    mMap_ppb[det][id],
			    mMap_ppp[det][id],
			    mMap_pph[det][id],
			    colJH[mMap_jcol[det][id]],
			    colW[mMap_wcol[det][id]],
			    lengH[mMap_jcol[det][id]]); 
		    fprintf(f1h,"%3d %3d %3d %3d     %3d %3d %3d %3d %3d %3d\n",
			    det,id,row,col,
			    mMap_ehp[det][id], 
			    mMap_ns[det][id], 
			    mMap_crt[det][id],
			    mMap_slt[det][id],
			    mMap_dep[det][id],
			    mMap_ch[det][id]); 
		    fprintf(f2,"%1d%02d-%02d%02d ",
			    mMap_crt[det][id],mMap_slt[det][id],
			    mMap_dep[det][id],mMap_ch[det][id]);
		}
		if(mScMap_ehp[det][id]>=0){
		    fprintf(f5,"%3d %3d %3d %3d     %3d %3d %3d %3d %3d %3d    %2d   J%1d\n",
                            det,id,row,col,
                            mScMap_ehp[det][id],
                            mScMap_ns [det][id],
                            mScMap_dep[det][id],
                            mScMap_bra[det][id],
                            mScMap_add[det][id],
                            mScMap_sipm[det][id],
                            mScMap_pp[det][id],
                            mScMap_j[det][id]);
		    fprintf(f5h,"%3d %3d %3d %3d     %3d %3d %3d %3d %3d %3d\n",
                            det,id,row,col,
                            mScMap_ehp[det][id],
                            mScMap_ns [det][id],
                            mScMap_dep[det][id],
                            mScMap_bra[det][id],
                            mScMap_add[det][id],
                            mScMap_sipm[det][id]);
		}
		id++;
	    }
	    fprintf(f2,"\n");
	}
    }
    
    //Prs
    for(int ns=0; ns<2; ns++){
	det=ns+4;
	id=0;
	fprintf(f2,"PRS NS=%1d\n",ns);
	for(row=1; row<=nRow(det); row++){
	    for(col=1; col<=nColumn(det); col++){
		if(mMap_ehp[det][id]>=0){
		    fprintf(f1,"%3d %3d %3d %3d     %3d %3d %3d %3d %3d %3d    %2d  %2d  %2d     %2d  P%1d H%02d %6s %6s %3.1f\n",
			    det,id,row,col,
			    mMap_ehp[det][id], 
			    mMap_ns[det][id], 
			    mMap_crt[det][id],
			    mMap_slt[det][id],
			    mMap_dep[det][id],
			    mMap_ch[det][id],
			    0,0,0,0,0,0,"x","x",0.0); 
		    fprintf(f1c,"%3d,%3d,%3d,%3d,%3d,%3d,%3d,%3d,%3d,%3d,%2d,%2d,%2d,%2d,P%1d,H%02d,%6s,%6s,%3.1f\n",
			    det,id,row,col,
			    mMap_ehp[det][id], 
			    mMap_ns[det][id], 
			    mMap_crt[det][id],
			    mMap_slt[det][id],
			    mMap_dep[det][id],
			    mMap_ch[det][id],
			    0,0,0,0,0,0,"x","x",0.0); 
		    fprintf(f1p,"%3d %3d %3d %3d     %3d %3d %3d %3d %3d %3d\n",
			    det,id,row,col,
			    mMap_ehp[det][id], 
			    mMap_ns[det][id], 
			    mMap_crt[det][id],
			    mMap_slt[det][id],
			    mMap_dep[det][id],
			    mMap_ch[det][id]); 
		    fprintf(f2,"%1d%02d-%02d%02d ",
			    mMap_crt[det][id],mMap_slt[det][id],
			    mMap_dep[det][id],mMap_ch[det][id]);
		}
		if(mScMap_ehp[det][id]>=0){
		    fprintf(f5,"%3d %3d %3d %3d     %3d %3d %3d %3d %3d %3d\n",
                            det,id,row,col,
                            mScMap_ehp[det][id],
                            mScMap_ns [det][id],
                            mScMap_dep[det][id],
                            mScMap_bra[det][id],
                            mScMap_add[det][id],
                            mScMap_sipm[det][id]);
		    fprintf(f5p,"%3d %3d %3d %3d     %3d %3d %3d %3d %3d %3d\n",
                            det,id,row,col,
                            mScMap_ehp[det][id],
                            mScMap_ns [det][id],
                            mScMap_dep[det][id],
                            mScMap_bra[det][id],
                            mScMap_add[det][id],
                            mScMap_sipm[det][id]);
		}
		id++;
	    }
	    fprintf(f2,"\n");
	}
    }
        
    //DEP map
    for(ehp=0; ehp<kFcsEHP; ehp++){
	for(ns=0; ns<2; ns++){
	    for(dep=0; dep<kFcsMaxDepBd; dep++){
		if(ehp==1 && dep>8) break;
		if(ehp==2 && dep>3) break;
		int flag=0;
		for(ch=0; ch<kFcsMaxDepCh; ch++){
		    if(mRMap_det[ehp][ns][dep][ch]<0){
			if(flag==1 && ch%8==7) fprintf(f4,"\n");
			continue;
		    }
		    flag=1;
		    det = mRMap_det[ehp][ns][dep][ch];
		    id  = mRMap_id[ehp][ns][dep][ch];
		    crt=mRMap_crt[ehp][ns][dep][ch];
		    slt=mRMap_slt[ehp][ns][dep][ch];
		    if(ch==0){
			fprintf(f4,"%2s%2d : NS=%1d %1d(%4s) DEP%02d : ",CRT[crt],slt,ns,ehp,EHP[ehp],dep);
		    }else if(ch%8==0){
			fprintf(f4,"                          : ");
		    }
		    if(det>=0 && det<kFcsNDet){
			row=getRowNumber(det,id);
			col=getColumnNumber(det,id);
			if(ehp<2){
			    const char* colj; float len;
			    if(ehp==0) { colj=colJ[mMap_jcol[det][id]];  len=leng[mMap_jcol[det][id]]; }
			    else       { colj=colJH[mMap_jcol[det][id]]; len=lengH[mMap_jcol[det][id]];}
 			    fprintf(f3,"%3d %3d %3d %3d   %2s %2d   %3d %3d %3d %3d      %2d  %2d  %2d     %2d  P%1d H%02d %6s %6s %3.1f\n",
				    ehp,ns,dep,ch,
				    CRT[crt],slt,
				    det,id,row,col,
				    (row-1)/2,
				    mScMap_add[det][id],
				    mScMap_sipm[det][id],
				    mMap_ppb[det][id],
				    mMap_ppp[det][id],
				    mMap_pph[det][id],
				    colj,
				    colW[mMap_wcol[det][id]],
				    len);
			    fprintf(f3c,"%3d,%3d,%3d,%3d,%2s,%2d,%3d,%3d,%3d,%3d,%2d,%2d,%2d,%2d,P%1d,H%02d,%6s,%6s,%3.1f\n",
				    ehp,ns,dep,ch,
				    CRT[crt],slt,
				    det,id,row,col,
				    (row-1)/2,
				    mScMap_add[det][id],
				    mScMap_sipm[det][id],
				    mMap_ppb[det][id],
				    mMap_ppp[det][id],
				    mMap_pph[det][id],
				    colj,
				    colW[mMap_wcol[det][id]],
				    len);
			}else{
 			    fprintf(f3,"%3d %3d %3d %3d   %2s %2d   %3d %3d %3d %3d\n",
				    ehp,ns,dep,ch,
				    CRT[crt],slt,
				    det,id,row,col);
			    fprintf(f3c,"%3d,%3d,%3d,%3d,%2s,%2d,%3d,%3d,%3d,%3d\n",
				    ehp,ns,dep,ch,
				    CRT[crt],slt,
				    det,id,row,col);
			}
		    }
		    fprintf(f4,"%2s%03d-%02d/%02d ",DET[det],id,row,col);  
		    if(ch%8==7) fprintf(f4,"\n");
		}
	    }
	}
    }

    //SC reverse map
    for(ehp=0; ehp<kFcsEHP; ehp++){
	for(ns=0; ns<2; ns++){
	    for(dep=0; dep<kFcsMaxDepBd; dep++){
		if(ehp==1 && dep>8) break;
		if(ehp==2 && dep>3) break;
		//		int flag=0;
		for(int bra=0; bra<kFcsMaxBranch; bra++){
		    for(int add=0; add<kFcsMaxAddr; add++){
			for(int sipm=0; sipm<kFcsMaxSiPM; sipm++){
			    if(mRScMap_det[ehp][ns][dep][bra][add][sipm]>=0){
				int det=mRScMap_det[ehp][ns][dep][bra][add][sipm];
				int id =mRScMap_id[ehp][ns][dep][bra][add][sipm];
				int col=getColumnNumber(det,id);
				int row=getRowNumber(det,id);
				fprintf(f6,"%3d %3d %3d %3d %3d %3d     %3d %3d %3d %3d\n",
					ehp,ns,dep,bra,add,sipm,
					det,id,row,col);
			    }
			}
		    }
		}
	    }
	}
    }
    
    //EPD map
    fprintf(f7,"#ehp ns crt slt dep  ch    det  id row col     pp  tt   Reversed(det id r c)\n");
    for(int det=4; det<=5; det++){
	for(int r=1; r<=nRow(det); r++){
	    for(int c=1; c<=nColumn(det); c++){
		int id = getId(det,r,c);
		int ehp,ns,crt,slt,dep,ch,det2,id2,r2,c2,pp,tt;
		getDepfromId(det,id,ehp,ns,crt,slt,dep,ch);
		getEPDfromId(det,id,pp,tt);
		getIdfromEPD(pp,tt,det2,id2);
		c2=getColumnNumber(det2,id2);
		r2=getRowNumber(det2,id2);
		fprintf(f7,"%3d %3d %3d %3d %3d %3d    %3d %3d %3d %3d    %3d %3d   %3d %3d %3d %3d\n",
			ehp,ns,crt,slt,dep,ch,
			det,id,r,c,
			pp,tt,
			det2,id2,r2,c2);
	    }
	}
    }

    for(int ehp=0; ehp<2; ehp++){
	for(int ns=0; ns<2; ns++){
	    if(ehp==0 && ns==1) continue;
	    int bmax=0;
	    if(ehp==0)          {bmax=8; fprintf(fpp,"Ecal\n");}
	    if(ehp==1 && ns==0) {bmax=4; fprintf(fpp,"Hcal North\n");}
	    if(ehp==1 && ns==1) {bmax=4; fprintf(fpp,"Hcal South\n");}
	    fprintf(fpp,"PPB# P# DEP  Ch  T                                                              Pwr/ctrl Row#\n");    
	    for(int b=1; b<=bmax; b++){
		for(int p=2; p<=7; p++){
		    int dep,lh,scr;
		    if(ehp==0){
			dep = EPPMap[b-1][p-2][0];
			lh  = EPPMap[b-1][p-2][1];
			scr = EPPMap[b-1][p-2][2];
		    }else{
			dep = HPPMap[b-1][p-2][0];
			lh  = HPPMap[b-1][p-2][1];
			scr = HPPMap[b-1][p-2][2];
		    }		    		
		    fprintf(fpp,"%2d %2d ",b,p);
		    if(lh>=0) {
			fprintf(fpp,"%2d  %02d-%02d  ",dep,lh*16,lh*16+15);
			for(int i=0; i<4; i++){
			    int t   = (p-2)*4+i;
			    int ch  = lh*16 + i*4;
			    int det = mRMap_det[ehp][ns][dep][ch];
			    int id  = mRMap_id[ehp][ns][dep][ch]; 
			    int row = getRowNumber(det,id);
			    int col = jacketColor(ehp,ns,dep,ch);
			    if(id>=0) { 
				if(ehp==0){
				    fprintf(fpp,"T%02d=R%02d-%6s  ",t,row,colJ[col]);
				}else{
				    fprintf(fpp,"T%02d=R%02d-%6s  ",t,row,colJH[col]);
				}
			    }else{
				fprintf(fpp,"T%02d=            ",t);
			    }
			}
		    }else{
			fprintf(fpp," -                                                                         ");
		    }
		    if(p%2==0) {
			fprintf(fpp,"J%1d   ",p/2);
			if(scr>0) fprintf(fpp,"%2d,%2d",scr*2-1,scr*2);
			else if(scr==-2) fprintf(fpp,"no power");
			else      fprintf(fpp,"-");
		    }
		    fprintf(fpp,"\n");
		}
	    }
	}
    }

    fclose(f1);
    fclose(f1c);
    fclose(f1e);
    fclose(f1h);
    fclose(f1p);
    fclose(f2);
    fclose(f3);
    fclose(f3c);
    fclose(f4);
    fclose(f5);
    fclose(f5e);
    fclose(f5h);
    fclose(f5p);
    fclose(f6);
    fclose(f7);
    fclose(fpp);
}

Float_t StFcsDbMaker::getEtGain(Int_t det, Int_t id) const{
  if(det<0 || det>=kFcsNDet) return 0.0;
  if(id<0 || id>=kFcsMaxId) return 0.0;
  return  mEtGain[det][id];
}

void  StFcsDbMaker::printEtGain(){
    FILE *f1 = fopen("fcsPtGain.txt","w");
    FILE *f2 = fopen("fcsPtGain2.txt","w");
    FILE *f3 = fopen("fcsPtGain3.txt","w");
    FILE *f4 = fopen("fcs_ecal_phys_gains.txt","w");
    FILE *f5 = fopen("fcs_hcal_phys_gains.txt","w");
    FILE *f6 = fopen("fcs_ecal_calib_gains.txt","w");
    FILE *f7 = fopen("fcs_hcal_calib_gains.txt","w");
    double norm[2]={0.24711, 0.21781}; // [MeV/coint]
    fprintf(f4,"#ehp ns  dep  ch   EtGain\n");
    fprintf(f5,"#ehp ns  dep  ch   EtGain\n");
    fprintf(f6,"#ehp ns  dep  ch   CalibGain\n");
    fprintf(f7,"#ehp ns  dep  ch   CalibGain\n");
    for(int det=0; det<kFcsNDet; det++){
	int id=0;
	int eh=det/2;
	double gain=getGain8(det,0);
	fprintf(f2,"DET=%1d ET/ch [unit = MeV/count]\n", det);
	fprintf(f3,"DET=%1d normalized ET/ch [unit=%f MeV/count]\n", det,norm[eh]);
        for(int row=1; row<=nRow(det); row++){
            for(int col=1; col<=nColumn(det); col++){
		StThreeVectorD xyz=getStarXYZ(det,id);
		double r=xyz.perp();
		double x=xyz.x();
		double y=xyz.y();
		double z=xyz.z();
		double l=xyz.mag();
		double ptch=gain/l*r;	    
		double ratio=ptch/norm[eh]*1000;
		mEtGain[det][id]=ratio;
		fprintf(f1,"D=%1d Id=%3d Row=%2d Column=%2d xyz=%7.2f %7.2f %7.2f Gain=%7.5f ET/ch=%6.4f [MeV/count] norm=%6.4f\n",
			det,id,row,col,x,y,z,gain,ptch*1000,ratio);
		fprintf(f2,"%7.5f ", ptch*1000);
		fprintf(f3,"%7.5f ", ratio);
		id++;
	    }
	    fprintf(f2,"\n");
	    fprintf(f3,"\n");
	}
    }
    for(int ehp=0; ehp<2; ehp++){
	for(int ns=0; ns<2; ns++){
	    for(int dep=0; dep<getNDep(ehp,ns); dep++){
		for(int ch=0; ch<kFcsMaxDepCh; ch++){
		    int det,id,crt,slt;
		    getIdfromDep(ehp,ns,dep,ch,det,id,crt,slt);
		    float ratio = mEtGain[det][id];
		    float calib = (ratio-1.0)/2.0+1.0;
		    if(ehp==0){
			fprintf(f4,"%3d %3d %3d %3d  %8.4f\n",ehp,ns,dep,ch,ratio);
			fprintf(f6,"%3d %3d %3d %3d  %8.4f\n",ehp,ns,dep,ch,calib);
		    }else{
			fprintf(f5,"%3d %3d %3d %3d  %8.4f\n",ehp,ns,dep,ch,ratio);
			fprintf(f7,"%3d %3d %3d %3d  %8.4f\n",ehp,ns,dep,ch,calib);
		    }
		}
	    }
	}
    }
    fclose(f1);
    fclose(f2);
    fclose(f3);
    fclose(f4);
    fclose(f5);
    fclose(f6);
    fclose(f7);
}

float StFcsDbMaker::pedestal(Int_t ehp, Int_t ns, Int_t dep, Int_t ch){
  return mPed[ehp][ns][dep][ch];
}

void StFcsDbMaker::setPedestal(Int_t ehp, Int_t ns, Int_t dep, Int_t ch, Float_t ped){
  mPed[ehp][ns][dep][ch]=ped;
}

void StFcsDbMaker::readPedFromText(const char* file){
  memset(mPed,0,sizeof(mPed));
  printf("Reading pedestal from %s\n",file);
  FILE* F=fopen(file,"r");
  if(F == NULL){
    printf( "Could not open %s\n",file);
    return;
  }
  int ehp,ns,dep,ch;
  float mean,rms;
  while(fscanf(F,"%d %d %d %d %f %f",&ehp,&ns,&dep,&ch,&mean,&rms) != EOF){
    mPed[ehp][ns][dep][ch]=mean;
    printf("PED %1d %1d %2d %2d %f %f\n",ehp,ns,dep,ch,mPed[ehp][ns][dep][ch],rms);	 
  }
  fclose(F);
}

void StFcsDbMaker::readGainFromText(const char* file){
  memset(mGain,0,sizeof(mGain));
  printf("Reading gain from %s\n",file);
  FILE* F=fopen(file,"r");
  if(F == NULL){
    printf( "Could not open %s\n",file);
    return;
  }
  mReadGainFromText=1;
  int ehp,ns,dep,ch;
  float gain;
  while(fscanf(F,"%d %d %d %d %f",&ehp,&ns,&dep,&ch,&gain) != EOF){
    mGain[ehp][ns][dep][ch]=gain;
    printf("GAIN %1d %1d %2d %2d %f\n",ehp,ns,dep,ch,mGain[ehp][ns][dep][ch]);	 
  }
  fclose(F);
}

void StFcsDbMaker::readGainCorrFromText(const char* file){
  memset(mGainCorr,0,sizeof(mGainCorr));
  printf("Reading gain corr from %s\n",file);
  FILE* F=fopen(file,"r");
  if(F == NULL){
    printf( "Could not open %s\n",file);
    return;
  }
  mReadGainCorrectionFromText=1;
  int ehp,ns,dep,ch;
  float gain;
  while(fscanf(F,"%d %d %d %d %f",&ehp,&ns,&dep,&ch,&gain) != EOF){
    mGainCorr[ehp][ns][dep][ch]=gain;
    printf("GAIN CORR  %1d %1d %2d %2d %f\n",ehp,ns,dep,ch,mGainCorr[ehp][ns][dep][ch]);	 
  }
  fclose(F);
}
