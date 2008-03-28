/***************************************************************************
 *
 * $Id: StTpcDb.cxx,v 1.45 2007/07/19 22:19:23 perev Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: This is the interface between the database and the offline
 *              TPC software.  This classes takes care of the annoying
 *              calls to the root infrastucture, packages and manipulates
 *              the data, and returns the data to the user via simple
 *              interface classes.    
 *
 ***************************************************************************
 *
 * $Log: StTpcDb.cxx,v $
 * Revision 1.45  2007/07/19 22:19:23  perev
 * Bug in drift velocity fixed
 *
 * Revision 1.44  2007/07/12 20:21:09  fisyak
 * Drift velocity depends on TPC half, use online RHIC clock
 *
 * Revision 1.43  2007/04/16 22:51:03  fisyak
 * Add protection from infinit endTime
 *
 * Revision 1.42  2007/04/15 20:57:01  fisyak
 * Add drift velocity interpolation between two measurement in time
 *
 * Revision 1.41  2007/03/21 17:27:01  fisyak
 * use TGeoHMatrix, change mode for switching drift velocities
 *
 * Revision 1.40  2005/07/06 22:26:53  fisyak
 * dEdx_t=>dEdxY2_t
 *
 * Revision 1.39  2005/03/30 17:56:59  fisyak
 * Fix a bug with flavor handling, StTpcDb has to be instantiated after setting flavor
 *
 * Revision 1.38  2004/11/19 10:21:54  jecc
 * Initialize pointers
 *
 * Revision 1.37  2004/10/27 21:44:28  fisyak
 * Add debug print for tables Validities, add access to ExB correction
 *
 * Revision 1.36  2004/03/16 22:17:46  jecc
 * Update triggerTimeOffset() due to a change in L0 TriggerActionWd
 *
 * Revision 1.35  2004/02/23 00:35:00  fisyak
 * Add access to tpcPadResponse
 *
 * Revision 1.34  2004/01/14 22:54:30  fisyak
 * Add hooks for Pedestal and tpcGain
 *
 * Revision 1.33  2002/04/02 00:16:30  hardtke
 * New class that gets hit errors from database
 *
 * Revision 1.32  2002/02/06 18:39:13  hardtke
 * Add tpc Field Cage structure
 *
 * Revision 1.31  2001/08/14 18:18:03  hardtke
 * Add sector position structures
 *
 * Revision 1.30  2001/06/20 22:25:26  hardtke
 * Get TRS gain parameters from tsspar table
 *
 * Revision 1.29  2001/05/21 23:25:34  hardtke
 * Add tpcGlobalPosition to StTpcDb.  This includes the global position offset and the rotation w.r.t. the magnet
 *
 * Revision 1.28  2000/08/18 17:19:21  hardtke
 * use laser velocity, if available
 *
 * Revision 1.27  2000/08/10 18:41:34  hardtke
 * only look for L0_trigger table once per event -- improves timing
 *
 * Revision 1.26  2000/08/09 14:54:54  hardtke
 * Add Clear option, set trigger table pointer to 0 after each event
 *
 * Revision 1.25  2000/08/09 13:00:03  hardtke
 * Add protections to make sure trigger table is filled before using
 *
 * Revision 1.24  2000/08/08 19:15:22  hardtke
 * use correct trigger time offset in case of laser
 *
 * Revision 1.23  2000/08/04 21:03:55  perev
 * Leaks + Clear() cleanup
 *
 * Revision 1.22  2000/05/12 20:31:38  fisyak
 * Add ClassImp for abstract classes, new rootcint requires them
 *
 * Revision 1.21  2000/05/11 17:17:27  hardtke
 * make trigger time offset available -- currently NOT different for beam and laser events
 *
 * Revision 1.20  2000/04/05 15:44:56  hardtke
 * fix solaris bug -- char* was too short for table name
 *
 * Revision 1.19  2000/03/30 17:02:36  hardtke
 * limit warning message in StRTpcPadPlane
 *
 * Revision 1.18  2000/02/23 21:03:17  hardtke
 * fix tpc_row_par -- causing tpt problems
 *
 * Revision 1.17  2000/02/15 22:21:47  hardtke
 * Add effective drift distances
 *
 * Revision 1.16  2000/02/10 00:29:08  hardtke
 * Add tpg functions to StTpcDbMaker, fix a few bugs
 *
 * Revision 1.15  2000/01/24 15:31:31  hardtke
 * change to use new gain and t0 tables
 *
 * Revision 1.14  2000/01/11 15:49:52  hardtke
 * get Electronics table from Calibrations database, Fix error messages
 *
 * Revision 1.13  1999/12/16 22:00:53  hardtke
 * add CVS tags
 *
 **************************************************************************/
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTpcDb                                                              //
//                                                                      //
//                                                                      //
 
#include "StChain.h"
#include "St_Table.h"
#include "StTpcDb.h"
#include "tables/St_tpcDriftVelocity_Table.h"
#include "tables/St_trgTimeOffset_Table.h"
#include "tables/St_dst_L0_Trigger_Table.h"
#include "TUnixTime.h"
StTpcDb* gStTpcDb = 0;

// C++ routines:
//_____________________________________________________________________________


#ifdef __ROOT__
ClassImp(StTpcDb)
ClassImp(StTpcWirePlaneI)
ClassImp(StTpcDimensionsI)
ClassImp(StTpcElectronicsI)
ClassImp(StTpcGainI)
ClassImp(StTpcPadPlaneI)
ClassImp(StTpcSlowControlSimI)
ClassImp(StTpcT0I)
ClassImp(StTpcGlobalPositionI)
ClassImp(StTpcFieldCageI)
ClassImp(StTpcHitErrorsI)
ClassImp(StTpcSectorPositionI)
#endif
//_____________________________________________________________________________
StTpcDb::StTpcDb(St_DataSet* input) : m_Debug(0) {
 assert(gStTpcDb==0);
 memset(this,0,sizeof(StTpcDb));
 if (input){
   const Char_t *bases[] = {"Calibrations","Geometry","Conditions"};
   int lBases = sizeof(bases)/sizeof(Char_t *);
   St_DataSetIter dataBase(input);
   int i;
   for (i = 0;i<2;i++,dataBase.Cd("/") )
     if ( !(tpctrg[i] = dataBase.Cd(bases[i]) ? dataBase("tpc") : 0 ) ){
       gMessMgr->Warning() << "StTpcDb::Error Getting TPC database: " << bases[i]       << endm;
     }
   for (i = 2;i<lBases;i++,dataBase.Cd("/") )   //only need conditions for trg
     if ( !(tpctrg[i] = dataBase.Cd(bases[i]) ? dataBase("trg") : 0 ) ){
       gMessMgr->Warning() << "StTpcDb::Error Getting trigger database: " << bases[i]       << endm;
     }
 }
 else{
   gMessMgr->Message("StTpcDb::Error Creating StTpcDb: Need to specify input DataSet","E");
 }
 gMessMgr->SetLimit("StRTpcPadPlane::Invalid Pad number",20);
 dvelcounter = 0;
 mExB = 0;
 mTpc2GlobalMatrix = new TGeoHMatrix("Default Tpc2Global"); 
 gStTpcDb = this;
}

//_____________________________________________________________________________
StTpcDb::StTpcDb(StMaker* maker) : m_Debug(0) {
 assert(gStTpcDb==0);
 memset(this,0,sizeof(StTpcDb));
 mk = maker;
 if (maker) GetDataBase(maker);
 gMessMgr->SetLimit("StRTpcPadPlane::Invalid Pad number",20);
 dvelcounter=0;
 mTpc2GlobalMatrix = new TGeoHMatrix("Default Tpc2Global"); 
 gStTpcDb = this;
}

void StTpcDb::Clear(){
  trigtype = 0;
  dvelcounter = 0;
  return;
}
//_____________________________________________________________________________
void StTpcDb::GetDataBase(StMaker* maker) {
 if (maker){
   const Char_t *bases[] = {"Calibrations/","Geometry/","Conditions/"};
   int lBases = sizeof(bases)/sizeof(Char_t *);
   int i;
   for (i = 0;i<2;i++) {
     TString dbFullPath = "StDb/";
     TString dbPath = bases[i];
     dbPath += "tpc";
     dbFullPath += dbPath;
     if ( ( tpctrg[i] = maker->GetDataBase(dbPath)) || 
          ( tpctrg[i] = maker->GetDataBase(dbFullPath)) ) continue;
     gMessMgr->Warning() << "StTpcDb::Error Getting TPC database: " << bases[i] << "   " << endm;
   }
   for (i = 2;i<lBases;i++) {
     TString dbFullPath = "StDb/";
     TString dbPath = bases[i];
     dbPath += "trg";
     dbFullPath += dbPath;
     if ( ( tpctrg[i] = maker->GetDataBase(dbPath)) || 
          ( tpctrg[i] = maker->GetDataBase(dbFullPath)) ) continue;
     gMessMgr->Warning() << "StTpcDb::Error Getting trg database: " << bases[i] << "   " << endm;
   }
 }
 else{
   gMessMgr->Message("StTpcDb::Error Getting TPC database","E");
 }
}

//_____________________________________________________________________________
StTpcDb::~StTpcDb() {
  //delete PadPlane;
  //delete WirePlane;
  //delete dimensions;
  //delete slowControlSim;
  //delete electronics;

for (int i = 0;i<24;i++) {
  //    delete gain[i]; delete t0[i];
}
 delete  mPedestal; mPedestal = 0;
 delete  mGain; mGain = 0;
 SafeDelete(mTpc2GlobalMatrix);
gStTpcDb = 0;
}
//_____________________________________________________________________________

StTpcPadPlaneI* StTpcDb::PadPlaneGeometry(){
  if (!PadPlane){            // get pad plane from data base
   const int dbIndex = kGeometry;
   if (tpctrg[dbIndex]){
     //    St_DataSet *tpd = tpctrg[dbIndex]->Find("tpcPadPlanes");
     St_DataSet *tpd = FindTable("tpcPadPlanes",dbIndex);
    if (!tpd) {
      gMessMgr->Message("StTpcDb::Error Finding Tpc Pad Planes","E");
      return 0;
    }   
   PadPlane = new StRTpcPadPlane((St_tpcPadPlanes*)tpd);
   }
  }
  return PadPlane;
}

//_____________________________________________________________________________
StTpcWirePlaneI* StTpcDb::WirePlaneGeometry(){
  if (!WirePlane){            // get wire plane from data base
   const int dbIndex = kGeometry;
   if (tpctrg[dbIndex]){
     //    St_DataSet* tpd = tpctrg[dbIndex]->Find("tpcWirePlanes");
     St_DataSet* tpd = FindTable("tpcWirePlanes",dbIndex);
    if (!(tpd && tpd->HasData()) ){
     gMessMgr->Message("StTpcDb::Error Finding Tpc Wire Planes","E");
     return 0;
    }   
    WirePlane = new StRTpcWirePlane((St_tpcWirePlanes*)tpd);
   }
  }
 return WirePlane;
}

//_____________________________________________________________________________
StTpcDimensionsI* StTpcDb::Dimensions(){
  if (!dimensions){            // get wire plane from data base
   int dbIndex = kGeometry;
   St_DataSet *tpd=0;
   St_DataSet *geo=0;
   if (tpctrg[dbIndex]){
     //    tpd = tpctrg[dbIndex]->Find("tpcDimensions");
     tpd = FindTable("tpcDimensions",dbIndex);
    if (!(tpd && tpd->HasData()) ){
     gMessMgr->Message("StTpcDb::Error Finding Tpc Dimensions","E");
     return 0;
    }
   }
   dbIndex = kCalibration;
   if (tpctrg[dbIndex]){
     //    geo = tpctrg[dbIndex]->Find("tpcEffectiveGeom");
     geo = FindTable("tpcEffectiveGeom",dbIndex);
     if (!(geo && geo->HasData()) ){
       gMessMgr->Message("StTpcDb::Error Finding Tpc Effective Geometry","E");
       return 0;
     }
   }
   StRTpcDimensions* rdimensions =  new StRTpcDimensions((St_tpcDimensions*)tpd,(St_tpcEffectiveGeom*)geo);
   rdimensions->SetPadPlanePointer(PadPlaneGeometry());
   rdimensions->SetWirePlanePointer(WirePlaneGeometry());
   dimensions = (StTpcDimensionsI*)rdimensions;
  }
  return dimensions;
}

//_____________________________________________________________________________
StTpcSlowControlSimI* StTpcDb::SlowControlSim(){
  if (!slowControlSim){            // get wire plane from data base
   const int dbIndex = kCalibration;
   if (tpctrg[dbIndex]){
     //    St_DataSet* tpd = tpctrg[dbIndex]->Find("tpcSlowControlSim");
     St_DataSet* tpd = FindTable("tpcSlowControlSim",dbIndex);
    if (!(tpd && tpd->HasData()) ){
     gMessMgr->Message("StTpcDb::Error Finding Slow Control Simulations Parameters","E");
     return 0;
    }
    St_DataSet *olddb = mk->GetDataBase("tpc");
    St_tss_tsspar* tss = (St_tss_tsspar*)olddb->Find("tsspars/tsspar");
    if (!(tss&&tss->HasData())){
      gMessMgr->Message("StTpcDb::Error Finding tsspars Parameters","E");  
      return 0;
     }
   slowControlSim = new StRTpcSlowControlSim((St_tpcSlowControlSim*)tpd,tss);
   }
  }
 return slowControlSim;
}

//_____________________________________________________________________________
StTpcElectronicsI* StTpcDb::Electronics(){
  if (!electronics){            // get electronics from data base
    const int dbIndex = kCalibration;
    if (tpctrg[dbIndex]){
      //    St_DataSet* tpd = tpctrg[dbIndex]->Find("tpcElectronics");
      St_DataSet* tpd = FindTable("tpcElectronics",dbIndex);
      if (!(tpd && tpd->HasData()) ){
	gMessMgr->Message("StTpcDb::Error Finding Tpc Electronics","E");
	return 0;
      }
      electronics = new StRTpcElectronics((St_tpcElectronics*)tpd);
    }
  }
  return electronics;
}

//_____________________________________________________________________________
StTpcGlobalPositionI* StTpcDb::GlobalPosition(){
  if (!GlobPos){            // get global position from data base
   const int dbIndex = kGeometry;
   if (tpctrg[dbIndex]){
     //    St_DataSet* tpd = tpctrg[dbIndex]->Find("tpcGlobalPosition");
     St_DataSet *tpd = FindTable("tpcGlobalPosition",dbIndex);
    if (!(tpd && tpd->HasData()) ){
     gMessMgr->Message("StTpcDb::Error Finding Tpc Global Positions","E");
     return 0;
    }
    GlobPos = new StRTpcGlobalPosition((St_tpcGlobalPosition*)tpd);
   }
  }
 return GlobPos;
}

//_____________________________________________________________________________
StTpcFieldCageI* StTpcDb::FieldCage(){
  if (!FC){            // get field cage from data base
   const int dbIndex = kGeometry;
   if (tpctrg[dbIndex]){
     //    St_DataSet* tpd = tpctrg[dbIndex]->Find("tpcFieldCage");
     St_DataSet *tpd = FindTable("tpcFieldCage",dbIndex);
    if (!(tpd && tpd->HasData()) ){
     gMessMgr->Message("StTpcDb::Error Finding Tpc Field Cage Info","E");
     return 0;
    }
    FC = new StRTpcFieldCage((St_tpcFieldCage*)tpd);
   }
  }
 return FC;
}

//_____________________________________________________________________________
StTpcHitErrorsI* StTpcDb::HitErrors(){
  if (!hitErrors){            // get hit errors from data base
   const int dbIndex = kCalibration;
   if (tpctrg[dbIndex]){
     //    St_DataSet* tpd = tpctrg[dbIndex]->Find("tpcHitErrors");
     St_DataSet *tpd = FindTable("tpcHitErrors",dbIndex);
    if (!(tpd && tpd->HasData()) ){
     gMessMgr->Message("StTpcDb::Error Finding Tpc Hit Error Info","E");
     return 0;
    }
    hitErrors = new StRTpcHitErrors((St_tpcHitErrors*)tpd);
   }
  }
 return hitErrors;
}

//_____________________________________________________________________________
StTpcGainI* StTpcDb::Gain(int sector){
  if(sector<1||sector>24){
    gMessMgr->Message("StTpcDb::Gains request for invalid sector","E");
    return 0;
  }
  if(!gain[sector-1]){
   const int dbIndex = kCalibration;
   char dbname[25],dbname2[25];
   sprintf(dbname,"Sector_%.2d/tpcISGains",sector);
   sprintf(dbname2,"Sector_%.2d/tpcOSGains",sector);
   //   printf("Getting %s , %s\n",dbname,dbname2);
   if (tpctrg[dbIndex]){
     //    St_DataSet* tpd = tpctrg[dbIndex]->Find(dbname);
     //    St_DataSet* tpd2 = tpctrg[dbIndex]->Find(dbname2);
     St_DataSet *tpd = FindTable(dbname,dbIndex);
     St_DataSet *tpd2 = FindTable(dbname2,dbIndex);
    if (!(tpd && tpd->HasData() && tpd2 && tpd2->HasData()) ){
     gMessMgr->Message("StTpcDb::Error Finding Tpc Gain Factors","E");
     return 0;
    }
    StRTpcGain* wptemp = new StRTpcGain((St_tpcISGains*)tpd,(St_tpcOSGains*)tpd2);
    wptemp->SetPadPlanePointer(PadPlaneGeometry());
    gain[sector-1] = (StTpcGainI*)wptemp;
   }
  }
 return gain[sector-1];
}

//_____________________________________________________________________________
StTpcT0I* StTpcDb::T0(int sector){
  if(sector<1||sector>24){
    gMessMgr->Message("StTpcDb::T0s request for invalid sector","E");
    return 0;
  }
  if(!t0[sector-1]){
   const int dbIndex = kCalibration;
   char dbname[40],dbname2[40];
   sprintf(dbname,"Sector_%.2d/tpcISTimeOffsets",sector);
   sprintf(dbname2,"Sector_%.2d/tpcOSTimeOffsets",sector);
   //   printf("Getting %s , %s \n",dbname,dbname2);
   if (tpctrg[dbIndex]){
     //    St_DataSet* tpd = (St_DataSet*)tpctrg[dbIndex]->Find(dbname);
     //    St_DataSet* tpd2 = (St_DataSet*)tpctrg[dbIndex]->Find(dbname2);
     St_DataSet *tpd = FindTable(dbname,dbIndex);
     St_DataSet *tpd2 = FindTable(dbname2,dbIndex);
    if (!(tpd && tpd->HasData() && tpd2 && tpd2->HasData()) ){
     gMessMgr->Message("StTpcDb::Error Finding Tpc Time Offsets","E");
     return 0;
    }
    StRTpcT0* wptemp = new StRTpcT0((St_tpcISTimeOffsets*)tpd,(St_tpcOSTimeOffsets*)tpd2);
    wptemp->SetPadPlanePointer(PadPlaneGeometry());
    t0[sector-1] = (StTpcT0I*)wptemp;
   }
  }
 return t0[sector-1];
}

//_____________________________________________________________________________
StTpcSectorPositionI* StTpcDb::SectorPosition(int sector){
  
  if(sector<1||sector>24){
    gMessMgr->Message("StTpcDb::SectorPosition request for invalid sector","E");
    return 0;
  }
  if(!sect[sector-1]){
   const int dbIndex = kGeometry;
   char dbname[40];
   sprintf(dbname,"Sector_%.2d/tpcSectorPosition",sector);
   if (tpctrg[dbIndex]){
     //    St_DataSet* tpd = (St_DataSet*)tpctrg[dbIndex]->Find(dbname);
     St_DataSet *tpd = FindTable(dbname,dbIndex);
    if (!(tpd && tpd->HasData()) ){
     gMessMgr->Message("StTpcDb::Error Finding Tpc Sector Position","E");
     return 0;
    }
    StRTpcSectorPosition* wptemp = new StRTpcSectorPosition((St_tpcSectorPosition*)tpd);
    assert(wptemp);
    sect[sector-1] = (StTpcSectorPositionI*)wptemp;
   }
  }
 return sect[sector-1];
}

//_____________________________________________________________________________
St_Table *StTpcDb::getTpcTable(int i){
  return (St_Table *)tpctrg[i];
}
//-----------------------------------------------------------------------------
float StTpcDb::DriftVelocity(Int_t sector)
{
  static UInt_t u0 = 0; // beginTime of current Table
  static UInt_t u1 = 0; // beginTime for next Table
  static UInt_t umax = TUnixTime(20250101,0,1).GetUTime(); // maximum time allowed for next table
  static St_tpcDriftVelocity *dvel0 = 0;
  static St_tpcDriftVelocity *dvel1 = 0;
  static StMaker *mk = StMaker::GetChain();
  Float_t driftvel = 0;
  UInt_t uc = TUnixTime(mk->GetDateTime(),1).GetUTime();

  if (! dvel0 ) {//First time only
    dvel0 = (St_tpcDriftVelocity *) mk->GetDataBase("Calibrations/tpc/tpcDriftVelocity");
    if (! dvel0) {
      gMessMgr->Message("StTpcDb::Error Finding Tpc DriftVelocity","E");
      return 0;
    }
  }//End First time only

  if (!(u0<=uc && uc<u1)) {//current time out of validity
    TDatime t[2];
    if (mk->GetValidity(dvel0,t) < 0) {
      gMessMgr->Message("StTpcDb::Error Wrong Validity Tpc DriftVelocity","E");
      return 0;
    }

    u0 = TUnixTime(t[0],1).GetUTime();
    u1 = TUnixTime(t[1],1).GetUTime();
    SafeDelete(dvel1);
    if (u1 < umax && u1 - u0 < 7*24*3600) {// next drift velocity should within a week from current
      dvel1 = (St_tpcDriftVelocity *) mk->GetDataBase("Calibrations/tpc/tpcDriftVelocity",&t[1]);
      if (! dvel1) {
	gMessMgr->Message("StTpcDb::Error Finding next Tpc DriftVelocity","W");
      }
    }
  }

  tpcDriftVelocity_st *d0 = dvel0->GetTable();
  if (dvel1) {
    tpcDriftVelocity_st *d1 = dvel1->GetTable();
    driftvel = sector > 12 ? 
      (d0->laserDriftVelocityEast  *(uc-u0) + d1->laserDriftVelocityEast  *(u1-uc))/(u1 - u0):
      (d0->laserDriftVelocityWest  *(uc-u0) + d1->laserDriftVelocityWest  *(u1-uc))/(u1 - u0);
    if (driftvel<= 0.0)  driftvel = sector > 12 ? 
      (d0->cathodeDriftVelocityEast*(uc-u0) + d1->cathodeDriftVelocityEast*(u1-uc))/(u1 - u0):
      (d0->cathodeDriftVelocityWest*(uc-u0) + d1->cathodeDriftVelocityWest*(u1-uc))/(u1 - u0);
  } else {
    driftvel = sector > 12 ? d0->laserDriftVelocityEast   : d0->laserDriftVelocityWest;
    if (driftvel<=0.0) 
    driftvel = sector > 12 ? d0->cathodeDriftVelocityEast : d0->cathodeDriftVelocityWest;
  }
  return 1e6*driftvel;
}



//-----------------------------------------------------------------------------
float StTpcDb::triggerTimeOffset(){
  if (!trigtype&&dvelcounter==0) trigtype = (St_dst_L0_Trigger*)mk->GetChain()->GetDataSet("L0_Trigger");
  dvelcounter++;
  if(!toff){              // get triggerTimeOffset
   const int dbIndex = kConditions;
   if (tpctrg[dbIndex]){
     //    St_DataSet* tpd = tpctrg[dbIndex]->Find("trgTimeOffset");
     St_DataSet *tpd = FindTable("trgTimeOffset",dbIndex);
    if (!(tpd && tpd->HasData()) ){
     gMessMgr->Message("StTpcDb::Error Finding Trigger Time Offset","E");
     return 0;
    }
    toff = (St_trgTimeOffset*)tpd;
   }
  }
  //  assert(trig);
  float theoffset = 1e-6*(*toff)[0].offset;
  if(trigtype&&trigtype->HasData()){if((0xff00 & (*trigtype)[0].TriggerActionWd)==(0xff00 & 36865)) 
    theoffset = 1e-6*(*toff)[0].laserOffset;}
  return theoffset;
}
//________________________________________________________________________________
TTable *StTpcDb::FindTable(const Char_t *name, Int_t dbIndex) {
  TTable *table = 0;
  if (tpctrg[dbIndex]){
    table = (TTable *) tpctrg[dbIndex]->Find(name);
    if (! (table && table->HasData()) ) {
      gMessMgr->Error() << "StTpcDb::Error Finding " << name << endm;
    } else if (Debug() && table->GetRowSize()< 1024) {
      StMaker *dbMk = StChain::GetChain()->Maker("db");
      if (dbMk) {
	TDatime t[2];
	dbMk->GetValidity(table,t);
	gMessMgr->Warning()  << " Validity:" << t[0].GetDate() << "/" << t[0].GetTime()
			     << "  -----   " << t[1].GetDate() << "/" << t[1].GetTime() << endm;
      }
      if (Debug()) table->Print(0,table->GetNRows());
    }
  }
  return table;
}
//________________________________________________________________________________
St_tpcPedestalC *StTpcDb::Pedestal() {
  if (!mPedestal){
    St_tpcPedestal *table = (St_tpcPedestal *) FindTable("tpcPedestal",kCalibration);
    if (! table) return 0;
    mPedestal = new St_tpcPedestalC(table);
  }
  return mPedestal;
}
//________________________________________________________________________________
St_tpcGainC     *StTpcDb::tpcGain() {
  if (! mGain){
    St_tpcGain *table = (St_tpcGain *) FindTable("tpcGain",kCalibration);
    if (! table) return 0;
    mGain = new St_tpcGainC(table);
  }
  return mGain;
}
//________________________________________________________________________________
St_tpcPadResponseC     *StTpcDb::PadResponse() {
  if (! mPadResponse){
    St_tpcPadResponse *table = (St_tpcPadResponse *) FindTable("tpcPadResponse",kCalibration);
    if (! table) return 0;
    mPadResponse = new St_tpcPadResponseC(table);
  }
  return mPadResponse;
}
//________________________________________________________________________________
void StTpcDb::SetTpc2GlobalMatrix(TGeoHMatrix *m) {
  if (m) {
    if (! mTpc2GlobalMatrix)  mTpc2GlobalMatrix = new TGeoHMatrix("Default Tpc2Global"); 
    *mTpc2GlobalMatrix = *m;
  }
}






