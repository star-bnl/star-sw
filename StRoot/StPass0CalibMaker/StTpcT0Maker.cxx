//*-- Author : David Hardtke
// 
// $Id: StTpcT0Maker.cxx,v 1.14 2009/01/26 14:49:53 fisyak Exp $
// $Log: StTpcT0Maker.cxx,v $
// Revision 1.14  2009/01/26 14:49:53  fisyak
// Account the fact that drift velocities are different for East and West part of TPC
//
// Revision 1.13  2007/05/29 22:27:35  fine
// Introduce logger-based output
//
// Revision 1.12  2007/04/28 17:56:30  perev
// Redundant StChain.h removed
//
// Revision 1.11  2005/12/06 16:37:44  genevb
// Remove compiler warnings
//
// Revision 1.10  2005/04/23 09:41:29  jeromel
// Minor change (wrap private root file)
//
// Revision 1.9  2005/04/14 20:26:41  jecc
// Now can also use FCF. Switch between TCL and FCF using m_Mode
//
// Revision 1.8  2004/06/08 23:02:32  jeromel
// Made changes according to Javier. Should made it db.
//
// Revision 1.7  2004/03/17 02:29:58  jecc
// Oups... remove useless StPreVertexMaker
//
// Revision 1.6  2004/03/16 22:23:53  jecc
// Now call also StPreVertexMaker and StVertexMaker, to avoidseg. violation as primary vertex was not in table
//
// Revision 1.5  2003/09/02 17:58:45  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.4  2003/07/18 18:31:48  perev
// test for nonexistance of XXXReader added
//
// Revision 1.3  2002/04/19 22:24:16  perev
// fixes for ROOT/3.02.07
//
// Revision 1.2  2002/02/05 22:20:54  hardtke
// Move Init code to InitRun
//
// Revision 1.1  2002/01/26 18:55:33  jeromel
// StTpcT0Maker moved from directory of the same name. First version
// of StVertexSeedMaker.
//
// Revision 1.12  2001/09/25 20:10:30  hardtke
// Add ad hoc correction factor to take care of bias suggested by TRS
//
// Revision 1.11  2001/08/21 18:48:38  hardtke
// Add StMatchMaker::InitRun() call to handle no field data
//
// Revision 1.10  2001/07/18 20:52:13  hardtke
// Extend range of histogram for t0 result
//
// Revision 1.9  2001/04/25 19:08:33  perev
// HPcorrs
//
// Revision 1.8  2001/04/17 23:54:51  hardtke
// add z Vertex contraint -- default to +-40cm
//
// Revision 1.7  2001/03/15 19:49:02  hardtke
// Add diagnostic ntuple t0hist file
//
// Revision 1.6  2001/03/09 22:44:43  hardtke
// Add vertex diagnostic histograms, create root file with these histograms by default
//
// Revision 1.5  2000/09/11 17:48:50  hardtke
// save values of trig offset, dvel, and tpc length for use in Finish()
//
// Revision 1.4  2000/09/01 21:24:28  hardtke
// Add kluge to evade infrastructure bug
//
// Revision 1.3  2000/08/28 17:42:29  hardtke
// Add new histogram
//
// Revision 1.2  2000/08/26 23:50:04  fisyak
// Make it chain
//
// Revision 1.1  2000/08/24 23:51:27  hardtke
// New package for drift velocity calibrations
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTpcT0Maker class                                                   //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <Stiostream.h>
#include "Stiostream.h"
#include "StTpcT0Maker.h"
#include "St_DataSetIter.h"
#include "StTpcDb/StTpcDbMaker.h"
#include "StTpcDb/StTpcDb.h"
#include "St_tcl_Maker/St_tcl_Maker.h"
#include "StRTSClient/FCFMaker/FCFMaker.h"
#include "StTpcHitMoverMaker/StTpcHitMoverMaker.h"
#include "tables/St_dst_vertex_Table.h"
#include "tables/St_dst_L0_Trigger_Table.h"
#include "StMessMgr.h"
#include "StVertexId.h"
#include "tables/St_tpcDriftVelocity_Table.h"
#include "TSystem.h"
#include "TFile.h"

ClassImp(StTpcT0Maker)

// #define JECC_DEBUG 1

//_____________________________________________________________________________
/// Default constructor
StTpcT0Maker::StTpcT0Maker(const char *name):StMaker(name){
  minEntries     = 5;         // require 5 valid t0s for a velocity determination
  desiredEntries = 40;        // must set limit by hand if set to 99999 ; was 10 in bfc.C until 2004
  maxRMS         = 0.05;      // t0 should be good to 50 ns
  //  mHistOut   = kFALSE;
  mHistOut=kTRUE;

  zVertexMin = -25.0; // was -40.0 until 2004
  zVertexMax =  25.0; // was 40.0  until 2004

  //  saveMk->cd();
}
//_____________________________________________________________________________
/// Dummy destructor
StTpcT0Maker::~StTpcT0Maker(){
}


//_____________________________________________________________________________
Int_t StTpcT0Maker::Init(){
  t0guess = 0;
  zVertexWest = -999.0;
  zVertexEast = -999.0; 
  T0HIST_MIN = 35.0;
  T0HIST_MAX = 38.0;
  SetCorrectionFactors(-9.7e-2,9.7e-5,-8e-6);  // take defaults from TRS
  t0result = new TH1F("t0result","t0result",1000,T0HIST_MIN,T0HIST_MAX);
  t0guessError = new TH1F("t0guessError","t0 measured - t0 guess",1000,-1,1);
  xVertexDiff = new TH1F("xVertexDiff","x Vertex: East - West",600,-0.3,0.3);
  yVertexDiff = new TH1F("yVertexDiff","y Vertex: East - West",600,-0.3,0.3);
  zVertexDiff = new TH1F("zVertexDiff","z Vertex: East - West",600,-0.3,0.3);
  resNtuple = new TNtuple("resNtuple","resNtuple","event:xEast:yEast:zEast:xWest:yWest:zWest:multEast:multWest:corrDelZ");

  AddHist(t0result);
  AddHist(t0guessError);
  AddHist(xVertexDiff);
  AddHist(yVertexDiff);
  AddHist(zVertexDiff);

  date = 0;
  time = 0;
  dvel_assumed=0.0;
  trigger_assumed=0.0;
  length_assumed=0.0;

  StMaker *saveMk = cd();
  if(m_Mode&0x1){    
    gMessMgr->Info() << "StTpcT0Maker will use fcf!" << endm; 
    New("StRTSClientFCFMaker","tpc_hits");  
  }
  else {
    gMessMgr->Info() << "StTpcT0Maker will use tcl!" << endm;
    New("St_tcl_Maker","tpc_hits");    
  }

  New("St_tpt_Maker","tpc_tracks");
  New("StMatchMaker","match");
  New("StVertexMaker","vertex"); 
  New("StPrimaryMaker","primary");
  New("St_dst_Maker","dst");


  saveMk->cd();


  return StMaker::Init();
}

Int_t StTpcT0Maker::InitRun(int runnumber){
  theDb = ((StTpcDbMaker*)GetMaker("tpcDB"))->tpcDbInterface();
  if (!theDb) return kStErr;
  return kStOK;
}

void StTpcT0Maker::Clear(Option_t *option){
  t0guess = 0;
  zVertexWest =  -999.0;
  zVertexEast = -1999.0; 
  yVertexWest =  -999.0;
  yVertexEast =   999.0; 
  xVertexWest =  -999.0;
  xVertexEast =   999.0; 
}

//_____________________________________________________________________________
Int_t StTpcT0Maker::Make(){
  if (date==0) {date = GetDate();cout << "date = " << date << endl;}
  if (time==0) {time = GetTime();cout << "time = " << time << endl;}
  if (dvel_assumed==0.0) {dvel_assumed = 1e-6*theDb->DriftVelocity(13);
  gMessMgr->Info() << "StTpcT0Maker::Drift Velocity (East) = " << dvel_assumed << endm;} 
  if (trigger_assumed==0.0){ trigger_assumed = theDb->triggerTimeOffset()*1e6;
  gMessMgr->Info() << "StTpcT0Maker::Trig Offset  = " << trigger_assumed << endm;} 
  if (length_assumed==0.0){ length_assumed =  theDb->Dimensions()->outerEffectiveDriftDistance();
  gMessMgr->Info() << "StTpcT0Maker::TPC Length = " << length_assumed << endm;} 
//   if (dvel_assumed!=(1e-6*theDb->DriftVelocity())||trigger_assumed!=(theDb->triggerTimeOffset()*1e6)||length_assumed!=(theDb->Dimensions()->outerEffectiveDriftDistance())) {
//     gMessMgr->Error() << "StTpcT0Maker::t0 assumed has changed, calibration ceasing" << endm;
//      return kStEOF;
//   }
  t0guess = length_assumed/dvel_assumed - trigger_assumed;
  gMessMgr->Info() << "StTpcT0Maker::t0 guess = " << t0guess << " micro seconds" << endm; 

  if(!(m_Mode==0||m_Mode==1)){
    gMessMgr->Info() << "StTpcT0Maker unknown mode: " << m_Mode << endm; 
    gMessMgr->Info() << "Check mode ... exit ... " << endm;
    return kStFatal;
  }
  if (!GetMaker("tpc_hits")||!GetMaker("tpc_tracks")||!GetMaker("match")||!GetMaker("vertex")||!GetMaker("primary")||!GetMaker("dst")) {
    gMessMgr->Error() << "StTpcT0Maker::Missing Another Maker, check chain options " << endm;
    return kStFatal;
  }

  StTpcHitMover *thm = (StTpcHitMover*)GetMaker("tpc_hit_mover");
  if(!thm){
    gMessMgr->Info() << "StTpcT0Maker: StTpcHitMover is not present! ExB correction will not be applied! " << endm; 
  }

  St_tcl_Maker *tcl = 0;
  StRTSClientFCFMaker *fcf = 0;
  if(m_Mode&0x1){    
    fcf = (StRTSClientFCFMaker*)GetMaker("tpc_hits");
    fcf->InitRun(0);
    GetMaker("tpc_tracks")->InitRun(0);
    fcf->Clear();
  }
  else {
    tcl = (St_tcl_Maker*)GetMaker("tpc_hits");
    tcl->InitRun(0);
    GetMaker("tpc_tracks")->InitRun(0);
    tcl->Clear();
  }
  if(thm) thm->Clear();
  GetMaker("tpc_tracks")->Clear();
  GetMaker("match")->Clear();
  GetMaker("vertex")->Clear();
  GetMaker("primary")->Clear();
  GetMaker("dst")->Clear();
  if(m_Mode&0x1){    
    fcf->AllOn();
    fcf->EastOff();
    fcf->Make();  
  }
  else{
    tcl->AllOn();
    tcl->EastOff();
    tcl->Make();  
  }
  if(thm) thm->Make();
  GetMaker("tpc_tracks")->Make();
  //  GetMaker("match")->InitRun(0);
  GetMaker("match")->Make();
  GetMaker("vertex")->Make();
  GetMaker("primary")->Make();
  GetMaker("dst")->Make();
  TDataSet *ds=GetDataSet("dst/vertex");
  if(!ds) return kStErr;
  TDataSetIter dsiter(ds);
  St_dst_vertex *vert = (St_dst_vertex *) dsiter.Find("vertex");
  if (!vert)  return kStErr;
  dst_vertex_st *sth = vert->GetTable();
  for (int ij=0;ij<vert->GetNRows();ij++,sth++){
    if(sth->iflag==1&&sth->vtx_id==kEventVtxId){
      zVertexWest = sth->z;
      yVertexWest = sth->y;
      xVertexWest = sth->x;
      multWest = (float)(sth->n_daughters);
      break;    // found primary vertex
    }    
  }
  if(m_Mode&0x1)
    fcf->Clear();
  else 
    tcl->Clear();
  if(thm) thm->Clear();
  GetMaker("tpc_tracks")->Clear();
  GetMaker("match")->Clear();
  GetMaker("vertex")->Clear(); 
  GetMaker("primary")->Clear();
  GetMaker("dst")->Clear();

  if(m_Mode&0x1)
    fcf->AllOn();
  else
    tcl->AllOn();

  //check to see if event is OK
  if (zVertexWest<-998) {
    gMessMgr->Info() << "StTpcT0Maker::No Vertex Found in West End" << endm;
    return kStOK;
  }
  if (zVertexWest<zVertexMin||zVertexWest>zVertexMax){
    gMessMgr->Info() << "StTpcT0Maker::Vertex out of range, skip event" << endm;
    return kStOK;
  }

  if(m_Mode&0x1){
    fcf->WestOff();
    fcf->Make();  
  }
  else{
    tcl->WestOff();
    tcl->Make();  
  }
  if(thm) thm->Make();
  GetMaker("tpc_tracks")->Make();
  GetMaker("match")->Make();
  GetMaker("vertex")->Make();
  GetMaker("primary")->Make();
  GetMaker("dst")->Make();
  TDataSet *ds1=GetDataSet("dst/vertex");
  if (!ds1) return kStErr;
  TDataSetIter dsiter1(ds1);
  St_dst_vertex *vert1 = (St_dst_vertex *) dsiter1.Find("vertex");
  if (!vert1)  return kStErr;
  dst_vertex_st *sth1 = vert1->GetTable();
  for (int ij=0;ij<vert1->GetNRows();ij++,sth1++){
    if(sth1->iflag==1&&sth1->vtx_id==kEventVtxId){
      zVertexEast = sth1->z;
      yVertexEast = sth1->y;
      xVertexEast = sth1->x;
      multEast = (float)(sth1->n_daughters);
      break;    // found primary vertex
    }
  }
  if (zVertexEast<-998) {
    gMessMgr->Info() << "StTpcT0Maker::No Vertex Found in East End" << endm;
    return kStOK;
  }
  
  if (zVertexEast>-999&&zVertexWest>-999&&zVertexEast>zVertexMin&&zVertexEast<zVertexMax){
    float deltaz = zVertexEast-zVertexWest;
    float meanz = (zVertexEast+zVertexWest)/2;
    float corrz = deltaz+GetCorrection(meanz);
    t0current = (corrz)/(2*dvel_assumed) + t0guess;
    gMessMgr->Info() << "StTpcT0Maker::zVertexWest = " << zVertexWest << endm;
    gMessMgr->Info() << "StTpcT0Maker::zVertexEast = " << zVertexEast << endm;
    gMessMgr->Info() << "StTpcT0Maker::t0 = " << t0current << endm;
    t0result->Fill(t0current);
    t0guessError->Fill(t0current-t0guess);
    xVertexDiff->Fill(xVertexEast-xVertexWest);
    yVertexDiff->Fill(yVertexEast-yVertexWest);
    zVertexDiff->Fill(zVertexEast-zVertexWest);
    eventNumber = (float)GetEventNumber();
    resNtuple->Fill(eventNumber,xVertexEast,yVertexEast,zVertexEast,xVertexWest,yVertexWest,zVertexWest,multEast,multWest,corrz);
    if (t0current<T0HIST_MIN||t0current>T0HIST_MAX){
      gMessMgr->Info() << "StTpcT0Maker::t0 out of defined range for histogram"<< endm;
    }
  }

  if(m_Mode&0x1)
    fcf->Clear();
  else
    tcl->Clear();  
  if(thm) thm->Clear();
  GetMaker("tpc_tracks")->Clear();
  GetMaker("match")->Clear();
  GetMaker("vertex")->Clear(); 
  GetMaker("primary")->Clear();
  GetMaker("dst")->Clear();

  if(m_Mode&0x1)
    fcf->AllOn();
  else
    tcl->AllOn();
  
  if (t0result->GetEntries()>=desiredEntries){
    gMessMgr->Info() << "StTpcT0Maker::Sufficient Statistics, Ending Chain" << endm;
//   if (t0result->GetEntries()>minEntries){
//     if (t0result->GetRMS()<maxRMS){
//      WriteTableToFile();
//     }
//     else{
//       gMessMgr->Error() << "StTpcT0Maker::t0 unstable. RMS = " << t0result->GetRMS() << " micro-seconds. No table will be written" << endm;
//     }
//   }
//   else {
//    gMessMgr->Error() << "StTpcT0Maker::Insufficient statistics for velocity determination.  Only " << t0result->GetEntries() << " entries. No table will be written" << endm;
//   }
//   minEntries = 9999;
    return kStEOF;
  }
  return kStOK;
}


Int_t StTpcT0Maker::Finish() {
  if (t0result->GetEntries()>=minEntries){
    if (t0result->GetRMS()<maxRMS){
     WriteTableToFile();
    }
    else{
      gMessMgr->Error() << "StTpcT0Maker::t0 unstable. RMS = " << t0result->GetRMS() << " micro-seconds. No table will be written" << endm;
    }
  }
  else {
   gMessMgr->Error() << "StTpcT0Maker::Insufficient statistics for velocity determination.  Only " << t0result->GetEntries() << " entries. No table will be written" << endm;
  }
  if (mHistOut){
    WriteHistFile();
  }  
    return StMaker::Finish();
}

void StTpcT0Maker::PrintInfo() {
  LOG_INFO << "**************************************************************"<< endm;
  LOG_INFO << "* $Id: StTpcT0Maker.cxx,v 1.14 2009/01/26 14:49:53 fisyak Exp $"<< endm;
  LOG_INFO << "**************************************************************"<< endm;

  if (Debug()) StMaker::PrintInfo();
}

void StTpcT0Maker::WriteTableToFile(){
  char filename[80]; 
  sprintf(filename,"./StarDb/Calibrations/tpc/tpcDriftVelocity.%08d.%06d.C",date,time);
  TString dirname = gSystem->DirName(filename);
  if (gSystem->OpenDirectory(dirname.Data())==0) { 
    if (gSystem->mkdir(dirname.Data())) {
      cout << "Directory " << dirname << " creation failed" << endl;
      cout << "Putting tpcDriftVelocity.C in current directory" << endl;
      for (int i=0;i<80;i++){filename[i]=0;}
      sprintf(filename,"tpcDriftVelocity.%08d.%06d.C",date,time);
    }
  }
  ofstream *out = new ofstream(filename);
  driftTable()->SavePrimitive(*out,"");
  return;
}

 St_tpcDriftVelocity* StTpcT0Maker::driftTable(){
   float velocity = length_assumed/(AverageT0() + trigger_assumed);
  St_tpcDriftVelocity* table = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st* row = table->GetTable();
  row->cathodeDriftVelocityEast = velocity;
  row->cathodeDriftVelocityWest = velocity;
  row->laserDriftVelocityEast = 0.0;
  row->laserDriftVelocityWest = 0.0;
  table->SetNRows(1);
  return table;
 }

void StTpcT0Maker::WriteHistFile(){
#if JECC_DEBUG
  char filename[80]; 

  sprintf(filename,"t0hist.%08d.%06d.root",date,time);
  TFile out(filename,"RECREATE");
#endif
  GetHistList()->Write();
  resNtuple->Write();
#if JECC_DEBUG
  out.Close();
#endif
}

float StTpcT0Maker::AverageT0(){float mean = t0result->GetMean();return mean;}
void StTpcT0Maker::SetMinEntries(int entries){minEntries = entries; }
void StTpcT0Maker::SetDesiredEntries(int entries){desiredEntries = entries;
if (minEntries>desiredEntries) minEntries = desiredEntries; }
void StTpcT0Maker::SetMaxRMS(float rms){maxRMS = rms;}
int  StTpcT0Maker::GetValidityDate(){return date;}
int  StTpcT0Maker::GetValidityTime(){return time;}
void StTpcT0Maker::HistFileByDefault(){mHistOut = kTRUE;} 
void StTpcT0Maker::SetVertexZmax(float zmax){zVertexMax = zmax;}
void StTpcT0Maker::SetVertexZmin(float zmin){zVertexMin = zmin;}
void StTpcT0Maker::SetCorrectionFactors(float constant, float linear, float quadratic){CorrFac[0]=constant;CorrFac[1]=linear;CorrFac[2]=quadratic;}  
float StTpcT0Maker::GetCorrection(float z){
  float factor = -1*(CorrFac[0]+CorrFac[1]*z+CorrFac[2]*z*z);
  return factor;
}
