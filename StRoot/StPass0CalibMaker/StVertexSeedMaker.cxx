//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StVertexSeedMaker class                                              //
// Author: G. Van Buren, BNL                                            //
// Description: calculates mean primary vertex positions from           //
//              suitable events to use as seeds in finding better       //
//              primary vertex positions (helpful for low               //
//              multiplicity events like pp collisions)                 //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "Stiostream.h"
#include "StVertexSeedMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StDAQMaker/StDAQReader.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StDbLib/StDbManager.hh"
#include "StDbLib/StDbConfigNode.hh"
#include "StDbLib/StDataBaseI.hh"
#include "StDetectorDbMaker/StDetectorDbTriggerID.h"
#include "St_tcl_Maker/St_tcl_Maker.h"
#include "tables/St_dst_vertex_Table.h"
#include "tables/St_dst_L0_Trigger_Table.h"
#include "StMessMgr.h"
#include "StVertexId.h"
#include "tables/St_vertexSeed_Table.h"
#include "tables/St_beamInfo_Table.h"
#include "TSystem.h"
#include "TFile.h"
#include "TVirtualFitter.h"
#include "TNtuple.h"
#include "TArrayF.h"
#include "StTree.h"

const char* defDir = "./StarDb/Calibrations/rhic";

//_____________________________________________________________________________
// C variables and functions for fit/minimization
//_____________________________________________________________________________
static TArrayF xVert, yVert, zVert, mult;
int nverts,nsize;
Double_t funcX(float z,Double_t *par) {
  Double_t x = par[0] + par[1]*z;
  return x;
}
Double_t funcY(float z,Double_t *par) {
  Double_t y = par[2] + par[3]*z;
  return y;
}
void fnch(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag) {
  //calculate chisquare
  double chisq = 0;
  double delta_sq;
  double error_sq;
  for (int i=0;i<nverts; i++) {
    // Error1 set such that 5 tracks => ~7mm, proportional to 1/::sqrt(mult)
    // This was determined by trying to get the chisq/dof distribution
    // to peak near 1.0
    //                              =>  Error1   = (0.7cm) / ::sqrt(mult/5)
    //                              =>  Error1^2 = (2.45) cm^2 / mult
    // Beam spot size (sigma) Error2 ~= 0.04 cm (400 microns)
    //                              =>  Error2^2 = 0.0016 cm^2
    // The total error should be ::sqrt(Error1^2 + Error2^2)
    error_sq = 0.0016 + (2.45 / mult[i]); 
    delta_sq = ::pow(xVert[i]-funcX(zVert[i],par),2) +
               ::pow(yVert[i]-funcY(zVert[i],par),2);
    chisq += (delta_sq/error_sq);
  }
  f = chisq;
}
void setArraySize(int n) {
  xVert.Set(n);
  yVert.Set(n);
  zVert.Set(n);
  mult.Set(n);
  nsize = n;
}
void addVert(float x, float y, float z, float m) {
  if (nverts >= nsize) setArraySize(nsize*2);
  xVert[nverts] = x;
  yVert[nverts] = y;
  zVert[nverts] = z;
   mult[nverts] = m;
  nverts++;
}
//_____________________________________________________________________________


ClassImp(StVertexSeedMaker)
//_____________________________________________________________________________
StVertexSeedMaker::StVertexSeedMaker(const char *name):StMaker(name){
  xdist = new TH1F("xdist","xdist",1000,HIST_MIN,HIST_MAX);
  ydist = new TH1F("ydist","ydist",1000,HIST_MIN,HIST_MAX);
  xerr  = new TH1F("xerr","x measured - x guess",1000,HIST_MIN,HIST_MAX);
  yerr  = new TH1F("yerr","y measured - y guess",1000,HIST_MIN,HIST_MAX);
  resNtuple = 0;
  nsize = 0;
  setArraySize(512);
  UseEventDateTime(); // By default, use the data & time from the first event
  Reset();
}
//_____________________________________________________________________________
void StVertexSeedMaker::Reset() {
  minEntries = 100;   //require 100 valid verts for a seed determination
  maxX0Err     = 0.05; //vertex x should be good to 500 microns
  maxY0Err     = 0.05; //vertex y should be good to 500 microns
  mHistOut=kTRUE;
  zVertexMax = 100.0;
  zVertexMin = -100.0;
  r2VertexMax = 15.0;
  nverts = 0;
  xguess = 0;
  yguess = 0;
  zvertex = -999.0;
  HIST_MIN = -3.0;
  HIST_MAX =  3.0;
  xdist->Reset();
  ydist->Reset();
  xerr->Reset();
  yerr->Reset();
  if (resNtuple) delete resNtuple;
  resNtuple = new TNtuple("resNtuple","resNtuple","event:x:y:z:mult");
  date = 0;
  time = 0;
  fill = -1;
  a[0]   = -888.0;
  //a[0]   = 0.0;
  a[1] = 0.0;
  a[2]   = 0.0;
  a[3] = 0.0;
  chi = 0.0;
  weight = 0.0;
}
//_____________________________________________________________________________
StVertexSeedMaker::~StVertexSeedMaker(){
}
//_____________________________________________________________________________
Int_t StVertexSeedMaker::Init(){
  AddHist(xdist);
  AddHist(ydist);
  AddHist(xerr);
  AddHist(yerr);
  return StMaker::Init();
}
//_____________________________________________________________________________
void StVertexSeedMaker::Clear(Option_t *option){
  xguess = 0;
  yguess = 0;
  zvertex = -999.0;
  yvertex = -999.0; 
  xvertex = -999.0; 
}
//_____________________________________________________________________________
Int_t StVertexSeedMaker::Make(){
  if (date==0) FillDateTime();

  // Currently only finds database values for first event
  if (a[0] == -888) {
    gMessMgr->Info("StVertexSeedMaker: Reading db values at the start...");
    Int_t status = FillAssumed();
    if (status != kStOk) return status;
  }


  // Check trigger word
  StDetectorDbTriggerID* dbTriggerId = StDetectorDbTriggerID::instance();
  St_DataSet *daqReaderSet = GetDataSet("StDAQReader");
  if (!daqReaderSet) {
    gMessMgr->Error("StVertexSeedMaker: No StDAQReader!");
    return kStErr;
  }
  StTrigSummary* trigSummary =
    ((StDAQReader*) (daqReaderSet->GetObject()))->getTrigSummary();
  UInt_t summary = 0;
  switch (dbTriggerId->getDefaultTriggerLevel()) {
    case (1) : { summary=trigSummary->L1summary[0]; break; }
    case (2) : { summary=trigSummary->L2summary[0]; break; }
    case (3) : { summary=trigSummary->L3summary[0]; break; }
    default  : {}
  }
  Bool_t notTrig = kTRUE;
  for (unsigned int iTrg = 0;
       (notTrig) && (iTrg < dbTriggerId->getIDNumRows()) ; iTrg++) {
    if (summary & (1 << (dbTriggerId->getDaqTrgId(iTrg)))) {
      switch (dbTriggerId->getOfflineTrgId(iTrg)) {
        case (1000) :     // ppMinBias
        case (1003) :     // ppFPDe-slow
        case (1009) :     // ppFPDw-slow
        case (2001) :     // dAuMinBias
        case (2003) :     // dAuMinBias
        case (2300) :     // dAuTOF

// 2004 data
        case (45201) :    // pp bht-1-slow
        case (45202) :    // pp bht-2-slow
        case (45203) :    // pp eht-1-slow
        case (45204) :    // pp eht-2-slow
        case (45010) :    // pp minbias
        case (1)     :    // pp bht-1-slow
        case (2)     :    // pp bht-2-slow
        case (3)     :    // pp eht-1-slow
        case (4)     :    // pp eht-2-slow
        case (10)    :    // pp minbias
                      { notTrig = kFALSE; }
        default     : {}
      }
    }
  }
  if (notTrig) {
    gMessMgr->Info("StVertexSeedMaker: event does not satisfy triggers");
    return kStOk;
  }
  


  // Get primary vertex from evr
  TDataSet *ds=GetDataSet("dst/vertex");
  if (!ds) {
    gMessMgr->Error("StVertexSeedMaker: Cannot locate vertex dataset!");
    return kStErr;
  }
  TDataSetIter dsiter(ds);
  St_dst_vertex *vert = (St_dst_vertex *) dsiter.Find("vertex");
  if (!vert) {
    gMessMgr->Error("StVertexSeedMaker: Cannot locate vertex table!");
    return kStErr;
  }
  dst_vertex_st *sth = vert->GetTable();
  for (int ij=0;ij<vert->GetNRows();ij++,sth++){
    if ((sth->iflag==1) && (sth->vtx_id==kEventVtxId)){
      zvertex = sth->z;
      yvertex = sth->y;
      xvertex = sth->x;
      mult = (float)(sth->n_daughters);
      break;    // found primary vertex
    }
  }
  if (zvertex<-998) {
    gMessMgr->Info("StVertexSeedMaker: No primary vertex");
    return kStOK;
  }

  // Calculate guessed vertex x & y from assumed params and measured z
  xguess = a[0] + (a[1] * zvertex);
  yguess = a[2] + (a[3] * zvertex);
  gMessMgr->Info() << "StVertexSeedMaker::x guess = " << xguess << endm; 
  gMessMgr->Info() << "StVertexSeedMaker::y guess = " << yguess << endm; 

  // Check to see if vertex is good for use in the fit
  float r2vertex = ::pow(xvertex,2) + ::pow(yvertex,2);
  if ((zvertex > zVertexMin) && (zvertex < zVertexMax) &&
      (mult >= 5) && (r2vertex < r2VertexMax)){
    xdist->Fill(xvertex);
    xerr ->Fill(xvertex-xguess);
    ydist->Fill(yvertex);
    yerr ->Fill(yvertex-yguess);
    eventNumber = (float)GetEventNumber();
    resNtuple->Fill(eventNumber,xvertex,yvertex,zvertex,mult);
    addVert(xvertex,yvertex,zvertex,mult);
    weight += mult; // Fixed at 50
  }

  return kStOK;
}
//_____________________________________________________________________________
Int_t StVertexSeedMaker::Finish() {
  FindResult();
  return StMaker::Finish();
}
//_____________________________________________________________________________
void StVertexSeedMaker::FindResult(Bool_t checkDb) {
  Bool_t writeIt = kFALSE;
  if (nverts >= minEntries){
    FitData();
    if (ep[0] > maxX0Err){
      gMessMgr->Error() << "StVertexSeedMaker::x unstable. x0 error = " <<
        ep[0] << " cm." << endm;
    }
    if (ep[2] > maxY0Err){
      gMessMgr->Error() << "StVertexSeedMaker::y unstable. y0 error = " <<
        ep[2] << " cm." << endm;
    }
    if ((ep[0] <= maxX0Err) && (ep[2] <= maxY0Err)) {
      if (checkDb) {
        // Do comparison of this data with data from database to see if
        // values have changed or improved.
        gMessMgr->Info("StVertexSeedMaker: Reading db values at the end...");
        Int_t status = FillAssumed();
        if (status == kStOk) {
          if (ChangedValues() || BetterErrors()) writeIt = kTRUE;
          else gMessMgr->Info("StVertexSeedMaker: Values have not changed/improved.");
        } else {
          gMessMgr->Warning("StVertexSeedMaker: Could not get db values.");
          writeIt = kTRUE;
        }
      } else {
        writeIt = kTRUE;
      }
    }
  } else {
    gMessMgr->Error() << "StVertexSeedMaker: Insufficient stats for " <<
     "mean vertex determination.\n  Only " << nverts << " entries." << endm;
  }

  if (writeIt) WriteTableToFile();
  else gMessMgr->Warning("StVertexSeedMaker: Not writing table!!!!!");

  if (mHistOut) WriteHistFile();
}
//_____________________________________________________________________________
void StVertexSeedMaker::PrintInfo() {
  printf("**************************************************************\n");
  printf("* $Id: StVertexSeedMaker.cxx,v 1.20 2004/07/23 16:56:01 genevb Exp $\n");
  printf("**************************************************************\n");

  if (Debug()) StMaker::PrintInfo();
}
//_____________________________________________________________________________
void StVertexSeedMaker::WriteTableToFile(){
  char filename[80]; 
  sprintf(filename,"%s/vertexSeed.%08d.%06d.C",defDir,date,time);
  gMessMgr->Info() << "StVertexSeedMaker: Writing new table to:\n  "
    << filename << endm;
  TString dirname = gSystem->DirName(filename);
  if (gSystem->OpenDirectory(dirname.Data())==0) { 
    if (gSystem->mkdir(dirname.Data())) {
      gMessMgr->Warning() << "Directory creation failed for:\n  " << dirname
      << "\n  Putting table file in current directory" << endm;
      for (int i=0;i<80;i++){filename[i]=0;}
      sprintf(filename,"vertexSeed.%08d.%06d.C",date,time);
    }
  }
  ofstream *out = new ofstream(filename);
  VertexSeedTable()->SavePrimitive(*out,"");
  return;
}
//_____________________________________________________________________________
St_vertexSeed* StVertexSeedMaker::VertexSeedTable(){
  St_vertexSeed* table = new St_vertexSeed("vertexSeed",1);
  vertexSeed_st* row = table->GetTable();
  row->x0 = p[0];
  row->dxdz = p[1];
  row->y0 = p[2];
  row->dydz = p[3];
  row->err_x0 = ep[0];
  row->err_dxdz = ep[1];
  row->err_y0 = ep[2];
  row->err_dydz = ep[3];
  row->chisq_dof = chi;
  row->weight = 100.0; // Fixed for now!!!
  row->stats = (float) nverts;
  table->SetNRows(1);
  return table;
}
//_____________________________________________________________________________
void StVertexSeedMaker::WriteHistFile(){
  char filename[80]; 
  // .ROOT is NOT a typo !!!
  sprintf(filename,"%s/vertexseedhist.%08d.%06d.ROOT",defDir,date,time);
  gMessMgr->Info() << "StVertexSeedMaker: Writing new histograms to:\n  "
    << filename << endm;
  TString dirname = gSystem->DirName(filename);
  if (gSystem->OpenDirectory(dirname.Data())==0) { 
    if (gSystem->mkdir(dirname.Data())) {
      gMessMgr->Warning() << "Directory creation failed for:\n  " << dirname
      << "\n  Putting histogram file in current directory" << endm;
      for (int i=0;i<80;i++){filename[i]=0;}
      sprintf(filename,"vertexseedhist.%08d.%06d.ROOT",date,time);
    }
  }
  TFile out(filename,"RECREATE");
  GetHistList()->Write();
  resNtuple->Write();
  out.Close();
}
//_____________________________________________________________________________
Int_t StVertexSeedMaker::FillAssumed(){
  TDataSet* dbDataSet = GetDataBase("Calibrations/rhic");
  if (!dbDataSet) {
    gMessMgr->Error("StVertexSeedMaker: Could not find Calibrations/rhic database");
    return kStErr;
  }
  St_vertexSeed* dbTableC =
    (St_vertexSeed*) (dbDataSet->FindObject("vertexSeed"));
  if (!dbTableC) {
    gMessMgr->Error("StVertexSeedMaker: Could not find vertexSeed in database");
    return kStErr;
  }
  vertexSeed_st* dbTable = dbTableC->GetTable();
  a[0] = dbTable->x0;
  a[1] = dbTable->dxdz;
  a[2] = dbTable->y0;
  a[3] = dbTable->dydz;
  ea[0] = dbTable->err_x0;
  ea[1] = dbTable->err_dxdz;
  ea[2] = dbTable->err_y0;
  ea[3] = dbTable->err_dydz;
  gMessMgr->Info() << "StVertexSeedMaker: Assumed values:"
    << "\n     x0 assumed = " << a[0] << " +/- " << ea[0]
    << "\n   dxdz assumed = " << a[1] << " +/- " << ea[1]
    << "\n     y0 assumed = " << a[2] << " +/- " << ea[2]
    << "\n   dydz assumed = " << a[3] << " +/- " << ea[3]
    << endm;
  return kStOk;
}
//_____________________________________________________________________________
Bool_t StVertexSeedMaker::BetterErrors(){
  Bool_t better = kFALSE;
  if ((ep[0] < ea[0]) || (ep[1] < ea[1]) ||
      (ep[2] < ea[2]) || (ep[3] < ea[3])) better = kTRUE;
  if (better) gMessMgr->Info("StVertexSeedMaker: Values have improved");
  return better;
}
//_____________________________________________________________________________
Bool_t StVertexSeedMaker::ChangedValues(){
  Bool_t changed = kFALSE;
  for (int i = 0; i<4; i++) {
    double diff = TMath::Abs(p[i] - a[i]);
    if ((diff >= ep[i]) || (diff >= ea[i])) changed = kTRUE;
  }
  if (changed) gMessMgr->Info("StVertexSeedMaker: Values have changed");
  return changed;
}
//_____________________________________________________________________________
void StVertexSeedMaker::FillDateTime() {
  date = GetDate();
  time = GetTime();
  gMessMgr->Info() << "StVertexSeedMaker: event date = " << date << endm;
  gMessMgr->Info() << "StVertexSeedMaker: event time = " << time << endm;
  if (!useEventDateTime) GetFillDateTime();
}
//_____________________________________________________________________________
void StVertexSeedMaker::GetFillDateTime() {

  StDbManager* mgr=StDbManager::Instance();
  StDbConfigNode* node=mgr->initConfig("RunLog_onl");
  StDbTable* tab=node->addDbTable("beamInfo");
  StDataBaseI* db=mgr->findDb("RunLog_onl");
  unsigned int ts;
  char queryStr[128];

  // Find beamInfo entry for this date time
  sprintf(queryStr,"%08d %06d",date,time);
  TString tdStr = queryStr;
  tdStr.Insert(4,'-').Insert(7,'-').Insert(13,':').Insert(16,':');
  const char* tdstr = tdStr.Data();
  sprintf(queryStr,
    " where beginTime<='%s' and deactive=0 order by beginTime desc limit 1",
    tdstr);
  ts = db->QueryDb(tab,queryStr);

  if (ts) {
    // Find earliest entry for this fill
    int thisRun = *(int*) (tab->getDataValue("runNumber",0));
    gMessMgr->Info() << "StVertexSeedMaker: " << tdstr << " is from run "
      << thisRun << endm;
    float thisFill = *(float*) (tab->getDataValue("blueFillNumber",0));
    sprintf(queryStr,
      " where blueFillNumber=%f and deactive=0 order by beginTime asc limit 1",
      thisFill);
    ts = db->QueryDb(tab,queryStr);

    // Extract date and time at start of fill
    char* start = tab->getBeginDateTime();
    fill = (int) thisFill;
    time = atoi(&(start[8]));
    start[8] = 0;
    date = atoi(start);

    gMessMgr->Info() << "StVertexSeedMaker: Using fill no.  = " << fill << endm;
    gMessMgr->Info() << "StVertexSeedMaker: Using fill date = " << date << endm;
    gMessMgr->Info() << "StVertexSeedMaker: Using fill time = " << time << endm;
  } else {
    gMessMgr->Warning() <<
      "StVertexSeedMaker: Could not find beamInfo in database\n" <<
      "  Using event date/time." << endm;
    UseEventDateTime();
  }
}
//_____________________________________________________________________________
void StVertexSeedMaker::FitData() {
   gMessMgr->Info() << "StVertexSeedMaker: Now fitting the data..." <<
     "\n  *****************************************************" << endm;
   TVirtualFitter *minuit = TVirtualFitter::Fitter(0,4);
   minuit->SetFCN(fnch);

// Set starting values and step sizes for parameters
   static Double_t pstart[4] = {0., 0., 0., 0.};
   static Double_t pstep[4]  = {0.0001, 0.0000001, 0.0001, 0.0000001};
   static Double_t plow[4]   = {-3., -0.05, -3., -0.05};
   static Double_t phigh[4]  = { 3.,  0.05,  3.,  0.05};
   minuit->SetParameter(0, "x0"  , pstart[0], pstep[0], plow[0], phigh[0]);
   minuit->SetParameter(1, "dxdz", pstart[1], pstep[1], plow[1], phigh[1]);
   minuit->SetParameter(2, "y0"  , pstart[2], pstep[2], plow[2], phigh[2]);
   minuit->SetParameter(3, "dydz", pstart[3], pstep[3], plow[3], phigh[3]);

// Now ready for minimization step
   double arglist[10];
   arglist[0] = 500;
   int status = minuit->ExecuteCommand("MIGRAD", arglist ,1);
   if (status) {
     gMessMgr->Error() << "StVertexMaker: error on migrad call, err = "
       << status << endm;
     return;
   }
   
   double amin,edm,errdef;
   int nvpar,nparx;
   minuit->GetStats(amin,edm,errdef,nvpar,nparx);
   chi = amin/((double) (nverts-4));
   gMessMgr->Info() << "StVertexSeedMaker: chisq = " << amin
     << ", chisq/dof = " << chi <<
     "\n  *****************************************************" << endm;
   
   char pname[10];
   for (int i=0; i<4; i++)
     minuit->GetParameter(i, pname, p[i], ep[i], plow[i], phigh[i]);
}
//_____________________________________________________________________________
Int_t StVertexSeedMaker::Aggregate(Char_t* dir) {
  // Format of filenames for parsing must be:
  // vertexseedhist.DDDDDDDD.TTTTTT.root
  // where D and T are 8 and 6 digit representations of date and time

  const char* defaultDir = "./";
  TString dirStr = dir;
  if (!dir) dirStr = defaultDir;
  if (dirStr.EndsWith("/")) dirStr.Append("vertexseedhist.*.root");
  StFile allFiles;
  allFiles.AddFile(dirStr.Data());
  // allFiles.ls(); allFiles.Rewind();
  TList fileList;
  const char* fileName;
  for (int fileb = 0; fileb < allFiles.GetNBundles(); fileb++) {
    allFiles.GetNextBundle();
    int filen=0;
    while ((fileName = allFiles.GetFileName(filen++))) {
      fileList.Add(new TNamed(fileName,fileName));
    }
  }
  fileList.Sort();

  TFile* currentFile=0;
  float* vals=0;
  int nfiles = fileList.GetSize();
  for (int filen = 0; filen < nfiles; filen++) {
    int fillf = fill;
    int datef = date;
    int timef = time;
    fileName = fileList.At(filen)->GetName();
    TString dateTime = fileName;
    dateTime.Remove(0,dateTime.First('.') + 1).Remove(15);
    TString dateStr = dateTime;
    date = atoi(dateStr.Remove(8).Data());
    time = atoi(dateTime.Remove(0,9).Remove(6).Data());
    GetFillDateTime();
    if ((currentFile) && (fill != fillf)) {
      gMessMgr->Info() << "StVertexSeedMaker: Fill number has changed\n"
        << "  Processing data from previous fill before continuing" << endm;
      int fillp = fill;
      int datep = date;
      int timep = time;
      fill = fillf;
      date = datef;
      time = timef;
      currentFile->Close();
      FindResult(kFALSE);
      Reset();
      fill = fillp;
      date = datep;
      time = timep;
    }

    gMessMgr->Info() << "StVertexSeedMaker: Now opening file:\n  "
      << fileName << endm;
    currentFile = new TFile(fileName);
    TNtuple* curNtuple = (TNtuple*) currentFile->Get("resNtuple");
    Int_t nentries = (Int_t) curNtuple->GetEntries();
    for (Int_t entryn = 0; entryn < nentries; entryn++) {
      curNtuple->GetEvent(entryn);
      vals = curNtuple->GetArgs();
      resNtuple->Fill(vals);
      addVert(vals[1],vals[2],vals[3],vals[4]);
      weight += vals[4];
    }
    gMessMgr->Info() << "StVertexSeedMaker: Current statistics: "
      << nverts << endm;
  }
  if (currentFile) {
    currentFile->Close();
    FindResult(kFALSE);
  }
  gMessMgr->Info() << "StVertexSeedMaker: Examined "
    << nfiles << " files" << endm;
  return nfiles;
}
//_____________________________________________________________________________
// $Id: StVertexSeedMaker.cxx,v 1.20 2004/07/23 16:56:01 genevb Exp $
// $Log: StVertexSeedMaker.cxx,v $
// Revision 1.20  2004/07/23 16:56:01  genevb
// 2004 pp triggers
//
// Revision 1.19  2003/09/02 17:58:45  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.18  2003/05/30 19:02:51  genevb
// Add ppFPDw-slow
//
// Revision 1.17  2003/05/19 14:34:54  genevb
// Add ppFPD slow triggers
//
// Revision 1.16  2003/04/22 19:20:52  genevb
// Add ppMinBias triggers
//
// Revision 1.15  2003/03/21 15:12:24  genevb
// Allow use of TOF triggers
//
// Revision 1.14  2003/02/25 03:49:16  genevb
// Choose only dAu minbias triggers
//
// Revision 1.13  2003/02/12 04:19:39  genevb
// Small improvements
//
// Revision 1.12  2003/02/11 22:24:20  genevb
// Update to use beamInfo table from database
//
// Revision 1.11  2003/01/22 23:32:23  genevb
// Type cast fix
//
// Revision 1.10  2002/04/19 22:24:16  perev
// fixes for ROOT/3.02.07
//
// Revision 1.9  2002/03/23 01:05:15  jeromel
// Create files with extension .ROOT instead of .root (db_Maker will read the .root
// and crash otherwise). Will fix this with a more elegant scheme later.
//
// Revision 1.8  2002/03/23 00:23:54  genevb
// switch from float arrays to TArrayF
//
// Revision 1.7  2002/03/22 20:29:09  jeromel
// Not .C but .root
//
// Revision 1.6  2002/03/20 21:09:10  genevb
// Oops...introduced a typo last time
//
// Revision 1.5  2002/03/20 19:22:59  genevb
// changed output directory of histogram files
//
// Revision 1.4  2002/03/20 00:40:42  genevb
// Addition of Aggregate feature, minor updates
//
// Revision 1.3  2002/01/28 22:16:33  genevb
// Some revisions to output date/time stamps
//
// Revision 1.2  2002/01/27 01:56:14  genevb
// Write db files with date/time for fill
//
// Revision 1.1  2002/01/26 18:55:33  jeromel
// StTpcT0Maker moved from directory of the same name. First version
// of StVertexSeedMaker.
//
//
