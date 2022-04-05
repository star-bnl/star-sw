//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StVertexSeedMaker class                                              //
// Author: G. Van Buren, BNL                                            //
// Description: Calculates mean primary vertex positions from           //
//              suitable events to use as seeds in finding better       //
//              primary vertex positions (helpful for low               //
//              multiplicity events like pp collisions).                //
//              See the header file for more information.               //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "Stiostream.h"
#include "StVertexSeedMaker.h"
#include "StDbLib/StDbManager.hh"
#include "StDbLib/StDbConfigNode.hh"
#include "StDbLib/StDataBaseI.hh"
#include "StDetectorDbMaker/StDetectorDbTriggerID.h"
#include "StMessMgr.h"
#include "StVertexId.h"
#include "tables/St_vertexSeed_Table.h"
#include "tables/St_vertexSeedTriggers_Table.h"
#include "tables/St_beamInfo_Table.h"
#include "TSystem.h"
#include "TFile.h"
#include "TVirtualFitter.h"
#include "TNtuple.h"
#include "TNtupleD.h"
#include "TTimeStamp.h"
#include "TEventList.h"
#include "TArrayF.h"
#include "StTree.h"
#include "TMath.h"
//_____________________________________________________________________________
// C variables and functions for fit/minimization
//_____________________________________________________________________________
static TArrayF xVert, yVert, zVert, multA, exVert, eyVert;
int nverts,nsize;
double beamWidth;
double funcX(float z,Double_t *par) {
  double x = par[0] + par[1]*z;
  return x;
}
double funcY(float z,Double_t *par) {
  double y = par[2] + par[3]*z;
  return y;
}
void fnch(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag) {
  //calculate chisquare
  double chisq = 0;
  for (int i=0;i<nverts; i++) {
    if (exVert[i]==0) {

      double delta_sq, error_sq;
      // Error1 set such that 5 tracks => ~7mm, proportional to 1/::sqrt(mult)
      // This was determined by trying to get the chisq/dof distribution
      // to peak near 1.0
      //                              =>  Error1   = (0.7cm) / ::sqrt(mult/5)
      //                              =>  Error1^2 = (2.45) cm^2 / mult
      // Beam spot size (sigma) Error2 ~= 0.04 cm (400 microns)
      //                              =>  Error2^2 = 0.0016 cm^2
      // The total error should be ::sqrt(Error1^2 + Error2^2)
      error_sq = 0.0016 + (2.45 / multA[i]); 
      delta_sq = TMath::Power(xVert[i]-funcX(zVert[i],par),2) +
                 TMath::Power(yVert[i]-funcY(zVert[i],par),2);
      chisq += (delta_sq/error_sq);

    } else {

      //chisq += TMath::Power((xVert[i]-funcX(zVert[i],par))/exVert[i],2) +
      //         TMath::Power((yVert[i]-funcY(zVert[i],par))/eyVert[i],2);

      double errx2 = exVert[i]*exVert[i] + beamWidth*beamWidth;
      double erry2 = eyVert[i]*eyVert[i] + beamWidth*beamWidth;
      chisq += TMath::Power(xVert[i]-funcX(zVert[i],par),2)/errx2 +
               TMath::Power(yVert[i]-funcY(zVert[i],par),2)/erry2;

    }
  }
  f = chisq;
}
void setArraySize(int n) {
  xVert.Set(n);
  yVert.Set(n);
  zVert.Set(n);
  multA.Set(n);
  exVert.Set(n);
  eyVert.Set(n);
  nsize = n;
}
void addVert(float x, float y, float z, float m, float ex, float ey) {
  if (nverts >= nsize) setArraySize(nsize*2);
  xVert[nverts] = x;
  yVert[nverts] = y;
  zVert[nverts] = z;
  multA[nverts] = m;
  exVert[nverts] = ex;
  eyVert[nverts] = ey;
  nverts++;
}
//_____________________________________________________________________________


ClassImp(StVertexSeedMaker)
//_____________________________________________________________________________
StVertexSeedMaker::StVertexSeedMaker(const char *name,
    const char* defaultDir):StMaker(name){
  // items which should be initialized only once
  xdist = new TH1F("xdist","xdist",1000,HIST_MIN,HIST_MAX);
  ydist = new TH1F("ydist","ydist",1000,HIST_MIN,HIST_MAX);
  xerr  = new TH1F("xerr","x measured - x guess",1000,HIST_MIN,HIST_MAX);
  yerr  = new TH1F("yerr","y measured - y guess",1000,HIST_MIN,HIST_MAX);
  defDir = defaultDir;
  mTempOut = 0;
  resNtuple = 0;
  parsNtuple = 0;
  nsize = 0;
  foffset = 0;
  noclobber = kTRUE;
  setArraySize(512);
  UseEventDateTime(); // By default, use the data & time from the first event
  useAllTriggers = kFALSE;
  dbTriggersTable = 0;
  minEntries = 100;    //require 100 valid verts for a seed determination
  maxX0Err     = 0.05; //vertex x should be good to 500 microns
  maxY0Err     = 0.05; //vertex y should be good to 500 microns
  mHistOut=kTRUE;
  zVertexMax = 100.0;
  zVertexMin = -100.0;
  r2VertexMax = 15.0;
  HIST_MIN = -3.0;
  HIST_MAX =  3.0;
  Reset();
}
//_____________________________________________________________________________
void StVertexSeedMaker::Reset() {
  // items which should be initialized for every new calibration measurement
  Clear("");
  xdist->Reset();
  ydist->Reset();
  xerr->Reset();
  yerr->Reset();

  if (mTempOut) delete mTempOut;
  else delete resNtuple;
  mTempOut = new TFile(Form("%s/vertexseedhist.%d.root",
    gSystem->TempDirectory(),
    gSystem->GetPid()),"RECREATE");
  resNtuple = new TNtuple("resNtuple","resNtuple","event:x:y:z:mult:trig:run:fill:zdc:rank:itpc:otpc:detmap:ex:ey:index:bmatch:ematch:tmatch:cmatch:hmatch:pmatch:pct:vpdz:tDay:tFill");
  LOG_INFO << "Opening new temp file at " << mTempOut->GetName() << endm;

  date = 0;
  time = 0;
  nverts = 0;
  sumzdc = 0;

  a[0]   = -888.0;
  //a[0]   = 0.0;
  a[1] = 0.0;
  a[2]   = 0.0;
  a[3] = 0.0;
  chi = 0.0;
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
  // items which should be initialized for every event
  fill = -1;
  run = -1;
  zdc = -1;
  xguess = 0;
  yguess = 0;
  zvertex = -999.0;
  yvertex = -999.0; 
  xvertex = -999.0; 
  exvertex = 0;
  eyvertex = 0;
  vpd_zvertex = -999.0;
  rank = 0;
  pvn = 0;
  itpc = 0;
  otpc = 0;
  detmap = 0;
  bmatch = 0;
  ematch = 0;
  tmatch = 0;
  cmatch = 0;
  hmatch = 0;
  pmatch = 0;
  pct = 0;
  timeEvent = -1;
  timeFill = -1;
}
//_____________________________________________________________________________
Int_t StVertexSeedMaker::Make(){
  if (date==0) GetADateTime();

  // Currently only finds database values for first event
  if (a[0] == -888) {
    LOG_INFO << "Reading db values at the start..." << endm;
    int status = FillAssumed();
    if (status != kStOk) return status;
    status = GetVertexSeedTriggers();
    if (status != kStOk) return status;
  }

  if (CheckTriggers()) {   // returns false for good events
    LOG_INFO << "event does not satisfy triggers" << endm;
    return kStOk;
  }

  int eventResult = GetEventData();
  if (eventResult != kStOk) return eventResult;

  if (zvertex<-998) {
    LOG_INFO << "No primary vertex" << endm;
    return kStOk;
  }

  // Calculate guessed vertex x & y from assumed params and measured z
  xguess = a[0] + (a[1] * zvertex);
  yguess = a[2] + (a[3] * zvertex);
  LOG_INFO << "x guess = " << xguess << endm; 
  LOG_INFO << "y guess = " << yguess << endm; 

  // Check to see if vertex is good for use in the fit
  float r2vertex = xvertex*xvertex + yvertex*yvertex;
  if ((zvertex > zVertexMin) && (zvertex < zVertexMax) &&
      (mult >= 5) && (r2vertex < r2VertexMax)){
    xdist->Fill(xvertex);
    xerr ->Fill(xvertex-xguess);
    ydist->Fill(yvertex);
    yerr ->Fill(yvertex-yguess);

    float XX[26];
    XX[0]  = (float) GetEventNumber();
    XX[1]  = xvertex;
    XX[2]  = yvertex;
    XX[3]  = zvertex;
    XX[4]  = mult;
    XX[5]  = trig;
    XX[6]  = (float) (run % 1000000);
    XX[7]  = (float) fill;
    XX[8]  = zdc;
    XX[9]  = rank;
    XX[10] = (float) itpc;
    XX[11] = (float) otpc;
    XX[12] = (float) detmap; // likely to be valid only up to ~23 bits
    XX[13] = exvertex;
    XX[14] = eyvertex;
    XX[15] = (float) pvn;
    XX[16] = (float) bmatch;
    XX[17] = (float) ematch;
    XX[18] = (float) tmatch;
    XX[19] = (float) cmatch;
    XX[20] = (float) hmatch;
    XX[21] = (float) pmatch;
    XX[22] = (float) pct;
    XX[23] = vpd_zvertex;
    XX[24] = (float) (timeEvent % 86400);
    XX[25] = (float) (timeFill >=0 ? timeEvent - timeFill : timeFill);
    resNtuple->Fill(XX);
    addVert(xvertex,yvertex,zvertex,mult,exvertex,eyvertex);
    sumzdc += zdc;
  }

  return kStOk;
}
//_____________________________________________________________________________
Int_t StVertexSeedMaker::Finish() {
  FindResult();
  return StMaker::Finish();
}
//_____________________________________________________________________________
bool StVertexSeedMaker::ValidTrigger(unsigned int tid) {
  // Determine if trigger id is among valid set
  if (!dbTriggersTable) return kTRUE; // running without DB access
  vertexSeedTriggers_st* trigsTable = dbTriggersTable->GetTable();
  int nTrigs = (int) (dbTriggersTable->GetNRows());
  for (int i = 0; i < nTrigs; i++, trigsTable++) {
    unsigned int dbTid = trigsTable->trigid;
    if (useAllTriggers || dbTid == 9999999 || (dbTid > 0 && tid == dbTid)) {
      trig = (float) tid;
      return kTRUE;
    }
  }
  return kFALSE;
}
//_____________________________________________________________________________
void StVertexSeedMaker::FindResult(bool checkDb) {
  bool writeIt = kFALSE;
  if (nverts >= minEntries){
    FitData();
    if (ep[0] > maxX0Err){
      LOG_ERROR << "x unstable. x0 error = " << ep[0] << " cm." << endm;
    }
    if (ep[2] > maxY0Err){
      LOG_ERROR << "y unstable. y0 error = " << ep[2] << " cm." << endm;
    }
    if ((ep[0] <= maxX0Err) && (ep[2] <= maxY0Err)) {
      if (checkDb) {
        // Do comparison of this data with data from database to see if
        // values have changed or improved.
        LOG_INFO << "Reading db values at the end..." << endm;
        int status = FillAssumed();
        if (status == kStOk) {
          if (ChangedValues() || BetterErrors()) writeIt = kTRUE;
          else { LOG_INFO << "Values have not changed/improved." << endm; }
        } else {
          LOG_WARN << "Could not get db values." << endm;
          writeIt = kTRUE;
        }
      } else {
        writeIt = kTRUE;
      }
    }
  } else {
    LOG_ERROR << "Insufficient stats for " <<
     "mean vertex determination.\n  Only " << nverts << " entries." << endm;
  }

  if (writeIt) WriteTableToFile();
  else { LOG_WARN << "Not writing table!!!!!" << endm; }

  if (mHistOut) WriteHistFile(writeIt);
}
//_____________________________________________________________________________
void StVertexSeedMaker::PrintInfo() {
  LOG_INFO << "\n**************************************************************"
           << "\n* $Id: StVertexSeedMaker.cxx,v 1.64 2017/08/08 03:58:20 genevb Exp $"
           << "\n**************************************************************" << endm;

  if (Debug()) StMaker::PrintInfo();
}
//_____________________________________________________________________________
void StVertexSeedMaker::WriteTableToFile(){
  TString fileName = NameFile("table","vertexSeed","C");
  ofstream *out = new ofstream(fileName.Data());
  St_vertexSeed* vertexSeedTable = VertexSeedTable();
  vertexSeedTable->SavePrimitive(*out,"");
  if (parsNtuple) AddResults(parsNtuple);
  delete out;
  delete vertexSeedTable;
  return;
}
//_____________________________________________________________________________
void StVertexSeedMaker::AddResults(TNtupleD* ntup){
  double datetime = ((double) date) + 1e-6*((double) time);
  TTimeStamp dt1(date,time,0,true,0);
  dt1.SetSec(dt1.GetSec() - 4*60*60); // convert GMT -> EDT
  int tm = dt1.GetTime();
  int se = tm%100; int hm = (tm-se)/100;
  int mn = hm%100; int hr = (hm-mn)/100;
  double days = ((double) dt1.GetDayOfYear()) +
                ((double) ((hr*60+mn)*60+se))/(24.*60.*60.);
  ntup->Fill(days,p[0],ep[0],p[2],ep[2],p[1],ep[1],p[3],ep[3],
             (double) nverts, datetime, (double) fill,
             sumzdc/((double) nverts),chi,beamWidth);
}
//_____________________________________________________________________________
St_vertexSeed* StVertexSeedMaker::VertexSeedTable(){
  // up to the user of this function to delete the table
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
void StVertexSeedMaker::WriteHistFile(bool writeFit){
  if (resNtuple->GetEntries() == 0) {
    LOG_INFO << "Not writing histograms - no entries!!!" << endm;
    return;
  }
  // .ROOT is NOT a typo !!!
  TString fileName = NameFile("histograms","vertexseedhist","ROOT");
  if (mTempOut) {
    mTempOut->Write();
    mTempOut->Close();
    if (gSystem->CopyFile(mTempOut->GetName(),fileName.Data()) ||
        gSystem->Unlink(mTempOut->GetName())) {
      LOG_ERROR << "Could not copy and/or delete temp vertexseedhist file!" << endm;
    }
    delete mTempOut;
    mTempOut = 0;
    // resNtuple disappears if & when mTempOut->Close() is called
  } else delete resNtuple;
  resNtuple = 0;

  TFile out(fileName.Data(),"UPDATE");
  GetHistList()->Write();
  if (writeFit) {
    // no need to garbage collect from newBLpars(): out.Close() takes care of it
    AddResults(newBLpars());
    out.Write();
  }
  out.Close();
}
//_____________________________________________________________________________
TString StVertexSeedMaker::NameFile(const char* type, const char* prefix, const char* suffix) {
  int fdate = date;
  int ftime = time;
  if (foffset) { // apply any time offsets
    TDatime fdatime(date,time);
    fdatime.Set(fdatime.Convert()+foffset);
    fdate = fdatime.GetDate();
    ftime = fdatime.GetTime();
  }
  TString fileNameBase = Form("%s.%08d.%06d.%s",prefix,fdate,ftime,suffix);

  if (defDir.Length()>0 && !defDir.EndsWith("/")) defDir.Append("/");
  TString fileName = defDir;
  fileName += fileNameBase;
  LOG_INFO << "Writing new " << type << " to:\n  "
    << fileName << endm;
  TString dirname = gSystem->DirName(fileName.Data());
  if (gSystem->OpenDirectory(dirname.Data())==0) { 
    if (gSystem->mkdir(dirname.Data())) {
      LOG_WARN << "Directory creation failed for:\n  " << dirname
      << "\n  Putting " << type << " file in current directory" << endm;
      fileName = fileNameBase;
    }
  }
  TString searchFile = fileName;
  if (gSystem->FindFile(".",searchFile)) {
    if (noclobber) {
      foffset++;
      LOG_WARN << "Existing file: trying 1 second later (offset="
               << foffset << ")..." << endm;
      fileName = NameFile(type,prefix,suffix);
      foffset--;
    } else {
      LOG_WARN << "Existing file: overwriting!" << endm;
    }
  }
  return fileName;
}
//_____________________________________________________________________________
int StVertexSeedMaker::FillAssumed(){
  TDataSet* dbDataSet = GetDataBase("Calibrations/rhic/vertexSeed");
  memset( a,0,4*sizeof(double));
  memset(ea,0,4*sizeof(double));
  if (!dbDataSet) {
    LOG_WARN << "Could not find Calibrations/rhic/vertexSeed in database" << endm;
  } else {
    St_vertexSeed* dbTableC =
      static_cast<St_vertexSeed*>(dbDataSet->FindObject("vertexSeed"));
    if (!dbTableC) {
      LOG_WARN << "Could not find vertexSeed in database" << endm;
    } else {
      vertexSeed_st* dbTable = dbTableC->GetTable();
       a[0] = dbTable->x0;
       a[1] = dbTable->dxdz;
       a[2] = dbTable->y0;
       a[3] = dbTable->dydz;
      ea[0] = dbTable->err_x0;
      ea[1] = dbTable->err_dxdz;
      ea[2] = dbTable->err_y0;
      ea[3] = dbTable->err_dydz;
    }
  }
  LOG_INFO << "Assumed values:"
    << "\n     x0 assumed = " << a[0] << " +/- " << ea[0]
    << "\n   dxdz assumed = " << a[1] << " +/- " << ea[1]
    << "\n     y0 assumed = " << a[2] << " +/- " << ea[2]
    << "\n   dydz assumed = " << a[3] << " +/- " << ea[3]
    << endm;
  return kStOk;
}
//_____________________________________________________________________________
int StVertexSeedMaker::GetVertexSeedTriggers(){
  TDataSet* dbDataSet = GetDataBase("Calibrations/rhic/vertexSeedTriggers");
  if (!dbDataSet) {
    LOG_ERROR << "Could not find Calibrations/rhic/vertexSeedTriggers in database" << endm;
    return kStErr;
  }
  dbTriggersTable =
    static_cast<St_vertexSeedTriggers*>(dbDataSet->FindObject("vertexSeedTriggers"));
  if (!dbTriggersTable) {
    LOG_ERROR << "Could not find vertexSeedTriggers in database" << endm;
    return kStErr;
  }
  return kStOk;
}
//_____________________________________________________________________________
bool StVertexSeedMaker::BetterErrors(){
  bool better = kFALSE;
  if ((ep[0] < ea[0]) || (ep[1] < ea[1]) ||
      (ep[2] < ea[2]) || (ep[3] < ea[3])) better = kTRUE;
  if (better) { LOG_INFO << "Values have improved" << endm; }
  return better;
}
//_____________________________________________________________________________
bool StVertexSeedMaker::ChangedValues(){
  bool changed = kFALSE;
  for (int i = 0; i<4; i++) {
    double diff = TMath::Abs(p[i] - a[i]);
    if ((diff >= ep[i]) || (diff >= ea[i])) changed = kTRUE;
  }
  if (changed) { LOG_INFO << "Values have changed" << endm; }
  return changed;
}
//_____________________________________________________________________________
void StVertexSeedMaker::GetADateTime() {
  date = GetDate();
  time = GetTime();
  LOG_INFO << "event date = " << date << endm;
  LOG_INFO << "event time = " << time << endm;
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
    run = *(static_cast<int*>(tab->getDataValue("runNumber",0)));
    LOG_INFO << tdstr << " is from run " << run << endm;
    float thisFill = *(static_cast<float*>(tab->getDataValue("blueFillNumber",0)));
    sprintf(queryStr,
      " where blueFillNumber=%f and deactive=0 order by beginTime asc limit 1",
      thisFill);
    db->QueryDb(tab,queryStr);

    // Extract date and time at start of fill
    char* start = tab->getBeginDateTime();
    fill = (int) thisFill;
    time = atoi(&(start[8]));
    start[8] = 0;
    date = atoi(start);
    timeFill = tab->getBeginTime();

    LOG_INFO << "Using fill no.  = " << fill << endm;
    LOG_INFO << "Using fill date = " << date << endm;
    LOG_INFO << "Using fill time = " << time << endm;
  } else {
    LOG_WARN << "Could not find beamInfo in database\n" <<
      "  Using event date/time." << endm;
    UseEventDateTime();
  }
}
//_____________________________________________________________________________
void StVertexSeedMaker::FitData() {
   LOG_INFO << "Now fitting the data..." <<
     "\n  *****************************************************" << endm;
   TVirtualFitter *minuit = TVirtualFitter::Fitter(0,26);
   // above '26' must be >25, or ROOT dictates a max of 3 params?!?
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

   beamWidth = 0;
   do {

   int status = minuit->ExecuteCommand("MIGRAD", arglist ,1);
   if (status) {
     LOG_ERROR << "StVertexMaker: error on migrad call, err = "
       << status << endm;
     return;
   }
   
   double amin,edm,errdef;
   int nvpar,nparx;
   minuit->GetStats(amin,edm,errdef,nvpar,nparx);
   chi = amin/((double) (nverts-4));
   LOG_INFO << "beamWidth = " << beamWidth << ", chisq = " << amin << ", chisq/dof = " << chi <<
     "\n  *****************************************************" << endm;

   beamWidth += 0.005 * TMath::Min((int) (chi*0.1+1),10); // 50 micron steps
   } while (chi>1.1 && beamWidth<=0.15);
   
   char pname[10];
   for (int i=0; i<4; i++)
     minuit->GetParameter(i, pname, p[i], ep[i], plow[i], phigh[i]);
}
//_____________________________________________________________________________
TNtupleD* StVertexSeedMaker::newBLpars() {
  // up to the user of this function to delete the ntuple
  return new TNtupleD("BLpars","BeamLine parameters",
    "days:x0:err_x0:y0:err_y0:dxdz:err_dxdz:dydz:err_dydz:stats:date:fill:zdc:chi:beamwidth");
}
//_____________________________________________________________________________
void StVertexSeedMaker::Packer(int firstbit, int nbits, int& var, unsigned short val) {
  var = val;
  // erase nbits of detmap, starting at firstbit,
  // and fill the bits with val, capped at 2^nbits-1
  int cap = ~((~0)<<nbits); // e.g. nbits=3 => cap=7
  (detmap &= ~(cap<<firstbit)) |= (TMath::Min(var,cap)<<firstbit);
}
//_____________________________________________________________________________
int StVertexSeedMaker::Aggregate(char* dir, const char* cuts, const int offset) {
  // Format of filenames for parsing must be:
  // vertexseedhist.DDDDDDDD.TTTTTT.root
  // where D and T are 8 and 6 digit representations of date and time

  // offset will be applied as a time offset in the date.time use in writing new files
  SetOffset(offset);

  TFile parsOut("BLpars.root","RECREATE");
  parsNtuple = newBLpars();

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
  fileList.SetOwner();
  fileList.Sort();

  int aggMode = 0;
  // 0 : by fill
  // 1 : all data into one result
  // 2 : by date (flawed for input files & runs that span dates)
  // 3 : by file (retains the same number of files)

  // Parse cuts for keywords
  TString cutsStr = cuts;
  if (cutsStr.Contains("ALL")) {
    aggMode = 1;
    cutsStr.ReplaceAll("ALL&&","");
    cutsStr.ReplaceAll("&&ALL","");
    cutsStr.ReplaceAll("ALL","");
  } else if (cutsStr.Contains("DATE")) {
    aggMode = 2;
    cutsStr.ReplaceAll("DATE&&","");
    cutsStr.ReplaceAll("&&DATE","");
    cutsStr.ReplaceAll("DATE","");
  } else if (cutsStr.Contains("FILE")) {
    aggMode = 3;
    cutsStr.ReplaceAll("FILE&&","");
    cutsStr.ReplaceAll("&&FILE","");
    cutsStr.ReplaceAll("FILE","");
  }
  const char* cleanedCuts = cutsStr.Data();

  // Try to catch stuck values and ignore them
  static float prevX = -987.0;

  TFile* currentFile=0;
  float* vals=0;
  int nfiles = fileList.GetSize();
  for (int filen = 0; filen < nfiles; filen++) {
    int fillf = fill;
    int datef = date;
    int timef = time;
    fileName = fileList.At(filen)->GetName();
    if (aggMode != 1 || date==0) {
      TString dateTime = fileName;
      dateTime.Remove(0,dateTime.Last('/') + 1);
      dateTime.Remove(0,dateTime.First('.') + 1).Remove(15);
      TString dateStr = dateTime;
      if (aggMode == 3) GetFillDateTime(); // fill date and time will be overwritten
      date = atoi(dateStr.Remove(8).Data());
      time = atoi(dateTime.Remove(0,9).Remove(6).Data());
      if (aggMode != 3) GetFillDateTime(); // file date and time will be overwritten
      if (aggMode == 2) time = 0;
    }
    if ((currentFile) && (
         (aggMode == 0 && fill != fillf) ||
         (aggMode == 2 && date != datef) ||
         (aggMode == 3) )) {
      LOG_INFO << "Aggregator has changed\n"
        << "  Processing data from previous aggregation before continuing" << endm;
      int fillp = fill;
      int datep = date;
      int timep = time;
      int timeFillp = timeFill;
      fill = fillf;
      date = datef;
      time = timef;
      currentFile->Close();
      currentFile = 0;
      FindResult(kFALSE);
      Reset();
      fill = fillp;
      date = datep;
      time = timep;
      timeFill = timeFillp;
    }

    LOG_INFO << "Now opening file:\n  " << fileName << endm;
    if (currentFile) currentFile->Close();
    currentFile = new TFile(fileName);
    TNtuple* curNtuple = static_cast<TNtuple*>(currentFile->Get("resNtuple"));
    if (!curNtuple) {
      LOG_ERROR << "No resNtuple found in " << fileName << endm;
      continue;
    }
    curNtuple->Draw(">>elistVtxSeed",cleanedCuts);
    TEventList* elist = static_cast<TEventList*>(gDirectory->Get("elistVtxSeed"));
    int nentries = (elist ? (int) elist->GetN() : 0);
    int nvar = curNtuple->GetNvar();
    int rvar = resNtuple->GetNvar();
    for (int entryn = 0; entryn < nentries; entryn++) {
      curNtuple->GetEntry(elist->GetEntry(entryn));
      vals = curNtuple->GetArgs();
      if (vals[1] == prevX) continue; // stuck value!
      else prevX = vals[1];
      unsigned int tid = (unsigned int) vals[5];
      bool updateForTimeFill = (nvar > 24 && timeFill >=0 &&
                                vals[24] >= 0 && vals[25] < 0);
      if (ValidTrigger(tid)) {
        if (nvar < rvar || updateForTimeFill) {
          float vals2[32];
          memset(vals2,0,32*sizeof(float));
          memcpy(vals2,vals,nvar*sizeof(float));
          if (nvar < 20) {
            // detmap should be converted...
            detmap = (int) (vals[12]);
            bmatch = (detmap)&7;
            ematch = (detmap>>3)&7;
            tmatch = (detmap>>6)&7;
            cmatch = (detmap>>9)&3;
            hmatch = (detmap>>11)&7;
            vals2[16] = (float) bmatch;
            vals2[17] = (float) ematch;
            vals2[18] = (float) tmatch;
            vals2[19] = (float) cmatch;
            vals2[20] = (float) hmatch;
          }
          if (nvar < 22) {
            vals2[21] = (float) pmatch;
            vals2[22] = (float) pct;
            vals2[23] = vpd_zvertex;
          }
          if (nvar < 25) {
            vals2[24] = (float) (timeEvent % 86400);
            vals2[25] = (float) (timeFill >= 0 ? timeEvent - timeFill : timeFill);
          } else if (updateForTimeFill) {
            // timeFill not obtained previously, but available now
            int timeTemp = ((int) vals[24]) - (timeFill % 86400);
            while (timeTemp < 0) timeTemp += 86400;
            vals2[25] = (float) timeTemp;
          }
          resNtuple->Fill(vals2);
        } else
          resNtuple->Fill(vals);
        if (nvar > 13) // errors are available
          addVert(vals[1],vals[2],vals[3],vals[4],vals[13],vals[14]);
        else
          addVert(vals[1],vals[2],vals[3],vals[4],0.,0.);
        sumzdc += vals[8];
      } else {
        LOG_INFO << "Invalid trigger: " << tid << endm;
      }
    }
    LOG_INFO << "Current statistics: " << nverts << endm;
  }
  if (currentFile) {
    currentFile->Close();
    delete currentFile;
    currentFile = 0;
    FindResult(kFALSE);
  }
  parsOut.Write();
  parsOut.Close();
  LOG_INFO << "Examined " << nfiles << " files" << endm;
  return nfiles;
}
//_____________________________________________________________________________
// $Id: StVertexSeedMaker.cxx,v 1.64 2017/08/08 03:58:20 genevb Exp $
// $Log: StVertexSeedMaker.cxx,v $
// Revision 1.64  2017/08/08 03:58:20  genevb
// Add vertex-seed-finding with picoDsts
//
// Revision 1.63  2017/08/07 18:10:57  genevb
// Introduce modes for aggregation
//
// Revision 1.62  2016/08/02 21:17:17  genevb
// Added tDay,tFill to resNtuple, and improved C++11 compliance
//
// Revision 1.61  2015/08/31 19:17:00  genevb
// Add chi and beamwidth to BLpars ntuple
//
// Revision 1.60  2015/05/23 02:38:07  genevb
// Ntuples attached to files should not get deleted, more rapid chi2 convergence
//
// Revision 1.59  2015/05/19 19:36:09  genevb
// Code cleanup in preparation for C++11
//
// Revision 1.58  2015/05/19 17:58:15  genevb
// Better garbage collection
//
// Revision 1.57  2015/05/15 05:38:21  genevb
// Include prompt hits and post-crossing tracks, simplify detmap packing, update doxygen documentation
//
// Revision 1.56  2015/05/14 20:29:25  genevb
// Add z of VPD vertex
//
// Revision 1.55  2013/08/14 21:42:48  genevb
// Introduce time offsets, noclobber toggle, more matched-tracks controls
//
// Revision 1.54  2012/10/11 16:33:12  genevb
// Protect against zero entries, and a more unique entry list name
//
// Revision 1.53  2012/10/01 17:50:07  genevb
// Reduce some overhead DB queries by being more specific about needed tables
//
// Revision 1.52  2012/08/22 04:52:35  genevb
// Add BeamLine parameter ntuples to output
//
// Revision 1.51  2012/08/17 22:57:33  genevb
// Add index of vertex within event to ntuple
//
// Revision 1.50  2012/08/16 05:37:07  genevb
// Missing mean ZDC for aggregation
//
// Revision 1.49  2012/08/15 22:11:06  genevb
// Improved doxygen-ready documentation
//
// Revision 1.48  2012/08/14 23:56:06  genevb
// detmap now includes BEMC+EEMC+BTOF+CM, added mean zdc to log output
//
// Revision 1.47  2012/02/29 02:00:04  genevb
// Hack to get TVirtualFitter to allow more than 3 parameters
//
// Revision 1.46  2012/02/28 21:54:37  genevb
// Restore .root to .ROOT to avoid St_db_Maker read-in
//
// Revision 1.45  2010/07/19 20:48:32  genevb
// Forgot to shift ex,ey after detmap addition to ntuple
//
// Revision 1.44  2010/07/02 22:36:10  genevb
// Option for using all triggers
//
// Revision 1.43  2009/11/23 21:38:56  genevb
// Fix problems with memory-resident TNtuple by using a temporary disk file
//
// Revision 1.42  2009/11/16 22:31:11  genevb
// phase out usage of old tables
//
// Revision 1.41  2009/11/10 20:54:13  fisyak
// pams Cleanup
//
// Revision 1.40  2009/05/22 23:50:50  genevb
// Code mods for BEMC matches, BeamWidth
//
// Revision 1.39  2008/05/21 17:48:39  genevb
// Use vertex errors for weighting
//
// Revision 1.38  2008/04/29 23:30:33  genevb
// Added cuts capability to Aggregate
//
// Revision 1.37  2008/04/29 19:06:06  genevb
// handle no DB access
//
// Revision 1.36  2007/11/27 23:42:47  genevb
// Move valid triggers from code to DB
//
// Revision 1.35  2007/07/12 19:46:56  fisyak
// Add includes for ROOT 5.16
//
// Revision 1.34  2007/04/28 17:56:31  perev
// Redundant StChain.h removed
//
// Revision 1.33  2007/04/22 04:25:59  genevb
// printf, gMessMgr ==> STAR Logger
//
// Revision 1.32  2006/09/01 22:27:16  genevb
// More detailed info in ntuple
//
// Revision 1.31  2006/08/16 21:58:01  genevb
// Added 2006 pp62 triggers
//
// Revision 1.30  2006/05/27 19:54:03  genevb
// Yet more 2006 triggers
//
// Revision 1.29  2006/05/11 18:09:44  genevb
// More pp2006 triggers
//
// Revision 1.28  2006/05/10 03:57:08  genevb
// ppProductionTrans trigger for 2006
//
// Revision 1.27  2006/05/08 02:38:21  genevb
// Added 2006 triggers
//
// Revision 1.26  2005/07/16 21:24:03  genevb
// Fixed bug with pp400 data from 2005
//
// Revision 1.25  2005/07/15 18:38:47  genevb
// ppTrans triggers, and fix for too many open files
//
// Revision 1.24  2005/07/14 21:02:40  genevb
// Modified use of test triggers
//
// Revision 1.23  2005/07/01 21:46:01  genevb
// Specify output directory
//
// Revision 1.22  2005/06/14 18:51:31  genevb
// Updates to allow for pp2005 triggers, and inheritance
//
// Revision 1.21  2004/08/02 01:19:33  genevb
// minor fixes for getting directories correct
//
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
