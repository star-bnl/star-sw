// $Id: StVertexSeedMaker.cxx,v 1.3 2002/01/28 22:16:33 genevb Exp $
// $Log: StVertexSeedMaker.cxx,v $
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
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StVertexSeedMaker class                                              //
// Author: G. Van Buren, BNL
// Description: calculates mean primary vertex positions from           //
//              suitable events to use as seeds in finding better       //
//              primary vertex positions (helpful for low               //
//              multiplicity events like pp collisions)                 //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "StVertexSeedMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StDAQMaker/StDAQReader.h"
#include "St_db_Maker/St_db_Maker.h"
#include "St_tcl_Maker/St_tcl_Maker.h"
#include "tables/St_dst_vertex_Table.h"
#include "tables/St_dst_L0_Trigger_Table.h"
#include "StMessMgr.h"
#include "StVertexId.h"
#include "tables/St_vertexSeed_Table.h"
#include "TSystem.h"
#include "TFile.h"
#include "TVirtualFitter.h"
#include "TNtuple.h"

//_____________________________________________________________________________
// C variables and functions for fit/minimization
//_____________________________________________________________________________
static float *xVert, *yVert, *zVert, *mult;
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
    // Error1 set such that 5 tracks => ~8mm, proportional to 1/sqrt(mult)
    // This was determined by trying to get the chisq/dof distribution
    // to peak near 1.0
    //                              =>  Error1   = (0.8cm) / sqrt(mult/5)
    //                              =>  Error1^2 = (3.2) cm^2 / mult
    // Beam spot size (sigma) Error2 ~= 0.025 cm (250 microns)
    //                              =>  Error2^2 = 0.000625 cm^2
    // The total error should be sqrt(Error1^2 + Error2^2)
    error_sq = 0.000625 + (3.2 / mult[i]); 
    delta_sq = pow(xVert[i]-funcX(zVert[i],par),2) +
               pow(yVert[i]-funcY(zVert[i],par),2);
    chisq += (delta_sq/error_sq);
  }
  f = chisq;
}
void setArraySize(int n) {
  if (n <= nsize) return;
  float* temp1 = new float[n];
  float* temp2 = new float[n];
  float* temp3 = new float[n];
  float* temp4 = new float[n];
  if (nsize) {
    int csize = nsize * sizeof(float);
    memcpy(temp1,xVert,csize);
    memcpy(temp2,yVert,csize);
    memcpy(temp3,zVert,csize);
    memcpy(temp4,mult ,csize);
    delete [] xVert;
    delete [] yVert;
    delete [] zVert;
    delete [] mult ;
  }
  xVert = temp1;
  yVert = temp2;
  zVert = temp3;
  mult  = temp4;
  nsize = n;
}
void addVert(float x, float y, float z, float m) {
  if (nverts > nsize) setArraySize(nsize*2);
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
  minEntries = 100;   //require 100 valid verts for a seed determination
  maxX0Err     = 0.05; //vertex x should be good to 500 microns
  maxY0Err     = 0.05; //vertex y should be good to 500 microns
  mHistOut=kTRUE;
  zVertexMax = 100.0;
  zVertexMin = -100.0;
  r2VertexMax = 15.0;
  nsize = 0;
  setArraySize(512);
  UseEventDateTime(); // By default, use the data & time from the first event
}
//_____________________________________________________________________________
StVertexSeedMaker::~StVertexSeedMaker(){
}
//_____________________________________________________________________________
Int_t StVertexSeedMaker::Init(){
  xguess = 0;
  yguess = 0;
  zvertex = -999.0;
  HIST_MIN = -3.0;
  HIST_MAX =  3.0;
  xdist = new TH1F("xdist","xdist",1000,HIST_MIN,HIST_MAX);
  ydist = new TH1F("ydist","ydist",1000,HIST_MIN,HIST_MAX);
  xerr  = new TH1F("xerr","x measured - x guess",1000,HIST_MIN,HIST_MAX);
  yerr  = new TH1F("yerr","y measured - y guess",1000,HIST_MIN,HIST_MAX);
  resNtuple = new TNtuple("resNtuple","resNtuple","event:x:y:z:x:mult");
  AddHist(xdist);
  AddHist(ydist);
  AddHist(xerr);
  AddHist(yerr);
  date = 0;
  time = 0;
  a[0]   = -888.0;
  //a[0]   = 0.0;
  a[1] = 0.0;
  a[2]   = 0.0;
  a[3] = 0.0;
  chi = 0.0;
  weight = 0.0;
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
    gMessMgr->Info("StVertexSeedMaker: reading db values at the start...");
    Int_t status = FillAssumed();
    if (status != kStOk) return status;
  }

  // Get primary vertex from evr
  TDataSet *ds=GetDataSet("dst/vertex");
  if (!ds) {
    gMessMgr->Error("StVertexSeedMaker: cannot locate vertex dataset!");
    return kStErr;
  }
  TDataSetIter dsiter(ds);
  St_dst_vertex *vert = (St_dst_vertex *) dsiter.Find("vertex");
  if (!vert) {
    gMessMgr->Error("StVertexSeedMaker: cannot locate vertex table!");
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
    gMessMgr->Info("StVertexSeedMaker: no primary vertex");
    return kStOK;
  }

  // Calculate guessed vertex x & y from assumed params and measured z
  xguess = a[0] + (a[1] * zvertex);
  yguess = a[2] + (a[3] * zvertex);
  gMessMgr->Info() << "StVertexSeedMaker::x guess = " << xguess << endm; 
  gMessMgr->Info() << "StVertexSeedMaker::y guess = " << yguess << endm; 

  // Check to see if vertex is good for use in the fit
  float r2vertex = pow(xvertex,2) + pow(yvertex,2);
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
  Bool_t writeIt = kFALSE;
  if (nverts >= minEntries){
    fitData();
    if (ep[0] > maxX0Err){
      gMessMgr->Error() << "StVertexSeedMaker::x unstable. x0 error = " <<
        ep[0] << " cm." << endm;
    }
    if (ep[2] > maxY0Err){
      gMessMgr->Error() << "StVertexSeedMaker::y unstable. y0 error = " <<
        ep[2] << " cm." << endm;
    }
    if ((ep[0] <= maxX0Err) && (ep[2] <= maxY0Err)) {
      // Do comparison of this data with data from database to see if
      // values have changed or improved.
      gMessMgr->Info("StVertexSeedMaker: reading db values at the end...");
      Int_t status = FillAssumed();
      if (status == kStOk) {
        if (ChangedValues() || BetterErrors()) writeIt = kTRUE;
        else gMessMgr->Info("StVertexSeedMaker: values have not changed/improved.");
      } else {
        gMessMgr->Warning("StVertexSeedMaker: could not get db values.");
        writeIt = kTRUE;
      }
    }
  } else {
    gMessMgr->Error() << "StVertexSeedMaker:Insufficient statistics for " <<
     "mean vertex determination.  Only " << nverts << " entries." << endm;
  }

  if (writeIt) WriteTableToFile();
  else gMessMgr->Warning("StVertexSeedMaker: not writing table!!!!!");

  if (mHistOut) WriteHistFile();
 
  return StMaker::Finish();
}
//_____________________________________________________________________________
void StVertexSeedMaker::PrintInfo() {
  printf("**************************************************************\n");
  printf("* $Id: StVertexSeedMaker.cxx,v 1.3 2002/01/28 22:16:33 genevb Exp $\n");
  printf("**************************************************************\n");

  if (Debug()) StMaker::PrintInfo();
}
//_____________________________________________________________________________
void StVertexSeedMaker::WriteTableToFile(){
  gMessMgr->Info("StVertexSeedMaker: Writing new table.");
  char filename[80]; 
  sprintf(filename,"./StarDb/Calibrations/rhic/vertexSeed.%08d.%06d.C",date,time);
  TString dirname = gSystem->DirName(filename);
  if (gSystem->OpenDirectory(dirname.Data())==0) { 
    if (gSystem->mkdir(dirname.Data())) {
      cout << "Directory " << dirname << " creation failed" << endl;
      cout << "Putting vertexSeed.C in current directory" << endl;
      for (int i=0;i<80;i++){filename[i]=0;}
      sprintf(filename,"vertexSeed.%08d.%06d.C",date,time);
    }
  }
  ofstream *out = new ofstream(filename);
  vertexSeedTable()->SavePrimitive(*out,"");
  return;
}
//_____________________________________________________________________________
St_vertexSeed* StVertexSeedMaker::vertexSeedTable(){
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
  table->SetNRows(1);
  return table;
}
//_____________________________________________________________________________
void StVertexSeedMaker::WriteHistFile(){
  char filename[80]; 
  sprintf(filename,"vertexseedhist.%08d.%06d.root",date,time);
  TFile out(filename,"RECREATE");
  GetHistList()->Write();
  resNtuple->Write();
  out.Close();
}
//_____________________________________________________________________________
Int_t StVertexSeedMaker::FillAssumed(){
  TDataSet* dbDataSet = GetDataBase("Calibrations/rhic");
  if (!dbDataSet) {
    gMessMgr->Error("StVertexSeedMaker: could not find Calibrations/rhic database");
    return kStErr;
  }
  St_vertexSeed* dbTableC =
    (St_vertexSeed*) (dbDataSet->FindObject("vertexSeed"));
  if (!dbTableC) {
    gMessMgr->Error("StVertexSeedMaker: could not find vertexSeed in database");
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
  gMessMgr->Info() << "StVertexSeedMaker: assumed values:"
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
  if (better) gMessMgr->Info("StVertexSeedMaker: values have improved");
  return better;
}
//_____________________________________________________________________________
Bool_t StVertexSeedMaker::ChangedValues(){
  Bool_t changed = kFALSE;
  for (int i = 0; i<4; i++) {
    double diff = TMath::Abs(p[i] - a[i]);
    if ((diff >= ep[i]) || (diff >= ea[i])) changed = kTRUE;
  }
  if (changed) gMessMgr->Info("StVertexSeedMaker: values have changed");
  return changed;
}
//_____________________________________________________________________________
void StVertexSeedMaker::FillDateTime(){
  date = GetDate();
  time = GetTime();
  gMessMgr->Info() << "StVertexSeedMaker: event date = " << date << endm;
  gMessMgr->Info() << "StVertexSeedMaker: event time = " << time << endm;
  if (!useEventDateTime) {
    int datef = 0;
    int timef = 0;
    int fillf = -1;
    char filename[128];
    sprintf(filename,"./StRoot/StPass0CalibMaker/fillNumberTimes.txt");
    ifstream in(filename);
    if (!in) { 
      sprintf(filename,
        gSystem->ExpandPathName("${STAR}/StRoot/StPass0CalibMaker/fillNumberTimes.txt"));
      in.open(filename);
    }
    if (in) {
      gMessMgr->Info() << "StVertexSeedMaker: using fill date/time file: " << filename << endm;
      const int maxc = 128;
      char inbuf[maxc];
      inbuf[2] = ' ';
      // loop until line with 5th char a space
      while ((in.getline(inbuf,maxc)) && (inbuf[2] != ' ')) {}
      if (inbuf[2] == ' ') {
          gMessMgr->Error("StVertexMaker: format of date/time file incorrect. Uh-oh....");
      }
      // Format from here should look like:
      // |  FFFF |  DDDDDDDDTTTTTT |
      // where the only length constraint is that the date D should have only 8 characters
      int datep = 0;
      int timep = 0;
      int fillp = -1;
      while ((date > datef) || ((date == datef) && (time > timef))) {
        fillp = fillf; datep = datef; timep = timef;
        char* start = &(inbuf[2]);
        fillf = atoi(start);
        start = strchr(start,'|');
        while ((*start == ' ') || (*start == '|')) { start++; } // get past spaces and bars
        timef = atoi(&(start[8]));  // date portion is 8 characters. Read time first,
        start[8] = 0;               // then get date by ending string where time starts.
        datef = atoi(start);
        in.getline(inbuf,maxc);
      }
      in.close();
      fill = fillp; date = datep; time = timep;
      gMessMgr->Info() << "StVertexSeedMaker: using fill no.  = " << fill << endm;
      gMessMgr->Info() << "StVertexSeedMaker: using fill date = " << date << endm;
      gMessMgr->Info() << "StVertexSeedMaker: using fill time = " << time << endm;
    } else {
      gMessMgr->Warning("StVertexSeedMaker: no fill date/time file found, using event date/time.");
      UseEventDateTime();
    }
  }
}
//_____________________________________________________________________________
void StVertexSeedMaker::fitData(){
   gMessMgr->Info("StVertexSeedMaker: Now fitting the data...");
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
     << ", chisq/dof = " << chi << endm;
   
   char pname[10];
   for (int i=0; i<4; i++)
     minuit->GetParameter(i, pname, p[i], ep[i], plow[i], phigh[i]);
}

