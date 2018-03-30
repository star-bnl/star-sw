///////////////////////////////////////////
//
// matchTpcFieldCageShorts.C
// Author: G. Van Buren, BNL
// Created: August 24, 2005
// Modified: April 11, 2007
//
// Creates an ntuple with IFC currents and daqSummary to indicate runs
//
///////////////////////////////////////////

#if !defined(__CINT__) || defined(__MAKECINT__)
#include "TROOT.h"
#include "TFile.h"
#include "TNtupleD.h"
#include <iostream>
#include <fstream>
#include "TTimeStamp.h"
#include "TDatime.h"
#endif



  const int size1 = 6; // number of elements per line 1
  const int size2 = 9; // " 2
  const int size3 = 5; // time + 4 currents

  ifstream in1("currents.txt");
  ifstream in2("run_times.txt");
  ofstream out2("dbout.csv");
  ofstream out3("dbout.txt");

  int writing=0;

///////////////////////////////////////////

  // data arrays
  Double_t ff1[15];
  Double_t ff2[9];
  // for averaging:
  Double_t ff3[5];
  Double_t ff4[5];

  TFile scf("cur.root","RECREATE");
  TNtupleD sc("cur","current","t:Ioe:Iie:Iow:Iiw:res:run:Y:M:D:h:m:s:begin:end");
  
  int li1=0;
  int li2=0;
  const int LI=1000000;
  
  int tcnt = 0;

  // db entry values
  int dataid;
  char* eT;

  double mincur,maxcur,dcur,maxdcur=0;
  double pflag = 0;
  double last_missing_resistance=0;
  double last_resistor=0;
  double maxsec = 2147483647.0; // 2^31-1
  double nexttime = 0;
  double resistor = 0;
  UInt_t tOffset = 0;

void Finish() {
  printf("Maximum current difference = %f\n",maxdcur);
  sc.Write();
  scf.Close();
  abort();
}

void ReadLine1() {
  if (in1.eof()) Finish();
  int i;
  ff1[0] = nexttime;
  for (i=1; i<size1; i++) in1 >> ff1[i];
  in1 >> nexttime;
  //ff1[4]-=0.377; // only if we need to ignore permanent short at locaton 181.4
  if (in1.eof()) nexttime = maxsec;
  for (i=size1; i<size1+size2; i++) ff1[i] = 0;
  if ((++li1)%LI==0) printf("Line1 = %d , time = %f\n",li1,ff1[0]);
}

void ReadLine2() {
  for (int i=0; i<size2; i++) in2 >> ff2[i];
  if (in2.eof()) {
    for (int i=0; i<size2; i++) ff2[i]=0;
    ff2[7] = maxsec-1;
    ff2[8] = maxsec;
    return;
  }
  if ((++li2)%LI==0) printf("Line2 = %d , time = %f\n",li2,ff2[7]);
}

void CopyF2F1() {
  for (int i=0; i<size2; i++) ff1[i+size1] = ff2[i];
}


void FindTOffset() {
  TTimeStamp begin1((UInt_t) ff2[1],(UInt_t) ff2[2],(UInt_t) ff2[3],
                    (UInt_t) ff2[4],(UInt_t) ff2[5],(UInt_t) ff2[6]);
  tOffset = begin1.GetSec() - (UInt_t) ff2[7];
  printf("Found time offset to be: %u\n",tOffset);
}

void UpdateF2() {
  TDatime newbegin(tOffset + (UInt_t) ff1[0]);
  newbegin.Set(newbegin.Convert(kTRUE)); // Fix a locatltime <-> GMT bug
  ff2[1] = newbegin.GetYear();
  ff2[2] = newbegin.GetMonth();
  ff2[3] = newbegin.GetDay();
  ff2[4] = newbegin.GetHour();
  ff2[5] = newbegin.GetMinute();
  ff2[6] = newbegin.GetSecond();
  CopyF2F1();
}

void WriteTable() {

  Int_t ii[6];
  for (int i=0; i<6; i++) ii[i] = (int) ff2[i+1];
  // Modify from current->resistance

  // Only do Iow (which is index i=4)
  Double_t ring = 80.5;
  Double_t reference = TMath::Max(ff3[2],ff3[3]); // Use max(Ioe,Iiw) as reference
  Double_t missing_resistance = 364.44*(1.0 - reference/ff3[4]); // delR = R*(1-(I1/I2))
//printf("GGGG ref = %g  , cur = %g\n",reference,ff3[4]);
  //missing_resistance += resistor;
  
  if (last_missing_resistance && TMath::Abs(missing_resistance - last_missing_resistance) > 0.5) {
    printf("BIG RESISTANCE JUMP:   %f   ->   %f  :: dif =   %f\n",
      last_missing_resistance,missing_resistance,missing_resistance-last_missing_resistance);
    pflag=3;
  }
  if ((last_missing_resistance && TMath::Abs(missing_resistance - last_missing_resistance) < 0.1)
    || (last_missing_resistance > 0.2 && missing_resistance > 0.2)
    || (last_missing_resistance < 0.2 && missing_resistance < 0.2)) {
    //&& TMath::Abs(last_resistor - resistor) < 0.1) {
    printf("SMALL RESISTANCE JUMP:   %f   ->   %f  :: dif =   %f\n",
      last_missing_resistance,missing_resistance,missing_resistance-last_missing_resistance);
    return;
  }

  dataid++;
  TString outname = Form("dbout/tpcFieldCageShort.%04d%02d%02d.%02d%02d%02d.C",ii[0],ii[1],ii[2],ii[3],ii[4],ii[5]);
  ofstream* out1=0;
  if (writing) out1 = new ofstream(outname.Data());

  double missing_res2 = missing_resistance;
  missing_res2 = (missing_resistance > 0.2 ? 0.345 : 0.0);

  TString buffer = Form("\"%d\",\"%s\",\"33\",\"1\",",dataid,eT);
  //TString buffer = "\"33\",\"1\",";
  buffer += Form("\"%04d-%02d-%02d %02d:%02d:%02d\",\"ofl\",\"1\",\"0\",",ii[0],ii[1],ii[2],ii[3],ii[4],ii[5]);
  buffer += Form("\"1.0\",\"1.0\",\"%5.3f\",\"%7.5f\",\"%7.5f\"",ring,0.0,missing_res2);
  if (writing) (*out1) << buffer << endl;
  out2 << buffer << endl;

  buffer = Form("%04d%02d%02d %02d%02d%02d",ii[0],ii[1],ii[2],ii[3],ii[4],ii[5]);
  buffer += Form(" 1.0 1.0 %5.3f %7.5f %7.5f",ring,0.0,missing_res2);
  out3 << buffer << endl;

  last_missing_resistance = missing_resistance;
  last_resistor = resistor;

  // QA
  if (ff3[2]==0) {printf("ZERO CURRENT!...\n"); pflag=2;}
  int tt = (int) (ff4[0]-ff3[0]);
  //if (pflag) {pflag=0; cout << outname << " :: " << dcur << " :: " << tcnt << " :: " << ff3[0] <<
  {pflag=0; cout << outname << " :: " << dcur << " :: " << tcnt << " :: " << ff3[0] <<
    "," << tt << "," << (int) ff2[0] << "," << ff3[4] << endl;}


  if (writing) {out1->close(); delete out1; out1=0;}
}
  

void ResetMeasure() {
  if (tcnt) {
    // Keep track of maximum current difference over a measure
    dcur = maxcur-mincur;
    if (dcur>0.15) {printf("LARGE CURRENT SPREAD: %f\n",dcur); pflag=1;}
    if (dcur>maxdcur) maxdcur = dcur;

    //printf("Writing table with tcnt=%d , ff3:0=%f\n",tcnt,ff3[0]);
    if (tcnt>1) for (int i=1; i<size3; i++) ff3[i] /= ff3[0];
    WriteTable();
    UpdateF2();
    tcnt = 0;
  }
  for (int i=0; i<size3; i++) { ff3[i]=0; ff4[i]=0; }

  maxcur = 0;
  mincur = 100;
}

void IncludeThisMeasure() {
  // Use value for only 1 measure, otherwise average over time difference
  for (int i=1; i<size3; i++) if (ff1[i] < 76.0) return; // Don't use if any currents are way off

// Big change test
  if (tcnt) {
  //if (tcnt>2) { // don't decide based on one or two points?
    double avg = ff3[4];
    if (tcnt>1) avg /= ff3[0];
    if (TMath::Abs(avg-ff1[4]) > 0.045) ResetMeasure(); // 45nA change from the average
  }


  if (tcnt==1) for (int i=0; i<size3; i++) ff3[i]=0;
  Double_t tdiff = 2;
  if (tcnt>0) tdiff = ff1[0]-ff4[0];
  if (tdiff == 0) tdiff = 0.001;
  ff3[0] += tdiff;
  // average this (ff1) and previous (ff4) currents, store in ff3.
  for (int i=1; i<size3; i++) ff3[i] += tdiff*0.5*(ff1[i]+ff4[i]);
  for (int i=0; i<size3; i++) ff4[i] = ff1[i];
  tcnt++;
  resistor = ff1[5];

  // Keep track of maximum and minimum current over a measure
  if (ff1[4]>maxcur) maxcur = ff1[4];
  if (ff1[4]<mincur) mincur = ff1[4];
}

void FindNextRun() {
  ResetMeasure();
  ReadLine2();
  FindTOffset();
}

void UseThisRun() {
  CopyF2F1();
  IncludeThisMeasure();
}

// Main routine
void run() {
  for (int i=0; i<size2; i++) ff2[i]=0;
  tcnt = 0;
  in1 >> nexttime;

  while (1) {
    ReadLine1();
    Double_t time = ff1[0];

    while (time > ff2[8]) FindNextRun();

    if (nexttime > ff2[7]) UseThisRun();

    sc.Fill(ff1);
  }

}

void matchTpcFieldCageShorts(char* T, double lr, double lm, int i) {
  dataid = i;
  eT = T;
  last_resistor = lr;
  last_missing_resistance = lm;
  run();
}

//////////////////////////////////////////
// $Id: matchTpcFieldCageShorts.C,v 1.5 2018/03/30 04:24:29 genevb Exp $
// $Log: matchTpcFieldCageShorts.C,v $
// Revision 1.5  2018/03/30 04:24:29  genevb
// Corrected currents order, no permanent shorts, compile pragmas
//
// Revision 1.4  2013/09/12 17:09:02  genevb
// Update DBs, use full unixtime, small improvements
//
// Revision 1.3  2010/01/08 19:48:36  genevb
// Update for permanent short at OFCW ring 181.4
//
// Revision 1.2  2009/09/23 00:06:55  genevb
// More precise resistances
//
// Revision 1.1  2009/02/20 18:50:58  genevb
// Placement in CVS of automatic TpcFieldCageShort calib codes
//
//

