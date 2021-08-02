#ifndef _MTDBUILDER_H
#define _MTDBUILDER_H

#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include <TH1F.h>
#include <TH2F.h>

#include <math.h>

#define nMTDtrays 30
#define nMTDtrig 128

class mtdBuilder : public JevpBuilder {
 public:

  //RunStatus status;
  int run;

  mtdBuilder(JevpServer *parent=NULL); 
  ~mtdBuilder();
  
  void initialize(int argc, char *argv[]);
  void startrun(daqReader *rdr);
  void stoprun(daqReader *rdr);
  void event(daqReader *rdr);
  
  static void main(int argc, char *argv[]);

 private:

  union {
    TH1 *array[];
    struct {
      // MTDhits histogram group
      TH2* hMTD_hitmap2D;
      TH2* hMTD_hitmap2D_good;
      TH2* hMTD_timeDiff;
      TH1*** hMTD_hitmap;

      // MTDtriggerinfo histogram group
      TH2* hMTD_trig2D;
      TH2* hMTD_trig2D_adc;
      TH2* hMTD_trig2D_tac;
      TH1** hMTD_trig;
      TH2* MTD_bunchid;
      TH1* MTD_EventCount;
      TH1* MTD_Error1;
      TH1* MTD_Error2;
      TH1* MTD_Error3;
      TH1* MTD_Tray_hits;
      TH1* MTD_Tray_hitsEvenOdd;
      TH1* MTD_Tray_hitsBinEven;
    };
  } contents;

  void ReadValidBunchidPhase();
  int  SetMtdQTmap();
  int  mReferenceTray;
  int  mValidShiftTray[2][2];	// index1=value, index2=RDO

  void ReadTraymaskoutList(); 
  bool MaskoutTray[30];
  
  int tdcchan2globalstrip(int,int,int);
  int tdig2slot(int, int);
  int istray3bl(int);
  int istray5bl(int);
  bool ValidDataword(int);
  int iGlobalSlot(int, int);
 
  //information from the QT map...
  int isADC[nMTDtrig];		// nonzero if ADC, value is channel number
  int isTAC[nMTDtrig];		// nonzero if TAC, value is channel number
  TString QTslot[128];
  TString QTslothex[128];
  TString QTboard[128];
  TString QTchanname[128];
  TString QTchanno[128];
  TString QTchanstring[128];
  TString QTcable[128];
  TString QTtpcsector[128];
 
  double numberforsort;
  vector<double> leadinghits;
  vector<double> trailinghits;
  TLatex *MTD_Error1_label;
  TLatex *MTD_Error2_label;
  TLatex *MTD_Error3_label;
  TLatex *MTD_BL_label[30];		// Run-13 15 trays

  JevpPlot **plots;
  ClassDef(mtdBuilder, 1);
  
};

#endif
