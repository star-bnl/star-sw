#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include <DAQ_READER/daq_dta.h> //xue
#include <DAQ_L3/daq_l3.h> //xue
#include <TStyle.h> //xue
#include "TVector3.h"//xue
#include "TFile.h"//xue
#include <fstream>//xue
#include <iostream>//xue
#include <iomanip>//xue
#include "TLorentzVector.h"//xue
#include "TRandom.h"//xue
#include "TRandom3.h"//xue
#include "string"
#include "TObjArray.h"
#include "TObjString.h"

#include <TH1I.h>
#include <TH1D.h>
#include <TH2F.h>
#include <TProfile.h>
#include <TCanvas.h>
#include <TH3D.h>
#include <math.h>
#include <TF1.h>
//-----Run10 AuAu new Format
#include <DAQ_HLT/daq_hlt.h>
#include "RTS/include/HLT/HLTFormats.h"
//-----added by xueliang

using namespace std;

class HltphiBuilder : public JevpBuilder {
public:

  unsigned int current_day;

  double twopi;
  double pi;
  double res2;
  double res2error;
  char Currentrun[256];
  char label[60];
  string str[60];

  TObjArray *flowphi;
  TH1D *TempFlowPhi;
  TH1D *TempFlowPhiWgt;
//  TProfile *TempCosRes;

  TH3D *InvMassv2;
  TH3D *InvMassv2_Sin;
  TH3D *DenInvMass;

  JevpPlot *run[46];
  JevpPlot *day[10];
  JevpPlot *all[31];

  HltphiBuilder(JevpServer *parent=NULL) : JevpBuilder(parent) {
    plotsetname = (char *)"hltp";
  }

  void WriteHistogram(char *outFile);    //kinds of histograms of current run
  void TempPhiWgtCal();//open phiwgt of last run
  void WriteList(char *outFile);
  void Jpsiflow();
  double chi(double res);
  double resEventPlane(double chi);
  int nbin;

  void initialize(int argc, char *argv[]);
  void startrun(daqReader *rdr);
  void stoprun(daqReader *rdr);
  void event(daqReader *rdr);
  int selectEvent(daqReader *rdr);
  int selectRun(daqReader *rdr);
  static void main(int argc, char *argv[]);

  ClassDef(HltphiBuilder, 1);
};

