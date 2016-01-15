#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include <TH1F.h>
#include <TH2F.h>

#include <math.h>

#define NTRAYS 120

class TLatex;
class TPad;

class tofBuilder : public JevpBuilder {
public:
  int run;

  tofBuilder(JevpServer *parent=NULL); 
  ~tofBuilder();
  
  void initialize(int argc, char *argv[]);
  void startrun(daqReader *rdr);
  void stoprun(daqReader *rdr);
  void event(daqReader *rdr);
  
  static void main(int argc, char *argv[]);

 private:

  // The tof data....
  int tinohit[120][24];
  int allbunchid[2][124];
  int parseData(daqReader *rdr);
  int Get_TOFTHUB(int trayid);
  int TDIGChan2TINOChan(int tdc, int chan);
  int ValidBunchid(int trayid, int halftrayid, int bunchid, int refbunchid);
  bool ValidDataword(int packetid);

  void ReadTrayList();
  void ReadValidBunchidPhase();
  void GetInfoTF00X(int it,int* in);
  void ReadHistConfig();

  float hist_maxtofmult_tof;
  float hist_maxtofmult_trg;

  //*** Histogram Declarations...
  //*** Use the union to be able to treat in bulk
  //*** As well as by name...
  union {
    TH1 *array[];
    struct {

      // TOFL0HistogramGroup
      TH1 *TOF_L0_trg[NTRAYS];

      // TOFL1HistogramGroup
      TH1 *TOF_L1mult_vs_ZDCadcsum;
      TH1 *TOF_L1mult_vs_sumL0;
      TH1 *TOF_L1mult;
      TH1 *TOF_sumL0;
      
      // TOFCheckHistogramGroup
      TH1 *TOF_Error1;
      TH1 *TOF_Error2;
      TH1 *TOF_Error3;
      TH1 *TOF_EventCount;      
      TH1 *TOF_Tray_hits1;
      TH1 *TOF_Tray_hits2;

      // TOFtrayHistogramGroup
      TH1 *TOF_Tray_LEhitmap[NTRAYS];

      // TOFupvpdHistogramGroup
      TH1 *upvpd_hitmap[2];
      TH1 *upvpd_ToT;
      TH1 *upvpd_eastT_vs_westT;
      
      TH2 *hBunchidShiftVSthub;

      TH2 *TOF_TF00X[6];
      
    };
  } contents;
  //*** End Histogram Declarations...

  union {
    TH1 *array[];
    struct {
      TH1 *TOF_Tray_TEhitmap[NTRAYS];
    };
  } extra;


  bool NotActiveTray[128];  // Highest TOF tray  number is MTD: 124, leave some room here. 

  TLatex *TOF_L0_trg_labels[NTRAYS];
  TLatex *TOF_Error1_label;
  TLatex *TOF_Error2_label;
  TLatex *TOF_Error3_label;
  TLatex *TOF_Error1_list;
  TLatex *TOF_Error2_list;
  TLatex *TOF_Error3_list;
  int nperror1, nperror2, nperror3;
  
  int mReferenceTray;
  int mValidShiftTray[2][4];
  int mValidShift121[2][2];
  int mValidShift122[2][2];

  int np;
  JevpPlot *plots[400];

  void ReadTraymaskoutList(); 
  bool MaskoutTray[128];


  double numberforsort;
  vector<double> leadinghits;
  vector<double> trailinghits;

  int tdcchan2upvpdPMTchan(int globaltdcchan, int edgeid,int trayid);

  ClassDef(tofBuilder, 1);
};
