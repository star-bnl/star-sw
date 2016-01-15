#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
//#include "RunStatus.h"

#include <TH1F.h>
#include <TH2F.h>

#include <math.h>

class trgBuilder : public JevpBuilder {
public:
  //RunStatus status;
  int run;
  int first_event;

  trgBuilder(JevpServer *parent=NULL); 
  ~trgBuilder();
  

  void initialize(int argc, char *argv[]);
  void startrun(daqReader *rdr);
  void stoprun(daqReader *rdr);
  void event(daqReader *rdr);
  
  static void main(int argc, char *argv[]);

 private:
  TH1* h76_zdc_time_east;
  TH1* h77_zdc_time_west;
  TH1* h78_zdc_timediff_east_west;
  TH1* h146_zdc_Vertex_cm;
  TH1* h480_zdc_unatt_eastsum;
  TH1* h481_zdc_unatt_westsum;
  

  // Trigger / ZDC_seg
  TH1* h474_zdc_unatt_east1;
  TH1* h475_zdc_unatt_west1;
  TH1* h476_zdc_unatt_east2;
  TH1* h477_zdc_unatt_west2;
  TH1* h478_zdc_unatt_east3;
  TH1* h479_zdc_unatt_west3;

  // Trigger / ZDC sums
  TH1* h482_zdc_sum_bbc;
  TH1* h483_zdc_hardwaresum;
  
  // Trigger / Bunch Crossing Counter
  TH1* h266_bbc_bunchid_y;
  TH1* h266_bbc_bunchid_b;
  
  // bunch crossing sub-histograms...
  TH1* h442_bunch_yellow_fill;
  TH1* h443_bunch_yellow_up;
  TH1* h444_bunch_yellow_down;
  TH1* h445_bunch_yellow_unpol;
  TH1* h446_bunch_blue_fill;
  TH1* h447_bunch_blue_up;
  TH1* h448_bunch_blue_down;
  TH1* h449_bunch_blue_unpol;

  TH1* h329_zdcsmd_w_v_N;
  TH1* h330_zdcsmd_w_h_N;
  TH1* h331_zdcsmd_e_v_N;
  TH1* h332_zdcsmd_e_h_N;
  TH1* h333_zdcsmd_w_v_A;
  TH1* h334_zdcsmd_w_h_A;
  TH1* h335_zdcsmd_e_v_A;
  TH1* h336_zdcsmd_e_h_A;

  // L2UpsilonCounts...
  TH1* hL2ups_Tag;
  TH1* hL2ups_Time;
  TH1* hL2ups_Event;
  TH1* hL2ups_NumberOfHotTowers;
  TH1* hL2ups_AbortRate;
  TH1* hL2ups_AbortRateCurrent;

  int mNumberOfHotTowers;
  int mHotTowerChanges;

  // L2UpsilonMass...
  TH1* hL2ups_EnergyL0;
  TH1* hL2ups_EnergyL2;
  TH1* hL2ups_Mass;
  TH1* hL2ups_CosTheta;

  TH1* hL2ups_TriggerTowerIdL0;
  TH1* hL2ups_TriggerTowerIdL2;
  TH1* hL2ups_NumberOfTowersL0;
  TH1* hL2ups_NumberOfTowersL2;
  TH2* hL2ups_EtaPhiL0;
  TH2* hL2ups_EtaPhiL2;

  ClassDef(trgBuilder, 1);
};
