#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
class daqReader;
//#include "RTS/trg/include/trgDataDefs.h"

//#include "RunStatus.h"

#include <TH1F.h>
#include <TH2F.h>
#include <TProfile.h>

#include <math.h>

#include "RTS/include/daqFormats.h"
struct QtEventInfo {
    double usec;      // total readout time
    double sz;
    double board_occ[32];   // boards are 0x10..0x1f
};
    
class trgBuilder : public JevpBuilder {
public:
  //RunStatus status;
  int run;
  int first_event;

  trgBuilder(JevpServer *parent=NULL); 
  ~trgBuilder();
  
  void fillQtHisto(int conf_num, TriggerDataBlk *trg, TH1D *sz, TH1D *usec, TProfile *board_occ);
  void initialize(int argc, char *argv[]);
  void startrun(daqReader *rdr);
  void stoprun(daqReader *rdr);
  void event(daqReader *rdr);
  
  void handleQTOccupancyPlots(daqReader *rdr);

  static void main(int argc, char *argv[]);

 private:

  union {
      TH1 *array[1];
      
      struct {
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
	  TH2* h337_zdcsmd_w_v_A_2D;
	  TH2* h338_zdcsmd_w_h_A_2D;
	  TH2* h339_zdcsmd_e_v_A_2D;
	  TH2* h340_zdcsmd_e_h_A_2D;
	  TH1* h341_zdcsmd_w_v_A_strip1;
	  TH1* h342_zdcsmd_w_v_A_strip2;
	  TH1* h343_zdcsmd_w_v_A_strip3;
	  TH1* h344_zdcsmd_w_v_A_strip4;
	  TH1* h345_zdcsmd_w_v_A_strip5;
	  TH1* h346_zdcsmd_w_v_A_strip6;
	  TH1* h347_zdcsmd_w_v_A_strip7;
	  TH1* h348_zdcsmd_w_v_A_strip8;
	  TH1* h349_zdcsmd_w_h_A_strip1;
	  TH1* h350_zdcsmd_w_h_A_strip2;
	  TH1* h351_zdcsmd_w_h_A_strip3;
	  TH1* h352_zdcsmd_w_h_A_strip4;
	  TH1* h353_zdcsmd_w_h_A_strip5;
	  TH1* h354_zdcsmd_w_h_A_strip6;
	  TH1* h355_zdcsmd_w_h_A_strip7;
	  TH1* h356_zdcsmd_w_h_A_strip8;
	  TH1* h357_zdcsmd_e_v_A_strip1;
	  TH1* h358_zdcsmd_e_v_A_strip2;
	  TH1* h359_zdcsmd_e_v_A_strip3;
	  TH1* h360_zdcsmd_e_v_A_strip4;
	  TH1* h361_zdcsmd_e_v_A_strip5;
	  TH1* h362_zdcsmd_e_v_A_strip6;
	  TH1* h363_zdcsmd_e_v_A_strip7;
	  TH1* h364_zdcsmd_e_v_A_strip8;
	  TH1* h365_zdcsmd_e_h_A_strip1;
	  TH1* h366_zdcsmd_e_h_A_strip2;
	  TH1* h367_zdcsmd_e_h_A_strip3;
	  TH1* h368_zdcsmd_e_h_A_strip4;
	  TH1* h369_zdcsmd_e_h_A_strip5;
	  TH1* h370_zdcsmd_e_h_A_strip6;
	  TH1* h371_zdcsmd_e_h_A_strip7;
	  TH1* h372_zdcsmd_e_h_A_strip8;

          // L2UpsilonCounts...
	  TH1* hL2ups_Tag;
	  TH1* hL2ups_Time;
	  TH1* hL2ups_Event;
	  TH1* hL2ups_NumberOfHotTowers;
	  TH1* hL2ups_AbortRate;
	  TH1* hL2ups_AbortRateCurrent;
	  
	  
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
	  
	  TH1* qt1_sz_h;
	  TH1* qt1_board_occ_h;
	  TH1* qt1_readout_time_h;
	  
	  TH1* qt2_sz_h;
	  TH1* qt2_board_occ_h;
	  TH1* qt2_readout_time_h;
	  
	  TH1* qt3_sz_h;
	  TH1* qt3_board_occ_h;
	  TH1* qt3_readout_time_h;
	  
	  TH1* qt4_sz_h;
	  TH1* qt4_board_occ_h;
	  TH1* qt4_readout_time_h;
	  
	  TH1* mxq_sz_h;
	  TH1* mxq_board_occ_h;
	  TH1* mxq_readout_time_h;
	  
	  TH1* bbq_sz_h;
	  TH1* bbq_board_occ_h;
	  TH1* bbq_readout_time_h;
	  
	  TH1* eq1_sz_h;
	  TH1* eq1_board_occ_h;
	  TH1* eq1_readout_time_h;
	
	  TH1* eq2_sz_h;
	  TH1* eq2_board_occ_h;
	  TH1* eq2_readout_time_h;
	
	  TH1* eq3_sz_h;
	  TH1* eq3_board_occ_h;
	  TH1* eq3_readout_time_h;


      };
  } contents;
  
  int mNumberOfHotTowers;
  int mHotTowerChanges;
      
  ClassDef(trgBuilder, 1);
};
