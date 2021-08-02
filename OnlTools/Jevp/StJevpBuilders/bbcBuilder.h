#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include <TH1F.h>
#include <TH2F.h>

#include <math.h>

class bbcBuilder : public JevpBuilder {
 public:
  //RunStatus status;
  int run;

  bbcBuilder(JevpServer *parent=NULL); 
  ~bbcBuilder();
  

  void initialize(int argc, char *argv[]);
  void startrun(daqReader *rdr);
  void stoprun(daqReader *rdr);
  void event(daqReader *rdr);
  
  static void main(int argc, char *argv[]);

 private:

  union {
    TH1 *array[];
    struct {
      TH1 *h190_bbc_hitmap_EAST;
      TH1 *h191_bbc_hitmap_WEST;
      TH1 *h192_bbc_weight_hitmap_EAST;
      TH1 *h193_bbc_weight_hitmap_WEST;

      // h194-197
      TH1 *h194_bbc_hits_east;
      TH1 *h195_bbc_hits_west;
      TH1 *h196_bbcl_hits_east;
      TH1 *h197_bbcl_hits_west;
      TH1 *h198_bbc_AdcSum_east;
      TH1 *h199_bbc_AdcSum_west;
      TH1 *h200_bbcl_AdcSum_east;
      TH1 *h201_bbcl_AdcSum_west;
      // h158-173
      TH1 *bbce_tdc[24];        // 1-24
      // h174-189
      TH1 *bbcw_tdc[24];        // 1-24
      TH1 *bbce_adcmip[24];     // 1-24
      TH1 *bbcw_adcmip[24];     // 1-24
      TH1 *bbce_adcfull[24];    // 1-24
      TH1 *bbcw_adcfull[24];    // 1-24
      TH1 *h202_bbc_earliest_tac_east;
      TH1 *h203_bbc_earliest_tac_west;
      TH1 *h204_bbc_tac_difference_ew;
      TH1 *h452_bbc_tac_difference_ew;
      TH1 *h205_bbc_tac_e_w;
    };
  } contents;

  JevpPlot **plots;

  ClassDef(bbcBuilder, 1);
};
