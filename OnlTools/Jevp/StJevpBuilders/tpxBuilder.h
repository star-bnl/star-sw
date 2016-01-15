#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include <TH1F.h>
#include <TH2F.h>

#include <math.h>

class LaserReader;

class tpxBuilder : public JevpBuilder {
 public:
  int run;
  int nlasers;
  double drift_vel;

  tpxBuilder(JevpServer *parent=NULL); 
  ~tpxBuilder();
  

  void initialize(int argc, char *argv[]);
  void startrun(daqReader *rdr);
  void event(daqReader *rdr);
  
  static void main(int argc, char *argv[]);

 private:

  int n_cld;
  int n_adc;

  //*** Histogram Declarations...
  //*** Use the union to be able to treat in bulk
  //*** As well as by name...
  union {
    TH1 *array[];
    struct {
      //TH1 *tpc_occ_physics;
      //TH1 *h44_tpc_occ_laser;
      //TH1 *h43_tpc_occ_pulser;
      TH1 *tpc_pix_occ_physics;
      TH1 *tpc_pix_occ_laser;
      TH1 *tpc_pix_occ_pulser;
      TH1 *h15_tpc_sec1;
      TH1 *h16_tpc_sec2;
      TH1 *h17_tpc_sec3;
      TH1 *h18_tpc_sec4;
      TH1 *h19_tpc_sec5;
      TH1 *h20_tpc_sec6;
      TH1 *h21_tpc_sec7;
      TH1 *h22_tpc_sec8;
      TH1 *h23_tpc_sec9;
      TH1 *h24_tpc_sec10;
      TH1 *h25_tpc_sec11;
      TH1 *h26_tpc_sec12;
      TH1 *h27_tpc_sec13;
      TH1 *h28_tpc_sec14;
      TH1 *h29_tpc_sec15;
      TH1 *h30_tpc_sec16;
      TH1 *h31_tpc_sec17;
      TH1 *h32_tpc_sec18;
      TH1 *h33_tpc_sec19;
      TH1 *h34_tpc_sec20;
      TH1 *h35_tpc_sec21;
      TH1 *h36_tpc_sec22;
      TH1 *h37_tpc_sec23;
      TH1 *h38_tpc_sec24;
      TH1 *h120_chargeStep_s1;
      TH1 *h121_chargeStep_s2;
      TH1 *h122_chargeStep_s3;
      TH1 *h123_chargeStep_s4;
      TH1 *h124_chargeStep_s5;
      TH1 *h125_chargeStep_s6;
      TH1 *h126_chargeStep_s7;
      TH1 *h127_chargeStep_s8;
      TH1 *h128_chargeStep_s9;
      TH1 *h129_chargeStep_s10;
      TH1 *h130_chargeStep_s11;
      TH1 *h131_chargeStep_s12;
      TH1 *h132_chargeStep_s13;
      TH1 *h133_chargeStep_s14;
      TH1 *h134_chargeStep_s15;
      TH1 *h135_chargeStep_s16;
      TH1 *h136_chargeStep_s17;
      TH1 *h137_chargeStep_s18;
      TH1 *h138_chargeStep_s19;
      TH1 *h139_chargeStep_s20;
      TH1 *h140_chargeStep_s21;
      TH1 *h141_chargeStep_s22;
      TH1 *h142_chargeStep_s23;
      TH1 *h143_chargeStep_s24;

      TH1 *h102_tpc_drift_vel;
      //TH1 *h113_tpc_drift_vel_dist;
      TH1 *h66_tpc_phi_charge;
      TH1 *h67_tpc_sector_charge;
    };
  } contents;

 
  // These are the cluster based versions of above...
  union {
    TH1 *array[];
    struct {
      TH1 *tpc_clpix_occ_physics;
      TH1 *tpc_clpix_occ_laser;
      TH1 *tpc_clpix_occ_pulser;
      TH1 *cl120_chargeStep_s1;
      TH1 *cl121_chargeStep_s2;
      TH1 *cl122_chargeStep_s3;
      TH1 *cl123_chargeStep_s4;
      TH1 *cl124_chargeStep_s5;
      TH1 *cl125_chargeStep_s6;
      TH1 *cl126_chargeStep_s7;
      TH1 *cl127_chargeStep_s8;
      TH1 *cl128_chargeStep_s9;
      TH1 *cl129_chargeStep_s10;
      TH1 *cl130_chargeStep_s11;
      TH1 *cl131_chargeStep_s12;
      TH1 *cl132_chargeStep_s13;
      TH1 *cl133_chargeStep_s14;
      TH1 *cl134_chargeStep_s15;
      TH1 *cl135_chargeStep_s16;
      TH1 *cl136_chargeStep_s17;
      TH1 *cl137_chargeStep_s18;
      TH1 *cl138_chargeStep_s19;
      TH1 *cl139_chargeStep_s20;
      TH1 *cl140_chargeStep_s21;
      TH1 *cl141_chargeStep_s22;
      TH1 *cl142_chargeStep_s23;
      TH1 *cl143_chargeStep_s24;

      TH1 *cl66_tpc_phi_charge;
      TH1 *cl67_tpc_sector_charge;
    };
  } extras;

  //*** End Histogram Declarations...
    
  void  setPhiAngleMap();
  float mPhiAngleMap[24][45][182];
  LaserReader *laserReader;

  ClassDef(tpxBuilder, 1);
};
