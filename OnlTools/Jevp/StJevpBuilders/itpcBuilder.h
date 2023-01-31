#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
class daqReader;
#include <TH1F.h>
#include <TH2F.h>

#include <math.h>

class LaserReader;

class itpcBuilder : public JevpBuilder {
 public:
  int run;
  int nlasers;
  double drift_vel;

  itpcBuilder(JevpServer *parent=NULL); 
  ~itpcBuilder();
  

  void initialize(int argc, char *argv[]);
  void startrun(daqReader *rdr);
  void event(daqReader *rdr);
  
  static void main(int argc, char *argv[]);

 private:

  int n_cld;
  int n_adc;
  int event_no;
  //*** Histogram Declarations...
  //*** Use the union to be able to treat in bulk
  //*** As well as by name...
  union {
    TH1 *array[1];
    struct {
      //TH1 *itpc_occ_physics;
      //TH1 *h44_itpc_occ_laser;
      //TH1 *h43_itpc_occ_pulser;
      TH1 *itpc_pix_occ_physics;
      TH1 *itpc_pix_occ_laser;
      TH1 *itpc_pix_occ_pulser;
      TH1 *h15_itpc_sec1;
      TH1 *h16_itpc_sec2;
      TH1 *h17_itpc_sec3;
      TH1 *h18_itpc_sec4;
      TH1 *h19_itpc_sec5;
      TH1 *h20_itpc_sec6;
      TH1 *h21_itpc_sec7;
      TH1 *h22_itpc_sec8;
      TH1 *h23_itpc_sec9;
      TH1 *h24_itpc_sec10;
      TH1 *h25_itpc_sec11;
      TH1 *h26_itpc_sec12;
      TH1 *h27_itpc_sec13;
      TH1 *h28_itpc_sec14;
      TH1 *h29_itpc_sec15;
      TH1 *h30_itpc_sec16;
      TH1 *h31_itpc_sec17;
      TH1 *h32_itpc_sec18;
      TH1 *h33_itpc_sec19;
      TH1 *h34_itpc_sec20;
      TH1 *h35_itpc_sec21;
      TH1 *h36_itpc_sec22;
      TH1 *h37_itpc_sec23;
      TH1 *h38_itpc_sec24;

      TH1 *e_itpc_sec1;
      TH1 *e_itpc_sec2;
      TH1 *e_itpc_sec3;
      TH1 *e_itpc_sec4;
      TH1 *e_itpc_sec5;
      TH1 *e_itpc_sec6;
      TH1 *e_itpc_sec7;
      TH1 *e_itpc_sec8;
      TH1 *e_itpc_sec9;
      TH1 *e_itpc_sec10;
      TH1 *e_itpc_sec11;
      TH1 *e_itpc_sec12;
      TH1 *e_itpc_sec13;
      TH1 *e_itpc_sec14;
      TH1 *e_itpc_sec15;
      TH1 *e_itpc_sec16;
      TH1 *e_itpc_sec17;
      TH1 *e_itpc_sec18;
      TH1 *e_itpc_sec19;
      TH1 *e_itpc_sec20;
      TH1 *e_itpc_sec21;
      TH1 *e_itpc_sec22;
      TH1 *e_itpc_sec23;
      TH1 *e_itpc_sec24;

      TH1 *h120_itpc_chargeStep_s1;
      TH1 *h121_itpc_chargeStep_s2;
      TH1 *h122_itpc_chargeStep_s3;
      TH1 *h123_itpc_chargeStep_s4;
      TH1 *h124_itpc_chargeStep_s5;
      TH1 *h125_itpc_chargeStep_s6;
      TH1 *h126_itpc_chargeStep_s7;
      TH1 *h127_itpc_chargeStep_s8;
      TH1 *h128_itpc_chargeStep_s9;
      TH1 *h129_itpc_chargeStep_s10;
      TH1 *h130_itpc_chargeStep_s11;
      TH1 *h131_itpc_chargeStep_s12;
      TH1 *h132_itpc_chargeStep_s13;
      TH1 *h133_itpc_chargeStep_s14;
      TH1 *h134_itpc_chargeStep_s15;
      TH1 *h135_itpc_chargeStep_s16;
      TH1 *h136_itpc_chargeStep_s17;
      TH1 *h137_itpc_chargeStep_s18;
      TH1 *h138_itpc_chargeStep_s19;
      TH1 *h139_itpc_chargeStep_s20;
      TH1 *h140_itpc_chargeStep_s21;
      TH1 *h141_itpc_chargeStep_s22;
      TH1 *h142_itpc_chargeStep_s23;
      TH1 *h143_itpc_chargeStep_s24;

      TH1 *h102_itpc_drift_vel;
      //TH1 *h113_itpc_drift_vel_dist;
      TH1 *h66_itpc_phi_charge;
      TH1 *h67_itpc_sector_charge;

      TH1 *cl_width_tb;
      TH1 *cl_width_pad;

    };
  } contents;

 
  // These are the cluster based versions of above...
  union {
    TH1 *array[1];
    struct {
      TH1 *itpc_clpix_occ_physics;
      TH1 *itpc_clpix_occ_laser;
      TH1 *itpc_clpix_occ_pulser;
      TH1 *cl120_itpc_chargeStep_s1;
      TH1 *cl121_itpc_chargeStep_s2;
      TH1 *cl122_itpc_chargeStep_s3;
      TH1 *cl123_itpc_chargeStep_s4;
      TH1 *cl124_itpc_chargeStep_s5;
      TH1 *cl125_itpc_chargeStep_s6;
      TH1 *cl126_itpc_chargeStep_s7;
      TH1 *cl127_itpc_chargeStep_s8;
      TH1 *cl128_itpc_chargeStep_s9;
      TH1 *cl129_itpc_chargeStep_s10;
      TH1 *cl130_itpc_chargeStep_s11;
      TH1 *cl131_itpc_chargeStep_s12;
      TH1 *cl132_itpc_chargeStep_s13;
      TH1 *cl133_itpc_chargeStep_s14;
      TH1 *cl134_itpc_chargeStep_s15;
      TH1 *cl135_itpc_chargeStep_s16;
      TH1 *cl136_itpc_chargeStep_s17;
      TH1 *cl137_itpc_chargeStep_s18;
      TH1 *cl138_itpc_chargeStep_s19;
      TH1 *cl139_itpc_chargeStep_s20;
      TH1 *cl140_itpc_chargeStep_s21;
      TH1 *cl141_itpc_chargeStep_s22;
      TH1 *cl142_itpc_chargeStep_s23;
      TH1 *cl143_itpc_chargeStep_s24;

      TH1 *cl66_itpc_phi_charge;
      TH1 *cl67_itpc_sector_charge;

    };
  } extras;

  //*** End Histogram Declarations...
    
  void  setPhiAngleMap();
  float mPhiAngleMap[24][45][182];
  LaserReader *laserReader;

  ClassDef(itpcBuilder, 1);
};
