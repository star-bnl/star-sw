#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
class daqReader;
#include <TH1F.h>
#include <TH2F.h>

#include <math.h>

class LaserReader;

class tpcBuilder : public JevpBuilder {
 public:
  int run;
  int nlasers;
  double drift_vel;

  int tpcDataInThisRun;

  tpcBuilder(JevpServer *parent=NULL); 
  ~tpcBuilder();
  

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
      TH1 *itpc_pix_occ_physics;
      TH1 *itpc_pix_occ_laser;
      TH1 *itpc_pix_occ_pulser;
 
      TH1 *tpc_pix_occ_physics;
      TH1 *tpc_pix_occ_laser;
      TH1 *tpc_pix_occ_pulser;


      TH1 *h_tpc_sec1;
      TH1 *h_tpc_sec2;
      TH1 *h_tpc_sec3;
      TH1 *h_tpc_sec4;
      TH1 *h_tpc_sec5;
      TH1 *h_tpc_sec6;
      TH1 *h_tpc_sec7;
      TH1 *h_tpc_sec8;
      TH1 *h_tpc_sec9;
      TH1 *h_tpc_sec10;
      TH1 *h_tpc_sec11;
      TH1 *h_tpc_sec12;
      TH1 *h_tpc_sec13;
      TH1 *h_tpc_sec14;
      TH1 *h_tpc_sec15;
      TH1 *h_tpc_sec16;
      TH1 *h_tpc_sec17;
      TH1 *h_tpc_sec18;
      TH1 *h_tpc_sec19;
      TH1 *h_tpc_sec20;
      TH1 *h_tpc_sec21;
      TH1 *h_tpc_sec22;
      TH1 *h_tpc_sec23;
      TH1 *h_tpc_sec24;

      TH1 *hcl_tpc_sec1;
      TH1 *hcl_tpc_sec2;
      TH1 *hcl_tpc_sec3;
      TH1 *hcl_tpc_sec4;
      TH1 *hcl_tpc_sec5;
      TH1 *hcl_tpc_sec6;
      TH1 *hcl_tpc_sec7;
      TH1 *hcl_tpc_sec8;
      TH1 *hcl_tpc_sec9;
      TH1 *hcl_tpc_sec10;
      TH1 *hcl_tpc_sec11;
      TH1 *hcl_tpc_sec12;
      TH1 *hcl_tpc_sec13;
      TH1 *hcl_tpc_sec14;
      TH1 *hcl_tpc_sec15;
      TH1 *hcl_tpc_sec16;
      TH1 *hcl_tpc_sec17;
      TH1 *hcl_tpc_sec18;
      TH1 *hcl_tpc_sec19;
      TH1 *hcl_tpc_sec20;
      TH1 *hcl_tpc_sec21;
      TH1 *hcl_tpc_sec22;
      TH1 *hcl_tpc_sec23;
      TH1 *hcl_tpc_sec24;

      TH1 *e_tpc_sec1;
      TH1 *e_tpc_sec2;
      TH1 *e_tpc_sec3;
      TH1 *e_tpc_sec4;
      TH1 *e_tpc_sec5;
      TH1 *e_tpc_sec6;
      TH1 *e_tpc_sec7;
      TH1 *e_tpc_sec8;
      TH1 *e_tpc_sec9;
      TH1 *e_tpc_sec10;
      TH1 *e_tpc_sec11;
      TH1 *e_tpc_sec12;
      TH1 *e_tpc_sec13;
      TH1 *e_tpc_sec14;
      TH1 *e_tpc_sec15;
      TH1 *e_tpc_sec16;
      TH1 *e_tpc_sec17;
      TH1 *e_tpc_sec18;
      TH1 *e_tpc_sec19;
      TH1 *e_tpc_sec20;
      TH1 *e_tpc_sec21;
      TH1 *e_tpc_sec22;
      TH1 *e_tpc_sec23;
      TH1 *e_tpc_sec24;

      TH1 *h_itpc_chargeStep_s1;
      TH1 *h_itpc_chargeStep_s2;
      TH1 *h_itpc_chargeStep_s3;
      TH1 *h_itpc_chargeStep_s4;
      TH1 *h_itpc_chargeStep_s5;
      TH1 *h_itpc_chargeStep_s6;
      TH1 *h_itpc_chargeStep_s7;
      TH1 *h_itpc_chargeStep_s8;
      TH1 *h_itpc_chargeStep_s9;
      TH1 *h_itpc_chargeStep_s10;
      TH1 *h_itpc_chargeStep_s11;
      TH1 *h_itpc_chargeStep_s12;
      TH1 *h_itpc_chargeStep_s13;
      TH1 *h_itpc_chargeStep_s14;
      TH1 *h_itpc_chargeStep_s15;
      TH1 *h_itpc_chargeStep_s16;
      TH1 *h_itpc_chargeStep_s17;
      TH1 *h_itpc_chargeStep_s18;
      TH1 *h_itpc_chargeStep_s19;
      TH1 *h_itpc_chargeStep_s20;
      TH1 *h_itpc_chargeStep_s21;
      TH1 *h_itpc_chargeStep_s22;
      TH1 *h_itpc_chargeStep_s23;
      TH1 *h_itpc_chargeStep_s24;

      TH1 *h_tpx_chargeStep_s1;
      TH1 *h_tpx_chargeStep_s2;
      TH1 *h_tpx_chargeStep_s3;
      TH1 *h_tpx_chargeStep_s4;
      TH1 *h_tpx_chargeStep_s5;
      TH1 *h_tpx_chargeStep_s6;
      TH1 *h_tpx_chargeStep_s7;
      TH1 *h_tpx_chargeStep_s8;
      TH1 *h_tpx_chargeStep_s9;
      TH1 *h_tpx_chargeStep_s10;
      TH1 *h_tpx_chargeStep_s11;
      TH1 *h_tpx_chargeStep_s12;
      TH1 *h_tpx_chargeStep_s13;
      TH1 *h_tpx_chargeStep_s14;
      TH1 *h_tpx_chargeStep_s15;
      TH1 *h_tpx_chargeStep_s16;
      TH1 *h_tpx_chargeStep_s17;
      TH1 *h_tpx_chargeStep_s18;
      TH1 *h_tpx_chargeStep_s19;
      TH1 *h_tpx_chargeStep_s20;
      TH1 *h_tpx_chargeStep_s21;
      TH1 *h_tpx_chargeStep_s22;
      TH1 *h_tpx_chargeStep_s23;
      TH1 *h_tpx_chargeStep_s24;

      TH1 *h_tpc_drift_vel;

      TH1 *h_tpx_phi_charge;
      TH1 *h_tpx_sector_charge;
      TH1 *h_itpc_phi_charge;
      TH1 *h_itpc_sector_charge;

      TH1 *cl_width_itpc_tb;
      TH1 *cl_width_itpc_pad;
      TH1 *cl_width_tpx_tb;
      TH1 *cl_width_tpx_pad;

      TH1 *no_clust_tpx;
      TH1 *no_clust_itpc;

    };
  } contents;

 
  // These are the cluster based versions of above...
  union {
    TH1 *array[1];
    struct {
      TH1 *itpc_clpix_occ_physics;
      TH1 *itpc_clpix_occ_laser;
      TH1 *itpc_clpix_occ_pulser;

      TH1 *tpc_clpix_occ_physics;
      TH1 *tpc_clpix_occ_laser;
      TH1 *tpc_clpix_occ_pulser;

      TH1 *cl_itpc_chargeStep_s1;
      TH1 *cl_itpc_chargeStep_s2;
      TH1 *cl_itpc_chargeStep_s3;
      TH1 *cl_itpc_chargeStep_s4;
      TH1 *cl_itpc_chargeStep_s5;
      TH1 *cl_itpc_chargeStep_s6;
      TH1 *cl_itpc_chargeStep_s7;
      TH1 *cl_itpc_chargeStep_s8;
      TH1 *cl_itpc_chargeStep_s9;
      TH1 *cl_itpc_chargeStep_s10;
      TH1 *cl_itpc_chargeStep_s11;
      TH1 *cl_itpc_chargeStep_s12;
      TH1 *cl_itpc_chargeStep_s13;
      TH1 *cl_itpc_chargeStep_s14;
      TH1 *cl_itpc_chargeStep_s15;
      TH1 *cl_itpc_chargeStep_s16;
      TH1 *cl_itpc_chargeStep_s17;
      TH1 *cl_itpc_chargeStep_s18;
      TH1 *cl_itpc_chargeStep_s19;
      TH1 *cl_itpc_chargeStep_s20;
      TH1 *cl_itpc_chargeStep_s21;
      TH1 *cl_itpc_chargeStep_s22;
      TH1 *cl_itpc_chargeStep_s23;
      TH1 *cl_itpc_chargeStep_s24;

      TH1 *cl_tpx_chargeStep_s1;
      TH1 *cl_tpx_chargeStep_s2;
      TH1 *cl_tpx_chargeStep_s3;
      TH1 *cl_tpx_chargeStep_s4;
      TH1 *cl_tpx_chargeStep_s5;
      TH1 *cl_tpx_chargeStep_s6;
      TH1 *cl_tpx_chargeStep_s7;
      TH1 *cl_tpx_chargeStep_s8;
      TH1 *cl_tpx_chargeStep_s9;
      TH1 *cl_tpx_chargeStep_s10;
      TH1 *cl_tpx_chargeStep_s11;
      TH1 *cl_tpx_chargeStep_s12;
      TH1 *cl_tpx_chargeStep_s13;
      TH1 *cl_tpx_chargeStep_s14;
      TH1 *cl_tpx_chargeStep_s15;
      TH1 *cl_tpx_chargeStep_s16;
      TH1 *cl_tpx_chargeStep_s17;
      TH1 *cl_tpx_chargeStep_s18;
      TH1 *cl_tpx_chargeStep_s19;
      TH1 *cl_tpx_chargeStep_s20;
      TH1 *cl_tpx_chargeStep_s21;
      TH1 *cl_tpx_chargeStep_s22;
      TH1 *cl_tpx_chargeStep_s23;
      TH1 *cl_tpx_chargeStep_s24;

      TH1 *cl_itpc_phi_charge;
      TH1 *cl_itpc_sector_charge;
      TH1 *cl_tpx_phi_charge;
      TH1 *cl_tpx_sector_charge;

      TH1 *clusters_per_bx;
    };
  } extras;

  //*** End Histogram Declarations...
    
  void  setPhiAngleMap();
  float mPhiAngleMap[24][72][144];
  LaserReader *laserReader;

  ClassDef(tpcBuilder, 1);
};
