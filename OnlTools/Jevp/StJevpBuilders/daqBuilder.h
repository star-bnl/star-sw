#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
class daqReader;
#include <TH1F.h>
#include <TH2F.h>
#include <math.h>

// 6/3/21 12:34
// 
class daqBuilder : public JevpBuilder {
public:
  int run;

  daqBuilder(JevpServer *parent=NULL); 
  ~daqBuilder();
  

  void initialize(int argc, char *argv[]);
  void startrun(daqReader *rdr);
  void stoprun(daqReader *rdr);
  void event(daqReader *rdr);
  
  static void main(int argc, char *argv[]);

 private:

  int t_2min;
  int t_10min;
  int t_120min;

  //*** Histogram Declarations...
  //*** Use the union to be able to treat in bulk
  //*** As well as by name...
  union {
    TH1 *array[1];
    struct {
      TH1 *h2_tpc;
      TH1 *h2_itpc;
      TH1 *h2_tpx;
      TH1 *h0_evt_size;
      //TH1 *h10_bemc_evsize;
      //TH1 *h11_ftp_evsize;
      //TH1 *h12_l3_evsize;
      //TH1 *h13_svt_evsize;
      //TH1 *h14_tof_evsize;
      TH1 *h103_tpc_frac;
      TH1 *h106_bemc_frac;
      //TH1 *h105_ftp_frac;
      //TH1 *h108_l3_frac;
      //TH1 *h104_svt_frac;
      TH1 *h107_tof_frac;
      TH1 *h155_time_size_2min;
      TH1 *h156_time_size_10min;
      TH1 *h157_time_size_2hour;
      //TH1 *h337_ftp_time_size_2hour;
    };
  } contents;

  //*** End Histogram Declarations...

  ClassDef(daqBuilder, 1);
};
