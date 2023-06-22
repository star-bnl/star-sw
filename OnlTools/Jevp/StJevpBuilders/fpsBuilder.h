#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
class daqReader;
//#include "DAQ_READER/daq_dta.h"
struct daq_dta;
#include <TH1F.h>
#include <TH2F.h>

#include <math.h>

class fpsBuilder : public JevpBuilder {
public:
  int run;

  fpsBuilder(JevpServer *parent=NULL); 
  ~fpsBuilder();
  
  void initialize(int argc, char *argv[]);
  void startrun(daqReader *rdr);
  void stoprun(daqReader *rdr);
  void event(daqReader *rdr);
  
  static void main(int argc, char *argv[]);

 private:

  daq_dta *dd;
  int t_2min;
  int pre_trig;
  int evt;

  //*** Histogram Declarations...
  //*** Use the union to be able to treat in bulk
  //*** As well as by name...
  union {
    TH1 *array[1];
    struct {
      TH1 *h0_evt_size;
      TH1 *h1_fps_size;
      TH1 *h155_time_size_2min;

      TH1 *h10_multi1;
      TH1 *h11_multi2;
      TH1 *h12_multi3;
      TH2 *h13_ch_rdo;
      TH2 *h20_ch_adc;
      TH2 *h21_ch_adc_full;
      TH1 *h30_adc1;
      TH1 *h31_adc2;
      TH1 *h32_adc3;
      TH1 *h33_adc1_full;
      TH1 *h34_adc2_full;
      TH1 *h35_adc3_full;
      TH2 *h40_hits12;
      TH1 *h50_rcc;

      TH1 *hh10_multi1;
      TH1 *hh11_multi2;
      TH1 *hh12_multi3;
      TH2 *hh13_ch_rdo;
      TH2 *hh20_ch_adc;
      TH2 *hh21_ch_adc_full;
      TH1 *hh30_adc1;
      TH1 *hh31_adc2;
      TH1 *hh32_adc3;
      TH1 *hh33_adc1_full;
      TH1 *hh34_adc2_full;
      TH1 *hh35_adc3_full;
      TH1 *hh50_rcc;
    };
  } contents;

  //*** End Histogram Declarations...

  Float_t xypos[23];

  ClassDef(fpsBuilder, 1);
};
