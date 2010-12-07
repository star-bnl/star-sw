#include <stdio.h>
#include <stdlib.h>

#include "Jevp/StJevpPlot/JevpPlotSet.h"
#include "DAQ_READER/daqReader.h"
#include <TH1F.h>
#include <TH2F.h>

#include <math.h>

class mtdBuilder : public JevpPlotSet {
 public:
  //RunStatus status;
  int run;

  mtdBuilder(); 
  ~mtdBuilder();
  

  void initialize(int argc, char *argv[]);
  void startrun(daqReader *rdr);
  void stoprun(daqReader *rdr);
  void event(daqReader *rdr);
  
  static void main(int argc, char *argv[]);

 private:

  union {
    TH1 *array[];
    struct {
      // MTDhits histogram group
      TH1 *MTD_hitmap[2];
      TH1 *MTD_ToT;
      TH1 *MTD_eastT_vs_westT;
      TH1 *MTD_eastT_westT;
      TH1 *MTD_hits_vs_TOF_hits;

      // MTDtriggerinfo histogram group
      TH1 *MTD_adc[2];
      TH1 *MTD_tac[2];
      TH1 *MTD_eastTac_vs_westTac;
      TH1 *MTD_aveTac_vs_vpd_aveTac;
      

    };
  } contents;

  vector<double> leadinghits;
  vector<double> trailinghits;

  JevpPlot **plots;

  int tdcchan2MTDchan(int,int);
  int tdcchan2mrpcchan(int);

  ClassDef(mtdBuilder, 1);
};
