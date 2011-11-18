#include <stdio.h>
#include <stdlib.h>

#include "Jevp/StJevpPlot/JevpPlotSet.h"
#include "DAQ_READER/daqReader.h"
#include <TH1F.h>
#include <TH2F.h>

#include <math.h>


#define nMTDtrays 3

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
      TH1* MTD26E_hitmap[2][2];
      TH1* MTD26C_hitmap[2][2];
      TH1* MTD26W_hitmap[2][2];
      TH1* MTD1_hitmap[2][2];
      TH2* MTD_ToT;
      TH2* MTD_eastT_vs_westT;
      TH1* MTD_eastT_westT;
      TH2* MTD_hits_vs_TOF_hits;

      // MTDtriggerinfo histogram group
      TH1* MTD_adc[nMTDtrays][2];// 3 trays; 0 east, 1 west
      TH1* MTD_tac[nMTDtrays][2];
    };
  } contents;

  // MTDhits support
  int tdcchan2globalstrip(int,int,int,int);
 
  double numberforsort;
  vector<double> leadinghits;
  vector<double> trailinghits;


  JevpPlot **plots;
  ClassDef(mtdBuilder, 1);
};
