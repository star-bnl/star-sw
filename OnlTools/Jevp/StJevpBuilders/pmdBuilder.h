#include <stdio.h>
#include <stdlib.h>

#include "Jevp/StJevpPlot/JevpPlotSet.h"
#include "DAQ_READER/daqReader.h"
#include <TH1F.h>
#include <TH2F.h>

#include <math.h>

class pmdBuilder : public JevpPlotSet {
public:
  int run;

  pmdBuilder(); 
  ~pmdBuilder();
  
  void initialize(int argc, char *argv[]);
  void startrun(daqReader *rdr);
  void stoprun(daqReader *rdr);
  void event(daqReader *rdr);
  
  static void main(int argc, char *argv[]);

 private:

  //*** Histogram Declarations...
  //*** Use the union to be able to treat in bulk
  //*** As well as by name...
  union {
    TH1 *array[];
    struct {
      //TH1 *h0_evt_size;       --> daqBuilder
      TH1 *pmd_chains[48];
      TH1 *h430_pmd_Chain_vs_channel;
      TH1 *h431_pmd_Chain_vs_channel_adc;
     };
  } contents;

  //*** End Histogram Declarations...

  ClassDef(pmdBuilder, 1);
};
