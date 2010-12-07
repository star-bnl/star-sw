#include <stdio.h>
#include <stdlib.h>

#include "Jevp/StJevpPlot/JevpPlotSet.h"
#include "DAQ_READER/daqReader.h"
#include <TH1F.h>
#include <TH2F.h>

#include <math.h>

class ftpBuilder : public JevpPlotSet {
public:
  int run;

  ftpBuilder(); 
  ~ftpBuilder();
  
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
      TH1 *h49_ftp;
      //TH1 *h11_ftp_evsize;    --> daqBuilder
      TH1 *h51_ftp_OccLaser;
      TH1 *h48_ftp_charge;
      TH1 *h50_ftp_OccPulser;
      TH1 *h109_ftp_west_time;
      TH1 *h110_ftp_east_time;
      TH2 *h338_ftp_west;
      TH2 *h339_ftp_east;
    };
  } contents;

  //*** End Histogram Declarations...

  ClassDef(ftpBuilder, 1);
};
