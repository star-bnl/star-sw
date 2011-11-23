#include <stdio.h>
#include <stdlib.h>

#include "Jevp/StJevpPlot/JevpPlotSet.h"
#include "DAQ_READER/daqReader.h"
#include <TH1F.h>
#include <TH2F.h>
#include <TRandom.h>


#include <math.h>

class fgtBuilder : public JevpPlotSet {
public:
  int run;

  fgtBuilder(); 
  ~fgtBuilder();
  

  void initialize(int argc, char *argv[]);
  void startrun(daqReader *rdr);
  void stoprun(daqReader *rdr);
  void event(daqReader *rdr);
  
  static void main(int argc, char *argv[]);

 private:
  TRandom tRnd;
  int t_2min;
  int t_10min;
  int t_120min;

  //*** Histogram Declarations...
  //*** Use the union to be able to treat in bulk
  //*** As well as by name...
  union {
    TH1 *array[];
    struct {
      TH1 *h2_tst;
    };
  } contents;

  //*** End Histogram Declarations...

  ClassDef(fgtBuilder, 1);
};
