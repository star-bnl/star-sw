#include <stdio.h>
#include <stdlib.h>

#include "Jevp/StJevpPlot/JevpPlotSet.h"
#include "DAQ_READER/daqReader.h"
#include <TH1F.h>
#include <TH2F.h>

#include <math.h>

class ppBuilder : public JevpPlotSet {
public:
  int run;

  ppBuilder(JevpServer *parent=NULL); 
  ~ppBuilder();
  
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
      TH1 * h_P2P[32];
    };
  } contents;

  //*** End Histogram Declarations...

  ClassDef(ppBuilder, 1);
};
