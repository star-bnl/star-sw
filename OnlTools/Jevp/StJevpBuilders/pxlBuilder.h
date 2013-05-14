#include <stdio.h>
#include <stdlib.h>

#include "Jevp/StJevpPlot/JevpPlotSet.h"
#include "DAQ_READER/daqReader.h"
#include <TH1F.h>
#include <TH2F.h>

#include <math.h>

class pxlBuilder : public JevpPlotSet {
public:
  int run;

  pxlBuilder(JevpServer *parent=NULL); 
  ~pxlBuilder();
  

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
      TH1 *myhisto;
    };
  } contents;

  //*** End Histogram Declarations...

  ClassDef(pxlBuilder, 1);
};
