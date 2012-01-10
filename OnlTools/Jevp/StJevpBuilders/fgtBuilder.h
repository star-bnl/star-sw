#include <stdio.h>
#include <stdlib.h>

#include "Jevp/StJevpPlot/JevpPlotSet.h"
#include "DAQ_READER/daqReader.h"
#include <TH1F.h>
#include <TH2F.h>
#include <TRandom.h>
#include "DAQ_READER/daq_dta.h"

#include <math.h>

class fgtBuilder : public JevpPlotSet {
public:
  int run;

  fgtBuilder(JevpServer *parent=NULL); 
  ~fgtBuilder();
  

  void initialize(int argc, char *argv[]);
  void startrun(daqReader *rdr);
  void stoprun(daqReader *rdr);
  void event(daqReader *rdr);
  
  static void main(int argc, char *argv[]);

 private:
  TRandom tRnd;
  int evtCt;
  int t_2min;
  int t_10min;
  int t_120min;



  string Gid2Label[]={"1AB","1BC","1CD","1DA","2AB","2BC","2DA","3AB","3BC","3DA","4AB","4BC","4DA","5AB","5BC","5DA","6AB","6BC","6DA"};
  //the gid encodes rdo etc. but since this is consecutive, we have to have another mapping
  int Indx2Gid[]={1,10,11,0,3,12,2,4,5,13,15,6,14,16,17,7,9,18,8};


  //*** Histogram Declarations...
  //*** Use the union to be able to treat in bulk
  //*** As well as by name...
  union {
    TH2 *array[];
    struct {
      TH2* q1;
      TH2* q2;
      TH2* q3;
      TH2* q4;
      TH2* q5;
      TH2* q6;
      TH2* q7;
      TH2* q8;
      TH2* q9;
      TH2* q10;
      TH2* q11;
      TH2* q12;
      TH2* q13;
      TH2* q14;
      TH2* q15;
      TH2* q16;
      TH2* q17;
      TH2* q18;
      TH2* q19;
    };
  } contents;
  daq_dta *dd;
  //*** End Histogram Declarations...

  ClassDef(fgtBuilder, 1);
};
