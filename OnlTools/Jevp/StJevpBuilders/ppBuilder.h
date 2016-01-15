#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include <TH1F.h>
#include <TH2F.h>
#include <TProfile.h>

#include <math.h>

class ppBuilder : public JevpBuilder {
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
  int mEntriesSVX[6][32];
  int mEntriesNCH[6][32];
  //*** Histogram Declarations...
  //*** Use the union to be able to treat in bulk
  //*** As well as by name...

  union {
    TH2 *array[];
    struct {
      TH2* PMT;
      TH2* SVX;
      TH2* VTIM; 
      TH2* SVX_NCH; 

   };
  } contVIP;

  union {
    TH1 *array[];
    struct {
      TH1* h1_P2P[16];
    };
  } contPMT_ADC;

  union {
    TH2 *array[];
    struct {
      TH2* h2_P2P[8];
    };
  } contPMT_TAC;

  union {
    TProfile *array[];
    struct {
      TProfile* hp_P2P[32];
    };
  } contentsSVX;

  TProfile *hitperbunch ;  // added by KY (2015-3-19) 

  //*** End Histogram Declarations...

  ClassDef(ppBuilder, 1);
};
