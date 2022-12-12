#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
class daqReader;
#include <TH1F.h>
#include <TH2F.h>

#include <math.h>

class fpdBuilder : public JevpBuilder {
 public:
  //RunStatus status;
  int run;

  fpdBuilder(JevpServer *parent=NULL); 
  ~fpdBuilder();
  

  void initialize(int argc, char *argv[]);
  void startrun(daqReader *rdr);
  void stoprun(daqReader *rdr);
  void event(daqReader *rdr);
  
  static void main(int argc, char *argv[]);

 private:

  union {
    TH1 *array[1];
    struct {
      TH1 *h206_fpd_EN_adcsum;
      TH1 *h207_fpd_ES_adcsum;
      TH1 *h208_fpd_ET_adcsum;
      TH1 *h209_fpd_EB_adcsum;
      TH1 *h210_fpd_WN_adcsum;
      TH1 *h211_fpd_WS_adcsum;
      TH1 *h212_fpd_WT_adcsum;
      TH1 *h213_fpd_WB_adcsum;
      TH1 *h214_fpd_EN_hitmap;
      TH1 *h215_fpd_ES_hitmap;
      TH1 *h216_fpd_ET_hitmap;
      TH1 *h217_fpd_EB_hitmap;
      TH1 *h218_fpd_WN_hitmap;
      TH1 *h219_fpd_WS_hitmap;
      TH1 *h220_fpd_WT_hitmap;
      TH1 *h221_fpd_WB_hitmap;
      TH1 *h222_fpd_EN_weighted_hitmap;
      TH1 *h223_fpd_ES_weighted_hitmap;
      TH1 *h224_fpd_ET_weighted_hitmap;
      TH1 *h225_fpd_EB_weighted_hitmap;
      TH1 *h226_fpd_WN_weighted_hitmap;
      TH1 *h227_fpd_WS_weighted_hitmap;
      TH1 *h228_fpd_WT_weighted_hitmap;
      TH1 *h229_fpd_WB_weighted_hitmap;
    };
  } contents;

  JevpPlot **plots;

  ClassDef(fpdBuilder, 1);
};
