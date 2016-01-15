#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include <TH1F.h>
#include <TH2F.h>

#include <math.h>

class upcBuilder : public JevpBuilder {
public:
  int run;

  upcBuilder(JevpServer *parent=NULL); 
  ~upcBuilder();
  
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
      // UPC_Zdc_HG
      TH1* h_zdce_sum;             // spectrum of ZDCE unattanuated sum
      TH1* h_zdcw_sum;             // spectrum of ZDCW unattanuated sum
      TH1* h_zdce_sum_vs_zdcw_sum;  // ZDCE unattanuated sum vs. ZDCW Unnattenuated sum

      TH1* h_bbce_adc;             // BBC East adc
      TH1* h_bbcw_adc;             // BBC East adc

      TH1* upcTOF_L1mult_vs_ZDCadcsum; //TOF mult vs ZDCadcsum
    };
  } contents;
  
  //*** End Histogram Declarations...
  
  ClassDef(upcBuilder, 1);
};
