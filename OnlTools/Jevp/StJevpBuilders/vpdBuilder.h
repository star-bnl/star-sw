#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
//#include "RunStatus.h"

#include "StEvent/StTriggerData2016.h"

#include <TH1I.h>
#include <TH2F.h>

#include <math.h>


// This is the one PlotSet that is guarenteed to always exist
// It's main purpose is to provide the run information 
// To the server...
//
// It has no plots (currently)
//


class vpdBuilder : public JevpBuilder {
 public:
 
  vpdBuilder(JevpServer *parent=NULL) : JevpBuilder(parent) {
    plotsetname = (char *)"vpd";
    memset(&contents, 0, sizeof(contents));
  }

  ~vpdBuilder() {
    int n = sizeof(contents) / sizeof(TH1 *);
    for(int i=0;i<n;i++) {
      if(contents.array[i]) delete contents.array[i];
    }
  }

  void ReadConfig();
    
    
  int pulserCh( int ich ) {
    return (ich / 4) * 4;
  }

  int pulserChToIndex( int pCh ){
    return pCh / 4;
  }

  static const int ADC_th = 10, TDC_min = 180, TDC_max = 3000;

  bool goodHit( int adc, int tdc ){
    if ( adc <= ADC_th || tdc <= TDC_min || tdc >= TDC_max )
      return false;
    return true;
  }

  int correctedTAC( StTriggerData2016 * td, int side, int channel );

  vector<int> expected_pulser_means_east; //= { 1054, 1328, 1294, 1040 };
  vector<int> expected_pulser_means_west; //= { 1422, 1317, 1233, 1166};
  vector<bool> eastGoodCh;
  vector<bool> westGoodCh;

  int refChannelEast;
  int refChannelWest;

  bool pulserSwitch;
  bool noiseCorr;

  // Histo declarations!
  union {
    TH1 *array[];
    struct {
      TH2 *cdb[4];
      TH2 *tac_east_vs_tac_west;
      //   TH2 *vertex_vs_l3_vertex;
      // TH2 *earliestTAC_vs_eastchan;
      // TH2 *earliestTAC_vs_westchan;
      
      TH2 *hi_cdb[4];
      TH2 *hi_tac_east_vs_tac_west;
      //  TH2 *hi_vertex_vs_l3_vertex;
      TH2 *hi_earliestTAC_vs_eastchan;
      TH2 *hi_earliestTAC_vs_westchan;

      TH2 *tac_align_east;
      TH2 *tac_align_west;

      TH2 *vtx_east_tacsum_on_vs_off;
      TH2 *vtx_west_tacsum_on_vs_off;

      TH2 *vtx_east_adcsum_on_vs_off;
      TH2 *vtx_west_adcsum_on_vs_off;

      TH1 *pulser_east[4];
      TH1 *pulser_west[4];

      TH1 *vtx_TAC_diff;
    };
  } contents;

  void initialize(int argc, char *argv[]);
  void startrun(daqReader *rdr);
  void stoprun(daqReader *rdr);
  void event(daqReader *rdr);
  
  int eBinEdges[19][10];
  int eCorrs[19][10];

  int wBinEdges[19][10];
  int wCorrs[19][10];

  int numBins;

  vector<int> maskedChannelsEast;
  vector<int> maskedChannelsWest;

  void readParams();
  vector<string> readTokenVector( TString &str );
  vector<int> readIntVector( TString &str, int start = 0 );
  int corrEast( int iCh, int adc, int tac );
  int corrWest( int iCh, int adc, int tac );
  int corrBin( int iCh, int adc, int *bins );

  static void main(int argc, char *argv[]);

  ClassDef(vpdBuilder, 1);
};
