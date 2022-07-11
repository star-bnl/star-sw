#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
class daqReader;
#include <TH1F.h>
#include <TH2F.h>
#include <TProfile2D.h>

#include <math.h>



class epdBuilder : public JevpBuilder {
  public:
    //RunStatus status;
    int run;

    epdBuilder(JevpServer *parent=NULL); 
    ~epdBuilder();

    void initialize(int argc, char *argv[]);
    void startrun(daqReader *rdr);
    void stoprun(daqReader *rdr);
    void event(daqReader *rdr);

    static void main(int argc, char *argv[]);

  private:
    int disable_builder;

    union {
      TH1 *array[1];
      struct {
	TH2 *hDummyAdc[2]; // Dummy plot to hold the polar plot, without this polar cdt won't work
	TProfile2D *hPolPlotAdc[2][2]; // 1st index for east or west, 2nd index: 0 for 1st ring, 1 for rest

	TH2 *hDummyTac[2]; 
	TProfile2D *hPolPlotTac[2][2]; 

	TH2 *hDummyHit[2]; 
	TH2D *hPolPlotHit[2][2]; 


	TH1 *hADC[2][12][32];
	TH1 *hTAC[2][12][10];

	TH2 *hEarliestTacEvsW;
	TH1 *hEarliestTacEast;
	TH1 *hEarliestTacWest;
	TH1 *hTacDiff;
	TH1 *hHitCountEast;
	TH1 *hHitCountWest;

      };
    } contents;

    JevpPlot **plots;




    struct EPDAnalysisMap {
      Short_t qt_crate_adc;
      Short_t qt_board_adc;
      Short_t qt_channel_adc;

      Short_t qt_crate_tac;
      Short_t qt_board_tac;
      Short_t qt_channel_tac;

      Float_t adc_min;
      Float_t adc_max;
      Float_t tac_min;
      Float_t tac_max;

    };
    EPDAnalysisMap mEPDMap[2][12][32];

    ClassDef(epdBuilder, 1);
};
