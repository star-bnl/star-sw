#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include <TH1F.h>
#include <TH2F.h>

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
      TH1 *array[];
      struct {
	TH1 *hQT[4];
	TH1 *hADC[2][12];
	TH1 *hTAC[2][12];
      };
    } contents;

    JevpPlot **plots;




    struct EPDAnalysisMap {
      Short_t qt_board_address; // channel number used in QT board or other physical numbering scheme 0x10...
      Short_t qt_channel_ADC; // QT board channel used 0....31
      Short_t qt_channel_TAC; // QT board channel used 0....31
    };

    //ew; // East-0, West-1;
    //position; // look from +Z in STAR cdt, 1'o clock is pp1, 2'o clock pp2
    //tile; // 1-inner most, 2-next left look from +Z in STAR cdt, 1'o clock is pp1, 2'o clock pp2, 3-next right look from +Z in STAR cdt, 1'o clock is pp1, 2'o clock pp2,

    EPDAnalysisMap mEPDMap[2][12][31];


    ClassDef(epdBuilder, 1);
};
