#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"

#include <TH1I.h>
#include <TH2F.h>

#include <math.h>
#include <memory>


// This is the one PlotSet that is guarenteed to always exist
// It's main purpose is to provide the run information 
// To the server...
//
// It has no plots (currently)
//

class Strip2CH;

class fttBuilder : public JevpBuilder {
 public:
 
  fttBuilder(JevpServer *parent=NULL) : JevpBuilder(parent) {
    plotsetname = (char *)"ftt";
    memset(&contents, 0, sizeof(contents));
  }

  ~fttBuilder() {
    int n = sizeof(contents) / sizeof(TH1 *);
    for(int i=0;i<n;i++) {
      if(contents.array[i]) delete contents.array[i];
    }
  }

  // Histo declarations!
  union {
    TH1 *array[];
    struct {
        // TPX electronics
        TH1 *tpxADC;
        TH1 *tpxADCZoom;

        TH1 *tpxFEE;
        TH1 *tpxALTRO;
        TH1 *tpxCHANNEL;
        // TH2 *tpxFEEALTRO;
        TH2 *tpxALTROCHANNEL;
        TH2 *tpxLayerRowStrip[2];
        TH2 *tpxLayerRowStripADC[2];
        TH2 *tpxLayerRowTimebinStripADC[6];
        TH2 *tpxTimebinStrip;
        TH2 *tpxTimebinADC;
        TH1 *tpxNStripsFired;
        // TH2 *tpxLayer2RowStrip;
        
        // VMM electronics
        TH1 *ADC;
        TH1 *ADCZoom;
    };
  } contents;

  void initialize(int argc, char *argv[]);
  void startrun(daqReader *rdr);
  void stoprun(daqReader *rdr);
  void event(daqReader *rdr);

  void processVMM(daqReader *rdr);
  void processTPX(daqReader *rdr);

  static void main(int argc, char *argv[]);

#ifndef __CINT__
  shared_ptr<Strip2CH> tpxMapLayer1, tpxMapLayer2;
#endif

  ClassDef(fttBuilder, 1);
};
