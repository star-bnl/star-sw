#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
//#include "RunStatus.h"

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
    // Histo declarations!
    
  union {
    TH1 *array[];
    struct {
      TH2 *cdb[4];
      TH2 *tac_east_vs_tac_west;
      //   TH2 *vertex_vs_l3_vertex;
      TH2 *earliestTAC_vs_eastchan;
      TH2 *earliestTAC_vs_westchan;
      
      TH2 *hi_cdb[4];
      TH2 *hi_tac_east_vs_tac_west;
      //  TH2 *hi_vertex_vs_l3_vertex;
      TH2 *hi_earliestTAC_vs_eastchan;
      TH2 *hi_earliestTAC_vs_westchan;
    };
  } contents;

  void initialize(int argc, char *argv[]);
  void startrun(daqReader *rdr);
  void stoprun(daqReader *rdr);
  void event(daqReader *rdr);
  
  static void main(int argc, char *argv[]);

  ClassDef(vpdBuilder, 1);
};
