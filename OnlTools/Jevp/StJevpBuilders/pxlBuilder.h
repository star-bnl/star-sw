#include <stdio.h>
#include <stdlib.h>
#include <iostream>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include <TH1F.h>
#include <TH2F.h>
#include <TH2D.h>
#include <TH3D.h>

#include <math.h>

#include "pxl_decoder.h"

const int NRDO = 10;

class pxlBuilder : public JevpBuilder {
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
  bitset2D<NROW,NCOL> *bs; /* allocate dynamically in constructor */

  int event_multiplicity;
  int multiplicity_inner;
  int multiplicity_outer;
  int sensor_count;
  int event_count;
  int number_of_events;

  int max_count;
  int max_count_sector[NRDO];

  int max_count_inner;
  int max_count_outer;

  int min_count;
  int min_count_sector[NRDO];

  int min_count_inner;
  int min_count_outer;

  int count_hits_inner[10][NRDO];
  int count_hits_outer[30][NRDO];
  int count_length_inner[10][NRDO];
  int count_length_outer[30][NRDO];

  map<int,int> *LadderCount;
  
  int sensor_hits[NRDO][NSENSOR];
  int sensor_hit_frequency[NRDO][NSENSOR];
  double avg_run_length[NRDO][NSENSOR];

  void IncrementMultiplicity(int sensor_number,int row_count);
  int WhichLadder(int sector_number,int sensor_number);
  void UpdateLadderCount(int sector_number,int sensor_number,int sensor_count);
  bool UpdateTH1(TH1 *hist,int bin,double value);
  bool UpdateTH1_Scale(TH1 *hist,int bin,double value, int number_of_events_old);
  bool UpdateTH2(TH1 *hist,int x_bin,int y_bin,double value);
  bool UpdateTH2_Scale(TH1 *hist,int x_bin,int y_bin,double value, int number_of_events_old);
  bool UpdateTH2_Scale2(const char* name,TH1 *hist,int x_bin,int y_bin,double value, int number_of_events_old);
  int IncrementArray(const char* name,int x_bin,int y_bin);

  void UpdateSectorErrorTypeTH2(TH1 *hist, int ret, int sector_number);

  int GetLadderCount(int ladder_number);
  void FillLadderHistogram(TH1 *hist);
  void UpdateLadderHistogram(TH1 *hist, TH1 *hist_single_evt, int number_of_events_old);
  void UpdateLayerHistograms(TH1 *h_hits_inner, TH1 *h_rl_inner, TH1 *h_hits_outer, TH1 *h_rl_outer, int number_of_events);

  //*** Histogram Declarations...
  //*** Use the union to be able to treat in bulk
  //*** As well as by name...
  union {
    TH1 *array[];
    struct {
      //Tab 1: Global Multiplicity
      TH1 *GlobalHitMultiplicity;
      TH1 *GlobalHitMultiplicitySector[NRDO];
 
      //Tab 2: Hit Multiplicity
      TH1 *HitMultiplicityPerEvent;
      TH1 *HitsPerLadder;
      TH1 *HitsPerLadderPerEvent;
      TH1 *HitCorrelation;

      //Tab 3: Hit Maps
      TH1 *SensorHitsInnerLayer;
      TH1 *SensorHitsOuterLayer;
      TH1 *AverageRunLengthInnerLayer;
      TH1 *AverageRunLengthOuterLayer;

      //Tab 4: Errors
      TH1 *ErrorCountSector[NRDO];
      TH1 *SectorErrorType;
      TH1 *SerdesErrors;
    };
  } contents;

  //*** End Histogram Declarations...

  ClassDef(pxlBuilder, 1);
};
