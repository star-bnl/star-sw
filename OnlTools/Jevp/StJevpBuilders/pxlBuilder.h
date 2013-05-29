#include <stdio.h>
#include <stdlib.h>
#include <bitset>
#include <iostream>

#include "Jevp/StJevpPlot/JevpPlotSet.h"
#include "DAQ_READER/daqReader.h"
#include <TH1F.h>
#include <TH2F.h>
#include <TH2D.h>
#include <TH3D.h>

#include <math.h>

const int NSENSOR = 40;
const int NCOL = 960;
const int NROW = 928;
const int NRDO = 10;


class pxlBuilder : public JevpPlotSet {
public:
  int run;

  pxlBuilder(JevpServer *parent=NULL); 
  ~pxlBuilder();
  

  void initialize(int argc, char *argv[]);
  void startrun(daqReader *rdr);
  void stoprun(daqReader *rdr);
  void event(daqReader *rdr);
  
  static void main(int argc, char *argv[]);

  static const int PXLERR_HEADER;
  static const int PXLERR_HITLEN;
  static const int PXLERR_SENSORNUM;
  static const int PXLERR_DUPLICATE;
  static const int PXLERR_ENDER;
  static const int PXLERR_RANGE;
  static const int PXLERR_OUTOFORDER;
  static const int PXLERR_UNKNOWNROW;
  

  bitset<NCOL> bs[NSENSOR][NROW];
  float ave_runlength[NSENSOR];

 private:

  int event_multiplicity;
  int multiplicity_inner;
  int multiplicity_outer;
  int sensor_count;
  int number_of_events;

  int max_count;
  int max_count_sector1;
  int max_count_sector2;
  int max_count_sector3;
  int max_count_sector4;
  int max_count_sector5;
  int max_count_sector6;
  int max_count_sector7;
  int max_count_sector8;
  int max_count_sector9;
  int max_count_sector10;

  int max_count_inner;
  int max_count_outer;

  int min_count;
  int min_count_sector1;
  int min_count_sector2;
  int min_count_sector3;
  int min_count_sector4;
  int min_count_sector5;
  int min_count_sector6;
  int min_count_sector7;
  int min_count_sector8;
  int min_count_sector9;
  int min_count_sector10;

  int min_count_inner;
  int min_count_outer;

  int count_hits_inner[10][10];
  int count_hits_outer[30][10];
  int count_length_inner[10][10];
  int count_length_outer[30][10];

  map<int,double> *AverageRunLength;
  map<int,int> *LadderCount;
  map<int,int> *LadderMap;
  
  struct _pxlHeaderInfo {
    unsigned short tcdWord;
    unsigned int rhicStrobeCtr;
    unsigned short temperature[4][2];
  };

  _pxlHeaderInfo pxlHeaderInfo;

  int pxl_decoder(const u_int *d, const int wordLength,bitset<NCOL> bs[][NROW],int *OVF, struct _pxlHeaderInfo *pxlHeaderInfo,int *error_cnt,float *ave_runlength);
  int decode16bit(unsigned short val, bool MS16,int sensor,int *row,int *prev_row,int *prev_col,int *error_cnt,int *OVF,bitset<NCOL> bs[][NROW],int runlength[][4]);

  void IncrementMultiplicity(int sensor_number,int row_count);
  int WhichLadder(int sector_number,int sensor_number);
  void UpdateLadderCount(int sector_number,int sensor_number,int sensor_count);
  void SetRunLength(int sensor_number,double average_run_length);
  bool UpdateTH1(TH1 *hist,int bin,double value);
  bool UpdateTH2(TH1 *hist,int x_bin,int y_bin,double value);
  bool UpdateTH2(const char* name,TH1 *hist,int x_bin,int y_bin,double value);
  bool UpdateTH1(TH1 *hist,int bin,double value,bool scale,int mod_val);
  bool ScaleTH1Bin(TH1 *hist,int bin,int scale_factor);
  void SetLadderMap();
  int IncrementArray(const char* name,int x_bin,int y_bin);

  //*** Histogram Declarations...
  //*** Use the union to be able to treat in bulk
  //*** As well as by name...
  union {
    TH1 *array[];
    struct {
      //TH1 *myhisto;

      //Tab 1: Global Multiplicity
      TH1 *GlobalHitMultiplicity;

      TH1 *GlobalHitMultiplicitySector1;
      TH1 *GlobalHitMultiplicitySector2;
      TH1 *GlobalHitMultiplicitySector3;
      TH1 *GlobalHitMultiplicitySector4;
      TH1 *GlobalHitMultiplicitySector5;
      TH1 *GlobalHitMultiplicitySector6;
      TH1 *GlobalHitMultiplicitySector7;
      TH1 *GlobalHitMultiplicitySector8;
      TH1 *GlobalHitMultiplicitySector9;
      TH1 *GlobalHitMultiplicitySector10;
      

      //Tab 2: Hit Multiplicity
      TH1 *HitMultiplicityPerEvent;

      TH1 *HitsPerLadder;

      TH1 *HitCorrelation;


      //Tab 2: Hit Maps
      TH1 *SensorHitsInnerLayer;
      TH1 *SensorHitsOuterLayer;

      TH1 *AverageRunLengthInnerLayer;
      TH1 *AverageRunLengthOuterLayer;

    };
  } contents;

  //*** End Histogram Declarations...

  ClassDef(pxlBuilder, 1);
};
