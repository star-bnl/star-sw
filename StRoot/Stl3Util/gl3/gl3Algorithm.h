//:>------------------------------------------------------------------
//: FILE:       gl3Algorithm.h
//: HISTORY:
//:              1feb2000 version 1.00
//:             31jul2000 add methods initRun, endRun, end
//:<------------------------------------------------------------------
// 
// tom: This class implements the "housekeeping"-issues common to 
// all Algorithms: counters, pre-/postscaling, parameter handling, 
// generation of L3 summary data.
//
// Any implementation of an L3 algorithm must inherit from this class.
//
//------------------------------------------------------------------

#ifndef GL3ALGORITHM 
#define GL3ALGORITHM

#include "Stl3Util/gl3/gl3Event.h"
#include "Stl3Util/gl3/gl3Histo.h"

//#include "foreign/daqFormats.h"
#include "Stl3Util/foreign/L3/L3Algorithms.h"

#include <stdio.h>
#include <math.h>


class gl3Algorithm {
 public:

  gl3Algorithm();
  virtual ~gl3Algorithm();

  virtual int init();
  virtual int end();
  
  int process (gl3Event* event_in);
  void incrementCounters();

  virtual int setParameters(int, int, int, int, int, 
			    float, float, float, float, float);
  void setScaling(int preScale, int postScale);

  void fillSummary(struct algorithm_data *dest);

  virtual const int   getAlgorithmID() = 0;
  virtual const char *getAlgorithmName() = 0;

 protected:
  virtual int  decide  () = 0;
  int priority; // 1 = soft trigger (EVB can drop the event to save bandwidth)
                // 2 = hard trigger (EVB will write in any case)

  
  // Pointer to L3_P containing the event data
  gl3Event * event;

  // Parameters from run control
  int   GI1, GI2, GI3, GI4, GI5;
  float GF1, GF2, GF3, GF4, GF5;

  // Algorithm summary data to be stored in algorithm_data
  // Filled by decide()
  float SummaryData[10];


 public:
  void showConfiguration();
  


 private:

  void resetCounters();

  typedef int counter_type;

  counter_type preScale_cnt;
  counter_type call_cnt;
  counter_type accept_cnt;
  counter_type build_cnt;

  int preScale;
  int postScale;

  char on, accept, build;
};

gl3Algorithm *gl3InstantiateAlgorithm(int algID);


#endif
