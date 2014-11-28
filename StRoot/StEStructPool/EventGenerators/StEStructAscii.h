/**********************************************************************
 *
 * $Id: StEStructAscii.h 
 *
 * Author: msd
 *
 **********************************************************************
 *
 * Description:  EStructEventReader wrapper for ascii files.  
 *               Modified from StEStructPythia.
 *
 **********************************************************************/
#ifndef __STESTRUCTASCII__H
#define __STESTRUCTASCII__H

#include "StEStructPool/AnalysisMaker/StEStructEventReader.h"
#include "TROOT.h"
#include <iostream>
#include <fstream>

class StEStructEventCuts;
class StEStructTrackCuts;

class StEStructAscii : public StEStructEventReader {

  int meventCount;
  int meventsToDo;
  bool mAmDone;
  int mnumTracks;
  int mlineNumber;  // line number of input file 

  std::ifstream in;  // input file reader

  void fillTracks(StEStructEvent* estructEvent);

 public:

  StEStructAscii();
  StEStructAscii(int nevents, char* infile, StEStructEventCuts* ecuts, StEStructTrackCuts* tcuts);

  virtual ~StEStructAscii(){};
  bool hasInputFile();

  virtual StEStructEvent* next();
  virtual bool         done();
  virtual StEStructEvent* generateEvent();

  ClassDef(StEStructAscii,1)
};


inline bool StEStructAscii::done(){ return mAmDone; };

#endif

/**********************************************************************
 *
 * $Log: 
 * Revision 1.1 
 * 
 *
 *
 *********************************************************************/
