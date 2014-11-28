/***************************************************************************
 *
 * $Id: 
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *       This is the HbtEventReader class to be used when reading
 *       event-generator (e.g. mevsim) files in the GSTAR text format.
 *
 ***************************************************************************
 *
 * $Log: 
 **************************************************************************/

#ifndef StHbtGstarTxtReader_hh
#define StHbtGstarTxtReader_hh

class StEvent;
//VP class ifstream;

#ifdef __ROOT__
#include "StMaker.h"
#endif
#include "StHbtMaker/Infrastructure/StHbtString.hh"
#include "StHbtMaker/Base/StHbtEventReader.hh"

class StHbtGstarTxtReader : public StHbtEventReader{

private:
  ifstream* mInputStream;              //!
  const char* mFileName;               //!

public:
  StHbtGstarTxtReader();
  StHbtGstarTxtReader(char* FileName);
  ~StHbtGstarTxtReader();

  // generic StHbtEventReader methods
  StHbtEvent* ReturnHbtEvent();
  StHbtString Report();
  //  virtual int WriteHbtEvent(StHbtEvent*);
  int Init(const char* ReadWrite, StHbtString& Message);
  void Finish();

  // methods special to this Reader
  void SetFileName(char* file);
#ifdef __ROOT__
ClassDef(StHbtGstarTxtReader,0)
#endif
};

inline void StHbtGstarTxtReader::SetFileName(char* file){mFileName=file;}


#endif
