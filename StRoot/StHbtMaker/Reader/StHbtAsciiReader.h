/***************************************************************************
 *
 *  
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *       This is the HbtEventReader class to be used when reading
 *       ASCII files.
 *
 ***************************************************************************
 *
 *  
 **************************************************************************/

#ifndef StHbtAsciiReader_hh
#define StHbtAsciiReader_hh

#include "StHbtMaker/Base/StHbtEventReader.hh"
#include <iostream>
#include <fstream>

class StHbtAsciiReader : public StHbtEventReader{

private:
  ifstream* mInputStream;              //!
  ofstream* mOutputStream;             //!
  const char* mFileName;               //!

public:
  StHbtAsciiReader();
  StHbtAsciiReader(char* FileName);
  virtual ~StHbtAsciiReader();

  // generic StHbtEventReader methods
  virtual StHbtEvent* ReturnHbtEvent();
  virtual StHbtString Report();
  virtual int WriteHbtEvent(StHbtEvent*);
  virtual int Init(const char* ReadWrite, StHbtString Message=" ");
  virtual void Finish();

  // methods special to this Reader
  void SetFileName(char* file);
#ifdef __ROOT__
  ClassDef(StHbtAsciiReader, 0)
#endif
};

inline void StHbtAsciiReader::SetFileName(char* file){mFileName=file;}


#endif
