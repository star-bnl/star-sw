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
#include <iostream.h>
#include <fstream.h>

class StHbtAsciiReader : public StHbtEventReader{

private:
  unsigned short mStHbtEventVersion;
  unsigned short mStHbtTrackVersion;
  unsigned short mStHbtV0Version;

  ifstream* mInputStream;              //!
  ofstream* mOutputStream;             //!
  const char* mFileName;               //!
  
public:
  StHbtAsciiReader();
  StHbtAsciiReader(char* FileName);
  ~StHbtAsciiReader();

  // generic StHbtEventReader methods
  //StHbtString Report();
  StHbtEvent* ReturnHbtEvent();
  int WriteHbtEvent(StHbtEvent*);
  int Init(const char* ReadWrite, StHbtString& Message);
  void Finish();

  // methods special to this Reader
  void SetFileName(char* file);
#ifdef __ROOT__
  ClassDef(StHbtAsciiReader, 0)
#endif
};

inline void StHbtAsciiReader::SetFileName(char* file){mFileName=file;}


#endif
