/***************************************************************************
 *
 *  
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *       This is the HbtEventReader class to be used when reading/writing
 *       BINARY files. 
 *       It is been developted out of a copy of the StHbtAsciiReader class 
 *       on 11/18/1999 
 *
 ***************************************************************************
 *
 *  
 **************************************************************************/

#ifndef StHbtBinaryReader_hh
#define StHbtBinaryReader_hh

#include "StHbtMaker/Base/StHbtEventReader.hh"
#include <iostream.h>
#include <fstream.h>

#include "StHbtMaker/Infrastructure/StHbtIOBinary.hh"

class StHbtBinaryReader : public StHbtEventReader{

private:
  StHbtIOBinary* binaryIO;

  ifstream* mInputStream;              //!
  ofstream* mOutputStream;             //!
  const char* mFileName;               //!
  const char* mInFileName;             //!

public:
  StHbtBinaryReader();
  StHbtBinaryReader(char* FileName);
  virtual ~StHbtBinaryReader();

  // generic StHbtEventReader methods
  virtual StHbtEvent* ReturnHbtEvent();
  virtual StHbtString Report();
  virtual int WriteHbtEvent(StHbtEvent*);
  virtual int Init(const char* ReadWrite, StHbtString Message=" ");
  virtual void Finish();

  // methods special to this Reader
  void SetFileName(char* file);

#ifdef __ROOT__
  ClassDef(StHbtBinaryReader, 0)
#endif
};

inline void StHbtBinaryReader::SetFileName(char* file){mFileName=file;}


#endif
