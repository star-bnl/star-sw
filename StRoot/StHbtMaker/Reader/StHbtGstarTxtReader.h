/***************************************************************************
 *
 *  
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
 *  
 **************************************************************************/

#ifndef StHbtGstarTxtReader_hh
#define StHbtGstarTxtReader_hh

#include "StHbtMaker/Base/StHbtEventReader.hh"
#include <iostream.h>
#include <fstream.h>

class StHbtGstarTxtReader : public StHbtEventReader{

private:
  ifstream* mInputStream;              //!
  const char* mFileName;               //!

public:
  StHbtGstarTxtReader();
  StHbtGstarTxtReader(char* FileName);
  virtual ~StHbtGstarTxtReader();

  // generic StHbtEventReader methods
  virtual StHbtEvent* ReturnHbtEvent();
  virtual StHbtString Report();
  //  virtual int WriteHbtEvent(StHbtEvent*);
  virtual int Init(const char* ReadWrite, StHbtString Message=" ");
  virtual void Finish();

  // methods special to this Reader
  void SetFileName(char* file);
#ifdef __ROOT__
  ClassDef(StHbtGstarTxtReader, 0)
#endif
};

inline void StHbtGstarTxtReader::SetFileName(char* file){mFileName=file;}


#endif
