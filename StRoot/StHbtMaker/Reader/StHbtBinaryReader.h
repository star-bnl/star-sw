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

#ifdef __ROOT__
#include "StMaker.h"
#include "StIOMaker/StIOMaker.h"
#include "StIOInterFace.h"
#endif

#include <list>
#if !defined(ST_NO_NAMESPACES)
using std::list;
#endif
#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StHbtString*, allocator<StHbtString*> >            fileCollection;
typedef list<StHbtString*, allocator<StHbtString*> >::iterator  fileIterator;
#else
typedef list<StHbtString*>            fileCollection;
typedef list<StHbtString*>::iterator  fileIterator;
#endif


class StHbtBinaryReader : public StHbtEventReader{

private:
  StHbtIOBinary* binaryIO;

  unsigned short mStHbtEventVersion;
  unsigned short mStHbtTrackVersion;
  unsigned short mStHbtV0Version;

  int mReaderStatus;                   //!
  const char* mFileName;               //!
  const char* mDirName;                //!
  const char* mAppendix;               //!
  fileCollection* mFileList;           //!
  int mRetrieve;
  StHbtString mTheMessage;
  StHbtString mCurrentFile;

  void FillFileList(char* fileList);
  int NextFile();

#ifdef __ROOT__
  StIOMaker* mIOMaker;                 //!
#endif

public:
  StHbtBinaryReader(const char* dir=0, const char* file="test", const char* appendix=0);
#ifdef __ROOT__
  StHbtBinaryReader(StIOMaker* ioMaker, const char* dir="./", const char* file="test", const char* appendix=".microDst");
#endif
  ~StHbtBinaryReader();

  // generic StHbtEventReader methods
  StHbtEvent* ReturnHbtEvent();
  int WriteHbtEvent(StHbtEvent*);
  int Init(const char* ReadWrite, StHbtString& Message);
  void Finish();
  StHbtString Report();
  
  // methods special to this Reader
  void SetDirName(const char*);
  void SetFileName(const char*);
  void SetAppendix(const char*);
  void AddFileList(const char*);

 private:
  void init(const char* dir, const char* file, const char* appendix);

#ifdef __ROOT__
  ClassDef(StHbtBinaryReader, 0)
#endif
      

};



#endif
