/**********************************************************
 *
 * Here are defined the input/output stream operators for
 *  StHbtEvent and associated classes
 *
 *********************************************************/
#ifndef StHbtIOBinary_hh
#define StHbtIOBinary_hh

#include "StHbtMaker/Infrastructure/StHbtV0.hh"
#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#include "StHbtMaker/Infrastructure/StHbtEvent.hh"
#include "StarClassLibrary/StPhysicalHelix.hh"
#include <float.h>    // these tell maximum values of types so we
#include <limits.h>   // don't write the characters "inf" to our microDST
#include <iostream.h>
#include <fstream.h>

// /////////////////////////////////////////////////////////////////////////
// the god damned SOLARIS compiler can handle templates only in global scope
// for that reason we have to stick the OStream in to the function call
// this would not be nessesarry on linux where we would declare the 
// binaryWrite/Read function as methods of the StHbtBinary class (see below)
// /////////////////////////////////////////////////////////////////////////
template<class T> int binaryWrite(ofstream* myStream, T x){
  myStream->write( (char*)&x, sizeof(x) );
  return sizeof(x);
}
template<class T> int binaryRead(ifstream* myStream, T& x){
  myStream->read( (char*)&x, sizeof(x) );
  return sizeof(x);
}

enum ioStatus { ioERR=-1, ioOK, ioEOF, ioEOL, ioERROpen }; 

class StHbtIOBinary /*: friend StHbtEvent, friend StHbtTrack, friend StHbtV0*/ {

public:
  int byteCounterEvent;
  int byteCounterTotal;
  ofstream* mOStream;
  ifstream* mIStream;

  StHbtIOBinary(const char* dirName, const char* fileName, const char* appendix, const char* readWrite);
  ~StHbtIOBinary();

  // StHbtEvent
  int writeEvent(StHbtEvent& ev);
  int readEvent(StHbtEvent& ev);
  // StHbtTrack
  int writeTrack(StHbtTrack& trk);
  int readTrack(StHbtTrack& trk);
  // StHbtV0
  int writeV0(StHbtV0& v0);
  int readV0(StHbtV0& v0);
  // StHbtString
  void writeString(StHbtString& v);
  void readString(StHbtString& v);

  int bytesWritten();
  int bytesRead();

  
  char* parseDirFile(const char*, const char*, const char*);

  void wait(int n, const char* c) { 
    for (int i = 0; i< 1e6*n; i++) {
      cout << c;
    }
  }

  // ////////////////////////////////////////////////
  // please read the comment at the beginning of file
  // ////////////////////////////////////////////////
#ifndef ST_NO_MEMBER_TEMPLATES
  template<class T> int write(T x){ //!
    mOStream->write( (char*) &x, sizeof(x) );
    return sizeof(x);
  }
  template<class T> int read(T& x){ //!
    mIStream->read( (char*)&x, sizeof(x) );
    return sizeof(x);
  }
#else
  int write(unsigned short x){
    mOStream->write( (char*) &x, sizeof(x) );
    return sizeof(x);
  }
  int read(unsigned short& x){
    mIStream->read( (char*)&x, sizeof(x) );
    return sizeof(x);
  }
#endif

};

#endif
