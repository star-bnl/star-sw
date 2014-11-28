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
#include <Stiostream.h>
#include "Stiostream.h"

typedef unsigned int colSizeType;

// /////////////////////////////////////////////////////////////////////////
// the god damned SOLARIS compiler can handle templates only in global scope
// for that reason we have to stick the OStream in to the function call
// this would not be nessesarry on linux where we would declare the 
// binaryWrite/Read function as methods of the StHbtBinary class (see below)
// /////////////////////////////////////////////////////////////////////////

enum ioStatus { ioOK=0, ioERR, ioEOF, ioEOL, ioERROpen }; 

class StHbtIOBinary /*: friend StHbtEvent, friend StHbtTrack, friend StHbtV0*/ {
protected:
  int mDebug;
public:
  int byteCounterEvent;
  int byteCounterTotal;
  ofstream* mOStream;
  ifstream* mIStream;
  
  StHbtIOBinary(const char* dirName, const char* fileName, const char* appendix, const char* readWrite);
  ~StHbtIOBinary();
  
#ifndef SOLARIS
  template<class T> int read(T& x);
  template<class T> int write(const T& x);
#else
  int read(unsigned short x) {return 0;}
  int read(const StHbtTrack) {return 0;}
  int read(const StHbtV0) {return 0;};
  int write(const StHbtTrack) {return 0;}
  int write(unsigned short x) {return 0;}
  int write(const StHbtV0) {return 0;}
#endif

  int read(StThreeVectorD& x);
  int write(const StThreeVectorD& x);
  int read(StPhysicalHelixD& x);
  int write(const StPhysicalHelixD& x);
  // StHbtEvent
  int read(StHbtEvent& event, unsigned short evVersion, unsigned short trVersion, unsigned short v0Version );
  int write(const StHbtEvent& event, unsigned short evVersion, unsigned short trVersion, unsigned short v0Version );
  // StHbtTrack
  int read(StHbtTrack&, unsigned short);
  int write(const StHbtTrack&, unsigned short);
  // StHbtV0
  int read(StHbtV0&, unsigned short);
  int write(const StHbtV0&, unsigned short);
  // StHbtString
  int readString(StHbtString&);
  int writeString(const StHbtString&);
  int readHeader(StHbtString&);
  int writeHeader(const StHbtString&);
  int bytesRead();
  int bytesWritten();

  // StHbtEvent Versions
  int read_V0(StHbtEvent& event, unsigned short trVersion, unsigned short v0Version );
  int read_V1(StHbtEvent& event, unsigned short trVersion, unsigned short v0Version );
  int read_V2(StHbtEvent& event, unsigned short trVersion, unsigned short v0Version );
  int write_V0(const StHbtEvent& event, unsigned short trVersion, unsigned short v0Version );
  int write_V1(const StHbtEvent& event, unsigned short trVersion, unsigned short v0Version );
  int write_V2(const StHbtEvent& event, unsigned short trVersion, unsigned short v0Version );
  // StHbtTrack Versions
  int read_V1(StHbtTrack&);
  int read_V2(StHbtTrack&);
  int write_V1(const StHbtTrack&);
  int write_V2(const StHbtTrack&);
  // StHbtV0 Versions
  int read_V1(StHbtV0&);
  int read_V2(StHbtV0&);
  int read_V3(StHbtV0&);
  int write_V1(const StHbtV0&);
  int write_V2(const StHbtV0&);
  int write_V3(const StHbtV0&);

  int outputStreamStatus();
  int inputStreamStatus();

  int readTrackList(StHbtEvent&, unsigned short trVersion);
  int readV0List(StHbtEvent&, unsigned short v0Version);
  int writeTrackList(const StHbtEvent&, unsigned short trVersion);
  int writeV0List(const StHbtEvent&, unsigned short v0Version);

  const char* parseDirFile(const char*, const char*, const char*);
  
  void wait(int n, const char* c) { 
    for (int i = 0; i< 1e6*n; i++) {
      cout << c;
    }
  }
};

inline int StHbtIOBinary::outputStreamStatus() {
  if (mOStream) return !mOStream->good();
  return ioERR;
}
inline int StHbtIOBinary::inputStreamStatus() {
  if (mIStream) return !mIStream->good();
  return ioERR;
}

#ifndef SOLARIS
template<class T> 
inline int StHbtIOBinary::read(T& x){
  mIStream->read( (char*)&x, sizeof(x) );
  byteCounterEvent += sizeof(x);
  return (!mIStream->good());
}
template<class T> 
inline int StHbtIOBinary::write(const T& x){
  mOStream->write( (char*)&x, sizeof(x) );
  byteCounterEvent += sizeof(x);
  return (!mOStream->good());
}
#endif

inline int StHbtIOBinary::read(StThreeVectorD& x) {
  int iret;
  double a,b,c; 
  iret = read(a);
  iret = read(b);
  iret = read(c);
  x = StHbtThreeVector(a,b,c);
  return (!mIStream->good());
};

inline int StHbtIOBinary::write(const StThreeVectorD& x) {
  int iret;
  double a,b,c;
  a = x.x(); b = x.y(); c = x.z();
  iret = write(a);
  iret = write(b);
  iret = write(c);
  return (!mOStream->good());
};
inline int StHbtIOBinary::read(StPhysicalHelixD& x){ 
  int iret;
  double c, dip, phase;
  StThreeVectorD o;
  int h=-1;
  iret = read(o);
  iret = read(dip);
  iret = read(c);
  iret = read(phase);
  iret = read(h);
  x = StPhysicalHelixD(c,dip,phase,o,h);
  return (!mIStream->good());
};

inline int StHbtIOBinary::write(const StPhysicalHelixD& x){ 
  int iret;
  double c, dip, phase;
  StThreeVectorD o;
  int h=-1;
  c = x.curvature();
  dip = x.dipAngle();
  phase = x.phase();
  o = x.origin();
  h = x.h();
  iret = write(o);
  iret = write(dip);
  iret = write(c);
  iret = write(phase);
  iret = write(h);
  return (!mOStream->good());
};

#endif
