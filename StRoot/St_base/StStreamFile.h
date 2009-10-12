// $Id: StStreamFile.h,v 1.1 2009/10/12 04:21:06 fine Exp $
// $Log: StStreamFile.h,v $
// Revision 1.1  2009/10/12 04:21:06  fine
// Add abstract STAR iostream-base file interface
//
//
  
/***************************************************************************
 * \Author: Valeri Fine )fine@bnl.gov)
 * Description: The  base proxy class for fstream-base I/O operation 
 **************************************************************************/

#ifndef __StStreamFile_h__
#define __StStreamFile_h__

#include <fstream>
using namespace std;

class StStreamFile {
private:
    fstream fStream;
    int mDebug;
public:
  explicit StStreamFile():fStream(), mDebug(0) {}

  explicit StStreamFile(const char *fileName, ios_base::openmode mode 
      = ios_base::in):fStream(fileName,mode), mDebug(0) {}
  virtual ~StStreamFile();
  int Debug() const { return mDebug; }
  void  SetDebug(int debug) {  mDebug=debug; }

public: // abstract interface
  virtual char *Record()        = 0;
  virtual int   Length()  const = 0;
  virtual int   Version() const = 0;

public: // fstream proxy interface   
  // fstream proxy methods
  void open(const char *fileName, ios_base::openmode mode = ios_base::in);
  bool bad()        const { return fStream.bad();     }
  bool good()       const { return fStream.good();    }
  bool fail()       const { return fStream.fail();    }
  bool eof()        const { return fStream.eof();     }
  bool is_open()          { return fStream.is_open(); }
  ios_base::iostate rdstate() const { return fStream.rdstate(); }

  fstream &stream()             { return fStream ;    }
  const fstream &stream() const { return fStream ;    }
  
protected:
  istream &read(char *s, streamsize n);
  void close();
};
#endif
