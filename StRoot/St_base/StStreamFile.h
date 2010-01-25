// $Id: StStreamFile.h,v 1.7 2010/01/25 17:17:11 fine Exp $
// $Log: StStreamFile.h,v $
// Revision 1.7  2010/01/25 17:17:11  fine
// Remove the redundant RecordSize method
//
// Revision 1.6  2010/01/15 21:26:51  fine
// RT #1816. Eliminate the side effect from RT 1803 fix
//
// Revision 1.5  2010/01/07 17:37:59  fine
// introduce closeFileSignal to process several DAT files at once. RT # 1794
//
// Revision 1.4  2010/01/06 20:42:26  fine
// Fix type EventNumber shoould be RunNumber . Thanks Akio
//
// Revision 1.3  2010/01/06 20:09:39  fine
// RT #1794. Add EventNumber method to the StStreamFile interface RT # 1794
//
// Revision 1.2  2009/10/13 15:44:59  fine
// add the method to provide the error message
//
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
#include <errno.h>
#include <string>

using namespace std;

class StStreamFile {
private:
    int mDebug;
    string fFilename; //< last open file name
    fstream fStream;

public:
  explicit StStreamFile(): mDebug(0), fStream(){}

  explicit StStreamFile(const char *fileName, ios_base::openmode mode 
      = ios_base::in) : mDebug(0), fStream() { open(fileName,mode); }
  virtual ~StStreamFile();
  int   Debug() const { return mDebug; }
  void  SetDebug(int debug) {  mDebug=debug; }
  void  Perror(const char * header=0) const;

public: // abstract interface  
  virtual fstream &Read()           = 0;
  virtual char *Record()            = 0;
  virtual int   Length()    const   = 0;
  virtual int   Version()   const   = 0;
  virtual int   RunNumber() const   = 0;
  virtual int   RecordUnixTime() const   = 0;

public: // fstream proxy interface   
  // fstream proxy methods
  void open(const char *fileName, ios_base::openmode mode = ios_base::in);
  void close();
  bool bad()        const { return fStream.bad();     }
  bool good()       const { return fStream.good();    }
  bool fail()       const { return fStream.fail();    }
  bool eof()        const { return fStream.eof();     }
  bool is_open()          { return fStream.is_open(); }
  ios_base::iostate rdstate() const { return fStream.rdstate(); }

  fstream &stream()                 { return fStream ;    }
  const fstream &stream()  const    { return fStream ;    }
  const string &filename() const    { return fFilename;   }
  
protected:
  istream &read(char *s, streamsize n);
  virtual bool closeFileSignal()  { return true; } //< the method to be overriden in subclass to customize the "new file has been open" status
};
#endif
