// $Id: StTrgDatReader.h,v 1.3 2010/01/06 20:42:26 fine Exp $
//
// $Log: StTrgDatReader.h,v $
// Revision 1.3  2010/01/06 20:42:26  fine
// Fix type EventNumber shoould be RunNumber . Thanks Akio
//
// Revision 1.2  2010/01/06 20:09:39  fine
// RT #1794. Add EventNumber method to the StStreamFile interface RT # 1794
//
// Revision 1.1  2009/10/13 15:53:31  fine
// Akio\'s DAT file format reader
//
//
  
/***************************************************************************
 * Author: akio ogawea
 * Description: Trigger Data file (run*.*.dat) reader
 **************************************************************************/

#ifndef __StTrgDatReader_h__
#define __StTrgDatReader_h__

#include "StStreamFile.h"

class StTrgDatReader : public StStreamFile {

public:
  explicit StTrgDatReader();
  explicit StTrgDatReader(const char *fileName, ios_base::openmode mode = ios_base::in);

  virtual ~StTrgDatReader();
  
public: // implementation of StStreamFile abstract interface
      
  fstream &Read();
  char *Record();
  int   Length()  const;
  int   Version() const;
  int   RunNumber() const;
  
protected :
  char *Buffer(streamsize n);

private:
        
  int     mLength;
  int     mVersion;
  int     mRunNumber;
  char   *mData;
  streamsize mAllocated;
  static const int mLheader; // the length of the header
};
#endif
