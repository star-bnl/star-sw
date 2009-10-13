// $Id: StTrgDatReader.h,v 1.1 2009/10/13 15:53:31 fine Exp $
//
// $Log: StTrgDatReader.h,v $
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
  
protected :
  char *Buffer(streamsize n);

private:
        
  int     mLength;
  int     mVersion;
  char   *mData;
  streamsize mAllocated;
  static const int mLheader; // the length of the header
};
#endif
