#ifndef GenericFile_h
#define GenericFile_h

#include "TMapFile.h"
#include "TFile.h"
#include <iostream>

class GenericFile {
 public:
  GenericFile(TMapFile* file) : mMapFile(file), mHisFile(0) {}
  GenericFile(TFile* file) : mMapFile(0), mHisFile(file) { }
  GenericFile( const GenericFile* file)
        : mMapFile(file->mMapFile), mHisFile(file->mHisFile) {}
  TObject* file() { 
    if (mMapFile ) return mMapFile;
    if (mHisFile ) return mHisFile;
    return 0;
  }
  TMapFile* mapFile() { return mMapFile; }
  TFile*    hisFile() { return mHisFile; }
  TObject* Get(const char* name, TObject* o=0) {
    if ( mMapFile ) {
      return mMapFile->Get(name,o);
    }
    if ( mHisFile ) return mHisFile->Get(name);
    return 0;
  }
 protected:
  TMapFile* mMapFile;
  TFile* mHisFile;
};

#endif





/***************************************************************************
 *
 * $Id: GenericFile.h,v 1.1 2009/01/12 18:32:51 fine Exp $
 *
 * Author: Frank Laue, laue@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: GenericFile.h,v $
 * Revision 1.1  2009/01/12 18:32:51  fine
 * Move GenericFile to St_base to remove IN_PANITKIN from muEztPanitkin/EEqaPresenter
 *
 * Revision 1.1  2007/02/27 15:23:37  laue
 * Initial version
 *
 * Revision 1.1  2006/10/04 20:31:16  laue
 * Initial Version
 *
 *
 ***************************************************************************/

