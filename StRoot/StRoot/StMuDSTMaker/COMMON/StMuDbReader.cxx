/***************************************************************************
 *
 * $Id: StMuDbReader.cxx,v 1.5 2016/05/04 19:08:27 smirnovd Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 **************************************************************************/
#include <fstream>
#include <stdio.h>
#include <stdlib.h>

#include "StMuException.hh"
#include "StMuDebug.h"
#include "StMuDbReader.h"

#include "StMaker.h"
#include "StChain.h"

ClassImp(StMuDbReader)

StMuDbReader* StMuDbReader::_instance=0;

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StMuDbReader* StMuDbReader::instance() {
  DEBUGMESSAGE2("");
  if (_instance==0) _instance = new StMuDbReader();
  return _instance;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StMuDbReader* StMuDbReader::Instance() {
  DEBUGMESSAGE2("");
  return instance();
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
/// Attention: constructor not public, this is a singleton
StMuDbReader:: StMuDbReader() {
  DEBUGMESSAGE2("");
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StMuDbReader::~StMuDbReader() {
  DEBUGMESSAGE2("");
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
/// add entries in dbFile to internal data base ( mDb ), will call sortDb(), returns number of entries in mDb
int StMuDbReader::addDb(const char* fileName) {
  DEBUGMESSAGE2("");
  char name[256];
  int numberOfEvents;
  char line[512];

  ifstream in(fileName);
  if (!in) {
    DEBUGMESSAGE2("can not open file");
    DEBUGVALUE2(fileName);
    return mDb.size();
  }
  while ( in ) {
    in.getline(line,511);
    DEBUGVALUE3(line);
    int iret = sscanf(line,"%s%i",name, &numberOfEvents);
    if (iret==2) {
      pair<string,int> aPair(name,numberOfEvents);
      mDb.push_back( aPair );
    }
  }
  in.close();
  sortDb();
  return mDb.size();
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
/// show all entries in internal data base
void StMuDbReader::showDb(){
  DEBUGMESSAGE2("");
  for (iter=mDb.begin(); iter!=mDb.end(); iter++) {
    cout << (*iter).first.c_str() << endl;
  }
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
/// sort all entries in internal data base according to file name
void StMuDbReader::sortDb(){
  DEBUGMESSAGE2("");
    list< pair<string,int> > tmpList;
    list< pair<string,int> >::iterator tmpIter;
    for (iter=mDb.begin(); iter!=mDb.end(); iter++) {
      tmpList.push_back( *iter );
    }
    tmpList.sort();

    mDb.clear();
    for (tmpIter=tmpList.begin(); tmpIter!=tmpList.end(); tmpIter++) {
      mDb.push_back( *tmpIter );
    }

}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
/// scan internal data base for file, if found return number of entries, otherwise return 0;
int StMuDbReader::entries(const char* file){
  string fileName(file);
  int lo=0;
  int hi=mDb.size();
  int pos=0;
  int oldPos=0;
  int entries=0;
  //cout << lo << " " << pos << " " << hi << endl;
  while (lo<hi) {
    pos = (int) (lo+hi)/2;
    //cout << lo << " " << pos << " " << hi << endl;
    if (oldPos==pos) break;
    if (fileName > mDb[pos].first) lo=pos;
    if (fileName < mDb[pos].first) hi=pos;
    if (fileName == mDb[pos].first) { 
      entries = mDb[pos].second;
      lo=hi;
    }
    oldPos=pos;
  }
  return entries;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
#include "TFile.h"
#include "TTree.h"
/// scan the files in inputList for the number of events add add them to the dbFile file. Create dbFile file if not existent returns number of entries in mDb
int StMuDbReader::createDB(const char* dbName, const char* inputList) {
  DEBUGMESSAGE("");

  /// fill db into list
  addDb(dbName);
  //showDb();
  // open db to add entries
  ofstream dbFile(dbName, ios::app);
  // open inputList

  // check streams
  ifstream in(inputList);
  if (!in || !dbFile) {
    DEBUGVALUE2("could not open file");
    return 0;
  }

  int count=0;
  char line[256];
  char fileName[256];
  // loop over input list
  while ( in ) {
    in.getline(line,255);
    int iret = sscanf(line,"%s",fileName);
    // if a filename is read
    if (iret==1) {
      cout << fileName << " ";
      //check whether file is alread in db or not
      if (entries(fileName)==0) {
	TFile f1(fileName);
	TTree *tree = dynamic_cast<TTree*>(f1.Get("MuDst"));
	if (tree) {
	  Stat_t nentries = tree->GetEntries();
	  dbFile << fileName << "  " << nentries << endl;
	  cout << " added with " << nentries << " entries " << endl;
	  count++;
	}
	f1.Close();
      } else {
	cout << " already in data base " << endl;
      }
    }
    cout << endl;
  }
  in.close();
  dbFile.close();
  return count;
}

/***************************************************************************
 *
 * $Log: StMuDbReader.cxx,v $
 * Revision 1.5  2016/05/04 19:08:27  smirnovd
 * StMuDbReader: Moved inlined method in the header
 *
 * Inlined methods need to be defined in the header to avoid unresolved external
 * errors from the linker
 *
 * Revision 1.4  2003/04/15 16:57:42  laue
 * Minor changes to be able to filter MuDst.root files and an example
 * how to do this. The StMuDstFilterMaker is just an example, it has to be
 * customized (spoilers, chrome weels, etc.) by the user.
 *
 * Revision 1.3  2002/10/03 19:27:13  laue
 * maximum filename length increased (256 characters)
 *
 * Revision 1.2  2002/05/04 23:56:29  laue
 * some documentation added
 *
 * Revision 1.1  2002/04/11 14:19:30  laue
 * - update for RH 7.2
 * - decrease default arrays sizes
 * - add data base readerfor number of events in a file
 *
 * Revision 1.1  2002/04/01 22:42:29  laue
 * improved chain filter options
 *
 *
 **************************************************************************/


















