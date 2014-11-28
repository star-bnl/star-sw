/*
  just chains root files


 */
#ifndef IO_H
#define IO_H

#include "TString.h"
#include "TObject.h"
#include <vector>
#include <string>
#include <list>
#include <utility>

//typedef pair<string,int> myPair

class TChain;

class IO : public TObject{
 public:
  IO(const char* dir="./", const char* match="st_physics", const char* ext=".root");
  ~IO();

  void setNFile(int n) { mNFile = n; }
  void chain(TChain* chain);
  void createDb(const char* dbFile);

/*
  int createDb(const char* dbFile, const char* inputList);
  int addDb(const char* dbFile);
  void showDb();
  void sortDb();
  int entriesDb();
  int entries(const char* file);
*/

 private:
  int mNFile;
  TString mDir;
  TString mMatch;
  TString mExt;

/*
  /// the internal database, a vector containing pairs of file names and number of events
  vector< myPair > mDb;
  vector< myPair >::iterator iter;
*/

  ClassDef(IO,1)

};

/// number of entries in internal data base
//inline int IO::entriesDb() { return mDb.size(); }

#endif
