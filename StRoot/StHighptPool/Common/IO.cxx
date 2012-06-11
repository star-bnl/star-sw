
#include "IO.h"
#include "TChain.h"
#include "TFile.h"
#include "TTree.h"
#include "TSystem.h"

#include "Stiostream.h"
#include <fstream>
#include <stdio.h>
#include <stdlib.h>

ClassImp(IO)

IO::IO(const char* dir, const char* match, const char* ext) 
  : mNFile(0), mDir(dir), mMatch(match), mExt(ext) {

}

IO::~IO() {}

void IO::chain(TChain* chain)
{

  void *pDir = gSystem->OpenDirectory(mDir.Data());
  if(!pDir){
    std::cerr << "##Cannot open directory " << mDir.Data() << endl;
    std::cerr << "##Goodbye" << endl;
    exit(1);
  }
  
  cout << "\tUsing directory : " << mDir.Data() << endl;
  cout << "\tMatch string    : " << mMatch.Data() << endl;
  cout << "\tMatch extension : " << mExt.Data() << endl;
  if(mNFile) cout << "\tMaximum # files : " << mNFile << endl;

  //
  // now find the files that end in the specified extension
  // and match the specified string
  //
  const char* fileName(0);
  Int_t count(0);

  while((fileName = gSystem->GetDirEntry(pDir))){
    if(strcmp(fileName,".")==0 || strcmp(fileName,"..")==0) continue;

    if(strstr(fileName,mExt.Data()) && strstr(fileName,mMatch.Data())){ // found a match
      char* fullFile = gSystem->ConcatFileName(mDir.Data(),fileName);

      // add it to the chain
      //  cout << "\tAdding " << fullFile << " to the chain" << endl;

      //Here implement appeal to event count to speed up chain read

      count++;

      int events = 0;
/*
      // read number of events in file from db
      events = entries(fullFile);
*/
      // if I can not read the number of events from db, open file and read number.
      if (events==0) {    
        TFile *f1 = new TFile(fullFile);
        TTree *tree = (TTree*)f1->Get("StHiMicroTree");
        if (tree) events = (int)tree->GetEntries();
        delete f1;
      }
      // add to chain if #events > 0
      if (events) {
        chain->Add(fullFile,events);    
	if (count%100==0) cout << "File # " << count << endl << "\tAdding " << 
				fullFile << " with " << events << " events to the chain" << endl;
        delete fullFile;
      }

      if(mNFile && count > mNFile) break;

    }
  }   
  cout << "Added " << count << " files to the chain" << endl;

}

void IO::createDb(const char *dbName)
{

  // open db to add entries
  ofstream dbFile(dbName, std::ios::app);
  
  void *pDir = gSystem->OpenDirectory(mDir.Data());
  if(!pDir){
    std::cerr << "##Cannot open directory " << mDir.Data() << endl;
    std::cerr << "##Goodbye" << endl;
    exit(1);
  }
  
  cout << "\tUsing directory : " << mDir.Data() << endl;
  cout << "\tMatch string    : " << mMatch.Data() << endl;
  cout << "\tMatch extension : " << mExt.Data() << endl;

  //
  // now find the files that end in the specified extension
  //
  const char* fileName(0);
  Int_t count(0);

  while((fileName = gSystem->GetDirEntry(pDir))){
    if(strcmp(fileName,".")==0 || strcmp(fileName,"..")==0) continue;

    if(strstr(fileName,mExt.Data()) && strstr(fileName,mMatch.Data())){ // found a match
      char* fullFile = gSystem->ConcatFileName(mDir.Data(),fileName);

      count++;
      int events = 0;
      //Open file and read number.
      if (events==0) {    
        TFile *f1 = new TFile(fullFile);
        TTree *tree = (TTree*)f1->Get("StHiMicroTree");
        if (tree) events = (int)tree->GetEntries();
        delete f1;
      }
      if (events) {
        dbFile << fullFile << "  " << events << endl;
	cout << "\tAdding " << fullFile << " with " << events << " events to the Database" << endl;
        delete fullFile;
      }

    }
  }   
  dbFile.close();

}

/*

//----------------------------------------------------------------
/// add entries in dbFile to internal data base ( mDb ), will call 
/// sortDb(), returns number of entries in mDb

int IO::addDb(const char* fileName) {

  char name[128];
  string nameString;
  int numberOfEvents;
  char line[256];
 
  ifstream in(fileName);
  if (!in) {
    cout << "Cannot open file " << fileName << endl;
    return mDb.size();
  }
  while ( in ) { 
    in.getline(line,255);
    int iret = sscanf(line,"%s  %i",name, &numberOfEvents);
    nameString = name;
    if (iret==2) {
      pair<string,int> aPair(nameString,numberOfEvents);
      mDb.push_back( aPair );
    }
  }
  in.close();    
  sortDb();
  return mDb.size();
}

//-----------------------------------------------------
/// show all entries in internal data base
void IO::showDb(){
  for (iter=mDb.begin(); iter!=mDb.end(); iter++) {
    cout << (*iter).first.c_str() << endl;
  }
}

//-----------------------------------------------------------------------
/// sort all entries in internal data base according to file name
void IO::sortDb(){

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
/// scan internal data base for file, if found return number of entries, 
/// otherwise return 0;

int IO::entries(const char* file){
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
/// scan the files in inputList for the number of events add add them to 
/// the dbFile file.
/// Create dbFile file if not existent returns number of entries in mDb

int IO::createDb(const char* dbName, const char* inputList) 
{
 
  /// fill db into list
  addDb(dbName);
  //showDb();
  // open db to add entries
  ofstream dbFile(dbName, std::ios::app);
  // open inputList

  // check streams
  ifstream in(inputList);

  if (!in || !dbFile) {
    cout << "Could not open file" << endl;
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
        TFile *f1 = new TFile(fileName);
        TTree *tree = (TTree*)f1->Get("StHiMicroTree");
        if (tree) {
          Stat_t nentries = tree->GetEntries();
          dbFile << fileName << "  " << nentries << endl;
          cout << " added with " << nentries << " entries " << endl;
          count++;
        }
        delete f1;
        delete tree;
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

*/
