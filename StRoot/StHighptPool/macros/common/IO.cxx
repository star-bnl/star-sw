
#include "IO.h"
#include "TChain.h"
#include "TSystem.h"
#include <iostream>

ClassImp(IO)

IO::IO(const char* dir, const char* ext) 
  : mNFile(0), mDir(dir), mExt(ext) {}

IO::~IO() {}

void
IO::chain(TChain* chain)
{
  void *pDir = gSystem->OpenDirectory(mDir.Data());
  if(!pDir){
    cerr << "##Cannot open directory " << mDir.Data() << endl;
    cerr << "##Goodbye" << endl;
    exit(1);
  }
  
  cout << "\tUsing directory : " << mDir.Data() << endl;
  cout << "\tMatch extension : " << mExt.Data() << endl;
  if(mNFile) cout << "\tMaximum # files : " << mNFile << endl;

  //
  // now find the files that end in the specified extension
  //
  const char* fileName(0);
  Int_t count(0);

  while((fileName = gSystem->GetDirEntry(pDir))){
    if(strcmp(fileName,".")==0 || strcmp(fileName,"..")==0) continue;

    if(strstr(fileName,mExt.Data())){ // found a match
      char* fullFile = gSystem->ConcatFileName(mDir.Data(),fileName);

      // add it to the chain
      cout << "\tAdding " << fullFile << " to the chain" << endl;

      chain->Add(fullFile); count++;
      delete fullFile;
      if(mNFile && count > mNFile) break;
    }   
  }
  cout << "Added " << count << " files to the chain" << endl;

}
