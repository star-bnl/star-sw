/*
  just chains root files


 */
#ifndef IO_H
#define IO_H

#include "TString.h"
#include "TObject.h"
class TChain;

class IO : public TObject{
 public:
  IO(const char* dir="./", const char* ext=".root");
  ~IO();

  void setNFile(int n) { mNFile = n; }
  void chain(TChain* chain);
 
 private:
  int mNFile;
  TString mDir;
  TString mExt;

  ClassDef(IO,1)

};

#endif
