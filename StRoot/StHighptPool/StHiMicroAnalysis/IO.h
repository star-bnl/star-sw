/***************************************************************************
 *
 * $Id: IO.h,v 1.1 2002/04/02 20:05:17 jklay Exp $
 *
 * Author: Bum Choi, UT Austin, Apr 2002
 *
 ***************************************************************************
 *
 * Description:  Useful utility to chain together root files with highpt
 *		 uDST's for analysis
 *
 *
 ***************************************************************************
 * 
 * $Log: IO.h,v $
 * Revision 1.1  2002/04/02 20:05:17  jklay
 * Bums analysis tools for highpt uDSTs
 *
 *
 **************************************************************************/
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
