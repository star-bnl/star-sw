/**********************************************************************
 *
 * $Id: StEStructHAdd.h,v 1.1 2004/07/01 00:37:17 porter Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description: Simple helper class for adding hists from cut selectoins
  *
 ***********************************************************************/
#ifndef __STESTRUCTHADD_H
#define __STESTRUCTHADD_H


#include "TROOT.h"
class TFile;

class StEStructHAdd : public TObject {

 protected:

 public:

  StEStructHAdd(){};
  ~StEStructHAdd(){};

  void addCuts(const char* outfile, TFile * tf, int* nlist, int num);
  void addCuts(const char* outfile, const char* infile, int* nlist, int num);


  ClassDef(StEStructHAdd,1)

};


#endif
/***********************************************************************
 *
 * $Log: StEStructHAdd.h,v $
 * Revision 1.1  2004/07/01 00:37:17  porter
 * new code previously my StEStructHelper. Takes hists from correltation
 * pass and builds final ressults.  Also the StEStructHAdd.h is a simple
 * replacemnt for my sumyt.C macro which could be expanded later as needed.
 *
 *
 *
 *********************************************************************/

