/***************************************************************************
 *
 * $Id: StHTListMasker.h,v 1.1 2004/03/25 19:03:51 thenry Exp $
 * 
 * Author: Thomas Henry March 2004
 ***************************************************************************
 *
 * Description:  Masker (Maker) which masks out hot towers.
 *
 ***************************************************************************
 *
 * Revision 1.0  2003/02/20 thenry
 *
 **************************************************************************/

#ifndef HTListMasker_h
#define HTListMasker_h
#include <map>
#include <fstream>
#include <string>
#include <iostream>
#include <vector>
#include <typeinfo>
#include <set>

typedef set<unsigned int, less<unsigned int> > towerSet;
typedef map<long, towerSet, less<long> > runSetMap;

class StHTListMasker : public SafetyArray {
 public:
  towerSet badTowers;
  runSetMap badRunTowers;
   
 private:
  string HTListName;

 public:
  StHTListMasker(const char* name,
		   const char* listName);

  virtual Int_t Make();
  virtual Int_t Init();
  virtual Int_t Finish();

  virtual bool isGood(unsigned int runNumber, long index);

  ClassDef(StHTListMasker,0)
};

#endif






