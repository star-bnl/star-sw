/***************************************************************************
 *
 * $Id: TowerSanityMaker.h,v 1.1 2003/08/28 16:53:06 thenry Exp $
 * 
 * Author: Thomas Henry August 2003
 ***************************************************************************
 *
 * Description:  Maker which creates a database of average tower values per
 * run, accessible by runnumber or eventid and towerid
 *
 ***************************************************************************
 *
 * Revision 1.0  2003/02/20 thenry
 *
 **************************************************************************/

#ifndef CumuTower_h
#define CumuTower_h
#include <map>
#include <fstream>
#include <string>
#include <iostream>
#include <vector>
#include <typeinfo>
using namespace std;

class tripower
{
 public:
  long events;
  double sum;
  double sumsqr;
  tripower() : events(0), sum(0), sumsqr(0) {};
  tripower(tripower& tr) : 
    events(tr.events), sum(tr.sum), sumsqr(tr.sumsqr) {};

  void Add(tripower& tr) { 
    events += tr.events; sum += tr.sum; sumsqr += tr.sumsqr; };

  double average(void) { if(events) return sum/events; else return 0; };
  double stddev(void) { 
    if(events) return sumsqr/events - average();
    else return 0;
  };
};

class TowerDB : map<long, tripower, less<long> >
{
};

class MultiRunTowerDB : map<long, TowerDB, less<long> >
{
};

#endif



#ifdef __ROOT__
#ifndef TowerSanityMaker_h
#define TowerSanityMaker_h

#include "StEmcTpcFourPMaker.h"

class TowerSanityMaker : public SafetyArray {
 public:
  enum dataBaseUse {toRead=0, toWrite=1};
   
 private:
  const char* dbName;
  dataBaseUse dbUse;

 public:
  TowerSanityMaker(const char* name, const char* dataBaseName, 
		   dataBaseUse dbu) : 
    SafetyArray(name), dbName(dataBaseName), dbUse(dbu) {};

  virtual Int_t Make();
  virtual Int_t Init();
  virtual Int_t Finish();

  virtual bool isGood(unsigned int runNumber, long index) { return true; };
    
  ClassDef(TowerSanityMaker,1)
};

#endif
#endif






