/***************************************************************************
 *
 * $Id: TowerSanityMaker.h,v 1.2 2003/09/04 18:54:24 thenry Exp $
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

class CumuTowerException
{
 public:
  string Msg;
 public:
  CumuTowerException(string str) : Msg(str) {};
};

class towerIdMismatchException : public CumuTowerException
{
 public:
  towerIdMismatchException() : CumuTowerException("Tower Id mismatch!"){};
};

class eofTowerException : public CumuTowerException
{
 public:
  eofTowerException() : CumuTowerException("EOF Exception!"){};
};

class tripower
{
 public:
  long towerId;
  long events;
  double sum;
  double sumsqr;
  double max;
  tripower() : towerId(0), events(0), sum(0), sumsqr(0), max(0) {};
  tripower(const tripower& tr) : 
    towerId(tr.towerId), events(tr.events), 
    sum(tr.sum), sumsqr(tr.sumsqr), max(tr.max) {};
  tripower(long ptowerId, long pevents, double psum, double psumsqr) :
    towerId(ptowerId), events(pevents), 
    sum(psum), sumsqr(psumsqr), max(0) {};
  tripower(long ptowerId, double energy) :
    towerId(ptowerId), events(1), 
    sum(energy) { sumsqr = energy*energy; max = energy; };

  void add(const tripower& tr) 
    {
      if(towerId == 0)
	towerId = tr.towerId;
      if(towerId != tr.towerId) 
	throw towerIdMismatchException();
      addAny(tr);
    };

  void addAny(const tripower& tr) 
    {
      events += tr.events; sum += tr.sum; sumsqr += tr.sumsqr;
      if(tr.max > max) max = tr.max;
    };

  void set(long ptowerId, double penergy) 
    {
      towerId = ptowerId;
      events = 1;
      sum = penergy;
      sumsqr = sum*sum;
      max = penergy;
    };

  double average(void) { if(events) return sum/events; else return 0; };
  double stddev(void) { 
    if(events) return sqrt(sumsqr/events - average()*average());
    else return 0;
  };
};

typedef map<long, tripower, less<long> > tdbType;

class TowerDB
{
 public:
  tdbType TDB;
  tripower totals;
 public:
  TowerDB() {};
  TowerDB(const TowerDB& tower) : TDB(tower.TDB) { };
  ~TowerDB() { clear(); };
  void clear(void) { 
    TDB.clear();
  };
  void add(const tripower& tr) {
    TDB[tr.towerId].add(tr);
    totals.addAny(tr);
  };
  void add(const TowerDB& db)
    {
      for(tdbType::const_iterator it = db.TDB.begin(); 
	  it != db.TDB.end(); ++it)
	add((*it).second);
      totals.addAny(db.totals);
    };
  istream &load(istream &is){
    if(is.eof()) throw eofTowerException();
    is.read(reinterpret_cast<char*>(&totals), sizeof(totals));
    unsigned long lsize = 0;
    is.read(reinterpret_cast<char*>(&lsize), sizeof(lsize));
    tripower tempload;
    for(unsigned long i = 0; i < lsize; i++)
      {
	is.read(reinterpret_cast<char*>(&tempload), sizeof(tempload));
	add(tempload);
      }
    return is;
  };
  ostream &save(ostream &os){
    os.write(reinterpret_cast<char*>(&totals), sizeof(totals));
    unsigned long lsize = TDB.size();
    os.write(reinterpret_cast<char*>(&lsize), sizeof(lsize));
    for(tdbType::iterator it = TDB.begin(); it != TDB.end(); ++it)
      {
	tripower tr = (*it).second;
	os.write(reinterpret_cast<char*>(&tr), sizeof(tr));
      }
    return os;
  };
};

typedef map<long, TowerDB, less<long> > edbType;

class EMCRunDB
{
 public:
  edbType EMCDB;
 public:
  EMCRunDB() {};
  EMCRunDB(const EMCRunDB& pEMCDB) : EMCDB(pEMCDB.EMCDB) {};
  ~EMCRunDB() { clear(); };
  void clear(void) {
    EMCDB.clear();
  };
  void add(const tripower& tr, long runNumber) {
    EMCDB[runNumber].add(tr);
  };
  void add(const TowerDB &db, long runNumber) {
    EMCDB[runNumber].add(db);
  };
  void add(const EMCRunDB &db)
    {
      for(edbType::const_iterator it = db.EMCDB.begin(); 
	  it != db.EMCDB.end(); ++it)
	EMCDB[(*it).first].add((*it).second);
    };
  istream &load(istream &is){
    if(is.eof()) throw eofTowerException();
    unsigned long lsize = 0;
    is.read(reinterpret_cast<char*>(&lsize), sizeof(lsize));
    TowerDB tempload;
    for(unsigned long i = 0; i < lsize; i++)
      {
	long runNumber = 0;
	is.read(reinterpret_cast<char*>(&runNumber), sizeof(runNumber));
	tempload.clear();
	tempload.load(is);
	add(tempload, runNumber);
      }
    return is;
  };
  ostream &save(ostream &os){
    unsigned long lsize = EMCDB.size();
    os.write(reinterpret_cast<char*>(&lsize), sizeof(lsize));
    for(edbType::iterator it = EMCDB.begin(); it != EMCDB.end(); ++it)
      {
	long runNumber = (*it).first;
	os.write(reinterpret_cast<char*>(&runNumber), sizeof(runNumber));
	((*it).second).save(os);
      }
    return os;
  };
};


#endif



#ifdef __ROOT__
#ifndef TowerSanityMaker_h
#define TowerSanityMaker_h

#include "StEmcTpcFourPMaker.h"

class TowerSanityMaker : public SafetyArray {
 public:
  enum dataBaseUse {toRead=0, toWrite=1, toMerge=2};
   
 private:
  StEmcADCtoEMaker* adcToE;
  StMuDstMaker* muDstMkr;
  const char* dbName;
  dataBaseUse dbUse;
  ofstream* ofile;
  EMCRunDB TowerSanity;

  double runThreshold;
  double maxTowerThreshold;
  double avgTowerThreshold;

 public:
  TowerSanityMaker(const char* name,
		   StEmcADCtoEMaker* adcToEMaker,
		   StMuDstMaker* uDstMkr,
		   const char* dataBaseName, 
		   dataBaseUse dbu);

  virtual Int_t Make();
  virtual Int_t Init();
  virtual Int_t Finish();

  virtual bool isGood(unsigned int runNumber, long index);

  double runAverageEnergyPerTower(long runNumber) { 
    return TowerSanity.EMCDB[runNumber].totals.average(); 
  };
  double runStandardDeviation(long runNumber) { 
    return TowerSanity.EMCDB[runNumber].totals.stddev(); 
  };
  double averageTowerEnergy(long runNumber, long index){
    return TowerSanity.EMCDB[runNumber].TDB[index].average();
  };
  double maxTowerEnergy(long runNumber, long index){
    return TowerSanity.EMCDB[runNumber].TDB[index].max;
  };
  double towerStdDev(long runNumber, long index){
    return TowerSanity.EMCDB[runNumber].TDB[index].stddev();
  };

  void setRunThreshold(double runThresh) { runThreshold = runThresh; };
  void setMaxTowerThreshold(double maxTowerThresh) { 
    maxTowerThreshold = maxTowerThresh; };
  void setAvgTowerThreshold(double avgTowerThresh) {
    avgTowerThreshold = avgTowerThresh; };
    
  ClassDef(TowerSanityMaker,1)
};

#endif
#endif






