#ifndef GroupCollection_h
#define GroupCollection_h

#ifndef NEW_DAQ_READER
  class evpReader;
#endif
class TMapFile;

#include "HistogramGroup.h"
#include "EvpUtil.h"

#include <map>
#include <set>
#include <list>
#include <vector>
#include <string>
using namespace std;

#include "Rtypes.h"
#include "TMapFile.h"
#include "GenericFile.h"
#include "TFileIter.h"

class TCanvas;

struct Alpha {
  bool operator()( HistogramGroup* g1, HistogramGroup* g2) const {
    return strcmp( g1->id(), g2->id() ) < 0;
  }
};

typedef set<HistogramGroup*,Alpha>::iterator GroupIterator;


class GroupCollection : public set<HistogramGroup*,Alpha > {
 public:
  void setTriggerDetectorBits(unsigned int trig, unsigned int det);
  void fill(evpReader* evp, char* datap, unsigned int trig, unsigned int det);
  void beginRun(evpReader* evp, char* datap);
  void endRun();
  void finish();
  void reset();
  void remove();
  void setActive();
  void addToMapFile(TMapFile*);  
  void list();
  void print(TCanvas* cc, const char* filename);
  void display(TCanvas*);
  void push(TMapFile*);
  void read(TMapFile*);
  void read(TFile*);
  void read(GenericFile*);
  void save(const char* filename=0);
  unsigned int numberOfActiveGroups();
  HistogramGroup* get(const char* group);   // find the group in group vector
  HistogramGroup* read(TMapFile*, const char* group);   // read that group from memory mapped file
  HistogramGroup* read(GenericFile*, const char* group);
  HistogramGroup* read(TFile*, const char* group); 
  void serverCreate();
 private:
  void printName(TCanvas* cc, HistogramGroup* gr);
  unsigned int mTriggerBits;
  unsigned int mDetectorBits;

};


// This maps key is the group name, it's value is a vector of groups
//class GroupMap : public map<string, set<HistogramGroup*,Alpha> > {
class GroupMap : public map<string, GroupCollection > {
 public:
  GroupMap( GroupCollection& coll );
  //HistogramGroup* get(const char* group, unsigned int index, TMapFile* mapfile);
  void print();
};

typedef map<string, GroupCollection >::iterator GroupMapIterator;

#endif
