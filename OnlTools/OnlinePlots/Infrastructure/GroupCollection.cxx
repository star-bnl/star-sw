#include "GroupCollection.h"
#ifndef PR
# define PR(x) { cout << __PRETTY_FUNCTION__ << " ### " << #x << " ### " << x << endl;}
#endif


#include "GroupStatisticsHistogramGroup.h"
#include "UPCHistogramGroup.h"
#include "UPCHistogramCtbGroup.h"
#include "UPCHistogramZdcGroup.h"
#include "LaserDriftTimeHistogramGroup.h"
#include "LaserTpcPadChargeHistogramGroup.h"
#include "L2UpsilonCountsHistogramGroup.h"
#include "L2UpsilonTowersHistogramGroup.h"
#include "L2UpsilonMassHistogramGroup.h"
#include "VPDHistogramGroup.h"
#include "FMSHistogramGroup.h"
#include "pp2ppHistogramGroup.h"
//#include "HighTowerFlensburgHistogramGroup.h"
#include "SVTAnodeHybridHistogramGroup.h"
#include "SVTAnodeSumHistogramGroup.h"
#include "SsdPedestalHistogramGroup.h"

#include "TLatex.h"


// see file /RTS/conf/handler/evpGroups.txt for available trigger groups
// see file /RTS/include/rtsSystems.h for available detectors  /RTS/src/EVP_READER/cfgutil.h

void GroupCollection::serverCreate() {
  insert( new GroupStatisticsHistogramGroup("Trigger") );
  insert( new LaserDriftTimeHistogramGroup("Laser","driftTime","laser","any") );
  insert( new LaserTpcPadChargeHistogramGroup("Laser","driftTime","laser","any") );
  insert( new L2UpsilonCountsHistogramGroup("L2 upsilon","Counts","upsilon","any") );
  insert( new L2UpsilonTowersHistogramGroup("L2 upsilon","Towers","upsilon","any") );
  insert( new L2UpsilonMassHistogramGroup("L2 upsilon","Mass","upsilon","any") );
  //insert( new HighTowerFlensburgHistogramGroup("L2 upsilon","Flensburg","upsilon","any") );
  insert( new SVTAnodeSumHistogramGroup("SVT","Anode Sum","any","svt") );
  insert( new SsdPedestalHistogramGroup("Ssd", "Pedestal", "pedestal", "ssd") );  
  insert( new UPCHistogramCtbGroup("Upc","Ctb","upc","any") );
  insert( new UPCHistogramZdcGroup("Upc","Zdc","upc","any") );
  insert( new VPDHistogramGroup("Trigger","vpd","any","trg") );
  insert( new FMSHistogramGroup("Trigger","fms","any","trg") );
  insert( new pp2ppHistogramGroup(0, "P2P","Trg1-16","any","trg") );
  insert( new pp2ppHistogramGroup(1, "P2P","Trg17-32","any","trg") );

  char sub[1024];
  for ( unsigned int i=0; i< 14; i++) {
    sprintf(sub,"%02d",i); 
    insert( new SVTAnodeHybridHistogramGroup(i,"SVTAnodeVsHybrid",sub,"any","svt") );
  }
}





void GroupCollection::setTriggerDetectorBits(unsigned int trig, unsigned int det) { 
  mTriggerBits = trig; 
  mDetectorBits = det;
}

unsigned int GroupCollection::numberOfActiveGroups() {
  int n=0;
  for ( GroupIterator iter = begin(); iter != end(); iter++) {
    if ( (*iter)->active() ) {
      n++;
    }
  }
  return n;
}

void  GroupCollection::remove() {
  for ( GroupIterator iter = begin(); iter != end(); iter++) {
    (*iter)->setActive(false);
    delete *iter;
  }
  clear();
} 

void  GroupCollection::save(const char* filename) {
  TFile* f = 0;
  if ( filename ) {
    f = new TFile("filename","update");
  }
  for ( GroupIterator iter = begin(); iter != end(); iter++) {
    if ( (*iter)->active() ) {
      (*iter)->Write();
    }
  }
  if ( f ) {
    f->Close();
  } 
}


void  GroupCollection::display(TCanvas* cc) {
  for ( GroupIterator iter = begin(); iter != end(); iter++) {
    if ( (*iter)->active() ) {
      cc->cd();
      (*iter)->draw(cc);
      cc->Update();
    }
  }
}

void  GroupCollection::print(TCanvas* cc, const char* filename) {
  for ( GroupIterator iter = begin(); iter != end(); iter++) {
    if ( (*iter)->active() ) {
      cc->cd(0);
      (*iter)->draw(cc);
      printName(cc,(*iter));
      cc->Print(filename);
    }
  }
}

void GroupCollection::printName(TCanvas* cc, HistogramGroup* gr) {
  //char txt[1024];
  cc->cd(0);
  TLatex* tl; 
  // run number 
  //sprintf(txt,"",mRS->getRunNumber() );
  tl = new TLatex(0.01,.99,gr->id()); 
  tl->SetTextSize(0.02);
  tl->SetTextColor(2);
  tl->Draw();
}


void  GroupCollection::fill(evpReader* evp, char* datap, unsigned int triggerBits,  unsigned int detectorBits) {
  for ( GroupIterator iter = begin(); iter != end(); iter++) {
    if ( (*iter)->testBits( triggerBits, detectorBits) ) {
      (*iter)->fill(evp,datap);
    }
  }
}

void  GroupCollection::reset() {
  for ( GroupIterator iter = begin(); iter != end(); iter++) {
    (*iter)->reset();
  }
}

void  GroupCollection::beginRun(evpReader* evp, char* datap) {
  for ( GroupIterator iter = begin(); iter != end(); iter++) {
    (*iter)->beginRun(evp, datap);
  }
}

void  GroupCollection::endRun() {
  for ( GroupIterator iter = begin(); iter != end(); iter++) {
    (*iter)->endRun();
  }
}

void  GroupCollection::finish() {
  for ( GroupIterator iter = begin(); iter != end(); iter++) {
    (*iter)->finish();
  }
}

void  GroupCollection::list() {
  for ( GroupIterator iter = begin(); iter != end(); iter++) {
    cout << (*iter)->id() << endl;
  }
}

void  GroupCollection::setActive() {
  for ( GroupIterator iter = begin(); iter != end(); iter++) {
    bool act = (*iter)->testBits(mTriggerBits,mDetectorBits);
    (*iter)->setActive( act );
    //cout << " set active " <<  (*iter)->id() << "    " << act << "   " << (*iter)->active() << endl;
  }
}

void  GroupCollection::addToMapFile(TMapFile*  mapfile) {
  if ( !mapfile ) return; 
  for ( GroupIterator iter = begin(); iter != end(); iter++) {
    mapfile->Add( (*iter), (*iter)->id() );
  }
  mapfile->Update();
}


void  GroupCollection::push(TMapFile* mapfile) {
  if ( !mapfile ) return; 
  for ( GroupIterator iter = begin(); iter != end(); iter++) {
    if ( (*iter)->active() ) {
      mapfile->Update( (*iter) );
    }
  }
}


void GroupCollection::read(GenericFile* genfile) {
  if ( genfile->mapFile() ) {
    read( genfile->mapFile() );
    return;
  }
  if ( genfile->hisFile() ) {
    read( genfile->hisFile() );
    return;
  }
}

void GroupCollection::read(TFile* file) {
  if ( !file ) {
    cout << __PRETTY_FUNCTION__ << " no file set " << endl;
    return;
  }
  TFileIter* iter = new TFileIter(file);
  TObject* obj;
  while ( (obj=iter->Next()) !=0 ) {
    //cout << obj->GetName() << endl;
    if ( obj && obj->InheritsFrom("HistogramGroup") ) {
      HistogramGroup* hg = (HistogramGroup*) file->Get( obj->GetName() );
      //cout <<__PRETTY_FUNCTION__ << " " << hg->GetName() << endl;
      pair<GroupIterator,bool> ret = insert ( hg  );
      if ( !ret.second ) { // already existed in list
	//cout << __PRETTY_FUNCTION__ << " " << (*(ret.first))->id() << " already exists in list " << endl;
	erase(ret.first);
	insert(hg);
      }
    }
    delete obj;
  }
  cout << __PRETTY_FUNCTION__ << " ### " << size() << " groups read from file  " << endl;
}

void GroupCollection::read(TMapFile* mapfile) {
  if ( !mapfile ) {
    cout << __PRETTY_FUNCTION__ << " no map file set " << endl;
    return;
  }
  TMapRec* mr = mapfile->GetFirst();
  while (mr ) {
    //cout << hex << mr << " " << last << dec << endl;
    TObject* obj = mapfile->Get( mr->GetName() );
    if ( obj && obj->InheritsFrom("HistogramGroup") ) {
      //HistogramGroup* hg = (HistogramGroup*) obj;
      HistogramGroup* hg = (HistogramGroup*) get( mr->GetName() );
      //cout <<__PRETTY_FUNCTION__ << " " << mr->GetName() << endl;
      hg = (HistogramGroup*)mapfile->Get( mr->GetName(),hg );
      pair<GroupIterator,bool> ret = insert ( hg  );
      if ( !ret.second ) { // already existed in list
	//cout << __PRETTY_FUNCTION__ << " " << (*(ret.first))->id() << " already exists in list " << endl;
	erase(ret.first);
	insert(hg);
      }
    }
    delete obj;
    mr = mr->GetNext();
  }
  cout << __PRETTY_FUNCTION__ << " ### " << size() << " groups read from file  " << endl;
}


/*
HistogramGroup* GroupCollection::get(unsigned int index) {
  if ( index<size() ) return 0;
  HistogramGroup* group = at(index);
  return group;
}
*/
/*
HistogramGroup* GroupCollection::read(TMapFile* mapfile, unsigned int index) {
  HistogramGroup* group = get(index);
  if (!group) return 0;
  group = (HistogramGroup*) mapfile->Get( group->id(), group );
  return group;
}
*/

HistogramGroup* GroupCollection::get(const char* name) {
  //cout << __PRETTY_FUNCTION__ << " number of group " << size() << endl;
  for( GroupIterator iter = begin(); iter != end(); iter++) {
    if ( strcmp((*iter)->id(),name)==0 ) {
      return (*iter);
    }
  }
  cout << __PRETTY_FUNCTION__ << " " << name << " not found " << endl;
  return 0;
}

HistogramGroup* GroupCollection::read(TMapFile* mapfile, const char* name) {
  //cout << __PRETTY_FUNCTION__ << " " << name <<  endl;
  for( GroupIterator iter = begin(); iter != end(); iter++) {
    if ( strcmp((*iter)->id(),name)==0 ) {
      HistogramGroup* hg = (*iter);
      erase(*iter);
      hg = (HistogramGroup*) mapfile->Get( name , hg);
      insert(hg);
      return hg;
    }
  }
  cout << __PRETTY_FUNCTION__ << " " << name << " not found " << endl;
  return 0;
}


#include <utility>
using namespace std;

GroupMap::GroupMap( GroupCollection& coll ) {
  for( GroupIterator iter = coll.begin(); iter != coll.end(); iter++) {
    string name( (*iter)->groupName() );
    GroupCollection groups;
    pair<GroupMapIterator, bool> ret;
    ret =  insert( make_pair(name, groups) );
    (*(ret.first)).second.insert( (*iter) );
  } 
}

void GroupMap::print() {
    for( GroupMapIterator mapIter = begin(); mapIter != end(); mapIter++) {
      cout << (*mapIter).first.c_str() << endl;
      for( GroupIterator groupIter = (*mapIter).second.begin(); groupIter != (*mapIter).second.end(); groupIter++) {
	cout << "\t " << (*groupIter)->subGroupName() << endl;;
      }
    }
}

