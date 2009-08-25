/***************************************************************************
 *
 * $Id $
 *
 * Author: Aleksei Pavlinov, May 2000
 ***************************************************************************
 *
 * Description: Monte Carlo Emc Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcEmcHitCollection.cc,v $
 * Revision 2.12  2009/08/25 20:57:54  fine
 * fix the compilation issues under SL5_64_bits  gcc 4.3.2
 *
 * Revision 2.11  2007/10/05 00:01:20  calderon
 * Changes to include a EMC hit collection that does not care about
 * parent tracks, so that now there are two collections.  This
 * new collection will be useful to compare all the deposited energy in a hit tower
 * in a given event. The information about the track parentage is still
 * kept in the original collection unchanged.
 *
 * Revision 2.10  2007/03/29 15:34:11  fisyak
 * comment out print out
 *
 * Revision 2.9  2005/11/22 21:44:51  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.8  2005/09/28 21:30:14  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.7  2005/08/09 03:30:30  perev
 * Cleanup
 *
 * Revision 2.6  2005/06/28 18:44:11  fine
 * fix assert
 *
 * Revision 2.5  2005/06/28 18:06:41  fine
 * Remove the redundant data-member StMcEmcModuleHitCollection mModules[mNumberOfModules] causing the crash duw double destruction of one and the same object
 *
 * Revision 2.4  2005/01/27 23:40:47  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.3  2001/05/13 21:12:10  calderon
 * Modifications by Aleksei to the Emc Hit Collections on indexing of
 * module numbers
 *
 * Revision 2.2  2000/08/30 14:52:02  calderon
 * New changes made by Aleksei.
 *
 * Revision 2.1  2000/06/06 23:01:09  calderon
 * Inital revision
 *
 *
 **************************************************************************/
#include "StMcEmcHitCollection.hh"
#include "StMcEmcModuleHitCollection.hh"
#include "StMcCalorimeterHit.hh"
#include "StMcTrack.hh"
#include "StParticleDefinition.hh"
#include "StMessMgr.h"
#include "TObjectSet.h"
#include "TDataSetIter.h"
#include <cassert>

static const char rcsid[] = "$Id ";

ClassImp(StMcEmcHitCollection);

StMcEmcHitCollection::StMcEmcHitCollection():TDataSet("emcHits",0,kTRUE) { MakeHitCollection(); }

StMcEmcHitCollection::StMcEmcHitCollection(char* name):TDataSet(name,0,kTRUE) {MakeHitCollection();}
StMcEmcHitCollection::StMcEmcHitCollection(const char* name):TDataSet(name,0,kTRUE) { MakeHitCollection();}

void 
StMcEmcHitCollection::MakeHitCollection() 
{
   MakeCollection();
   TObjArray *modules = GetObjArray();
   assert(modules);
   // Create the dummy modules collection
   for (int i=0; i < mNumberOfModules; i++) 
      Add(new StMcEmcModuleHitCollection());
}
StMcEmcHitCollection::~StMcEmcHitCollection() 
{ /* noop */ }
    
StMcEmcHitCollection::EAddHit 
StMcEmcHitCollection::addHit(StMcCalorimeterHit* hit)
{
    if(!hit) { LOG_ERROR << "tried to add a NULL hit to StMcEmcHitCollection" << endm; }
    
    unsigned int m = hit->module();
    unsigned int i = m-1;
    
    if(m<=mNumberOfModules && m>=1) {
        StMcCalorimeterHit *detectorHit = new StMcCalorimeterHit(hit->module(), hit->eta(), hit->sub(), hit->dE());
        for(unsigned int ih=0; ih<thisModule(i).numberOfDetectorHits(); ih++) {
            if((*thisModule(i).detectorHits()[ih]) == (*detectorHit)) {
                (*thisModule(i).detectorHits()[ih]) += (*detectorHit);
                delete detectorHit; detectorHit = NULL;
                break;
            }
        }
        if(detectorHit) { // hit on a new element
            thisModule(i).detectorHits().push_back(detectorHit);
        }
    }
    
    if (m<=mNumberOfModules && m>=1) {
        if(thisModule(i).numberOfHits() == 0) {
            thisModule(i)(m); // Set name
            thisModule(i).hits().push_back(hit); // New hit(first)
            return kNew;
        }
        else {
            for(unsigned int ih=0; ih<thisModule(i).numberOfHits(); ih++){
                if((*thisModule(i).hits()[ih]) == (*hit)) { // Hits from the same particle
                    (*thisModule(i).hits()[ih])  += (*hit);
                    return kAdd;
                }
            }
            thisModule(i).hits().push_back(hit); // New hit
            return kNew;
        }
    }
    else {
        Warning("addHit","Wrong hit: module=%d but mNumberOfModules=%d",m,mNumberOfModules);
        return kErr; // Bad number of module
    }
}

unsigned long
StMcEmcHitCollection::numberOfHits() const
{
    unsigned long sum = 0;
    for (int i=0; i<mNumberOfModules; i++) {
	sum += thisModule(i).hits().size();
    }
    return sum;
}

float 
StMcEmcHitCollection::sum() const
{
    float s = 0.0;
    unsigned int i, m;
    for(i=0; i < mNumberOfModules; i++){
      m  = i + 1;
      s += module(m)->sum();
    }
    return s;
}

StMcEmcModuleHitCollection*
StMcEmcHitCollection::module(unsigned int m)
{
// m - module number; i-index number;  i = m - 1; // 10-may-2001 
   TObjArray &modules = *GetObjArray();
   unsigned int i = m - 1;
    if (i>=0 && i < mNumberOfModules)
        return (StMcEmcModuleHitCollection *)modules[i];
    else
        return 0;
}

const StMcEmcModuleHitCollection*
StMcEmcHitCollection::module(unsigned int m) const
{
    TObjArray &modules = *GetObjArray();
    unsigned int i = m - 1;
    if (i>=0 && i < mNumberOfModules)
        return (StMcEmcModuleHitCollection *)modules[i];
    else
        return 0;
}

void
StMcEmcHitCollection::Browse(TBrowser *b)
{
  TDataSet::Browse(b);
  print();
}

void StMcEmcHitCollection::print() {/*  cout << *this << endl; */}
//________________________________________________________________________________
ostream&  operator<<(ostream& os, const StMcEmcHitCollection &emcColl) {
  os<<"-----------\n"<<emcColl.GetName()<<" has "<<emcColl.numberOfHits()<<" hits in "
      <<emcColl.GetListSize()<<" modules. Deposit Energy "<<emcColl.sum()<<" GeV \n-----------\n";
  unsigned int i, m;
  for (i=0; i<emcColl.numberOfModules(); i++) {
    m = i + 1;
    const StSPtrVecMcCalorimeterHit& hits = emcColl.thisModule(i).hits();
    int nh = hits.size();
    if (nh > 0) {
      os<<" " <<emcColl.module(m)->GetName()<<" #hits "<<nh<<" Dep.Energy " << 
      emcColl.module(m)->sum()<< endl;
      os << *(emcColl.module(m)->hits())[0];
      os << "Parent track of this Hit" << endl;
      StMcTrack *track = (emcColl.module(m)->hits())[0]->parentTrack();
      if (track) os << *track << endl;
    }
  }
  return os;
}
//________________________________________________________________________________
void StMcEmcHitCollection::Print(Option_t *option) const {
  TString Option(option);
  if (Option.Contains("all",TString::kIgnoreCase)) {
    cout<<"-----------\n"<<GetName()<<" has "<<numberOfHits()<<" hits in "
	<<GetListSize()<<" modules. Deposit Energy "<<sum()<<" GeV \n-----------\n";
    unsigned int i, m;
    for (i=0; i<numberOfModules(); i++) {
      m = i + 1;
      const StSPtrVecMcCalorimeterHit& hits = thisModule(i).hits();
      int nh = hits.size();
      if (nh > 0) {
	cout<<" " <<module(m)->GetName()<<" #hits "<<nh<<" Dep.Energy " << 
	  module(m)->sum()<< endl;
	for (int j = 0; j < nh; j++) {
	  StMcCalorimeterHit *hit = module(m)->hits()[j];
	  cout << *hit;
	  if ( hit->parentTrack()) {
	    cout << " key " << hit->parentTrack()->key();
	  }
	  cout << endl;
	}
      }
    } 
  } else cout << *this;
}
