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
#include "TObjectSet.h"

static const char rcsid[] = "$Id ";

#ifdef PERSISTENT
ClassImp(StMcEmcHitCollection)
#endif

StMcEmcHitCollection::StMcEmcHitCollection():TDataSet("emcHits") {/* Nothing */}

StMcEmcHitCollection::StMcEmcHitCollection(char* name):TDataSet(name) {/* Nothing */}
StMcEmcHitCollection::StMcEmcHitCollection(const char* name):TDataSet(name) {/* Nothing */}

StMcEmcHitCollection::~StMcEmcHitCollection() 
{ /* noop */ }
    
StMcEmcHitCollection::EAddHit 
StMcEmcHitCollection::addHit(StMcCalorimeterHit* hit)
{
    unsigned int m, i;
    if (hit && (m = hit->module()-1) < mNumberOfModules) {
      if(mModules[m].numberOfHits() == 0) {

        mModules[m](m); // Set name
        Add((TDataSet*)(&mModules[m]));

        mModules[m].hits().push_back(hit); // New hit(first)
        return kNew;
      }
      else {
        for(i=0; i<mModules[m].numberOfHits(); i++){
          if((*mModules[m].hits()[i]) == (*hit)) { // Hits from the same particle
            (*mModules[m].hits()[i])  += (*hit);
            return kAdd;
          }
        }
        mModules[m].hits().push_back(hit); // New hit
        return kNew;
      }
    }
    else return kErr; // Bad number of module
}

unsigned int
StMcEmcHitCollection::numberOfModules() const { return mNumberOfModules; }

unsigned long
StMcEmcHitCollection::numberOfHits() const
{
    unsigned long sum = 0;
    for (int i=0; i<mNumberOfModules; i++) {
	sum += mModules[i].hits().size();
    }
    return sum;
}

float 
StMcEmcHitCollection::sum() const
{
    float s = 0.0;
    for(unsigned int m=0; m < mNumberOfModules; m++){
        s += module(m)->sum();
    }
    return s;
}

StMcEmcModuleHitCollection*
StMcEmcHitCollection::module(unsigned int i)
{
    if (i < mNumberOfModules)
        return &(mModules[i]);
    else
        return 0;
}

const StMcEmcModuleHitCollection*
StMcEmcHitCollection::module(unsigned int i) const
{
    if (i < mNumberOfModules)
        return &(mModules[i]);
    else
        return 0;
}

void
StMcEmcHitCollection::Browse(TBrowser *b)
{
  TDataSet::Browse(b);
  print();
}

void
StMcEmcHitCollection::print()
{
  //  cout<<"\n  "<<numberOfHits()<<" hits for "<<GetName()<<" sum Energy "<<sum()<<"\n";
  cout<<"-----------\n"<<GetName()<<" has "<<numberOfHits()<<" hits in "
      <<GetListSize()<<" modules. Deposit Energy "<<sum()<<" GeV \n-----------\n";
  for (int m=0; m<mNumberOfModules; m++) {
    StSPtrVecMcCalorimeterHit& hits=mModules[m].hits();
    int nh = hits.size();
    if (nh > 0) {
      cout<<" " <<module(m)->GetName()<<" #hits "<<nh<<" Dep.Energy " << 
      module(m)->sum()<<"\n";
    }
  }
}
