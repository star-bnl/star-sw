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
 * Revision 2.1  2000/06/06 23:01:09  calderon
 * Inital revision
 *
 *
 **************************************************************************/
#include "StMcEmcHitCollection.hh"
#include "StMcEmcModuleHitCollection.hh"
#include "StMcCalorimeterHit.hh"

static const char rcsid[] = "$Id ";

#ifdef PERSISTENT
ClassImp(StMcEmcHitCollection)
#endif

StMcEmcHitCollection::StMcEmcHitCollection():TDataSet("emcHits") { /* noop */ }

StMcEmcHitCollection::StMcEmcHitCollection(char* name):TDataSet(name) { /* noop */ }

StMcEmcHitCollection::~StMcEmcHitCollection() 
{ /* noop */ }
//{cout<<"Delete "<<GetName()<<" \n";}
    
StMcEmcHitCollection::EAddHit 
StMcEmcHitCollection::addHit(StMcCalorimeterHit* hit)
{
    unsigned int m, i;
    if (hit && (m = hit->module()-1) < mNumberOfModules) {
      if(mModules[m].numberOfHits() == 0) {
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
  cout<<"\n  "<<numberOfHits()<<" hits for "<<GetName()<<" sum Energy "<<sum()<<"\n";
  for (int m=0; m<mNumberOfModules; m++) {
    StSPtrVecMcCalorimeterHit& hits=mModules[m].hits();
    int nh = hits.size();
    if (nh > 0) {
      cout<<" ============ "<<nh<<" hits in module "<<m+1<<" sum Energy " <<module(m)->sum()<<"\n";
      for (int ih=0; ih<nh; ih++){
        StMcCalorimeterHit *hitEmc = hits[ih];
        cout<<"   "<<ih+1<<" "<<(*hitEmc);
      }
    }
  }
}
