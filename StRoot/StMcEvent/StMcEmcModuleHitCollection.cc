/***************************************************************************
 *
 * $Id $
 *
 * Author: Aleksei Pavlinov, May 2000
 ***************************************************************************
 *
 * Description: Monte Carlo Emc Module Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcEmcModuleHitCollection.cc,v $
 * Revision 2.2  2000/08/30 14:52:03  calderon
 * New changes made by Aleksei.
 *
 * Revision 2.1  2000/06/06 23:01:09  calderon
 * Inital revision
 *
 *
 **************************************************************************/
#include "StMcEmcModuleHitCollection.hh"
#include "StMcCalorimeterHit.hh"

static const char rcsid[] = "$Id: StMcEmcModuleHitCollection.cc,v 2.2 2000/08/30 14:52:03 calderon Exp $";

#ifdef PERSISTENT
ClassImp(StMcEmcModuleHitCollection)
#endif

StMcEmcModuleHitCollection::StMcEmcModuleHitCollection(const unsigned int m)  
{init(m);}

StMcEmcModuleHitCollection::StMcEmcModuleHitCollection()  
{init(0);}

void StMcEmcModuleHitCollection::init(const unsigned int m)  
{
  //
  // m - index in container
  //
  char name[10];
  sprintf(name,"m%3.3i",m+1);
  SetName(name);
}

StMcEmcModuleHitCollection::~StMcEmcModuleHitCollection()
{
    for (unsigned int i=0; i<mHits.size(); i++) {
        delete mHits[i];
        mHits[i] = 0;
    }
    mHits.clear();
}

unsigned long 
StMcEmcModuleHitCollection::numberOfHits() const
{
    return mHits.size();
}

float 
StMcEmcModuleHitCollection::sum() const
{
    float s = 0.0;
    for(unsigned int i=0; i<mHits.size(); i++){
	s += (*mHits[i]).dE();
    }
    return s;
} 

const StSPtrVecMcCalorimeterHit&
StMcEmcModuleHitCollection::hits() const { return mHits; }

StSPtrVecMcCalorimeterHit&
StMcEmcModuleHitCollection::hits() { return mHits; }

void StMcEmcModuleHitCollection::Browse(TBrowser *b)
{
  int nh = mHits.size();
  if (nh > 0) {
    cout<<" == " << GetName()<<" == hits "<<nh<<" == sum Energy " << sum()<<"\n";
    for (int ih=0; ih<nh; ih++){
      StMcCalorimeterHit *hitEmc = mHits[ih];
      cout<<"   "<<ih+1<<" "<<(*hitEmc);
    }
  }
}
