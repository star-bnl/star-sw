/*****************************************************************
 * $Id: StMuPmdCollection.cxx,v 1.7 2012/11/26 23:14:33 fisyak Exp $
 *
 * Class : StMuPmdCollection
 * Author: Supriya Das & Subhasis Chattopadhyay
 * ****************************************************************
 *
 * Description: This class holds the PMD clusters for MuDst
 * ****************************************************************
 * $Log: StMuPmdCollection.cxx,v $
 * Revision 1.7  2012/11/26 23:14:33  fisyak
 * Replace GetEntries() by GetEntriesFast(), fix print outs
 *
 * Revision 1.6  2004/10/19 01:39:35  mvl
 * Changed for splitting on file. Added support for hits
 *
 * Revision 1.5  2004/08/17 17:42:30  mvl
 * Removed warning message for empty Pmd collection + corrected some range checking
 *
 * Revision 1.4  2004/05/02 04:10:14  perev
 * private => protected
 *
 * Revision 1.3  2004/04/09 23:03:16  jeromel
 * Ignoring indent, commented out redundant messages (lots of them in the log file)
 *
 * Revision 1.2  2004/04/08 23:55:41  jeromel
 * Extraneous message commented out
 *
 * Revision 1.1  2004/04/02 03:36:21  jeromel
 * New files for PMD
 *
 * ****************************************************************/

#include <assert.h>
#include <string.h>

#include "StMuPmdCollection.h"
#include "Stiostream.h"
#include "StMuPmdUtil.h"
#include "StMuPmdCluster.h"
#include "StMuPmdHit.h"
static StMuPmdUtil util; // to ease decoding of EEMC hits
ClassImp(StMuPmdCollection)

StMuPmdCollection::StMuPmdCollection()
{    
  for(int i=0;i<2;i++) {
    mPmdHits[i]       = 0;
    mPmdClusters[i]   = 0;
  }
} 

StMuPmdCollection::StMuPmdCollection(StMuPmdCollection& o) {
  for ( int i=0; i<2; i++) {
    mPmdClusters[i] = (TClonesArray*)o.mPmdClusters[i]->Clone();
    mPmdHits[i] = (TClonesArray*)o.mPmdHits[i]->Clone();
  }
}

void StMuPmdCollection::init(int detector)
{
   mPmdClusters[detector - pmd]   = new TClonesArray("StMuPmdCluster",0);
   mPmdHits[detector - pmd]       = new TClonesArray("StMuPmdHit",0);
}

StMuPmdCollection::~StMuPmdCollection()
{
  delete mPmdClusters[0];mPmdClusters[0]=0;
  delete mPmdClusters[1];mPmdClusters[1]=0;


  delete mPmdHits[0];mPmdHits[0]=0;
  delete mPmdHits[1];mPmdHits[1]=0;
}

void StMuPmdCollection::DeleteThis()
{
    for ( int i=0; i<2; i++) mPmdClusters[i]->Delete();    
    for ( int i=0; i<2; i++) mPmdHits[i]->Delete();    
}

void StMuPmdCollection::clear(Option_t *option)
{
  mPmdClusters[0]->Delete();
  mPmdClusters[1]->Delete();

  mPmdHits[0]->Delete();
  mPmdHits[1]->Delete();
  return;
}

int StMuPmdCollection::getNClusters(int detector)
{
  if(detector<pmd || detector>cpv) return 0;
  TClonesArray *tca =NULL;
  if(detector>=pmd && detector <= cpv) tca = mPmdClusters[detector-pmd];

  if (tca) {
    //cout << "Got tca"<<endl;
    return tca->GetEntriesFast();
  } else  {
    //cout << "StMuPmdCollection::getNClusters No tca" << endl;
    return 0;
  }
}

int StMuPmdCollection::getNHits(int detector)
{
  if(detector<pmd || detector>cpv) return 0;
  TClonesArray *tca =NULL;
  if(detector>=pmd && detector <= cpv) tca = mPmdHits[detector-pmd];

  if (tca) {
    //cout << "Got tca"<<endl;
    return tca->GetEntriesFast();
  } else  {
    //cout << "StMuPmdCollection::getNClusters No tca" << endl;
    return 0;
  }
}

StMuPmdCluster* StMuPmdCollection::getCluster(int clusterId,int detector)
{
  if(detector<pmd || detector>cpv) return NULL;
  TClonesArray *tca = NULL;
  if(detector>=pmd && detector <= cpv) tca = mPmdClusters[detector-pmd];
  int counter = tca->GetEntriesFast();
  if(clusterId<0 || clusterId>counter) return NULL;
  return (StMuPmdCluster*)tca->At(clusterId);
}

StMuPmdHit* StMuPmdCollection::getHit(int hitId,int detector)
{
  if(detector<pmd || detector>cpv) return NULL;
  TClonesArray *tca = NULL;
  if(detector>=pmd && detector <= cpv) tca = mPmdHits[detector-pmd];
  int counter = tca->GetEntriesFast();
  if(hitId<0 || hitId>counter) return NULL;
  return (StMuPmdHit*)tca->At(hitId);
}
void StMuPmdCollection::addCluster(int detector)
{
  if(detector<pmd || detector>cpv) return;
  TClonesArray *tca =NULL;
  if(detector>=pmd && detector <= cpv) tca = mPmdClusters[detector-pmd];
  
  int counter =0;
  if(mPmdClusters[detector -pmd] == NULL){
    init(detector);
    tca = mPmdClusters[detector-pmd];
  }
  if(tca) counter=tca->GetEntriesFast();
  //cout << "addcluster: entry "<<counter<<" "<<detector-pmd<<endl;
  new ((*tca)[counter]) StMuPmdCluster();
  // cout<<"counter after2  "<<tca->GetEntriesFast()<<endl;
  return;
}

void StMuPmdCollection::addHit(int detector)
{
  if(detector<pmd || detector>cpv) return;
  TClonesArray *tca =NULL;
  if(detector>=pmd && detector <= cpv) tca = mPmdHits[detector-pmd];
  
  int counter =0;
  if(mPmdHits[detector -pmd] == NULL){
    init(detector);
    tca = mPmdHits[detector-pmd];
  }
  if(tca) counter=tca->GetEntriesFast();
  //cout << "addcluster: entry "<<counter<<" "<<detector-pmd<<endl;
  new ((*tca)[counter]) StMuPmdHit();
  // cout<<"counter after2  "<<tca->GetEntriesFast()<<endl;
  return;
}
