/*****************************************************************
 * $Id: StMuPmdCollection.cxx,v 1.5 2004/08/17 17:42:30 mvl Exp $
 *
 * Class : StMuPmdCollection
 * Author: Supriya Das & Subhasis Chattopadhyay
 * ****************************************************************
 *
 * Description: This class holds the PMD clusters for MuDst
 * ****************************************************************
 * $Log: StMuPmdCollection.cxx,v $
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
static StMuPmdUtil util; // to ease decoding of EEMC hits
ClassImp(StMuPmdCollection)

StMuPmdCollection::StMuPmdCollection()
{    
  int n = sizeof(mPmdClusters[0])+sizeof(mPmdClusters[1]);
  memset(&mPmdClusters[0],0,n);
  //cout<<"memset "<<n<<endl;
  for(int i=0;i<2;i++)
	  {
        mPmdClusters[i]       =NULL; 
	  }

} 

StMuPmdCollection::StMuPmdCollection(StMuPmdCollection& o) {
//  memcpy( mTowerADC,  o.mTowerADC,  sizeof(mTowerADC)  );
//  memcpy( mEndcapTowerADC,  o.mEndcapTowerADC,  sizeof(mEndcapTowerADC)  );
  
  for ( int i=0; i<2; i++) {
    mPmdClusters[i] = (TClonesArray*)o.mPmdClusters[i]->Clone();
  }
}
/*
void StMuPmdCollection::init() 
{
  for(int i=0;i<2;i++) 
  { 
    mPmdClusters[i]       = new TClonesArray("StMuPmdCluster",0);
  }
}

*/

void StMuPmdCollection::init(int detector)
{
	mPmdClusters[detector - pmd]       = new TClonesArray("StMuPmdCluster",0);
}

StMuPmdCollection::~StMuPmdCollection()
{
  delete mPmdClusters[0];mPmdClusters[0]=0;
  delete mPmdClusters[1];mPmdClusters[1]=0;
}

void StMuPmdCollection::DeleteThis()
{
    for ( int i=0; i<2; i++) mPmdClusters[i]->Delete();    
}

void StMuPmdCollection::clear(Option_t *option)
{
  mPmdClusters[0]->Delete();
  mPmdClusters[1]->Delete();
  return;
}
void StMuPmdCollection::packbits(unsigned char *data, unsigned int value, unsigned int nbits, unsigned int index)
{
  unsigned int start     = index*nbits;
  unsigned int startByte = start/8;
  unsigned int startBit  = start%8;  
  unsigned int a         = 0;
  unsigned int s         = 1;
  for(int i=0;i<4;i++)  { a+=data[startByte+i]*s; s*=256; }
  unsigned int mask = ((unsigned int)(1<<nbits)-1);
  unsigned int b = ((value&mask)<<startBit) | (a&(~(mask<<startBit)));  
  data[startByte+0] = (unsigned char)((b & 0x000000FF));
  data[startByte+1] = (unsigned char)((b & 0x0000FF00)>>8);
  data[startByte+2] = (unsigned char)((b & 0x00FF0000)>>16);
  data[startByte+3] = (unsigned char)((b & 0xFF000000)>>24);
  return;
}
unsigned int StMuPmdCollection::unpackbits(unsigned char *data, unsigned int nbits, unsigned int index)
{
  unsigned int start     = index*nbits;
  unsigned int startByte = start/8;
  unsigned int startBit  = start%8;  
  unsigned int a         = 0;
  unsigned int s         = 1;
  for(int i=0;i<4;i++)  { a+=data[startByte+i]*s; s*=256; }  
  unsigned int mask = ((unsigned int)(1<<nbits)-1);
  unsigned int b = (unsigned int)(a&(mask<<startBit))>>startBit;
  return b;
}

int StMuPmdCollection::getNClusters(int detector)
{
  if(detector<pmd || detector>cpv) return 0;
  TClonesArray *tca =NULL;
  if(detector>=pmd && detector <= cpv) tca = mPmdClusters[detector-pmd];

  //cout << "DEBUG :: detector bemc bsmdp " 
  //     << detector << " " << bemc << " " << bsmdp << endl;

  if (tca) {
    //cout << "Got tca"<<endl;
    return tca->GetEntries();
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
  int counter = tca->GetEntries();
  if(clusterId<0 || clusterId>counter) return NULL;
  return (StMuPmdCluster*)tca->At(clusterId);
}

void StMuPmdCollection::addCluster(int detector)
{
  if(detector<pmd || detector>cpv) return;
  TClonesArray *tca =NULL;
  if(detector>=pmd && detector <= cpv) tca = mPmdClusters[detector-pmd];
  /*
  int counter =0;
  if(tca)counter=tca->GetEntries();
  if(counter==0){
	  init();
	  tca = mPmdClusters[detector-pmd];
  }
  */
  
  int counter =0;
  if(mPmdClusters[detector -pmd] == NULL){
    init(detector);
    tca = mPmdClusters[detector-pmd];
  }
  if(tca) counter=tca->GetEntries();
  //cout << "addcluster: entry "<<counter<<" "<<detector-pmd<<endl;
  new ((*tca)[counter]) StMuPmdCluster();
  // cout<<"counter after2  "<<tca->GetEntries()<<endl;
  return;
}

