//###########################################################
// EMC Micro Event
// Author: Alexandre A. P. Suaide
// initial version 08/2001
//
// See README for details
//########################################################### 
#include "StMuEmcCollection.h"
#include <iostream>
ClassImp(StMuEmcCollection)

StMuEmcCollection::StMuEmcCollection()
{    
  for(int i=0;i<7200;i++) mTowerADC[i]=0;
  mEmcPoints=new TClonesArray("StMuEmcPoint",0);
  mPrsHits = new TClonesArray("StMuEmcHit",0);
  for(int i=0;i<4;i++) 
  { 
    if(i<2) mSmdHits[i]=new TClonesArray("StMuEmcHit",0);
    mEmcClusters[i]=new TClonesArray("StMuEmcCluster",0);
  }
}

StMuEmcCollection::StMuEmcCollection(StMuEmcCollection& o) {
  memcpy( mTowerADC,  o.mTowerADC,  sizeof(mTowerADC)  );

  for ( int i=0; i<2; i++) {
    mSmdHits[i] = (TClonesArray*)o.mSmdHits[i]->Clone();
  } 
  for ( int i=0; i<4; i++) {
    mEmcClusters[i] = (TClonesArray*)o.mEmcClusters[i]->Clone();
  }
  mPrsHits = (TClonesArray*)o.mPrsHits->Clone();
  mEmcPoints = (TClonesArray*)o.mEmcPoints->Clone(); 
}

StMuEmcCollection::~StMuEmcCollection()
{
  //clear();
  for(int i=0;i<4;i++) 
  {
    if(i<2) {if(mSmdHits[i]) {mSmdHits[i]->Clear(); delete mSmdHits[i]; mSmdHits[i]=NULL;}}
    if(mEmcClusters[i]) {mEmcClusters[i]->Clear(); delete mEmcClusters[i]; mEmcClusters[i]=NULL;}
  } 
  if(mEmcPoints) {mEmcPoints->Clear(); delete mEmcPoints;}
  if(mPrsHits) {mPrsHits->Clear(); delete mPrsHits;}
}
void StMuEmcCollection::clear(Option_t *option)
{
  for(int i=0;i<7200;i++) mTowerADC[i]=0;
  // deleting points ...
  for(int i=0;i<getNPoints();i++)
  {
    StMuEmcPoint *point = getPoint(i);
    if(point) delete point;
  }
  // deleting clusters and hits
  for(int d=0;d<4;d++)
  {
    for(int i=0; i<getNClusters(d);i++)
    {
      StMuEmcCluster* cluster = getCluster(d,i);
      if(cluster) delete cluster;
    }
  }
  for(int d=2;d<4;d++)
  {
    for(int i=0; i<getNSmdHits(d);i++)
    {
      StMuEmcHit* hit = getSmdHit(d,i);
      if(hit) delete hit;
    }
  }
  for(int i=0; i<getNPrsHits();i++)
  {
    StMuEmcHit* hit = getPrsHit(i);
    if(hit) delete hit;
  }
  
  for(int i=0;i<4;i++) mEmcClusters[i]->Clear();
  for(int i=0;i<2;i++) mSmdHits[i]->Clear();
  mPrsHits->Clear();
  mEmcPoints->Clear();
  return;
}
void StMuEmcCollection::packbits(unsigned char *data, unsigned int value, unsigned int nbits, unsigned int index)
{
  unsigned int start     = index*nbits;
  unsigned int startByte = start/8;
  unsigned int startBit  = start%8;  
  unsigned int a         = 0;
  unsigned int s         = 1;
  for(int i=0;i<4;i++)  { a+=data[startByte+i]*s; s*=256; }
  unsigned int mask = ((unsigned int)pow(2,nbits)-1);
  unsigned int b = ((value&mask)<<startBit) | (a&(~(mask<<startBit)));  
  data[startByte+0] = (unsigned char)((b & 0x000000FF));
  data[startByte+1] = (unsigned char)((b & 0x0000FF00)>>8);
  data[startByte+2] = (unsigned char)((b & 0x00FF0000)>>16);
  data[startByte+3] = (unsigned char)((b & 0xFF000000)>>24);
  return;
}
unsigned int StMuEmcCollection::unpackbits(unsigned char *data, unsigned int nbits, unsigned int index)
{
  unsigned int start     = index*nbits;
  unsigned int startByte = start/8;
  unsigned int startBit  = start%8;  
  unsigned int a         = 0;
  unsigned int s         = 1;
  for(int i=0;i<4;i++)  { a+=data[startByte+i]*s; s*=256; }  
  unsigned int mask = ((unsigned int)pow(2,nbits)-1);
  unsigned int b = (unsigned int)(a&(mask<<startBit))>>startBit;
  return b;
}
int StMuEmcCollection::getTowerADC(int id)
{
  if(id<1 || id>4800) return 0;
  return (int)unpackbits(mTowerADC,12,(unsigned int)(id-1));
}
int StMuEmcCollection::getNSmdHits(int detector)
{
  if(detector<3 && detector>4) return 0;
  TClonesArray *tca = mSmdHits[detector-3];
  return tca->GetEntries();
}
StMuEmcHit* StMuEmcCollection::getSmdHit(int detector,int hitId)
{
  if(detector<3 && detector>4) return NULL;
  TClonesArray *tca = mSmdHits[detector-3];
  int counter = tca->GetEntries();
  if(hitId<0 || hitId>counter) return NULL;
  return (StMuEmcHit*)tca->At(hitId);
}
int StMuEmcCollection::getNPrsHits()
{
  TClonesArray *tca = mPrsHits;
  return tca->GetEntries();
}
StMuEmcHit* StMuEmcCollection::getPrsHit(int hitId)
{
  TClonesArray *tca = mPrsHits;
  int counter = tca->GetEntries();
  if(hitId<0 || hitId>counter) return NULL;
  return (StMuEmcHit*)tca->At(hitId);
}
int StMuEmcCollection::getNClusters(int detector)
{
  if(detector <1 && detector>4) return 0;
  TClonesArray *tca =mEmcClusters[detector-1];
  return tca->GetEntries();
}
StMuEmcCluster* StMuEmcCollection::getCluster(int detector,int clusterId)
{
  if(detector <1 && detector>4) return NULL;
  TClonesArray *tca =mEmcClusters[detector-1];
  int counter = tca->GetEntries();
  if(clusterId<0 || clusterId>counter) return NULL;
  return (StMuEmcCluster*)tca->At(clusterId);
}
int StMuEmcCollection::getNPoints()
{
  TClonesArray *tca =mEmcPoints;
  return tca->GetEntries();
}
StMuEmcPoint* StMuEmcCollection::getPoint(int pointId)
{
  TClonesArray *tca =mEmcPoints;
  int counter = tca->GetEntries();
  if(pointId<0 || pointId>counter) return NULL;
  return (StMuEmcPoint*)tca->At(pointId);
}
void StMuEmcCollection::setTowerADC(int id,int adc)
{
  if(id<1 || id>4800) return;
  packbits(mTowerADC,adc,12,(unsigned int)(id-1));
  return;
}
void StMuEmcCollection::addSmdHit(int detector)
{
  if(detector<3 && detector>4) return;
  TClonesArray *tca = mSmdHits[detector-3];
  int counter = tca->GetEntries();
  new ((*tca)[counter]) StMuEmcHit();
  return;
}
void StMuEmcCollection::addPrsHit()
{
  TClonesArray *tca = mPrsHits;
  int counter = tca->GetEntries();
  new ((*tca)[counter]) StMuEmcHit();
  return;
}
void StMuEmcCollection::addCluster(int detector)
{
  if(detector<1 && detector>4) return;
  TClonesArray *tca =mEmcClusters[detector-1];
  int counter = tca->GetEntries();
  new ((*tca)[counter]) StMuEmcCluster();
  return;
}
void StMuEmcCollection::addPoint()
{
  TClonesArray *tca =mEmcPoints;
  int counter = tca->GetEntries();
  new ((*tca)[counter]) StMuEmcPoint();
  return;
}




