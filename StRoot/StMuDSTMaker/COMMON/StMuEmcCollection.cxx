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
  mEmcPoints=new TObjArray();
  for(int i=0;i<4;i++) 
  { 
    if(i<2) mSmdHits[i]=new TObjArray();
    mEmcClusters[i]=new TObjArray();
  }
}

StMuEmcCollection::StMuEmcCollection(StMuEmcCollection& o) {
  memcpy( mTowerADC,  o.mTowerADC,  sizeof(mTowerADC)  );
  memcpy( mNSmdHits,  o.mNSmdHits,  sizeof(mNSmdHits)  );
  memcpy( mNClusters, o.mNClusters, sizeof(mNClusters) );
  mNPoints = o.mNPoints;

  for ( int i=0; i<2; i++) {
    mSmdHits[i] = (TObjArray*)o.mSmdHits[i]->Clone();
  } 
  for ( int i=0; i<4; i++) {
    mEmcClusters[i] = (TObjArray*)o.mEmcClusters[i]->Clone();
  }
  mEmcPoints = (TObjArray*)o.mEmcPoints->Clone(); 
}

StMuEmcCollection::~StMuEmcCollection()
{
  clear();
  for(int i=0;i<4;i++) 
  {
    if(i<2) {delete mSmdHits[i]; mSmdHits[i]=NULL;}
    delete mEmcClusters[i]; mEmcClusters[i]=NULL;
  } 
  delete mEmcPoints;
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
  mNPoints=0;
  // deleting clusters and hits
  for(int d=0;d<4;d++)
  {
    for(int i=0; i<getNClusters(d);i++)
    {
      StMuEmcCluster* cluster = getCluster(d,i);
      if(cluster) delete cluster;
    }
    mNClusters[d]=0;
  }
  for(int d=2;d<4;d++)
  {
    for(int i=0; i<getNSmdHits(d);i++)
    {
      StMuEmcHit* hit = getSmdHit(d,i);
      if(hit) delete hit;
    }
    mNSmdHits[d]=0;
  }
  
  for(int i=0;i<4;i++) mEmcClusters[i]->Clear();
  for(int i=0;i<2;i++) mSmdHits[i]->Clear();
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
  return mNSmdHits[detector-3];
}
StMuEmcHit* StMuEmcCollection::getSmdHit(int detector,int hitId)
{
  if(detector<3 && detector>4) return NULL;
  if(hitId<0 || hitId>mNSmdHits[detector-3]) return NULL;
  return (StMuEmcHit*)mSmdHits[detector-3]->At(hitId);
}
int StMuEmcCollection::getNClusters(int detector)
{
  if(detector <1 && detector>4) return 0;
  return mNClusters[detector-1];
}
StMuEmcCluster* StMuEmcCollection::getCluster(int detector,int clusterId)
{
  if(detector <1 && detector>4) return NULL;
  if(clusterId<0 || clusterId>mNClusters[detector-1]) return NULL;
  return (StMuEmcCluster*)mEmcClusters[detector-1]->At(clusterId);
}
int StMuEmcCollection::getNPoints()
{
  return mNPoints;
}
StMuEmcPoint* StMuEmcCollection::getPoint(int pointId)
{
  if(pointId<0 || pointId>mNPoints) return NULL;
  return (StMuEmcPoint*)mEmcPoints->At(pointId);
}
void StMuEmcCollection::setTowerADC(int id,int adc)
{
  if(id<1 || id>4800) return;
  packbits(mTowerADC,adc,12,(unsigned int)(id-1));
  return;
}
void StMuEmcCollection::addSmdHit(int detector,StMuEmcHit* hit)
{
  if(detector<3 && detector>4) return;
  mSmdHits[detector-3]->AddLast(hit);
  mNSmdHits[detector-3]++;
  return;
}
void StMuEmcCollection::addCluster(int detector,StMuEmcCluster* cluster)
{
  if(detector<1 && detector>4) return;
  mEmcClusters[detector-1]->AddLast(cluster);
  mNClusters[detector-1]++;
  return;
}
void StMuEmcCollection::addPoint(StMuEmcPoint* point)
{
  mEmcPoints->AddLast(point);
  mNPoints++;
  return;
}




