//
// $id$
//
// $Log: StEmcPreClusterCollection.cxx,v $
// Revision 1.1  2000/05/15 21:24:00  subhasis
// initial version
//
//
// PreClusters Finder Maker for EMC
//
//
// Author: Subhasis Chattopadhyay,
//         Aleksei Pavlinov , July 1999
//          initial version from Akio Ogawa    
//

//////////////////////////////////////////////////////////////////////////
//                                   
// StEmcPreClusterCollection
//
// StEmcPreClusterCollectio is base class for electromagnetic cluster 
// collection. 
//
//////////////////////////////////////////////////////////////////////////
#include "StEmcPreClusterCollection.h"
#include "emc_def.h"
ClassImp(StEmcPreClusterCollection)

TArrayF energyw;
TArrayI ew, sw, used, hitsid;
Int_t nh, first, last, keyEta, keyPhi,nhits;
Int_t nhit;
//_____________________________________________________________________________
StEmcPreClusterCollection::StEmcPreClusterCollection():St_DataSet("Default")
{
  SetTitle("ecl");
}
//_____________________________________________________________________________
StEmcPreClusterCollection::StEmcPreClusterCollection(const Char_t *Name):St_DataSet(Name)
{
  SetTitle("ecl");
  mNclusters=0;
  if(!strcmp(Name,"bemc")){
   mDetector  = 1;     mEnergySeed         = 0.1;  
    mEnergyAdd = 0.001; mEnergyThresholdAll = 0.2;
    mSizeMax = 4;
//   mDetector  = 1;     mEnergySeed         = 0.;  
//    mEnergyAdd = 0.0; mEnergyThresholdAll = 0.0;
//    mSizeMax = 4;
  }
  else if(!strcmp(Name,"bsmde")){
    mDetector  = 3;     mEnergySeed         = 0.05; 
    mEnergyAdd = 0.001; mEnergyThresholdAll = 0.1;  
    mSizeMax = 5;
  }
  else if(!strcmp(Name,"bsmdp")){
    mDetector  = 4;     mEnergySeed         = 0.05; 
    mEnergyAdd = 0.001; mEnergyThresholdAll = 0.1;  
    mSizeMax = 4;
  }
  else{
    mEnergySeed = -1.0; mEnergyAdd = 0.0; mEnergyThresholdAll = 0.0;  
    cout<<"For detector "<<Name<<" PreCluster Finder has not ipmlimented yet:"<<endl;  
  }
}
//_____________________________________________________________________________
Int_t StEmcPreClusterCollection::findClusters(StEmcHitCollection* hits)
{
  nhits=hits->NHit();

  TArrayS firstLast=(*hits->IndexFirstLast());
  TArrayS numsModule=(*hits->NumsModule());
  for(Int_t mod=0; mod < hits->Module(); mod++) {
    first = (Int_t)firstLast[mod];
    last  = (Int_t)firstLast[mod+1];
    nh    = last - first;           // nh must be positive
    if(nh<=0) {cout<<" nh "<<nh<<" mod "<<mod<<" first,last "<<first<<last<<endl; return 1;}

// Arrays for #module=numsModule[mod]
    energyw.Set(nh); ew.Set(nh); sw.Set(nh);
    energyw.Reset(); ew.Reset(); sw.Reset();
//
//  for(Int_t jh=nh-1; jh>=0; jh--){ 
//if(mDetector==1)cout<<" nh, jh,ew,sw,energyw "<<nh<<" "<<jh<<" "<<ew[jh]<<" " <<sw[jh]<<" "<<energyw[jh]<<endl;
//  }
//
    Int_t idw, mw, ih, jh;
    for(ih=first; ih<last; ih++){
      jh=ih-first;
      energyw[jh] = hits->HitEnergy(ih);
      idw=hits->HitId(ih);
      hits->getBin(idw, mw, ew[jh], sw[jh]);
//if(mDetector==1)cout<<" hits for Cluster**nh,mdet,m,e "<<nh<<" "<<jh<<" "<<mDetector<<" "<<mw<<" "<<ew[jh]<<" " <<sw[jh]<<" "<<energyw[jh]<<endl;
      if(mw != numsModule[mod]) {  // Only for checking 
        printf("<E> Something wrong mw=%i numsModule=%i \n",mw,numsModule[mod]);
      }
    }
    findClustersInModule(hits);
  }
  printf(" Det %s #Clusters %i \n",GetName(), mNclusters);
  return kStOK;
}
//_____________________________________________________________________________
void StEmcPreClusterCollection::findClustersInModule(StEmcHitCollection* hits)
{
  used.Set(nh);
  used.Reset();
//
  if(nh == 1){ 
    if(energyw[0]>mEnergySeed){
      hitsid.Set(1); hitsid[0]=first;
      addPreCluster(hits, &hitsid);
      nhit = 0;
    }
    return;
  }

//  used.Set(nh);
  St_TableSorter index(energyw.GetArray(), nh); // The last element is biggest
  hitsid.Set(10);
  hitsid.Reset();

  Int_t i, ii;
  for(i=nh-1; i>=0; i--){ 
    Int_t j = index.GetIndex(i);
//    if(mDetector==1)cout<<"USED FOR HITS**"<<j<<" "<<used[j]<<endl;
    if(energyw[j] < mEnergySeed){break;}
    if(used[j] == 0){
      hitsid[0]=j; // First hit in cluster
      nhit=1;
      used[j]=1;
      keyEta=0; keyPhi=0;
//if(mDetector==1)cout<<"FINCLINseed**nh,nhit,i,j,en**"<<nh<<" "<<nhit<<" "<<i<<" "<<j<<" "<<energyw[j]<<endl;

      for(ii=i-1; ii>=0; ii--){
        int jj = index.GetIndex(ii);
        if(energyw[jj] < mEnergyAdd) break;
//if(mDetector==1)cout<<"FINCLINnei**nh,nhit,ii,jj,en**"<<nh<<" "<<nhit<<" "<<ii<<" "<<jj<<" "<<energyw[jj]<<" "<<used[jj]<<endl;
        if(used[jj] == 0){
//if(mDetector==1)cout<<"nei USEDOK**"<<nhit<<" "<<jj<<" "<<energyw[jj]<<"keyPhi "<<keyPhi<<endl;
          if(testOnNeighbor(jj)==0){
//if(mDetector==1)cout<<"nei TESTOK**"<<nhit<<" "<<jj<<" "<<energyw[jj]<<endl;
            used[jj]=1;
            hitsid[nhit]=jj; nhit++; 
            if(nhit == mSizeMax) break;
          }
        }
      }
      if(nhit>0){
        TArrayI *hidw = new TArrayI(nhit);
        for(Int_t i1=0; i1<nhit; i1++){
          (*hidw)[i1] = hitsid[i1] + first; // Trans. from module to all
        }
        addPreCluster(hits, hidw);
        nhit = 0; delete hidw;
      }
    }
  }
}
//_____________________________________________________________________________
Int_t  StEmcPreClusterCollection::testOnNeighbor(Int_t jn)
{
  Int_t js,imax;
  static Int_t keyDir;
  imax = (nhit<2) ? 1 : 2; 
  for(Int_t i=0; i<imax; i++){
    js=hitsid[i];
    if(ew[js] == ew[jn] && keyEta==0) {// Neighbors in eta row
      if(abs(sw[js]-sw[jn])!=1) {      // Only for testing
        printf(" testOnNeighbor => sw(seed) %i sw(neigbour) %i \n",sw[js],sw[jn]);
      }
      if(nhit == 1) keyEta = 1; 
      return 0;
    }
    else if(sw[js] == sw[jn] && abs(ew[js]-ew[jn]) == 1 && keyPhi==0){// Neighbors in phi row 
      if     (nhit == 1) {keyPhi=1; return 0;}
      else if(nhit == 2) {keyDir=ew[js]-ew[jn]; return 0;}
      else if(nhit == 3) {
        if(keyDir == ew[js]-ew[jn]) return 0; // 4th hit must be at the same side as 3th !!! 
        else return 1;
      }
    }
  }
  return 1;
}
//_____________________________________________________________________________
void StEmcPreClusterCollection::addPreCluster(StEmcHitCollection* hits, TArrayI *hid)
{
  StEmcPreCluster *cluster = new StEmcPreCluster(hid);
  cluster->calcMeanAndRms(hits);
  mClusters.Add(cluster);
  mNclusters  += 1;
}
//_____________________________________________________________________________
void StEmcPreClusterCollection::printCluster(Int_t ic, StEmcPreCluster *cl)
{
  cout <<" =========  PreCluster  "<<ic<<" =========== "<<endl;
  cout << (*cl) << endl;
}
//_____________________________________________________________________________
void StEmcPreClusterCollection::printClusters(Int_t n, Int_t start)
{
  cout << endl << mDetector << "  " << GetName() << " : ";
  if(mNclusters<=0){cout << "No Clusters" << endl; return;}
  else{cout << mNclusters << " clusters" <<endl;} 
  cout<<" mEnergySeed " << mEnergySeed;
  cout<<" mEnergyAdd "  << mEnergyAdd; 
  cout<<" mEnergyThresholdAll " << mEnergyThresholdAll << endl;
  cout<<" mSizeMax " << mSizeMax;
  cout<<" Capacity "<<mClusters.Capacity()<<endl<<endl;

  TIter next(&mClusters);
  StEmcPreCluster *cl;
  if(start>0){
    for(Int_t i=0; i<start; i++) cl = (StEmcPreCluster*)next();
  }

  if(start<0) start=0;
  if(n<=0) n=1;
  if(start>=mNclusters) start=mNclusters-1;
  if(start+n>=mNclusters) n=mNclusters-start;
  for(Int_t i=start; i<start+n; i++){
    if(i<=mClusters.Capacity()){
      if( cl=(StEmcPreCluster*)next() ) printCluster(i,cl);
    }
    else cout<<" PreCluster "<<i<<" out of capacity "<<endl;
  }
}
//_____________________________________________________________________________
void StEmcPreClusterCollection::printClustersAll()
{
  printClusters(mNclusters,0);
}
//_____________________________________________________________________________
void StEmcPreClusterCollection::Browse(TBrowser *b)
{
  printClusters(mNclusters,0);
}
