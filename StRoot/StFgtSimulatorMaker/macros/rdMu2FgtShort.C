#include "StRoot/StMuDSTMaker/COMMON/StMuArrays.h"
class StFgtEvent;

rdMu2FgtShort() {
  int nEve=1;
  Int_t nFiles  = 1;
  //char *inDir="/star/u/sgliske/FGT/2012-Jan/leak-test/test4/";
  char *inDir="./";
  char *file="QCDprodMBa.MuDst.root";
 

  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();

  cout << " loading done " << endl;
  chain = new StChain("StChain");   // create chain    
  
  // Now we add Makers to the chain...   
  muMk = new StMuDstMaker(0,0,inDir,file,"MuDst.root",nFiles);
  TChain* tree=muMk->chain(); assert(tree);
  int nEntries=(int) tree->GetEntries();
  printf("total eve in muDst chain =%d\n",nEntries);  // return ;
  if(nEntries<0) return;

  chain->Init();
  // chain->ls(3);
  
  chain->Clear();
  int stat = chain->Make();
  printf("EEEE stat=%d\n",stat); 
  
  // Access to muDst .......................
  StMuEvent* muEve = muMk->muDst()->event();
  assert(muEve);

  printf("JJJJJ check 1 for FGT\n");
  TClonesArray *fgtStrips    = muMk->muDst()->fgtArray( muFgtStrips );
  assert( fgtStrips );
 
  Int_t nStrips = fgtStrips->GetEntriesFast();
  printf("JJJJJ check 2 for FGT  nStrips=%d\n",nStrips);
  for( Int_t i = 0; i < nStrips; ++i ){
    StMuFgtStrip* strip = static_cast< StMuFgtStrip* >( (*fgtStrips)[i] );
    if(strip==0) continue;
    Int_t geoId = strip->getGeoId();
    Short_t seedType=strip->getClusterSeedType();
    printf("i=%d geoId=%d charge=%.1f +/-%.1f  seedType=%d\n",i,geoId, strip->getCharge(), strip->getChargeUncert(),seedType);
  } 

  
  TClonesArray  * fgtClusters = muMk->muDst()->fgtArray( muFgtClusters );
  assert( fgtClusters );
  Int_t nClusters = fgtClusters->GetEntriesFast();
  printf("JJJJJ check 3 for FGT  nClusters=%d\n",nClusters);
  for( Int_t i = 0; i < nClusters; ++i ){
    StMuFgtCluster* clus = static_cast< StMuFgtCluster* >( (*fgtClusters)[i] );
    if(clus==0) continue;
    Int_t geoId = clus->getCentralStripGeoId();
    printf("i=%d cntrGeoId=%d charge=%.1f R=%.1f+/-%.1f  phi=%.3f+/-%.3f  nStrip=%d\n",i,geoId , clus->getCharge(), clus->getR(),clus->getErrR(), clus->getPhi(), clus->getErrPhi(), clus->getNumStrips());
  }

  TClonesArray * fgtInfo     = muMk->muDst()->fgtArray( muFgtInfo );
  assert( fgtInfo );
  Int_t nInfo = fgtInfo->GetEntriesFast();
  printf("JJJJJ check 4 for FGT  nInfo=%d\n",nInfo);
  for( Int_t i = 0; i < nInfo; ++i ){
    StMuFgtInfo* info = static_cast< StMuFgtInfo* >( (*fgtInfo)[i] );
    if(info==0) continue;
    //printf("i=%d info=%s=\n",i,info->mMsg);
  } 

  printf("JJJJJ check 9 for FGT done\n\n");
  return ;
#if 0
#include "StRoot/StMuDSTMaker/COMMON/StMuFgtStrip.h"
#include "StRoot/StMuDSTMaker/COMMON/StMuFgtCluster.h"
 StRoot/StMuDSTMaker/COMMON/StMuFgtInfo.h 
#endif  

}
