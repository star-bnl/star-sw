#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <fakeRtsLog.h>

/*********************************************************************
 * $Id: L2eemcGamma2012.cxx,v 1.4 2012/03/21 18:18:03 jml Exp $
 * \author Jan Balewski,MIT , 2008 
 *********************************************************************
 * Descripion: see .h
  *********************************************************************
 */


#ifdef  IS_REAL_L2  //in l2-ana  environment
  #include "../L2algoUtil/L2EmcDb2012.h"
  #include "../L2algoUtil/L2Histo.h"
#else    //full path needed for cvs'd code
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2EmcDb2012.h"
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2Histo.h"
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2EmcGeom2012.h"
#endif

#include "L2eemcGamma2012.h"


//=================================================
//=================================================
void L2eemcGamma2012::quickSort(int array[], int start, int end)
{
        int i = start;                          // index of left-to-right scan
        int k = end;                            // index of right-to-left scan

        if (end - start >= 1)                   // check that there are at least two elements to sort
        {
                float pivot = wrkEtow_et[array[start]];       // set the pivot as the first element in the partition

                while (k > i)                   // while the scan indices from left and right have not met,
                {
                        while (wrkEtow_et[array[i]] <= pivot && i <= end && k > i)  // from the left, look for the first
                                i++;                                    // element greater than the pivot
                        while (wrkEtow_et[array[k]] > pivot && k >= start && k >= i) // from the right, look for the first
                            k--;                                        // element not greater than the pivot
                        if (k > i)                                       // if the left seekindex is still smaller than
                                swap(array, i, k);                      // the right index, swap the corresponding elements
                }
                swap(array, start, k);          // after the indices have crossed, swap the last element in
                                                // the left partition with the pivot 
                quickSort(array, start, k - 1); // quicksort the left partition
                quickSort(array, k + 1, end);   // quicksort the right partition
        }
        else    // if there is only one element in the partition, do not do any sorting
        {
                return;                     // the array is sorted, so exit
        }
}

void L2eemcGamma2012::swap(int array[], int index1, int index2) 
// pre: array is full and index1, index2 < array.length
// post: the values at indices 1 and 2 have been swapped
{
	int temp = array[index1];           // store the first value in a temp
	array[index1] = array[index2];      // copy the value of the second into the first
	array[index2] = temp;               // copy the value of the temp into the second
}



//=================================================
//=================================================
L2eemcGamma2012::L2eemcGamma2012(const char* name, const char *uid, L2EmcDb2012* db, L2EmcGeom2012 *geoX, char* outDir, int resOff)  :  L2VirtualAlgo2012( name, uid, db, outDir,  false,true, resOff) { 
  /* called one per days
     all memory allocation must be done here
  */
  mGeom=geoX; 
  if (!mGeom)
    criticalError("L2eemcGamma is broken -- can't find geom.");

  setMaxHist(16); // set upper range, I uses only 2^N -it is easier to remember
  createHisto();

  //------- self-consistency checks, should never fail
  if (sizeof(L2gammaResult2012)!= L2gammaResult2012::mySizeChar) 

    criticalError("L2eemcGamma has failed consistency check. sizeof(L2gammaResult2012)!= L2gammaResult2012::mySizeChar");



}

/* ========================================
  ======================================== */
int 
L2eemcGamma2012::initRunUser( int runNo, int *rc_ints, float *rc_floats) {

  // unpack params from run control GUI
  par_dbg           =  rc_ints[0];
  par_RndAcceptPrescale      =  rc_ints[1];
  par_seedEtThres   =  rc_floats[0];
  par_clusterEtThres=  rc_floats[1];
  par_eventEtThres  =  rc_floats[2];

  // verify consistency of input params
  int kBad=0;
  kBad+=0x00001 * (par_seedEtThres<1.0);
  kBad+=0x00002 * (par_clusterEtThres<par_seedEtThres);

  if (mLogFile) { 
    fprintf(mLogFile,"L2%s algorithm initRun(R=%d), compiled: %s , %s\n params:\n",getName(),mRunNumber,__DATE__,__TIME__);
    fprintf(mLogFile," - use  Thres/GeV seed=%.2f, clusterList=%.2f debug=%d, prescale=%d\n", par_seedEtThres,par_clusterEtThres, par_dbg,par_RndAcceptPrescale);

    fprintf(mLogFile," - accept event cluster Thres/GeV=%.2f\n",par_eventEtThres);
    fprintf(mLogFile,"initRun() params checked for consistency, Error flag=0x%04x\n",kBad);
  }


  // clear content of all histograms & token-dependent memory
  int i;
  for (i=0; i<mxHA;i++) if(hA[i])hA[i]->reset();

  if(kBad) return -1*kBad;    

  memset(mEtow,0,sizeof(mEtow));
 
  // update tiltles of histos
  char txt[1000];
  sprintf(txt,"ETOW-compute: #seed towers ET>%.2f GeV / event; x: # ETOW towers; y: counts",par_seedEtThres);
  hA[3]->setTitle(txt);
  
  sprintf(txt,"ETOW-decision: #cluster ET>%.2f GeV / event ; x: # ETOW towers; y: counts",par_clusterEtThres);
  hA[4]->setTitle(txt);

 sprintf(txt,"ETOW-decision: acc cluster ET>%.2f GeV; x:cluster ET(GeV) ; y: counts",par_eventEtThres);
  hA[4]->setTitle(txt);
  
  

  for ( int index=0; index<EmcDbIndexMax; index++ )
     {
       const L2EmcDb2012::EmcCDbItem *x = mDb->getByIndex(index);
       if ( x==0 ) continue;
       if ( !mDb->isETOW(x) ) continue; 
       int sec = x->sec - 1;
       int sub = 8192; 
       sub = x->sub - 'A';
       int eta = x->eta - 1;
       int phi = EtowGeom::mxSubs *sec + sub;
       int tow = EtowGeom::mxEtaBin *phi + eta; // phi- changes faster
       int rdo = x->rdo;
       if (tow<0 || tow>mxEtow || rdo<0 || rdo>mxEtow) return -101;
       
       mTower2rdo[ tow ] = rdo;    // returns rdo channel for given tower
       mRdo2tower[ rdo ] = tow;    // returns tower for given rdo channel
    }
  return 0; //OK

}               

/* ========================================
  ======================================== */
int
L2eemcGamma2012::countNonZeroTow(int phi, int eta) {
  int tow = EtowGeom::mxEtaBin *((phi+EtowGeom::mxPhiBin)%EtowGeom::mxPhiBin) + ((eta+EtowGeom::mxEtaBin)%EtowGeom::mxEtaBin); // phi- changes faster
  const int maxTowers = EtowGeom::mxEtaBin * EtowGeom::mxPhiBin;
  int towPlusOne;
  int count=0;

  if (etow_used[tow]==0) {
    if(wrkEtow_et[tow]!=0) count++;
  }

  towPlusOne = tow+1;
  towPlusOne%= maxTowers;
  if (etow_used[towPlusOne]==0) {
    if(wrkEtow_et[towPlusOne]!=0) count++;
  }

  tow+=EtowGeom::mxEtaBin;
  tow%=maxTowers;
  if (etow_used[tow]==0) {
    if(wrkEtow_et[tow]!=0) count++;
  }

  towPlusOne = tow+1;
  towPlusOne%= maxTowers;
  if (etow_used[towPlusOne]==0) {
    if(wrkEtow_et[towPlusOne]!=0) count++;
  }
  return count;
}
  

/* ========================================
  ======================================== */
void
L2eemcGamma2012::flagUsed(int phi, int eta) {
  int tow = EtowGeom::mxEtaBin *((phi+EtowGeom::mxPhiBin)%EtowGeom::mxPhiBin) + ((eta+EtowGeom::mxEtaBin)%EtowGeom::mxEtaBin); // phi- changes faster
  const int maxTowers = EtowGeom::mxEtaBin * EtowGeom::mxPhiBin;
  int towPlusOne;
  etow_used[tow] = 1;

  towPlusOne = tow+1;
  towPlusOne%= maxTowers;
  etow_used[towPlusOne] = 1;

  tow+=EtowGeom::mxEtaBin;
  tow%=maxTowers;
  etow_used[tow] = 1;

  towPlusOne = tow+1;
  towPlusOne%= maxTowers;
  etow_used[towPlusOne] = 1;
}
  
/* ========================================
  ======================================== */
void
L2eemcGamma2012::averageEtaPhi(int phi, int eta, float *avePhi, float *aveEta) {
  int tow = EtowGeom::mxEtaBin *((phi+EtowGeom::mxPhiBin)%EtowGeom::mxPhiBin) + ((eta+EtowGeom::mxEtaBin)%EtowGeom::mxEtaBin); // phi- changes faster
  const int maxTowers = EtowGeom::mxEtaBin * EtowGeom::mxPhiBin;
  int towPlusOne;
  float ETsum;
  float etaSum;
  float phiSum;

  ETsum=wrkEtow_et[tow];
  etaSum = wrkEtow_et[tow]*(float)eta;
  phiSum = wrkEtow_et[tow]*(float)phi;

  towPlusOne = tow+1;
  towPlusOne%= maxTowers;
  ETsum+=wrkEtow_et[towPlusOne];
  etaSum += wrkEtow_et[towPlusOne]*(float)(eta+1);
  phiSum += wrkEtow_et[towPlusOne]*(float)phi;

  tow+=EtowGeom::mxEtaBin;
  tow%=maxTowers;
  ETsum+=wrkEtow_et[tow];
  etaSum += wrkEtow_et[tow]*(float)eta;
  phiSum += wrkEtow_et[tow]*(float)(phi+1);

  towPlusOne = tow+1;
  towPlusOne%= maxTowers;
  ETsum+=wrkEtow_et[towPlusOne];
  etaSum += wrkEtow_et[towPlusOne]*(float)(eta+1);
  phiSum += wrkEtow_et[towPlusOne]*(float)(phi+1);


  *aveEta = etaSum/ETsum;
  *avePhi = phiSum/ETsum;
}
  

/* ========================================
  ======================================== */
float
L2eemcGamma2012::sumET(int phi, int eta) {
  int tow = EtowGeom::mxEtaBin *((phi+EtowGeom::mxPhiBin)%EtowGeom::mxPhiBin) + ((eta+EtowGeom::mxEtaBin)%EtowGeom::mxEtaBin); // phi- changes faster

  const int maxTowers = EtowGeom::mxEtaBin * EtowGeom::mxPhiBin;
  int towPlusOne;
  float sum;
  sum=0;
  if (etow_used[tow]==0) {
    sum=wrkEtow_et[tow];
  }
  towPlusOne = tow+1;
  towPlusOne%= maxTowers;
  if (etow_used[towPlusOne]==0) {
    sum+=wrkEtow_et[towPlusOne];
  }

  tow+=EtowGeom::mxEtaBin;
  tow%=maxTowers;
  if (etow_used[tow]==0) {
    sum+=wrkEtow_et[tow];
  }
  towPlusOne = tow+1;
  towPlusOne%= maxTowers;
  if (etow_used[towPlusOne]==0) {
    sum+=wrkEtow_et[towPlusOne];
  }
  return sum;
}
  

/* ========================================
  ======================================== */
void 
L2eemcGamma2012::computeUser(int token){
  // token range is guaranteed by virtual08-class

  /* 2x2 cluster finder:
     - can find clusters through the tower border in Phi
     - sorts hit towers by Et and selects Highest Seed Towers first
  */
  
  clearEvent(token);

  // ------ PROJECT INPUT LIST TO 2D ARRAY AND SCAN FOR SEED TOWERS ----
  int i;
 
  L2eemcGammaEvent2012 *etowEve=mEtow+token;
  const HitTower1 *hit=mEveStream_etow[token].get_hits();
  const int hitSize=mEveStream_etow[token].get_hitSize();
  for(i=0;i< hitSize;i++,hit++) {
    //sortIndex[i] = i;
    int tower=mRdo2tower[hit->rdo];
    wrkEtow_tower_index[i]=tower;
    wrkEtow_et[tower]=hit->et;
    //if(hit->et<par_seedEtThres)continue;
    //wrkEtow_tower_seed[wrkEtow_tower_seed_size++]=tower;
  }
  hA[2]->fill(hitSize);
  //  long int start, stop;
  
  quickSort(wrkEtow_tower_index,0,hitSize-1);
  //print4(token,hitSize);

  // ----------- FIND 2x2 CLUSTER AROUND EVERY SEED -----
  for(i=hitSize-1;i >= 0; i--) {
    if(wrkEtow_et[wrkEtow_tower_index[i]] < par_seedEtThres) {
      i = -1;  // Early loop termination condition.  Remaining elements are below threshold
      continue;
    }
    int seedTow=wrkEtow_tower_index[i];
    int seedEta=seedTow%EtowGeom::mxEtaBin;
    int seedPhi=seedTow/EtowGeom::mxEtaBin;
    
    // ........drop seeds close to boundaries, fix it 
    /// PMN - fixed
    //if(seedEta==0 || seedEta==EtowGeom::mxEtaBin-1) continue;
    //if(seedPhi==0 || seedPhi==EtowGeom::mxPhiBin-1) continue;
    // now every seed has 4 2x2 clusters

    //.... find max of 4 possible clusters
    float maxET;
    float sum;

    if (etow_used[seedTow]!=0) continue; // if seed tower has been used in another cluster
    wrkEtow_tower_seed_size++;

    int high_quadrant;
    high_quadrant = 0;
    maxET = 0;
    if(seedEta < EtowGeom::mxEtaBin) {
      maxET = sumET(seedPhi,seedEta);
      sum=sumET(seedPhi-1,seedEta);
      if(maxET<sum) {
        maxET=sum;
        high_quadrant = 1;
      }
    }
    if (seedEta > 0) {
      sum=sumET(seedPhi-1,seedEta-1);
      if(maxET<sum) {
        maxET=sum;
        high_quadrant = 2;
      }
      sum=sumET(seedPhi,seedEta-1);
      if(maxET<sum) {
        maxET=sum;
        high_quadrant = 3;
      }
    }

    if(maxET<par_clusterEtThres)continue; 
    //........record clusters....

    hA[6]->fill((int)(maxET));
    hA[7]->fill(seedTow);
    hA[8]->fill(hitSize-i);
    //hA[9]->fill(high_quadrant);
    //hA[10]->fill((int)maxET);
    hA[11]->fill((int)wrkEtow_et[seedTow]);
    hA[12]->fill((int)(10.*wrkEtow_et[seedTow]/maxET));

    if(etowEve->size>=L2eemcGammaEvent2012::mxClust) continue; // overflow protection
    etowEve->clusterET[etowEve->size]=maxET;
    etowEve->clusterQuad[etowEve->size]=high_quadrant;
    etowEve->clusterSeedTow[etowEve->size]=seedTow;



    // flag all towers used in this cluster so they will 
    // not be used in other clusters
    int numNonZeroTow=0;
    float clusterEta;
    float clusterPhi;
    if (high_quadrant==0) {
      numNonZeroTow = countNonZeroTow(seedPhi, seedEta);
      flagUsed(seedPhi, seedEta); 
      averageEtaPhi(seedPhi, seedEta,&clusterPhi, &clusterEta); 
    } else if (high_quadrant==1) {
      numNonZeroTow = countNonZeroTow(seedPhi-1, seedEta);
      flagUsed(seedPhi-1, seedEta);
      averageEtaPhi(seedPhi-1, seedEta,&clusterPhi, &clusterEta);
    } else if (high_quadrant==2) {
      numNonZeroTow = countNonZeroTow(seedPhi-1, seedEta-1); 
      flagUsed(seedPhi-1, seedEta-1); 
      averageEtaPhi(seedPhi-1, seedEta-1,&clusterPhi, &clusterEta); 
    } else if (high_quadrant==3) {
      numNonZeroTow = countNonZeroTow(seedPhi, seedEta-1);
      flagUsed(seedPhi, seedEta-1);
      averageEtaPhi(seedPhi, seedEta-1,&clusterPhi, &clusterEta);
    }
    hA[13]->fill(numNonZeroTow);
    etowEve->clusterEta[etowEve->size]=clusterEta;
    etowEve->clusterPhi[etowEve->size]=clusterPhi;

    etowEve->size++;

  }// end of cluster search

  hA[3]->fill(wrkEtow_tower_seed_size);
  hA[4]->fill(etowEve->size);

  // debugging should be off for any time critical computation
  if(par_dbg>0){
    LOG(DBG,"dbg=%s, etow-adcL-size=%d\n",getName(),hitSize);
    // if(par_dbg>0) print1();
    print2();
    print3();// tmp, must have token
  } 
} 


/* ========================================
  ======================================== */
bool 
L2eemcGamma2012::decisionUser(int token, int *myL2Result){
  // INPUT: token + comput() results stored internally
  // OUTPUT: yes/now + pointer to  L2Result

  //the below is now handled in virtual algo
  //if(par_prescale>0) {
  //  prescale++;
  //  prescale %= par_prescale;
  //};

  // get pointers to internal private event storage
  L2eemcGammaEvent2012 *etowEve=mEtow+token;

  int ic;  

  //...... some histos just for fun
  if(etowEve->size>= L2eemcGammaEvent2012::mxClust) mhN->fill(5);  // was overflow
  if(etowEve->isFresh>L2eemcGammaEvent2012::kDataFresh) mhN->fill(6); // stale data
  etowEve->isFresh++; // mark the data as  stale

  hA[4]->fill(etowEve->size);

  for(ic=0;ic<etowEve->size;ic++) {
    float clustET=etowEve->clusterET[ic];
    hA[5]->fill((int)clustET);
    if(clustET<par_eventEtThres) continue;
    hA[6]->fill((int)clustET);
  }

  //........ fill (some) L2Result
  //etowEve->resultBlob.kTicksCompute=mComputeTimeDiff[token]/1000;
  //etowEve->resultBlob.decision=0;
  //etowEve->resultBlob.numberOfL2Clust=etowEve->size;

  float maxClusterEt;
  maxClusterEt=-1;
  int maxClusterID;
  maxClusterID =-1;
  
  //........ compute the final decision
  for(ic=0;ic<etowEve->size;ic++) {
    if(etowEve->clusterET[ic]>maxClusterEt) {
      maxClusterEt = etowEve->clusterET[ic];
      maxClusterID = ic;
    }
  }
  if(maxClusterEt>par_eventEtThres) {
    etowEve->resultBlob.clusterEt=(unsigned char)(etowEve->clusterET[maxClusterID]*256.0/60.0);
    etowEve->resultBlob.TowerID=(unsigned short)etowEve->clusterSeedTow[maxClusterID]+(etowEve->clusterQuad[maxClusterID]<<13);
    etowEve->resultBlob.meanEtaBin=(unsigned char)((int)(etowEve->clusterEta[maxClusterID]*256.0/EtowGeom::mxEtaBin)%256);
    etowEve->resultBlob.meanPhiBin=(unsigned char)((int)(etowEve->clusterPhi[maxClusterID]*256.0/EtowGeom::mxPhiBin)%256);
    etowEve->resultBlob.isolationSum=(unsigned char)1;
    etowEve->resultBlob.Time=(unsigned char)(mComputeTimeDiff[token]/1000);
    etowEve->resultBlob.trigger=(unsigned char)5;
    if (mRandomAccept) {//accept at random - never gets there for events below threshold, JanB
      etowEve->resultBlob.trigger=(unsigned char)7;
    }

    hA[9]->fill(etowEve->clusterQuad[maxClusterID]);
    hA[10]->fill((int)etowEve->clusterET[maxClusterID]);
    if(etowEve->size>= L2eemcGammaEvent2012::mxClust) mhN->fill(15);
    mhN->fill(15); 
  memcpy(myL2Result,&(etowEve->resultBlob),sizeof(L2gammaResult2012));
    return true;
  }

  // never gets there for accepted events, JanB
  if (mRandomAccept) {
    etowEve->resultBlob.trigger=(unsigned char)6;
    etowEve->resultBlob.Time=(unsigned char)(mComputeTimeDiff[token]/1000);
    mhN->fill(16);
  memcpy(myL2Result,&(etowEve->resultBlob),sizeof(L2gammaResult2012));
  return false; //randomAccept is handled in the virtualAlgo, so return false here and let Virtual do the ||
  }

  memcpy(myL2Result,&(etowEve->resultBlob),sizeof(L2gammaResult2012));
  return false;

} 


/* ========================================
  ======================================== */
void
L2eemcGamma2012::finishRunUser() {  /* called once at the end of the run */
  // do whatever you want, log-file & histo-file are still open
  
  if (mLogFile){ 
    fprintf(mLogFile,"finishRunUser-%s bhla bhla\n",getName());
  }
  
}


//=======================================
//=======================================
void 
L2eemcGamma2012::createHisto() {
  setMaxHist(15); // PMN added - histogram count does not seem to be initialiazed anywere.
  //memset(hA,0,sizeof(hA));

  hA[2]=new L2Histo(2,"ETOW-compute: #towers w/ energy /event; x: # ETOW towers; y: counts", 100); 
  hA[3]=new L2Histo(3,"ETOW-compute: #seed ....... ", 100); // title in initRun
  hA[4]=new L2Histo(4,"ETOW-decision: #clust ....... ", 50); // title in initRun

  hA[5]=new L2Histo(5,"ETOW-decision: any cluster ; x: ET(GeV)", 30);
  hA[6]=new L2Histo(6,"ETOW-decision: accepted clust ...  ; x: ET(GeV)", 30);// title in initRun

  hA[7]=new L2Histo(7,"ETOW: Seed Tower Number", 5000); // title in initRun
  hA[8]=new L2Histo(8,"ETOW: Seed Tower Rank", 20); // title in initRun
  hA[9]=new L2Histo(9,"ETOW: Seed Tower Quadrant", 5); // title in initRun
  hA[10]=new L2Histo(10,"ETOW: Cluster Et Sum GeV", 30); // title in initRun
  hA[11]=new L2Histo(11,"ETOW: Cluster Seed Et GeV", 30); // title in initRun
  hA[12]=new L2Histo(12,"ETOW: 10*Seed Et/Cluster Et GeV", 30); // title in initRun
  hA[13]=new L2Histo(13,"ETOW: # non-zero towers per cluster", 5); // title in initRun

}

//=======================================
//=======================================
void 
L2eemcGamma2012::clearEvent(int token){
  memset(wrkEtow_et,0,sizeof(wrkEtow_et)); 
  memset(etow_used,0,sizeof(etow_used)); 
  memset(wrkEtow_tower_seed,0,sizeof(wrkEtow_tower_seed));
  wrkEtow_tower_seed_size=0; 
  //etow_clusterET_size=0;
  mEtow[token].size=0; 
  //previously the fresh flag was not set here, but now it is, since it was never being set anywhere else either.
  mEtow[token].isFresh=L2eemcGammaEvent2012::kDataFresh;
// do not set 'fresh-flag here, only after compute() finishes
  memset(&(mEtow[token].resultBlob),0, sizeof(L2gammaResult2012));
}
  
/* ========================================
  ======================================== */
void 
L2eemcGamma2012::print2(){ // full , local ADC array
  int i;
  printf("pr2-%s: ---ETOW ADC 2D array, only non-zero\n",getName());

  for(i=0;i<mxEtow;i++) {
    if(wrkEtow_et[i]<=0) continue;
    int rdo=mTower2rdo[i];
    float et=wrkEtow_et[i];
    printf("  etow: tower=%4d  rdo=%4d   et=%.3f \n",i,rdo,et);
  }

}

/* ========================================
  ======================================== */
void 
L2eemcGamma2012::print3(){ // seed list
  int i;
  printf("pr3-%s: ---seed list, size=%d\n",getName(),wrkEtow_tower_seed_size);

  for(i=0;i<wrkEtow_tower_seed_size;i++) {
    int tower=wrkEtow_tower_seed[i];
    float et=wrkEtow_et[tower];
    printf("  etow: i=%4d  tower=%4d   et=%.3f \n",i,tower,et);
  }

}

/* ========================================
  ======================================== */
void 
L2eemcGamma2012::print4(int token, int hitSize){ // L2-algo input list
  int i;
  printf("print4 IS NOT Fully FUNCTIONAL **********************\n");
  printf("pr1-%s: ---ETOW Sorted ADC list--- size=%d\n",getName(),hitSize);
  //const HitTower *hit=globEve_etow_hit;
  for(i=0;i< hitSize;i++) {
    int adc=0;//(mEveStream_etow[token].get_hits()[wrkEtow_tower_index[i]]).adc;
    int rdo=0;//(mEveStream_etow[token].get_hits()[wrkEtow_tower_index[i]]).rdo;
    float et=wrkEtow_et[wrkEtow_tower_index[i]];
    float ene=0;//(mEveStream_etow[token].get_hits()[wrkEtow_tower_index[i]]).ene;
    printf("  tower=%2d ",wrkEtow_tower_index[i]);
    printf("  etow: i=%2d rdo=%4d  adc=%d  et=%.3f  ene=%.3f\n",i,rdo,adc,et,ene);
  }
}

/**********************************************************************
  $Log: L2eemcGamma2012.cxx,v $
  Revision 1.4  2012/03/21 18:18:03  jml
  got rid of printfs from 2012 files

  Revision 1.3  2011/10/19 16:12:11  jml
  more 2012 stuff

  Revision 1.2  2011/10/19 15:39:43  jml
  2012

  Revision 1.1  2011/10/18 15:11:42  jml
  adding 2012 algorithms

  Revision 1.1  2011/03/09 16:29:07  pibero
  Added L2gamma2009

  Revision 1.6  2008/01/30 21:56:40  balewski
  E+B high-enery-filter L2-algo fuly functional

  Revision 1.5  2008/01/30 00:47:17  balewski
  Added L2-Etow-calib

  Revision 1.4  2008/01/18 23:29:13  balewski
  now L2result is exported

  Revision 1.3  2008/01/17 23:15:51  balewski
  bug in token-addressed memory fixed

  Revision 1.2  2008/01/16 23:32:35  balewski
  toward token dependent compute()

  Revision 1.1  2007/12/19 02:30:18  balewski
  new L2-etow-calib-2008


 
*/


