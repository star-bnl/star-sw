#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>


#ifdef  IS_REAL_L2  //in l2-ana  environment
  #include "../L2algoUtil/L2EmcDb2012.h"
  #include "../L2algoUtil/L2Histo.h"
#else    //full path needed for cvs'd code
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2EmcDb2012.h"
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2Histo.h"
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2EmcGeom2012.h"
#endif

#include "L2Upsilon2012.h"
#include "read_in_geog.h"
//extern inline void *memset(void *s, int c, size_t n);


//=================================================
//=================================================
L2Upsilon2012::L2Upsilon2012(const char* name, const char *uid, L2EmcDb2012* db, L2EmcGeom2012 *geoX, char* outDir, int resOff)  :  L2VirtualAlgo2012( name, uid, db, outDir, true, false, resOff) { 
  /* called one per days
     all memory allocation must be done here
  */

  mGeom=geoX;
  if (!mGeom)
    criticalError("L2Upsilon2012 is broken -- can't find geom.");

  setMaxHist(32); // set upper range, I uses only 2^N -it is easier to remember
  createHisto();

  //------- self-consistency checks, should never fail
  if (!(sizeof(L2UpsilonResult2012)== L2UpsilonResult2012::mySizeChar))
    {
      criticalError("L2Upsilon2012 has failed consistency check 'sizeof(L2UpsilonResult2012)== L2UpsilonResult2012::mySizeChar'");
    }

  prescale = 1;
}

/* ========================================
  ======================================== */
int 
L2Upsilon2012::initRunUser( int runNo, int *rc_ints, float *rc_floats) {
	
  // unpack params from run control GUI

  par_prescale      =  rc_ints[0];
  fMaxDynamicMaskTowers   =rc_ints[1];
  fHowManyEventPerUpdateDynamicMask=rc_ints[2];
  par_RndAcceptPrescale=rc_ints[3]; //added by Ross.

  fL0SeedThreshold = rc_floats[0];
  fL2SeedThreshold = rc_floats[1];
  fMinL0ClusterEnergy = rc_floats[2];
  fMinL2ClusterEnergy = rc_floats[3];
  fMinInvMass         = rc_floats[4];
  fMaxInvMass         = rc_floats[5];
  fMaxCosTheta        = rc_floats[6];
  fHotTowerThreshold=rc_floats[7];
  fThresholdRatioOfHotTower=rc_floats[8];

  fHotTowerSeenTimesThreshold=int(fHowManyEventPerUpdateDynamicMask*fThresholdRatioOfHotTower);

/*   suggested pars
  par_prescale      =  1;
  fMaxDynamicMaskTowers   =25;
  fHowManyEventPerUpdateDynamicMask=50;

  fL0SeedThreshold = 4.;
  fL2SeedThreshold = 2.5;
  fMinL0ClusterEnergy = 4.5;
  fMinL2ClusterEnergy = 3.;
  fMinInvMass         = 5;
  fMaxInvMass         = 20;
  fMaxCosTheta        = 0;
  fHotTowerThreshold=2.5;
  fThresholdRatioOfHotTower=0.5;
  
  fHotTowerSeenTimesThreshold=25;
*/

/*
---  temp par values for test -----
  par_prescale =2;
  fL0SeedThreshold = 1.;
  fL2SeedThreshold = 1;
  fMinL0ClusterEnergy = 1;
  fMinL2ClusterEnergy = 1.;
  fMinInvMass         = 1;
  fMaxInvMass         = 20;
  fMaxCosTheta        = 1;
//--------------------------------   
*/

	EventSeen=0;
	event_accept=0;
	memset(wrkDynamicMask_tower_stat,0,sizeof(wrkDynamicMask_tower_stat)); 
	wrkNumberOfMasked=0;

    // verify consistency of input params
  int kBad=0;
  kBad+=0x00001 * (fMaxCosTheta>1.0||fMaxCosTheta<-1.0);
  kBad+=0x00002 * (fL0SeedThreshold<fL2SeedThreshold);
  kBad+=0x00004 * (fMinL0ClusterEnergy<fMinL2ClusterEnergy);
  kBad+=0x00008 * (fMinInvMass>fMaxInvMass);
  kBad+=0x00010 * (fHowManyEventPerUpdateDynamicMask<fHotTowerSeenTimesThreshold);
  kBad+=0x00020 * (fMaxDynamicMaskTowers>100);
  kBad+=0x00040 * (fThresholdRatioOfHotTower>1);

  if (mLogFile) { 
    fprintf(mLogFile," L2%s upsilon algorithm initRun(R=%d), compiled: %s , %s\n params:\n",getName(),mRunNumber,__DATE__,__TIME__);
    fprintf(mLogFile," initRun() params checked for consistency, Error flag=%d\n",kBad);
    fprintf(mLogFile," prescale=%d \n",  par_prescale);
    fprintf(mLogFile," rndAccept prescale=%d \n",  par_RndAcceptPrescale);
    fprintf(mLogFile," L0SeedThreshold= %.2f  (GeV)\n",fL0SeedThreshold);
    fprintf(mLogFile," L2SeedThreshold= %.2f  (GeV)\n",fL2SeedThreshold);
    fprintf(mLogFile," MinL0ClusterEnergy= %.2f  (GeV)\n",fMinL0ClusterEnergy);
    fprintf(mLogFile," MinL2ClusterEnergy= %.2f  (GeV)\n",fMinL2ClusterEnergy);
    fprintf(mLogFile," MinInvMass= %.2f  (GeV)\n",fMinInvMass);
    fprintf(mLogFile," MaxInvMass= %.2f  (GeV)\n",fMaxInvMass);
    fprintf(mLogFile," MaxCosTheta= %.2f  \n",fMaxCosTheta);
    fprintf(mLogFile," MaxDynamicMaskedTowers= %d (should not greater than 100) \n",fMaxDynamicMaskTowers);
    fprintf(mLogFile," HotTowerThreshold= %f  (GeV) \n",fHotTowerThreshold);
    fprintf(mLogFile," HowManyEventsPerUpdateDynamicMask= %d \n",fHowManyEventPerUpdateDynamicMask);
    fprintf(mLogFile," fHotTowerSeenTimesThreshold= %d   \n",fHotTowerSeenTimesThreshold);
    fprintf(mLogFile," fThresholdRatioOfHotTower= %f  (count_hot / count_check) \n",fThresholdRatioOfHotTower);
  }


  // clear content of all histograms & token-dependet memory
  int i;
  for (i=0; i<mxHA;i++) if(hA[i])hA[i]->reset();
  memset(mBtow,0,sizeof(mBtow));

  if(kBad>0) return -1*kBad;    
  else if(kBad<0) return kBad;
 
  for ( int index=0; index<EmcDbIndexMax; index++ )
     {
       const L2EmcDb2012::EmcCDbItem *x = mDb->getByIndex(index);
       if ( x==0 ) continue;
       if ( !mDb->isBTOW(x) ) continue; 
       int sec = x->sec - 1;
       int sub = 8192; 
       sub = x->sub - 'a';
       int eta = x->eta - 1;
       int phi = BtowGeom::mxSubs *sec + sub;
       int tow = BtowGeom::mxEtaBin *phi + eta; // phi- changes faster
       int rdo = x->rdo;
       if (tow<0 || tow>mxBtow || rdo<0 || rdo>mxBtow) return -101;
       
       mTower2rdo[ tow ] = rdo;    // returns rdo channel for given tower
       mRdo2tower[ rdo ] = tow;    // returns tower for given rdo channel

		int a,b,c,d;
		sscanf(x->tube,"id%d-%d-%d-%d",&a,&b,&c,&d);
		rdo2softID[x->rdo]=a;
//		hA[20]->fill(a,x->rdo);
	}
  return 0; //OK

}               


/* ========================================
  ======================================== */
void 
L2Upsilon2012::computeUser(int token){
  clearEvent(token);

  EventSeen++;
  if(EventSeen%fHowManyEventPerUpdateDynamicMask==0) update_DynamicMask();

  L2UpsilonEvent2012 *btowEve=mBtow+token;
  const HitTower1 *hit=mEveStream_btow[token].get_hits();
  const int hitSize=mEveStream_btow[token].get_hitSize();

  for(int i=0;i< hitSize;i++,hit++) {
//    int tower=mRdo2tower[hit->rdo];   
    int tower=rdo2softID[hit->rdo];
	wrkBtow_tower[i]=tower;
    wrkBtow_ene[tower]=hit->ene;     //    fill energy array    (all towers)
	if(wrkBtow_ene[tower] > fHotTowerThreshold)   wrkDynamicMask_tower_stat[tower]++;  //  wrkDynamicMask_tower_stat is needed to judge hot towers
	hA[2]->fill(int(wrkBtow_ene[tower]));
  }
  hA[1]->fill(hitSize);

//  Dynamic Masking 
	for(int j=0;j<wrkNumberOfMasked;j++) 
		wrkBtow_ene[wrkDynamicMasked_tower[j]]=0;

// ---- store all L0 L2 candidates
  wrkNumberOfL0=0;
  wrkNumberOfL2=0;
  for(int i=0;i< hitSize;i++) {
	int tower=wrkBtow_tower[i];
	if(wrkBtow_ene[tower] > fL2SeedThreshold){
		float clusterE=wrkBtow_ene[tower];
		for(int k=0;k<geog_Nneighbors[tower];k++){
			float Ek=wrkBtow_ene[geog_neighbor[tower][k]];
			int Ecount=0;
			for(int m=0;m<geog_Nneighbors[tower];m++){
				if(m==k) continue;
				if(Ek<wrkBtow_ene[geog_neighbor[tower][m]])Ecount++;
			}
			if(Ecount<2)clusterE+=Ek;  // top 2 cluster
		}

		if(clusterE>fMinL2ClusterEnergy) {   //   tower seed good && cluster good  for L2 threshold
			wrkL2_seed_tower[wrkNumberOfL2]=tower;
			wrkL2_seed_ClusterE[wrkNumberOfL2]=clusterE;
			hA[6]->fill(int(wrkBtow_ene[tower]));
			hA[8]->fill(int(clusterE));
			wrkNumberOfL2++;
			if(wrkBtow_ene[tower] > fL0SeedThreshold&&clusterE>fMinL0ClusterEnergy) {   //   tower seed good && cluster good  for L0 threshold
				wrkL0_seed_tower[wrkNumberOfL0]=tower;
				wrkL0_seed_ClusterE[wrkNumberOfL0]=clusterE;
				hA[5]->fill(int(wrkBtow_ene[tower]));
				hA[7]->fill(int(clusterE));
				wrkNumberOfL0++;
			}
		}
	}
  }
  hA[3]->fill(wrkNumberOfL2,wrkNumberOfL0);

//  ------  pair L0 L2, calculate invMass
	for(int i=0;i<wrkNumberOfL0;i++){
		int idL2,idL0;
		float cosTheta,invMass;
		idL0= wrkL0_seed_tower[i];
		for(int j=0;j<wrkNumberOfL2;j++){
			idL2= wrkL2_seed_tower[j];
			if(idL0==idL2)continue;
			cosTheta=geog_x[idL0]*geog_x[idL2]+geog_y[idL0]*geog_y[idL2]+geog_z[idL0]*geog_z[idL2];
			if(cosTheta>fMaxCosTheta)  continue;
			invMass = sqrt(2 *wrkL0_seed_ClusterE[i] * wrkL2_seed_ClusterE[j] * (1 - cosTheta));
			if(fMinInvMass>invMass || invMass>fMaxInvMass) continue;
			hA[10]->fill(int(invMass));
			// accept !!!
			btowEve->resultBlob.L0SeedTowerID=idL0;
			btowEve->resultBlob.L2SeedTowerID=idL2;
			if(wrkL0_seed_ClusterE[i]>25.5)wrkL0_seed_ClusterE[i]=25.5;
			if(wrkL0_seed_ClusterE[j]>25.5)wrkL0_seed_ClusterE[j]=25.5;
			if(invMass>25.5)invMass=25.5;
			btowEve->resultBlob.energyOfL0Cluster=(unsigned char)(wrkL0_seed_ClusterE[i]*10);  //   256/25.6=10
			btowEve->resultBlob.energyOfL2Cluster=(unsigned char)(wrkL2_seed_ClusterE[j]*10);  //   256/25.6=10
			btowEve->resultBlob.invMass=(unsigned char)(invMass*10);  //   256/25.6=10
			btowEve->resultBlob.trigger=true;
//			btowEve->resultBlob.numberOfL0SeedTowers=wrkNumberOfL0;
//			btowEve->resultBlob.numberOfL2SeedTowers=wrkNumberOfL2;
			return;
		}
	}
//// abort
	btowEve->resultBlob.L0SeedTowerID=5555;
	btowEve->resultBlob.L2SeedTowerID=5555;
	btowEve->resultBlob.energyOfL0Cluster=0;
	btowEve->resultBlob.energyOfL2Cluster=0;
	btowEve->resultBlob.invMass=0;
	btowEve->resultBlob.trigger=false;
//	btowEve->resultBlob.numberOfL0SeedTowers=wrkNumberOfL0;
//	btowEve->resultBlob.numberOfL2SeedTowers=wrkNumberOfL2;
  return;
}


/* ========================================
  ======================================== */
bool 
L2Upsilon2012::decisionUser(int token, int *myL2Result){
  // INPUT: token + comput() results stored internally
  // OUTPUT: yes/no + pointer to  L2Result

  // get pointers to internal private event storage
  L2UpsilonEvent2012 *btowEve=mBtow+token;
//  (*myL2Result)=&(btowEve->resultBlob);
  memcpy(myL2Result,&(btowEve->resultBlob),sizeof(L2UpsilonResult2012));
  if (par_prescale > 1) {
    prescale++;
	prescale%=par_prescale;
    if(prescale!=0)    btowEve->resultBlob.trigger=false;
  }
  btowEve->isFresh++; // mark the data as  stale
	hA[13]->fill(1);

	if(btowEve->resultBlob.trigger){
		event_accept++;
		hA[13]->fill(2);
		hA[14]->fill(btowEve->resultBlob.L0SeedTowerID);
		hA[15]->fill(btowEve->resultBlob.L2SeedTowerID);
		hA[16]->fill(int((btowEve->resultBlob.energyOfL0Cluster)*0.1));
		hA[17]->fill(int((btowEve->resultBlob.energyOfL2Cluster)*0.1));
		hA[18]->fill(int((btowEve->resultBlob.invMass)*0.1));
	}
	else hA[13]->fill(3);

  return btowEve->resultBlob.trigger;
} 


/* ========================================
  ======================================== */
void
L2Upsilon2012::finishRunUser() {  /* called once at the end of the run */
  // do whatever you want, log-file & histo-file are still open
  
  if (mLogFile){ 
    fprintf(mLogFile,"finishRunUser - %s \n",getName());
    fprintf(mLogFile,"EventSeen = %d \n",EventSeen);
    fprintf(mLogFile,"event accept = %d \n",event_accept);
  }
  
}


//=======================================
//=======================================
void 
L2Upsilon2012::createHisto() {
  setMaxHist(32); // PMN added - histogram count does not seem to be initialiazed anywere.
  //memset(hA,0,sizeof(hA));

	hA[1]=new L2Histo(1,"Upsilon:# of hits (no L0 required)", 200);
	hA[2]=new L2Histo(2,"Upsilon:energy for all hits (GeV)  (no L0 required)", 20);
	hA[3]=new L2Histo(3,"Upsilon: # of L0 candidates vs. # of L2 candidates (no L0 required)", 50,50);

	hA[5]=new L2Histo(5,"Upsilon: L0 seeds Energy (GeV) (no L0 required)", 20);
	hA[6]=new L2Histo(6,"Upsilon: L2 seeds Energy (GeV) (no L0 required)", 20);

	hA[7]=new L2Histo(7,"Upsilon: L0 cluster Energy (GeV) (no L0 required)", 25);
	hA[8]=new L2Histo(8,"Upsilon: L2 cluster Energy (GeV) (no L0 required)", 25);

	hA[10]=  new L2Histo(10,"Upsilon: InvMass of accepted pair (GeV) (no L0 required)", 25);
	hA[11]=new L2Histo(11,"Upsilon: # of masked towers  (no L0 required)", 100);
	hA[12]=new L2Histo(12,"Upsilon: masked tower softid  (no L0 required) ", 4801);

	hA[13] = new L2Histo(13,"Upsilon: L0 counter (1=L0 fired, 2=L0 fired+L2 accepted, 3=L0 fired+L2 NOT accepted)",5);
	hA[14] = new L2Histo(14,"Upsilon: L0 seed tower id (L0 fired+L2 accepted)",4801);
	hA[15] = new L2Histo(15,"Upsilon: L2 seed tower id (L0 fired+L2 accepted)",4801);
	hA[16]=  new L2Histo(16,"Upsilon: L0 cluster Energy (GeV) (L0 fired+L2 accepted)", 25);
	hA[17]=  new L2Histo(17,"Upsilon: L2 cluster Energy (GeV) (L0 fired+L2 accepted)", 25);
	hA[18]=  new L2Histo(18,"Upsilon: InvMass of accepted pair (GeV) (L0 fired+L2 accepted)", 25);

//	hA[20]=new L2Histo(20,"id",4801,4801);

}

//=======================================
//=======================================
void 
L2Upsilon2012::clearEvent(int token){
  memset(wrkBtow_ene,0,sizeof(wrkBtow_ene)); 
  memset(wrkBtow_tower,0,sizeof(wrkBtow_tower)); 
  memset(wrkL2_seed_tower,0,sizeof(wrkL2_seed_tower)); 
  memset(wrkL2_seed_ClusterE,0,sizeof(wrkL2_seed_ClusterE)); 
  memset(wrkL0_seed_tower,0,sizeof(wrkL0_seed_tower)); 
  memset(wrkL0_seed_ClusterE,0,sizeof(wrkL0_seed_ClusterE)); 
  memset(&(mBtow[token].resultBlob),0, sizeof(L2UpsilonResult2012));
  wrkNumberOfL2=wrkNumberOfL0=0;
}

void
L2Upsilon2012::update_DynamicMask(){
	int count=0;
	for(int i=0;i<mxBtow;i++){
		if(wrkDynamicMask_tower_stat[i]>fHotTowerSeenTimesThreshold && count<fMaxDynamicMaskTowers){   // tower i is hot
			wrkDynamicMasked_tower[count]=i;
			hA[12]->fill(i);
			count++;
		}
	}
	memset(wrkDynamicMask_tower_stat,0,sizeof(wrkDynamicMask_tower_stat)); 
	wrkNumberOfMasked=count;
	hA[11]->fill(count);
}
