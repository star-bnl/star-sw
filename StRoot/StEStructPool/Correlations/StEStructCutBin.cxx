/**********************************************************************
 *
 * $Id: StEStructCutBin.cxx,v 1.7 2006/10/02 22:21:00 prindle Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Cut-bins for building histograms based on kinematic selections
 *               Singleton class with several implementations based on
 *               a mode ID
 *
 ***********************************************************************/
#include "StEStructCutBin.h"
#include "StEStructPairCuts.h"

ClassImp(StEStructCutBin)


StEStructCutBin* StEStructCutBin::mInstance=0;

StEStructCutBin* StEStructCutBin::Instance(){
  if(!mInstance) mInstance=new StEStructCutBin();    
  return mInstance;
}


StEStructCutBin::~StEStructCutBin(){};

void StEStructCutBin::setMode(int mode){

  /* if(mcutModeName && mode!=mcutMode){
    cout<<" Changing cut mode from mode="<<mcutMode<<" to mode="<<mode<<endl;
    delete [] mcutModeName;
    }*/

  bool silent = false;  // do we need to output cutbin info?
  if (mcutModeName && mode==mcutMode) silent = true;
  if (!silent && mode!=mcutMode) cout<<" Changing cut mode from mode="<<mcutMode<<" to mode="<<mode<<endl;
 
  if(mcutModeName) delete [] mcutModeName; 
  mcutModeName=new char[128];

  switch(mode){
  case 0:
    {
      mnumBins=1;
      strcpy(mcutModeName," No Cut Binning ");
      initPtBinMode0();
      break;
    }
  case 1:
    { 
      mnumBins=27;
      strcpy(mcutModeName," yt1 x yt2 Cut Binning, 27 bins ");
      initPtBinMode1();
      break;
    }
  case 2:
    {
      mnumBins=4;
      strcpy(mcutModeName,"Trig/Assoc. Pt, 4 bins");
      initPtBinMode0();
      //setMode(1); // mode 2 is now obsolete ... same as mode 1
      //      mnumBins=56;
      //      strcpy(mcutModeName," yt_sum vs yt_delta Cut Binning, 54 bins");
      //      initPtBinMode2();
      break;
    }
  case 3:
    {
      mnumBins=16;
      strcpy(mcutModeName," yt_sum, yt_delta, same-side, away-side Cut Binning, 16 bins");
      initPtBinMode3();
      break;
    }
  case 4:
    {
      mnumBins=32;
      strcpy(mcutModeName," yt_sum, yt_delta, same-side, away-side, minijet fine binning, 16 bins");
      initPtBinMode4();
      break;
    }
  case 5:
    {
      mnumBins=14;
      strcpy(mcutModeName," same-side, away-side, identified particles, 14 bins");
      initPtBinMode5();
      break;
    }
  default:
    {
      cout<<"Warning: cut bin mode="<<mode<<" not defined "<<endl;
      break;
    }
  }

  mcutMode=mode;
  if (!silent) cout<<"  Cut Bin Mode = "<<printCutBinName()<<endl;
}
int StEStructCutBin::getMode() {
    return mcutMode;
}
//------------------------- Mode=0 ----------------------------------------
// no cut

void StEStructCutBin::initPtBinMode0(){
  mPtBins[0]=0;
  mPtBins[1]=-1;
  mPtBinMin[0]=0.;
  mPtBinMax[0]=9999.;
}

//------------------------ Mode=1 -------------------------------------------

//  ytyt space in even bins: 
//  - 7 bins from 1.0-4.5 (>4.5 included in last bin)
//  - diagonal symmetry
//  - 6 bins (7-12), 5 bins (13-17), 4 bins( 19-22), 2 bins (23-25), 1 bin (27)

static int __yt1_x_yt2_bin[7]={0,7,13,18,22,25,27};

int StEStructCutBin::getCutBinMode1(StEStructPairCuts* pc){

  int imin,imax,istore;
  imin=((StEStructTrack*)pc->Track1())->getYtBin();
  imax=((StEStructTrack*)pc->Track2())->getYtBin();

  if( imin > imax ){
    istore=imin;
    imin=imax;
    imax=istore;
  }
  return __yt1_x_yt2_bin[imin]+imax-imin;
}

void StEStructCutBin::initPtBinMode1(){
  // everyone is 0-6

  for(int i=0;i<7;i++){
    mPtBinMin[i]=0.;
    mPtBinMax[i]=999.;
  }
  mPtBinMin[7]=0.139*sinh(1.5);
  mPtBinMax[7]=0.139*sinh(2.0);
  mPtBinMin[8]=mPtBinMin[13]=mPtBinMax[7];
  mPtBinMax[8]=mPtBinMax[13]=0.139*sinh(2.5);

  mPtBinMin[9]=mPtBinMin[14]=mPtBinMin[18]=mPtBinMax[8];
  mPtBinMax[9]=mPtBinMax[14]=mPtBinMax[18]=0.139*sinh(3.0);

  mPtBinMin[10]=mPtBinMin[15]=mPtBinMin[19]=mPtBinMin[22]=mPtBinMax[9];
  mPtBinMax[10]=mPtBinMax[15]=mPtBinMax[19]=mPtBinMax[22]=0.139*sinh(3.5);

  mPtBinMin[11]=mPtBinMin[16]=mPtBinMin[20]=mPtBinMin[23]=mPtBinMin[25]=mPtBinMax[10];
  mPtBinMax[11]=mPtBinMax[16]=mPtBinMax[20]=mPtBinMax[23]=mPtBinMax[25]=0.139*sinh(4.0);

  mPtBinMin[12]=mPtBinMin[17]=mPtBinMin[21]=mPtBinMin[24]=mPtBinMin[26]=mPtBinMin[27]=mPtBinMax[11];
  mPtBinMax[12]=mPtBinMax[17]=mPtBinMax[21]=mPtBinMax[24]=mPtBinMax[26]=mPtBinMax[27]=999.;

}

//------------------------ Mode=2 -------------------------------------------

// TAKEN OVER FOR TRIGGER STUDY
//********* 
//********* OBSOLETE!!! used to be 2x finer than mode1 but never used
//*********
//*********   so now default to  Mode1

  // binning on yt_sum,yt_delta
  // 13 sum-bins from 2.0-8.5 with <2. & >8.5 included in first & last bins
  // 7 delta-bins from 0-3.5 with >3.5 included in last bin
/*
  static int __ytsum_ytdelta_bin[13][7]={0,13,25,36,46,46,53,
                     1,13,25,36,46,46,53,
                     2,14,25,36,46,46,53,
                     3,15,26,36,46,46,53,
                     4,16,27,37,46,46,53,
                     5,17,28,38,47,47,53,
                     6,18,29,39,48,48,53,
                     7,19,30,40,49,49,53,
                     8,20,31,41,50,50,54,
                     9,21,32,42,51,51,54,
		     10,22,33,43,52,52,54,
                     11,23,34,44,52,52,54,
                     12,24,35,45,52,52,54};

*/

int StEStructCutBin::getCutBinMode2(StEStructPairCuts* pc){

  // Now set to copy a trig/assoc study with pt 2.5<trig<3
  //   .3<assoc<.8 = bin 1,  .8<assoc<1.3 = bin 2, 1.3<assoc<1.8 = bin 3
  //   everything else = bin 0
  
  float min,max,temp;
  int retVal;

  min=pc->Track1()->Pt();
  max=pc->Track2()->Pt();
  if( min > max ){
    temp=min;
    min=max;
    max=temp;
  }
                                                                                                             
  if (max>3.0 || max<2.5) retVal=0;
  else if (min<0.3 || min>1.8) retVal=0;
  else {
    retVal=2;
    if (min<0.8) retVal=1; 
    if (min>1.3) retVal=3;
  }
  
  return retVal;

}

/*
  float s=pc->SigmaYt();
  float d=fabs(pc->DeltaYt());
  int is=(int) floor((s-2.0)/0.5);
  int id=(int) floor(d/0.5);
  
  if(is<0)is=0;
  if(is>12)is=12;
  if(id<0) id=0;
  if(id>6) id=6;

  return __ytsum_ytdelta_bin[is][id];
}
*/
void StEStructCutBin::initPtBinMode2(){ return initPtBinMode1(); }



//------------------------ Mode=3 -------------------------------------------
//
// --> now modified for 
//        soft= yt<1.8
//        neck=1.8<=yt<=2.2
//        hard=yt>2.2
//        rest .. is the rest in the 2pt space ... all pt's satisfy it

// ytyt plot deta,dphi
// 0-3   = soft
// 4-7   = 'neck'
// 8-11  = hard
// 12-15 = rest
// 0,4,8,12 away-side large deta
// 1,5,9,13  away-side small deta
// 2,6,10,14 same-side small deta
// 3,7,11,15 same-side large deta

//static int __yt_deta_dphi_bin[4][4]={0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
static int __yt_deta_dphi_bin[4][4]={ {0,1,2,3}, {4,5,6,7}, {8,9,10,11}, {12,13,14,15} };  // remove compiler warning

int StEStructCutBin::getCutBinMode3(StEStructPairCuts* pc){

  int iyt;
  
  float yt1=pc->Track1()->Yt();
  float yt2=pc->Track2()->Yt();

  if(yt1<1.8 && yt2<1.8){
    iyt=0;
  } else if(yt1<2.2 && yt2<2.2){
    iyt=1;
  } else if(yt1>=2.2 && yt2>=2.2){
    iyt=2;
  } else {
    iyt=3;
  }

  float deta=fabs(pc->DeltaEta());
  //  float dphi=fabs(pc->DeltaPhi());

  int idedp;

  if(deta<1.0) { 
    if(pc->sameSide()){ // dphi<M_PI/2.0 || dphi> 1.5*M_PI){
      idedp=2;
    } else {
      idedp=1;
    }
  } else {    
    if(pc->sameSide()){  // dphi<M_PI/2. || dphi> 1.5*M_PI){
      idedp=3;
    } else {
      idedp=0;
    }
  }   

  return  __yt_deta_dphi_bin[iyt][idedp];
}

void StEStructCutBin::initPtBinMode3(){

    mPtBinMin[0]=mPtBinMin[1]=mPtBinMin[2]=mPtBinMin[3]=0.;
    mPtBinMax[0]=mPtBinMax[1]=mPtBinMax[2]=mPtBinMax[3]=0.139*sinh(1.8);

    mPtBinMin[4]=mPtBinMin[5]=mPtBinMin[6]=mPtBinMin[7]=mPtBinMax[0];
    mPtBinMax[4]=mPtBinMax[5]=mPtBinMax[6]=mPtBinMax[7]=0.139*sinh(2.2);

    mPtBinMin[8]=mPtBinMin[9]=mPtBinMin[10]=mPtBinMin[11]=mPtBinMax[4];
    mPtBinMax[8]=mPtBinMax[9]=mPtBinMax[10]=mPtBinMax[11]=999.;

    mPtBinMin[12]=mPtBinMin[13]=mPtBinMin[14]=mPtBinMin[15]=0.;
    mPtBinMax[12]=mPtBinMax[13]=mPtBinMax[14]=mPtBinMax[15]=999.;

}

//------------------------ Mode=4 -------------------------------------------

/*
   ns, as:

   0,16 = soft (yt<1.8) 
   1-13,17-29 = dyt<1.8, syt>=4.0, binned in 0.25 up to 6.75 then by 0.5
   14,30 = yt>2.0, dyt>2.0
   15,31 = rest

*/

int StEStructCutBin::getCutBinMode4(StEStructPairCuts* pc){

  float yt1=((StEStructTrack*)pc->Track1())->Yt();
  float yt2=((StEStructTrack*)pc->Track2())->Yt();

  //  float dphi=fabs(pc->DeltaPhi());
  int iside=0;

  if( !pc->sameSide() ) iside+=16; // away-side
  if(yt1<=1.8 && yt2<=1.8) return iside;    

  float ytsum=yt1+yt2;
  float ytdel=fabs(yt1-yt2);
  int ival=0;
  if(ytdel<1.8 && ytsum>4.0){
    ival=(int)floor((ytsum-4.0)/0.25)+1;
    if(ival>11){
      if(ival==12 || ival==13){
	ival=12;
      } else {
	ival=13;
      }
    }
  } else if(yt1>2.0 && yt2>2.0){
    ival=14;
  } else {
    ival=15;
  }

  ival+=iside;
  return ival;
}

void StEStructCutBin::initPtBinMode4(){

  /***********************************************
      
  I don't recomment this cut-selecction for pt-correlations ...
  but for completeness I define the pt-binning here  

  *************************************************/

    mPtBinMin[0]=mPtBinMin[16]=0.;
    mPtBinMax[0]=mPtBinMax[16]=0.139*sinh(1.8);

    mPtBinMin[1]=mPtBinMin[17]=0.139*sinh(1.1); // min for yt+yt=4
    mPtBinMax[1]=mPtBinMax[17]=0.139*sinh(3.025); // max for yt+yt=4.25

    mPtBinMin[2]=mPtBinMin[18]=0.139*sinh(1.125); // min for yt+yt=4.25
    mPtBinMax[2]=mPtBinMax[18]=0.139*sinh(3.15); // max for yt+yt=4.5

    mPtBinMin[3]=mPtBinMin[19]=0.139*sinh(1.25); // min for yt+yt=4.5
    mPtBinMax[3]=mPtBinMax[19]=0.139*sinh(3.275); // max for yt+yt=4.75

    //

    mPtBinMin[4]=mPtBinMin[20]=0.139*sinh(1.275); // min for yt+yt=4.75
    mPtBinMax[4]=mPtBinMax[20]=0.139*sinh(3.4); // max for yt+yt=5.

    mPtBinMin[5]=mPtBinMin[21]=0.139*sinh(1.4); // min for yt+yt=5.
    mPtBinMax[5]=mPtBinMax[21]=0.139*sinh(3.525); // max for yt+yt=5.25

    mPtBinMin[6]=mPtBinMin[22]=0.139*sinh(1.525); // min for yt+yt=5.25
    mPtBinMax[6]=mPtBinMax[22]=0.139*sinh(3.65); // max for yt+yt=5.5

    //

    mPtBinMin[7]=mPtBinMin[23]=0.139*sinh(1.65); // min for yt+yt=5.5
    mPtBinMax[7]=mPtBinMax[23]=0.139*sinh(3.775); // max for yt+yt=5.75

    mPtBinMin[8]=mPtBinMin[24]=0.139*sinh(1.775); // min for yt+yt=5.75
    mPtBinMax[8]=mPtBinMax[24]=0.139*sinh(3.9); // max for yt+yt=6.

    mPtBinMin[9]=mPtBinMin[25]=0.139*sinh(1.9); // min for yt+yt=6.
    mPtBinMax[9]=mPtBinMax[25]=0.139*sinh(4.025); // max for yt+yt=6.25

    //

    mPtBinMin[10]=mPtBinMin[26]=0.139*sinh(2.025); // min for yt+yt=6.25
    mPtBinMax[10]=mPtBinMax[26]=0.139*sinh(4.15); // max for yt+yt=6.5

    mPtBinMin[11]=mPtBinMin[27]=0.139*sinh(2.15); // min for yt+yt=6.5
    mPtBinMax[11]=mPtBinMax[27]=0.139*sinh(4.275); // max for yt+yt=6.75

    mPtBinMin[12]=mPtBinMin[28]=0.139*sinh(2.275); // min for yt+yt=6.75
    mPtBinMax[12]=mPtBinMax[28]=0.139*sinh(4.525); // max for yt+yt=7.25

    mPtBinMin[13]=mPtBinMin[29]=0.139*sinh(2.525); // min for yt+yt=7.25
    mPtBinMax[13]=mPtBinMax[29]=999.;              // max for yt+yt=...


    // -- never use these odd regions for pt-correlations in this cut-selection
    mPtBinMin[14]=mPtBinMin[30]=mPtBinMin[15]=mPtBinMin[31]=9999.;
    mPtBinMax[14]=mPtBinMax[30]=mPtBinMax[15]=mPtBinMax[31]=-1.;

    

};


//------------------------ Mode=5 -------------------------------------------
//
// Mode 3 with additions for dE/dx identifications.
//    Use dE/dx to identify pi, K, p within momentum ranges where this
//    is possible. 
// No yt cut for now. Having problem with memory usage (I think)
// and cutting on Ptot (because of dEdx selection) at mid-rapidity
// turns out to be a cut in yt space.
//
// First bit used for eta-phi cut.
//     0 = away-side
//     1 = same-side
// Next three bits for dE/dx (shift by 1 for actual value)
//   000 = pi-pi
//   001 = pi-K
//   010 = pi-p
//   011 = K-K
//   100 = K-p
//   101 = p-p
//   110 = rest
//
// So a bin of 9 (for example) would mean K-p in dE/dx space,
// and same side in eta-phi space.
// Number of bins required is 14.
//
// To check on charge symmetry we have split out the -+ from the +-
// in the other parts of the Correlation code.

int StEStructCutBin::getCutBinMode5(StEStructPairCuts* pc){

  //  float dphi = fabs(pc->DeltaPhi());
  int idedp = 1;
  if(!pc->sameSide()) {
    idedp=0;
  }

  int it1 = getdEdxPID( pc->Track1() );
  int it2 = getdEdxPID( pc->Track2() );
  int ipid = 6;
  if (0 == it1 || 0 == it2) {
      ipid = 6;
      return idedp + 2*ipid;
  }

  if (1 == it1) {
      if (1 == it2) {
          ipid = 0;
      } else if (2 == it2) {
          ipid = 1;
      } else if (3 == it2) {
          ipid = 2;
      } else {
          ipid = 6;
      }
  } else if (2 == it1) {
      if (1 == it2) {
          ipid = 1;
      } else if (2 == it2) {
          ipid = 3;
      } else if (3 == it2) {
          ipid = 4;
      } else {
          ipid = 6;
      }
  } else if (3 == it1) {
      if (1 == it2) {
          ipid = 2;
      } else if (2 == it2) {
          ipid = 4;
      } else if (3 == it2) {
          ipid = 5;
      } else {
          ipid = 6;
      }
  }
  return  idedp + 2*ipid;
}
int StEStructCutBin::ignorePair5(StEStructPairCuts* pc) {

 /*
  * Accept pair if charges are the same.
  */
    int ic1 = pc->Track1()->Charge();
    int ic2 = pc->Track2()->Charge();
    if ( ic1 == ic2 ) {
        return 0;
    }

 /*
  * Ignore particles with the same pid and opposite charge when the first
  * charge is negative.
  * In the main track pair loop every pair will come up twice, the
  * second time in reversed order and we only want it one time.
  */
    int ip1 = getdEdxPID( pc->Track1() );
    int ip2 = getdEdxPID( pc->Track2() );
    if (ip1 == ip2) {
        if (-1 == ic1) {
            return 1;
        } else {
            return 0;
        }
    }
 /*
  * For particles with different pid and opposite charge we only want
  * pair if pi is before K or p or else K is before p.
  */
    if (1 == ip1) {
        if ((2 == ip2) || (3 == ip2)) {
            return 0;
        }
    } else if (2 == ip1) {
        if (3 == ip2) {
            return 0;
        }
    }
    return 1;
}
int StEStructCutBin::symmetrizeYt5(StEStructPairCuts* pc) {

 /*
  * If particle types and charges are the same we symmetrize.
  */
    if ( pc->Track1()->Charge() != pc->Track2()->Charge() ) {
        return 0;
    }
    if (getdEdxPID( pc->Track1() ) != getdEdxPID( pc->Track2() )) {
        return 0;
    }
    return 1;
}
int StEStructCutBin::switchYt5(StEStructPairCuts* pc) {

 /*
  * For different pid want pi before K and p, K before p.
  * For same pid want + before -.
  */
    int ipid1 = getdEdxPID( pc->Track1() );
    int ipid2 = getdEdxPID( pc->Track2() );
    if (ipid1 == ipid2) {
        if ( (-1 == pc->Track1()->Charge()) &&
             (+1 == pc->Track2()->Charge()) ) {
            return 1;
        }
        return 0;
    }
    if (1 == ipid2) {
        if ( (2 == ipid1) || (3 == ipid1) ) {
            return 1;
        }
    } else if (2 == ipid2) {
        if (3 == ipid1) {
            return 1;
        }
    }
    return 0;
}

/*
  Use mPtMin and mPtMax as limits of where we trust pid.
  Index 1 for pi
        2 for K
        3 for p
  Would be nice to initialize these in some way we didn't have to
  recompile to change them.
 */
void StEStructCutBin::initPtBinMode5(){
  // For Hijing we have perfect pid at all pts.
  mPtBinMin[0]=0.;
  mPtBinMax[0]=9999.;
  mPtBinMin[1]=0.;
  mPtBinMax[1]=9999.;
  mPtBinMin[2]=0.;
  mPtBinMax[2]=9999.;
  mPtBinMin[3]=0.;
  mPtBinMax[3]=9999.;
  // For data we want to exclude relativistic rise region which
  // might possibly give us a few tracks.
  mPtBinMin[0] =    0.0;
  mPtBinMax[0] = 9999.0;
  mPtBinMin[1] =    0.0;
  mPtBinMax[1] =    1.0;
  mPtBinMin[2] =    0.0;
  mPtBinMax[2] =    1.0;
  mPtBinMin[3] =    0.0;
  mPtBinMax[3] =    1.5;
}

// pi  -> 1
// K   -> 2
// p   -> 3
// Everything else
//     -> 0
//
// June 8, 2006 djp If track is within 1sigma of electron we
//                  exclude it as pi, K, p. (Tried 2sigma and
//                  visually that looks really bad.)
int StEStructCutBin::getdEdxPID(const StEStructTrack *t) {
  float e  = fabs(t->PIDe());
  if (e < 1) {
      return 0;
  }
  float ptot = t->Ptot();
  float pi = fabs(t->PIDpi());
  float k  = fabs(t->PIDk());
  float p  = fabs(t->PIDp());

  if ((mPtBinMin[1]<ptot) && (ptot<mPtBinMax[1]) && (pi<2.0) && (k>2.0) && (p>2.0)) {
      return 1;
  }
  if ((mPtBinMin[2]<ptot) && (ptot<mPtBinMax[2]) && (pi>2.0) && (k<2.0) && (p>2.0)) {
      return 2;
  }
  if ((mPtBinMin[3]<ptot) && (ptot<mPtBinMax[3]) && (pi>2.0) && (k>2.0) && (p<2.0)) {
      return 3;
  }
  return 0;
}
/***********************************************************************
 *
 * $Log: StEStructCutBin.cxx,v $
 * Revision 1.7  2006/10/02 22:21:00  prindle
 * Store only quadrant of eta_Delta - phi_Delta array/histogram.
 * Store half of eta_Sigma - phi_Delta array/histogram.
 * This required modifications in Binning.
 * I had a bug in the pair loop (which left +- not fully symmetrized)
 * and had to make changes in cut bins for mode 5 (and 3 I think)
 * when I fixed this.
 * Also change crossing cut to use only two parameters, the sign of
 * the magnetic field being taken from the MuDst.
 *
 * Revision 1.6  2006/04/10 23:42:32  porter
 * Added sameSide() & awaySide() methods to PairCut (so only defined in 1 place)
 * and added the eta_delta weighting as a binned correctin defined by the eta-limits in
 * the StEStructBinning object
 *
 * Revision 1.5  2006/04/06 01:01:19  prindle
 *
 *   New mode in CutBin, 5, to do pid correlations. There is still an issue
 * of how to set the pt ranges allowed for the different particle types.
 * For data we probably want to restrict p to below 1GeV for pi and K, but
 * for Hijing and Pythia we can have perfect pid. Currently cuts are type
 * into the code (so you have to re-compile to change them.)
 *
 *   In the Correlations code I split -+ from +- and am keeping track of
 * pt for each cut bin. These required changes in the Support code.
 *
 * Revision 1.4  2006/02/22 22:05:16  prindle
 * Removed all references to multRef (?)
 * Added cut mode 5 for particle identified correlations.
 * Other cut modes should be same as before
 *
 * Revision 1.3  2005/09/14 17:14:23  msd
 * Large update, added new pair-cut system, added pair density plots for new analysis mode (4), added event mixing cuts (rewrote buffer for this)
 *
 * Revision 1.2  2005/03/03 01:30:44  porter
 * updated StEStruct2ptCorrelations to include pt-correlations and removed
 * old version of pt-correlations from chunhuih (StEStruct2ptPtNbar)
 *
 * Revision 1.1  2004/06/25 03:11:49  porter
 * New cut-binning implementation and modified pair-cuts for chunhui to review
 *
 *
 *********************************************************************/
