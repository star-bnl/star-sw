/**********************************************************************
 *
 * $Id: StEStructCutBin.cxx,v 1.1 2004/06/25 03:11:49 porter Exp $
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

/*
StEStructCutBin* StEStructCutBin::Instance(int mode){
  if(!mInstance) {
      mInstance=new StEStructCutBin(mode);    
  } else if(mode!=mcutMode){
    cout<<"****  Warning  ***** Changing CUT Mode "<<endl;
    cout<<" From StEStructBinning::Instance("<<mcutMode<<")";
    cout<<" to StEStructBinning::Instance("<<mode<<")"<<endl;   
    mInstance->setMode(mode);
  }
  return mInstance;
}
*/

StEStructCutBin::~StEStructCutBin(){};

void StEStructCutBin::setMode(int mode){

  if(mcutModeName){
    cout<<" Changing cut mode from mode="<<mcutMode<<" to mode="<<mode<<endl;
    delete [] mcutModeName;
  }
  mcutModeName=new char[64];

  switch(mode){
  case 0:
    {
      mnumBins=1;
      strcpy(mcutModeName," No Cut Binning ");
      break;
    }
  case 1:
    { 
      mnumBins=27;
      strcpy(mcutModeName," yt1 x yt2 Cut Binning, 27 bins ");
      break;
    }
  case 2:
    {
      mnumBins=54;
      strcpy(mcutModeName," yt_sum vs yt_delta Cut Binning, 54 bins");
      break;
    }
  case 3:
    {
      mnumBins=16;
      strcpy(mcutModeName," yt_sum, yt_delta, same-side, away-side Cut Binning, 16 bins");
      break;
    }
  default:
    {
      cout<<"Warning: cut bin mode="<<mode<<" not defined "<<endl;
      break;
    }
  }

  mcutMode=mode;
  cout<<"  Cut Bin Mode = "<<printCutBinName()<<endl;
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

//------------------------ Mode=2 -------------------------------------------

  // binning on yt_sum,yt_delta
  // 13 sum-bins from 2.0-8.5 with <2. & >8.5 included in first & last bins
  // 7 delta-bins from 0-3.5 with >3.5 included in last bin
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

int StEStructCutBin::getCutBinMode2(StEStructPairCuts* pc){

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


//------------------------ Mode=3 -------------------------------------------
// ytyt plot deta,dphi
// 0-3   = soft
// 4-7   = hard
// 8-11  = 'neck'
// 12-15 = rest
// 0,4,8,12 away-side large deta
// 1,5,9,13  away-side small deta
// 2,6,10,14 same-side small deta
// 3,7,11,15 same-side large deta

static int __yt_deta_dphi_bin[4][4]={0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};

int StEStructCutBin::getCutBinMode3(StEStructPairCuts* pc){

  int iyt;
  
  float s=pc->SigmaYt();
  float d=fabs(pc->DeltaYt());

  if(s<3.2){
    iyt=0;
  } else if(pc->Track1()->Yt()>2.0 && pc->Track2()->Yt()>2.0){
    iyt=2;
  } else if(d<0.5){
    iyt=1;
  } else {
    iyt=3;
  }

  float deta=fabs(pc->DeltaEta());
  float dphi=fabs(pc->DeltaPhi());

  int idedp;

  if(deta<1.0) {
    if(dphi<1.0){
      idedp=2;
    } else if((M_PI-dphi)<M_PI/2.){
      idedp=1;
    } else {
      idedp=3;
    }
  } else {    
    if((M_PI-dphi)<M_PI/2.){
      idedp=0;
    } else {
      idedp=3;
    }
  }   

  return  __yt_deta_dphi_bin[iyt][idedp];
}

/***********************************************************************
 *
 * $Log: StEStructCutBin.cxx,v $
 * Revision 1.1  2004/06/25 03:11:49  porter
 * New cut-binning implementation and modified pair-cuts for chunhui to review
 *
 *
 *********************************************************************/
