/**********************************************************************
 *
 * $Id: StEStructCutBin.cxx,v 1.17 2012/11/16 21:22:27 prindle Exp $
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
      mnumParentBins=1;
      strcpy(mcutModeName," No Cut Binning ");
      break;
    }
  case 1:
    { 
      mnumBins=27;
      mnumParentBins=1;
      strcpy(mcutModeName," yt1 x yt2 Cut Binning, 27 bins ");
      break;
    }
  case 2:
    {
      mnumBins=6;
      mnumParentBins=1;
      strcpy(mcutModeName,"Simple soft/hard same-side/away-side, 6 bins");   // *** new change
      break;
    }
  case 3:
    {
      mnumBins=16;
      mnumParentBins=3;
      strcpy(mcutModeName," yt_sum, yt_delta, same-side, away-side Cut Binning, 16 bins");
      break;
    }
  case 4:
    {
      mnumBins=32;
      mnumParentBins=1;
      strcpy(mcutModeName," yt_sum, yt_delta, same-side, away-side, minijet fine binning, 16 bins");
      break;
    }
  case 5:
    {
      // mnumBins=14;
      mnumBins=10;
      mnumParentBins=4;
      // strcpy(mcutModeName," same-side, away-side, identified particles, 14 bins");
      strcpy(mcutModeName," identified particles, 10 bins");
      break;
    }
  case 6:
    {
      mnumBins=10;
      mnumParentBins=1;
      strcpy(mcutModeName," event-wise z-vertex binning, 10 bins");
      break;
    }
  case 7:
    {
      mnumBins=60;
      mnumParentBins=1;
      strcpy(mcutModeName," event-wise z-vertex binning & soft/hard SS/AS, 60 bins");
      break;
    }
  case 8:
    {
      mnumBins=6;
      mnumParentBins=3;
      strcpy(mcutModeName," Checking LS pair cuts for soft-hard combinations, 6 bins");
      break;
    }
  case 9:
    {
      mnumBins=21;
      mnumParentBins=6;
      strcpy(mcutModeName," only p_t binning");
      break;
    }
  case 10:
    {
      mnumBins=45;
      mnumParentBins=1;
      strcpy(mcutModeName," yt x yt dependence, 45 bins");
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
void StEStructCutBin::setCutBinHistMode(int mode) {
    mcutBinHistMode = mode;
}
int StEStructCutBin::getCutBinHistMode() {
    return mcutMode;
}
//------------------------- Mode=0 ----------------------------------------
// no cut

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

// now trying simple soft/hard SS/AS binning
//   using cut from pp ytxyt paper with soft == yt_sum < 3.3
//   should also require yt > 2 for hard, but I don't want to make a third case right now
//   0 = soft SS;  1 = hard SS;  2 = other SS;  3 = soft AS;  4 = hard AS;  5 = other AS;

int StEStructCutBin::getCutBinMode2(StEStructPairCuts* pc){

  int retVal;
  int iyt, idp;
  
  float yt1=pc->Track1()->Yt();
  float yt2=pc->Track2()->Yt();

  iyt = 2;
  if (yt1+yt2 <= 3.3) iyt = 0;
  if (yt1>=2 && yt2>=2) iyt = 1;

  if (pc->sameSide()) idp = 0;
  else idp = 1;

  retVal = iyt + 3*idp;

  return retVal;

}



//------------------------ Mode=3 -------------------------------------------
//
// --> now modified for 
//        soft= yt<1.99  = 0.5 GeV
//        neck=1.99<=yt<=2.383  = 0.75 GeV
//        hard=yt>2.383
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

  // These numbers are also used in StEStructCutBin::getParentBin (change both)
  if(yt1<1.99 && yt2<1.99){
    iyt=0;
  } else if( ((1.99<yt1) && (yt1<2.383)) && ((1.99<yt2) && (yt2<2.383))) {
    iyt=1;
  } else if(yt1>=2.383 && yt2>=2.383){
    iyt=2;
  } else {
    iyt=3;
  }

  float deta=fabs(pc->DeltaEta());
  //  float dphi=fabs(pc->DeltaPhi());

  int idedp;

  if(deta<0.5*mMaxDEta) { 
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
int StEStructCutBin::symmetrizeXX3(StEStructPairCuts* pc) {

 /*
  * Off diagonal yt-yt bins are not symmetrized.
  */
    if (getCutBinMode3(pc) < 12) {
        return 1;
    }
    return 0;
}
int StEStructCutBin::switchXX3(StEStructPairCuts* pc) {

 /*
  * For soft-neck and soft-hard want soft first.
  * For neck-hard want neck first.
  */
    if (getCutBinMode3(pc) < 12) {
        return 0;
    }
    if (pc->Track1()->Yt() > pc->Track2()->Yt()) {
        return 1;
    }
    return 0;
}
int StEStructCutBin::notSymmetrizedXX3(int cutBin, int pairCharge) {
    // Used in HAdd.
    // Return true if XX histogram was not symmetrized (i.e. phi,phi).
    // This is case for off-diagonal Yt-Yt.
    if (cutBin < 12) {
        return 0;
    }
    return 1;
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

//------------------------ Mode=5 -------------------------------------------
//
// Particle id using dE/dx plus ToF identification.
//    If we have both dEdx and ToF information require it to agree.
// With ToF we can think of Yt cuts although ToF efficiency appears to be around 60% which might be a problem
//
// The cut values and their meanings are:
//   0  pi-o  pair (where o is not pi, K or p)
//   1  pi-pi pair.
//   2  pi-K  pair
//   3  pi-p  pair
//   4  K-o   pair (where o is not pi, K or p)
//   5  K-K   pair
//   6  K-p   pair
//   7  p-o   pair (where o is not pi, K or p)
//   8  p-p   pair
//   9  o-o   pair (where o is not pi, K or p)
//
// To check on charge symmetry we have split out the -+ from the +-
// in the other parts of the Correlation code.

int StEStructCutBin::getCutBinMode5(StEStructPairCuts* pc, int pairCase) {

    int mode[4][4] = {{9, 0, 4, 7}, {0, 1, 2, 3}, {4, 2, 5, 6}, {7, 3, 6, 8}};

    int it1 = pc->Track1()->PID();
    int it2 = pc->Track2()->PID();
    if (it1 < 0 || 3 < it1) {
        return -1;
    }
    if (it2 < 0 || 3 < it2) {
        return -2;
    }
    int iBin = mode[it1][it2];

    if (!mcutBinHistMode) {
        return iBin;
    }

    // Might want to make invariant mass cuts.
    double e, e1, e2, p1, p2, p[3], m, m1, m2;
    p1   = pc->Track1()->Ptot();
    p2   = pc->Track2()->Ptot();
    p[0] = pc->Track1()->Px() + pc->Track2()->Px();
    p[1] = pc->Track1()->Py() + pc->Track2()->Py();
    p[2] = pc->Track1()->Pz() + pc->Track2()->Pz();
    float Mass[]  = {0.1396,  0.1396,  0.497, 0.9383};
    float Mass2[] = {0.01949, 0.01949, 0.247, 0.880};
    if (9 == iBin) {
        // For o-o try using m1 = m2 = 0.
        m1 = 0;
        m2 = 0;
        e1 = p1;
        e2 = p2;
    } else {
        m1 = Mass[it1];
        m2 = Mass[it2];
        e1 = sqrt(p1*p1 + Mass2[it1]);
        e2 = sqrt(p2*p2 + Mass2[it2]);
    }
    e = e1 + e2;
    m = sqrt(e*e - p[0]*p[0] - p[1]*p[1] - p[2]*p[2]);

    mHCutBinHists[pairCase][iBin]->Fill(m - m1 - m2 + 0.1);

    e1 = sqrt(p1*p1 + Mass2[1]);
    e2 = sqrt(p2*p2 + Mass2[1]);
    e = e1 + e2;
    m = sqrt(e*e - p[0]*p[0] - p[1]*p[1] - p[2]*p[2]) - Mass[1] - Mass[1] + 0.1;
    mHCutBinHists[pairCase][10]->Fill(m);

    return iBin;
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
  *
  * If both particles are un-identified we put pair in ipid=9
  * bin and treat as identical.
  */
    int ip1 = pc->Track1()->PID();
    int ip2 = pc->Track2()->PID();
    if (ip1 == ip2) {
        if (-1 == ic1) {
            return 1;
        } else {
            return 0;
        }
    }
 /*
  * For particles with different pid and opposite charge we only keep if
  * o < pi < K < p
  */
    if (ip1 < ip2) {
        return 0;
    }
    return 1;
}
int StEStructCutBin::symmetrizeXX5(StEStructPairCuts* pc) {

 /*
  * If particle types and charges are the same we symmetrize.
  * If both particles are un-identified the pair will be treated
  * as identical.
  */
    if ( pc->Track1()->Charge() != pc->Track2()->Charge() ) {
        return 0;
    }
    int ip1 = pc->Track1()->PID();
    int ip2 = pc->Track2()->PID();
    if (ip1 != ip2) {
        return 0;
    }
    return 1;
}
int StEStructCutBin::switchXX5(StEStructPairCuts* pc) {

 /*
  * For different pid order as 0 < pi < K < p.
  * For same pid want + before -.
  */
    int ipid1 = pc->Track1()->PID();
    int ipid2 = pc->Track2()->PID();
    if (ipid1 == ipid2) {
        if ( (-1 == pc->Track1()->Charge()) &&
             (+1 == pc->Track2()->Charge()) ) {
            return 1;
        }
        return 0;
    }
    if (ipid2 < ipid1) {
        return 1;
    }
    return 0;
}
int StEStructCutBin::notSymmetrizedXX5(int cutBin, int pairCharge) {
    // Used in HAdd.
    // Return true if XX histogram was not symmetrized (i.e. phi,phi).
    if (1==cutBin || 5==cutBin || 8==cutBin || 9==cutBin) {
        if (0==pairCharge || 3==pairCharge) {
            return 0;
        }
    }
    return 1;
}
void StEStructCutBin::initCutBinHists5(){
    if (mcutBinHistMode) {
        TString hname;
        const char *types[] = {"Sibpp", "Sibpm", "Sibmp", "Sibmm",
                               "Mixpp", "Mixpm", "Mixmp", "Mixmm"};
        const char *bases[] = {"piAll", "pipi", "piK", "pip",
                               "KAll",  "KK",   "Kp",  "pAll",
                               "pp",    "OO",   "All"};
        for (int pairCase=0;pairCase<8;pairCase++) {
            mHCutBinHists[pairCase] = new TH1D*[11];
            for (int it=0;it<11;it++) {
                hname  = "Mass";
                hname += bases[it];
                hname += types[pairCase];
                mHCutBinHists[pairCase][it] = new TH1D(hname.Data(),hname.Data(),500,0.0,3.0);
//                cout << "Creating histogram for " << hname.Data() << endl;
            }
        }
    }
}
void StEStructCutBin::writeCutBinHists5() {
    if (mcutBinHistMode) {
        for (int pairCase=0;pairCase<8;pairCase++) {
            if (mHCutBinHists[pairCase]) {
                for (int it=0;it<11;it++) {
                    if (mHCutBinHists[pairCase][it]) {
                        mHCutBinHists[pairCase][it]->Write();
//                        cout << "Deleting histogram [" << pairCase << "][" << it << "]" << endl;
                        delete mHCutBinHists[pairCase][it];
                        mHCutBinHists[pairCase][it] = 0;
                    }
                }
//                cout << "Deleting array [" << pairCase << "]" << endl;
                delete [] mHCutBinHists[pairCase];
                mHCutBinHists[pairCase] = 0;
            }
        }
    }
}


//------------------------ Mode=6 -------------------------------------------
//  Event-wise z-vertex binning
//  This mode breaks the model of everything else, so it is a hack.
//  pc object doesn't have event level info, so cutbin number is set in
//    2ptanalysis by looking at mixing event buffer index.

int StEStructCutBin::getCutBinMode6(StEStructPairCuts*, int zbin){
  // This function should never be used, can't access z-vertex position from here...
  return zbin;
}
  
//------------------------ Mode=7 -------------------------------------------     
//  Combines modes 2 and 6:
//  Event-wise z-vertex binning WITH soft/hard SS/AS binning

int StEStructCutBin::getCutBinMode7(StEStructPairCuts* pc, int zbin){

  if (zbin<0 || zbin>9) return 0;

  int retVal;
  int iyt, idp;

  float yt1=pc->Track1()->Yt();
  float yt2=pc->Track2()->Yt();

  iyt = 2;
  if (yt1+yt2 <= 3.3) iyt = 0;
  if (yt1>=2 && yt2>=2) iyt = 1;

  if (pc->sameSide()) idp = 0;
  else idp = 1;

  retVal = zbin*6 + iyt + 3*idp;

  return retVal;

}
  
//------------------------ Mode=8 -------------------------------------------
//
// --> Having problem with soft-hard pair cuts.
//     When tracks have quite different yt then LS has split peak at 0,0

// ytyt plot deta,dphi
// 0   = soft
// 1   = 'neck'
// 2   = hard
// 3   = soft-neck
// 4   = soft-hard
// 5   = neck-hard


int StEStructCutBin::getCutBinMode8(StEStructPairCuts* pc){

  int iyt;
  
  float yt1=pc->Track1()->Yt();
  float yt2=pc->Track2()->Yt();

  // These numbers are also used in StEStructCutBin::getParentBin (change both)
  if(yt1<1.8 && yt2<1.8){
    iyt=0;
//>>>>> Test moving soft-hard pairs. Am having trouble with pair cuts for LS in this region
  } else if(yt1<1.8 && yt2<2.2){
    iyt=3;
  } else if(yt1<2.2 && yt2<1.8){
    iyt=3;
  } else if(yt1<2.2 && yt2<2.2){
    iyt=1;
  } else if(yt1<1.8){
    iyt=4;
  } else if(yt2<1.8){
    iyt=4;
  } else if(yt1<2.2){
    iyt=5;
  } else if(yt2<2.2){
    iyt=5;
  } else {
    iyt=2;
  }
  return  iyt;
}
int StEStructCutBin::symmetrizeXX8(StEStructPairCuts* pc) {

 /*
  * softNeck, softHard and neckHard are not symmetrized.
  */
    if (getCutBinMode8(pc) < 3) {
        return 1;
    }
    return 0;
}
int StEStructCutBin::switchXX8(StEStructPairCuts* pc) {

 /*
  * For soft-neck and soft-hard want soft first.
  * For neck-hard want neck first.
  */
    if (getCutBinMode8(pc) < 3) {
        return 0;
    }
    if (pc->Track1()->Yt() > pc->Track2()->Yt()) {
        return 1;
    }
    return 0;
}
int StEStructCutBin::notSymmetrizedXX8(int cutBin, int pairCharge) {
    // Used in HAdd.
    // Return true if XX histogram was not symmetrized (i.e. phi,phi).
    if (cutBin < 3) {
        return 0;
    }
    return 1;
}
//------------------------ Mode=9 -------------------------------------------
//
// pt binning only. Create a grid of pt1 vs. pt2 so we can recreate
// 1 < p_t < 3 GeV as well as exploring some pt dependence.

int StEStructCutBin::getCutBinMode9(StEStructPairCuts* pc){

    int mode[6][6] = {{ 0,  6, 11, 15, 18, 20},
                      { 6,  1,  7, 12, 16, 19},
                      {11,  7,  2,  8, 13, 17},
                      {15, 12,  8,  3,  9, 14},
                      {18, 16, 13,  9,  4, 10},
                      {20, 19, 17, 14, 10,  5}};
    int ipt1, ipt2;

    float pt1 = pc->Track1()->Pt();
    float pt2 = pc->Track2()->Pt();

    // These numbers are also used in StEStructCutBin::getParentBin (change both)
    if (pt1 < 0.5) {
        ipt1 = 0;
    } else if (pt1 < 1.0) {
        ipt1 = 1;
    } else if (pt1 < 2.0) {
        ipt1 = 2;
    } else if (pt1 < 3.0) {
        ipt1 = 3;
    } else if (pt1 < 4.0) {
        ipt1 = 4;
    } else {
        ipt1 = 5;
    }
    if (pt2 < 0.5) {
        ipt2 = 0;
    } else if (pt2 < 1.0) {
        ipt2 = 1;
    } else if (pt2 < 2.0) {
        ipt2 = 2;
    } else if (pt2 < 3.0) {
        ipt2 = 3;
    } else if (pt2 < 4.0) {
        ipt2 = 4;
    } else {
        ipt2 = 5;
    }
    return  mode[ipt1][ipt2];
}
int StEStructCutBin::symmetrizeXX9(StEStructPairCuts* pc) {
    return 1;
}
int StEStructCutBin::switchXX9(StEStructPairCuts* pc) {
    return 0;
}
int StEStructCutBin::notSymmetrizedXX9(int cutBin, int pairCharge) {
    return 0;
}

//------------------------ Mode=10 -------------------------------------------
//  Full 2D yt x yt dependence
//  Part 1
//
//  An explanation of the binning is in order.  See the following map in yt x yt space:
//
//  8  16 23 29 34 38 41 43 44
//  7  15 22 28 33 37 40 42 43
//  6  14 21 27 32 36 39 40 41
//  5  13 20 26 31 35 36 37 38
//  4  12 19 25 30 31 32 33 34
//  3  11 18 24 25 26 27 28 29
//  2  10 17 18 19 20 21 22 23
//  1  9  10 11 12 13 14 15 16
//  0  1  2  3  4  5  6  7  8
//

int StEStructCutBin::getCutBinMode10(StEStructPairCuts* pc){
  float yt1=pc->Track1()->Yt();
  float yt2=pc->Track2()->Yt();
  int retVal;

  int binMap[9][9] = { 0, 1, 2, 3, 4, 5, 6, 7, 8,
                       1, 9, 10, 11, 12, 13, 14, 15, 16,
                       2, 10, 17, 18, 19, 20, 21, 22, 23,
                       3, 11, 18, 24, 25, 26, 27, 28, 29,
                       4, 12, 19, 25, 30, 31, 32, 33, 34,
                       5, 13, 20, 26, 31, 35, 36, 37, 38,
                       6, 14, 21, 27, 32, 36, 39, 40, 41,
                       7, 15, 22, 28, 33, 37, 40, 42, 43,
                       8, 16, 23, 29, 34, 38, 41, 43, 44};

  int i1 = ((int) ((yt1-1.)/.4));
  int i2 = ((int) ((yt2-1.)/.4));

  if(i1>8) i1=8;
  if(i2>8) i2=8;

  retVal = binMap[i1][i2];

  return retVal;
}



/***********************************************************************
 *
 * $Log: StEStructCutBin.cxx,v $
 * Revision 1.17  2012/11/16 21:22:27  prindle
 * 2ptCorrelations: SS, AS histograms.  Get eta limits from cuts. Fit PtAll histogram. Add histograms to keep track of eta, phi limits. A few more histograms
 * Binning: Add quality cut.
 * CutBin: modify mode9
 * PairCuts: modify goodDeltaZ for case of one track leaving via endcap.
 *
 * Revision 1.16  2012/10/30 00:16:50  dkettler
 * Cut bins for marginal pt bins added
 *
 * Revision 1.15  2011/08/02 20:34:02  prindle
 *   More detailed histograms for event mixing.
 *   Buffer: increased mixed events to 4 (from 2)
 *   CutBin: added mode 9 for exploration of p_t space, fixed place in mode 5 where
 *           histogram was written before checking it existed.
 *   OneBuffer: added ZDC coincidence rate to event sorting space.
 *
 * Revision 1.14  2010/09/02 21:24:07  prindle
 *   2ptCorrelations: Fill histograms for event mixing information
 *                    Option for common mixing buffer
 *                    Switch to selectively fill QInv histograms (which take a long time)
 *   CutBin: Moved PID code to Track class from Pair class. Needed to update this code.
 *   PairCuts: Moved PID code from here to Track class.
 *             Removed unnecessary creation of StThreeVector which seem to take a long time
 *             Add ToF momentum cuts, modify dEdx momentum cuts. (Now allow dEdx to be
 *             be identified up to 15GeV/c, ToF up to 10GeV/c.)
 *
 * Revision 1.13  2009/11/09 21:32:41  prindle
 * Fix warnings about casting char * to a const char * by redeclaring as const char *.
 *
 * Revision 1.12  2008/12/02 23:45:06  prindle
 * Changed switchYt to switchXX (etc.) to better reflect function.
 * Change minYt to 1.0 in Binning so YtYt histogram doesn't have empty lower bin (pt = 0.164 for yt = 1.0)
 * In CutBin: remove initPtBin
 *            add mode 8
 *            add notSymmetrized (used in Support)
 * Added LUT (Look Up Table) for pair cuts. Experimental for now.
 * Modified cutMerging2 (to look at track separation at a few radii)
 * and cutCrossing2 so it doesn't accidentally reject almost back to back tracks.
 *
 * Revision 1.11  2008/03/19 22:06:01  prindle
 * Added doInvariantMass flag.
 * Added some plots in pairDensityHistograms.
 * SetZOffset used to only be done when doPairDensity was true.
 * Moved creating/copying pairDensity histograms to same place as other histograms.
 * Added cutBinHistMode
 * mode3 neck was defined as yt1<2.2 && yt2<2.2 (and not soft)
 *            now is        1.8<yt1<2.2  && 1.8<yt2<2.2
 * Added gooddzdxy, Merging2 and Crossing2 to pair cuts.
 *
 * Revision 1.10  2007/11/26 19:55:24  prindle
 * In 2ptCorrelations: Support for keeping all z-bins of selected centralities
 *                     Change way \hat{p_t} is calculated for parent distributions in pid case.
 *    Binning          Added parent binning (for \hat{p_t}
 *    CutBin:          Mode 5 extensively modified.
 *                     Added invariant mass cuts (probably a bad idea in general.)
 *
 * Revision 1.9  2007/05/27 22:45:02  msd
 * Added new cut bin modes 2 (soft/hard SS/AS), 6 (z-vertex binning), and 7 (modes 2*6).
 * Fixed bug in merging cut.
 * Added a few histograms to 2pt corr.
 *
 * Revision 1.8  2007/01/26 17:17:09  msd
 * Implemented new binning scheme: dEta stored in array with bin centered at zero, dPhi array has bins centered at zero and pi.  Final DEtaDPhi has 25x25 bins with dPhi bin width of pi/12 so all major angles are centered in bins.
 *
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
