/**********************************************************************
 *
 * $Id: StEStructPairCuts.cxx,v 1.15 2012/11/16 21:22:27 prindle Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Cut class for track-pair level quantities
 *
 *
 ***********************************************************************/
#include "StEStructPairCuts.h"
#include <math.h>
#include <stdlib.h>

ClassImp(StEStructPairCuts)

StEStructPairCuts::StEStructPairCuts(): StEStructCuts(){ init(); };
StEStructPairCuts::StEStructPairCuts(const char* cutfileName): StEStructCuts(cutfileName) { init(); };

StEStructPairCuts::~StEStructPairCuts() {
    delete mLUT;
};

void StEStructPairCuts::init(){ 

  strcpy(mcutTypeName,"Pair");
  initCuts();
  initNames();
  mLUT = new StEStructPairLUT();
  if(isLoaded())loadCuts();

}

void StEStructPairCuts::initCuts(){

    mdphi[0]=mdphi[1]=0;
    mdeta[0]=mdeta[1]=0;
    mgooddzdxy[0]=mgooddzdxy[1]=0;
    mdmt[0]=mdmt[1]=0;
    mqInv[0]=mqInv[1]=0;
    mEntSep[0]=mEntSep[1]=0;
    mExitSep[0]=mExitSep[1]=0;
    mQuality[0]=mQuality[1]=0;
    mMidTpcSepLS[0]=mMidTpcSepLS[1]=0;    
    mMidTpcSepUS[0]=mMidTpcSepUS[1]=0;
    mHBT[0]=mHBT[1]=mHBT[2]=mHBT[3]=0;
    mCoulomb[0]=mCoulomb[1]=mCoulomb[2]=mCoulomb[3]=0;
    mMerging[0]=mMerging[1]=0;
    mMerging2[0]=mMerging2[1]=0;
    mCrossing[0]=mCrossing[1]=0;
    mCrossing2[0]=mCrossing2[1]=0;
    mLUTParams[0]=mLUTParams[1]=0;

    // Would be nice to have some way to change these without recompiling.
    // Normally upper pt cuts are 999, 1.0, 1.0, 1.5 for all, pi, K, p
    // Want to run Pythia without upper cut (I think we actually use 20 in cuts file.)
    // I don't know right off what reasonable upper limits are on pid.
    // dEdx can be done in the relativistic rise region, but probably Kaons can't be distinguished.
    // With a requirement track is 2\sigma from any other particle we are probably ok without upper limits on data.
    // For comparison with Pythia, Hijing and Therminator we need to think about it.
    mdEdxMomentumCut[0][0] =   0.0;
    mdEdxMomentumCut[0][1] = 999.0;
    mdEdxMomentumCut[1][0] =   0.1;  // pi
    mdEdxMomentumCut[1][1] =  15.0;
    mdEdxMomentumCut[2][0] =   0.1;  // K
    mdEdxMomentumCut[2][1] =  15.0;
    mdEdxMomentumCut[3][0] =   0.2;  // p
    mdEdxMomentumCut[3][1] =  15.0;

    mToFMomentumCut[0][0] =   0.0;
    mToFMomentumCut[0][1] = 999.0;
    mToFMomentumCut[1][0] =   0.1;  // pi
    mToFMomentumCut[1][1] =  10.0;
    mToFMomentumCut[2][0] =   0.1;  // K
    mToFMomentumCut[2][1] =  10.0;
    mToFMomentumCut[3][0] =   0.2;  // p
    mToFMomentumCut[3][1] =  10.0;

    mdeltaPhiCut=mdeltaEtaCut=mGooddeltaZdeltaXYCut=mdeltaMtCut=mqInvCut=mEntSepCut=mExitSepCut=mQualityCut=mMidTpcSepLSCut=mMidTpcSepUSCut=false;
    mHBTCut=mCoulombCut=mMergingCut=mCrossingCut=mMergingCut2=mCrossingCut2=mLUTCut = false;
    mpionMomentumCut=mKaonMomentumCut=mprotonMomentumCut = false;
    mpionOtherMassCut=mpionpionMassCut=mpionKaonMassCut=mpionprotonMassCut = false;
    mKaonOtherMassCut=mKaonKaonMassCut=mKaonprotonMassCut=mprotonOtherMassCut = false;
    mprotonprotonMassCut=mOtherOtherMassCut = false;


    for(int i=0;i<4;i++) {
        mdphiCounter[i]=mdetaCounter[i]=mgooddzdxyCounter[i]=mdmtCounter[i]=mqInvCounter[i]=mEntSepCounter[i]=0;
        mExitSepCounter[i]=mQualityCounter[i]=msplitLSCounter[i]=msplitUSCounter[i]=0;
        mHBTCounter[i]=mCoulombCounter[i]=mMergingCounter[i]=mCrossingCounter[i]=mMergingCounter2[i]=mCrossingCounter2[i]=mLUTCounter[i]=0;
        mpionMomentumCounter[i]=mKaonMomentumCounter[i]=mprotonMomentumCounter[i]=0;
        mpionOtherMassCounter[i]=mpionpionMassCounter[i]=mpionKaonMassCounter[i]=mpionprotonMassCounter[i]=0;
        mKaonOtherMassCounter[i]=mKaonKaonMassCounter[i]=mKaonprotonMassCounter[i]=mprotonOtherMassCounter[i]=0;
        mprotonprotonMassCounter[i]=mOtherOtherMassCounter[i]=0;
    }
    mdeltaPhi=mdeltaEta=mdeltaMt=mqInvariant= mEntranceSeparation=mExitSeparation=mQualityVal=mMidTpcSeparationLS=mMidTpcSeparationUS=0;

    mapMask0 = 0xFFFFFF00;
    mapMask1 = 0x1FFFFF;
    for(int i=0;i<32;i++)bitI[i]=1UL<<i;

    mZoffset = 0;

};

void StEStructPairCuts::initNames(){

  strcpy(mdphiName.name,"DeltaPhi");
  strcpy(mdetaName.name,"DeltaEta");
  strcpy(mgooddzdxyName.name,"GoodDeltaZDeltaXY");
  strcpy(mdmtName.name,"DeltaMt");
  strcpy(mqInvName.name,"qInv");
  strcpy(mEntSepName.name,"EntranceSep");
  strcpy(mExitSepName.name,"ExitSep");
  strcpy(mQualityName.name,"Quality");
  strcpy(mMidTpcSepLSName.name,"MidTpcSepLikeSign");
  strcpy(mMidTpcSepUSName.name,"MidTpcSepUnlikeSign");
  strcpy(mHBTName.name,"HBT");
  strcpy(mCoulombName.name,"Coulomb");
  strcpy(mMergingName.name,"Merging");
  strcpy(mMergingName2.name,"Merging2");
  strcpy(mCrossingName.name,"Crossing");
  strcpy(mCrossingName2.name,"Crossing2");
  strcpy(mLUTName.name,"LUT");
  strcpy(mpionMomentumName.name,"pionMomentumRange");
  strcpy(mKaonMomentumName.name,"KaonMomentumRange");
  strcpy(mprotonMomentumName.name,"protonMomentumRange");

  strcpy(mpionOtherMassName.name,"pionOtherMass");
  strcpy(mpionpionMassName.name,"pionpionMass");
  strcpy(mpionKaonMassName.name,"pionKaonMass");
  strcpy(mpionprotonMassName.name,"pionprotonMass");
  strcpy(mKaonOtherMassName.name,"KaonOtherMass");
  strcpy(mKaonKaonMassName.name,"KaonKaonMass");
  strcpy(mKaonprotonMassName.name,"KaonprotonMass");
  strcpy(mprotonOtherMassName.name,"protonOtherMass");
  strcpy(mprotonprotonMassName.name,"protonprotonMass");
  strcpy(mOtherOtherMassName.name,"OtherOtherMass");
  
};


bool StEStructPairCuts::loadBaseCuts(const char* name, const char** vals, int nvals){

  //  cout<<" Cut Name="<<name<<endl;
  if(!strcmp(name,mdphiName.name)){
    mdphi[0]=(float)(M_PI*atof(vals[0])); mdphi[1]=(float)(M_PI*atof(vals[1]));
    mdphiName.idx=createCutHists(name,mdphi);
    mdeltaPhiCut=true;
    return true;
  }

  if(!strcmp(name,mdetaName.name)){
    mdeta[0]=atof(vals[0]); mdeta[1]=atof(vals[1]);
    mdetaName.idx=createCutHists(name,mdeta);
    mdeltaEtaCut=true;
    return true;
  }

  if(!strcmp(name,mgooddzdxyName.name)){
    mgooddzdxy[0]=atof(vals[0]); mgooddzdxy[1]=atof(vals[1]);
    mgooddzdxyName.idx=createCutHists(name,mgooddzdxy);
    mGooddeltaZdeltaXYCut=true;
    return true;
  }

  if(!strcmp(name,mdmtName.name)){
    mdmt[0]=atof(vals[0]); mdmt[1]=atof(vals[1]);
    mdmtName.idx=createCutHists(name,mdmt);
    mdeltaMtCut=true;
    return true;
  }

  if(!strcmp(name,mqInvName.name)){
    mqInv[0]=atof(vals[0]); mqInv[1]=atof(vals[1]);
    mqInvName.idx=createCutHists(name,mqInv);
    mqInvCut=true;
    return true;
  }

  if(!strcmp(name,mEntSepName.name)){
    mEntSep[0]=atof(vals[0]); mEntSep[1]=atof(vals[1]);
    mEntSepName.idx=createCutHists(name,mEntSep);
    mEntSepCut=true;
    return true;
  }

  if(!strcmp(name,mExitSepName.name)){
    mExitSep[0]=atof(vals[0]); mExitSep[1]=atof(vals[1]);
    mExitSepName.idx=createCutHists(name,mExitSep);
    mExitSepCut=true;
    return true;
  }

  if(!strcmp(name,mQualityName.name)){
    mQuality[0]=atof(vals[0]); mQuality[1]=atof(vals[1]);
    mQualityName.idx=createCutHists(name,mQuality);
    mQualityCut=true;
    return true;
  }

  if(!strcmp(name,mMidTpcSepLSName.name)){
    mMidTpcSepLS[0]=atof(vals[0]); mMidTpcSepLS[1]=atof(vals[1]);
    mMidTpcSepLSName.idx=createCutHists(name,mMidTpcSepLS);
    mMidTpcSepLSCut=true;
    return true;
  }

  if(!strcmp(name,mMidTpcSepUSName.name)){
    mMidTpcSepUS[0]=atof(vals[0]); mMidTpcSepUS[1]=atof(vals[1]);
    mMidTpcSepUSName.idx=createCutHists(name,mMidTpcSepUS);
    mMidTpcSepUSCut=true;
    return true;
  }

  if(!strcmp(name,mHBTName.name)){
    mHBT[0]=atof(vals[0]); mHBT[1]=atof(vals[1]);
    mHBT[2]=atof(vals[2]); mHBT[3]=atof(vals[3]);
    //mHBTName.idx=createCutHists(name,mHBT,4);  // not making cut histograms
    mHBTCut=true;
    cout << " Loading HBT cut with range of cuts = "<<mHBT[0]<<","<<mHBT[1]<<","<<mHBT[2]<<","<<mHBT[3]<<endl;
    return true;
  }

  if(!strcmp(name,mCoulombName.name)){
    mCoulomb[0]=atof(vals[0]); mCoulomb[1]=atof(vals[1]);
    mCoulomb[2]=atof(vals[2]); mCoulomb[3]=atof(vals[3]);
    //mCoulombName.idx=createCutHists(name,mCoulomb,4);  // not making cut histograms
    mCoulombCut=true;
    cout << " Loading Coulomb cut with range of cuts = "<<mCoulomb[0]<<","<<mCoulomb[1]<<","<<mCoulomb[2]<<","<<mCoulomb[3]<<endl;
    return true;
  }

  if(!strcmp(name,mMergingName.name)){
    mMerging[0]=atof(vals[0]); mMerging[1]=atof(vals[1]);
    //mMergingName.idx=createCutHists(name,mMerging,2);  // not making cut histograms
    mMergingCut=true;
    cout << " Loading Merging cut with range of cuts = "<<mMerging[0]<<","<<mMerging[1]<<endl;
    return true;
  }

  if(!strcmp(name,mMergingName2.name)){
    mMerging2[0]=atof(vals[0]); mMerging2[1]=atof(vals[1]);
    //mMergingName2.idx=createCutHists(name,mMerging2,2);  // not making cut histograms
    mMergingCut2=true;
    cout << " Loading Merging2 cut with range of cuts = "<<mMerging2[0]<<","<<mMerging2[1]<<endl;
    return true;
  }

  if(!strcmp(name,mCrossingName.name)){
    mCrossing[0]=atof(vals[0]); mCrossing[1]=atof(vals[1]);
    //mCrossingName.idx=createCutHists(name,mCrossing,2);  // not making cut histograms
    mCrossingCut=true;
    cout << " Loading Crossing cut with range of cuts = "<<mCrossing[0]<<","<<mCrossing[1]<<endl;
    return true;
  }

  if(!strcmp(name,mCrossingName2.name)){
    mCrossing2[0]=atof(vals[0]); mCrossing2[1]=atof(vals[1]);
    //mCrossingName2.idx=createCutHists(name,mCrossing2,2);  // not making cut histograms
    mCrossingCut2=true;
    cout << " Loading Crossing2 cut with range of cuts = "<<mCrossing2[0]<<","<<mCrossing2[1]<<endl;
    return true;
  }
  if(!strcmp(name,mLUTName.name)){
    mLUTParams[0]=atof(vals[0]); mLUTParams[1]=atof(vals[1]);
    mLUTCut=true;
    cout << " Calculating Look Up Table with dXY cut = "<<mLUTParams[0]<<" and dZ cut = "<<mLUTParams[1]<< " (takes a little while)" <<endl;
    mLUT->mDelXYCut = mLUTParams[0];
    mLUT->mDelZCut  = mLUTParams[1];
    mLUT->initHists();
    mLUT->fillLUTs();
    cout << " LUT has been calculated " <<endl;
    return true;
  }

  if(!strcmp(name,mpionMomentumName.name)){
    mdEdxMomentumCut[1][0]=atof(vals[0]); mdEdxMomentumCut[1][1]=atof(vals[1]);
    mToFMomentumCut[1][0]=atof(vals[0]); mToFMomentumCut[1][1]=atof(vals[1]);
    mpionMomentumCut=true;
    cout << " Loading pion momentum cut with range of values = "<<mdEdxMomentumCut[1][0]<<","<<mdEdxMomentumCut[1][1]<<endl;
    return true;
  }

  if(!strcmp(name,mKaonMomentumName.name)){
    mdEdxMomentumCut[2][0]=atof(vals[0]); mdEdxMomentumCut[2][1]=atof(vals[1]);
    mToFMomentumCut[2][0]=atof(vals[0]); mToFMomentumCut[2][1]=atof(vals[1]);
    mKaonMomentumCut=true;
    cout << " Loading Kaon momentum cut with range of values = "<<mdEdxMomentumCut[2][0]<<","<<mdEdxMomentumCut[2][1]<<endl;
    return true;
  }

  if(!strcmp(name,mprotonMomentumName.name)){
    mdEdxMomentumCut[3][0]=atof(vals[0]); mdEdxMomentumCut[3][1]=atof(vals[1]);
    mToFMomentumCut[3][0]=atof(vals[0]); mToFMomentumCut[3][1]=atof(vals[1]);
    mprotonMomentumCut=true;
    cout << " Loading proton momentum cut with range of values = "<<mdEdxMomentumCut[3][0]<<","<<mdEdxMomentumCut[3][1]<<endl;    return true;
  }

  if(!strcmp(name,mpionOtherMassName.name)){
    mpionOtherMass[0]=atof(vals[0]); mpionOtherMass[1]=atof(vals[1]);
    mpionOtherMassCut=true;
    cout << " Loading pion-Other mass cut with range of values = "<<mpionOtherMass[0]<<","<<mpionOtherMass[1]<<endl;    return true;
  }
  if(!strcmp(name,mpionpionMassName.name)){
    mpionpionMass[0]=atof(vals[0]); mpionpionMass[1]=atof(vals[1]);
    mpionpionMassCut=true;
    cout << " Loading pion-pion mass cut with range of values = "<<mpionpionMass[0]<<","<<mpionpionMass[1]<<endl;    return true;
  }
  if(!strcmp(name,mpionKaonMassName.name)){
    mpionKaonMass[0]=atof(vals[0]); mpionKaonMass[1]=atof(vals[1]);
    mpionKaonMassCut=true;
    cout << " Loading pion-Kaon mass cut with range of values = "<<mpionKaonMass[0]<<","<<mpionKaonMass[1]<<endl;    return true;
  }
  if(!strcmp(name,mpionprotonMassName.name)){
    mpionprotonMass[0]=atof(vals[0]); mpionprotonMass[1]=atof(vals[1]);
    mpionprotonMassCut=true;
    cout << " Loading pion-proton mass cut with range of values = "<<mpionprotonMass[0]<<","<<mpionprotonMass[1]<<endl;    return true;
  }
  if(!strcmp(name,mKaonOtherMassName.name)){
    mKaonOtherMass[0]=atof(vals[0]); mKaonOtherMass[1]=atof(vals[1]);
    mKaonOtherMassCut=true;
    cout << " Loading Kaon-Other mass cut with range of values = "<<mKaonOtherMass[0]<<","<<mKaonOtherMass[1]<<endl;    return true;
  }
  if(!strcmp(name,mKaonKaonMassName.name)){
    mKaonKaonMass[0]=atof(vals[0]); mKaonKaonMass[1]=atof(vals[1]);
    mKaonKaonMassCut=true;
    cout << " Loading Kaon-Kaon mass cut with range of values = "<<mKaonKaonMass[0]<<","<<mKaonKaonMass[1]<<endl;    return true;
  }
  if(!strcmp(name,mKaonprotonMassName.name)){
    mKaonprotonMass[0]=atof(vals[0]); mKaonprotonMass[1]=atof(vals[1]);
    mKaonprotonMassCut=true;
    cout << " Loading Kaon-proton mass cut with range of values = "<<mKaonprotonMass[0]<<","<<mKaonprotonMass[1]<<endl;    return true;
  }
  if(!strcmp(name,mprotonOtherMassName.name)){
    mprotonOtherMass[0]=atof(vals[0]); mprotonOtherMass[1]=atof(vals[1]);
    mprotonOtherMassCut=true;
    cout << " Loading proton-Other mass cut with range of values = "<<mprotonOtherMass[0]<<","<<mprotonOtherMass[1]<<endl;    return true;
  }
  if(!strcmp(name,mprotonprotonMassName.name)){
    mprotonprotonMass[0]=atof(vals[0]); mprotonprotonMass[1]=atof(vals[1]);
    mprotonprotonMassCut=true;
    cout << " Loading proton-proton mass cut with range of values = "<<mprotonprotonMass[0]<<","<<mprotonprotonMass[1]<<endl;    return true;
  }
  if(!strcmp(name,mOtherOtherMassName.name)){
    mOtherOtherMass[0]=atof(vals[0]); mOtherOtherMass[1]=atof(vals[1]);
    mOtherOtherMassCut=true;
    cout << " Loading Other-Other mass cut with range of values = "<<mOtherOtherMass[0]<<","<<mOtherOtherMass[1]<<endl;    return true;
  }

  //  cout<<" didn't find any cut with this name "<<endl;
  return false;
};

void StEStructPairCuts::printCutCounts(ostream& ofs, const char* cutName,int c1,int c2){
  ofs<<cutName<<c1<<" + "<<c2<<"  =  "<<c1+c2<<endl;
}
  

void StEStructPairCuts::printCutStats(ostream& ofs){

  //  ofs<<"# ******************************************** "<<endl;
  //  ofs<<"# *************** Pair Cuts ****************** "<<endl;
  //  ofs<<"# *** format = Cut, minvalue, maxvalue     *** "<<endl;
  // ofs<<"# ***      Sib LS + US = Total             *** "<<endl;
  // ofs<<"# ***      Mix LS + US = Total             *** "<<endl;
  //  ofs<<"# ******************************************** "<<endl;
  ofs<<endl;
  const char* cutTypes[]={"#---  Sibling Pairs : LS + US = ",
                          "#---  Mixed   Pairs : LS + US = "};
  if(mdeltaPhiCut){
    ofs<<mdphiName.name<<","<<mdphi[0]/M_PI<<","<<mdphi[1]/M_PI<<"\t\t\t"<<" # pair dphi cut"<<endl;
    printCutCounts(ofs,cutTypes[0],mdphiCounter[0],mdphiCounter[1]);
    printCutCounts(ofs,cutTypes[1],mdphiCounter[2],mdphiCounter[3]);
  }
  if(mdeltaEtaCut){
     ofs<<mdetaName.name<<","<<mdeta[0]<<","<<mdeta[1]<<"\t\t\t"<<" # pair deta cut"<<endl;
    printCutCounts(ofs,cutTypes[0],mdetaCounter[0],mdetaCounter[1]);
    printCutCounts(ofs,cutTypes[1],mdetaCounter[2],mdetaCounter[3]);
  }
  if(mGooddeltaZdeltaXYCut){
     ofs<<mgooddzdxyName.name<<","<<mgooddzdxy[0]<<","<<mgooddzdxy[1]<<"\t\t\t"<<" # pair deta cut"<<endl;
    printCutCounts(ofs,cutTypes[0],mgooddzdxyCounter[0],mgooddzdxyCounter[1]);
    printCutCounts(ofs,cutTypes[1],mgooddzdxyCounter[2],mgooddzdxyCounter[3]);
  }

  if(mdeltaMtCut){
     ofs<<mdmtName.name<<","<<mdmt[0]<<","<<mdmt[1]<<"\t\t\t"<<" # pair dmt cut"<<endl;
    printCutCounts(ofs,cutTypes[0],mdmtCounter[0],mdmtCounter[1]);
    printCutCounts(ofs,cutTypes[1],mdmtCounter[2],mdmtCounter[3]);
  }

  if(mqInvCut){
     ofs<<mqInvName.name<<","<<mqInv[0]<<","<<mqInv[1]<<"\t\t\t"<<" # pair qInv cut"<<endl;
    printCutCounts(ofs,cutTypes[0],mqInvCounter[0],mqInvCounter[1]);
    printCutCounts(ofs,cutTypes[1],mqInvCounter[2],mqInvCounter[3]);
  }

  if(mEntSepCut){
    ofs<<mEntSepName.name<<","<<mEntSep[0]<<","<<mEntSep[1]<<"\t\t\t"<<" # pair EntSep cut"<<endl;
    printCutCounts(ofs,cutTypes[0],mEntSepCounter[0],mEntSepCounter[1]);
    printCutCounts(ofs,cutTypes[1],mEntSepCounter[2],mEntSepCounter[3]);
  }

  if(mExitSepCut){
    ofs<<mExitSepName.name<<","<<mExitSep[0]<<","<<mExitSep[1]<<"\t\t\t"<<" # pair ExitSep cut"<<endl;
    printCutCounts(ofs,cutTypes[0],mExitSepCounter[0],mExitSepCounter[1]);
    printCutCounts(ofs,cutTypes[1],mExitSepCounter[2],mExitSepCounter[3]);
  }

  if(mQualityCut){
     ofs<<mQualityName.name<<","<<mQuality[0]<<","<<mQuality[1]<<"\t\t\t"<<" # pair Quality cut"<<endl;
    printCutCounts(ofs,cutTypes[0],mQualityCounter[0],mQualityCounter[1]);
    printCutCounts(ofs,cutTypes[1],mQualityCounter[2],mQualityCounter[3]);
  }

  if(mMidTpcSepLSCut){
     ofs<<mMidTpcSepLSName.name<<","<<mMidTpcSepLS[0]<<","<<mMidTpcSepLS[1]<<"\t\t\t"<<" # pair MidTpcSepLS cut"<<endl;
    printCutCounts(ofs,cutTypes[0],msplitLSCounter[0],msplitLSCounter[1]);
    printCutCounts(ofs,cutTypes[1],msplitLSCounter[2],msplitLSCounter[3]);
  }

  if(mMidTpcSepUSCut){
     ofs<<mMidTpcSepUSName.name<<","<<mMidTpcSepUS[0]<<","<<mMidTpcSepUS[1]<<"\t\t\t"<<" # pair MidTpcSepUS cut"<<endl;
    printCutCounts(ofs,cutTypes[0],msplitUSCounter[0],msplitUSCounter[1]);
    printCutCounts(ofs,cutTypes[1],msplitUSCounter[2],msplitUSCounter[3]);
  }

  if(mHBTCut){
    ofs<<mHBTName.name<<","<<mHBT[0]<<","<<mHBT[1]<<","<<mHBT[2]<<","<<mHBT[3]<<"\t\t"<<" # pair HBT cut"<<endl;
    printCutCounts(ofs,cutTypes[0],mHBTCounter[0],mHBTCounter[1]);
    printCutCounts(ofs,cutTypes[1],mHBTCounter[2],mHBTCounter[3]);
  }
  if(mCoulombCut){
    ofs<<mCoulombName.name<<","<<mCoulomb[0]<<","<<mCoulomb[1]<<","<<mCoulomb[2]<<","<<mCoulomb[3]<<"\t"<<" # pair Coulomb cut"<<endl;
    printCutCounts(ofs,cutTypes[0],mCoulombCounter[0],mCoulombCounter[1]);
    printCutCounts(ofs,cutTypes[1],mCoulombCounter[2],mCoulombCounter[3]);
  }
  if(mMergingCut){
    ofs<<mMergingName.name<<","<<mMerging[0]<<","<<mMerging[1]<<"\t\t\t"<<" # pair Merging cut"<<endl;
    printCutCounts(ofs,cutTypes[0],mMergingCounter[0],mMergingCounter[1]);
    printCutCounts(ofs,cutTypes[1],mMergingCounter[2],mMergingCounter[3]);
  }
  if(mMergingCut2){
    ofs<<mMergingName2.name<<","<<mMerging2[0]<<","<<mMerging2[1]<<"\t\t\t"<<" # pair Merging2 cut"<<endl;
    printCutCounts(ofs,cutTypes[0],mMergingCounter2[0],mMergingCounter2[1]);
    printCutCounts(ofs,cutTypes[1],mMergingCounter2[2],mMergingCounter2[3]);
  }
  if(mCrossingCut){
    ofs<<mCrossingName.name<<","<<mCrossing[0]<<","<<mCrossing[1]<<"\t\t"<<" # pair Crossing cut"<<endl;
    printCutCounts(ofs,cutTypes[0],mCrossingCounter[0],mCrossingCounter[1]);
    printCutCounts(ofs,cutTypes[1],mCrossingCounter[2],mCrossingCounter[3]);
  }
  if(mCrossingCut2){
    ofs<<mCrossingName2.name<<","<<mCrossing2[0]<<","<<mCrossing2[1]<<"\t\t"<<" # pair Crossing2 cut"<<endl;
    printCutCounts(ofs,cutTypes[0],mCrossingCounter2[0],mCrossingCounter2[1]);
    printCutCounts(ofs,cutTypes[1],mCrossingCounter2[2],mCrossingCounter2[3]);
  }
  if(mLUTCut){
    ofs<<mLUTName.name<<","<<mLUTParams[0]<<","<<mLUTParams[1]<<"\t\t"<<" # pair LUT cut"<<endl;
    printCutCounts(ofs,cutTypes[0],mLUTCounter[0],mLUTCounter[1]);
    printCutCounts(ofs,cutTypes[1],mLUTCounter[2],mLUTCounter[3]);
  }
  if(mpionMomentumCut){
    ofs<<mpionMomentumName.name<<","<<mdEdxMomentumCut[1][0]<<","<<mdEdxMomentumCut[1][1]<<"\t\t"<<" # pion momentum range cut"<<endl;
    printCutCounts(ofs,cutTypes[0],mpionMomentumCounter[0],mpionMomentumCounter[1]);
    printCutCounts(ofs,cutTypes[1],mpionMomentumCounter[2],mpionMomentumCounter[3]);
  }
  if(mKaonMomentumCut){
    ofs<<mKaonMomentumName.name<<","<<mdEdxMomentumCut[2][0]<<","<<mdEdxMomentumCut[2][1]<<"\t\t"<<" # Kaon momentum range cut"<<endl;
    printCutCounts(ofs,cutTypes[0],mKaonMomentumCounter[0],mKaonMomentumCounter[1]);
    printCutCounts(ofs,cutTypes[1],mKaonMomentumCounter[2],mKaonMomentumCounter[3]);
  }
  if(mprotonMomentumCut){
    ofs<<mprotonMomentumName.name<<","<<mdEdxMomentumCut[3][0]<<","<<mdEdxMomentumCut[3][1]<<"\t\t"<<" # proton momentum range cut"<<endl;
    printCutCounts(ofs,cutTypes[0],mprotonMomentumCounter[0],mprotonMomentumCounter[1]);
    printCutCounts(ofs,cutTypes[1],mprotonMomentumCounter[2],mprotonMomentumCounter[3]);
  }

  if(mpionOtherMassCut){
    ofs<<mpionOtherMassName.name<<","<<mpionOtherMass[0]<<","<<mpionOtherMass[1]<<"\t\t"<<" # pion-Other mass cut range cut"<<endl;
    printCutCounts(ofs,cutTypes[0],mpionOtherMassCounter[0],mpionOtherMassCounter[1]);
    printCutCounts(ofs,cutTypes[1],mpionOtherMassCounter[2],mpionOtherMassCounter[3]);
  }
  if(mpionpionMassCut){
    ofs<<mpionpionMassName.name<<","<<mpionpionMass[0]<<","<<mpionpionMass[1]<<"\t\t"<<" # pion-pion mass cut range cut"<<endl;
    printCutCounts(ofs,cutTypes[0],mpionpionMassCounter[0],mpionpionMassCounter[1]);
    printCutCounts(ofs,cutTypes[1],mpionpionMassCounter[2],mpionpionMassCounter[3]);
  }
  if(mpionKaonMassCut){
    ofs<<mpionKaonMassName.name<<","<<mpionKaonMass[0]<<","<<mpionKaonMass[1]<<"\t\t"<<" # pion-Kaon mass cut range cut"<<endl;
    printCutCounts(ofs,cutTypes[0],mpionKaonMassCounter[0],mpionKaonMassCounter[1]);
    printCutCounts(ofs,cutTypes[1],mpionKaonMassCounter[2],mpionKaonMassCounter[3]);
  }
  if(mpionprotonMassCut){
    ofs<<mpionprotonMassName.name<<","<<mpionprotonMass[0]<<","<<mpionprotonMass[1]<<"\t\t"<<" # pion-proton mass cut range cut"<<endl;
    printCutCounts(ofs,cutTypes[0],mpionprotonMassCounter[0],mpionprotonMassCounter[1]);
    printCutCounts(ofs,cutTypes[1],mpionprotonMassCounter[2],mpionprotonMassCounter[3]);
  }
  if(mKaonOtherMassCut){
    ofs<<mKaonOtherMassName.name<<","<<mKaonOtherMass[0]<<","<<mKaonOtherMass[1]<<"\t\t"<<" # pion-Other mass cut range cut"<<endl;
    printCutCounts(ofs,cutTypes[0],mKaonOtherMassCounter[0],mKaonOtherMassCounter[1]);
    printCutCounts(ofs,cutTypes[1],mKaonOtherMassCounter[2],mKaonOtherMassCounter[3]);
  }
  if(mKaonKaonMassCut){
    ofs<<mKaonKaonMassName.name<<","<<mKaonKaonMass[0]<<","<<mKaonKaonMass[1]<<"\t\t"<<" # Kaon-Kaon mass cut range cut"<<endl;
    printCutCounts(ofs,cutTypes[0],mKaonKaonMassCounter[0],mKaonKaonMassCounter[1]);
    printCutCounts(ofs,cutTypes[1],mKaonKaonMassCounter[2],mKaonKaonMassCounter[3]);
  }
  if(mKaonprotonMassCut){
    ofs<<mKaonprotonMassName.name<<","<<mKaonprotonMass[0]<<","<<mKaonprotonMass[1]<<"\t\t"<<" # Kaon-proton mass cut range cut"<<endl;
    printCutCounts(ofs,cutTypes[0],mKaonprotonMassCounter[0],mKaonprotonMassCounter[1]);
    printCutCounts(ofs,cutTypes[1],mKaonprotonMassCounter[2],mKaonprotonMassCounter[3]);
  }
  if(mprotonOtherMassCut){
    ofs<<mprotonOtherMassName.name<<","<<mprotonOtherMass[0]<<","<<mprotonOtherMass[1]<<"\t\t"<<" # proton-Other mass cut range cut"<<endl;
    printCutCounts(ofs,cutTypes[0],mprotonOtherMassCounter[0],mprotonOtherMassCounter[1]);
    printCutCounts(ofs,cutTypes[1],mprotonOtherMassCounter[2],mprotonOtherMassCounter[3]);
  }
  if(mprotonprotonMassCut){
    ofs<<mprotonprotonMassName.name<<","<<mprotonprotonMass[0]<<","<<mprotonprotonMass[1]<<"\t\t"<<" # proton-proton mass cut range cut"<<endl;
    printCutCounts(ofs,cutTypes[0],mprotonprotonMassCounter[0],mprotonprotonMassCounter[1]);
    printCutCounts(ofs,cutTypes[1],mprotonprotonMassCounter[2],mprotonprotonMassCounter[3]);
  }
  if(mOtherOtherMassCut){
    ofs<<mOtherOtherMassName.name<<","<<mOtherOtherMass[0]<<","<<mOtherOtherMass[1]<<"\t\t"<<" # Other-Other mass cut range cut"<<endl;
    printCutCounts(ofs,cutTypes[0],mOtherOtherMassCounter[0],mOtherOtherMassCounter[1]);
    printCutCounts(ofs,cutTypes[1],mOtherOtherMassCounter[2],mOtherOtherMassCounter[3]);
  }



  //  ofs<<"# ******************************************** "<<endl<<endl;

}

//------------------------------------------------------------
int StEStructPairCuts::cutPair(){
  // return 0 to use pair, return 1 to cut pair 

  //  if(cutDeltaPhi() || cutDeltaEta() || cutDeltaMt()) return 1;
  //if(goodDeltaPhi() && goodDeltaEta() && goodDeltaMt()) return 0;
  
  // *** new test ***
  /*  cout.precision(3);     
  cout <<MidTpcXYSeparation()<<"\t"<<MidTpcZSeparation()<<"\t"<<MidTpcSeparation();    
  cout << "::\t"<<NominalTpcXYExitSeparation()<<"\t"<<NominalTpcZExitSeparation()<<"\t"<<NominalTpcExitSeparation()<<endl;
  //cout << endl;
  if (cutCoulomb())  cout << "COUL\t"<<DeltaEta()<<"\t"<<DeltaPhi()<<"\t"<<DeltaPt()<<"\t"<<mTrack1->Pt()<<"\t"<<mTrack2->Pt()<<endl;
  if (cutHBT())      cout << "HBT\t"<<mType<<endl;
  if (cutMerging())  cout << "MERG\t"<<NominalTpcAvgXYSeparation()<<"\t"<<NominalTpcAvgZSeparation()<<endl;
  if (cutCrossing())  {
    cout << "CROS\t";
    if (mTrack1->Charge()>0) cout << "+ ";
    else cout << "- ";
    if (mTrack2->Charge()>0) cout << "+\t";
    else cout << "-\t";
    cout <<MidTpcXYSeparation()<<"\t"<<MidTpcZSeparation();
    float dphi = mTrack1->Phi()-mTrack2->Phi(); // signed DeltaPhi
    float dpt =  mTrack1->Pt()- mTrack2->Pt();  // signed DeltaPt
    cout << dphi << "\t" << dpt << endl;
    }*/
  // *** 

    if (goodDeltaZ() || goodDeltaXY()) {
        return 0;
    }
    if(cutMerging() || cutCrossing()) {
        return 1;
    }
    if(cutMerging2() || cutCrossing2()) {
        return 1;
    }
    if(cutLUT()) {
        return 1;
    }
    if(cutCoulomb() || cutHBT()) {
        return 1;
    }

    if (!mdeltaEta) {
        mdeltaEta = fabs(DeltaEta());  // may have been set above
    }
  
    if(mdeltaEta<0.03){

        //--> qInv and EntSep are combined for speed & small delta eta 
        if(cutqInvORNominalEntranceSep()) return 1;

        if(mType==1 || mType==3) {
            if(cutMidTpcSepUS()) return 1;
        } else {
            if(cutMidTpcSepLS()) return 1;
        }
    }

    if (cutQuality()) {
        return 1;
    }

    if(cutMass()) return 1;

    //  if(cutExitSep() || cutQuality()) return 1;
    return 0;
}

//------------------------------------------------------------
int StEStructPairCuts::cutPairHistograms(){

  //>>>>> djp  I haven't been updating this code with to include my attempts at pair cuts.
  //>>>>>      Should do this sometime.

  // much much slower cut code - should be run on a small sub-sample to view 
  // results of histograms.  Tricky piece here is the connection between pair
  // pair types and cut and event inter-cut relations. 
  // where I need to set the value used for histogramming to be
  // at a specific overflow: xhi+2.0*(xhi-xlo) when this particular cut is
  // not in play for this specific Pair.

  int cutCount=0;

  cutCount+=cutDeltaPhiH(); 
  cutCount+=cutDeltaEtaH();
  cutCount+=cutDeltaMtH(); 

  cutCount+=cutHBT();
  cutCount+=cutCoulomb();
  cutCount+=cutMerging();
  cutCount+=cutCrossing();

  if(mdeltaEta<0.03){
    cutCount+=cutqInvH();
    cutCount+=cutEntranceSepH();
    if(mType==1 || mType==3){
      cutCount+=cutMidTpcSepUSH();
      if(mMidTpcSepLSCut)mvalues[mMidTpcSepLSName.idx]=mMidTpcSepLS[1]+2.0*(mMidTpcSepLS[1]-mMidTpcSepLS[0]);
    } else {
      cutCount+=cutMidTpcSepLSH();
      if(mMidTpcSepUSCut)mvalues[mMidTpcSepUSName.idx]=mMidTpcSepUS[1]+2.0*(mMidTpcSepUS[1]-mMidTpcSepUS[0]);
    }    
  } else {
      if(mqInvCut)mvalues[mqInvName.idx]=mqInv[1]+2.0*(mqInv[1]-mqInv[0]);
      if(mEntSepCut)mvalues[mEntSepName.idx]=mEntSep[1]+2.0*(mEntSep[1]-mEntSep[0]);
      if(mMidTpcSepUSCut)mvalues[mMidTpcSepUSName.idx]=mMidTpcSepUS[1]+2.0*(mMidTpcSepUS[1]-mMidTpcSepUS[0]);
      if(mMidTpcSepLSCut)mvalues[mMidTpcSepLSName.idx]=mMidTpcSepLS[1]+2.0*(mMidTpcSepLS[1]-mMidTpcSepLS[0]);
  }
  
  cutCount+=cutExitSepH();
  cutCount+=cutQualityH();

  fillHistograms((cutCount==0));

  return cutCount;
}

StLorentzVectorF 
StEStructPairCuts::fourMomentumSum() const
{
  StLorentzVectorF temp = mTrack1->FourMomentum()+mTrack2->FourMomentum();
  return temp;
}
//-----------------------------------------------------------------
StLorentzVectorF 
StEStructPairCuts::fourMomentumDiff() const
{
  StLorentzVectorF temp = mTrack1->FourMomentum()-mTrack2->FourMomentum();
  return temp;
}

double 
StEStructPairCuts::quality() const {

 
  unsigned long padRow1To24Track1 = mTrack1->TopologyMapData(0) & mapMask0;
  unsigned long padRow25To45Track1 = mTrack1->TopologyMapData(1) & mapMask1;
  unsigned long padRow1To24Track2 = mTrack2->TopologyMapData(0) & mapMask0;
  unsigned long padRow25To45Track2 = mTrack2->TopologyMapData(1) & mapMask1;
  
  // AND logic
  unsigned long bothPads1To24 = padRow1To24Track1 & padRow1To24Track2;
  unsigned long bothPads25To45 = padRow25To45Track1 & padRow25To45Track2;
  // XOR logic
  unsigned long onePad1To24 = padRow1To24Track1 ^ padRow1To24Track2;
  unsigned long onePad25To45 = padRow25To45Track1 ^ padRow25To45Track2;
  int ibits;
  int Quality = 0;

  double normQual = 0.0;
  

  int MaxQuality = mTrack1->TopologyMapTPCNHits() + mTrack2->TopologyMapTPCNHits();

  for (ibits=8;ibits<=31;ibits++) {
    //    bitI = 0;
    // bitI |= 1UL<<(ibits);
    if ( onePad1To24 & bitI[ibits] ) {
      Quality++;
      //      continue;
    } else if ( bothPads1To24 & bitI[ibits] ) {
      Quality--;
    }
  }

  for (ibits=0;ibits<=20;ibits++) {
    //  bitI = 0;
    //bitI = 1UL<<(ibits);
    if ( onePad25To45 & bitI[ibits] ) {
      Quality++;
      //continue;
    } else if ( bothPads25To45 & bitI[ibits] ) {
      Quality--;
    }
  }

  normQual = (double)Quality/((double)MaxQuality );
  return ( normQual );

}

//--------------------------------------------------------
double 
StEStructPairCuts::OpeningAngle() const {
  StThreeVectorF p1 = mTrack1->FourMomentum().vect();
  StThreeVectorF p2 = mTrack2->FourMomentum().vect();
  double dAngInv = 57.296*acos((p1.dot(p2))/(p1.mag()*p2.mag()));
  return (dAngInv);
}

//--------------------------------------------------------
// Zoffset correction is needed for mixed events.  
double 
StEStructPairCuts::NominalTpcExitSeparation() const {
  StThreeVectorF off(0,0,mZoffset);
  StThreeVectorF diff = mTrack1->NominalTpcExitPoint() - mTrack2->NominalTpcExitPoint() + off;
  return (double)(diff.mag());
}

double 
StEStructPairCuts::NominalTpcXYExitSeparation() const {
  StThreeVectorF diff = mTrack1->NominalTpcExitPoint() - mTrack2->NominalTpcExitPoint();
  return (double)(diff.perp());
}

double 
StEStructPairCuts::NominalTpcZExitSeparation() const {
    // There is a problem when both tracks exit the TPC through the same endcap, then both exit
    //   points have z = (+/-) 200, and the z separation is exactly zero.
    // Distinguish casese
    //   -1 One track crosses endcap.
    //   -2 Both tracks cross same endcap.
    //   -3 Both tracks cross endcap but different endcaps.
    // Not sure what I was thinking, comparing floating point numbers with ==.
    // if ( fabs(mTrack1->NominalTpcExitPoint().z())==200 || fabs(mTrack2->NominalTpcExitPoint().z())==200 ) return -1;
    if ( mTrack1->EndCapOuter()!=0 || mTrack2->EndCapOuter()!=0 ) {
        if ( mTrack1->EndCapOuter()==mTrack2->EndCapOuter() ) {
            return -2;
        } else if ( mTrack1->EndCapOuter()==0 || mTrack2->EndCapOuter()==0 ) {
            return -1;
        } else {
            return -3;
        }
    }
    double diff = mTrack1->NominalTpcExitPoint().z() - mTrack2->NominalTpcExitPoint().z();
    return (double)(fabs(diff+mZoffset));

}
double 
StEStructPairCuts::TpcRadialEndCapSeparation() const {
    // Use Radial dseparation at endcap in some cases when one or both tracks cross
    // endcap before getting to some radius.
    float rdiff = mTrack1->EndCapRadius() - mTrack2->EndCapRadius();
    return (double)(fabs(rdiff));

}

//--------------------------------------------------------
double 
StEStructPairCuts::NominalTpcEntranceSeparation() const {
  StThreeVectorF off(0,0,mZoffset);
  StThreeVectorF diff = mTrack1->NominalTpcEntrancePoint() - mTrack2->NominalTpcEntrancePoint() + off;
  return (double)(diff.mag());
}

double 
StEStructPairCuts::NominalTpcXYEntranceSeparation() const {
  StThreeVectorF diff = mTrack1->NominalTpcEntrancePoint() - mTrack2->NominalTpcEntrancePoint();
  return (double)(diff.perp());
}
double 
StEStructPairCuts::NominalTpcZEntranceSeparation() const {
  double diff = mTrack1->NominalTpcEntrancePoint().z() - mTrack2->NominalTpcEntrancePoint().z();
  return (double)(fabs(diff + mZoffset));
}

//--------------------------------------------------------
double 
StEStructPairCuts::NominalTpcAvgXYSeparation() const {
  double x1=NominalTpcXYEntranceSeparation();
  double x2=MidTpcXYSeparation();
  double x3=NominalTpcXYExitSeparation();
  return (x1+x2+x3)/3.;
}
double
StEStructPairCuts::NominalTpcAvgZSeparation() const {
  double x1=NominalTpcZEntranceSeparation();
  double x2=MidTpcZSeparation();
  double x3=NominalTpcZExitSeparation();
  if (x3==-1) return (x1+x2)/2.;  // if particle exited the endcap, exlude from average
  else return (x1+x2+x3)/3.;
}
bool StEStructPairCuts::TracksCrossInPhi() const {
    // Try a direct (but must be quite a bit slower than the cross product)
    // calculation of phi differences. I think the cross product method had too many corner cases.
    // Really want dPhi between -pi and pi. I was making sure they were between -2pi and 2pi.
    double phiEnt1  = mTrack1->NominalTpcEntrancePoint().phi();
    double phiEnt2  = mTrack2->NominalTpcEntrancePoint().phi();
    double dphiEnt = phiEnt1 - phiEnt2;
    while (dphiEnt > M_PI) {
        dphiEnt -= 2*M_PI;
    }
    while (dphiEnt < -M_PI) {
        dphiEnt += 2*M_PI;
    }
    double phiExit1 = mTrack1->NominalTpcExitPoint().phi();
    double phiExit2 = mTrack2->NominalTpcExitPoint().phi();
    double dphiExit = phiExit1 - phiExit2;
    while (dphiExit > M_PI) {
        dphiExit -= 2*M_PI;
    }
    while (dphiExit < -M_PI) {
        dphiExit += 2*M_PI;
    }
    if ((dphiEnt*dphiExit < 0) && (fabs(dphiEnt) < M_PI/4)) {
        return true;
    }
    return false;
}

//--------------------------------------------------------
double 
StEStructPairCuts::MidTpcSeparation() const {
  StThreeVectorF off(0,0,mZoffset);
  StThreeVectorF diff = mTrack1->MidTpcPoint() - mTrack2->MidTpcPoint() + off;
  //cout << "Mid XY Sep" << diff.perp() << endl;
  return (double)(diff.mag());
}

double 
StEStructPairCuts::MidTpcXYSeparation() const {
  StThreeVectorF diff = mTrack1->MidTpcPoint() - mTrack2->MidTpcPoint();
  //cout << "Mid XY Sep" << diff.perp() << endl;
  return (double)(diff.perp());
}

double 
StEStructPairCuts::MidTpcZSeparation() const {
  double diff = mTrack1->MidTpcPoint().z() - mTrack2->MidTpcPoint().z();
  //cout << "Mid Z Sep" << fabs(diff.z()) << endl;
  return (double)(fabs(diff+mZoffset));
}
//--------------------------------------------------------
double StEStructPairCuts::OuterMidTpcSeparation() const {
    StThreeVectorF off(0,0,mZoffset);
    StThreeVectorF diff = mTrack1->OuterMidTpcPoint() - mTrack2->OuterMidTpcPoint() + off;
    return (double)(diff.mag());
}

double StEStructPairCuts::OuterMidTpcXYSeparation() const {
    StThreeVectorF diff = mTrack1->OuterMidTpcPoint() - mTrack2->OuterMidTpcPoint();
    return (double)(diff.perp());
}

double StEStructPairCuts::OuterMidTpcZSeparation() const {
    // See comments for NominalTpcZExitSeparation()
    if ( mTrack1->EndCapOuterMid()!=0 || mTrack2->EndCapOuterMid()!=0 ) {
        if ( mTrack1->EndCapOuterMid()==mTrack2->EndCapOuterMid() ) {
            return -2;
        } else if ( mTrack1->EndCapOuterMid()==0 || mTrack2->EndCapOuterMid()==0 ) {
            return -1;
        } else {
            return -3;
        }
    }
    double diff = mTrack1->OuterMidTpcPoint().z() - mTrack2->OuterMidTpcPoint().z();
    return (double)(fabs(diff+mZoffset));
}


/***********************************************************************
 *
 * $Log: StEStructPairCuts.cxx,v $
 * Revision 1.15  2012/11/16 21:22:27  prindle
 * 2ptCorrelations: SS, AS histograms.  Get eta limits from cuts. Fit PtAll histogram. Add histograms to keep track of eta, phi limits. A few more histograms
 * Binning: Add quality cut.
 * CutBin: modify mode9
 * PairCuts: modify goodDeltaZ for case of one track leaving via endcap.
 *
 * Revision 1.14  2010/09/02 21:24:08  prindle
 *   2ptCorrelations: Fill histograms for event mixing information
 *                    Option for common mixing buffer
 *                    Switch to selectively fill QInv histograms (which take a long time)
 *   CutBin: Moved PID code to Track class from Pair class. Needed to update this code.
 *   PairCuts: Moved PID code from here to Track class.
 *             Removed unnecessary creation of StThreeVector which seem to take a long time
 *             Add ToF momentum cuts, modify dEdx momentum cuts. (Now allow dEdx to be
 *             be identified up to 15GeV/c, ToF up to 10GeV/c.)
 *
 * Revision 1.13  2010/03/02 21:45:28  prindle
 *   Had a problem with pair cuts when one track exited via endplate
 *   Calculate maxDEta properly
 *   Warning if you try turning histograms for pair cuts on
 *
 * Revision 1.12  2009/11/09 21:32:41  prindle
 * Fix warnings about casting char * to a const char * by redeclaring as const char *.
 *
 * Revision 1.11  2009/05/08 00:09:55  prindle
 * In 2ptCorrelations we added switches to select blocks of histograms to fill.
 * (See constructor in StEStruct2ptCorrelations.cxx)
 * Use a brute force method for checking crossing cuts. I had too many corner
 * cases with my clever check.
 * In Binning, change Yt limit and add methods for accessing number of histogram bins
 * to use (used in Support)
 *
 * Revision 1.10  2008/12/02 23:45:06  prindle
 * Changed switchYt to switchXX (etc.) to better reflect function.
 * Change minYt to 1.0 in Binning so YtYt histogram doesn't have empty lower bin (pt = 0.164 for yt = 1.0)
 * In CutBin: remove initPtBin
 *            add mode 8
 *            add notSymmetrized (used in Support)
 * Added LUT (Look Up Table) for pair cuts. Experimental for now.
 * Modified cutMerging2 (to look at track separation at a few radii)
 * and cutCrossing2 so it doesn't accidentally reject almost back to back tracks.
 *
 * Revision 1.9  2008/03/19 22:06:01  prindle
 * Added doInvariantMass flag.
 * Added some plots in pairDensityHistograms.
 * SetZOffset used to only be done when doPairDensity was true.
 * Moved creating/copying pairDensity histograms to same place as other histograms.
 * Added cutBinHistMode
 * mode3 neck was defined as yt1<2.2 && yt2<2.2 (and not soft)
 *            now is        1.8<yt1<2.2  && 1.8<yt2<2.2
 * Added gooddzdxy, Merging2 and Crossing2 to pair cuts.
 *
 * Revision 1.8  2007/11/26 19:55:25  prindle
 * In 2ptCorrelations: Support for keeping all z-bins of selected centralities
 *                     Change way \hat{p_t} is calculated for parent distributions in pid case.
 *    Binning          Added parent binning (for \hat{p_t}
 *    CutBin:          Mode 5 extensively modified.
 *                     Added invariant mass cuts (probably a bad idea in general.)
 *
 * Revision 1.7  2007/05/27 22:45:03  msd
 * Added new cut bin modes 2 (soft/hard SS/AS), 6 (z-vertex binning), and 7 (modes 2*6).
 * Fixed bug in merging cut.
 * Added a few histograms to 2pt corr.
 *
 * Revision 1.6  2007/01/26 17:17:10  msd
 * Implemented new binning scheme: dEta stored in array with bin centered at zero, dPhi array has bins centered at zero and pi.  Final DEtaDPhi has 25x25 bins with dPhi bin width of pi/12 so all major angles are centered in bins.
 *
 * Revision 1.5  2006/10/02 22:21:02  prindle
 * Store only quadrant of eta_Delta - phi_Delta array/histogram.
 * Store half of eta_Sigma - phi_Delta array/histogram.
 * This required modifications in Binning.
 * I had a bug in the pair loop (which left +- not fully symmetrized)
 * and had to make changes in cut bins for mode 5 (and 3 I think)
 * when I fixed this.
 * Also change crossing cut to use only two parameters, the sign of
 * the magnetic field being taken from the MuDst.
 *
 * Revision 1.4  2006/04/04 22:10:13  porter
 * a handful of changes (specific to correlations)
 *  - added StEStructQAHists so that if NOT input frm Maker, each analysis has its own
 *  - used ability to get any max,min val from the cut class - or z-vertex binning
 *  - put z-vertex binning into 1 place
 *  - switched back 1st line of pair cut method to keep pair if good, not to reject if bad.
 *  - Pair cut object is now pointer in correlations
 *  - some diagnostic printouts available from macro
 *  - Duncan's delta-phi binning change
 *
 * Revision 1.3  2005/09/14 17:14:25  msd
 * Large update, added new pair-cut system, added pair density plots for new analysis mode (4), added event mixing cuts (rewrote buffer for this)
 *
 * Revision 1.2  2004/06/25 03:11:49  porter
 * New cut-binning implementation and modified pair-cuts for chunhui to review
 *
 * Revision 1.1  2003/10/15 18:20:46  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/




