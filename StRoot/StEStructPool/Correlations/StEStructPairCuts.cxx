/**********************************************************************
 *
 * $Id: StEStructPairCuts.cxx,v 1.2 2004/06/25 03:11:49 porter Exp $
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

StEStructPairCuts::~StEStructPairCuts() {};

void StEStructPairCuts::init(){ 

  initCuts();
  initNames();
  if(isLoaded())loadCuts();

}

void StEStructPairCuts::initCuts(){

    mdphi[0]=mdphi[1]=0;
    mdeta[0]=mdeta[1]=0;
    mdmt[0]=mdmt[1]=0;
    mqInv[0]=mqInv[1]=0;
    mEntSep[0]=mEntSep[1]=0;
    mExitSep[0]=mExitSep[1]=0;
    mQuality[0]=mQuality[1]=0;
    mMidTpcSepLS[0]=mMidTpcSepLS[1]=0;    
    mMidTpcSepUS[0]=mMidTpcSepUS[1]=0;
    mdeltaPhiCut=mdeltaEtaCut=mdeltaMtCut=mqInvCut=mEntSepCut=mExitSepCut=mQualityCut=mMidTpcSepLSCut=mMidTpcSepUSCut=false;

    for(int i=0;i<4;i++)
    mdphiCounter[i]=mdetaCounter[i]=mdmtCounter[i]=mqInvCounter[i]=mEntSepCounter[i]=mExitSepCounter[i]=mQualityCounter[i]=msplitLSCounter[i]=msplitUSCounter[i]=0;

    
    mapMask0 = 0xFFFFFF00;
    mapMask1 = 0x1FFFFF;
    for(int i=0;i<32;i++)bitI[i]=1UL<<i;

};

void StEStructPairCuts::initNames(){

  strcpy(mdphiName.name,"DeltaPhi");
  strcpy(mdetaName.name,"DeltaEta");
  strcpy(mdmtName.name,"DeltaMt");
  strcpy(mqInvName.name,"qInv");
  strcpy(mEntSepName.name,"EntranceSep");
  strcpy(mExitSepName.name,"ExitSep");
  strcpy(mQualityName.name,"Quality");
  strcpy(mMidTpcSepLSName.name,"MidTpcSepLikeSign");
  strcpy(mMidTpcSepUSName.name,"MidTpcSepUnlikeSign");
  
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

  //  cout<<" didn't find any cut with this name "<<endl;
  return false;
};

void StEStructPairCuts::printCuts(ostream& ofs, char* cutName,int c1,int c2){
  ofs<<cutName<<c1<<" + "<<c2<<"  =  "<<c1+c2<<endl;
}
  

void StEStructPairCuts::printCuts(ostream& ofs){

  ofs<<"# ******************************************** "<<endl;
  ofs<<"# *************** Pair Cuts ****************** "<<endl;
  ofs<<"# *** format = Cut, minvalue, maxvalue     *** "<<endl;
  ofs<<"# ***      Sib LS + US = Total             *** "<<endl;
  ofs<<"# ***      Mix LS + US = Total             *** "<<endl;
  ofs<<"# ******************************************** "<<endl;
  ofs<<endl;
  char* cutTypes[]={"#---  Sibling Pairs : LS + US = ",
                    "#---  Mixed   Pairs : LS + US = "};
  if(mdeltaPhiCut){
    ofs<<mdphiName.name<<","<<mdphi[0]/M_PI<<","<<mdphi[1]/M_PI<<"\t\t\t"<<" # pair dphi cut"<<endl;
    printCuts(ofs,cutTypes[0],mdphiCounter[0],mdphiCounter[1]);
    printCuts(ofs,cutTypes[1],mdphiCounter[2],mdphiCounter[3]);
  }
  if(mdeltaEtaCut){
     ofs<<mdetaName.name<<","<<mdeta[0]<<","<<mdeta[1]<<"\t\t\t"<<" # pair deta cut"<<endl;
    printCuts(ofs,cutTypes[0],mdetaCounter[0],mdetaCounter[1]);
    printCuts(ofs,cutTypes[1],mdetaCounter[2],mdetaCounter[3]);
  }

  if(mdeltaMtCut){
     ofs<<mdmtName.name<<","<<mdmt[0]<<","<<mdmt[1]<<"\t\t\t"<<" # pair dmt cut"<<endl;
    printCuts(ofs,cutTypes[0],mdmtCounter[0],mdmtCounter[1]);
    printCuts(ofs,cutTypes[1],mdmtCounter[2],mdmtCounter[3]);
  }

  if(mqInvCut){
     ofs<<mqInvName.name<<","<<mqInv[0]<<","<<mqInv[1]<<"\t\t\t"<<" # pair qInv cut"<<endl;
    printCuts(ofs,cutTypes[0],mqInvCounter[0],mqInvCounter[1]);
    printCuts(ofs,cutTypes[1],mqInvCounter[2],mqInvCounter[3]);
  }

  if(mEntSepCut){
    ofs<<mEntSepName.name<<","<<mEntSep[0]<<","<<mEntSep[1]<<"\t\t\t"<<" # pair EntSep cut"<<endl;
    printCuts(ofs,cutTypes[0],mEntSepCounter[0],mEntSepCounter[1]);
    printCuts(ofs,cutTypes[1],mEntSepCounter[2],mEntSepCounter[3]);
  }

  if(mExitSepCut){
    ofs<<mExitSepName.name<<","<<mExitSep[0]<<","<<mExitSep[1]<<"\t\t\t"<<" # pair ExitSep cut"<<endl;
    printCuts(ofs,cutTypes[0],mExitSepCounter[0],mExitSepCounter[1]);
    printCuts(ofs,cutTypes[1],mExitSepCounter[2],mExitSepCounter[3]);
  }

  if(mQualityCut){
     ofs<<mQualityName.name<<","<<mQuality[0]<<","<<mQuality[1]<<"\t\t\t"<<" # pair Quality cut"<<endl;
    printCuts(ofs,cutTypes[0],mQualityCounter[0],mQualityCounter[1]);
    printCuts(ofs,cutTypes[1],mQualityCounter[2],mQualityCounter[3]);
  }

  if(mMidTpcSepLSCut){
     ofs<<mMidTpcSepLSName.name<<","<<mMidTpcSepLS[0]<<","<<mMidTpcSepLS[1]<<"\t\t\t"<<" # pair MidTpcSepLS cut"<<endl;
    printCuts(ofs,cutTypes[0],msplitLSCounter[0],msplitLSCounter[1]);
    printCuts(ofs,cutTypes[1],msplitLSCounter[2],msplitLSCounter[3]);
  }

  if(mMidTpcSepUSCut){
     ofs<<mMidTpcSepUSName.name<<","<<mMidTpcSepUS[0]<<","<<mMidTpcSepUS[1]<<"\t\t\t"<<" # pair MidTpcSepUS cut"<<endl;
    printCuts(ofs,cutTypes[0],msplitUSCounter[0],msplitUSCounter[1]);
    printCuts(ofs,cutTypes[1],msplitUSCounter[2],msplitUSCounter[3]);
  }

  ofs<<"# ******************************************** "<<endl<<endl;

}

//------------------------------------------------------------
int StEStructPairCuts::cutPair(){

  //  if(cutDeltaPhi() || cutDeltaEta() || cutDeltaMt()) return 1;

  if(goodDeltaPhi() && goodDeltaEta() && goodDeltaMt()) return 0;

  if(mdeltaEta<0.03){

    //--> qInv and EntSep are combined for speed & small delta eta 
    if(cutqInvORNominalEntranceSep()) return 1;
   
    if(mType==1 || mType==3) {
       if(cutMidTpcSepUS()) return 1;
    } else {
       if(cutMidTpcSepLS()) return 1;
    }
 
  }

  if(cutQuality()) return 1;

  //  if(cutExitSep() || cutQuality()) return 1;
  return 0;
}

//------------------------------------------------------------
int StEStructPairCuts::cutPairHistograms(){

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

  if(mdeltaEta<0.03){
    cutCount+=cutqInvH();
    cutCount+=cutEntranceSepH();
    if(mType==1 | mType==3){
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
double 
StEStructPairCuts::NominalTpcExitSeparation() const {
  StThreeVectorF diff = mTrack1->NominalTpcExitPoint() - mTrack2->NominalTpcExitPoint();
  return (double)(diff.mag());
}


//--------------------------------------------------------
double 
StEStructPairCuts::NominalTpcEntranceSeparation() const {
  StThreeVectorF diff = mTrack1->NominalTpcEntrancePoint() - mTrack2->NominalTpcEntrancePoint();
  //cout << "EntSep" << diff.mag() << endl;
  return (double)(diff.mag());


}
//--------------------------------------------------------
//--------------------------------------------------------
double 
StEStructPairCuts::MidTpcXYSeparation() const {
  StThreeVectorF diff = mTrack1->MidTpcPoint() - mTrack2->MidTpcPoint();
  //cout << "Mid XY Sep" << diff.perp() << endl;
  return (double)(diff.perp());
}

double 
StEStructPairCuts::MidTpcSeparation() const {
  StThreeVectorF diff = mTrack1->MidTpcPoint() - mTrack2->MidTpcPoint();
  //cout << "Mid XY Sep" << diff.perp() << endl;
  return (double)(diff.mag());
}


//--------------------------------------------------------
double 
StEStructPairCuts::MidTpcZSeparation() const {
  StThreeVectorF diff = mTrack1->MidTpcPoint() - mTrack2->MidTpcPoint();
  //cout << "Mid Z Sep" << fabs(diff.z()) << endl;
  return (double)(fabs(diff.z()));
}


/***********************************************************************
 *
 * $Log: StEStructPairCuts.cxx,v $
 * Revision 1.2  2004/06/25 03:11:49  porter
 * New cut-binning implementation and modified pair-cuts for chunhui to review
 *
 * Revision 1.1  2003/10/15 18:20:46  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/




