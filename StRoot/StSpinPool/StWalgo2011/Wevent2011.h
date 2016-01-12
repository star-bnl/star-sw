// $Id: Wevent2011.h,v 1.9 2016/01/08 02:08:49 jlzhang Exp $
//
//*-- Author : Jan Balewski, MIT

//----------------------------
//------- W-reco event container
//----------------------------
#ifndef W_EVENT_2011_HH
#define W_EVENT_2011_HH

#ifdef __APPLE__
#include <sys/types.h>
#endif
#include <TVector3.h>
#include <TH1.h>
//get L2
#include <StTriggerUtilities/L2Emulator/L2wAlgo/L2wResult2009.h>

#include <StMuDSTMaker/COMMON/StMuTrack.h>
#include "WanaConst.h"

//---------------
class WeveCluster { // info BTOW cluster
 public:
  float energy,ET, adcSum;
  int nTower; // with non-zero ADC>kSigPed
  int iEta, iPhi; // lower-left corner of the cluster, can be negative, L2-indexing convention
  //int iEtaE[4]; int iPhiE[4]; //index of cluster towers
  TVector3 position; // 3D ln(E) weighted sume of tower positions
  WeveCluster() {clear();}

  void clear() {  position=TVector3(0,0,0);
    ET=energy=adcSum=0; nTower=0; iEta=iPhi=999;
    //memset(iEtaE,9999,sizeof(iEta)); memset(iPhiE,9999,sizeof(iPhi));
  }
  void print( int flag=0){
    printf("     Cluster ET=%.1f E=%.1f GeV, sumAdc=%.0f nTw=%d iEta=%d, iPhi=%d XYZ(%.0f,%.0f,%.0f)cm\n",ET,energy,adcSum,nTower,iEta, iPhi,position.x(),position.y(),position.z());}

 private:
 protected:
  ClassDef(WeveCluster,1);

};
 


//---------------
class WevePointTower { // tower pointed by the track
 public:
  TVector3 R; // extrapolated position of primary track
  TVector3 Rglob; // extrapolated position of global track
  int id; // BTOW tower id, not used for ETOW
  int iEta, iPhi; // eta x phi bin using L2 indexing convention
  void clear() {    id=0; R=TVector3(0,0,0); Rglob=TVector3(0,0,0); iEta=iPhi=9999; }
  void print( int flag=0){
    printf("     pointed tower ID=%d; L2index: iEta=%d iPhi=%d; XYZ=(%.0f,%.0f,%.0f)cm\n",
	   id,iEta, iPhi, R.x(), R.y(), R.z());
  }

 private:
 protected:
  ClassDef(WevePointTower,1);

};

//---------------
class WeveEleTrack { // electron track info
 public:
  int isMatch2Cl; // result of cuts
  WevePointTower pointTower;
  const StMuTrack *glMuTrack, *prMuTrack;
  WeveCluster cluster,cl4x4;
  TVector3 primP; // primary momentum vector
  float nearTpcPT, nearEmcET, nearBtowET, nearEtowET, smallNearTpcPT; // (GeV/c), around prim track direction
  float awayTpcPT, awayEmcET, awayBtowET, awayEtowET; // (GeV/c), opposite in phi to  prim track direction
  float nearTotET, awayTotET, nearTotET_noEEMC, awayTotET_noEEMC; // (GeV), for nearCone 10 GeV is subtracted to avoid double counting
     
  TVector3 ptBalance,ptBalance_noEEMC;
  float sPtBalance,sPtBalance_noEEMC;// signed pT balance (GeV/c)
  float sPtBalance2,sPtBalance_noEEMC2;// invariant
  int jetCount;// number of jets out the near-cone; add in Dec 19, 2014, jinlong

  TVector3 hadronicRecoil;

  //esmd shower info
  int hitSector; int esmdGlobStrip[mxEsmdPlane];
  float esmdShower[mxEsmdPlane][41];
  float esmdDca[mxEsmdPlane]; float esmdDcaGlob[mxEsmdPlane];
  float esmdE[mxEsmdPlane]; float esmdEsum7[mxEsmdPlane]; int esmdNhit[mxEsmdPlane];
  float esmdPeakSumE[mxEsmdPlane]; int esmdPeakOffset[mxEsmdPlane];//in strips 

  TVector3 esmdXPcentroid;
  int esmdMaxADC;

  //eprs cluster
  float enePre1,enePre2,enePost;

  WeveEleTrack() { clear();}
  
  void clear() {  pointTower.clear();
    cluster.clear();cl4x4.clear();  isMatch2Cl=false;  primP=TVector3(0,0,0);
    prMuTrack=glMuTrack=0; 
    awayTpcPT=nearTpcPT=nearTotET=awayTotET=nearEmcET=awayEmcET=nearBtowET=awayBtowET=nearEtowET=awayEtowET=smallNearTpcPT=nearTotET_noEEMC=awayTotET_noEEMC=0; 
    enePre1=enePre2=enePost=0; esmdMaxADC=0;

    ptBalance=TVector3(0,0,0); ptBalance_noEEMC=TVector3(0,0,0); 
    sPtBalance=sPtBalance_noEEMC=0;
    sPtBalance2=sPtBalance_noEEMC2=0;
	
	jetCount=0; 

    hadronicRecoil=TVector3(0,0,0);

    memset(esmdGlobStrip,-999,sizeof(esmdGlobStrip));
    memset(esmdDca,-999.,sizeof(esmdDca)); memset(esmdDcaGlob,-999.,sizeof(esmdDcaGlob)); memset(esmdE,0.,sizeof(esmdE)); memset(esmdEsum7,0.,sizeof(esmdEsum7)); memset(esmdNhit,0,sizeof(esmdNhit)); memset(esmdPeakSumE,0,sizeof(esmdPeakSumE)); memset(esmdPeakOffset,0,sizeof(esmdPeakOffset));
    esmdXPcentroid=TVector3(0,0,0);

  } 
  
  void print( int flag=0){
    if(prMuTrack==0) {  printf("   Track NULL pointer???\n"); return;}
    printf("   Track glPT=%.1f GeV/c   isMatch2Cl=%d, nearTotET=%.1f, awayTotET=%.1f primPT=%.1f\n",
	   glMuTrack->pt(),isMatch2Cl,nearTotET, awayTotET,primP.Pt());
    pointTower.print(flag);
    cluster.print(flag);   
    TVector3 D=pointTower.R-cluster.position;
    printf("                XYZ(track-cluster):  |3D dist|=%.1fcm  delZ=%.1fcm\n",D.Mag(),D.z());
    printf("     4x4 :"); cl4x4.print(flag);    
    printf("     nearET/GeV:    TPC=%.1f   Emc=%.1f (BTOW=%.1f ETOW=%.1f) sum=%.1f\n",nearTpcPT,nearEmcET,nearBtowET,nearEtowET,nearTotET);
    printf("     awayET/GeV:    TPC=%.1f   Emc=%.1f (BTOW=%.1f ETOW=%.1f) sum=%.1f\n",awayTpcPT,awayEmcET,awayBtowET,awayEtowET,awayTotET);
  }

 private:
 protected:
  ClassDef(WeveEleTrack,2);

};

//---------------
class WeveVertex { // info about vertex
 public:
  int id; // as store do muDst list
  float z; // cm
  float rank,funnyRank;
  int nEEMCMatch; //# of matched endcap towers
  vector <WeveEleTrack> eleTrack;
  vector <StMuTrack*> prTrList;

  void clear() {
    id=-999; z=-999; funnyRank=-9999; rank=-9999;
    eleTrack.clear(); nEEMCMatch=-999;
  }
  
  void print( int flag=0){
    printf(" Vertex ID=%d Z=%.1f cm  nTrack=%d\n",id,z, eleTrack.size());
    for(uint i=0;i< eleTrack.size();i++) 
      eleTrack[i].print();
  }

 private:
 protected:
  ClassDef(WeveVertex,1);
  
};

//---------------
class WeveBEMC { // info about BEMC
 public:
  //raw BTOW/BPRS hits
  int   tileIn[mxBTile]; // 0 if no data
  float adcTile[mxBTile][mxBtow];
  float eneTile[mxBTile][mxBtow];
  int   statTile[mxBTile][mxBtow];
  float maxAdc;
  int maxHtDsm;

  //raw BSMD hits, both planes
  float adcBsmd[mxBSmd][mxBStrips];
  int  statBsmd[mxBSmd][mxBStrips];

  void clear() {
    memset(adcTile,0,sizeof(adcTile));
    memset(eneTile,0,sizeof(eneTile));
    memset(statTile,-1,sizeof(statTile));  // default all dead
    memset(tileIn,0,sizeof(tileIn)); // detector was On/Off
    memset(adcBsmd,0,sizeof(adcBsmd));
    memset(statBsmd,-1,sizeof(statBsmd));// default all dead
    maxAdc=0;
    maxHtDsm=-1;
  }

  void print( int flag=0){
    printf(" BTOW tower ADC>500 list: ");
    for(int i=0;i< mxBtow;i++) {
      if(adcTile[kBTow][i]<500) continue;
      int id=i+1;
      printf("id=%d adc=%.1f ene=%.1f;  ",id,adcTile[kBTow][i],eneTile[kBTow][i]);
    }    printf("\n");
   
#if 0 
    printf(" BSMDE tower ADC>200 list: ");
    for(int i=0;i< mxBStrips;i++) {
      if(adcBsmd[ kBSE][i]<200) continue;
      int id=i+1;
      int module= 1+i/150;
      printf("id=%d mod=%d adc=%.1f ;  ",id,module,adcBsmd[ kBSE][i]);
    }    printf("\n");
#endif

    printf(" BTOW maxAdc=%.1f  maxHtDsm=%d\n",maxAdc, maxHtDsm);
   
    if(flag&1) {//..................
      for(int i=0;i<120;i++) {
        int id=7+i*40;
        if(i%10==0) printf("\n  softID=%4d adc format  BTOW:BPRS=  ",id);
        printf("%.0f : %.0f, ", adcTile[kBTow][id-1], adcTile[kBPrs][id-1]);
      }
      printf("\n");
    }
  }// end of print

 private:
 protected:
  ClassDef(WeveBEMC,2);
  
};

//--------------
class WeveETOW { // info about ETOW           
 public:
  
  //raw ETOW hit
  int etowIn;
  float adc[mxEtowSec*mxEtowSub][mxEtowEta]; //[phibin][etabin]
  float ene[mxEtowSec*mxEtowSub][mxEtowEta];
  int stat[mxEtowSec*mxEtowSub][mxEtowEta];
  float maxAdc; 
  int maxSec,maxSub,maxEta;
  int maxHtDsm;
  
  void clear() {
    memset(adc,0,sizeof(adc));
    memset(ene,0,sizeof(ene));
    memset(stat,-1,sizeof(stat));  // default all dead
    maxAdc=0;
    maxSec=maxSub=maxEta=0;
    maxHtDsm=-1;
  }
  
 private:
 protected:
  ClassDef(WeveETOW,2);

};

//--------------
class WeveEPRS { // info about EPRS           
 public:

  //raw EPRS hit
  int eprsIn;
  float adc[mxEtowSec*mxEtowSub][mxEtowEta][mxPrs]; //[phibin][etabin][layer]
  float ene[mxEtowSec*mxEtowSub][mxEtowEta][mxPrs];
  int stat[mxEtowSec*mxEtowSub][mxEtowEta][mxPrs];

  void clear() {
    memset(adc,0,sizeof(adc));
    memset(ene,0,sizeof(ene));
    memset(stat,-1,sizeof(stat));  // default all dead
  }

 private:
 protected:
  ClassDef(WeveEPRS,1);

};

//--------------
class WeveESMD { // info about ESMD           
 public:

  //raw ESMD hit
  int esmdIn;
  float adc[mxEtowSec][mxEsmdPlane][mxEsmdStrip]; //[phibin][etabin]
  float ene[mxEtowSec][mxEsmdPlane][mxEsmdStrip];
  int stat[mxEtowSec][mxEsmdPlane][mxEsmdStrip];

  void clear() {
    memset(adc,0,sizeof(adc));
    memset(ene,0,sizeof(ene));
    memset(stat,-1,sizeof(stat));  // default all dead
  }

 private:
 protected:
  ClassDef(WeveESMD,1);

};
 

//---------------
class Wevent2011 : public TObject {
 public:
  // .....variables ....
  int l2bitET,l2bitRnd;
  int l2EbitET,l2EbitRnd;
  int trigAwaySum[16]; //for lumi
  int trigTotalSum;  //for lumi

  int id; // eventID
  int runNo;
  int time;
  float zdcRate;
  int bx7, bx48; // raw from muDst
  int bxStar7, bxStar48, spin4; // using spinDb or -1 if failed
  bool zTag;
  vector <WeveVertex> vertex;
  WeveBEMC bemc;
  WeveETOW etow;
  WeveEPRS eprs;
  WeveESMD esmd;

  // .... methods ....
  Wevent2011() {};

  void clear() { 
    //printf("W2011event:clear()\n");
    id=runNo=time=0;
    zdcRate=0;
    l2bitET=l2bitRnd=0;
    l2EbitET=l2EbitRnd=0;
    bx7=bx48=-1;
    zTag=false;
    bxStar7=bxStar48= spin4=-1;
    vertex.clear();
    bemc.clear();
    etow.clear(); eprs.clear(); esmd.clear(); 
  }
  
  //...........................
  void print( int flag=0, int isMC=0) {
    printf("\nmy W2011event runNo=%d ID=%d  L2Wbits: ET=%d rnd=%d;  muDst: bx7=%d bx48=%d nVert=%d star: Bx7m=%d, Bx48=%d, spin4=%d \n",runNo,id,l2bitET,l2bitRnd,bx7,bx48, vertex.size(),bxStar7, bxStar48, spin4);
    int  yyyymmdd,  hhmmss; getGmt_day_hour( yyyymmdd,  hhmmss);
    printf("  event time is: day=%d, hour=%d (GMT)\n",yyyymmdd,hhmmss);

    for(uint i=0;i< vertex.size();i++) vertex[i].print(flag);
    bemc.print(flag);
    
  }// end of PRINT
  
  void getGmt_day_hour(int & yyyymmdd, int & hhmmss) {
    time_t rawtime=this->time;
    struct tm * timeinfo= gmtime ( &rawtime );
    char buffer [80];
    strftime (buffer,80,"%k%M%S",timeinfo);
    //puts (buffer);
    hhmmss=atoi(buffer);
    strftime (buffer,80,"%G%m%d",timeinfo);
    //puts (buffer);
    yyyymmdd=atoi(buffer);
    //printf("day=%d, hour=%d\n",yyyymmdd,hhmmss);

  }
  
 private:
 protected:
  ClassDef(Wevent2011,2);

};

#endif


// $Log: Wevent2011.h,v $
// Revision 1.9  2016/01/08 02:08:49  jlzhang
// added couples histograms and fixed a small bug
//
// Revision 1.8  2013/09/13 19:33:13  stevens4
// Updates to code for combined 2011+2012 result presented to spin PWG 9.12.13
//
// Revision 1.7  2013/01/15 23:26:35  fisyak
// add sys/types.h for APPLE
//
// Revision 1.6  2012/09/21 16:59:10  balewski
// added ESMD peak adjustement - partialy finished
//
// Revision 1.5  2012/08/21 18:29:16  stevens4
// Updates to endcap W selection using ESMD strip ratio
//
// Revision 1.4  2012/07/12 20:49:21  balewski
// added spin info(star: bx48, bx7, spin4) and maxHtDSM & BTOW to Wtree
// removed dependence of spinSortingMaker from muDst
// Now Wtree can be spin-sorted w/o DB
// rdMu.C & readWtree.C macros modified
// tested so far on real data run 11
// lot of misc. code shuffling
//
// Revision 1.3  2012/06/18 18:28:01  stevens4
// Updates for Run 9+11+12 AL analysis
//
// Revision 1.2  2011/02/25 06:03:56  stevens4
// addes some histos and enabled running on MC
//
// Revision 1.1  2011/02/10 20:33:26  balewski
// start
//
