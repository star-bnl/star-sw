// $Id: EEsoloPi0.cxx,v 1.11 2009/02/04 20:33:21 ogrebeny Exp $
 
#include <assert.h>
#include <stdlib.h>


#include <TClonesArray.h>
#include <TVector3.h>
#include <TObjArray.h> 
#include <TH1.h> 
#include <TH2.h> 

#include "EEsoloPi0.h"

#include "StEEmcUtil/EEfeeRaw/EEfeeRawEvent.h"
#include "StEEmcUtil/EEfeeRaw/EEstarTrig.h"
#include "StEEmcUtil/EEfeeRaw/EEmcEventHeader.h"

#include "StEEmcUtil/EEfeeRaw/EEfeeDataBlock.h"
#include "StEEmcUtil/EEfeeRaw/EEname2Index.h"

#include "StEEmcUtil/database/EEmcDbItem.h"


#ifdef StRootFREE
  #include "EEmcDb/EEmcDb.h"
#else
  #include "StEEmcUtil/database/StEEmcDb.h"
#endif


#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"

ClassImp(EEsoloPi0)
//--------------------------------------------------
//--------------------------------------------------
EEsoloPi0::EEsoloPi0(){

  nInpEve=0;
  HList=0;
  eeDb=0;
  dbMapped=-1;

  geom= new EEmcGeomSimple();
  set(0,0,0);
  printf("EEsoloPi0() constructed\n");
  nClust=0; 
  
  float XscaleFactor=1;
  float XseedEnergy=0.8;
  float XshapeLimit=.7;
  float XmassLo=0.07, XmassHi=0.22;
  set(XscaleFactor, XseedEnergy,XshapeLimit,XmassLo,XmassHi);
  memset(hA,0, sizeof(hA));
  memset(hR,0, sizeof(hR));
  memset(hM,0, sizeof(hM));

}

//--------------------------------------------------
//--------------------------------------------------
EEsoloPi0::~EEsoloPi0() {/* noop */}

//-------------------------------------------------
//-------------------------------------------------
void EEsoloPi0::initRun(int runID){
  printf(" EEsoloPi0::initRun(%d)\n",runID);

  assert(dbMapped<0); // at the moment DB reloading is not implemented/tested,JB
  dbMapped=runID;
}

//-------------------------------------------------
//-------------------------------------------------
//-------------------------------------------------
void EEsoloPi0::init(){
  int i;
  float Emax=20;
  TString C="";

  hA[0]=new TH1F ("tE","Eneregy (GeV) from any tower",100,0.,Emax); 
  hA[1]=new TH1F ("sE","Total  Eneregy in event (GeV) (sum from all tower)",100,0.,Emax*24); 

  hA[2]=new TH1F ("cE","cluster Eneregy (GeV) ",200,0.,1.5*Emax);
 
  hA[3]=new TH1F ("cSh","Shape: energy ratio highT/cluster",55,0.,1.1); 
  hA[4]=new TH1F ("cN","No. of clusters per event",30,-0.5,29.5);
  hA[5]=new TH1F ("tT","Transverse Eneregy (GeV) from any tower",100,0.,Emax); 
  hA[6]=0;
  hA[7]=new TH1F ("ctbSum","CTB ADC sum", 240,0,12000);

  if(HList) {
    for(i=0;i<=14;i++) {
      if(hA[i]==0) continue;
      HList->Add(hA[i]);
    }  
  }

  // Mix/noMix histos
  hR[0]=0;
  
  hR[1]=new TH1F ("invm","Invariant mass of 2 gammas (GeV)",80,0.,1.2);
  
  hR[2]=(TH1F*)new TH2F ("d-E","Distance (cm) vs. 2clust energy (GeV)",50,0,1.5*Emax,100,0.,100);
  hR[3]=(TH1F*)new TH2F ("eta12","eta1 vs. eta2 (bins)",12,0.5,12.5,12,0.5,12.5);
  
  hR[4]=(TH1F*)new TH2F ("xyL","Y vs. X  (cm) of LOW energy cluster",200,-250.,250,200,-250,250);
  hR[5]=(TH1F*)new TH2F ("xyH","Y vs. X  (cm) of HIGH energy cluster",200,-250.,250,200,-250,250);
  hR[6]=new TH1F ("oAng","Opening angle/rad of any pair",30,0.,.3);
  
  
  char ttt[100];
  sprintf(ttt,"cut invM=[%.2f,%.2f]",mLo,mHi);
  hR[7]=new TH1F ("0ener","Energy (GeV), "+C+ttt,50,0.,Emax/2.);
  hR[8]=new TH1F ("0eta","Pseudorapidity, "+C+ttt,25,0.,2.5); 
  hR[9]=new TH1F ("0phi","phi/rad, "+C+ttt,30,-3.14,3.14); 
  hR[10]=new TH1F ("0pt","pT (GeV/c), "+C+ttt,50,0.,Emax/4.);
  hR[11]=(TH1F*)new TH2F ("0xyL","Y vs. X  (cm) of LOW energy, "+C+ttt,200,-250.,250,200,-250,250);
  hR[12]=(TH1F*)new TH2F ("0xyH","Y vs. X  (cm) of HIGH energy, "+C+ttt,200,-250.,250,200,-250,250);
  
  hR[13]=new TH1F ("ytw","Yield of clusters ;X= iphi+(Eta-1)*60, spiral",721,-.5,720.5); 

  hR[14]=new TH1F ("0ytw","Yield of clusters, "+C+ttt+";X= iphi+(Eta-1)*60, spiral",721,-.5,720.5); 

  hR[15]=(TH1F*)new TH2F ("0d-E","Distance (cm) vs. 2clust energy (GeV), "+C+ttt,50,0,Emax,100,0.,100);


  hR[24]=new TH1F ("0Ang","Opening angle/rad of pairs "+C+ttt,50,0.,.7);
  hR[25]=(TH1F*)new TH2F ("invH","Time (minutes) vs. Invariant mass of 2 gammas (GeV)",75,0.,1.5,50,0,100);
  hR[26]=new TH1F ("0Z","Z=|E1-E2|/sum ,"+C+ttt,25,0.,1.0);

  
  for(i=0;i<12;i++) {
    char t1[100],t2[100];
    sprintf(t1,"invm%02d",i+1);
    sprintf(t2,"Invariant mass(GeV), ETA=%d",i+1);
    hR[27+i]=new TH1F (t1,t2,80,0.,1.2);
  }

  for(i=0;i<=38;i++) {
    TH1F *h1=hR[i];
    if(h1==0) continue;
    TH1F *h2=(TH1F *)h1->Clone();
    TString tt=h2->GetTitle();
    h2->SetTitle( "MIX: "+tt);
    tt=h2->GetName();
    h2->SetName( "X"+tt);
    h2->SetLineColor(kGreen);
    hM[i]=h2;
    if(HList) {	   
      HList->Add(h1);
      HList->Add(h2);
    }  
  }
  

  printf("\nEEsoloPi0::init(), cuts: scaleFactor=%f ch/GeV, seedEnergy=%f GeV ,shapeLimit=%f,  mLo=%.2f GeV, mHi=%.2f GeV\n\n",scaleFactor, seedEnergy,shapeLimit,mLo, mHi);

  //  HList->ls();


  oldClust.eH=0.5;
  oldClust.eC=0.6;
  oldClust.k1=1;
  oldClust.fphi=1.;
  oldClust.feta=1.;


  clear();
  TotN2g=totPi0=totXPi0=0;
  
}

//-------------------------------------------------
//-------------------------------------------------
void EEsoloPi0::clear(){
  if( nClust>1) {// preserve a random cluster from previous event
    int j=rand()%nClust;
    oldClust=clust[j];
  }
  memset(soloMip,0,sizeof(soloMip));
  memset(clust,0,sizeof(clust));
  nClust=0;

}

//-------------------------------------------------
//-------------------------------------------------
//-------------------------------------------------
void EEsoloPi0::finish(){
  float s1=totPi0;
  float s2=totXPi0;
  if(s1==0) s1=-1;
  if(s2==0) s2=-1;
  float x=s1-s2;
  float ex=sqrt(s1+s2);
  printf("s1=%f s2=%f\n",s1,s2);
  float s2b=x/s2; // signal to background 
  //error calculated  assuming s1 & s2 are independent
  float es2b=s2b*sqrt(1/s1+1/s2);

  printf("\n  EEsoloPi0::finish() TotN2g=%d, totPi0=%d totXPi0=%d \n",TotN2g,totPi0,totXPi0);
  printf(" Npi0=%.0f + / - %.0f ,  s2b=%.2f + / - %.3f  for invm=[%.2f, %.2f]\n\n", x,ex,s2b,es2b,mLo,mHi);
 
   
}

//-------------------------------------------------
//-------------------------------------------------
//-------------------------------------------------
void EEsoloPi0::print(){
  printf("\n  EEsoloPi0::print()\n soloMip dump:\n");
  int k0;
  for(k0=0;k0<MxTw;k0++) { 
    if(soloMip[k0].e<=0) continue;
   // if(soloMip[k0].key<=0) continue;
    int ieta=k0%12;
    int iphi=k0/12;
    printf("ieta=%d iphi=%d key=%d e=%f id=%d\n",ieta,iphi,soloMip[k0].key,soloMip[k0].e,soloMip[k0].id);
  }

  printf("Clusters found:\nid   k1   feta    fphi  eneTot\n");
  int ic;
  for(ic=0;ic<nClust;ic++) {
    printf("%d   %d   %.2f %.2f %5g\n",ic+1, clust[ic].k1,clust[ic].feta,clust[ic].fphi,clust[ic].eC);
  }
}

//---------------------------------------------------
//---------------------------------------------------
int  EEsoloPi0:: findTowerClust() {
  assert(seedEnergy>0);  // makes no sense to set it lower
  //..... some histos
  int k;
  float totEner=0;
  for(k=0;k<MxTw;k++) {
    float ene=soloMip[k].e;
    if(ene<=0) continue;
    // printf("irad=%d, e=%f\n",k,soloMip[k].e);
    hA[0]->Fill(ene);
    totEner+=ene;
  }
  hA[1]->Fill(totEner);
  
  //............  search for hight towers
  while(1) {
    float maxE=seedEnergy;
    int k0,k1=-1;
    for(k0=0;k0<MxTw;k0++) {
      if(soloMip[k0].e<maxE) continue;
      if(soloMip[k0].key>0) continue; // droped marked towers
      k1=k0;
      maxE=soloMip[k0].e;
    }
    if(k1<0) break; // no cluster found
    //  printf("iCl=%d, irad=%d energy=%f\n",nClust,k1,soloMip[k1].e);
    clust[nClust].k1=k1;
    clust[nClust].eH=soloMip[k1].e;
    nClust++;
    soloMip[k1].id=nClust;
    tagCluster(k1);     
  }
  //printf("nClust=%d\n",nClust);
  //if(nClust<2) return ;
  
  //............  sum energy of clusters, find centroid
  Cluster clustG[MxTw]; // Good cluster storage
  int nClustG=0;
  int ic;
  for(ic=0;ic<nClust;ic++) {
    sumTwClusterEnergy(ic);
    float rat=clust[ic].eH/clust[ic].eC;
    // printf(" sum energy ic=%d eH=%f eC=%f rat=%f\n",ic,clust[ic].eH,clust[ic].eC,rat);
    if(rat<shapeLimit)  continue;
    hA[2]->Fill(clust[ic].eC);   
    hA[3]->Fill(rat);    
    clustG[nClustG++]=clust[ic];
  }

  // printf(" nClustG=%d\n",nClustG);

  //.....  copy only good clusters
  for(ic=0;ic<nClustG;ic++) {
    //printf("copyG ic=%d k1=%d, eC=%f feta=%f fphi=%f\n",ic, clustG[ic].k1,clustG[ic].eC,clustG[ic].feta,clustG[ic].fphi);
    clust[ic]=clustG[ic];
  }
  nClust=nClustG;
    
  hA[4]->Fill(nClust);
  //printf("doneE nCl=%d %f %d\n",nClust,clust[0].eC,clust[0].k1);
  return nClust;
}



//---------------------------------------------------
//---------------------------------------------------
void EEsoloPi0::findTowerPi0() {
  if(nClust<2) return ;

  //..................  scan pairs of clusters
  int i,j;
  for(i=0;i<nClust;i++) 
    for(j=i+1;j<nClust;j++) {

      Cluster *cl1=&clust[i];
      Cluster *cl2=&clust[j];

      totPi0+=findInvM(cl1,cl2,hR);


      // mix event pair
      if(rand()%2)
	cl1=&oldClust;
      else
	cl2=&oldClust;

      // 
      // The pi0 finder cannot identify pi0's when the
      //  seed towers of the two clusters are adjacent.
      //  We therefore require the two mixed clusters
      //  to be separated in eta and/or phi by at least
      //  one 1.4 towers... (JB)
      //

      float a=cl1->feta - cl2->feta;
      float b=cl1->fphi - cl2->fphi;
      // trick to accomodate cyclic  phi
      // fphi1, fphi2 are in [0,MxTwPhi);
      // 'b' measures the opening angle in phi
      if(b<0) b+=MxTwPhi; 
      if(b>MxTwPhi/2.) b=MxTwPhi-b;
      if(sqrt(a*a+b*b) <sqrt(2.) )continue;
      
      totXPi0+=findInvM(cl1,cl2,hM);
      
    }


}



//---------------------------------------------------
//---------------------------------------------------
void EEsoloPi0:: tagCluster(int k0,int d){ 
  
  int ieta=k0%12;
  int iphi=k0/12;
 
  int i,j;
  float ener=soloMip[ ieta + MxTwEta*iphi ].e;
  for(i=ieta-d; i<=ieta+d;i++){
    // printf("try   ieta=%d \n",i);
    if( i>=MxTwEta || i<0) continue;
    for(j=iphi-d;j<=iphi+d;j++){
      int jj=j;
      if( jj<0 ) jj+=MxTwPhi;
      if( jj>=MxTwPhi ) jj-=MxTwPhi;
      assert( jj>=0 && jj<MxTwPhi );
      soloMip[ i + MxTwEta*jj ].key+=(int)(1000*ener);
      // printf("tag iphi=%d ieta=%d key=%d\n",j,i,  soloMip[ i + MxTwEta*j ].key);
    }
  }
}

//---------------------------------------------------
//---------------------------------------------------
void EEsoloPi0:: sumTwClusterEnergy(int ic,int d){ 
  
  int k0=clust[ic].k1;
  int ieta=k0%12;
  int iphi=k0/12;
 
  int i,j;
  float w0=soloMip[ieta + MxTwEta*iphi].key;
  double sum=0, sumi=0, sumj=0;
  for(i=ieta-d; i<=ieta+d;i++){
    if( i>=MxTwEta || i<0) continue;
    for(j=iphi-d;j<=iphi+d;j++){
      int jj=j;
      if( jj<0 ) jj+=MxTwPhi;
      if( jj>=MxTwPhi ) jj-=MxTwPhi;
      assert( jj>=0 && jj<MxTwPhi );
      int k1=i + MxTwEta*jj;
      if(soloMip[k1].e<=0) continue;

      float w=w0/soloMip[ k1 ].key;
      // printf("add to cl.k0=%d@ ieta=%d,phi=%d  ieta=%d iphi=%d w=%f\n",k0,ieta,iphi,i,j,w); 
      //      float e=soloMip[k1 ].e/soloMip[ k1 ].key;
      float e=w*soloMip[k1 ].e;
      
      sum+=e;
      sumi+=e*i;
      sumj+=e*j;
    }
  }
  assert(sum>0);
  //  printf("k0=%d sum=%f, sumi=%f, sumj=%f ic=%d \n",k0, sum,sumi,sumj,ic);
  clust[ic].eC=sum;
  clust[ic].feta=sumi/sum;
  clust[ic].fphi=sumj/sum;
  //printf("  feta=%f   fphi=%f\n",clust[ic].feta,   clust[ic].fphi);

}

//---------------------------------------------------
//---------------------------------------------------
float EEsoloPi0::sumPatchEnergy(int k0,int d,EEsoloMipA *soloMipX, float *maxVal){ 
  
  int ieta=k0%12;
  int iphi=k0/12; 
 
  int i,j;
  double sum=0;
  double max=0;
  for(i=ieta-d; i<=ieta+d;i++){
    if( i>=MxTwEta || i<0) continue;
    for(j=iphi-d;j<=iphi+d;j++){
      int jj=j;
      if( jj<0 ) jj+=MxTwPhi;
      if( jj>=MxTwPhi ) jj-=MxTwPhi;
      assert( jj>=0 && jj<MxTwPhi );
      int k1=i + MxTwEta*jj;
      if(soloMipX[k1].e<=0) continue;
      sum+=soloMipX[k1 ].e;
      if(max<soloMipX[k1 ].e) max=soloMipX[k1 ].e;
    }
  }

  if(maxVal) *maxVal=max;

  printf("sumPatchEnergy(k0=%d, d=%d)=%f ,max=%f\n",k0, d,sum,max);
  return sum;
}


//-------------------------------------------------
//-------------------------------------------------
//-------------------------------------------------
int EEsoloPi0::findInvM(Cluster *c1, Cluster *c2, TH1F **h){
  int isPi0=0;
  float e1=c1->eC;
  float e2=c2->eC;
  //  ((TH2F*) h[0])->Fill(e1,e2);

  TVector3 r1=geom-> getDirection( c1->feta, c1->fphi);
  TVector3 r2=geom-> getDirection( c2->feta, c2->fphi);
  
  TVector3  p1=e1* r1.Unit();
  TVector3  p2=e2* r2.Unit();
  
  float opAngle=p1.Angle(p2);
  // printf("-->%.1f\n", opAngle/3.1416*180);

  h[6]->Fill(opAngle);

  TVector3 p12=p1+p2;
  float e12=e1+e2;    
  float invm2= e12*e12-p12*p12;
  float invm=sqrt(invm2);
  TVector3 r12=r1-r2;
  float d12=sqrt(r12*r12);

#if 0
  if(invm>0.1 & invm<0.20) 
{
  printf("\ninvM=%.3f e1=%.2f e2=%.2f ",  invm,e1,e2);
  
#if 0
    //invm2 = 2.0*e1*e2*(1.0-cos(theta12))
    float mm2=2.0*e1*e2*(1.0-cos(p1.Angle(p2)));
 printf("\ninvM=%f e1=%f e2=%f feta1=%f fphi1=%f  feta2=%f fphi2=%f\n  d12/cm=%f ang01=%f ang02=%f ang12=%f etot=%f m=%f\n",  
	 invm,e1,e2,c1->feta, c1->fphi,c2->feta, c2->fphi,d12,p12.Angle(p1),p12.Angle(p2),p1.Angle(p2),e12, sqrt(mm2));

  printf(" x1,y1,z1=%f %f %f\n",r1.x(),r1.y(),r1.z());
  printf(" x2,y2,z2=%f %f %f\n",r2.x(),r2.y(),r2.z());
  printf(" p1,p1,p1=%f %f %f\n",p1.x(),p1.y(),p1.z());
  printf(" p2,p2,p2=%f %f %f\n",p2.x(),p2.y(),p2.z());
#endif
}
  
#endif

  h[1]->Fill(invm);
  h[25]->Fill(invm,timeSec/60.);
  // printf("%f %d\n",c1->feta, (int)(c1->feta+1));
  h[(int) (27+c1->feta)] ->Fill(invm);
  h[(int) (27+c2->feta)] ->Fill(invm);

  if(c1->eC < c2->eC) {
    ((TH2F*) h[4])->Fill(r1.x(),r1.y());
    ((TH2F*) h[5])->Fill(r2.x(),r2.y());
  } else {
   ((TH2F*) h[5])->Fill(r1.x(),r1.y());
   ((TH2F*) h[4])->Fill(r2.x(),r2.y());
  }

  int bin1=1+c1->k1%12;
  int bin2=1+c2->k1%12;
  if (bin2>bin1) { int kk=bin1; bin1=bin2; bin2=kk;}
  
  ((TH2F*) h[3])->Fill(bin1, bin2);  
  
  ((TH2F*) h[2])->Fill(e12,d12);
  
  //        ieta              iphi
  int kk1=(c1->k1%12)*60+ (c1->k1/12);
  int kk2=(c2->k1%12)*60+ (c2->k1/12);
  h[13]->Fill(kk1);
  h[13]->Fill(kk2);

  if(mLo<invm && invm<mHi){   // accepted pi0
    isPi0=1;
    h[7]->Fill(e12);
    h[8]->Fill(p12.Eta());
    h[9]->Fill(p12.Phi());
    h[10]->Fill(p12.Pt());
    h[14]->Fill(kk1);
    h[14]->Fill(kk2);
    ((TH2F*) h[15])->Fill(e12,d12);
    h[24]->Fill(opAngle);
       
    float zE=fabs(c1->eC - c2->eC)/(c1->eC + c2->eC);
    h[26]->Fill(zE);

    if(c1->eC < c2->eC) {
      ((TH2F*) h[11])->Fill(r1.x(),r1.y());
      ((TH2F*) h[12])->Fill(r2.x(),r2.y());
    } else {
      ((TH2F*) h[12])->Fill(r1.x(),r1.y());
      ((TH2F*) h[11])->Fill(r2.x(),r2.y());
    }
  }
  return isPi0;
}


/*****************************************************************
 * $Log: EEsoloPi0.cxx,v $
 * Revision 1.11  2009/02/04 20:33:21  ogrebeny
 * Moved the EEMC database functionality from StEEmcDbMaker to StEEmcUtil/database. See ticket http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1388
 *
 * Revision 1.10  2007/10/19 23:18:40  balewski
 * 2008 cleanup, now works only w/ regular muDst
 *
 * Revision 1.9  2005/03/01 20:02:15  balewski
 * hack to access 2005 trigger data
 *
 * Revision 1.8  2004/09/29 18:04:44  balewski
 * now it runs on M-C as well
 *
 * Revision 1.7  2004/09/03 04:50:52  balewski
 * big clenup
 *
 * Revision 1.6  2004/08/26 04:39:40  balewski
 * towards pi0
 *
 */ 


  


#if 0
  float adc2gev[MaxEtaBins]={ // ideal gain (ch/GeV)
    18.938, 20.769,  22.650,  24.575,
    26.539,  28.514, 30.493, 32.473,
    34.438, 36.387, 38.28, 40.146 };

      if(strstr(x->name,"01TA05")) continue;
      if(strstr(x->name,"11TD09")) continue;
      if(strstr(x->name,"09TB06")) continue;


#endif

