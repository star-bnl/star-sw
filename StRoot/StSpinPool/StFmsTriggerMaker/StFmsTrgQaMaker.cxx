#include "StFmsTrgQaMaker.h"
#include "StFmsTriggerMaker.h"
#include "StTriggerData.h"

static const char* BSname[68]={
  "ST_A","ST_B","ST_BC","ST_C","ST_CD","ST_D",
  "SM_DD",
  "SB_D","SB_CD","SB_C","SB_BC","SB_B","SB_A",
  "NB_A","NB_B","NB_BC","NB_C","NB_CD","NB_D",
  "NM_DD",
  "NT_D","NT_CD","NT_C","NT_BC","NT_B","NT_A",
  "ST_E","ST_EF","ST_F","ST_G","ST_GH","ST_H","ST_HI","ST_I","ST_IJ","ST_J",
  "SM_JJ",
  "SB_J","SB_IJ","SB_I","SB_HI","SB_H","SB_GH","SB_G","SB_F","SB_EF","SB_E",
  "NB_E","NB_EF","NB_F","NB_G","NB_GH","NB_H","NB_HI","NB_I","NB_IJ","NB_J",
  "NM_JJ",
  "NT_J","NT_IJ","NT_I","NT_HI","NT_H","NT_GH","NT_G","NT_F","NT_EF","NT_E"};

static const char* JPname[6]={"STop","SMid","SBot","NBot","NMid","NTop"};

static const int BSGRP[68]={0,1,1,1,1,1,
			    1,
			    1,1,1,1,1,2,
			    3,4,4,4,4,4,
			    4,
			    4,4,4,4,4,5,
			    6,6,7,7,7,7,7,7,7,7,
			    7,
			    7,7,7,7,7,7,7,7,8,8,
			    9,9,10,10,10,10,10,10,10,10,
			    10,
			    10,10,10,10,10,10,10,10,11,11};

inline int bt(int x, int pos) { return x >> pos & 1; }

ClassImp(StFmsTrgQaMaker);

StFmsTrgQaMaker::StFmsTrgQaMaker(const char* name): StMaker(name), mRun(0), mPrint(0) {}

void StFmsTrgQaMaker::Clear(Option_t* option){}

int StFmsTrgQaMaker::Init(){
  int yday=mRun/1000;
  sprintf(mFilename,"%d/fmstrg.%d.root",yday,mRun);
  printf("StFpsQaMaker::Init - Opening %s\n",mFilename);
  mFile=new TFile(mFilename,"RECREATE");
  mBS[0]=new TH1F("BS3","BS3",NBS,0.0,float(NBS));
  mBS[1]=new TH1F("BS2","BS2",NBS,0.0,float(NBS));
  mBS[2]=new TH1F("BS1","BS1",NBS,0.0,float(NBS));
  mJP[0]=new TH1F("JP2","JP2",NJP,0.0,float(NJP));
  mJP[1]=new TH1F("JP1","JP1",NJP,0.0,float(NJP));
  mJP[2]=new TH1F("JP0","JP0",NJP,0.0,float(NJP));
  mDIBSg=new TH2F("DiBSg","DiBSg",NBSG,0.0,float(NBSG),NBSG,0.0,float(NBSG));
  mDIBS=new TH2F("DiBS","DiBS",NBS,0.0,float(NBS),NBS,0.0,float(NBS));
  mDIJP=new TH2F("DiJP","DiJP",NJP,0.0,float(NJP),NJP,0.0,float(NJP));
  for(int i=0; i<NBS; i++){ hBS[i]=new TH1F(BSname[i],BSname[i],256,0.0,4096.0); }
  for(int i=0; i<NJP; i++){ hJP[i]=new TH1F(JPname[i],JPname[i],64,0.0,256.0); }  
  readtrgid();
  return kStOk;
}

int StFmsTrgQaMaker::Finish(){
  printf("Nevent=%d NFMStriggeredEvent=%d NFSMtrg=%d overlap=%f\n",
	 count[0],count[1],count[2],float(count[2])/float(count[1]));
  mFile->Write();
  mFile->Close();
  printf("StFmsTrgQaMaker::Finish - Closing %s\n",mFilename);
  return kStOK;
}

int StFmsTrgQaMaker::Make(){
  mSIM=(StFmsTriggerMaker*)GetMaker("fmstrigger"); 
  if(!mSIM) { printf("No StFmsTriggerMaker found\n"); return kStErr;}
  mTrgd=(StTriggerData*)GetDataSet("StTriggerData")->GetObject();
  if(!mTrgd) { printf("No StTriggerData found\n"); return kStErr;}
  
  int npre=mTrgd->numberOfPreXing();
  int npost=mTrgd->numberOfPostXing();
  //printf("Npre/Npost=%2d/%2d ",npre,npost); printTriggers();

  countOverlap();
  if(isTrg("FMS-LED")) return kStOK;
  fillBS();
  fillJP();
  fillBSsum();
  fillJPsum();
  fillDiBS();
  fillDiJp();
  return kStOK;
}

void StFmsTrgQaMaker::fillDiBS(){
  if(isTrg("FMS-DiBS")){
    char BSbit[NBS];
    memset(BSbit,0,sizeof(BSbit));
    int k=-1, i=2;
    for(int j=0; j<6;  j++){ k++; if(bt(mSIM->FM0xxuserdata( 1,i+7),j)) {BSbit[k]=1;}}
    for(int j=0; j<1;  j++){ k++; if(bt(mSIM->FM1xxuserdata( 1,i+7),j)) {BSbit[k]=1;}}
    for(int j=5; j>=0; j--){ k++; if(bt(mSIM->FM0xxuserdata( 2,i+7),j)) {BSbit[k]=1;}}
    
    for(int j=0; j<6 ; j++){ k++; if(bt(mSIM->FM0xxuserdata( 4,i+7),j)) {BSbit[k]=1;}}
    for(int j=0; j<1;  j++){ k++; if(bt(mSIM->FM1xxuserdata( 2,i+7),j)) {BSbit[k]=1;}}
    for(int j=5; j>=0; j--){ k++; if(bt(mSIM->FM0xxuserdata( 3,i+7),j)) {BSbit[k]=1;}}
    
    for(int j=0; j<3;  j++){ k++; if(bt(mSIM->FM0xxuserdata( 6,i+7),j)) {BSbit[k]=1;}}
    for(int j=0; j<7;  j++){ k++; if(bt(mSIM->FM0xxuserdata( 5,i+7),j)) {BSbit[k]=1;}}
    for(int j=0; j<1;  j++){ k++; if(bt(mSIM->FM1xxuserdata( 3,i+7),j)) {BSbit[k]=1;}}
    for(int j=6; j>=0; j--){ k++; if(bt(mSIM->FM0xxuserdata( 7,i+7),j)) {BSbit[k]=1;}}
    for(int j=2; j>=0; j--){ k++; if(bt(mSIM->FM0xxuserdata( 8,i+7),j)) {BSbit[k]=1;}}
    
    for(int j=0; j<3;  j++){ k++; if(bt(mSIM->FM0xxuserdata(12,i+7),j)) {BSbit[k]=1;}}
    for(int j=0; j<7;  j++){ k++; if(bt(mSIM->FM0xxuserdata(11,i+7),j)) {BSbit[k]=1;}}
    for(int j=0; j<1;  j++){ k++; if(bt(mSIM->FM1xxuserdata( 4,i+7),j)) {BSbit[k]=1;}}
    for(int j=6; j>=0; j--){ k++; if(bt(mSIM->FM0xxuserdata( 9,i+7),j)) {BSbit[k]=1;}}
    for(int j=2; j>=0; j--){ k++; if(bt(mSIM->FM0xxuserdata(10,i+7),j)) {BSbit[k]=1;}}
    
    union u_t{
      int INT[36];
      char DBS[NBSG][NBSG];
    } u;
    for(int i=0; i<36; i++) u.INT[i]=mSIM->FP201userdata(10+i);
    for(int j=0; j<NBSG; j++){
      if(mPrint) printf("** DiBSg ");
      for(int i=0; i<NBSG; i++){
	if(mPrint) printf(" %1s", u.DBS[j][i]?"1":"0");
	if(u.DBS[j][i]) {
	  float w=1.0;
	  if(i==7 && j==10) w=0.2;
	  mDIBSg->Fill(float(i),float(NBSG-j-1),w);
	  //printf("BSbit=");
	  for(int jj=0; jj<NBS; jj++){
	    //printf("%1d",BSbit[jj]); if(BSGRP[jj]!=BSGRP[jj+1]) printf(" ");}
	    if(BSGRP[jj]==j && BSbit[jj]==1){
	      for(int ii=0; ii<NBS; ii++){
		if(BSGRP[ii]==i && BSbit[ii]==1){
		  mDIBS->Fill(float(ii),float(NBS-jj-1));
		}
	      }
	    }
	  }
	  //printf("\n");
	}
      }
      if(mPrint) printf("\n");
    }
  }
}

void StFmsTrgQaMaker::fillDiJp(){
  if(isTrg("FMS-DiJP")){
    union u_t{
      int INT[9];
      char DJp[NJP][NJP];
    } u;
    for(int i=0; i<9; i++) u.INT[i]=mSIM->FP201userdata(50+i);
    for(int j=0; j<NJP; j++){
      if(mPrint) printf("** DiJp ");
      for(int i=0; i<NJP; i++){
	if(mPrint) printf(" %1s", u.DJp[j][i]?"1":"0");
	if(u.DJp[j][i]) mDIJP->Fill(float(i),float(NJP-j-1));
      }
      if(mPrint) printf("\n");
    }  
  }
}

void StFmsTrgQaMaker::fillJP(){
  char trg[20];
  for(int i=0; i<NTHR; i++){
    sprintf(trg,"FMS-JP%1d",2-i);
    if(isTrg(trg)){
      for(int j=0; j<NJP; j++){
	int b= ((mSIM->FP201userdata(i+7)) >> j) & 0x1;
	if(b) mJP[i]->Fill(float(j));
      }
    }
  }
}

void StFmsTrgQaMaker::fillBS(){
  char trg[20];
  int k;
  for(int i=0; i<NTHR; i++){
    sprintf(trg,"FMS-sm-bs%1d",3-i);
    if(isTrg(trg)){
      k=-1;
      for(int j=0; j<6;  j++){ k++; if(bt(mSIM->FM0xxuserdata( 1,i+7),j)) {mBS[i]->Fill(float(k));}}
      for(int j=0; j<1;  j++){ k++; if(bt(mSIM->FM1xxuserdata( 1,i+7),j)) {mBS[i]->Fill(float(k));}}
      for(int j=5; j>=0; j--){ k++; if(bt(mSIM->FM0xxuserdata( 2,i+7),j)) {mBS[i]->Fill(float(k));}}
 
      for(int j=0; j<6 ; j++){ k++; if(bt(mSIM->FM0xxuserdata( 4,i+7),j)) {mBS[i]->Fill(float(k));}}
      for(int j=0; j<1;  j++){ k++; if(bt(mSIM->FM1xxuserdata( 2,i+7),j)) {mBS[i]->Fill(float(k));}}
      for(int j=5; j>=0; j--){ k++; if(bt(mSIM->FM0xxuserdata( 3,i+7),j)) {mBS[i]->Fill(float(k));}}
    }

    sprintf(trg,"FMS-lg-bs%1d",3-i);
    if(isTrg(trg)){
      k=25;
      for(int j=0; j<3;  j++){ k++; if(bt(mSIM->FM0xxuserdata( 6,i+7),j)) {mBS[i]->Fill(float(k));}}
      for(int j=0; j<7;  j++){ k++; if(bt(mSIM->FM0xxuserdata( 5,i+7),j)) {mBS[i]->Fill(float(k));}}
      for(int j=0; j<1;  j++){ k++; if(bt(mSIM->FM1xxuserdata( 3,i+7),j)) {mBS[i]->Fill(float(k));}}
      for(int j=6; j>=0; j--){ k++; if(bt(mSIM->FM0xxuserdata( 7,i+7),j)) {mBS[i]->Fill(float(k));}}
      for(int j=2; j>=0; j--){ k++; if(bt(mSIM->FM0xxuserdata( 8,i+7),j)) {mBS[i]->Fill(float(k));}}
      
      for(int j=0; j<3;  j++){ k++; if(bt(mSIM->FM0xxuserdata(12,i+7),j)) {mBS[i]->Fill(float(k));}}
      for(int j=0; j<7;  j++){ k++; if(bt(mSIM->FM0xxuserdata(11,i+7),j)) {mBS[i]->Fill(float(k));}}
      for(int j=0; j<1;  j++){ k++; if(bt(mSIM->FM1xxuserdata( 4,i+7),j)) {mBS[i]->Fill(float(k));}}
      for(int j=6; j>=0; j--){ k++; if(bt(mSIM->FM0xxuserdata( 9,i+7),j)) {mBS[i]->Fill(float(k));}}
      for(int j=2; j>=0; j--){ k++; if(bt(mSIM->FM0xxuserdata(10,i+7),j)) {mBS[i]->Fill(float(k));}}
    }
  }
}

void StFmsTrgQaMaker::fillJPsum(){
  int sum[NJP];  
  sum[ 0]=mSIM->FP201userdata(0);  
  sum[ 1]=mSIM->FP201userdata(1); 
  sum[ 2]=mSIM->FP201userdata(2);  
  sum[ 3]=mSIM->FP201userdata(3); 
  sum[ 4]=mSIM->FP201userdata(4);  
  sum[ 5]=mSIM->FP201userdata(5); 
  for(int i=0; i<NJP; i++){
    hJP[i]->Fill(float(sum[i]));
  }
  return;
}

void StFmsTrgQaMaker::fillBSsum(){
  int sum[NBS];
  sum[ 0]=mSIM->FM0xxuserdata( 1,0); //ST-A 
  sum[ 1]=mSIM->FM0xxuserdata( 1,1); //ST-B
  sum[ 2]=mSIM->FM0xxuserdata( 1,2); //ST-BC
  sum[ 3]=mSIM->FM0xxuserdata( 1,3); //ST-C
  sum[ 4]=mSIM->FM0xxuserdata( 1,4); //ST-CD
  sum[ 5]=mSIM->FM0xxuserdata( 1,5); //ST-D
  sum[ 6]=mSIM->FM1xxuserdata( 1,0); //SM-DD
  sum[ 7]=mSIM->FM0xxuserdata( 2,5); //SB-D
  sum[ 8]=mSIM->FM0xxuserdata( 2,4); //SB-CD
  sum[ 9]=mSIM->FM0xxuserdata( 2,3); //SB-C
  sum[10]=mSIM->FM0xxuserdata( 2,2); //SB-BC
  sum[11]=mSIM->FM0xxuserdata( 2,1); //SB-B
  sum[12]=mSIM->FM0xxuserdata( 2,0); //SB-A

  sum[13]=mSIM->FM0xxuserdata( 4,0); //NB-A
  sum[14]=mSIM->FM0xxuserdata( 4,1); //NB-B
  sum[15]=mSIM->FM0xxuserdata( 4,2); //NB-BC
  sum[16]=mSIM->FM0xxuserdata( 4,3); //NB-C
  sum[17]=mSIM->FM0xxuserdata( 4,4); //NB-CD
  sum[18]=mSIM->FM0xxuserdata( 4,5); //NB-D
  sum[19]=mSIM->FM1xxuserdata( 2,0); //NM-DD
  sum[20]=mSIM->FM0xxuserdata( 3,5); //NT-D
  sum[21]=mSIM->FM0xxuserdata( 3,4); //NT-CD
  sum[22]=mSIM->FM0xxuserdata( 3,3); //NT-C
  sum[23]=mSIM->FM0xxuserdata( 3,2); //NT-BC
  sum[24]=mSIM->FM0xxuserdata( 3,1); //NT-B
  sum[25]=mSIM->FM0xxuserdata( 3,0); //NT-A

  sum[26]=mSIM->FM0xxuserdata( 6,0); //ST-E  
  sum[27]=mSIM->FM0xxuserdata( 6,1); //ST-EF 
  sum[28]=mSIM->FM0xxuserdata( 6,2); //ST-F  
  sum[29]=mSIM->FM0xxuserdata( 5,0); //ST-G  
  sum[30]=mSIM->FM0xxuserdata( 5,1); //ST-GH 
  sum[31]=mSIM->FM0xxuserdata( 5,2); //ST-H  
  sum[32]=mSIM->FM0xxuserdata( 5,3); //ST-HI 
  sum[33]=mSIM->FM0xxuserdata( 5,4); //ST-I  
  sum[34]=mSIM->FM0xxuserdata( 5,5); //ST-IJ 
  sum[35]=mSIM->FM0xxuserdata( 5,6); //ST-J  
  sum[36]=mSIM->FM1xxuserdata( 3,0); //SM-JJ 
  sum[37]=mSIM->FM0xxuserdata( 7,6); //SB-J  
  sum[38]=mSIM->FM0xxuserdata( 7,5); //SB-IJ 
  sum[39]=mSIM->FM0xxuserdata( 7,4); //SB-I  
  sum[40]=mSIM->FM0xxuserdata( 7,3); //SB-HI 
  sum[41]=mSIM->FM0xxuserdata( 7,2); //SB-H  
  sum[42]=mSIM->FM0xxuserdata( 7,1); //SB-GH 
  sum[43]=mSIM->FM0xxuserdata( 7,0); //SB-G  
  sum[44]=mSIM->FM0xxuserdata( 8,2); //SB-F  
  sum[45]=mSIM->FM0xxuserdata( 8,1); //SB-EF 
  sum[46]=mSIM->FM0xxuserdata( 8,0); //SB-E  

  sum[47]=mSIM->FM0xxuserdata(12,0); //NB-E 
  sum[48]=mSIM->FM0xxuserdata(12,1); //NB-EF
  sum[49]=mSIM->FM0xxuserdata(12,2); //NB-F 
  sum[50]=mSIM->FM0xxuserdata(11,0); //NB-G 
  sum[51]=mSIM->FM0xxuserdata(11,1); //NB-GH
  sum[52]=mSIM->FM0xxuserdata(11,2); //NB-H 
  sum[53]=mSIM->FM0xxuserdata(11,3); //NB-HI
  sum[54]=mSIM->FM0xxuserdata(11,4); //NB-I 
  sum[55]=mSIM->FM0xxuserdata(11,5); //NB-IJ
  sum[56]=mSIM->FM0xxuserdata(11,6); //NB-J 
  sum[57]=mSIM->FM1xxuserdata( 4,0); //NM-JJ
  sum[58]=mSIM->FM0xxuserdata( 9,6); //NT-J 
  sum[59]=mSIM->FM0xxuserdata( 9,5); //NT-IJ
  sum[60]=mSIM->FM0xxuserdata( 9,4); //NT-I 
  sum[61]=mSIM->FM0xxuserdata( 9,3); //NT-HI
  sum[62]=mSIM->FM0xxuserdata( 9,2); //NT-H 
  sum[63]=mSIM->FM0xxuserdata( 9,1); //NT-GH
  sum[64]=mSIM->FM0xxuserdata( 9,0); //NT-G 
  sum[65]=mSIM->FM0xxuserdata(10,2); //NT-F 
  sum[66]=mSIM->FM0xxuserdata(10,1); //NT-EF
  sum[67]=mSIM->FM0xxuserdata(10,0); //NT-E   
  for(int i=0; i<NBS; i++){ 
    hBS[i]->Fill(float(sum[i]));
  }
}

void StFmsTrgQaMaker::readtrgid(){
  int i,yearday=mRun/1000;
  char filename[200],trgn[200];
  sprintf(filename,"%d/%d.trgid",yearday,mRun);
  FILE* fp=fopen(filename,"r");
  if(!fp) {printf("Cannot open %s\n",filename); return; }
  while(!feof(fp)) {
    fscanf(fp,"%d %s",&i,trgn);
    printf("%d %s\n",i,trgn);
    trgname[i]=trgn;
  }
  fclose(fp);
}

int StFmsTrgQaMaker::isTrg(const char* trgn){
  int id=-1;
  unsigned long long one=1;  
  /*
  printf("l2sum=%x   TRG=",mTrgd->l2sum());
  for(int i=0; i<64; i++){
    if(mTrgd->l2sum() & (one << i)) printf("%s ",trgname[i].Data());
  }
  printf("\n");
  */
  for(int i=0; i<64; i++){
    if( trgname[i].EqualTo(trgn) ) {id=i; break;}
  }
  if(id==-1) return 0;
  unsigned long long flag=(mTrgd->l2sum() & (one << id));
  //if(flag) printf("Yes triggered by %s\n",trgn);
  if(flag>0) return 1;
  return 0;
}

void StFmsTrgQaMaker::printTriggers(){
  int id=-1;
  unsigned long long one=1;
  printf("l2sum=%16llx  TRG=",mTrgd->l2sum());
  for(int i=0; i<64; i++){                                                                                                                       
    if(mTrgd->l2sum() & (one << i)) printf("%s ",trgname[i].Data());                                                                                
  }
  printf("\n");                                                                                                                                     
  return;
}

void StFmsTrgQaMaker::countOverlap(){
  static const int NTRG=11;
  const char* tname[NTRG]={"FMS-sm-bs1","FMS-sm-bs2","FMS-sm-bs3",
			   "FMS-lg-bs1","FMS-lg-bs2","FMS-lg-bs3",
			   "FMS-JP0","FMS-JP1","FMS-JP2",
			   "FMS-DiBS","FMS-DiJP"};
  int flag=0;
  for(int i=0; i<NTRG; i++){
    if(isTrg(tname[i])) {count[2]++; flag=1;}
  }
  if(flag==1) count[1]++;
  count[0]++;
}

