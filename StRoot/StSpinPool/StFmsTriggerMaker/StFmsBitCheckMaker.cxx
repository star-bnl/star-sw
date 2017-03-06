#include "StFmsBitCheckMaker.h"
#include "StFmsTriggerMaker.h"
#include "StTriggerData.h"

enum {MAXD=MAXPP*2, MAXDT=MAXD*2+1};

unsigned short TCU;
StFmsTriggerMaker* SIM;
static int PRINTLEVEL;

static int NPRE,NPOST;
static unsigned int N0[12][MAXDT][4][32][4];
static unsigned int N1[4][MAXDT][4][32][4];
static unsigned int N2[MAXDT][4][32][4];
static unsigned int N3[16][4];
static float MM[MAXDT][18];

void printMismatch(int run){
  printf("Run      Mismatch% =  FM001  FM002  FM003  FM004  FM005  FM006  FM007  FM008  FM009  FM010  FM011  FM012 |  FM101  FM102  FM03   FM104 |  FP201 |  TCU   | Average\n");
  printf("%8d mismatch\% = ",run);
  float ave=0.0;
  for(int id=0; id<18; id++) {
    ave+=MM[MAXD][id];
    printf("%6.3f ",MM[MAXD][id]);
    if(id==11 || id==15 || id==16) printf("| ");
  }
  printf("| ave=%6.3f\n",ave/18.0);
  printf("%8.4f AVGMM\n",ave/18.0);
}

void printCount(int id, const char* name, unsigned int n[MAXDT][4][32][4], int run, int layer2=0){  
  int dmin,dmax;
  dmin=-(NPRE+NPOST); dmax=NPRE+NPOST;
  //  if(layer2==0){ dmin=-(NPRE+NPOST); dmax=NPRE+NPOST; }
  //else         { dmin=-NPRE;         dmax=NPOST;      }
  int nt[MAXDT]; 
  int ng[MAXDT]; 
  memset(nt,0,sizeof(nt));
  memset(ng,0,sizeof(ng));
  for(int dt=0; dt<MAXDT; dt++){
    int d=dt-MAXD;
    if(d<dmin || d>dmax) continue;
    printf("%8d %15s : %2d ",run,name,d);
    for(int i=0; i<4; i++){
      for(int j=31; j>=0; j--){
	char a[2];
	int n00=n[dt][i][j][0];
	int n01=n[dt][i][j][1];
	int n10=n[dt][i][j][2];
	int n11=n[dt][i][j][3];	
	nt[dt]+=n00+n01+n10+n11;
	ng[dt]+=n00+n11;
	if(n01==0 && n10==0) {
	  sprintf(a,"."); 
	}else{
	  float r = float(n00+n11)/float(n00+n01+n10+n11)*10.0;
	  sprintf(a,"%1d",(int)r);
	  if(n00==0 && n01==0) sprintf(a,"!");
	  if(n10==0 && n11==0) sprintf(a,"o");
	}	
	printf("%1s",a);
      }
      printf("  ");
    }
    printf("\n");
  }
  printf("%8d %15s MMdeltaT\% = ",run,name); 
  for(int dt=0; dt<MAXDT; dt++){
    int d=dt-MAXD;
    if(d<dmin || d>dmax) continue;
    float mm=float(nt[dt]-ng[dt])/float(nt[dt])*100.0;
    printf("%6.3f ",mm);
    MM[dt][id]=mm;
  }
  printf("\n");
  printf("%8d %15s Mismatch\% = ",run,name); 
  int dt=MAXD;
  int d=dt-MAXD;
  float mm=float(nt[dt]-ng[dt])/float(nt[dt])*100.0;
  printf("%6.3f ",mm);
  MM[dt][id]=mm;
  printf("\n");
}

void printCount3(int id, const char* name, unsigned int n[32][4], int run){  
  int nt=0,ng=0;
  printf("%8d %15s :  0 ",run,name);
  for(int j=15; j>=0; j--){
    char a[2];
    int n00=n[j][0];
    int n01=n[j][1];
    int n10=n[j][2];
    int n11=n[j][3];
    nt+=n00+n01+n10+n11;
    ng+=n00+n11;
    if(n01==0 && n10==0) {
      sprintf(a,".");
    }else{
      float r = float(n00+n11)/float(n00+n01+n10+n11)*10.0;
      sprintf(a,"%1d",(int)r);
      if(n00==0 && n01==0) sprintf(a,"!");
      if(n10==0 && n11==0) sprintf(a,"o");
    }
    printf("%1s",a);
  }
  printf("\n");
  float mm=float(nt-ng)/float(nt)*100.0;
  printf("%8d %15s Mismatch\% = %6.3f\n",run,name,mm); 
  MM[MAXD][id]=mm;
}

void Count(int d1, int d2, int d3, int d4, int s1, int s2, int s3, int s4, unsigned int n[4][32][4]){
  int c;
  for(int i=0; i<32; i++){
    c = ((d1>>i) & 0x1)*2 + ((s1>>i) & 0x1); n[0][i][c]++;
    c = ((d2>>i) & 0x1)*2 + ((s2>>i) & 0x1); n[1][i][c]++;
    c = ((d3>>i) & 0x1)*2 + ((s3>>i) & 0x1); n[2][i][c]++;
    c = ((d4>>i) & 0x1)*2 + ((s4>>i) & 0x1); n[3][i][c]++;
  }  
}

void Count3(int d1, int s1, unsigned int n[32][4]){
  int c;
  for(int i=0; i<16; i++){
    c = ((d1>>i) & 0x1)*2 + ((s1>>i) & 0x1); n[i][c]++;
  }
}

void Comp0(const char* name, int slot, int num, int t1, int t2, int run){
  int x1=t1-MAXPP;
  int x2=t2-MAXPP;
  /*
  unsigned int d1=getDSM(slot,0);
  unsigned int d2=getDSM(slot,1);
  unsigned int d3=getDSM(slot,2);
  unsigned int d4=getDSM(slot,3); 
  */
  unsigned int d1=SIM->FM0xxdata(num,0,t1);
  unsigned int d2=SIM->FM0xxdata(num,1,t1);
  unsigned int d3=SIM->FM0xxdata(num,2,t1);
  unsigned int d4=SIM->FM0xxdata(num,3,t1);
  unsigned int s1=SIM->FM0xxinput(num,0,t2);
  unsigned int s2=SIM->FM0xxinput(num,1,t2);
  unsigned int s3=SIM->FM0xxinput(num,2,t2);
  unsigned int s4=SIM->FM0xxinput(num,3,t2);
  if(PRINTLEVEL>0){
    printf("%8d %15s | %2d %2d %2d | %08x %08x %08x %08x | %08x %08x %08x %08x | %08x %08x %08x %08x\n",
	   run,name,x1-x2,x1,x2,
	   d1,d2,d3,d4,
	   s1,s2,s3,s4,
	   d1^s1, d2^s2, d3^s3, d4^s4);
  }
  Count(d1,d2,d3,d4,s1,s2,s3,s4,N0[num-1][x1-x2+MAXD]);
}

void Comp1(const char* name, int slot, int num, int t1, int t2, int run){
  int x1=t1-MAXPP;
  int x2=t2-MAXPP;
  /*
  unsigned int d1=getDSM(slot,0);
  unsigned int d2=getDSM(slot,1);
  unsigned int d3=getDSM(slot,2);
  unsigned int d4=getDSM(slot,3); 
  */
  unsigned int d1=SIM->FM1xxdata(num,0,t1);
  unsigned int d2=SIM->FM1xxdata(num,1,t1);
  unsigned int d3=SIM->FM1xxdata(num,2,t1);
  unsigned int d4=SIM->FM1xxdata(num,3,t1);
  unsigned int s1=SIM->FM1xxinput(num,0,t2);
  unsigned int s2=SIM->FM1xxinput(num,1,t2);
  unsigned int s3=SIM->FM1xxinput(num,2,t2);
  unsigned int s4=SIM->FM1xxinput(num,3,t2);
  if(PRINTLEVEL>0)
    printf("%8d %15s | %2d %2d %2d | %08x %08x %08x %08x | %08x %08x %08x %08x | %08x %08x %08x %08x\n",
	   run,name,x1-x2,x1,x2,
	   d1,d2,d3,d4,
	   s1,s2,s3,s4,
	   d1^s1, d2^s2, d3^s3, d4^s4);
  Count(d1,d2,d3,d4,s1,s2,s3,s4,N1[num-1][x1-x2+MAXD]);
}

void Comp2(const char* name, int t2, int run){
  int x1=0;
  int x2=t2-MAXPP;
  /*
  unsigned int d1=getDSM(16,0);
  unsigned int d2=getDSM(16,1);
  unsigned int d3=getDSM(16,2);
  unsigned int d4=getDSM(16,3);
  */
  unsigned int d1=SIM->FP201data(0);
  unsigned int d2=SIM->FP201data(1);
  unsigned int d3=SIM->FP201data(2);
  unsigned int d4=SIM->FP201data(3);
  unsigned int s1=SIM->FP201input(0,t2);
  unsigned int s2=SIM->FP201input(1,t2);
  unsigned int s3=SIM->FP201input(2,t2);
  unsigned int s4=SIM->FP201input(3,t2);
  if(PRINTLEVEL>0)
    printf("%8d %15s | %2d %2d %2d | %08x %08x %08x %08x | %08x %08x %08x %08x | %08x %08x %08x %08x\n",
	   run,name,x1-x2,x1,x2,
	   d1,d2,d3,d4,
	   s1,s2,s3,s4,
	   d1^s1, d2^s2, d3^s3, d4^s4);
  Count(d1,d2,d3,d4,s1,s2,s3,s4,N2[x1-x2+MAXD]);
}

void Comp3(const char* name, int run){
  unsigned int d1=TCU;
  unsigned int s1=SIM->FP201output();
  int jp0=SIM->FP201userdata(1);
  if(PRINTLEVEL>0) {
    printf("%8d %15s | %04x / %04x | %04x  DiJp=",
	   run,name,
	   d1,s1,d1^s1);
    for(int i=5; i>=0; i--) printf("%1x",(jp0>>i)&0x1);  
    printf("\n");
  }
  Count3(d1,s1,N3);
}

ClassImp(StFmsBitCheckMaker);

StFmsBitCheckMaker::StFmsBitCheckMaker(const char* name): StMaker(name), mPrint(0) {
}

void StFmsBitCheckMaker::Clear(Option_t* option){
}

int StFmsBitCheckMaker::Init(){
  memset(N0,0,sizeof(N0));
  memset(N1,0,sizeof(N1));
  memset(N2,0,sizeof(N2));
  memset(N3,0,sizeof(N3));
  PRINTLEVEL=mPrint;
  return kStOk;
}

int StFmsBitCheckMaker::InitRun(int runNumber){
  return kStOK;
}

int StFmsBitCheckMaker::Finish(){
  printf("Run      Name            Mismatch%   Data Xing - Previous layer&Emulation xing\n");
  printCount(0,"QT1/DCBA=>FM001",N0[0],mRun);
  printCount(1,"QT2/DCBA=>FM002",N0[1],mRun);
  printCount(2,"QT3/DCBA=>FM003",N0[2],mRun);
  printCount(3,"QT4/DCBA=>FM004",N0[3],mRun);
  printCount(4,"QT1/JIHG=>FM005",N0[4],mRun);
  printCount(5,"QT1/FE  =>FM006",N0[5],mRun);
  printCount(6,"QT2/JIHG=>FM007",N0[6],mRun);
  printCount(7,"QT2/FE  =>FM008",N0[7],mRun);
  printCount(8,"QT3/JIHG=>FM009",N0[8],mRun);
  printCount(9,"QT3/FE  =>FM010",N0[9],mRun);
  printCount(10,"QT4/JIHG=>FM011",N0[10],mRun);
  printCount(11,"QT4/FE  =>FM012",N0[11],mRun);
  printCount(12,"FM001/2 =>FM101", N1[0],mRun);
  printCount(13,"FM003/4 =>FM102", N1[1],mRun);
  printCount(14,"FM005/8 =>FM103", N1[2],mRun);
  printCount(15,"FM09/12 =>FM104", N1[3],mRun);
  printCount(16,"FM101/4 =>FP201", N2,mRun,1);    
  printCount3(17,"FP201   =>TCU  ",N3,mRun);
  printMismatch(mRun);
}

int StFmsBitCheckMaker::Make(){
  StTriggerData *td = (StTriggerData*)GetDataSet("StTriggerData")->GetObject();  
  if(!td) {printf("No StTriggerData found\n"); return kStErr;}
  SIM=(StFmsTriggerMaker*)GetMaker("fmstrigger");
  if(!SIM) {printf("No StFmsTriggerMaker found\n"); return kStErr;}
  
  //unsigned int detmask=td->getTrgDetMask();
  unsigned int cratemask=td->getTrgCrateMask();
  //printf("TrgDetMask=%8x TrgCrateMask=%8x\n",detmask,cratemask);
  //if((cratemask>>FMS_CONF_NUM) & 0x1) printf("FMS ");
  //if((cratemask>>QT1_CONF_NUM) & 0x1) printf("QT1 ");
  //if((cratemask>>QT2_CONF_NUM) & 0x1) printf("QT2 ");
  //if((cratemask>>QT3_CONF_NUM) & 0x1) printf("QT3 ");
  //if((cratemask>>QT4_CONF_NUM) & 0x1) printf("QT4 ");
  //printf("\n");
  if(! (cratemask>>FMS_CONF_NUM) & 0x1) return kStOK;
  if(! (cratemask>>QT1_CONF_NUM) & 0x1) return kStOK;
  if(! (cratemask>>QT2_CONF_NUM) & 0x1) return kStOK;
  if(! (cratemask>>QT3_CONF_NUM) & 0x1) return kStOK;
  if(! (cratemask>>QT4_CONF_NUM) & 0x1) return kStOK;

  static int dbc0 = -999;
  unsigned long long bxinghi,bxing1,bxinglo,bx,bxdiff;
  bxinghi = td->bcData(3);
  bxing1 =  td->bcData(10);
  bxinglo = (bxing1 << 16) + td->bcData(11);
  bx = (bxinghi << 32) + bxinglo;
  int bunchid7  = td->bunchId7Bit();
  int bunchid48 = td->bunchId48Bit();
  if(bx%120 != bunchid48)
    printf("BC48 check :  (BX from BCdata)\%120=%d is not consistent with  bunchId48Bit=%d\n",bx%120,bunchid48);
  int dbc = bunchid7-bunchid48;
  if(dbc<0) dbc+=120;
  if(dbc0==-999) dbc0=dbc;
  int dd=dbc-dbc0;
  int bc48c=bunchid48+dbc0; if(bc48c<0) bc48c+=120;  if(bc48c>119)bc48c-=120;
  if(dd!=0)
    printf("BC7: bc7=%3d bc48%120=%3d bc48corr=%3d D=%3d D0=%3d DD=%3d BC48=%llx\n",bunchid7,bunchid48,bc48c,dbc,dbc0,dd,bx);
  //if(td->bcData(1)>0)
  if(td->bcData(12)&0x1==1 && bunchid7!=1) {
    //if(bunchid7<4 || bunchid7>117)
    printf("Revtick : bc7=%3d %3d RT1=%x %x %x %x %x %x RT2=%1d BC48=%llx\n",
	   bunchid7,td->bcData(2)&0x7f,
	   td->bcData(1),
	   td->bcData(0),
	   td->bcData(4),
	   td->bcData(5),
	   td->bcData(6),
	   td->bcData(7),
	   td->bcData(12)&0x1,bx);
  }

  //FMS=td->getDsm_FMS();  
  //for(int i=0; i<8; i++) {FP201[i]=td->fpdLayer2DSMRaw(i);} // printf("FP201 %d %d\n",i,td->fpdLayer2DSMRaw(i));}
  TCU = td->lastDSM(5);
  NPRE=td->numberOfPreXing();
  NPOST=td->numberOfPostXing();

  //QT -> layer0
  for(int t1=0; t1<MAXT; t1++){ //data xing loop
    int x1=t1-MAXPP;
    if(-x1>NPRE || x1>NPOST) continue;
    for(int t2=0; t2<MAXT; t2++){ //sim xing loop
      int x2=t2-MAXPP;
      if(-x2>NPRE || x2>NPOST) continue;
      Comp0("QT1/DCBA=>FM001",0,1,t1,t2,mRun);
      Comp0("QT2/DCBA=>FM002",1,2,t1,t2,mRun);
      Comp0("QT3/DCBA=>FM003",3,3,t1,t2,mRun);
      Comp0("QT4/DCBA=>FM004",4,4,t1,t2,mRun);
      Comp0("QT1/JIHG=>FM005",6,5,t1,t2,mRun);
      Comp0("QT1/FE  =>FM006",7,6,t1,t2,mRun);
      Comp0("QT2/JIHG=>FM007",8,7,t1,t2,mRun);
      Comp0("QT2/FE  =>FM008",9,8,t1,t2,mRun);
      Comp0("QT3/JIHG=>FM009",11,9,t1,t2,mRun);
      Comp0("QT3/FE  =>FM010",12,10,t1,t2,mRun);
      Comp0("QT4/JIHG=>FM011",13,11,t1,t2,mRun);
      Comp0("QT4/FE  =>FM012",14,12,t1,t2,mRun);
      
      //Layer0->Layer1
      Comp1("FM001/2 =>FM101", 2,1,t1,t2,mRun);
      Comp1("FM003/4 =>FM102", 5,2,t1,t2,mRun);
      Comp1("FM005/8 =>FM103",10,3,t1,t2,mRun);
      Comp1("FM09/12 =>FM104",15,4,t1,t2,mRun);
      
      //Layer1->Layer2
      if(t1==MAXPP) Comp2("FM101/4 =>FP201",t2,mRun);      
      
      //Layer2->TCU
      if(t1==MAXPP && t2==MAXPP) Comp3("FP201   =>TCU",mRun);
    }
  }
  PRINTLEVEL--;
  return kStOk;
}

