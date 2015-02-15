#include "StFmsBitCheckMaker.h"
#include "StFmsTriggerMaker.h"
#include "StTriggerData.h"

unsigned char* FMS;
unsigned short FP201[8];
unsigned short TCU;
StFmsTriggerMaker* SIM;

unsigned int N0[12][4][32][4];
unsigned int N1[4][4][32][4];
unsigned int N2[4][32][4];
unsigned int N3[16][4];

unsigned int getDSM(int slot, int ch){
  static const int chadd[4]={7,3,15,11};
  static const int chadd2[4]={3,1,7,5};
  if(slot<16){
    int add=slot*16+chadd[ch];
    return FMS[add] + (FMS[add-1]<<8) + (FMS[add-2]<<16) + (FMS[add-3]<<24);
  }else if(slot==16){
    int add=chadd2[ch];
    return FP201[add] + (FP201[add-1]<<16);
  }
  return 0;
}

void printCount(const char* name, unsigned int n[4][32][4]){  
  printf("%15s : ",name);
  for(int i=0; i<4; i++){
    for(int j=31; j>=0; j--){
      char a[2];
      if(n[i][j][1]==0 && n[i][j][2]==0) {
	sprintf(a,"."); 
      }else{
	float r = float(n[i][j][0] + n[i][j][3])/float(n[i][j][0] + n[i][j][1] + n[i][j][2] + n[i][j][3])*10.0;
	sprintf(a,"%1d",(int)r);
	if(n[i][j][0]==0 && n[i][j][1]==0) sprintf(a,"!");
	if(n[i][j][2]==0 && n[i][j][3]==0) sprintf(a,"o");
      }	
      printf("%1s",a);
    }
    printf("  ");
  }
  printf("\n");
}

void printCount3(const char* name, unsigned int n[32][4]){  
  printf("%15s : ",name);
  for(int j=15; j>=0; j--){
    char a[2];
    if(n[j][1]==0 && n[j][2]==0) {
      sprintf(a,"."); 
    }else{
      float r = float(n[j][0] + n[j][3])/float(n[j][0] + n[j][1] + n[j][2] + n[j][3])*10.0;
      sprintf(a,"%1d",(int)r);
      if(n[j][0]==0 && n[j][1]==0) sprintf(a,"!");
      if(n[j][2]==0 && n[j][3]==0) sprintf(a,"o");
    }	
    printf("%1s",a);
  }
  printf("\n");
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

void Comp0(const char* name, int slot, int num){
  unsigned int d1=getDSM(slot,0);
  unsigned int d2=getDSM(slot,1);
  unsigned int d3=getDSM(slot,2);
  unsigned int d4=getDSM(slot,3); 
  unsigned int s1=SIM->FM0xxinput(num,0);
  unsigned int s2=SIM->FM0xxinput(num,1);
  unsigned int s3=SIM->FM0xxinput(num,2);
  unsigned int s4=SIM->FM0xxinput(num,3);
  printf("%15s | %08x %08x %08x %08x / %08x %08x %08x %08x | %08x %08x %08x %08x\n",name,
	 d1,d2,d3,d4,
	 s1,s2,s3,s4,
	 d1^s1, d2^s2, d3^s3, d4^s4);
  Count(d1,d2,d3,d4,s1,s2,s3,s4,N0[num-1]);
}

void Comp1(const char* name, int slot, int num){
  unsigned int d1=getDSM(slot,0);
  unsigned int d2=getDSM(slot,1);
  unsigned int d3=getDSM(slot,2);
  unsigned int d4=getDSM(slot,3); 
  unsigned int s1=SIM->FM1xxinput(num,0);
  unsigned int s2=SIM->FM1xxinput(num,1);
  unsigned int s3=SIM->FM1xxinput(num,2);
  unsigned int s4=SIM->FM1xxinput(num,3);
  printf("%15s | %08x %08x %08x %08x / %08x %08x %08x %08x | %08x %08x %08x %08x\n",name,
	 d1,d2,d3,d4,
	 s1,s2,s3,s4,
	 d1^s1, d2^s2, d3^s3, d4^s4);
  Count(d1,d2,d3,d4,s1,s2,s3,s4,N1[num-1]);
}

void Comp2(const char* name){
  unsigned int d1=getDSM(16,0);
  unsigned int d2=getDSM(16,1);
  unsigned int d3=getDSM(16,2);
  unsigned int d4=getDSM(16,3);
  unsigned int s1=SIM->FP201input(0);
  unsigned int s2=SIM->FP201input(1);
  unsigned int s3=SIM->FP201input(2);
  unsigned int s4=SIM->FP201input(3);
  printf("%15s | %08x %08x %08x %08x / %08x %08x %08x %08x | %08x %08x %08x %08x\n",name,
         d1,d2,d3,d4,
         s1,s2,s3,s4,
         d1^s1, d2^s2, d3^s3, d4^s4);
  Count(d1,d2,d3,d4,s1,s2,s3,s4,N2);
}

void Comp3(const char* name){
  unsigned int d1=TCU;
  unsigned int s1=SIM->FP201output();
  printf("%15s | %04x / %04x | %04x\n",name,
         d1,s1,d1^s1);
  Count3(d1,s1,N3);
}

ClassImp(StFmsBitCheckMaker);

StFmsBitCheckMaker::StFmsBitCheckMaker(const char* name): StMaker(name){
}

void StFmsBitCheckMaker::Clear(Option_t* option){
}

int StFmsBitCheckMaker::Init(){
  memset(N0,0,sizeof(N0));
  memset(N1,0,sizeof(N1));
  memset(N2,0,sizeof(N2));
  memset(N3,0,sizeof(N3));
  return kStOk;
}

int StFmsBitCheckMaker::InitRun(int runNumber){
  return kStOK;
}

int StFmsBitCheckMaker::Finish(){
  printCount("QT1/ABCD=>FM001",N0[0]);
  printCount("QT2/ABCD=>FM002",N0[1]);
  printCount("QT3/ABCD=>FM003",N0[2]);
  printCount("QT4/ABCD=>FM004",N0[3]);
  printCount("QT1/GHIJ=>FM005",N0[4]);
  printCount("QT1/EF  =>FM006",N0[5]);
  printCount("QT2/GHIJ=>FM007",N0[6]);
  printCount("QT2/EF  =>FM008",N0[7]);
  printCount("QT3/GHIJ=>FM009",N0[8]);
  printCount("QT3/EF  =>FM010",N0[9]);
  printCount("QT4/GHIJ=>FM011",N0[10]);
  printCount("QT4/EF  =>FM012",N0[11]);

  //Layer0->Layer1                                                                                                                                                    
  printCount("FM001/2 =>FM101", N1[0]);
  printCount("FM003/4 =>FM102", N1[1]);
  printCount("FM005/8 =>FM103", N1[2]);
  printCount("FM09/12 =>FM104", N1[3]);

  //Layer1->Layer2                                                                                                                                                   
  printCount("FM101/4 =>FP201", N2);

  //Layer2->TCU                                                                                                                                                   
  printCount3("FP201   =>TCU  ", N3);

}

int StFmsBitCheckMaker::Make(){
  StTriggerData *td = (StTriggerData*)GetDataSet("StTriggerData")->GetObject();  
  if(!td) {printf("No StTriggerData found\n"); return kStErr;}
  SIM=(StFmsTriggerMaker*)GetMaker("fmstrigger");
  if(!SIM) {printf("No StFmsTriggerMaker found\n"); return kStErr;}
  
  unsigned long long bxinghi,bxing1,bxinglo,bx,bxdiff;
  static unsigned long long bxkeep=0;
  bxinghi = td->bcData(3);
  bxing1 =  td->bcData(10);
  bxinglo = (bxing1 << 16) + td->bcData(11);
  bx = (bxinghi << 32) + bxinglo;
  bxdiff=bx-bxkeep;
  printf("Year=%d Version=%x Event=%d Token=%d BC=%lld BCDiff=%lld\n",td->year(),td->version(),td->eventNumber(),td->token(),bx,bxdiff);
  bxkeep=bx;

  FMS=td->getDsm_FMS();  
  for(int i=0; i<8; i++) {FP201[i]=td->fpdLayer2DSMRaw(i); printf("FP201 %d %d\n",i,td->fpdLayer2DSMRaw(i));}
  TCU = td->lastDSM(5);
  //QT -> layer0
  Comp0("QT1/ABCD=>FM001",0,1);
  Comp0("QT2/ABCD=>FM002",1,2);
  Comp0("QT3/ABCD=>FM003",3,3);
  Comp0("QT4/ABCD=>FM004",4,4);
  Comp0("QT1/GHIJ=>FM005",6,5);
  Comp0("QT1/EF  =>FM006",7,6);
  Comp0("QT2/GHIJ=>FM007",8,7);
  Comp0("QT2/EF  =>FM008",9,8);
  Comp0("QT3/GHIJ=>FM009",11,9);
  Comp0("QT3/EF  =>FM010",12,10);
  Comp0("QT4/GHIJ=>FM011",13,11);
  Comp0("QT4/EF  =>FM012",14,12);

  //Layer0->Layer1
  Comp1("FM001/2 =>FM101", 2,1);
  Comp1("FM003/4 =>FM102", 5,2);
  Comp1("FM005/8 =>FM103",10,3);
  Comp1("FM09/12 =>FM104",15,4);

  //Layer1->Layer2
  Comp2("FM101/4 =>FP201");

  //Layer2->TCU
  Comp3("FP201   =>TCU  ");

  return kStOk;
}

