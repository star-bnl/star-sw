#include "bits.hh"
#include "l1_fp201_2017_a.hh"
#include "fms_fm101_2017_a.hh"
#include "fms_fm101_2017_a.hh"
#include "fms_fm103_2017_a.hh"
#include <stdio.h>

//#include "registerHack.hh"

static const int NBITBS=12;
static const int tableBS[NBITBS][NBITBS]={
  {0,0,0,0,0,0,0,0,0,0,0,0}, //SmST	 
  {0,0,0,0,0,0,0,0,0,0,0,0}, //SmSM	 
  {1,0,0,0,0,0,0,0,0,0,0,0}, //SmSB	 
  {1,1,0,0,0,0,0,0,0,0,0,0}, //SmNB	 
  {1,1,1,0,0,0,0,0,0,0,0,0}, //SmNM	 
  {0,1,1,1,0,0,0,0,0,0,0,0}, //SmNT	 
  {0,0,1,1,1,0,0,0,0,0,0,0}, //LgST	 
  {0,0,0,1,1,1,0,0,0,0,0,0}, //LgSM	 
  {1,0,0,0,1,1,1,0,0,0,0,0}, //LgSB	 
  {1,1,0,0,0,1,1,1,0,0,0,0}, //LgNB	 
  {1,1,1,0,0,0,1,1,1,0,0,0}, //LgNM	 
  {0,1,1,1,0,0,0,1,1,1,0,0}};//LgNT    
// Small       Large
// South North South North
// T M B B M T T M B B M T 

static const int NBITJp=6;
static const int tableJp[NBITJp][NBITJp]={
  {0,0,0,1,1,0}, //ST	 
  {0,0,0,1,1,1}, //SM	 
  {0,0,0,0,1,1}, //SB	 
  {1,1,0,0,0,0}, //NB	 
  {1,1,1,0,0,0}, //NM	 
  {0,1,1,0,0,0}};//NT	 
// South North
// T M B B M T
   
void l1_fp201_2017_a(Board& fp201, int t, int simdat){
  const int JpThr0=fp201.registers[0];
  const int JpThr1=fp201.registers[1];
  const int JpThr2=fp201.registers[2];
  //Hack until we know details of registers
  //int JpThr0=JPthr0; 
  //int JpThr1=JPthr1; 
  //int JpThr2=JPthr2; 

  //input
  int* in;
  if(simdat==0) {in=(int*)fp201.channels[t];}
  else          {in=(int*)fp201.dsmdata[t];}
  int fm101 = in[3];   // small cells south
  int fm102 = in[0];   // small cells north
  int fm103 = in[1];   // large cells south
  int fm104 = in[2];   // large cells north
  
  //BS
  int smBS3 = getFM101_2017a_BS3(fm101) | getFM101_2017a_BS3(fm102);
  int lgBS3 = getFM103_2017a_BS3(fm103) | getFM103_2017a_BS3(fm104);
  int smBS2 = getFM101_2017a_BS2(fm101) | getFM101_2017a_BS2(fm102);
  int lgBS2 = getFM103_2017a_BS2(fm103) | getFM103_2017a_BS2(fm104);
  int smBS1 = getFM101_2017a_BS1T(fm101) | getFM101_2017a_BS1M(fm101) | getFM101_2017a_BS1B(fm101)
            | getFM101_2017a_BS1T(fm102) | getFM101_2017a_BS1M(fm102) | getFM101_2017a_BS1B(fm102);
  int lgBS1 = getFM103_2017a_BS1T(fm103) | getFM103_2017a_BS1M(fm103) | getFM103_2017a_BS1B(fm103)
            | getFM103_2017a_BS1T(fm104) | getFM103_2017a_BS1M(fm104) | getFM103_2017a_BS1B(fm104);

  //DiBS
  int DiBS=0;
  union u_t{
    int INT[36];
    char DBS[NBITBS][NBITBS];
  } u;
  memset(u.INT,0,sizeof(u.INT));
  int bs1 
    = (getFM101_2017a_BS1T(fm101)<<0) | (getFM101_2017a_BS1M(fm101)<<1) | (getFM101_2017a_BS1B(fm101)<<2)
    | (getFM101_2017a_BS1B(fm102)<<3) | (getFM101_2017a_BS1M(fm102)<<4) | (getFM101_2017a_BS1T(fm102)<<5)
    | (getFM103_2017a_BS1T(fm103)<<6) | (getFM103_2017a_BS1M(fm103)<<7) | (getFM103_2017a_BS1B(fm103)<<8)
    | (getFM103_2017a_BS1B(fm104)<<9) | (getFM103_2017a_BS1M(fm104)<<10)| (getFM103_2017a_BS1T(fm104)<<11);
  for(int i=0; i<NBITBS; i++){
    if(btest(bs1,i)){
      for(int j=i+1; j<NBITBS; j++){
	if(tableBS[j][i] & btest(bs1,j)) {u.DBS[j][i]=1; DiBS=1;}
      }      
    }
  }
  for(int i=0; i<36; i++) fp201.userdata[t][10+i]=u.INT[i];

  //Jp
  int jp[NBITJp];
  jp[0] = getFM101_2017a_JpT(fm101) + getFM103_2017a_JpT(fm103); //ST
  jp[1] = getFM101_2017a_JpM(fm101) + getFM103_2017a_JpM(fm103); //SM
  jp[2] = getFM101_2017a_JpB(fm101) + getFM103_2017a_JpB(fm103); //SB  
  jp[3] = getFM101_2017a_JpB(fm102) + getFM103_2017a_JpB(fm104); //NB
  jp[4] = getFM101_2017a_JpM(fm102) + getFM103_2017a_JpM(fm104); //NM
  jp[5] = getFM101_2017a_JpT(fm102) + getFM103_2017a_JpT(fm104); //NT
  fp201.userdata[t][0]=jp[0];
  fp201.userdata[t][1]=jp[1];
  fp201.userdata[t][2]=jp[2];
  fp201.userdata[t][3]=jp[3];
  fp201.userdata[t][4]=jp[4];
  fp201.userdata[t][5]=jp[5];

  int JP2=0, JP1=0, JP0=0, jp0=0;
  fp201.userdata[t][7]=0;
  fp201.userdata[t][8]=0;
  fp201.userdata[t][9]=0;
  for(int i=0; i<NBITJp; i++){
    if(jp[i]>JpThr2) {JP2=1; fp201.userdata[t][7]+=(1<<i); }
    if(jp[i]>JpThr1) {JP1=1; fp201.userdata[t][8]+=(1<<i); }
    if(jp[i]>JpThr0) {JP0=1; fp201.userdata[t][9]+=(1<<i); jp0+=(1<<i);}
  }

  //DiJp
  int DiJp=0;
  union u2_t{
    int INT[9];
    char DJp[NBITJp][NBITJp];
  } u2;
  memset(u2.INT,0,sizeof(u2.INT));
  for(int i=0; i<NBITJp-1; i++){
    if(btest(jp0,i)){
      for(int j=i+1; j<NBITJp; j++){
        if(tableJp[j][i] & btest(jp0,j)) {u2.DJp[j][i]=1; DiJp=1;}
      }
    }
  }
  for(int i=0; i<9; i++) fp201.userdata[t][50+i]=u2.INT[i];

  fp201.output[t]
    = smBS3<<0 | smBS2<<1 | smBS1<<2 
    | lgBS3<<3 | lgBS2<<4 | lgBS1<<5
    | DiBS<<6
    | JP2<<7 | JP1<<8 | JP0<<9 
    | DiJp<<10;
  
  //if(1){
  if(PRINT){
    printf("%s input FM101=%08x FM102=%08x FM103=%08x FM104=%08x\n",fp201.name,fm101,fm102,fm103,fm104); 
    printf("%s out=%08x smBS3/2/1=%1d %1d %1d lgBS3/2/1=%1d %1d %1d DiBS=%1d JP=%1d %1d %1d DiJp=%1d\n",
	   fp201.name,fp201.output[t],
	   smBS3,smBS2,smBS1,
	   lgBS3,lgBS2,lgBS1,
	   DiBS,
	   JP2,JP1,JP0,
	   DiJp);
  }

  if(PRINT){
  //if(DiBS){
    printf("%s DiBS bs1=0x%03x=",fp201.name,bs1);
    for(int i=NBITBS-1; i>=0; i--) printf("%1x",btest(bs1,i));
    printf("\n"); 
    for(int j=0; j<NBITBS; j++){
      printf("DiBS %2d : ",j);
      for(int i=0; i<=j; i++){
	printf(" %1s", u.DBS[j][i]?"1":"0");
      }
      printf("\n");
    }
  }

  if(PRINT){
  //if(1){
    printf("%s DiJp jp1=0x%02x=",fp201.name,jp0);
    for(int i=NBITJp-1; i>=0; i--) printf("%1x",btest(jp0,i));
    printf("\n");
    for(int j=0; j<NBITJp; j++){
      printf("DiJp ");
      for(int i=0; i<NBITJp; i++){
	printf(" %1s", u2.DJp[j][i]?"1":"0");
      }
      printf("\n");
    }
  }
}


