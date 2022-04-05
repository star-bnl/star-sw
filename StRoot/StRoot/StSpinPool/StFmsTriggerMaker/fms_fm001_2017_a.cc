#include "bits.hh"
#include "qt32b_fms_2015_a.hh"
#include "fms_fm001_2017_a.hh"
#include <stdio.h>

//#include "registerHack.hh"

void fms_fm001_2017_a(Board& fm001, int t, int simdat){
  // thresholds
  const int BSThr1    = fm001.registers[0];
  const int BSThr2    = fm001.registers[1];
  const int BSThr3    = fm001.registers[2];
  const int BitSelect = fm001.registers[3];
  //Hack until we know details of registers
  //int BSThr1=Sm_BSThr1; 
  //int BSThr2=Sm_BSThr2; 
  //int BSThr3=Sm_BSThr3; 
  //int BitSelect=Sm_BitSelect;

  //input
  int* in;
  if(simdat==0) {in=(int*)fm001.channels[t];}
  else          {in=(int*)fm001.dsmdata[t];}
  int A=in[3];
  int B=in[2];
  int C=in[1];
  int D=in[0];

  //2x8 sums from QT
  int A01 = getQT01Sum(A);
  int A23 = getQT23Sum(A);
  int B01 = getQT01Sum(B);
  int B23 = getQT23Sum(B);
  int C01 = getQT01Sum(C);
  int C23 = getQT23Sum(C);
  int D01 = getQT01Sum(D);
  int D23 = getQT23Sum(D);

  // Form 2x4 board sums 
  const int MAX=6; 
  int bs[MAX];
  bs[0] = A01 + A23; //A
  bs[1] = B01 + B23; //B
  bs[2] = B23 + C01; //BC
  bs[3] = C01 + C23; //C
  bs[4] = C23 + D01; //CD
  bs[5] = D01 + D23; //D
  fm001.userdata[t][0]=bs[0];
  fm001.userdata[t][1]=bs[1];
  fm001.userdata[t][2]=bs[2];
  fm001.userdata[t][3]=bs[3];
  fm001.userdata[t][4]=bs[4];
  fm001.userdata[t][5]=bs[5];

  //Compare BS to thresholds
  int BS3=0, BS2=0;
  fm001.userdata[t][7]=0;
  fm001.userdata[t][8]=0;
  fm001.userdata[t][9]=0;
  for(int i=0; i<MAX; i++){
    if(bs[i]>BSThr3) {BS3=1; fm001.userdata[t][7]+=(1<<i); }
    if(bs[i]>BSThr2) {BS2=1; fm001.userdata[t][8]+=(1<<i); }
    if(bs[i]>BSThr1) {       fm001.userdata[t][9]+=(1<<i); }
  }
  int BS1ABC=(bs[0]>BSThr1) | (bs[1]>BSThr1) | (bs[2]>BSThr1) | (bs[3]>BSThr1);
  int BS1CD=(bs[4]>BSThr1) | (bs[5]>BSThr1);

  //Jp Sum
  int JpAB = (bs[0] + bs[1])>>BitSelect;  //A+B
  int JpCD = (bs[3] + bs[5])>>BitSelect;  //C+D
  if(JpAB>0xFF) JpAB=0xFF;
  if(JpCD>0xFF) JpCD=0xFF;

  // Output 
  fm001.output[t]
    = BS3          | BS2    << 1
    | BS1CD  << 2 | BS1ABC   << 3 
    | D23   << 4
    | JpAB  << 16  | JpCD   << 24;

  
  if(PRINT){
    printf("%s input A=%08x B=%08x C=%08x D=%08x\n",fm001.name,A,B,C,D); 
    printf("%s out=%08x BS3=%1d BS2=%1d BS1ABC/CD=%1d %1d sum=%4d %4d %4d %4d %4d %4d Jp=%3d %3d\n",
	   fm001.name,fm001.output[t],BS3,BS2,BS1ABC,BS1CD,
	   bs[0],bs[1],bs[2],bs[3],bs[4],bs[5],JpAB,JpCD);
  }
}

int getFM001_2017a_BS3(int out)    {return getbits(out, 0, 1);}
int getFM001_2017a_BS2(int out)    {return getbits(out, 1, 1);}
int getFM001_2017a_BS1CD(int out) {return getbits(out, 2, 1);}
int getFM001_2017a_BS1ABC(int out)   {return getbits(out, 3, 1);}
int getFM001_2017a_D23(int out)    {return getbits(out, 4,12);}
int getFM001_2017a_JpAB(int out)   {return getbits(out,16, 8);}
int getFM001_2017a_JpCD(int out)   {return getbits(out,24, 8);}
