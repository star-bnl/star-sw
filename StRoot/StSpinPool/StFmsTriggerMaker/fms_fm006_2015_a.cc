#include "bits.hh"
#include "qt32b_fms_2015_a.hh"
#include "fms_fm006_2015_a.hh"
#include <stdio.h>

#include "registerHack.hh"

void fms_fm006_2015_a(Board& fm006, int t, int simdat){
  // thresholds
  int BSThr1   = fm006.registers[0];
  int BSThr2   = fm006.registers[1];
  int BSThr3   = fm006.registers[2];
  int BitSelect= fm006.registers[3];
  //Hack until we know details of registers
  //int BSThr1=Lg_BSThr1;
  //int BSThr2=Lg_BSThr2;
  //int BSThr3=Lg_BSThr3;
  //int BitSelect=Lg_BitSelect;
  
  //input
  int* in;
  if(simdat==0) {in=(int*)fm006.channels[t];}
  else          {in=(int*)fm006.dsmdata[t];}
  int E=in[1];
  int F=in[0];

  //2x8 sums from QT
  int E01 = getQT01Sum(E);
  int E23 = getQT23Sum(E);
  int F01 = getQT01Sum(F);
  int F23 = getQT23Sum(F);

  // Form 2x4 board sums 
  const int MAX=3; 
  int bs[MAX];
  bs[0] = E01 + E23; //E
  bs[1] = E23 + F01; //EF
  bs[2] = F01 + F23; //F
  fm006.userdata[t][0]=bs[0];
  fm006.userdata[t][1]=bs[1];
  fm006.userdata[t][2]=bs[2];

  //Compare BS to thresholds
  int BS3=0, BS2=0;
  fm006.userdata[t][7]=0;
  fm006.userdata[t][8]=0;
  fm006.userdata[t][9]=0;
  for(int i=0; i<MAX; i++){
    if(bs[i]>BSThr3) {BS3=1; fm006.userdata[t][7]+=(1<<i); }
    if(bs[i]>BSThr2) {BS2=1; fm006.userdata[t][8]+=(1<<i); }
    if(bs[i]>BSThr1) {       fm006.userdata[t][9]+=(1<<i); }
  }
  int BS1E =(bs[0]>BSThr1) | (bs[1]>BSThr1);
  int BS1F =(bs[2]>BSThr1);
  
  //Jp Sum
  int JpEF  = (bs[0] + bs[2])>>BitSelect; //E+F
  if(JpEF>0xFF) JpEF  =0xFF;
  
  // Output the resulting 6 5-bit sums to the Layer-1 DSM (30 bits)
  fm006.output[t] 
    = BS3         | BS2  << 1
    | BS1E  << 2  | BS1F << 3  
    | JpEF  << 16;
  

  if(PRINT){
    printf("%s input E=%08x F=%08x\n",fm006.name,E,F); 
    printf("%s out=%08x BS3=%1d BS2=%1d BS1E/F=%1d %1d sum=%4d %4d %4d JpEF=%3d\n",
	   fm006.name,fm006.output[t],BS3,BS2,BS1E,bs[0],bs[1],bs[2],BS1F,JpEF);
  }
}

int getFM006_BS3(int out)   {return getbits(out, 0, 1);}
int getFM006_BS2(int out)   {return getbits(out, 1, 1);}
int getFM006_BS1E(int out)  {return getbits(out, 2, 1);}
int getFM006_BS1F(int out)  {return getbits(out, 3, 1);}
int getFM006_JpEF(int out)  {return getbits(out,16, 8);}
