#include "bits.hh"
#include "qt32b_fms_2015_a.hh"
#include "fms_fm005_2015_a.hh"
#include <stdio.h>

//#include "registerHack.hh"

void fms_fm005_2015_a(Board& fm005, int t, int simdat){
  int BSThr1   = fm005.registers[0];
  int BSThr2   = fm005.registers[1];
  int BSThr3   = fm005.registers[2];
  int BitSelect= fm005.registers[3];
  //Hack until we know details of registers
  //int BSThr1=Lg_BSThr1;
  //int BSThr2=Lg_BSThr2;
  //int BSThr3=Lg_BSThr3;
  //int BitSelect=Lg_BitSelect;

  //input
  int* in;
  if(simdat==0) {in=(int*)fm005.channels[t];}
  else          {in=(int*)fm005.dsmdata[t];}
  int G=in[3];
  int H=in[2];
  int I=in[1];
  int J=in[0];

  //2x8 sums from QT
  int G01 = getQT01Sum(G);
  int G23 = getQT23Sum(G);
  int H01 = getQT01Sum(H);
  int H23 = getQT23Sum(H);
  int I01 = getQT01Sum(I);
  int I23 = getQT23Sum(I);
  int J01 = getQT01Sum(J);
  int J23 = getQT23Sum(J);
  //printf("G01-J23=%x %x %x %x %x %x %x %x\n",G01,G23,H01,H23,I01,I23,J01,J23);

  // Form 2x4 board sums 
  const int MAX=7; 
  int bs[MAX];
  bs[0] = G01 + G23; //G
  bs[1] = G23 + H01; //GH
  bs[2] = H01 + H23; //H
  bs[3] = H23 + I01; //HI
  bs[4] = I01 + I23; //I
  bs[5] = I23 + J01; //IJ
  bs[6] = J01 + J23; //J
  fm005.userdata[t][0]=bs[0];
  fm005.userdata[t][1]=bs[1];
  fm005.userdata[t][2]=bs[2];
  fm005.userdata[t][3]=bs[3];
  fm005.userdata[t][4]=bs[4];
  fm005.userdata[t][5]=bs[5];
  fm005.userdata[t][6]=bs[6];
  
  //Compare BS to thresholds
  int BS3=0, BS2=0;
  fm005.userdata[t][7]=0;
  fm005.userdata[t][8]=0;
  fm005.userdata[t][9]=0;
  for(int i=0; i<MAX; i++){
    if(bs[i]>BSThr3) {BS3=1; fm005.userdata[t][7]+=(1<<i); }
    if(bs[i]>BSThr2) {BS2=1; fm005.userdata[t][8]+=(1<<i); }
    if(bs[i]>BSThr1) {       fm005.userdata[t][9]+=(1<<i); }
  }
  int BS1GHIJ= (bs[0]>BSThr1) | (bs[1]>BSThr1) | (bs[2]>BSThr1) | (bs[3]>BSThr1)
             | (bs[4]>BSThr1) | (bs[5]>BSThr1) | (bs[6]>BSThr1);

  //Jp Sum
  int JpGH = (bs[0] + bs[2])>>BitSelect;  //G+H
  int JpIJ = (bs[4] + bs[6])>>BitSelect;  //I+J
  if(JpGH >0xFF) JpGH=0xFF;
  if(JpIJ >0xFF) JpIJ=0xFF;

  // Output the resulting 6 5-bit sums to the Layer-1 DSM (30 bits)
  fm005.output[t] 
    = BS3          | BS2   << 1 
    | BS1GHIJ << 2   
    | J23     << 4
    | JpGH    << 16 | JpIJ << 24;
  
  if(PRINT){
    printf("%s input G=%08x H=%08x I=%08x J=%08x\n",fm005.name,G,H,I,J); 
    printf("%s out=%08x BS3=%1d BS2=%1d BS1GHIJ=%1d sum=%4d %4d %4d %4d %4d %4d %4d JpGH/IJ=%3d %3d\n",
	   fm005.name,fm005.output[t],BS3,BS2,BS1GHIJ,
	   bs[0],bs[1],bs[2],bs[3],bs[4],bs[5],bs[6],JpGH,JpIJ);
  }
}

int getFM005_BS3(int out)    {return getbits(out, 0, 1);}
int getFM005_BS2(int out)    {return getbits(out, 1, 1);}
int getFM005_BS1GHIJ(int out){return getbits(out, 2, 1);}
int getFM005_J23(int out)    {return getbits(out, 4,12);}
int getFM005_JpGH(int out)   {return getbits(out,16, 8);}
int getFM005_JpIJ(int out)   {return getbits(out,24, 8);}
