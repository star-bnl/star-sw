#include "bits.hh"
#include "fms_fm103_2017_a.hh"
#include "fms_fm005_2017_a.hh"
#include "fms_fm006_2017_a.hh"
#include <stdio.h>

//#include "registerHack.hh"

void fms_fm103_2017_a(Board& fm103, int t, int simdat){
  const int BSThr1=fm103.registers[0];
  const int BSThr2=fm103.registers[1];
  const int BSThr3=fm103.registers[2];
  //Hack until we know details of registers
  //int BSThr1=Lg_BSThr1; 
  //int BSThr2=Lg_BSThr2; 
  //int BSThr3=Lg_BSThr3; 
  //printf("%s BSthr1=%d BSthr2=%d BSthr3=%d\n",fm103.name,BSThr1,BSThr2,BSThr3);

  //input
  int* in;
  if(simdat==0) {in=(int*)fm103.channels[t];}
  else          {in=(int*)fm103.dsmdata[t];}
  int T1=in[0];  //fm005,fm009
  int T2=in[1];  //fm006,fm010
  int B1=in[2];  //fm007,fm011
  int B2=in[3];  //fm008,fm012

  //From another 2*4 sum
  int JJ=getFM005_2017a_J23(T1) + getFM005_2017a_J23(B1);
  fm103.userdata[t][0]=JJ;

  //BS bits
  int BS3 = getFM005_2017a_BS3(T1) | getFM006_2017a_BS3(T2) | getFM005_2017a_BS3(B1) | getFM006_2017a_BS3(B2) | (JJ>BSThr3);
  int BS2 = getFM005_2017a_BS2(T1) | getFM006_2017a_BS2(T2) | getFM005_2017a_BS2(B1) | getFM006_2017a_BS2(B2) | (JJ>BSThr2);
  int BS1JJ=(JJ>BSThr1);
  int BS1T=getFM006_2017a_BS1EF(T2) | getFM005_2017a_BS1GH(T1);
  int BS1M=getFM005_2017a_BS1IJ(T1) | BS1JJ | getFM005_2017a_BS1IJ(B1);
  int BS1B=getFM006_2017a_BS1EF(B2) | getFM005_2017a_BS1GH(B1);
  fm103.userdata[t][7]=(JJ>BSThr3);
  fm103.userdata[t][8]=(JJ>BSThr2);
  fm103.userdata[t][9]=BS1JJ;
  //printf("BBB %s %d %d %d\n",fm103.name,fm103.userdata[t][7],fm103.userdata[t][8],fm103.userdata[t][9]);

  //Jp
  int JpT=getFM006_2017a_JpEF(T2)+getFM005_2017a_JpGH(T1)+getFM005_2017a_JpIJ(T1);
  int JpM=getFM005_2017a_JpIJ(T1)+getFM005_2017a_JpIJ(B1);
  int JpB=getFM006_2017a_JpEF(B2)+getFM005_2017a_JpGH(B1)+getFM005_2017a_JpIJ(B1);
  if(JpT  >0xFF) JpT  =0xFF;
  if(JpM  >0xFF) JpM  =0xFF;
  if(JpB  >0xFF) JpB  =0xFF;

  fm103.output[t] =  BS3 | BS2   << 1
    | BS1T << 2 | BS1M <<  3 | BS1B <<4 
    | JpT  << 5 | JpM  << 16 | JpB  <<24;

  if(PRINT){
    printf("%s input T1=%08x T2=%08x B1=%08x B2=%08x\n",fm103.name,T1,T2,B1,B2); 
    printf("%s out=%08x BS3=%1d BS2=%1d BS1T/M/B=%1d %1d %1d JpT/M/B %3d %3d %3d\n",
	   fm103.name,fm103.output[t],
	   BS3,BS2,BS1T,BS1M,BS1B,JpT,JpM,JpB);
  }
}

int getFM103_2017a_BS3(int out)  {return getbits(out, 0, 1);}
int getFM103_2017a_BS2(int out)  {return getbits(out, 1, 1);}
int getFM103_2017a_BS1T(int out) {return getbits(out, 2, 1);}
int getFM103_2017a_BS1M(int out) {return getbits(out, 3, 1);}
int getFM103_2017a_BS1B(int out) {return getbits(out, 4, 1);}
int getFM103_2017a_JpT(int out)  {return getbits(out, 5, 8);}
int getFM103_2017a_JpM(int out)  {return getbits(out,16, 8);}
int getFM103_2017a_JpB(int out)  {return getbits(out,24, 8);}
