#include "bits.hh"
#include "fms_fm101_2017_a.hh"
#include "fms_fm001_2017_a.hh"
#include <stdio.h>

//#include "registerHack.hh"

void fms_fm101_2017_a(Board& fm101, int t, int simdat){
  const int BSThr1=fm101.registers[0];
  const int BSThr2=fm101.registers[1];
  const int BSThr3=fm101.registers[2];
  //Hack until we know details of registers
  //int BSThr1=Sm_BSThr1; 
  //int BSThr2=Sm_BSThr2; 
  //int BSThr3=Sm_BSThr3; 
  //printf("%s BSthr1=%d BSthr2=%d BSthr3=%d\n",fm101.name,BSThr1,BSThr2,BSThr3);

  //input
  int* in;
  if(simdat==0) {in=(int*)fm101.channels[t];}
  else          {in=(int*)fm101.dsmdata[t];}
  //  int T=in[1];
  //  int B=in[0];
  int T=in[0];
  int B=in[1];

  //Make 1 more BS sum
  int DD=getFM001_2017a_D23(T) + getFM001_2017a_D23(B);
  fm101.userdata[t][0]=DD;

  //Compare to threshold and OR
  int BS3 = getFM001_2017a_BS3(T) | getFM001_2017a_BS3(B) | (DD>BSThr3);
  int BS2 = getFM001_2017a_BS2(T) | getFM001_2017a_BS2(B) | (DD>BSThr2);
  int BS1DD=(DD>BSThr1);
  fm101.userdata[t][7]=(DD>BSThr3);
  fm101.userdata[t][8]=(DD>BSThr2);
  fm101.userdata[t][9]=BS1DD;

  //BS1 bits
  int BS1T=getFM001_2017a_BS1ABC(T);
  int BS1M=getFM001_2017a_BS1CD(T) | getFM001_2017a_BS1CD(B) | BS1DD;
  int BS1B=getFM001_2017a_BS1ABC(B);

  //Jp sums
  int JpT=getFM001_2017a_JpAB(T)+getFM001_2017a_JpCD(T);
  int JpM=getFM001_2017a_JpCD(T)+getFM001_2017a_JpCD(B);
  int JpB=getFM001_2017a_JpAB(B)+getFM001_2017a_JpCD(B);
  if(JpT>0xff) JpT=0xff;
  if(JpM>0xff) JpM=0xff;
  if(JpB>0xff) JpB=0xff;
  
  fm101.output[t] =  BS3 <<13 | BS2   << 1
    | BS1T << 2 | BS1M <<  3 | BS1B <<4 
    | JpT  << 5 | JpM  << 16 | JpB  <<24;
  
  if(PRINT){
    printf("%s input T=%08x B=%08x\n",fm101.name,T,B);
    printf("%s out=%08x BS3=%1d BS2=%1d BS1T/M/B=%1d %1d %1d JpT/M/B %3d %3d %3d\n",
	   fm101.name,fm101.output[t],
	   BS3,BS2,BS1T,BS1M,BS1B,JpT,JpM,JpB);
  }
}

int getFM101_2017a_BS3(int out)  {return getbits(out,13, 1);}
int getFM101_2017a_BS2(int out)  {return getbits(out, 1, 1);}
int getFM101_2017a_BS1T(int out) {return getbits(out, 2, 1);}
int getFM101_2017a_BS1M(int out) {return getbits(out, 3, 1);}
int getFM101_2017a_BS1B(int out) {return getbits(out, 4, 1);}
int getFM101_2017a_JpT(int out)  {return getbits(out, 5, 8);}
int getFM101_2017a_JpM(int out)  {return getbits(out,16, 8);}
int getFM101_2017a_JpB(int out)  {return getbits(out,24, 8);}
