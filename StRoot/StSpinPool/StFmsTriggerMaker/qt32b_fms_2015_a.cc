#include <algorithm>
using namespace std;

#include "bits.hh"
#include "qt32b_fms_2015_a.hh"
#include <stdio.h>

#include "registerHack.hh"

void qt32b_fms_2015_a(Board& qt, int t){
  qt.output[t] = 0;
  int sum[2]={0, 0};
  for (int i=0; i<2; i++) { //AB sum and CD sum
    for (int j = 0; j<16; j++) {
      int id = i*16 + j;
      int adc = qt.channels[t][id];

      //adc = 4095;       //fake data
      
      sum[i] += adc;
    }
    if(sum[i] > 0xfff) sum[i] = 0xfff;
  }

  qt.output[t] |= sum[0];
  qt.output[t] |= sum[1]<<16;
  
  if(PRINT)
    if(sum[0]>0 || sum[1]>0)
      printf("%s=%08x sum=%4d %4d\n",
	     qt.name,qt.output[t],getQT01Sum(qt.output[t]),getQT23Sum(qt.output[t]));
}

int getQT01Sum(int qtout){return getbits(qtout, 0,12);}
int getQT23Sum(int qtout){return getbits(qtout,16,12);}
