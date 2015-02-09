#include <algorithm>
using namespace std;

#include "bits.hh"
#include "Board.hh"
#include "qt32b_fms_2015_a.hh"
#include <stdio.h>

#include "registerHack.hh"

void qt32b_fms_2015_a(Board& qt)
{
  qt.output = 0;
  int sum[2]={0, 0};
  for (int i=0; i<2; i++) { //AB sum and CD sum
    for (int j = 0; j<16; j++) {
      int id = i*16 + j;
      int adc = qt.channels[id];

      adc = 4095;       //fake data
      
      sum[i] += adc;
    }
    if(sum[i] > 0xfff) sum[i] = 0xfff;
  }

  qt.output |= sum[0];
  qt.output |= sum[1]<<12;
  
  if(sum[0]>0 | sum[1]>0)
    printf("%s=%08x sum=%4d %4d\n",
	   qt.name,qt.output,getQT01Sum(qt.output),getQT23Sum(qt.output));
}

int getQT01Sum(int qtout){return getbits(qtout, 0,12);}
int getQT23Sum(int qtout){return getbits(qtout,12,12);}
