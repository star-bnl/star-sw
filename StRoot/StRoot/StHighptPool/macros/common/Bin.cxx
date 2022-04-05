#include "Bin.h"
#include <iostream>
using namespace Bin;

void Bin::initPtAry(TArrayD* ary, int type)
{
  double ptAry0[]=
    {
    1.6,1.7,1.8,1.9,2.0,2.2,2.45,2.7,3.0,3.35,3.8,4.4,5.1,6.0,7.0,8.0
    };
  int n0 = (int)sizeof(ptAry0)/sizeof(double);

  double ptAry1[]=
    {
      1.6,1.7,1.8,1.9,2.0,2.2,2.4,2.6,2.8,3.0,3.25,3.5,3.75,4.0,4.3,4.6,5.1,6.0,7.0,8.0
    };
  int n1 = (int)sizeof(ptAry1)/sizeof(double);
  switch(type){
  case 0: ary->Set(n0,ptAry0); break;
  case 1: ary->Set(n1,ptAry1); break;
  default:
    cout << "ERROR. type " << type << "out of range" << endl; exit(1);
  }
  
}
