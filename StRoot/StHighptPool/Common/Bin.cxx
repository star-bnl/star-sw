#include "Bin.h"
#include "Stiostream.h"
#ifndef ST_NO_NAMESPACES
using namespace std;
#endif
using namespace Bin;


void Bin::initPtAry(TArrayD* ary, int type)
{
  double ptAry0[]=
    {
    1.6, 1.8, 2.0, 2.25, 2.5, 2.75, 3.0, 3.3, 3.6, 3.9, 
    4.2, 4.5, 4.8, 5.1, 5.5, 6.0, 6.5, 7.0, 8.0, 9.0, 
    10.0, 12.0, 14.0
    };
  int n0 = (int)sizeof(ptAry0)/sizeof(double);

  double ptAry1[]=
    {
    1.6, 1.7, 1.8, 1.9, 2.0, 2.2, 2.4, 2.6, 2.8,
    3.0, 3.25, 3.5, 3.75, 4.0, 4.25, 4.5, 4.75, 5.0,
    5.25, 5.5, 5.75, 6.0, 7.0, 8.0, 9.0, 10.0, 12.0, 14.0, 16.0
    };
  int n1 = (int)sizeof(ptAry1)/sizeof(double);
  switch(type){
  case 0: ary->Set(n0,ptAry0); break;
  case 1: ary->Set(n1,ptAry1); break;
  default:
    cout << "ERROR. type " << type << "out of range" << endl; exit(1);
  }
  
}
