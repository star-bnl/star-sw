//
// Jan Balewski ,MIT
// Example container for results from L2-algo
//
// L2Result must have a size N*4 char, 
//

#ifndef L2_HIEN_RESULT_2012_H
#define L2_HIEN_RESULT_2012_H

struct L2hienResult2012 {
  enum {mySizeChar=164};// negotiate size w/ John before extending
  enum {maxTowers=40};
 
  unsigned int header;//contains total Ntowers (from left, bits [0,7], total ADC [8,31].

  unsigned int value[maxTowers]; //up to maxTowers ped-subtracted ADC , softID pairs
};

//...................................
inline void 
L2hienResult2012_print(L2hienResult2012 *p)
{
  if(p==0) {printf("print L2hienResult2012 - NULL pointer ????\n"); return;}
  printf("print L2hienResult2012: nTowers=%d, total ADC=%d\n",
	 (p->header & 0xFF000000)>>24,
	 (p->header & 0x00FFFFFF));
  for (unsigned int i=0;i<L2hienResult2012::maxTowers && i<((p->header & 0xFF000000)>>24);i++)
    printf("      L2hienResult2012: %d:  ADC=%d\tsoftID=%d\n",
	   i,
	   (p->value[i] & 0xFFFF0000)>>16,
	   (p->value[i] & 0x0000FFFF));
  return;
};

inline void 
L2hienResult2012_unpackValue(int *ADC, int* softID, unsigned int value)
{
  *ADC=(value & 0xFFFF0000)>>16;
  *softID=(value & 0x0000FFFF);
  return;
};

inline void 
L2hienResult2012_unpackAllValues(int* nTowers, int* totalADC, int *ADC, int* softID, L2hienResult2012* result)
{
  *nTowers = (result->header & 0xFF000000)>>24;
  *totalADC =(result->header & 0x00FFFFFF);
  for (int i=0;i<L2hienResult2012::maxTowers && i<*nTowers;i++)
    {
     ADC[i]=(result->value[i] & 0xFFFF0000)>>16;
     softID[i]=(result->value[i] & 0x0000FFFF);
    }
  return;
};


#endif
