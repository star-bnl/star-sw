//
// Jan Balewski ,MIT
// Example container for results from L2-algo
//
// L2Result must have a size N*4 char, 
//

#ifndef L2_HIEN_RESULT_2009_H
#define L2_HIEN_RESULT_2009_H

struct L2hienResult2009 {
  enum {mySizeChar=164};// negotiate size w/ John before extending
  enum {maxTowers=40};
 
  unsigned int header;//contains total Ntowers (from left, bits [0,7], total ADC [8,31].

  unsigned int value[maxTowers]; //up to maxTowers ped-subtracted ADC , softID pairs
};

//...................................
inline void 
L2hienResult2009_print(L2hienResult2009 *p)
{
  if(p==0) {printf("print L2hienResult2009 - NULL pointer ????\n"); return;}
  printf("print L2hienResult2009: nTowers=%d, total ADC=%d\n",
	 (p->header & 0xFF000000)>>24,
	 (p->header & 0x00FFFFFF));
  for (unsigned int i=0;i<L2hienResult2009::maxTowers && i<((p->header & 0xFF000000)>>24);i++)
    printf("      L2hienResult2009: %d:  ADC=%d\tsoftID=%d\n",
	   i,
	   (p->value[i] & 0xFFFF0000)>>16,
	   (p->value[i] & 0x0000FFFF));
  return;
};

inline void 
L2hienResult2009_unpackValue(int *ADC, int* softID, unsigned int value)
{
  *ADC=(value & 0xFFFF0000)>>16;
  *softID=(value & 0x0000FFFF);
  return;
};

inline void 
L2hienResult2009_unpackAllValues(int* nTowers, int* totalADC, int *ADC, int* softID, L2hienResult2009* result)
{
  *nTowers = (result->header & 0xFF000000)>>24;
  *totalADC =(result->header & 0x00FFFFFF);
  for (int i=0;i<L2hienResult2009::maxTowers && i<*nTowers;i++)
    {
     ADC[i]=(result->value[i] & 0xFFFF0000)>>16;
     softID[i]=(result->value[i] & 0x0000FFFF);
    }
  return;
};


#endif
