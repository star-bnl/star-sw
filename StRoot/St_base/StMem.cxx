/***************************************************************************
 *
 * $Id: StMem.cxx,v 1.1 2000/07/30 01:40:11 perev Exp $
 *
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 **************************************************************************/
#include "StMem.h"
#include <stdio.h>
#include <malloc.h>
double  StMem::fUsed=0;

double StMem::Used()
{
  struct mallinfo info;
  info = mallinfo();
  return double(info.uordblks + info.usmblks)/1000000;
}
void StMem::Print(const char *tit)
{
  struct mallinfo info = mallinfo();
  double used = (info.uordblks + info.usmblks)/1000000.;

  if (tit) printf("\nStMem::%s",tit);
  printf("\t %10.6f (%+10.6f)\n",used,used-fUsed);
  fUsed = used;
}
