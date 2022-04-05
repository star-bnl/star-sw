#include "FactoryT.h"
double FactoryB::fgTotal=0;
FactoryB::FactoryB(const char* name): TNamed(name,"")
{
fCurCount=0; fMaxCount = 1000000;fUseCount=0;fFastDel=0;
fInstCount=0;fFreeCount=0;
}
