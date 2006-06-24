#include "Factory.h"
double BFactory::fgTotal=0;
BFactory::BFactory(const string& name): Named(name)
{
fCurCount=0; fMaxCount = 1000000;fUseCount=0;fFastDel=0;
fInstCount=0;fFreeCount=0;
}
