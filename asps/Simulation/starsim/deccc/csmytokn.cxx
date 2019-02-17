
#include <map>
#include <vector>
#include <algorithm>
#include <assert.h>

extern "C" int           csvptokn_(unsigned long addr);
extern "C" unsigned long csvplong (         int  tokn);

static std::map<unsigned long,int> mapLong;
static std::vector<unsigned long>  vecLong;
#define kMASK 0x40000000
#define kMAZK 0xEF000000
int csvptokn_(unsigned long addr)
{
  int &tokn = mapLong[addr];
  assert (tokn>=0 && tokn < 0x00FFFFFF);
  if (!tokn) {
    vecLong.push_back(addr);
    tokn = vecLong.size();
    assert(tokn < 0xFFFFFF);
  }
  return tokn|kMASK;
}
unsigned long csvplong(int tokn)
{
  assert((tokn&kMAZK)== kMASK);
  tokn = tokn&(~kMASK);
  assert(tokn<=(int)vecLong.size());
  unsigned long addr = vecLong[tokn-1];
  return addr;
}
int csvpint_(int *tokn)
{
 return *((int*)csvplong(*tokn));
}
int csvpreal_(int *tokn)
{
 return *((float*)csvplong(*tokn));
}
int csvpbyte_(int *tokn)
{
 return *((char*)csvplong(*tokn));
}
