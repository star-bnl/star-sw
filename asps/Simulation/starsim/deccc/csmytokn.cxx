
#include <map>
#include <vector>
#include <algorithm>
#include <assert.h>

extern "C" int           csvptokn(unsigned long addr);
extern "C" unsigned long csvplong (         int  tokn);

static std::map<unsigned long,int> mapLong;
static std::vector<unsigned long>  vecLong;
#define kMASK 0x40000000
#define kMAZK 0xEF000000
//______________________________________________________________________________
int csvptokn(unsigned long addr)
{
//		Convert unsigned long to  token
  int &tokn = mapLong[addr];
  assert (tokn>=0 && tokn < 0x00FFFFFF);
  if (!tokn) {
    vecLong.push_back(addr);
    tokn = vecLong.size();
    assert(tokn < 0xFFFFFF);
  }
  return tokn|kMASK;
}
//______________________________________________________________________________
unsigned long csvplong(int tokn)
{
//		Convert unsigned long  from token
  assert((tokn&kMAZK)== kMASK);
  tokn = tokn&(~kMASK);
  assert(tokn<=(int)vecLong.size());
  unsigned long addr = vecLong[tokn-1];
  return addr;
}

//______________________________________________________________________________
int csvpint_(int *tokn)
{
//		Get int from token
 return *((int*)csvplong(*tokn));
}

//______________________________________________________________________________
int csvpreal_(int *tokn)
{
//		Get float from token
 return *((float*)csvplong(*tokn));
}

//______________________________________________________________________________
int csvpbyte_(int *tokn)
{
//		Get char from token
 return *((char*)csvplong(*tokn));
}
