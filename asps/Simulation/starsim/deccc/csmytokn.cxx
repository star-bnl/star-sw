
#include <map>
#include <vector>
#include <algorithm>
#include <assert.h>

extern "C" int           csvptokn(unsigned long addr);
extern "C" unsigned long csvplong(        int  tokn);
extern "C" int           csvpyes (         int  tokn);

static std::map<unsigned long,int> mapLong;
static std::vector<unsigned long>  vecLong;
enum {kVPMASK=0xABC00000, kVPMAZK=0xFFF00000, kVPMAX = 0x000FFFFF};

//______________________________________________________________________________
int csvptokn(unsigned long addr)
{
//		Convert unsigned long to  token
  int &tokn = mapLong[addr];
  assert (tokn>=0 && tokn < 0x00FFFFFF);
  if (!tokn) {
    vecLong.push_back(addr);
    tokn = vecLong.size();
    assert(tokn*100 < (int)kVPMAX);
  }
  return (tokn*100)|kVPMASK;
}
//______________________________________________________________________________
unsigned long csvplong(int tokn)
{
//		Convert unsigned long  from token
  assert((tokn&kVPMAZK)==kVPMASK);
  tokn = (tokn&kVPMAX);
assert(!(tokn%100));
  tokn = tokn/100-1;
  assert(tokn<(int)vecLong.size());
  unsigned long addr = vecLong[tokn];
  return addr;
}
//______________________________________________________________________________
int csvpyes(int tokn)
{
  return ((tokn&kVPMAZK)==kVPMASK);
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
