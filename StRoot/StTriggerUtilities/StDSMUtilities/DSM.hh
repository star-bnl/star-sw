//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 1 Jan 2009
//

#ifndef DSM_HH
#define DSM_HH

#include <string>
#include <sstream>
#include <iomanip>
#include <stdio.h>
using namespace std;

struct DSM {
  enum { NREGISTERS = 32, NCHANNELS = 16, LUT_SIZE = 16, INFO_SIZE = 64 };

  string name;
  int    registers[NREGISTERS];
  short  channels[NCHANNELS];
  char   lut[LUT_SIZE];
  int    info[INFO_SIZE];
  int    output;

  DSM(const string& name = "") : name(name) { clear(); }
  void clear();
  void setName(const string& basename, int layer, int dsm);
  void dump();
};

inline void DSM::clear()
{
  fill(registers,registers+NREGISTERS,0);
  fill(channels,channels+NCHANNELS,0);
  fill(lut,lut+LUT_SIZE,0);
  fill(info,info+INFO_SIZE,0);
  output = 0;
}

inline void DSM::setName(const string& basename, int layer, int dsm)
{
  ostringstream ss;
  ss << basename << layer << setw(2) << setfill('0') << dsm+1;
  name = ss.str();
}

inline void DSM::dump()
{
  printf("%s: ", name.c_str());
  for (int ch = 0; ch < 8; ++ch)
    printf("%04x ", (unsigned short)channels[ch]);
  printf("\n");
}

#endif	// DSM_HH
