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

#endif	// DSM_HH
