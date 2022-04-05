//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M
// 16 July 2012
//

#ifndef BOARD_HH
#define BOARD_HH

#include <cstring>

enum { NCHANNELS = 32, NREGISTERS = 32, MAXPP=5, MAXT=MAXPP*2+1, PRINT=0};

struct Board {
  char name[10];
  unsigned int bitmask;
  unsigned short channels[MAXT][NCHANNELS];
  unsigned short dsmdata[MAXT][NCHANNELS];
  unsigned int registers[NREGISTERS];
  unsigned int output[MAXT];
  int userdata[MAXT][100];

  Board();
  unsigned int& registerAt(int i) { return registers[i>>24]; }
  const unsigned int& registerAt(int i) const { return registers[i>>24]; }
  void setName(const char* s);
  void clear();
  void reset();
};

inline Board::Board()
{
  reset();
}

inline void Board::setName(const char* s)
{
  strcpy(name,s);
}

inline void Board::clear()
{
  memset(channels,0,sizeof(channels));
  memset(dsmdata,0,sizeof(dsmdata));
  memset(output,0,sizeof(output));
}

inline void Board::reset()
{
  setName("");
  bitmask = 0xffffffff;
  memset(channels,0,sizeof(channels));
  memset(dsmdata,0,sizeof(dsmdata));
  memset(registers,0,sizeof(registers));
  memset(output,0,sizeof(output));
}

#endif // BOARD_HH
