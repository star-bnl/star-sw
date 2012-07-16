#ifndef BOARD_HH
#define BOARD_HH

#include <cstring>

struct Board {
  enum { NCHANNELS = 32, NREGISTERS = 32 };

  char name[6];
  unsigned int bitmask;
  unsigned short channels[NCHANNELS];
  unsigned int registers[NREGISTERS];
  unsigned int output;

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
  output = 0;
}

inline void Board::reset()
{
  setName("");
  bitmask = 0xffffffff;
  memset(channels,0,sizeof(channels));
  memset(registers,0,sizeof(registers));
  output = 0;
}

#endif // BOARD_HH
