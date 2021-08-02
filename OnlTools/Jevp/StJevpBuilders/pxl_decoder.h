/* $Id: pxl_decoder.h,v 1.2 2014/05/01 19:46:24 jschamba Exp $ */
#include <bitset>
using namespace std;

template <size_t M, size_t N>
class bitset2D
{
public:
  //typedefs
  typedef typename std::bitset<M*N>::reference reference;

  //bitset::bitset
  //See http://www.cplusplus.com/reference/stl/bitset/bitset/
  bitset2D() : m_bits(){}

  //bitset operators
  //See http://www.cplusplus.com/reference/stl/bitset/operators/

  //Bit access
  bool operator()(size_t m, size_t n) const {return m_bits[m*N + n];}
  reference operator()(size_t m, size_t n) {return m_bits[m*N + n];}

  //Bit operations:
  bitset2D<M, N>& reset() {m_bits.reset(); return *this;}
  bitset2D<M, N>& reset(size_t m, size_t n) {m_bits.reset(m*N + n); return *this;}
  bitset2D<M, N>& flip() {m_bits.flip(); return *this;}
  bitset2D<M, N>& flip(size_t m, size_t n) {m_bits.flip(m*N + n); return *this;}
  bitset2D<M, N>& set() {m_bits.set(); return *this;}
  bitset2D<M, N>& set(size_t m, size_t n, bool val = true) {
    m_bits.set(m*N + n, val); return *this;}

  //Bitset operations:
  unsigned long to_ulong() const {return m_bits.to_ulong();}
  template <class charT, class traits, class Allocator>
  std::basic_string<charT,traits,Allocator> to_string() const {
    m_bits.to_string<charT, traits, Allocator>(); return *this;}
  unsigned long count() const {return m_bits.count();}
  unsigned long size() const {return m_bits.size();}
  bool test(size_t m, size_t n) const {return m_bits.test(m*N + n);}
  unsigned long any() const {return m_bits.any();}
  unsigned long none() const {return m_bits.none();}

private:
  std::bitset<M*N> m_bits;
};

//#define PRINT_DEBUG 1
//#define PRINT_STATUS 1
#define CRC_CALC 1

#ifdef PRINT_DEBUG
#define DEBUGP(...) fprintf(stdout, __VA_ARGS__)
#else
#define DEBUGP(...)
#endif

// some constants for PXL RDO
const int NSENSOR = 40;
const int NCOL = 960;
const int NROW = 928;

const int PXLERR_HEADER = 0x001;
const int PXLERR_HITLEN = 0x002;
const int PXLERR_SENSORNUM = 0x004;
const int PXLERR_DUPLICATE = 0x008;
const int PXLERR_ENDER = 0x010;
const int PXLERR_RANGE = 0x020;
const int PXLERR_OUTOFORDER = 0x040;
const int PXLERR_UNKNOWNROW = 0x080;
const int PXLERR_CRC = 0x100;
const int PXLERR_LENGTH = 0x200;

struct _pxlHeaderInfo {
  unsigned short tcdWord;
  unsigned int rhicStrobeCtr;
  unsigned short temperature[4][2];
};

int pxl_decoder(const u_int *d, 
		const int wordLength, 
		bitset2D<NROW,NCOL> *bs, 
		int *OVF,
		struct _pxlHeaderInfo *pxlHeaderInfo,
		float *ave_runlength,
		int *error_cnt);

