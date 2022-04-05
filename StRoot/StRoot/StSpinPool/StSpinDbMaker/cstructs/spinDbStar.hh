#ifndef TAB_SPIN_DB_STAR__hh
#define TAB_SPIN_DB_STAR__hh
#include "spinConstDB.hh"

/*
 * Spin related STAR variables
 */
struct spinDbStar {
  int bXoff7; /* offsets for 7-bit bXing, range=(0,119) */
  int bXoff48; /* offsets for 48-bit bXing, range=(0,119) */
  char  comment[SPINDbMaxComment];
};

#endif

