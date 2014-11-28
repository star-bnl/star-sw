#ifndef TAB_SPIN_DB_BXMASK__hh
#define TAB_SPIN_DB_BXMASK__hh
#include "spinConstDB.hh"

/*
 * Mask for 120 bXings @ STAR
 */
struct spinDbBXmask {
   unsigned char  bXmask[SPINDbMaxBXings]; /* 0=use=default, non-0=drop bXing, range=(0,119) at STAR IP */
  char  comment[SPINDbMaxComment];
};

#endif

