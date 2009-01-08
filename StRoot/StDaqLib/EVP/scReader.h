#ifndef _SC_READER_H_
#define _SC_READER_H_
#include "sys/types.h"
// returns EVP_DATA_ERR if no bank
// >0 if OK
//
// Note, even if bank is present, check
// "sc.valid"... this says if the information is timely...
//
namespace  OLDEVP {
struct sc_t {
  u_int valid;       
  u_int time;
  int timelag;  // lag between read time and event time (to 1 sec)
  float mag_field;
  u_int rich_scalers[17];
};

extern struct sc_t sc;

extern int scReader(char *m);   // returns EVP_DATA_ERR
} 
#endif
