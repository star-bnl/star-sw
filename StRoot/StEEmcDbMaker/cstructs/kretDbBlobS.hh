#ifndef TAB_KRET_DB_BLOBC__hh
#define TAB_KRET_DB_BLOBC__hh
#include "kretConstDB.hh"
/*
   description:  online-DB, any large string of characters, ver 2
 */
struct kretDbBlobS {
  char dataS[KRETmxBlobSlen]; // string with  data 
  char comment[KRETDbMaxComment]; 
};

#endif
