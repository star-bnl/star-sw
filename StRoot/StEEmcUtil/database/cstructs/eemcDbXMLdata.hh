#ifndef TAB_EEMC_DB_XMLDATA__hh
#define TAB_EEMC_DB_XMLDATA__hh
#include "eemcConstDB.hh"
/*
   description:  online-DB, xml data 
 */
struct eemcDbXMLdata {
  char xmldata[EEMCDbMaxXMLData];  // xmldata
  char comment[EEMCDbMaxComment];  // comment
};

#endif
