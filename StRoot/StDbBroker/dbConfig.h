/***************************************************************************
 *
 * $Id: dbConfig.h,v 1.1 2000/01/10 20:31:16 porter Exp $
 *
 * Author: R. Jeff Porter & V. Perevoztchikov
 ***************************************************************************
 *
 * Description: c-struct for listing a configuration used by St_db_Maker
 *              
 *
 ***************************************************************************
 *
 * $Log: dbConfig.h,v $
 * Revision 1.1  2000/01/10 20:31:16  porter
 * modified StDbBroker to be an interface to the DB-interface, StDbLib.
 *  - old functionality is retained for the short-term & modifications
 *    are extensions
 *
 *
 **************************************************************************/
#ifndef DBCONFIG_H
#define DBCONFIG_H
//
//:Description:: hierarchy of tables accessed via StDbBroker
//

struct dbConfig_st {

  char tabname[20]; // named reference to table or node
  int  tabID;       // StDbBroker's table ID
  char tabtype[20]; // type of table or node referenced
  char parname[20]; // named reference of parent to table or node referenced
  int  parID;       // StDbBroker's parent ID

};

#endif
