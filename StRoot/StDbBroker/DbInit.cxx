/***************************************************************************
 *
 * $Id: DbInit.cxx,v 1.5 2000/01/14 14:49:10 porter Exp $
 *
 * Author: S. Vanyashin
 ***************************************************************************
 *
 * Description: Initializes connection to database 
 *
 ***************************************************************************
 *
 * $Log: DbInit.cxx,v $
 * Revision 1.5  2000/01/14 14:49:10  porter
 * set verbose level for checking, added $Id & $Logs, & made node container
 * more robust for interactions with StDbLib
 *
 *
 **************************************************************************/
//this function check if it is possible to connect to database server
#include <iostream.h>
#include "mysql.h"

extern "C" int DbInit(const char * dbName)
{

MYSQL mysql;

mysql_init(&mysql);

//set timout in seconds for bnl.local domain

#ifndef __sun
//on sun:
//Program received signal SIGBUS, Bus error.
//0xed737d68 in mysql_options ()

mysql_options(&mysql,MYSQL_OPT_CONNECT_TIMEOUT,"2");

#endif
// Try to establish a connection to the MySQL database engine 

const char *database=dbName;
//only db1 is visible from rcas0202 machine
const char *dbHost="db1.star.bnl.gov";
//char *dbHost="duvall.star.bnl.gov";
//mysql_real_connect(MYSQL *mysql, const char *host, const char *user, const char *passwd, const char *db, uint port, const char *unix_socket, uint client_flag) 

if (!mysql_real_connect(&mysql,dbHost,"","",database,0,NULL,0))
   {
     cerr << "Failed to connect to database: "<<database<< endl;
     cerr << "MySQL ERROR: " <<  mysql_error(&mysql) << endl;
     return 1;
   }

// We are done with the connection, call mysql_close() to terminate it
mysql_close(&mysql);

return 0;

}


