#if !defined(__CINT__) || defined(__MAKECINT__)
#include "TSQLServer.h"
#include "TMySQLResult.h"
#include "TMySQLRow.h"
#include "TStopwatch.h"
#include "TSystem.h"
#endif
// void Load() {
//   //  gSystem->Load("libmysqlclient"); 
//   gSystem->Load("libMySQL.so"); 
// }
enum enum_field_types { FIELD_TYPE_DECIMAL, FIELD_TYPE_TINY,
			FIELD_TYPE_SHORT,  FIELD_TYPE_LONG,
			FIELD_TYPE_FLOAT,  FIELD_TYPE_DOUBLE,
			FIELD_TYPE_NULL,   FIELD_TYPE_TIMESTAMP,
			FIELD_TYPE_LONGLONG,FIELD_TYPE_INT24,
			FIELD_TYPE_DATE,   FIELD_TYPE_TIME,
			FIELD_TYPE_DATETIME, FIELD_TYPE_YEAR,
			FIELD_TYPE_NEWDATE,
			FIELD_TYPE_ENUM=247,
			FIELD_TYPE_SET=248,
			FIELD_TYPE_TINY_BLOB=249,
			FIELD_TYPE_MEDIUM_BLOB=250,
			FIELD_TYPE_LONG_BLOB=251,
			FIELD_TYPE_BLOB=252,
			FIELD_TYPE_VAR_STRING=253,
			FIELD_TYPE_STRING=254
};
Char_t *MySQLType(Int_t k) {
  switch (k) {
  case FIELD_TYPE_DECIMAL:     return "DECIMAL";      
  case FIELD_TYPE_TINY:        return "TINY";         
  case FIELD_TYPE_SHORT:       return "SHORT";        
  case FIELD_TYPE_LONG:        return "LONG";         
  case FIELD_TYPE_FLOAT:       return "FLOAT";        
  case FIELD_TYPE_DOUBLE:      return "DOUBLE";       
  case FIELD_TYPE_NULL:        return "NULL";         
  case FIELD_TYPE_TIMESTAMP:   return "TIMESTAMP";    
  case FIELD_TYPE_LONGLONG:    return "LONGLONG";     
  case FIELD_TYPE_INT24:       return "INT24";        
  case FIELD_TYPE_DATE:        return "DATE";         
  case FIELD_TYPE_TIME:        return "TIME";         
  case FIELD_TYPE_DATETIME:    return "DATETIME";     
  case FIELD_TYPE_YEAR:        return "YEAR";         
  case FIELD_TYPE_NEWDATE:     return "NEWDATE";      
  case FIELD_TYPE_ENUM:        return "ENUM";         
  case FIELD_TYPE_SET:         return "SET";          
  case FIELD_TYPE_TINY_BLOB:   return "TINY_BLOB";    
  case FIELD_TYPE_MEDIUM_BLOB: return "MEDIUM_BLOB";  
  case FIELD_TYPE_LONG_BLOB:   return "LONG_BLOB";    
  case FIELD_TYPE_BLOB:        return "BLOB";         
  case FIELD_TYPE_VAR_STRING:  return "VAR_STRING";   
  case FIELD_TYPE_STRING:      return "STRING";       
  default:                     return "Unknown";
  };
} 
void MySelect()
{
  //   TSQLServer *db = TSQLServer::Connect("mysql://localhost/test","nobody", "");
  //   TSQLServer *db = TSQLServer::Connect("mysql://db1.usatlas.bnl.gov/Production", "atlas", "insider");
//   Load();
   TSQLServer *db = TSQLServer::Connect("mysql://dbx.star.bnl.gov:3316/Calibrations_tpc", "", "");
   printf("Server info: %s\n", db->ServerInfo());
   
   TMySQLRow *row;
   TMySQLResult *res;
   
   // list databases available on server
   printf("\nList all databases on server %s\n", db->GetHost());
   res = (TMySQLResult *) db->GetDataBases();
   while ((row = (TMySQLRow *) res->Next())) {
      printf("%s\n", row->GetField(0));
      delete row;
   }
   delete res;

   // list tables in database "Calibrations_tpc" (the permission tables)
   printf("\nList all tables in database \"Calibrations_tpc\" on server %s\n",
          db->GetHost());
   res = (TMySQLResult *) db->GetTables("Calibrations_tpc");
   while ((row = (TMySQLRow *) res->Next())) {
     
      printf("%s\n", row->GetField(0));
      delete row;
   }
   delete res;
   
   // list columns in table "tpcPressureB" in database "mysql"
   printf("\nList all columns in table \"tpcPressureB\" in database \"Calibrations_tpc\" on server %s\n",
          db->GetHost());
   res = (TMySQLResult *) db->GetColumns("Calibrations_tpc", "tpcPressureB");
   //   enum_field_types k = res->GetFieldType(0);
   const MYSQL_FIELD *fieldInfo = res->FieldInfo();
   int i = 0;
   while ((row = (TMySQLRow *) res->Next())) {
     //     const MYSQL_FIELD *fieldInfo = row->FieldInfo();
    printf("%s\n", row->GetField(0));
      delete row;
   }
   delete res;

   // start timer
   TStopwatch timer;
   timer.Start();

   // query database and print results
   const char *sql = "SELECT * from Calibrations_tpc.tpcPressureB ";//"WHERE run=1 OR run=122";
//   const char *sql = "select count(*) from Calibrations_tpc.tpcPressureB "
//                     "WHERE tag&(1<<2)";
   
   res = (TMySQLResult *) db->Query(sql);

   int nrows = res->GetRowCount();
   printf("\nGot %d rows in result\n", nrows);
   
   int nfields = res->GetFieldCount();
   for (int i = 0; i < nfields; i++)
   printf("\n");
   //   for (int i = 0; i < nfields*40; i++)
   //     printf("=");
   //   printf("\n");
   
   for (int j = 0; j < nfields; j++) {
     printf("%10s\t%i\t%s", res->GetFieldName(j), res->GetFieldType(j), MySQLType(res->GetFieldType(j)));
     for (int i = 0; i < nrows; i++) {
       row = (TMySQLRow *) res->Next();
       printf("%40s", row->GetField(j));
       delete row;
     }
     printf("\n");
   }
   
   delete res;
   delete db;

   // stop timer and print results
   timer.Stop();
   Double_t rtime = timer.RealTime();
   Double_t ctime = timer.CpuTime();

   printf("\nRealTime=%f seconds, CpuTime=%f seconds\n", rtime, ctime);
}
