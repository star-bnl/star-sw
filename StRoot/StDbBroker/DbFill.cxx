/***************************************************************************
 *
 * $Id: DbFill.cxx,v 1.2 2000/01/10 20:31:16 porter Exp $
 *
 * Author: S. Vanyashin
 ***************************************************************************
 *
 * Description: low-level C-code to write to "params" database
 *              
 *
 ***************************************************************************
 *
 * $Log: DbFill.cxx,v $
 * Revision 1.2  2000/01/10 20:31:16  porter
 * modified StDbBroker to be an interface to the DB-interface, StDbLib.
 *  - old functionality is retained for the short-term & modifications
 *    are extensions
 *
 *
 **************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strstream.h>

#include "mysql.h"
#include "mysql_com.h"
extern "C" char *strmov(char *dst,const char *src);//from mysqlclient lib

#include "StDbBroker.h"

extern "C" void DbFill(uint * datetime,
		       const char * tableName,
		       const char * structName,
		       uint nVar,
		       StDbBroker::Descriptor *d,//array (for each of nVar)
		       const char **Comments,
		       uint nRows,
		       uint sizeOfStruct,
		       void * pData)
{
  //cout << "DbFill: structure: " << structName << endl;
//cout << "Sorry, this software release does not let users to fill the database" << endl;
//  return;

//  for (uint i=0;i<nVar;i++){
//    cout <<i<<endl;
//    cout <<d[i].name<<endl;
//    cout <<d[i].firstDimension<<endl;
//    cout <<d[i].offset<<endl;
//    cout <<d[i].dimensions<<endl;
//    cout <<StDbBroker::GetTypeName(d[i].type)<<endl;
//  }

MYSQL mysql;
MYSQL_RES *result;
MYSQL_ROW row;

unsigned int num_fields;
unsigned int num_rows;

const int MAXBUF=2048;
char query[MAXBUF];
char buf[MAXBUF];
ostrstream Query(buf,MAXBUF);
char *end;
char *binQuery;

//sun:
uint count;
//uint num_elem;
uint i, j;

//cout<<"filling: "<<tableName<<", date "<<datetime[0]<<" and time "<<datetime[1]<<endl;

//char validFrom[15];
char validFrom[20];
char time[7];
//TIMESTAMP format: "YYYYMMDDHHMMSS";
sprintf(validFrom,"%.8d",datetime[0]);
sprintf(time,"%.6d",datetime[1]);
strcat(validFrom,time);

//convert to DATETIME format: 0000-00-00 00:00:00

validFrom[19]='\0';
validFrom[18]=validFrom[13];
validFrom[17]=validFrom[12];
validFrom[16]=':';
validFrom[15]=validFrom[11];
validFrom[14]=validFrom[10];
validFrom[13]=':';
validFrom[12]=validFrom[9];
validFrom[11]=validFrom[8];
validFrom[10]=' ';
validFrom[9]=validFrom[7];
validFrom[8]=validFrom[6];
validFrom[7]='-';
validFrom[6]=validFrom[5];
validFrom[5]=validFrom[4];
validFrom[4]='-';

cout<<"validFrom: " <<validFrom<<endl;

// Initialize a connection handler

mysql_init(&mysql);

// Establish a connection to the MySQL database engine 
// params - for reading, test_blob -for development

//if (!mysql_real_connect(&mysql,"duvall.star.bnl.gov","root","","params",0,NULL,0))
//if (!mysql_real_connect(&mysql,"duvall.star.bnl.gov","writer","","params",0,NULL,0))
if (!mysql_real_connect(&mysql,"localhost","stardb","","params",0,NULL,0))
//if (!mysql_real_connect(&mysql,"db1.star.bnl.gov","writer","","params",0,NULL,0))
   {
     cerr << "Failed to connect to database: Error: "
 	 <<  mysql_error(&mysql) << endl;
   }

//check structure name/size first

int maxVersion=0;
uint *existingStrID=NULL;
uint structureID=0;
uint num_struct=0;

Query.seekp(0);
Query << "SELECT ID,version FROM structures WHERE name=\""<<structName
      <<"\" AND sizeOfStruct="<<sizeOfStruct
      <<" AND nElements="<<nVar<<" ORDER BY version"<<ends; 

//cout << "database query: " << Query.str() << endl;

if (mysql_real_query(&mysql,Query.str(),Query.pcount()-1))
  {
    cerr << "Failed to query: Error: " <<  mysql_error(&mysql) << endl;
     mysql_close(&mysql);
     return;
  }
else // query succeeded, get result
  {
    result = mysql_store_result(&mysql);
    if (result) 
      {
	num_fields = mysql_num_fields(result);
	if (num_fields!=2) cerr <<"ERROR: wrong size of struct query"<<endl;

	num_struct = mysql_num_rows(result);
	if (num_struct==0)// this is the new struct name/size to insert
	  {
 	    //cout<< "struct "<<structName<<" do not exist in database" <<endl;
	    //cout << "database query: " << Query.str() << endl;
	    maxVersion=0;
	  }
	else   // this structure name/size already exists
	  {
// 	    if (num_struct>0) {
// 	      cout << "INFO: there is structure "
// 		   << structName << endl;
	      //cout << "database query: " << Query.str() << endl;
// 	    } 
	    existingStrID = new uint[num_struct]; 
	    for (uint id=0;id<num_struct;id++)
	      {
		row = mysql_fetch_row(result);
		existingStrID[id]=(uint)atoi(row[0]);
		maxVersion=atoi(row[1]);
	      }
	  }
	mysql_free_result(result);
      }
    else   // query OK but, no result?!
      {
	cerr << "no result: Error: " <<  mysql_error(&mysql) << endl;
	mysql_close(&mysql);
	return;
      } 
 }

int same=0;

for (i=0;i<num_struct;i++)
  {
    //same variables counter
    count=0;

    //cout << "i, num_struct, existingStrID[i]: " << i<<" "<<num_struct<<" "<<existingStrID[i] << endl;

    Query.seekp(0);
    Query << "SELECT headers.name, headers.offset, headers.nDims, headers.firstDim, headers.comment  FROM structures, headers WHERE structures.ID=headers.strID AND structures.ID="<<existingStrID[i] <<" ORDER BY headers.offset"<<ends;

    //cout << "database query: " << Query.str() << endl;
    if (mysql_real_query(&mysql,Query.str(),Query.pcount()-1))
      {
	cerr << "Failed to query: Error: " <<  mysql_error(&mysql) << endl;
	mysql_close(&mysql);
	return;
      }
    else // query succeeded, get result
      {
	result = mysql_store_result(&mysql);
	if (result)// query OK
	  {
	    num_fields = mysql_num_fields(result);
	    if (num_fields!=5) cerr << "ERROR: wrong size of headers query"<<endl;
	    
	    num_rows = mysql_num_rows(result);
	    
	    if (num_rows!=nVar) //size is different from this ID
	      {
		cout<<"database structure "<<i<<" is of "<< num_rows
		    <<" variables, this structure is "<< nVar <<endl;
		continue;
	      }
	    else //same size size: check names
	      {
		for (j=0;j<num_rows;j++)
		  {
		    row = mysql_fetch_row(result);
		    
		    if (strcmp(d[j].name,row[0]))
		      break;
		    
		    //same name: check offset
		    //cout<<"offset: \""<<d[j].offset<<"\" \""<<row[1]<<"\""<<endl;
		    if ( d[j].offset!=atoi(row[1]) )
		      break;
		    
		    //same offset: check nDims
		    //cout<<"nDims: \""<<d[j].dimensions<<"\" \""<<row[2]<<"\""<<endl;
		    if ( d[j].dimensions!=atoi(row[2]) )
		      break;
		    
		    //same nDims: check firstDim
		    //cout<<"nDims: \""<<d[j].firstDimension<<"\" \""<<row[3]<<"\""<<endl;
		    if ( d[j].firstDimension!=atoi(row[3]) )
		      break;
		    
		    //same value: check comment
		    
		    // strip trailing blanks from Comments
// 		    int jc=strlen(Comments[j]);
// 		    while (jc != 0  && Comments[j][jc-1] == ' ') jc--;
// 		    Comments[j][jc]='\0';
		    
		    //cout<<"Comment: \n\""<<Comments[j]<<"\"\n\""<<row[4]<<"\""<<endl;
		    //DATABASE Comments are truncated to CHAR[64]
		    if (strncmp(Comments[j],row[4],64))//different comment, new structure?
		      {
			break;
		      }
		    //cout<<"variable "<< j+1 <<" is the same"<<endl;
		    count++;
		  }
		
		if (count==nVar)
		  {
		    structureID=existingStrID[i];
		    cout<<"structure " << structName
			<<" is found in database with ID "<<structureID<<endl;
		    same=1;
		    break;
		  }
	      }//end if equal size structures
	    mysql_free_result(result);
	  }
	else   // something wrong
	  {
	    cerr << "no result, Error: " <<  mysql_error(&mysql) << endl;
	    mysql_close(&mysql);
	    return;
	  }
      }
    
  }//end of loop over all existing structure IDs

if (same==0)//we have to insert this structure
  {
    // add struct to structures table
    
    Query.seekp(0);
    Query << "INSERT INTO structures SET name=\""<<structName
	  <<"\", version="<<maxVersion+1
	  <<", sizeOfStruct="<<sizeOfStruct
	  <<", nElements="<<nVar<<ends; 
    
    //cout << "database query: " << Query.str() << endl;
    if (mysql_real_query(&mysql,Query.str(),Query.pcount()-1))
      {
	cerr<<"ERROR: Failed to insert, " <<  mysql_error(&mysql) << endl;
	cerr << "database query: " << Query.str() << endl;
	mysql_close(&mysql);
	return;
      }
    else
      {
	structureID = mysql_insert_id(&mysql);	
	cout << "new strID: " << structureID << endl;
      }    
    //insert parameters here
    for (j=0;j<nVar;j++)
      {
	Query.seekp(0);
	Query << "INSERT INTO headers SET strID=\""<<structureID
	      <<"\", name=\""<<d[j].name
	      <<"\", type=\""<<StDbBroker::GetTypeName(d[j].type)
	      <<"\", nDims=\""<<d[j].dimensions//ENUM has to be as string
	      <<"\", firstDim="<<d[j].firstDimension
	      <<", offset="<<d[j].offset
	      <<", comment=\""<<ends;

	//we have to escape comments
int lenComment=strlen(Comments[j]);
end = strmov(query,Query.str());
end += mysql_escape_string(end,Comments[j],lenComment);
*end++ = '\"';
	      
  //if (mysql_real_query(&mysql,Query.str(),Query.pcount()-1))
	if (mysql_real_query(&mysql,query,(end - query)))
	  {
	    cerr<<"ERROR: Failed to insert, " <<  mysql_error(&mysql) << endl;
	    *end++ = '\0';
	    cerr << "database query: " << query << endl;
	    mysql_close(&mysql);
	    return;
	  }
	else
	  {
	    //cout << "new parmeter ID: " << mysql_insert_id(&mysql)<< endl;
	  }

      }//end loop over parameters
  }//end of same==0 if

//we have the structureID, now insert the sttable instances

uint tableID=0;
Query.seekp(0);
Query << "INSERT INTO instances SET name=\""<<tableName
      <<"\", validFrom=\""<<validFrom
      <<"\", strID="<<structureID
      <<", nrows="<<nRows<<ends; 

//cout << "database query: " << Query.str() << endl;

if (mysql_real_query(&mysql,Query.str(),Query.pcount()-1))
  {
    cerr << "Failed to query: Error: " <<  mysql_error(&mysql) << endl;
    cerr << "database query: " << Query.str() << endl;
    mysql_close(&mysql);
    return;
  }
else
  {
    tableID = mysql_insert_id(&mysql);
//     cout << "new table ID: " << tableID << endl;
  }

Query.seekp(0);
Query << "INSERT INTO bytes SET instanceID="<<tableID
      <<", bytes=\""<<ends; 
//cout << "database query: " << Query.str() << endl;

//make space for escaped bin data of length*2+1

uint byteSize = sizeOfStruct*nRows;
binQuery = new char[2*byteSize+Query.pcount()];
end = strmov(binQuery,Query.str());
end += mysql_escape_string(end,(const char*)pData,byteSize);
*end++ = '\"';

if (mysql_real_query(&mysql,binQuery,(int) (end - binQuery)))
  {
      cerr << "Failed to insert binQuery: Error: "<<mysql_error(&mysql)<<endl;
      cerr << "start of query: " << Query.str() << endl;
      mysql_close(&mysql);
      return;
  }

if (existingStrID!=NULL) delete [] existingStrID;
delete [] binQuery; 
// We are done with the connection, call mysql_close() to terminate it

mysql_close(&mysql);

return;

}





