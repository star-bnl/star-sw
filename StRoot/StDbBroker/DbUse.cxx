/***************************************************************************
 *
 * $Id: DbUse.cxx,v 1.14 2016/05/24 17:44:16 dmitry Exp $
 *
 * Author: S. Vanyashin
 ***************************************************************************
 *
 * Description: low-level C-code to read from  "params" database
 *              
 *
 ***************************************************************************
 *
 * $Log: DbUse.cxx,v $
 * Revision 1.14  2016/05/24 17:44:16  dmitry
 * first batch of fixes for Coverity findings
 *
 * Revision 1.13  2012/06/11 14:33:19  fisyak
 * std namespace
 *
 * Revision 1.12  2007/05/16 22:47:54  deph
 * Replaced cerr with LOG_ERROR <<endm; for logger
 *
 * Revision 1.11  2005/12/06 21:33:03  deph
 * clean up to remove warnings
 *
 * Revision 1.10  2003/09/02 17:55:35  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.9  2000/04/13 20:22:56  porter
 * - reconnected tableDescriptor that had been broken via St_tableDescriptor.
 * - added unix timestamp as standard
 * - top node returned via InitConfig will be a database type
 *
 * Revision 1.8  2000/03/26 16:47:13  fine
 * Adjusted to ROOT 2.24
 *
 * Revision 1.7  2000/01/31 17:11:18  porter
 * fix break caused by the interaction design between
 * 'StRoot/St_base/tableDescriptor.h' & 'StDbBroker::Descriptor'
 * Now  StDbBroker::Descriptor==tableDescriptor_st
 * And  StDbBroker::GetTableDescriptor() returns abstract StTableDescriptorI*
 * Interface to StDbLib is (and was) handle correctly.
 * StDbBroker is now tied to StRoot/St_base via tableDescriptor.h
 * No problems would have occured if St_base interactions were based
 * on StTableDesciptorI in the first place.
 *
 * Revision 1.6  2000/01/10 20:31:16  porter
 * modified StDbBroker to be an interface to the DB-interface, StDbLib.
 *  - old functionality is retained for the short-term & modifications
 *    are extensions
 *
 *
 **************************************************************************/
// idea: to get data from database
// DbUse allocate memory for the DbData array of fetched structures
// returning number of structures that was found
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// database data are stored in binary LITTLE_ENDIAN format              //
// on BIG_ENDIAN machines byte swapping has to be done here             //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Stiostream.h"
#include "Stsstream.h"
#include "mysql.h"
#include "mysql_com.h"
#include "DbEndian.h"
#include "StMessMgr.h"
#include "StDbBroker.h"

extern "C" void * DbUse(uint *nRows,
			uint *datetime,
			const char * tableName,
			const char * structName,
			uint nVar,
			uint sizeOfStruct,
			StDbBroker::oldDescriptor *d) 
{
  enum EColumnType {kNAN, kFloat, kInt, kLong, kShort, kDouble, kUInt
		    ,kULong, kUShort, kUChar, kChar };

//cout << "DbUse, event date time " << datetime[0]<<" "<< datetime[1] <<endl;
//   cout << "DbUse, structure: "<<structName<<", table: "<<tableName<< endl;
//   cout << " nVar"<<nVar<<", sizeOfStruct "<<sizeOfStruct<< endl;

  uint j;
uint count;

MYSQL mysql;
MYSQL_RES *result;
MYSQL_ROW row;

unsigned int num_fields;
unsigned int num_rows;
//unsigned int num_struct;

//const int MAXBUF=1024;
ostrstream Query;
char temps[128];

char validFrom[20];
char currentDateTime[20];
char time[7];

sprintf(currentDateTime,"%.8d",datetime[0]);
sprintf(time,"%.6d",datetime[1]);
strcat(currentDateTime,time);

currentDateTime[19]='\0';
currentDateTime[18]=currentDateTime[13];
currentDateTime[17]=currentDateTime[12];
currentDateTime[16]=':';
currentDateTime[15]=currentDateTime[11];
currentDateTime[14]=currentDateTime[10];
currentDateTime[13]=':';
currentDateTime[12]=currentDateTime[9];
currentDateTime[11]=currentDateTime[8];
currentDateTime[10]=' ';
currentDateTime[9]=currentDateTime[7];
currentDateTime[8]=currentDateTime[6];
currentDateTime[7]='-';
currentDateTime[6]=currentDateTime[5];
currentDateTime[5]=currentDateTime[4];
currentDateTime[4]='-';

mysql_init(&mysql);

//set timout in seconds for bnl.local domain

//on sun:
//Program received signal SIGBUS, Bus error.
//0xed737d68 in mysql_options ()

//mysql_options(&mysql,MYSQL_OPT_CONNECT_TIMEOUT,"4");

// Establish a connection to the MySQL database engine 

char *dbName=new char[strlen("params")+1]; strcpy(dbName,"params");
//only db1 is visible from rcas0202 machine
char *dbHost=new char[strlen("db1.star.bnl.gov")+1];strcpy(dbHost,"db1.star.bnl.gov");
//char *dbHost="duvall.star.bnl.gov";

//if (!mysql_real_connect(&mysql,"localhost","","",dbName,0,"/tmp/mysql.sock",0))
//mysql_real_connect(MYSQL *mysql, const char *host, const char *user, const char *passwd, const char *db, uint port, const char *unix_socket, uint client_flag) 
//if (!mysql_real_connect(&mysql,"duvall.star.bnl.gov","","",dbName,0,NULL,0))

if (!mysql_real_connect(&mysql,dbHost,"","",dbName,0,NULL,0))
   {
     LOG_ERROR << "Failed to connect to database: Error: "
 	 <<  mysql_error(&mysql) << endm;
     *nRows=0;
     return NULL;
   }

//check if the instance+structure pair already exist

Query.seekp(0);
if(strlen(tableName)>0)
  {
    //find latest date
    Query << "SELECT DISTINCT MAX(instances.validFrom) FROM instances, structures WHERE instances.strID=structures.ID AND instances.name=\"" << tableName << "\" AND structures.name=\"" << structName << "\" AND instances.validFrom<\""<<currentDateTime<<"\" GROUP BY instances.name"<< std::ends;
// cout << "database query: " << Query.str() << endl;
  }
else
  {
    LOG_ERROR<<"ERROR: Zero length for Table Name is not allowed"<<endm;
    return NULL;
  }

uint num_latest;
int latestDirDate;
int latestDirTime;

if (mysql_real_query(&mysql,Query.str(),Query.pcount()-1))
  {
    LOG_ERROR << "Failed to query: Error: " <<  mysql_error(&mysql) << endm;
    mysql_close(&mysql);
    return NULL;
  }
else // query succeeded, get result
  {
    result = mysql_store_result(&mysql);
    if (result) 
      {
	num_fields = mysql_num_fields(result);
	if (num_fields!=1) LOG_ERROR << "ERROR: wrong size of LATEST query"<<endm;

	num_latest = mysql_num_rows(result);

	if (num_latest==0)// found nothing
	  {
  	    LOG_ERROR << "INFO: db " << dbName << " on host "<< dbHost 
		 << " has no struct+table pair "<<structName<<"+"<< tableName 
 		 << " valid for "<<currentDateTime <<endm;
	    mysql_close(&mysql);
	    return NULL;
	  }
	else   // this structure name already exists
	  {
	if (num_latest>1) LOG_ERROR << "ERROR: found more than one latest date"
			       << tableName << endm;

// 	    cout<<"Found "<<num_latest<<" struct "<<structName<<" for table "<<tableName<<endl;
		row = mysql_fetch_row(result);
//convert hhmmss from: 1999-06-17 12:48:33
                strncpy(validFrom,row[0],19);validFrom[19]='\0';
                //start from blank at position row[0][10] 
                int ic=10;
                for(int i3=0;i3<3;i3++,++ic) {
		  for(int i2=0;i2<2;i2++,++ic) {
		    temps[ic]=row[0][ic];
		  }
		}
 		temps[6]='\0';
		latestDirTime = atoi(temps);
        
  		//get date from: 1999-06-17 12:48:33

                strncpy(temps,validFrom,10);
                temps[4]=temps[5];
                temps[5]=temps[6];
                temps[6]=temps[8];
                temps[7]=temps[9];
		temps[8]='\0';

		latestDirDate = atoi(temps);
	  }
	mysql_free_result(result);
      }
    else   // query OK but, no result?!
      {
	LOG_ERROR << "no result: Error: " <<  mysql_error(&mysql) << endm;
     mysql_close(&mysql);
     return NULL;
      }
  }

    //find if there are many entries with the latest date
Query.seekp(0);
    Query << "SELECT DISTINCT instances.ID, instances.nRows, instances.strID, structures.sizeOfStruct, structures.nElements FROM instances, structures WHERE instances.strID=structures.ID AND instances.name=\"" << tableName << "\" AND structures.name=\"" << structName << "\" AND instances.validFrom=\""<<validFrom<<"\" ORDER BY instances.entered"<< std::ends;

//cout << "database query: " << Query.str() << endl;

uint num_instances=99999;
int latestDirID=99999;
int latestStrID=99999;
uint sizeOfDbStruct=99999;
uint nDbVar=99999;

if (mysql_real_query(&mysql,Query.str(),Query.pcount()-1))
  {
    LOG_ERROR << "Failed to query: Error: " <<  mysql_error(&mysql) << endm;
     mysql_close(&mysql);
     return NULL;
  }
else // query succeeded, get result
  {
    result = mysql_store_result(&mysql);
    if (result) 
      {
	num_fields = mysql_num_fields(result);
	if (num_fields!=5) LOG_ERROR << "ERROR: wrong size of latest entries query"<<endm;

	num_instances = mysql_num_rows(result);

	if (num_instances==0)// found nothing
	  {
            LOG_ERROR << "ERROR: db " << dbName << " has lost the struct+table pair "
                 <<structName<<"+"<< tableName << endm;
	    mysql_close(&mysql);
	    return NULL;
	  }
	else 
	  {
	    if (num_instances>1)// bad
	      {
		LOG_ERROR << "INFO: db " << dbName << " has more then one struct+table pair "
		     <<structName<<"+"<< tableName 
		     << " valid for "<<currentDateTime
		     <<", the LAST INSERTED is used"<<endl;
		LOG_ERROR << "database query: " << Query.str() << endm;
	      }
            for (uint id=0;id<num_instances;id++)
              {
                row = mysql_fetch_row(result);
		latestDirID = atoi(row[0]);
		*nRows = (uint) atoi(row[1]);
		latestStrID = atoi(row[2]);
		sizeOfDbStruct = (uint)atoi(row[3]);
		nDbVar = (uint)atoi(row[4]);
              }
	  }
	mysql_free_result(result);
      }
    else   // query OK but, no result?!
      {
	LOG_ERROR << "no result: Error: " <<  mysql_error(&mysql) << endm;
	mysql_close(&mysql);
	return NULL;
      }
  }

Query.seekp(0);

if(sizeOfDbStruct!=sizeOfStruct)
  {
    LOG_ERROR<<"ERROR: DB struct byteSize is "<<sizeOfDbStruct<<endm;
    LOG_ERROR<<"     user struct byteSize is "<<sizeOfStruct<<endm;
    LOG_ERROR << "structure: "<<structName<<endm;
    *nRows=0;
    return NULL;
  }

if(nVar!=nDbVar)
  {
    LOG_ERROR << "ERROR: DB struct nVariables is "<<nDbVar<<endm;
    LOG_ERROR << "     user struct nVariables is "<<nVar<<endm; 
    LOG_ERROR << "structure: "<<structName<<endm;
   *nRows = 0;
    return NULL;
  }

//to compare header file info in db and descriptor fetch db header info
char **types =0;
char **names =0;
int *offset=0;
int *nDims=0;
int *firstDim=0;


Query.seekp(0);
Query<<"SELECT name, type, offset, nDims, firstDim FROM headers WHERE strID="
     << latestStrID << " ORDER BY offset" << std::ends;
  //cout << "database query: " << Query.str() << endl;

if (mysql_real_query(&mysql,Query.str(),Query.pcount()-1))
  {
    LOG_ERROR << "Failed to query: Error: " << mysql_error(&mysql) << endm;
    mysql_close(&mysql);
    return NULL;
  }
else // query succeeded, get result
  {
    result = mysql_store_result(&mysql);
    if (result)// query OK
      {
	num_fields = mysql_num_fields(result);
	if (num_fields!=5) LOG_ERROR<<"ERROR: wrong size of headers query"<<endm;
	
	num_rows = mysql_num_rows(result);
	//        cout<<"total variables: "<< num_rows <<endl;
	
	if (num_rows!=nVar) //size is different from this ID
	  {
	    cout<<"WARNING: database structure is of "<< num_rows
		<<" variables, this structure is "<< nVar <<endl;
	  }
	
	types  = new char*[num_rows];
	names  = new char*[num_rows];
	offset   = new int[num_rows];
	nDims    = new int[num_rows];
	firstDim = new int[num_rows];
	
	for (j=0;j<num_rows;j++)
	  {
	    row = mysql_fetch_row(result);
	    
	    names[j] = strdup(row[0]);
	    types[j] = strdup(row[1]);
	    offset[j] = atoi(row[2]);
	    nDims[j] = atoi(row[3]);
	    firstDim[j] = atoi(row[4]);
	    
	    //cout<<"name: \""<<row[0]<<"\""<<endl;
	    //cout<<"type: \""<<row[1]<<"\""<<endl;
	    //cout<<"offset: "<<offset[j]<<endl;
	    //cout<<"nDims: "<<nDims[j]<<endl;
	    //cout<<"firstDim: "<<firstDim[j]<<endl;
	  }
	mysql_free_result(result);
      }
  }

//now compare header file info in db and descriptor

//same variables counter
count = 0;

for (j=0;j<nVar;j++)
  {
    if (strcmp(d[j].fColumnName,names[j]))
      break;
    
    //cout<<"offset: \""<<d[j].offset<<"\" \""<<row[1]<<"\""<<endl;
    if ( d[j].fOffset!=(unsigned int)offset[j] )
      break;
    
    //same offset: check nDims
    //cout<<"nDims: \""<<d[j].dimensions<<"\" \""<<row[2]<<"\""<<endl;
    if ( d[j].fDimensions!=(unsigned int)nDims[j] )
      break;
    
    //same nDims: check firstDim
    //cout<<"nDims: \""<<d[j].firstDimension<<"\" \""<<row[3]<<"\""<<endl;
    if ( d[j].fIndexArray[0]!=(unsigned int)firstDim[j] )
      break;
    
    //cout<<"variable "<< j+1 <<" is the same"<<endl;
    count++;
  }

if (count!=nVar)
  {
    LOG_ERROR<<"structure " << structName
	<<" is not compatible with database instance ID "<<latestDirID<<endm;
    LOG_ERROR <<"nVar "<<nVar<<", count "<<count<<endm;

    for (j=0;j<nVar;j++)
      {
	LOG_ERROR<<"names: \""<<d[j].fColumnName<<"\" \""<<names[j]<<"\""<<endm;
	if (strcmp(d[j].fColumnName,names[j]))
	  break;
	
	LOG_ERROR<<"offset: "<<d[j].fOffset<<" "<<offset[j]<<endm;
	if ( d[j].fOffset!=(unsigned int)offset[j] )
	  break;
	
	LOG_ERROR<<"nDims: "<<d[j].fDimensions<<" "<<nDims[j]<<endm;
	if ( d[j].fDimensions!=(unsigned int)nDims[j] )
	  break;

	LOG_ERROR<<"firstDim: "<<d[j].fIndexArray[0]<<" "<<firstDim[j]<<endm;
	if ( d[j].fIndexArray[0]!=(unsigned int)firstDim[j] )
	  break;
      }
    
    *nRows=0;
	delete [] types;
	delete [] names;
	delete [] offset;
	delete [] nDims;
	delete [] firstDim;
    return NULL;
  }

//char* pCurrent=new char[num_struct*sizeOfStruct];
//void * pDbData = (void*) pCurrent;
//Valeri Fine need's malloc() for ROOT free()
//void* pCurrent = malloc((size_t) num_struct*sizeOfStruct);
//use calloc() for array of structs?
void* pDbData = calloc((size_t) *nRows, (size_t) sizeOfDbStruct);

if (pDbData==NULL){
  LOG_ERROR<<"DbUse: failed to allocate memory nedeed for the array of "
      << *nRows <<" structs " <<structName <<" each of "
      <<sizeOfStruct <<" bytes"<<endm;
  *nRows=0;
	delete [] types;
	delete [] names;
	delete [] offset;
	delete [] nDims;
	delete [] firstDim;
	free(pDbData);
  return NULL;
}

char* pCurrent = (char*) pDbData;

uint num_blobs;
unsigned long *lengths;

Query.seekp(0);
Query << "SELECT bytes FROM bytes WHERE instanceID="<<latestDirID<< std::ends;
 
// cout << "database query: " << Query.str() << endl;

if (mysql_real_query(&mysql,Query.str(),Query.pcount()-1))
  {
    LOG_ERROR << "Failed to query: Error: " <<  mysql_error(&mysql) << endm;
    LOG_ERROR << "database query: " << Query.str() << endm;
     mysql_close(&mysql);
     return NULL;
  }
else // query succeeded, get result
  {
    result = mysql_store_result(&mysql);
    if (!result)   // query OK but, no result?!
      {
	LOG_ERROR << "no result: Error: " <<  mysql_error(&mysql) << endm;
	mysql_close(&mysql);
	delete [] types;
	delete [] names;
	delete [] offset;
	delete [] nDims;
	delete [] firstDim;
	free(pDbData);
	pCurrent = 0;

	return NULL;
      }
    else 
      {
	num_fields = mysql_num_fields(result);
	if (num_fields!=1) LOG_ERROR << "ERROR: wrong size of blob query"<<endm;

	num_blobs = mysql_num_rows(result);

	if (num_blobs==0)// found nothing
	  {
  	    LOG_ERROR << "ERROR: db " << dbName << " has lost BLOB for struct+table pair "
  		 <<structName<<"+"<< tableName 
 		 << " valid for "<<currentDateTime <<endm;
	    mysql_close(&mysql);
		delete [] types;
		delete [] names;
		delete [] offset;
		delete [] nDims;
		delete [] firstDim;
		free(pDbData);
		pCurrent = 0;
	    return NULL;
	  }
	else
	  {
	    if (num_blobs>1) LOG_ERROR << "ERROR: found more than one BLOB for "
				  <<tableName<<endm;
	    //this is the place where data are fethched
	    row = mysql_fetch_row(result);

	    if (row)
	      {
		lengths = mysql_fetch_lengths(result);
		
		//cout << "blob size "<<lengths[0]<<" for table "<<tableName<<endl;
		if (lengths[0]!=(*nRows)*sizeOfDbStruct)
		  {
		    LOG_ERROR << "ERROR: wrong blob size "
			 <<tableName<<endm;
		    LOG_ERROR <<"lengths[0] "<<lengths[0]
			 <<", nRows "<<(*nRows)
			 <<", sizeOfDbStruct "<<sizeOfDbStruct
			 <<", nRows*sizeOfDbStruct "<<(*nRows)*sizeOfDbStruct
			 <<endm;
		    mysql_close(&mysql);
		    *nRows=0;

			delete [] types;
			delete [] names;
			delete [] offset;
			delete [] nDims;
			delete [] firstDim;
			free(pDbData);
		    return NULL;
		  }
		
#ifndef BIG_ENDIAN		
		memcpy(pCurrent,row[0],(size_t) (*nRows)*sizeOfDbStruct);
#else // do byte swapping 
		//memset(pCurrent,0,(size_t) (*nRows)*sizeOfDbStruct);
		uint i, k, nTimes, firstByte, firstDbByte;
		//convert to user type (not Db type)

		for (i=0;i<*nRows;i++)
		  {
		    for (j=0;j<nVar;j++)
		      {
			
			switch(d[j].fDimensions)
			  {
			  case 0:
			    nTimes=1;
			    break;
			  case 1:
			    nTimes=d[j].fIndexArray[0];
			    break;
			  case 2:
			    LOG_ERROR<<"two-dims not handled yet"<<endm;
			    nTimes=0;
			    //nTimes=d[j].firstDimension*d[j].secondDimension;
			    break;
			  default:
			    LOG_ERROR << "ERROR: more that one dimension "<<endm;
			    nTimes=0;
			    break;
			  }
			
			//from db offset to user offset
			firstByte=i*sizeOfStruct+d[j].fOffset;
			firstDbByte=i*sizeOfDbStruct+offset[j];
			
			for (k=0;k<nTimes;k++) {

			  //MYSQL_ROW row is just char**
			  switch((StDbBroker::EColumnType)d[j].fType)
			    {
			    case kUChar:
			    case kChar:
			      pCurrent[firstByte+k]=row[0][firstDbByte+k];
			      break;
			      
			    case kShort:
			    case kUShort:
			      pCurrent[firstByte+2*k+1]=row[0][+2*k  ];
			      pCurrent[firstByte+2*k  ]=row[0][firstDbByte+2*k+1];
			      break;
			      
			    case kInt:
			    case kUInt:
			      
			    case kULong:
			    case kLong:
			      
			    case kFloat:
			      
			      pCurrent[firstByte+4*k+3]=row[0][firstDbByte+4*k  ];
			      pCurrent[firstByte+4*k+2]=row[0][firstDbByte+4*k+1];
			      pCurrent[firstByte+4*k+1]=row[0][firstDbByte+4*k+2];
			      pCurrent[firstByte+4*k  ]=row[0][firstDbByte+4*k+3];
			      break;
			      
			    case kDouble:
			      //and long long
			      pCurrent[firstByte+8*k+7]=row[0][firstDbByte+8*k  ];
			      pCurrent[firstByte+8*k+6]=row[0][firstDbByte+8*k+1];
			      pCurrent[firstByte+8*k+5]=row[0][firstDbByte+8*k+2];
			      pCurrent[firstByte+8*k+4]=row[0][firstDbByte+8*k+3];
			      pCurrent[firstByte+8*k+3]=row[0][firstDbByte+8*k+4];
			      pCurrent[firstByte+8*k+2]=row[0][firstDbByte+8*k+5];
			      pCurrent[firstByte+8*k+1]=row[0][firstDbByte+8*k+6];
			      pCurrent[firstByte+8*k  ]=row[0][firstDbByte+8*k+7];
			      break;
			      
			    case kNAN:
			    default:
			  LOG_ERROR << "ERROR: unknown type!"<<endm;
			      break;
			    }
			}//end loop over array variable
		      }//end loop over variables
		  }//end loop over nRows
#endif
	      }
	  }
	mysql_free_result(result);
      }
  }
 
// select just one table with the smallest date after the datetime 
Query.seekp(0);
    //find latest date
    Query << "SELECT DISTINCT MIN(instances.validFrom) FROM instances, structures WHERE instances.strID=structures.ID AND instances.name=\"" << tableName << "\" AND structures.name=\"" << structName << "\" AND instances.validFrom>\""<<currentDateTime<<"\" GROUP BY instances.name"<<std::ends;

// cout << "database query: " << Query.str() << endl;

uint num_next;
int nextDirDate;
int nextDirTime;

if (mysql_real_query(&mysql,Query.str(),Query.pcount()-1))
  {
    LOG_ERROR << "Failed to query: Error: " <<  mysql_error(&mysql) << endm;
     mysql_close(&mysql);
     return NULL;
  }
else // query succeeded, get result
  {
    result = mysql_store_result(&mysql);
    if (result) 
      {
	num_fields = mysql_num_fields(result);
	if (num_fields!=1) LOG_ERROR << "ERROR: wrong size of next date query"<<endm;

	num_next = mysql_num_rows(result);

	if (num_next==0)// found nothing no next table = no validity limit
	  {
//  	    cout << "INFO: db " << dbName << " has no struct+table pair "
//  		 << structName<<"+"<< tableName 
// 		 << " limiting validity for "<<setw(8)<<datetime[0]<<setw(6)<<datetime[1]<<endl;
	    nextDirDate=20380101;
	    nextDirTime=0;
	  }
	else   //validity limit exists
	  {
	    if (num_next>1) LOG_ERROR << "ERROR: found more than one next date"
				 <<tableName<<endm;
//  	    cout<<"Found next "<<num_next<<" struct "<<structName<<" for table "<<tableName<<endl;

		row = mysql_fetch_row(result);


//convert hhmmss from: 1999-06-17 12:48:33
                strncpy(validFrom,row[0],19);validFrom[19]='\0';
                //start from blank at position row[0][10] 
                int ic=10;
                for(int i3=0;i3<3;i3++,++ic) {
		  for(int i2=0;i2<2;i2++,++ic) {
		    temps[ic]=row[0][ic];
		  }
		}
 		temps[6]='\0';
		nextDirTime = atoi(temps);
        
  		//get date from: 1999-06-17 12:48:33
                strncpy(temps,validFrom,10);
                temps[4]=temps[5];
                temps[5]=temps[6];
                temps[6]=temps[8];
                temps[7]=temps[9];
		temps[8]='\0';

		nextDirDate = atoi(temps);
	  }
	mysql_free_result(result);
      }
    else   // query OK but, no result?!
      {
	LOG_ERROR << "no result: Error: " <<  mysql_error(&mysql) << endm;
	mysql_close(&mysql);
	delete [] types;
	delete [] names;
	delete [] offset;
	delete [] nDims;
	delete [] firstDim;
	free(pDbData);
	pCurrent = 0;
	return NULL;
      }
  }

// We are done with the connection, call mysql_close() to terminate it

mysql_close(&mysql);

delete [] types;
delete [] names;
delete [] offset;
delete [] nDims;
delete [] firstDim;

//fill return datetime values 

datetime[0] = latestDirDate;
datetime[1] = latestDirTime;
datetime[2] = nextDirDate;
datetime[3] = nextDirTime;

// cout<<"Begin "<<datetime[0]<<" "<<datetime[1]<<endl;
// cout<<"End   "<<datetime[2]<<" "<<datetime[3]<<endl;

//cout <<"nRows = "<<*nRows<<endl;    
return pDbData;

}


