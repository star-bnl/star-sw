/***************************************************************************
 *
 * $Id: StDbXmlWriter.cc,v 1.10 2016/05/27 16:26:23 dmitry Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  implement typeAcceptor for WRITING XML files of DB-tables
 *
 ***************************************************************************
 *
 * $Log: StDbXmlWriter.cc,v $
 * Revision 1.10  2016/05/27 16:26:23  dmitry
 * more coverity
 *
 * Revision 1.9  2016/05/26 15:15:30  dmitry
 * unused variable in unused code - commented out
 *
 * Revision 1.8  2015/05/15 19:37:11  dmitry
 * fixed type issue, as signed int was assumed
 *
 * Revision 1.7  2001/10/24 04:05:20  porter
 * added long long type to I/O and got rid of obsolete dataIndex table
 *
 * Revision 1.6  2000/01/10 20:37:55  porter
 * expanded functionality based on planned additions or feedback from Online work.
 * update includes:
 * 	1. basis for real transaction model with roll-back
 * 	2. limited SQL access via the manager for run-log & tagDb
 * 	3. balance obtained between enumerated & string access to databases
 * 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
 * 	5. restructured Node model for better XML support
 *
 * Revision 1.5  1999/12/03 17:03:24  porter
 * added multi-row support for the Xml reader & writer
 *
 * Revision 1.4  1999/09/30 02:06:12  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/

#include "StDbXmlWriter.h"
#include "StDbTable.h"

//----------------------------------------------------

void
StDbXmlWriter::streamHeader(const char* name){
  *os << "<StDbName>"<<name<< endl;
}

//----------------------------------------------------

void
StDbXmlWriter::streamAccessor(){
  *os << "<StDbAccessor>" << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::endAccessor(){
  *os << "</StDbAccessor>" << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::streamTail(){
  *os << "</StDbName>" << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::streamRow(int row){
  *os << "<TabRow>" << row << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::streamEndRow(){
  *os << "</TabRow>" << endl;
}
//----------------------------------------------------

void
StDbXmlWriter::streamTableName(const char* name){
  *os << "<StDbTable>"<<name << endl;
}
//----------------------------------------------------

void
StDbXmlWriter::streamEndTableName(){
  *os << "</StDbTable>"<< endl;
}

//----------------------------------------------------

void
StDbXmlWriter::ioTable(StDbTable* table){

    char * name = table->getName();
    streamTableName(name); delete [] name;
    streamAccessor();
    table->StreamAccessor((typeAcceptor*)this, false);
    endAccessor();

    int k;
    int nrows = 1;
//    int* elements = table->getElementID(nrows);

//    if(!elements){
//      elements = new int[nrows];
//      for(k=0;k<nrows;k++)elements[k]=k;
//    }

    table->setRowNumber(); // set to 0

    for(k=0;k<nrows;k++){    
      streamRow(k);
      table->dbStreamer((typeAcceptor*)this, false);
      streamEndRow();
    }
    streamEndTableName();
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, char*& i,  int& len){
*os << "<dbString> "<<name<<" <value> "<< i <<" </value> </dbString>" << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, unsigned char& i,  int& len){
*os << "<dbUChar> "<<name<<" <value> "<< (int)i <<" </value> </dbUChar>" << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, unsigned char*& i,  int& len){
  *os << "<dbUCharArray> " << name << " <length> ";
  *os << len << " </length>" << endl;
  *os << "<value>" << endl;

  int icount = 0;
  for(int j=0; j<len-1;j++){ 
    *os << (int)i[j] << ", ";
    icount++;
    if(icount==8){
      *os << endl;
      icount = 0;
    }
  }   

  *os << (int)i[len-1] << endl;
  *os << "</value> </dbUCharArray> " << endl;
}


//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, short& i,  int& len){
*os << "<dbShort> "<<name<<" <value> "<< i <<" </value> </dbShort>" << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, short*& i,  int& len){

  *os << "<dbShortArray> " << name << " <length> ";
  *os << len << " </length>" << endl;
  *os << "<value>" << endl;

  int icount = 0;
  for(int j=0; j<len-1;j++){ 
    *os << i[j] << ", ";
    icount++;
    if(icount==8){
      *os << endl;
      icount = 0;
    }
  }   

  *os << i[len-1] << endl;
  *os << "</value> </dbShortArray> " << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, unsigned short& i,  int& len){
*os << "<dbUShort> "<<name<<" <value> "<< i <<" </value> </dbUShort>" << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, unsigned short*& i,  int& len){

  *os << "<dbUShortArray> " << name << " <length> ";
  *os << len << " </length>" << endl;
  *os << "<value>" << endl;

  int icount = 0;
  for(int j=0; j<len-1;j++){ 
    *os << i[j] << ", ";
    icount++;
    if(icount==8){
      *os << endl;
      icount = 0;
    }
  }   

  *os << i[len-1] << endl;
  *os << "</value> </dbUShortArray> " << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, int& i,  int& len){
*os << "<dbInt> "<<name<<" <value> "<< i <<" </value> </dbInt>" << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, int*& i,  int& len){

  *os << "<dbIntArray> " << name << " <length> ";
  *os << len << " </length>" << endl;
  *os << "<value>" << endl;

  int icount = 0;
  for(int j=0; j<len-1;j++){ 
    *os << i[j] << ", ";
    icount++;
    if(icount==8){
      *os << endl;
      icount = 0;
    }
  }   

  *os << i[len-1] << endl;
  *os << "</value> </dbIntArray> " << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, unsigned int& i,  int& len){
*os << "<dbUInt> "<<name<<" <value> "<< i <<" </value> </dbUInt>" << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, unsigned int*& i,  int& len){

  *os << "<dbUIntArray> " << name << " <length> ";
  *os << len << " </length>" << endl;
  *os << "<value>" << endl;

  int icount = 0;
  for(int j=0; j<len-1;j++){ 
    *os << i[j] << ", ";
    icount++;
    if(icount==8){
      *os << endl;
      icount = 0;
    }
  }   

  *os << i[len-1] << endl;
  *os << "</value> </dbUIntArray> " << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, float& i,  int& len){
*os << "<dbFloat> "<<name<<" <value> "<< i <<" </value> </dbFloat>" << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, float*& i,  int& len){

  *os << "<dbFloatArray> " << name << " <length> ";
  *os << len << " </length>" << endl;
  *os << "<value>" << endl;

  int icount = 0;
  for(int j=0; j<len-1;j++){ 
    *os << i[j] << ", ";
    icount++;
    if(icount==8){
      *os << endl;
      icount = 0;
    }
  }   

  *os << i[len-1] << endl;
  *os << "</value> </dbFloatArray> " << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, double& i,  int& len){
*os << "<dbDouble> "<<name<<" <value> "<< i <<" </value> </dbDouble>" << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, double*& i,  int& len){

  *os << "<dbDoubleArray> " << name << " <length> ";
  *os << len << " </length>" << endl;
  *os << "<value>" << endl;

  int icount = 0;
  for(int j=0; j<len-1;j++){ 
    *os << i[j] << ", ";
    icount++;
    if(icount==8){
      *os << endl;
      icount = 0;
    }
  }   

  *os << i[len-1] << endl;
  *os << "</value> </dbDoubleArray> " << endl;
}

//----------------------------------------------------
void
StDbXmlWriter::pass(char* name, long& i,  int& len){
*os << "<dbLong> "<<name<<" <value> "<< i <<" </value> </dbLong>" << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, long*& i,  int& len){

  *os << "<dbLongArray> " << name << " <length> ";
  *os << len << " </length>" << endl;
  *os << "<value>" << endl;

  int icount = 0;
  for(int j=0; j<len-1;j++){ 
    *os << i[j] << ", ";
    icount++;
    if(icount==8){
      *os << endl;
      icount = 0;
    }
  }   

  *os << i[len-1] << endl;
  *os << "</value> </dbLongArray> " << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, unsigned long& i,  int& len){
*os << "<dbULong> "<<name<<" <value> "<< i <<" </value> </dbULong>" << endl;
}
void
StDbXmlWriter::pass(char* name, long long& i,  int& len){
*os << "<dbLongLong> "<<name<<" <value> "<< i <<" </value> </dbLongLong>" << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, unsigned long*& i,  int& len){

  *os << "<dbULongArray> " << name << " <length> ";
  *os << len << " </length>" << endl;
  *os << "<value>" << endl;

  int icount = 0;
  for(int j=0; j<len-1;j++){ 
    *os << i[j] << ", ";
    icount++;
    if(icount==8){
      *os << endl;
      icount = 0;
    }
  }   

  *os << i[len-1] << endl;
  *os << "</value> </dbULongArray> " << endl;
}
//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, long long*& i,  int& len){

  *os << "<dbLongLongArray> " << name << " <length> ";
  *os << len << " </length>" << endl;
  *os << "<value>" << endl;

  int icount = 0;
  for(int j=0; j<len-1;j++){ 
    *os << i[j] << ", ";
    icount++;
    if(icount==8){
      *os << endl;
      icount = 0;
    }
  }   

  *os << i[len-1] << endl;
  *os << "</value> </dbLongLongArray> " << endl;
}
//----------------------------------------------------













