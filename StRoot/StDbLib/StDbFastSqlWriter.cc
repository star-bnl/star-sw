#include "StDbFastSqlWriter.h"
#include "StDbTable.h"

void
StDbFastSqlWriter::ioTable(StDbTable* table){

  table->setRowNumber();
  int nrows=table->GetNRows();
  for(int k=0;k<nrows;k++){
    if(k>0)*os<<"),(";
    *os<<table->getBeginTime();
    table->dbStreamer((typeAcceptor*)this,false);
  }

}

void
StDbFastSqlWriter::pass(char* name, unsigned char*& i, int& len){
  *os<<",'"<<(int)i[0];
  for(int j=1;j<len;j++)*os<<","<<(int)i[j];
  *os<<"'";
}

void
StDbFastSqlWriter::pass(char* name, short*& i, int& len){
  *os<<",'"<<i[0];
  for(int j=1;j<len;j++)*os<<","<<i[j];
  *os<<"'";
}

void
StDbFastSqlWriter::pass(char* name, int*& i, int& len){
  *os<<",'"<<i[0];
  for(int j=1;j<len;j++)*os<<","<<i[j];
  *os<<"'";
}

void
StDbFastSqlWriter::pass(char* name, long*& i, int& len){
  *os<<",'"<<i[0];
  for(int j=1;j<len;j++)*os<<","<<i[j];
  *os<<"'";
}


void
StDbFastSqlWriter::pass(char* name, unsigned short*& i, int& len){
  *os<<",'"<<i[0];
  for(int j=1;j<len;j++)*os<<","<<i[j];
  *os<<"'";
}

void
StDbFastSqlWriter::pass(char* name, unsigned int*& i, int& len){
  *os<<",'"<<i[0];
  for(int j=1;j<len;j++)*os<<","<<i[j];
  *os<<"'";
}

void
StDbFastSqlWriter::pass(char* name, unsigned long*& i, int& len){
  *os<<",'"<<i[0];
  for(int j=1;j<len;j++)*os<<","<<i[j];
  *os<<"'";
}

void
StDbFastSqlWriter::pass(char* name, long long*& i, int& len){
  *os<<",'"<<i[0];
  for(int j=1;j<len;j++)*os<<","<<i[j];
  *os<<"'";
}

void
StDbFastSqlWriter::pass(char* name, float*& i, int& len){
  *os<<",'"<<i[0];
  for(int j=1;j<len;j++)*os<<","<<i[j];
  *os<<"'";
}

void
StDbFastSqlWriter::pass(char* name, double*& i, int& len){
  *os<<",'"<<i[0];
  for(int j=1;j<len;j++)*os<<","<<i[j];
  *os<<"'";
}


