#include "StDbXmlWriter.h"


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
StDbXmlWriter::streamTableName(const char* name){
  *os << "<dbTable>"<<name << endl;
}
//----------------------------------------------------

void
StDbXmlWriter::streamEndTableName(){
  *os << "</dbTable>"<< endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, char* i,  int len){
*os << "<dbString> "<<name<<" <value> "<< i <<" </value> </dbString>" << endl;
}

void
StDbXmlWriter::pass(char* name, unsigned char* i,  int len){
*os << "<dbUChar> "<<name<<" <value> "<< i <<" </value> </dbUChar>" << endl;
}


//----------------------------------------------------
//----------------------------------------------------
void
StDbXmlWriter::pass(char* name, short& i,  int len){
*os << "<dbShort> "<<name<<" <value> "<< i <<" </value> </dbShort>" << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, short* i,  int len){

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
StDbXmlWriter::pass(char* name, unsigned short& i,  int len){
*os << "<dbUShort> "<<name<<" <value> "<< i <<" </value> </dbUShort>" << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, unsigned short* i,  int len){

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
StDbXmlWriter::pass(char* name, int& i,  int len){
*os << "<dbInt> "<<name<<" <value> "<< i <<" </value> </dbInt>" << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, int* i,  int len){

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
StDbXmlWriter::pass(char* name, unsigned int& i,  int len){
*os << "<dbUInt> "<<name<<" <value> "<< i <<" </value> </dbUInt>" << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, unsigned int* i,  int len){

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
StDbXmlWriter::pass(char* name, float& i,  int len){
*os << "<dbFloat> "<<name<<" <value> "<< i <<" </value> </dbFloat>" << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, float* i,  int len){

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
StDbXmlWriter::pass(char* name, double& i,  int len){
*os << "<dbDouble> "<<name<<" <value> "<< i <<" </value> </dbDouble>" << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, double* i,  int len){

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
StDbXmlWriter::pass(char* name, long& i,  int len){
*os << "<dbLong> "<<name<<" <value> "<< i <<" </value> </dbLong>" << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, long* i,  int len){

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
StDbXmlWriter::pass(char* name, unsigned long& i,  int len){
*os << "<dbULong> "<<name<<" <value> "<< i <<" </value> </dbULong>" << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, unsigned long* i,  int len){

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













