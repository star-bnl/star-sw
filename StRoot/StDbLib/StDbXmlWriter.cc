#include "StDbXmlWriter.h"

ClassImp(StDbXmlWriter)

//----------------------------------------------------

void
StDbXmlWriter::streamHeader(const char* name){
  *os << "<StDbClass> StDb_"<<name<< endl;
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
  *os << "</StDbClass>" << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, char* i,  unsigned int size){
*os << "<element String> <name> "<<name<<" </name>"<< i <<"</element String>" << endl;
}



//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, int& i,  unsigned int size){
*os << "<element Int> <name> "<<name<<" </name>"<< i <<"</element Int>" << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, int* i,  unsigned int size){

  *os << "<element IntArray> <name>" << name << "</name> <length> ";
  *os << size << "</length>" << endl;

  int d = 0;
  int len = sizeof(d);
  int iend = (int)(size/len);

  int icount = 0;
  for(int j=0; j<iend-1;j++){ 
    *os << i[j] << ", ";
    icount++;
    if(icount==10){
      *os << endl;
      icount = 0;
    }
  }   

  *os << i[iend-1] << endl;
  *os << "</element IntArray> " << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, float& i,  unsigned int size){
*os << "<element Float> <name> "<<name<<" </name>"<< i <<"</element Float>" << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, float* i,  unsigned int size){

  *os << "<element FloatArray> <name>" << name << "</name> <length> ";
  *os << size << "</length>" << endl;

  float d = 0;
  int len = sizeof(d);
  int iend = (int)(size/len);

  int icount = 0;
  for(int j=0; j<iend-1;j++){ 
    *os << i[j] << ", ";
    icount++;
    if(icount==10){
      *os << endl;
      icount = 0;
    }
  }   

  *os << i[iend-1] << endl;
  *os << "</element FloatArray> " << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, double& i,  unsigned int size){
*os << "<element Double> <name> "<<name<<" </name>"<< i <<"</element Double>" << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, double* i,  unsigned int size){

  *os << "<element DoubleArray> <name>" << name << "</name> <length> ";
  *os << size << "</length>" << endl;

  double d = 0;
  int len = sizeof(d);
  int iend = (int)(size/len);

  int icount = 0;
  for(int j=0; j<iend-1;j++){ 
    *os << i[j] << ", ";
    icount++;
    if(icount==10){
      *os << endl;
      icount = 0;
    }
  }   

  *os << i[iend-1] << endl;
  *os << "</element DoubleArray> " << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, long& i,  unsigned int size){
*os << "<element Long> <name> "<<name<<" </name>"<< i <<"</element Long>" << endl;
}

//----------------------------------------------------

void
StDbXmlWriter::pass(char* name, long* i,  unsigned int size){

  *os << "<element LongArray> <name>" << name << "</name> <length> ";
  *os << size << "</length>" << endl;

  long d = 0;
  int len = sizeof(d);
  int iend = (int)(size/len);

  int icount = 0;
  for(int j=0; j<iend-1;j++){ 
    *os << i[j] << ", ";
    icount++;
    if(icount==10){
      *os << endl;
      icount = 0;
    }
  }   

  *os << i[iend-1] << endl;
  *os << "</element LongArray> " << endl;
}


//----------------------------------------------------







