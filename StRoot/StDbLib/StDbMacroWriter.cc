#include "StDbMacroWriter.h"

ClassImp(StDbMacroWriter)
//----------------------------------------------------

void
StDbMacroWriter::streamHeader(const char* name){

  *os << "StDbTableComponent * LoadDbTable() { " << endl;
  *os << endl;
  *os << "  StDb_"<<name<<" *table = new StDb_"<<name<<"(\""<<name<<"\");"<<endl;
  *os << endl;
  *os << "  "<<name<<"* row = new " <<name<<";"<< endl;
  *os << endl;

}

//----------------------------------------------------

void
StDbMacroWriter::streamAccessor(){

  *os << "  StDbAccessor access; " << endl;

}

//----------------------------------------------------

void
StDbMacroWriter::endAccessor(){
  *os << endl;
  *os << "  table->setAccessor(access); " << endl;
  *os << endl;
}

//----------------------------------------------------

void
StDbMacroWriter::streamTail(){

  *os << endl;
  *os << " table->addTable(row); " << endl;
  *os << " return table; " << endl;
  *os << " }; " << endl;

}

//----------------------------------------------------

void
StDbMacroWriter::pass(char* name, char* i,  unsigned int size){
  // assume char* is define as char[n] in a struct
  *os << " row."<<name<<" = " << i<<";"  << endl;
}



//----------------------------------------------------

void
StDbMacroWriter::pass(char* name, int& i,  unsigned int size){
  if(strcmp(name,"schemaID")==0 ||
     strcmp(name,"beginTime")==0 ||
     strcmp(name,"endTime")==0 ||
     strcmp(name,"version")==0 ||
     strcmp(name,"elementID")==0 ) {
    *os << "  access."<<name<<" = " << i <<";"<< endl;
  } else {
  *os << " row."<<name<<" = " << i <<";"<< endl;
  }
}

//----------------------------------------------------

void
StDbMacroWriter::pass(char* name, int* i,  unsigned int size){

  int d = 0;
  int len = sizeof(d);
  int iend = (int)(size/len);
  *os << "  int "<<name<<"["<<iend<<"] = {";
  int icount = 0;
  for(int j=0; j<iend-1;j++){ 
    *os << i[j] << ", ";
    icount++;
    if(icount==10){
      *os << endl;
      icount = 0;
    }
  }   
  *os << i[iend-1] << "};"<< endl;
  *os <<"  memcpy(row."<<name<<","<<name<<","<<size<<");"<<endl;
}

//----------------------------------------------------

void
StDbMacroWriter::pass(char* name, float& i,  unsigned int size){
  *os << " row."<<name<<" = " << i <<";"<< endl;
}

//----------------------------------------------------

void
StDbMacroWriter::pass(char* name, float* i,  unsigned int size){

  float d = 0;
  int len = sizeof(d);
  int iend = (int)(size/len);
  *os << "  float "<<name<<"["<<iend<<"] = {";
  int icount = 0;
  for(int j=0; j<iend-1;j++){ 
    *os << i[j] << ", ";
    icount++;
    if(icount==10){
      *os << endl;
      icount = 0;
    }
  }   
  *os << i[iend-1] << "};"<< endl;
  *os <<"  memcpy(row."<<name<<","<<name<<","<<size<<");"<<endl;
}

//----------------------------------------------------

void
StDbMacroWriter::pass(char* name, double& i,  unsigned int size){
  *os << " row."<<name<<" = " << i <<";"<< endl;
}

//----------------------------------------------------

void
StDbMacroWriter::pass(char* name, double* i,  unsigned int size){

  double d = 0;
  int len = sizeof(d);
  int iend = (int)(size/len);
  *os << "  double "<<name<<"["<<iend<<"] = {";
  int icount = 0;
  for(int j=0; j<iend-1;j++){ 
    *os << i[j] << ", ";
    icount++;
    if(icount==10){
      *os << endl;
      icount = 0;
    }
  }   
  *os << i[iend-1] << "};"<< endl;
  *os <<"  memcpy(row."<<name<<","<<name<<","<<size<<");"<<endl;
}


//----------------------------------------------------

void
StDbMacroWriter::pass(char* name, long& i,  unsigned int size){
  *os << " row."<<name<<" = " << i <<";"<< endl;
}

//----------------------------------------------------

void
StDbMacroWriter::pass(char* name, long* i,  unsigned int size){

  long d = 0;
  int len = sizeof(d);
  int iend = (int)(size/len);
  *os << "  long "<<name<<"["<<iend<<"] = {";
  int icount = 0;
  for(int j=0; j<iend-1;j++){ 
    *os << i[j] << ", ";
    icount++;
    if(icount==10){
      *os << endl;
      icount = 0;
    }
  }   
  *os << i[iend-1] << "};"<< endl;
  *os <<"  memcpy(row."<<name<<","<<name<<","<<size<<");"<<endl;
}



//----------------------------------------------------












