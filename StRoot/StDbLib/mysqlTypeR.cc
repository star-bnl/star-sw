#include "mysqlTypeR.hh"
#include <strings.h>

////////////////////////////////////////////////////////////////

bool 
mysqlTypeR::initArray(MYSQL_ROW row, MYSQL_FIELD* fields, int column, int max_column, unsigned long* col_length){

  if(column > max_column){
    cerr << "mysqlTypeR: Attempt to init pointer beyond array limit" << endl;
    return false;
  }

  if( row && fields ){
    mrow = row;
    mfields = fields;
    mcolumn = column;
    mnum_fields = max_column;
    mcol_length = col_length;
  } else {
    cerr << "mysqlTypeR: attempt to load with NULL pointer " << endl;
    return false;
  }

return true;
}


void
mysqlTypeR::pass(char* name, long& i,   unsigned int size){

  if(mcolumn > mnum_fields){
    cerr << "mysqlTypeR: Attempt read beyond array limit" << endl;
    return;
  }
  //  cout << mfields[mcolumn].name << " in int& " << endl;
  i = atoi(mrow[mcolumn]);
  mcolumn++;
}

void
mysqlTypeR::pass(char* name, int& i,   unsigned int size){

  if(mcolumn > mnum_fields){
    cerr << "mysqlTypeR: Attempt read beyond array limit" << endl;
    return;
  }
  //  cout << mfields[mcolumn].name << " in long& " << endl;
  i = atoi(mrow[mcolumn]);
  mcolumn++;
}

void
mysqlTypeR::pass(char* name, float& i,   unsigned int size){

  if(mcolumn > mnum_fields){
    cerr << "mysqlTypeR: Attempt read beyond array limit" << endl;
    return;
  }

  //  cout << mfields[mcolumn].name << " in float& " << endl;
  i = (float)atof(mrow[mcolumn]);
  mcolumn++;
}

void
mysqlTypeR::pass(char* name, double& i,   unsigned int size){

  if(mcolumn > mnum_fields){
    cerr << "mysqlTypeR: Attempt read beyond array limit" << endl;
    return;
  }
  //  cout << mfields[mcolumn].name << " in double& " << endl;
  i = atof(mrow[mcolumn]);
  mcolumn++;
}

void
mysqlTypeR::pass(char* name, char* i,   unsigned int size){

  if(mcolumn > mnum_fields){
    cerr << "mysqlTypeR: Attempt read beyond array limit" << endl;
    return;
  }

  //  cout << mfields[mcolumn].name << " in char* " << endl;
  int len = mcol_length[mcolumn];
  i = new char[len+1];
  strcpy(i,mrow[mcolumn]);

}


void
mysqlTypeR::pass(char* name, int* i,  unsigned int size){

  if(mcolumn > mnum_fields){
    cerr << "mysqlTypeR: Attempt read beyond array limit" << endl;
    return;
  }

  //  cout << mfields[mcolumn].name << " in int* " << endl;
  int d = 0;
  int len = sizeof(d);
  int iend = (int)(size/len);

  if(mfields[mcolumn].type == 250 ||
     mfields[mcolumn].type == 251){

#ifdef _ByteSwap_
   int* itmp = new int[iend];
   memcpy(itmp,mrow[mcolumn],size);
   for(int j=0;j<iend;j++){
     char* si = (char*)&itmp[j];
     char* so = (char*)&i[j];
     so[0] = si[3]; 
     so[1] = si[2];
     so[2] = si[1];
     so[3] = si[0];
   }
   delete [] itmp;
#else     
   memcpy(i,mrow[mcolumn],size);  
#endif

   mcolumn++;

  } else { // the array is a TEXT Blob

   char* p1;
   char* p2;
   char tmp[2048];
   char* dstring;
   char* dataString = new char[2048];
   ostrstream os(dataString,2048);
   os << mrow[mcolumn]<<ends;
   int iloc;
   int ilen;
   for(int iloop=0; iloop<iend-1; iloop++){
     p1 = strstr(dataString,", "); 
      iloc = p1-dataString;
      p2 = &dataString[0];
      strncpy(tmp,p2,iloc); tmp[iloc]='\0';
      //      cout << "Data passed " << tmp << " from " << dataString << endl;
      i[iloop] = atoi(tmp);
      p1++; 
      ilen = strlen(dataString)-iloc;
      dstring=new char[ilen];
      strncpy(dstring,p1,ilen);
      delete [] dataString;
      dataString = new char[strlen(dstring)+1];
      strcpy(dataString,dstring);
      delete [] dstring;                  
   }
      i[iend-1] = atoi(dataString);
      delete [] dataString;
      mcolumn++;
  }    
}

/////////////////////////////////////////////////////////////////////////

void
mysqlTypeR::pass(char* name, long* i,  unsigned int size){

  if(mcolumn > mnum_fields){
    cerr << "mysqlTypeR: Attempt read beyond array limit" << endl;
    return;
  }

  //  cout << mfields[mcolumn].name << " in long* " << endl;
  long d = 0;
  int len = sizeof(d);
  int iend = (int)(size/len);

  if(mfields[mcolumn].type == 250 ||
     mfields[mcolumn].type == 251){

#ifdef _ByteSwap_
   long* itmp = new long[iend];
   memcpy(itmp,mrow[mcolumn],size);
   for(int j=0;j<iend;j++){
     char* si = (char*)&itmp[j];
     char* so = (char*)&i[j];
     so[0] = si[3]; 
     so[1] = si[2];
     so[2] = si[1];
     so[3] = si[0];
   }
   delete [] itmp;
#else     
   memcpy(i,mrow[mcolumn],size);  
#endif

   mcolumn++;

  } else { // the array is a TEXT Blob

   char* p1;
   char* p2;
   char tmp[2048];
   char* dstring;
   char* dataString = new char[2048];
   ostrstream os(dataString,2048);
   os << mrow[mcolumn]<<ends;
   int iloc;
   int ilen;
   for(int iloop=0; iloop<iend-1; iloop++){
     p1 = strstr(dataString,", "); 
      iloc = p1-dataString;
      p2 = &dataString[0];
      strncpy(tmp,p2,iloc); tmp[iloc]='\0';
      //     cout << "Data passed " << tmp << " from " << dataString << endl;
      i[iloop] = atoi(tmp);
      p1++; 
      ilen = strlen(dataString)-iloc;
      dstring=new char[ilen];
      strncpy(dstring,p1,ilen);
      delete [] dataString;
      dataString = new char[strlen(dstring)+1];
      strcpy(dataString,dstring);
      delete [] dstring;                  
   }
      i[iend-1] = atoi(dataString);
      delete [] dataString;
      mcolumn++;
  }    
}

void
mysqlTypeR::pass(char* name, float* i,  unsigned int size){

  if(mcolumn > mnum_fields){
    cerr << "mysqlTypeR: Attempt read beyond array limit" << endl;
    return;
  }

  //cout << mfields[mcolumn].name << " in float* " << endl;
  float d = 0.;
  int len = sizeof(d);
  int iend = (int)(size/len);

  if(mfields[mcolumn].type == 250 ||
     mfields[mcolumn].type == 251){

#ifdef _ByteSwap_
   float* itmp = new float[iend];
   memcpy(itmp,mrow[mcolumn],size);
   for(int j=0;j<iend;j++){
     char* si = (char*)&itmp[j];
     char* so = (char*)&i[j];
     so[0] = si[3]; 
     so[1] = si[2];
     so[2] = si[1];
     so[3] = si[0];
   }
   delete [] itmp;
#else     
   memcpy(i,mrow[mcolumn],size);  
#endif

   mcolumn++;
  } else { // the array is a TEXT Blob

   char* p1;
   char* p2;
   char tmp[2048];
   char* dstring;
   char* dataString = new char[2048];
   ostrstream os(dataString,2048);
   os << mrow[mcolumn]<<ends;
   int iloc;
   int ilen;
   for(int iloop=0; iloop<iend-1; iloop++){
     p1 = strstr(dataString,", "); 
      iloc = p1-dataString;
      p2 = &dataString[0];
      strncpy(tmp,p2,iloc); tmp[iloc]='\0';
      //     cout << "Data passed " << tmp << " from " << dataString << endl;
      i[iloop] = (float)atof(tmp);
      p1++; 
      ilen = strlen(dataString)-iloc;
      dstring=new char[ilen];
      strncpy(dstring,p1,ilen);
      delete [] dataString;
      dataString = new char[strlen(dstring)+1];
      strcpy(dataString,dstring);
      delete [] dstring;                  
   }
      i[iend-1] = (float)atof(dataString);
      delete [] dataString;
      mcolumn++;

  }    
}

void
mysqlTypeR::pass(char* name, double* i,  unsigned int size){

  if(mcolumn > mnum_fields){
    cerr << "mysqlTypeR: Attempt read beyond array limit" << endl;
    return;
  }

  //  cout << mfields[mcolumn].name << " in double* " << endl;
  double d = 0.;
  int len = sizeof(d);
  int iend = (int)(size/len);

  if(mfields[mcolumn].type == 250 ||
     mfields[mcolumn].type == 251){


#ifdef _ByteSwap_
   double* itmp = new double[iend];
   memcpy(itmp,mrow[mcolumn],size);
   for(int j=0;j<iend;j++){
     char* si = (char*)&itmp[j];
     char* so = (char*)&i[j];
     so[0] = si[7]; 
     so[1] = si[6];
     so[2] = si[5];
     so[3] = si[4];
     so[4] = si[3]; 
     so[5] = si[2];
     so[6] = si[1];
     so[7] = si[0];
   }
   delete [] itmp;
#else     
   memcpy(i,mrow[mcolumn],size);  
#endif

   mcolumn++;
  } else {  // the array is a TEXT Blob

   char* p1;
   char* p2;
   char tmp[2048];
   char* dstring;
   char* dataString = new char[2048];
   ostrstream os(dataString,2048);
   os << mrow[mcolumn]<<ends;
   int iloc;
   int ilen;
   for(int iloop=0; iloop<iend-1; iloop++){
     p1 = strstr(dataString,", "); 
      iloc = p1-dataString;
      p2 = &dataString[0];
      strncpy(tmp,p2,iloc); tmp[iloc]='\0';
      //     cout << "Data passed " << tmp << " from " << dataString << endl;
      i[iloop] = atof(tmp);
      p1++; 
      ilen = strlen(dataString)-iloc;
      dstring=new char[ilen];
      strncpy(dstring,p1,ilen);
      delete [] dataString;
      dataString = new char[strlen(dstring)+1];
      strcpy(dataString,dstring);
      delete [] dstring;                  
   }
      i[iend-1] = atof(dataString);
      delete [] dataString;
      mcolumn++;
  }    
}

/*
void
mysqlTypeR::pass(char* name, char* i,  unsigned int size){

  // neither passType nor size are needed here

  if(mcolumn > mnum_fields){
    cerr << "mysqlTypeR: Attempt read beyond array limit" << endl;
    return;
  }

   int len = mcol_length[mcolumn];
   i = new char[len+1];
   memcpy(i,mrow[mcolumn],len);  
   i[len+1]='\0';
   mcolumn++;
}
*/








