#include "mysqlTypeW.hh"
#include <strings.h>

////////////////////////////////////////////////////////////////

bool 
mysqlTypeW::initArray(MYSQL_ROW row, MYSQL_FIELD* fields, int column, int max_column, unsigned long* col_length){

  if(column > max_column){
    cerr << "mysqlTypeW: Attempt to init pointer beyond array limit" << endl;
    return false;
  }

  if( row && fields ){
    mrow = row;
    mfields = fields;
    mcolumn = column;
    mnum_fields = max_column;
    mcol_length = col_length;
  } else {
    cerr << "mysqlTypeW: attempt to load with NULL pointer " << endl;
    return false;
  }

return true;
}

void
mysqlTypeW::pass(char* name, long& i,   unsigned int size){

  if(mcolumn > mnum_fields){
    cerr << "mysqlTypeW: Attempt read beyond array limit" << endl;
    return;
  }
  ostrstream os;
  os<<mfields[mcolumn].name<<"="<<i<<ends;
  mrow[mcolumn] = os.str();
  mcolumn++;

}

void
mysqlTypeW::pass(char* name, int& i,   unsigned int size){

  if(mcolumn > mnum_fields){
    cerr << "mysqlTypeW: Attempt read beyond array limit" << endl;
    return;
  }
  ostrstream os;
  os<<mfields[mcolumn].name<<"="<<i<<ends;
  mrow[mcolumn] = os.str();
  mcolumn++;

}

void
mysqlTypeW::pass(char* name, float& i,   unsigned int size){

  if(mcolumn > mnum_fields){
    cerr << "mysqlTypeW: Attempt read beyond array limit" << endl;
    return;
  }

  ostrstream os;
  os<<mfields[mcolumn].name<<"="<<i<<ends;
  mrow[mcolumn] = os.str();
  mcolumn++;

}

void
mysqlTypeW::pass(char* name, double& i,   unsigned int size){

  if(mcolumn > mnum_fields){
    cerr << "mysqlTypeW: Attempt read beyond array limit" << endl;
    return;
  }

  ostrstream os;
  os<<mfields[mcolumn].name<<"="<<i<<ends;
  mrow[mcolumn] = os.str();
  mcolumn++;


}

void
mysqlTypeW::pass(char* name, char* i,   unsigned int size){

  if(mcolumn > mnum_fields){
    cerr << "mysqlTypeW: Attempt read beyond array limit" << endl;
    return;
  }

  ostrstream os;
  os<<mfields[mcolumn].name<<"="<<i<<ends;
  mrow[mcolumn] = os.str();
  mcolumn++;

}

void
mysqlTypeW::pass(char* name, int* i,  unsigned int size){

  if(mcolumn > mnum_fields){
    cerr << "mysqlTypeW: Attempt read beyond array limit" << endl;
    return;
  }

  int len = sizeof(i);
  int iend = (int)(size/len);


  if(mfields[mcolumn].type == 250 ||
     mfields[mcolumn].type == 251 ){

   char * dataString = new char[2*size+1];

#ifdef _ByteSwap_
   int* itmp = new int[iend];
   for(int j=0;j<iend;j++){
     char* si = (char*)&i[j];
     char* so = (char*)&itmp[j];
     so[0] = si[3]; 
     so[1] = si[2];
     so[2] = si[1];
     so[3] = si[0];
   }
   unsigned long uend = mysql_escape_string(dataString,(char*)itmp,size);
   delete [] itmp;
#else     
   unsigned long uend = mysql_escape_string(dataString,(char*)i,size);
#endif
   ostrstream os;
   os << mfields[mcolumn].name << "=\""<<dataString<<"\""<<ends;
   mrow[mcolumn] = os.str();
   delete [] dataString;
   mcolumn++;

  } else {  // write as a text string
     ostrstream os;
     os << mfields[mcolumn].name <<"=\"";
     for(int iloop=0; iloop<iend-1; iloop++){os <<i[iloop]<<", ";}
     os<<i[iend]<<"\""<<ends;
     mrow[mcolumn] = os.str();
     mcolumn++;
  }    
}
void
mysqlTypeW::pass(char* name, long* i,  unsigned int size){

  if(mcolumn > mnum_fields){
    cerr << "mysqlTypeW: Attempt read beyond array limit" << endl;
    return;
  }

  int len = sizeof(i);
  int iend = (int)(size/len);


  if(mfields[mcolumn].type == 250 ||
     mfields[mcolumn].type == 251){

   char * dataString = new char[2*size+1];

#ifdef _ByteSwap_
   long* itmp = new long[iend];
   for(int j=0;j<iend;j++){
     char* si = (char*)&i[j];
     char* so = (char*)&itmp[j];
     so[0] = si[3]; 
     so[1] = si[2];
     so[2] = si[1];
     so[3] = si[0];
   }
   unsigned long uend = mysql_escape_string(dataString,(char*)itmp,size);
   delete [] itmp;
#else     
   unsigned long uend = mysql_escape_string(dataString,(char*)i,size);
#endif
   ostrstream os;
   os << mfields[mcolumn].name << "=\""<<dataString<<"\""<<ends;
   mrow[mcolumn] = os.str();
   delete [] dataString;
   mcolumn++;

  } else { // write as a text string
     ostrstream os;
     os << mfields[mcolumn].name <<"=\"";
     for(int iloop=0; iloop<iend-1; iloop++){os <<i[iloop]<<", ";}
     os<<i[iend]<<"\""<<ends;
     mrow[mcolumn] = os.str();
     mcolumn++;
  }    
}

void
mysqlTypeW::pass(char* name, float* i,  unsigned int size){

  if(mcolumn > mnum_fields){
    cerr << "mysqlTypeW: Attempt read beyond array limit" << endl;
    return;
  }

  int len = sizeof(i);

  int iend = (int)(size/len);


  if(mfields[mcolumn].type == 250 ||
     mfields[mcolumn].type == 251 ){

   char * dataString = new char[2*size+1];

#ifdef _ByteSwap_
   float* itmp = new float[iend];
   for(int j=0;j<iend;j++){
     char* si = (char*)&i[j];
     char* so = (char*)&itmp[j];
     so[0] = si[3]; 
     so[1] = si[2];
     so[2] = si[1];
     so[3] = si[0];
   }
   unsigned long uend = mysql_escape_string(dataString,(char*)itmp,size);
   delete [] itmp;
#else     
   unsigned long uend = mysql_escape_string(dataString,(char*)i,size);
#endif
   ostrstream os;
   os << mfields[mcolumn].name << "=\""<<dataString<<"\""<<ends;
   mrow[mcolumn] = os.str();
   delete [] dataString;
   mcolumn++;

  } else { // write as a text string
     ostrstream os;
     os << mfields[mcolumn].name <<"=\"";
     for(int iloop=0; iloop<iend-1; iloop++){os <<i[iloop]<<", ";}
     os<<i[iend]<<"\""<<ends;
     mrow[mcolumn] = os.str();
     mcolumn++;
  }    
}


void
mysqlTypeW::pass(char* name, double* i,  unsigned int size){

  if(mcolumn > mnum_fields){
    cerr << "mysqlTypeW: Attempt read beyond array limit" << endl;
    return;
  }

  int len = sizeof(i);
  int iend = (int)(size/len);


  if(mfields[mcolumn].type == 250 ||
     mfields[mcolumn].type == 251 ){

   char * dataString = new char[2*size+1];

#ifdef _ByteSwap_
   double* itmp = new double[iend];
   for(int j=0;j<iend;j++){
     char* si = (char*)&i[j];
     char* so = (char*)&itmp[j];
     so[0] = si[7]; 
     so[1] = si[6];
     so[2] = si[5];
     so[3] = si[4];
     so[4] = si[3]; 
     so[5] = si[2];
     so[6] = si[1];
     so[7] = si[0];
   }
   unsigned long uend = mysql_escape_string(dataString,(char*)itmp,size);
   delete [] itmp;
#else     
   unsigned long uend = mysql_escape_string(dataString,(char*)i,size);
#endif
   ostrstream os;
   os << mfields[mcolumn].name << "=\""<<dataString<<"\""<<ends;
   mrow[mcolumn] = os.str();
   delete [] dataString;
   mcolumn++;

  } else {  // write as a text string
     ostrstream os;
     os << mfields[mcolumn].name <<"=\"";
     for(int iloop=0; iloop<iend-1; iloop++){os <<i[iloop]<<", ";}
     os<<i[iend]<<"\""<<ends;
     mrow[mcolumn] = os.str();
     mcolumn++;
  }    

}

/*
void
mysqlTypeW::pass(char* name, char* i,  unsigned int size){

  // neither passType nor size are needed here

  if(mcolumn > mnum_fields){
    cerr << "mysqlTypeW: Attempt read beyond array limit" << endl;
    return;
  }

   mrow[mcolumn] = new char[strlen(i)+1];
   strcpy(mrow[mcolumn],i);
   mcolumn++;
}
*/








