#ifndef FINDMARK_H
#define FINDMARK_H
/*
The functions of this family are for finding wanted sequenses of simbols 
in strings or input streams. Such functions are often needed 
at treating of any data for finding right
position from which some meaningful data begin at stream. 

Copyright (c) 2000 I. B. Smirnov

Permission to use, copy, modify, distribute and sell this file for any purpose
is hereby granted without fee, provided that the above copyright notice, 
this permission notice, and notices about any modifications of the original 
text appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
*/

#include "wcpplib/util/String.h"
#include "wcpplib/safetl/AbsArr.h"

/*
The function findmark finds string s in stream file
and returns 1 at current position at the next symbol.
Finding no string it returns 0.  
*/
int findmark(istream &file, const char * s);

// Returns the same, also finds the length of string, the index of its 
// beginning and the index of the next symbol after string.
// Indexes are count from current position in stream at which the function
// was called.
// Previously this function was findmark_uca from unsigned char alternative.
// Note that dispite of T the internally this class is working with char.
// The string ws is copyed to string char before the work. 
template< class T > int findmark_a(istream &file, 
			T ws, long qws,  // T is any class with index operation
//			DynLinArr< T > ws, long qws, 
			long& nbeg, long& nnext);

// the same but returns also the previous symbol or '\0' for the string
// at the beginning of the stream.
template< class T > int findmark_b(istream &file, 
			T ws, long qws,  // T is any class with index operation
//			DynLinArr< T > ws, long qws, 
			long& nbeg, long& nnext, char& prev);


template< class T > int findmark_a(istream &file, 
			T ws, long qws, 
//			DynLinArr< T > ws, long qws, 
			long& nbeg, long& nnext)
{
  mfunname("int findmark_a(...)");
  check_econd11(qws , < 1 , mcerr);
  //check_econd12(qws , > ,  ws.get_qel() , mcerr);

  nbeg = 0;
  nnext = 0;
  //char c;

  char *s =  new char[qws+1];
  long n;
  for(n=0; n<qws; n++)
  {
    s[n] = ws[n];
  }
  s[qws] = '\0';
  int ic;
  //int l=strlen(s);  // length does not include end symbol 
  //cout<<"l="<<l<<'\n';

  char *fs=new char[qws+1];
  for( n=0; n<qws; n++)
  {
    if(file.eof() == 1)
    {
      delete[] fs; delete[] s; return 0; 
    }
    else
    {
      ic=file.get();
      fs[n]=ic;
    }
  }
    //if((ic=file.get())==EOF) { delete fs; return 0; }
    //else fs[n]=ic;
  fs[qws]='\0';
  //cout<<fs<<'\n';
  nnext = qws;
  
  while(strcmp(fs,s)!=0)
  {
    for( n=1; n<qws; n++) fs[n-1]=fs[n];
    if(file.eof() == 1)
    {
      delete[] fs; delete[] s; return 0; 
    }
    else
    {
      ic=file.get();
    }
    //if((ic=file.get())==EOF) { delete fs; return 0; }
    fs[qws-1]=ic;
    //fs[qws]='\0';
    nbeg++;
    nnext++;
    //cout<<fs<<'\n';
  }
  delete[] fs; delete[] s; 
  return 1;
}


template< class T > int findmark_b(istream &file, 
			T ws, long qws, 
//			DynLinArr< T > ws, long qws, 
			long& nbeg, long& nnext, char& prev)
{
  mfunname("int findmark_b(...)");
  check_econd11(qws , < 1 , mcerr);
  //check_econd12(qws , > ,  ws.get_qel() , mcerr);

  nbeg = 0;
  nnext = 0;
  //char c;
  prev = '\0';

  char *s =  new char[qws+1];
  long n;
  for(n=0; n<qws; n++)
  {
    s[n] = ws[n];
  }
  s[qws] = '\0';
  int ic;
  //int l=strlen(s);  // length does not include end symbol 
  //cout<<"l="<<l<<'\n';

  char *fs=new char[qws+1];
  for( n=0; n<qws; n++)
  {
    if(file.eof() == 1)
    {
      //mcout<<"eof at:\n";
      //Iprintn(mcout, n);
      delete[] fs; delete[] s; return 0; 
    }
    else
    {
      ic=file.get();
      fs[n]=ic;
    }
  }
    //if((ic=file.get())==EOF) { delete fs; return 0; }
    //else fs[n]=ic;
  fs[qws]='\0';
  //cout<<fs<<'\n';
  nnext = qws;
  
  while(strcmp(fs,s)!=0)
  {
    prev = fs[0];
    for( n=1; n<qws; n++) fs[n-1]=fs[n];
    if(file.eof() == 1)
    {
      //mcout<<"eof at:\n";
      //Iprint2n(mcout, nbeg, nnext);
      delete[] fs; delete[] s; return 0; 
    }
    else
    {
      ic=file.get();
    }
    //if((ic=file.get())==EOF) { delete fs; return 0; }
    fs[qws-1]=ic;
    //fs[qws]='\0';
    nbeg++;
    nnext++;
    //cout<<fs<<'\n';
  }
  //mcout<<"mark found\n";
  delete[] fs; delete[] s; 
  return 1;
}


/*
I don't remember what this function in doing
*/
int findmark_other_repeat(istream &file, ostream &outfile, const char * s);

//returns -1 or index of found word

/*
The following two functions find any of given strings, the first met.
They return the number of string met ( in the interval 0 -- q-1).
If none of strings found, they return -1. 
*/

int find1ofnmark(istream &file, int q,  // number of strings 
		 char * s[]);     // addresses
int find1ofnmark(istream &file, int q,  // number of strings 
		 const String str[]);     // addresses


#endif
