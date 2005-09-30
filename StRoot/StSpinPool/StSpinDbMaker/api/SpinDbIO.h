// -*-C++-*-
// $Id: SpinDbIO.h,v 1.1 2005/09/30 23:47:48 balewski Exp $:
#ifndef __SPIN_DBIO_H_
#define __SPIN_DBIO_H_

// ------------------------------------------------------------------
class SpinDbIOBase  {
public:
  SpinDbIOBase (int nmemb=0 ,int sz=1);
  virtual ~SpinDbIOBase();
  virtual int   read (FILE *f) = 0;
  virtual int   write(FILE *f) = 0;
  virtual char* setComment(char *c) { comment = c; return comment; };
  virtual char* getComment() { return comment; };
  void   setData(char *ptr)  { bytePtr = ptr; };
  void   resetData(int sz=0) { memset(bytePtr,0x00,(sz>0) ? sz : bytes); };
  char  *getData()   { return (bytePtr);  };
  int   *getIndices(){ return (indexArr); }; 
  int    getBytes()  { return (bytes)  ;  };
  int    getSize()   { return (nElem);    };
  //  bool   checkLine(const char* line);

protected:
  char  *bytePtr;  // pointer to data
  int    bytes;    // array size
  int    nElem;    // size of one element
  int   *indexArr; // array of indices
  char  *comment;
};


// ------------------------------------------------------------------
struct spinDbV124;
class SpinDbV124IO : public SpinDbIOBase  {
public:
  SpinDbV124IO    (int n);
  int   read (FILE *f);
  int   write(FILE *f);
protected:
  spinDbV124  *data  (int i=0);
};


// ------------------------------------------------------------------
struct spinDbStar;
class SpinDbStarIO : public SpinDbIOBase  {
public:
  SpinDbStarIO    (int n);
  int   read (FILE *f);
  int   write(FILE *f);
protected:
  spinDbStar  *data  (int i=0);
};



// ------------------------------------------------------------------
struct spinDbBXmask;
class SpinDbBXmaskIO : public SpinDbIOBase  {
public:
  SpinDbBXmaskIO    (int n);
  int   read (FILE *f);
  int   write(FILE *f);
protected:
  spinDbBXmask  *data  (int i=0);
};


#endif


// $Log: SpinDbIO.h,v $
// Revision 1.1  2005/09/30 23:47:48  balewski
// start
//
