// -*-C++-*-
// $Id: EEmcDbIO.h,v 1.1 2013/01/25 16:46:48 stevens4 Exp $:
#ifndef __EEMC_DBIO_H_
#define __EEMC_DBIO_H_

// should move it to EEmcGeom_EEmcDefs_h
const int   kEEmcMaxSect  = 12;
const int   kEEmcMaxBox   =  9;
const int   kEEmcMaxInput = 12;
const int   kEEmcMaxIndex = kEEmcMaxSect*kEEmcMaxBox*kEEmcMaxInput;
const char* const kEEmcBoxInp[] = { "TA", "TB", "TC", "TD", "TE", "S1", "S2", "S3", "P1", "" };  


extern char *packString  (char *buf, int pos, int maxlen, char *str);
extern char *unpackString(char *buf, int pos, int maxlen, char *str);
extern int   indexFromString(const char *name, const int MaxInp=kEEmcMaxIndex);

// ------------------------------------------------------------------
class EEmcDbIOBase  {
public:
  EEmcDbIOBase (int nmemb=0 ,int sz=1);
  virtual ~EEmcDbIOBase();
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
  bool   checkLine(const char* line);

protected:
  char  *bytePtr;  // pointer to data
  int    bytes;    // array size
  int    nElem;    // size of one element
  int   *indexArr; // array of indices
  char  *comment;
};

// ------------------------------------------------------------------
// CC - calibration constants type database
// ------------------------------------------------------------------
template<class T>
class EEmcDbCCIO : public EEmcDbIOBase  {
public:
  EEmcDbCCIO(int n) : EEmcDbIOBase(1,sizeof(T)) { nElem=n; };
  virtual int   read (FILE *f);
  virtual int   write(FILE *f);
protected:
  T   *data  ()  { return (T *)bytePtr ; };
  int  scan  (const char *str, char *name, int i);
  int  print (      char *str, char *name, int i);
};

// ------------------------------------------------------------------
// QA - quality assurance type database
// ------------------------------------------------------------------
template<class T>
class EEmcDbQAIO : public EEmcDbIOBase  {
public:
  EEmcDbQAIO    (int n) : EEmcDbIOBase(n,sizeof(T)) {};
  virtual int   read (FILE *f);
  virtual int   write(FILE *f);
protected:
  T   *data  (int i=0) { return ( (T *)bytePtr + i ); };
  int  scan  (const char *str , int i);
  int  print (      char *str , int i);
};

// ------------------------------------------------------------------
// HV - HVsys type database 
// ------------------------------------------------------------------
template<class T>
class EEmcDbHVIO : public EEmcDbIOBase  {
public:
  EEmcDbHVIO    (int n) : EEmcDbIOBase(n,sizeof(T)) {};
  virtual int   read (FILE *f);
  virtual int   write(FILE *f);
protected:
  T   *data  (int i=0) { return ( (T *)bytePtr + i ); };
  int  scan  (const char *str , int i);
  int  print (      char *str , int i);
  int  str2index (const char *str);
};

// ------------------------------------------------------------------
// XML data 
// ------------------------------------------------------------------
template<class T>
class EEmcDbXML : public EEmcDbIOBase  {
public:
  EEmcDbXML    (int n) : EEmcDbIOBase(n,sizeof(T)) {};
  virtual int   read (FILE *f);
  virtual int   write(FILE *f);
protected:
  T   *data    () { return ( (T *)bytePtr); };
};


// ------------------------------------------------------------------
struct kretDbBlobS;
class KretDbBlobSIO : public EEmcDbIOBase  {
public:
  KretDbBlobSIO    (int n);
  int   read (FILE *f);
  int   write(FILE *f);
protected:
  kretDbBlobS  *data  (int i=0);// { return ( (kretDbWCM *)bytePtr + i ); };
};

#endif


// $Log: EEmcDbIO.h,v $
// Revision 1.1  2013/01/25 16:46:48  stevens4
// Scripts used to upload EEMC tables to the DB
//
// Revision 1.5  2004/01/13 16:43:22  zolnie
// allowed for inline comments
// lines starting with # will be ignored (except the first one
// which contains the signature)
// for EEmcDbCCIO, EEmcDbQAIO, EEmcDbHVIO
// but no for EEmcDbXML, KretDbBlobSIO
//
// Revision 1.4  2003/10/28 21:18:49  zolnie
// updates for Run2004
//
// Revision 1.3  2003/04/10 21:44:25  zolnie
// *** empty log message ***
//
// Revision 1.2  2003/02/04 18:10:08  zolnie
// added eemcHVtemp online database
//
// Revision 1.1  2003/01/28 23:22:18  balewski
// start
//
// Revision 1.8  2003/01/25 20:09:18  balewski
// add BlobS, remove old kret*
//
// Revision 1.7  2003/01/24 20:54:32  zolnie
// merger with Jan + updates for "HVindex" stuff
//
// Revision 1.6  2003/01/24 17:11:34  balewski
// cleanup
//
// Revision 1.5  2003/01/24 16:44:48  balewski
// added WCM+someRing online info
//
// Revision 1.4  2003/01/10 18:48:34  zolnie
// submision version
//
// Revision 1.3  2003/01/10 04:52:03  zolnie
// updates to Tcl/Tk interface (czyli Zadana Pana Jana)
//
// Revision 1.2  2003/01/03 21:14:49  zolnie
// fixed string packing in EEmcDbCCIO<T>::read(FILE *f)
// added resetString
// first version of tkEEmcDb
//
