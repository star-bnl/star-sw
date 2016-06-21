#if 0
/*
** $Id: dblib.cxx,v 1.10 2016/06/21 14:23:19 jwebb Exp $
**
** $Log: dblib.cxx,v $
** Revision 1.10  2016/06/21 14:23:19  jwebb
** Retire unused DB / Zebra interface.
**
** Revision 1.9  2010/08/27 16:28:06  perev
** Simlification
**
** Revision 1.8  2010/08/27 14:20:11  fisyak
** Add stdio.h for gcc  4??
**
** Revision 1.7  2009/11/19 04:58:19  perev
** DUMMY mYSql
**
** Revision 1.6  2009/08/18 19:23:20  jeromel
** string.h needed for gcc 4 (??)
**
** Revision 1.5  2005/10/12 21:26:06  potekhin
** Comment out the previous "fix", as we now have a better solution
** by excluding "-ansi" in the branch of Cons specific for this Linux version
** and compiler (i386, 2.96)
**
** Revision 1.4  2005/10/12 20:07:19  potekhin
** Fix the problem of undeclared "strdup" by adding the missing declaration.
** This module is unused anyway nowadays, so this will at least make it compile.
**
** Revision 1.3  2004/11/24 00:08:35  potekhin
** Corrected one typo which misused the ostrstream declaration,
** deleted an extra parenthesis and added two buffers that were
** hereto undeclared. The code now compiles on 7.2/2.96.
**
** Revision 1.2  2004/03/01 17:26:33  fisyak
** Get rid of staf
**
** Revision 1.1.1.1  2004/01/12 23:49:38  potekhin
**
**
** Revision 1.18  2003/11/13 16:23:36  nevski
**
** more splitting
**
** Revision 1.17  2003/11/12 18:33:41  nevski
** -m dblib warnings corrected
**
** Revision 1.15  2003/11/10 16:43:51  nevski
** dblib returns error flag
**
** Revision 1.14  2003/11/10 09:22:50  nevski
** use gcflag for debug print control
**
** Revision 1.11  2003/04/24 08:52:53  nevski
** nova library corrected for material special case
**
** Revision 1.10  2003/03/17 12:40:34  nevski
** separate executable and library
**
** Revision 1.9  2003/03/10 18:39:46  nevski
** mods for gcc 3.2
**
** Revision 1.8  2003/02/24 14:44:56  nevski
** move to MySQL release 4
**
** Revision 1.7  2002/06/12 19:19:11  nevski
** bugfix - do not use stack for buffer
**
** Revision 1.6  2002/04/29 01:07:24  nevski
** dynamic array support improved
**
** Revision 1.4  2001/06/17 18:25:59  nevski
** long lines splitted
**
** Revision 1.3  2001/06/15 23:00:19  nevski
** if-endif corrected
**
** Revision 1.2  2001/06/08 17:16:59  nevski
**  MySQL interface enabled
**
*/
#if defined(CERNLIB_MYSQL)
#undef  CERNLIB_MYSQ
#endif
#if defined(CERNLIB_MYSQL)
/*******************************************************************************/
/*                             MYSQL INTERFACE                                 */
//  fill three tables:
//  structure - table (recursive fill of directory, structure, etc.
//                     has to be only one-to many)
//  relation  - table to resolve many-to-many of structure-parameter
//  parameter - table (recursive fill for arrays, since it is only one-to-many)
//
//  fourth table "bytes" for dynamic arrays
//               it is filled during the second DBUSE call
//
// Terms: age      database
//        system   directory
//        module   dataset
//        bank     structure
/*                                                                             */
/*******************************************************************************/
// todo: AV replace structure with the same IDNUM during the fill (MFLG case)
//
// Nov 12 2003 AV moved countArrays() code into persistent() class
//                introduced converter convert()
//
// Nov 11 2003 AV introduced class dictionary and class metadata
//                utility function convert2string()
//                queryStr() to fix <sstream> printout features
//
// Nov 10 2003 PN ISTAT pointer bug fix in dbuse
//
// Nov  9 2003 AV introduced dbuse printout control
//
// Nov  8 2003 AV use <sstream> instead of the deprecated <strstream.h>
//                introduced functions my_query(), countArrays(), addIDNUM()
//
// Nov  7 2003 AV optimized slow queries (requested by David Rousseau)
//
// Nov  6 2003 AV introduced class persistent
//                split old dbfill into
//                   1)new dbfill (fill the persistent class:
//                                 converts the data dictionary
//                                      and the object data
//                                      for the persistent NOVA data model)
//                   2) and dbwrite (writes persistent representation to DB) 
//                       
// Apr 16 2003 AV reduced printout
// Apr 16 2003 AV support for dyn arrays of dimension 1 (for materials)
// Mar 10 2003 AV dbset handles database connection error
// Mar 09 2003 AV debug printout control introduced
// Mar 08 2003 AV gcc-3.2 mods
// Feb 04 2003 AV do not fill soon-to-be-obsolete structures:
//                                                DDSC,TTGC,RRPC,BBDT,QQGC
// Jan 29 2003 AV do not fill the event-store structures (BPATH start "/RECB")
// Jan 21 2003 AV changes for MySQL 4.0
// Jan 15 2003 AV remove the top element: DETM(1) from BPATH, IDNUM arrays
//                remove the second element: MUCH(1) from BPATH, IDNUM arrays
//                rename reduced IDNUM vector as IDNUM2, IDNUM3,... variables
//                do not fill BPATH vector.
//
// Jan 15 2003 AV optimized IDNUM and BPATH metadata.
//
// BPATH "/DETM/PIXE/PMOD*" at the end repeat structure name: do not use it
//                          at the top have the DETM(1) for all: remove it
//
// I1 is dimension for IDNUM vector
//    reduce dimension for metadata IDNUM by 2
//           remove the first  IDNUM[0], it always = 1
//           remove the second IDNUM[1], it always = 1
//
//           record the last   IDNUM[I1-1] also as the structure instance
//
//    if IDNUM[I1-1]==0 do nothing
//    if IDNUM[I1-1]>0 use it as a instanceProvidedFromIDNUM
//    if instanceProvidedFromIDNUM>0 && I1>1
//         add PATH = "/DETM(1)/PIXE(1)" to structure table
//         add IDNUM and BPATH metadata to parameters
//         IDNUM and BPATH duplicate the PATH information
//
// Jan 11 2003 AV added IDNUM and BPATH metadata to the structures' data
// Dec 15 2002 AV added IDNUM to existence check for arrays of structures
// Sep 22 2002 AV break out of the loop over DB structures when found same;
// Sep 21 2002 AV added md5sum check to avoid duplication of blobs
//      added type checking for variables to track schema evolution
// Sep 20 2002 AV minor cleanup:
//      added mysql_options() call
//      changed to mysql_real_escape_string()
//      removed static int lastInsertedStrID 
//              used to check if this is a second call
//      ignore first call for structures with dyn arrays
//      removed unused commented out code
// Jun 12 2002 AV,PN moved mysqlBinaryQueryBuffer from stack to heap
// Apr 27 2002 AV,PN added dynamic array link lookup
// Apr 25 2002 AV added trailing blanks removal from text (caused duplicates)
// Mar 27 2002 AV added dynamic array support
// Feb 28 2002 AV removed mysql_close due to a conflict with mysql_ping
/*******************************************************************************/
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

//gcc3.2
#if defined(__GNUC__) &&  (__GNUC__ >= 3)
#        include <iostream>
#        include <cstdlib>
#        include <string>
#        include <sstream>
         using namespace std;
int my_query(ostringstream *Query);
#else //.h for gcc-2 and SunOS
#        include <strstream.h>
// extern char *strdup (__const char *__s) __THROW __attribute_malloc__;
int my_query(ostrstream *Query);
#endif

#include "mysql.h"
#include "mysql_com.h"


// #include "m_string.h" //for strmov
// extern "C" char *strmov(char *dst,const char *src);//from mysqlclient lib

//for printout control
extern "C" int gcflag_;

//subroutine getdynbank (IDYN, IADR)
extern "C" void getdynbank_(int *, int *);

struct dbdata  { char *user, *passwd, *server, *name; } DBAS;
MYSQL mysql;

//for blobs
  char* mysqlBinaryQueryBuffer=0;

//------------------------------------------------------------------------------/
//         forward declarations
//------------------------------------------------------------------------------/
class metadata;
class dictionary;
class persistent;
//------------------------------------------------------------------------------/
//         function prototypes
//------------------------------------------------------------------------------/
const char* convertPath(const char* BPATH, const int I1, const int* IDNUM);
unsigned int addIDNUM (persistent *Pobj, const int I1, const int* IDNUM);
void convert(unsigned int outIndex, persistent *Pobj,
	     float *pBANK, const dictionary *d);
void dbwrite(persistent *Pobj);
//------------------------------------------------------------------------------/
//         function definitions
//------------------------------------------------------------------------------/
#if defined(__GNUC__) &&  (__GNUC__ >= 3)
int my_query(ostringstream *Query) {
  return mysql_real_query(&mysql,Query->str().c_str(),int(Query->tellp())-1);
}
const char * queryStr(ostringstream *Q) {
  string s=Q->str(); s.resize(Q->tellp()); return s.c_str();
}
#else //.h for gcc-2 and SunOS
int my_query(ostrstream *Query) {
  return mysql_real_query(&mysql,Query->str(),Query->pcount()-1);
}
const char * queryStr(ostrstream *Q) { return Q->str(); }
#endif
//------------------------------------------------------------------------------/
//
// convert2string makes new null-terminated string from the bank data
//
const char* convert2string (const char *s, int i)
{
  char *r = new char[i+1];
  strncpy(r,s,i);

  //remove trailing spaces (if any)
  while (i != 0  && r[i-1] == ' ') i--;
  r[i]='\0';

  return r;
}
//------------------------------------------------------------------------------/
//#include "dictionary.h"
//------------------------------------------------------------------------------/
//
// Class dictionary keeps the data object dictionary - array of
// descriptors for each data member of the object/structure 
//

  // kAscii not a blob, kBinary - persistify as a blob
  enum EPersistencyMode {kAscii, kBinary};

  enum EVariableType {kNAN, kFloat, kInt, kLong, kShort, kDouble, kUInt,
                      kULong, kUShort, kUChar, kChar, kText};
  struct Descriptor{
    const char *name;              // variable name 
    const char *comment;           // variable comment
    unsigned int nDims;            // number of dimensions
    unsigned int firstDimension;   // first dimension, if this is an array
    unsigned int secondDimension;  // second dimension
    unsigned int offset;           // variable offset in bytes
    unsigned int size;             // total size of this variable/array in bytes
    unsigned int typeSize;         // unit size
    EVariableType type;            // type of element
    EPersistencyMode mode;         // persistency mode for data
                                   // (can be binary for large arrays)
  };

class dictionary {

public:
  dictionary  (
    int*   pMAP,          // array of dimensions
    char*  pNAME,         // array of variables names
    char*  pCOMMEN,       // array of variables comments
    float* pBANK,         // dynamic arrays dimensions encoded in pBANK
    int    LLEN,          // number of variables
    int    lNAME,         // fixed length for variable name = 12
    int    lCOMMEN)       // fixed length for comment field = 40
    : m_size(LLEN), m_d ( new Descriptor[m_size] ) {

const int M = 1000000;

unsigned int offset;

offset=0;

for (unsigned int i=0; i<m_size; i++)
{
  m_d[i].offset=offset;
  //
  // decode dzdoc variable types: 'R.', 'I.' or 'H.'
  //
  switch((pNAME+i*lNAME)[0])
    {
    case 'R':
      m_d[i].type=kFloat;
      break;
    case 'I':
      m_d[i].type=kInt;
      break;
    case 'H':
      m_d[i].type=kText;
      break;
    default:
      cerr << "descriptor: ERROR: usupported type!"<<endl;
      break;
    }
  //use for now the 4-byte size for the (fortran) word
  m_d[i].typeSize=4;
  
  m_d[i].name = convert2string(pNAME+i*lNAME+2,lNAME-2);
  m_d[i].comment = convert2string(pCOMMEN+i*lCOMMEN,lCOMMEN);
  
  //get dimensions for array
  
  int D1=*(pMAP+2*i);
  int D2=*(pMAP+2*i+1);
  
  //
  // decode fortran dynamic arrays
  //	
  m_d[i].mode = kAscii;
  
  //special case - dynamic arrays
  if (D1<0 || D2<0)
    {
      if (D1<0)
	{
	  D1=-D1;
	  if (D1<M)
	    D1= *( (int*)pBANK + (D1-1) );
	  else
	    D1= (int) *( pBANK + (D1-1 - M) );
	}
      if (D2<0)
	{
	  D2=-D2;
	  if (D2<M)
	    D2= *( (int*)pBANK + (D2-1) );
	  else
	    D2= (int) *( pBANK + (D2-1 - M) );
	}
      m_d[i].mode = kBinary;
    }
  
  //decide if these can be 0 or 1
  m_d[i].firstDimension=D1;
  m_d[i].secondDimension=D2;
  
  m_d[i].nDims = 0;
  if (D1>1) {
    m_d[i].nDims = 1;
    if (D2>1) {
      m_d[i].nDims = 2;
    }
  }
  
  //D1 or D2 can be 0
  m_d[i].size=m_d[i].typeSize;
  if (D1*D2>1) m_d[i].size=D1*D2*m_d[i].typeSize;
  
  offset+=m_d[i].size;
}
  }
  
  virtual ~dictionary() { delete [] m_d; }

  //accessors:
  const Descriptor* getDescriptor(unsigned int i) const {
    if (i<m_size) return &m_d[i]; return NULL;
  }
  unsigned int size() const {return m_size;}
  unsigned int arrays() const {
    unsigned int arrays=0;
    for (unsigned int i=0; i<m_size; i++) {
      int D1=m_d[i].firstDimension;
      int D2=m_d[i].secondDimension;
      if ( D1 * D2 > 1 ) arrays++;
      if ( m_d[i].mode == kBinary ) {
	//it could be that D1=0, count these arrays as dynamic arrays
	if ( D1 * D2 == 0 ) arrays++;
	//in materials: D1=1 & D1=1, count these dynamic arrays
	if ( D1 == 1 && D2 == 1 ) arrays++;
      }
    }
    if (gcflag_>3) cout << "dictionary::arrays() counted "<<arrays<<endl;
    return arrays;
  }
  unsigned int blobs() const {
    unsigned int blobs=0;
    for (unsigned int i=0; i<m_size; i++) if ( m_d[i].mode == kBinary ) blobs++;
    if (gcflag_>3) cout << "dictionary::blobs() counted "<<blobs<<endl;
    return blobs;
  }
  unsigned int outlen() const {
    unsigned int outlen=0;
    for (unsigned int i=0; i<m_size; i++) {
      {
	switch (m_d[i].nDims) 
	  {
	  case 0:
	    outlen++;//simple variable
	    break;
	  case 1:
	    outlen++;//array header
	    if ( m_d[i].mode == kAscii ) {
	      outlen+=m_d[i].firstDimension;
	    }
	    break;
	  case 2:
	    outlen++;//array header
	    //must be a blob
	    //	outlen+=m_d[i].firstDimension*m_d[i].secondDimension;
	    break;
	  default:
	    cerr<<"dictionary::outlen() ERROR: unsported number of dimensions"<<endl;
	    break;
	  }
      }
    }
    if (gcflag_>3) cout << "dictionary::outlen() counted "<<outlen<<endl;
    return outlen;
  }
  
  //utilities:
  static const char * GetTypeName( EVariableType type) {
    switch (type)
      {
      case kFloat:  return "float";
      case kInt:    return "int";
      case kLong:   return "long";
      case kShort:  return "short";
      case kDouble: return "double";
      case kChar:   return "char";
	//temporarily
      case kUInt:   return "int";
      case kULong:  return "long";
      case kUShort: return "short";
      case kUChar:  return "char";
	
      case kText:  return "text";
	
      case kNAN:    return "";
      default:      return "";
      } 
  };
  
private:
  const unsigned int m_size;
  Descriptor *m_d;
};
//------------------------------------------------------------------------------/
//#include "metadata.h"
//------------------------------------------------------------------------------/
//
// Class metadata keeps the extra information about the structure 
//
class metadata {

public:
  
  metadata ( const char *pDIRECTORY,
	     const char *pDCOMMENT,
	     const char *pAUTHOR,
	     // 	    const char *pCREATED, //not used
	     const char *pSTRUCT_NAME,
	     const char *pBANK_TITLE,
	     const char *pBPATH,
	     int*   pIDNUM,
	     int*   pI1,
	     int    lDIRECTORY,
	     int    lDCOMMENT,
	     int    lAUTHOR,
//    	     int    lCREATED, //not used
	     int    lBANK_TITLE,
	     int    lSTRUCT_NAME,
	     int    lBPATH)
    : m_PATH(NULL), m_nIDNUM(0)
  {
    m_DIRECTORY=convert2string(pDIRECTORY, lDIRECTORY);
    m_DCOMMENT=convert2string(pDCOMMENT,lDCOMMENT);
    m_AUTHOR=convert2string(pAUTHOR,lAUTHOR);
    m_STRUCT_NAME=convert2string(pSTRUCT_NAME,lSTRUCT_NAME);
    m_BANK_TITLE=convert2string(pBANK_TITLE,lBANK_TITLE);

    int I1 = *pI1;

    //can it be 0?
    if (I1==0) I1=1;
    
    //assume this:
    // not needed yet, since IDNUM not always provided
    // when IDNUM=0 "bank idet v dozapis'" - no overwrite requested
    // if (IDNUM==0) IDNUM = 1;
    //   int instanceProvidedFromIDNUM = IDNUM[I1-1];
   
    m_instance = pIDNUM[I1-1];

    if (m_instance>0 && I1>3 ) m_nIDNUM = I1-2;
    if ( m_nIDNUM>0 ) {
      m_PATH=convertPath(convert2string(pBPATH,lBPATH),
			 (const int) I1, (const int*) pIDNUM);
    }
  }

  virtual ~metadata() {
    delete [] m_DIRECTORY;
    delete [] m_DCOMMENT;
    delete [] m_AUTHOR;
    //  delete [] m_CREATED;
    delete [] m_BANK_TITLE;
    delete [] m_STRUCT_NAME;
    if (m_PATH != NULL ) delete [] m_PATH;
  }

  //accessors
  const char* DIRECTORY () const { return m_DIRECTORY; }
  const char* DCOMMENT () const { return m_DCOMMENT; }
  const char* AUTHOR () const { return m_AUTHOR; }
  const char* STRUCT_NAME () const { return m_STRUCT_NAME; }
  const char* BANK_TITLE () const { return m_BANK_TITLE; }
  const char* PATH () const { return m_PATH; }

  unsigned int nIDNUM () const { return m_nIDNUM; }
  unsigned int instance () const { return m_instance; }

private:

  //structure information
  const char *m_DIRECTORY;
  const char *m_DCOMMENT;
  const char *m_AUTHOR;
  const char *m_STRUCT_NAME;
  const char *m_BANK_TITLE;

  const char *m_PATH;

  unsigned int m_instance; // structure instance provided from IDNUM
  unsigned int m_nIDNUM; // number of extra IDNUM variables, now = I1-2
};
//------------------------------------------------------------------------------/
//#include "persistent.h"
//------------------------------------------------------------------------------/
class persistent {

public:
//   persistent( const metadata *m, const dictionary *d, unsigned int LEN)
  persistent( const metadata *m, const dictionary *d)
    : m_metadata(m),
      m_byteSize(0),
      m_nObj(0),
      m_nElements(0),
      ppBytes(NULL),
      byteSize(NULL),
      position(NULL),
      ppMD5sum(NULL)
{  
  dynLEN=d->blobs();

  //outLEN is used until the vector<> implementation is done

  outLEN = d->outlen() + m->nIDNUM();
  outLLEN = d->size() + m->nIDNUM();

  if( gcflag_>1 && m->nIDNUM() > 0 ) {
     cout<<"there is PATH of depth "<< m->nIDNUM()<<" increased outLEN to:  "<< outLEN <<endl;
  }

  if (gcflag_>1) {
    if (dynLEN>0) {
      cout<<"Special Case: outLEN = "<< outLEN
	  <<", there are "<< d->arrays() <<" arrays in total, of which "
	  <<dynLEN<<" are dyn arrays"<<endl;
    }
    else {
      cout<<"there are "<<d->arrays()<<" arrays, outLEN:  "<< outLEN <<endl;
    }
  }

  ppName =   new char*[outLEN];
  ppCommen = new char*[outLEN];
  ppType =   new char*[outLEN];
  ppValue =  new char*[outLEN];
  
  if ( dynLEN >0 ) {  
    ppBytes =  new const char*[dynLEN];
    byteSize = new unsigned int[dynLEN];
    position = new unsigned int[dynLEN];
    ppMD5sum = new char*[dynLEN];
  }
}
  virtual ~persistent() {

    if (gcflag_>2) cout << "cleanup: ~persistent() "
			<< m_metadata->STRUCT_NAME() << endl;
    unsigned int i;
    for (i=0;i<outLEN;i++)
      {	
	free((void*)ppName[i]);
	free((void*)ppCommen[i]);
	free((void*)ppType[i]);
	free((void*)ppValue[i]);
      }
    for (i=0;i<dynLEN;i++)
      {	
	free((void*)ppMD5sum[i]);
      }
    delete [] ppName;
    delete [] ppCommen;
    delete [] ppType;
    delete [] ppValue;
    if ( dynLEN >0 ) {  
      delete [] ppBytes;
      delete [] byteSize;
      delete [] position;
      delete [] ppMD5sum;
    }
  }

  void putName (unsigned int i, const char* str) {
    if (i<outLEN) ppName[i]=strdup(str); }
  void putType (unsigned int i, const char* str) {
    if (i<outLEN) ppType[i]=strdup(str); }
  void putValue (unsigned int i, const char* str) {
    if (i<outLEN) ppValue[i]=strdup(str); }
  void putCommen (unsigned int i, const char* str) {
    if (i<outLEN) ppCommen[i]=strdup(str); }

  void putBytes (unsigned int i, const char* str) {
    if (i<dynLEN) ppBytes[i]=str; }

  void putMD5sum (unsigned int i, const char* str) {
    if (i<dynLEN) ppMD5sum[i]=strdup(str); }
  void putByteSize (unsigned int i, unsigned int size ) {
    if (i<dynLEN) byteSize[i]=size; }
  //position in relation table is counted starting from 1
  //position in the temp table is counted starting from 0
  void putPosition (unsigned int i, unsigned int j) {
    if (i<dynLEN && j<outLEN) position[i]=j; }

  const char* getName (unsigned int i = 0) const {
    if (i<outLEN) return ppName[i]; return NULL; }
  const char* getCommen (unsigned int i = 0) const {
    if (i< outLEN) return ppCommen[i]; return NULL; }
  const char* getType (unsigned int i = 0) const {
    if (i<outLEN) return ppType[i]; return NULL; }
  const char* getValue (unsigned int i = 0) const {
    if (i<outLEN) return ppValue[i]; return NULL; }

  const char* getBytes (unsigned int i = 0) const {
    if (i<dynLEN) return ppBytes[i]; return NULL; }

  const char* getMD5sum (unsigned int i = 0) const {
    if (i<dynLEN) return ppMD5sum[i]; return NULL; }

  unsigned int getByteSize (unsigned int i = 0) const {
    if (i<dynLEN) return byteSize[i]; return 0; }
  unsigned int getPosition (unsigned int i = 0) const {
    if (i<dynLEN) return position[i]; return 0; }

  unsigned int getOutLLEN () const { return outLLEN; }
  unsigned int getOutLEN () const { return outLEN; }
  unsigned int getDynLEN () const { return dynLEN; }

  const metadata *get () const { return m_metadata; }

private:

  //structure information
  const metadata *m_metadata;

  unsigned int      m_byteSize;  // byte size of one object
  unsigned int      m_nObj;      // number of structures if this is an array
  unsigned int      m_nElements; // number of elements/fields in object

  unsigned int outLEN;  // number of parameter lines for DB insertion
                        // (includes array headers and array elements)
  unsigned int dynLEN;  // number of data members written to DB as blobs
  unsigned int outLLEN;  // new number of variables that includes added IDNUMs
                        // (includes array headers and array elements)

  //information for each blob data member of the structure
  const char **ppBytes;
  unsigned int *byteSize;
  unsigned int *position;
  char **ppMD5sum;

  //information for each data member of the structure
  char **ppName;
  char **ppCommen;
  char **ppType;
  char **ppValue;
};
//------------------------------------------------------------------------------/
extern "C" void  dbget_( char* key, char* field,int l,int len)
//void  dbget_( char* key, char* field,int l,int len)
{ //pn, 29.04.00:

  //using namespace std;

  //AV, 16.06.01 to suppress compile warning use l:
  //  if (l<0) std::cerr<<"no key len"<<std::endl;
  if (l<0) cerr<<"no key len"<<endl;
                                memset (field, ' ', len);
  if (key[0]=='u'&&DBAS.user  ) strncpy(field, (const char*) DBAS.user,  len);
  if (key[0]=='p'&&DBAS.passwd) strncpy(field, (const char*) DBAS.passwd,len);
  if (key[0]=='s'&&DBAS.server) strncpy(field, (const char*) DBAS.server,len);
  if (key[0]=='n'&&DBAS.name  ) strncpy(field, (const char*) DBAS.name,  len);
  return;
}
//------------------------------------------------------------------------------/
extern "C" void  dbset_( char* user, char* passwd, char* server, char* dbname)
{
// Initialize a connection handler

mysql_init(&mysql);

//this is to control the port number, etc
mysql_options(&mysql,MYSQL_READ_DEFAULT_GROUP,"nova");
//mysql_options(&mysql,MYSQL_OPT_COMPRESS,0);

// Establish a connection to the MySQL database engine
  if (mysql_real_connect(&mysql, server, user, passwd, dbname, 0, NULL, 0))
   {
     // success - keep these values
     DBAS.user  =strdup(user);    // leak: old not returned
     DBAS.passwd=strdup(passwd);
     DBAS.server=strdup(server);
     DBAS.name  =strdup(dbname);
     cout << "dbset INFO: user=" <<DBAS.user << ", passwd=" << DBAS.passwd
	  <<",server=" << DBAS.server << ", name=" << DBAS.name << endl;
   }
  else {
     cerr << "dbset ERROR: Failed to connect to database: Error: "
	  <<  mysql_error(&mysql) << endl;
     cout << "dbset WARNING: faulty parameters provided to dbset: user=\'"
	  << user << "\', passwd=****, server=\'" << server << "\', db=\'"
	  << dbname <<"\'"<< endl;
	  
     cout << "dbset WARNING: database connection parameters are not set" << endl;
   }

  return;
}
//------------------------------------------------------------------------------/
extern "C" void  dbls_( char* directory)
{

//MYSQL mysql;
//MYSQL_RES *result;
// MYSQL_ROW row;

// unsigned int num_fields;
// unsigned int num_rows;
// int num_struct;

// Initialize a connection handler

mysql_init(&mysql);

// Establish a connection to the MySQL database engine
 if (!mysql_real_connect(&mysql,
			 DBAS.server,
			 DBAS.user,
			 DBAS.passwd,
			 DBAS.name,0,NULL,0))
   {
     cerr << "Failed to connect to database: Error: "
 	 <<  mysql_error(&mysql) << endl;
   }

  cout<< "DBLS: listing " << directory <<endl;

// We are done with the connection, call mysql_close() to terminate it

//mysql_close(&mysql);
  return;
}
//------------------------------------------------------------------------------/
extern "C" void  dbprint_( char* structure)
{
  cout<< "DBPRINT: printing" << structure << endl;
  return;
}
//*****************************************************************************//
//  DBFILL is called from AGDOCUM
//  Just once for normal structures
//
//  Twice (once from AGDOCUM, second time from AGDOCUME) for structures with
//  dynamic arrays
//  at the first call dynamic arrays are epmty and call is ignored
//  at the second call dynamic arrays are stored in the bytes table as longblob
//------------------------------------------------------------------------------/
//                  Subroutine   A g D O C U M E _
// call DBFILL (Module,MTitle,Author,Created,Btit,Bank,Bpath,Num,Lb,
//                            Map,Names,Comment,Par,LL,LL1,Link,Flag,1)
                          // names used in calling routine AGDOCUM:
//extern "C" void adbfill_(
extern "C" void dbfill_(
char*  pDIRECTORY,
char*  pDCOMMENT,
char*  pAUTHOR,       // AUTHOR,AUTHOR*40
char*  pCREATED,      //       CREATED    CREATED*40 /'23 MARCH 1996'/
char*  pBANK_TITLE,   //       BANK_TITLE /'*'/
char*  pSTRUCT_NAME,  //       BANK_NAME  'CTBB'
char*  pBPATH,        // *60 BPATH="//DETM/MUCH/NMDT/AADT*" or BPATH="ATLS*"
int*   pIDNUM,        // IDNUM_CTBB,     1   1    1     i      path elements
int*   pI1,           // dimension of IDNUM array
int*   pMAP,          // int MAP_CTBB(2,LLEN_CTBB)
char*  pNAME,         // NAMES_CTBB(LLEN_CTBB)*12
char*  pCOMMEN,       // COMMEN_CTBB(LLEN_CTBB)*40
float* pBANK,         // binary array of all variables
int*   pLEN,          // total length of BANK (including array elements)
int*   pLLEN,         // number of variables
int*   pLINK,
int*   pFLAG,
int*   pCONT,         // 0 - first call, 1 - endfill call (for Dynamic structs)
int    lDIRECTORY,
int    lDCOMMENT,
int    lAUTHOR,
int    lCREATED,
int    lBANK_TITLE,
int    lSTRUCT_NAME,
int    lBPATH,
int    lNAME,
int    lCOMMEN)
{
  //cout<<"DEBUG in dbfill: DBAS.user="<<DBAS.user<<endl;

  if (!DBAS.user || DBAS.user[0]!='w') return;

//has dyn arrays?
  int dynamic = 0;
  for (int i=0; i<*pLLEN; i++) {
    if (*(pMAP+2*i)<0 || *(pMAP+2*i+1)<0) dynamic = 1;
  }

//ignore second call for structures without dyn arrays
  if (*pCONT==1 && dynamic == 0) return;
//ignore first call for structures with dyn arrays
  if (*pCONT==0 && dynamic == 1) return;

  //AV, 16.06.01 to suppress compile warning use these variables:
  void* myDummyP;
  myDummyP = pLEN;
  myDummyP = pLINK;
  myDummyP = pFLAG;
  myDummyP = pCREATED;

  if (gcflag_>4) cout << "lDIRECTORY " << lDIRECTORY << endl;
  if (gcflag_>4) cout << "lDCOMMENT " << lDCOMMENT << endl;
  if (gcflag_>4) cout << "lAUTHOR " << lAUTHOR << endl;
  if (gcflag_>4) cout << "lCREATED " << lCREATED << endl;
  if (gcflag_>4) cout << "lBANK_TITLE " << lBANK_TITLE << endl;
  if (gcflag_>4) cout << "lSTRUCT_NAME " <<lSTRUCT_NAME  << endl;
  if (gcflag_>4) cout << "lBPATH " << lBPATH << endl;
  if (gcflag_>4) cout << "lNAME " << lNAME << endl;
  if (gcflag_>4) cout << "lCOMMEN " << lCOMMEN << endl;

  metadata *m = new metadata((const char *)pDIRECTORY,
			     (const char *)pDCOMMENT,
			     (const char *)pAUTHOR,
// 			     (const char *)pCREATED,
			     (const char *)pSTRUCT_NAME,
			     (const char *)pBANK_TITLE,
			     (const char *)pBPATH,
			     pIDNUM,
			     pI1,
			     lDIRECTORY,
			     lDCOMMENT,
			     lAUTHOR,
// 			     lCREATED,
			     lBANK_TITLE,
			     lSTRUCT_NAME,
			     lBPATH);
// save structure info

  if (gcflag_>1)  {
    cout <<"trying to fill structure: "<< m->STRUCT_NAME() << endl;
    cout << " BANK_TITLE \"" << m->BANK_TITLE() <<"\" I1="<<*pI1 
	 << " instanceProvidedFromIDNUM = " << m->instance() << endl;
    if (m->PATH() != NULL) cout << " PATH \""<<m->PATH()<<"\""<< endl;
  }

// only
// if (strcmp(DIRECTORY,"MFLDGEO") != 0) return;
// if (strcmp(DIRECTORY,"MUCHGEO") != 0) return;
// if (strcmp(STRUCT_NAME,"AADT") != 0) return;
// if (strcmp(DIRECTORY,"MINTGEO") != 0) return;
// if (strcmp(DIRECTORY,"AXXXREC") != 0) return;
//it is wrong to do only for not the first structure
// if (strcmp(STRUCT_NAME,"MMRI") != 0) return;
// if (strcmp(STRUCT_NAME,"AGCR") != 0) return;
// if (strcmp(STRUCT_NAME,"PMOD") != 0) return;
// if (strcmp(STRUCT_NAME,"DBSC") != 0) return;
// if (strcmp(STRUCT_NAME,"COBMAG") != 0) return;

// skip
// this is a per event structure
// if (strcmp(DIRECTORY,"HCALREC_TIC") == 0) return;
// if (strcmp(STRUCT_NAME,"DBSC") == 0) return;
// if (strcmp(STRUCT_NAME,"COBMAG") == 0) return;

// Do not fill the event-store structures (when BPATH starts with "/RECB")
 if (lBPATH>4 && strncmp(pBPATH,"/RECB",5) == 0) return;
// if (strncmp(BPATH,"//RECB",6) == 0) return;

// Do not fill soon-to-be-obsolete muon structures DDSC,TTGC,RRPC,BBDT,QQGC
 if (m->instance() > 1 && strcmp(m->STRUCT_NAME(),"DDSC") == 0) return;
 if (m->instance() > 1 && strcmp(m->STRUCT_NAME(),"TTGC") == 0) return;
 if (m->instance() > 1 && strcmp(m->STRUCT_NAME(),"RRPC") == 0) return;
 if (m->instance() > 1 && strcmp(m->STRUCT_NAME(),"BBDT") == 0) return;
 if (m->instance() > 1 && strcmp(m->STRUCT_NAME(),"QQGC") == 0) return;

 //| DDSC   | /NCSC(1)/CCSC(1) |   96 |
 //| TTGC   | /NTGC(1)         |  528 |
 //| RRPC   | /NMDT(1)/AADT(3) | 1092 |
 //| BBDT   | /NMDT(1)/AADT(1) | 1184 |
 //| QQGC   | /NTGC(1)/TTGC(1) | 1584 |

  dictionary *d = new dictionary(pMAP, pNAME, pCOMMEN, pBANK, *pLLEN,
				 lNAME, lCOMMEN);

  persistent *Pobj = new persistent(m, d);

  unsigned int outIndex;
  outIndex=0;

// the IDNUM array is replaced with IDNUM2, IDNUM3, IDNUM4,...
// add IDNUMs (if neccessary)
  if( m->nIDNUM() > 0 ) {
    outIndex=addIDNUM ( Pobj, (const int) *pI1, (const int *) pIDNUM );
  }

// todo: remove outIndex
convert(outIndex, Pobj, pBANK, d);

dbwrite(Pobj);

if (gcflag_>3) cout <<"DBFILL: start cleanup"<<endl;

 delete m;

if (gcflag_>3) cout <<"DBFILL: end"<<endl;

} //end of dbfill
//*****************************************************************************//
//
//  convert data bank into persistent object shape for insertion into NOVA DB
//
//*****************************************************************************//
void convert(unsigned int outIndex, persistent *Pobj,
	     float *pBANK, const dictionary *d)
{
  float * pCURRENT;

  const int Lvalue=16;
  char VALUE[Lvalue+1]="";

//for IN () queries
  const int MAXBUF=8192;
  char temp[MAXBUF];

  unsigned int count=0;
  unsigned int offset=0;
  unsigned int outBytesIndex;
  outBytesIndex=0;

//for each array we will put array header first, than array values
//determine how many arrays there are
//LEN-2+arrays = include all arrays, exclude two first system words in the BANK
//   outLEN=LEN-2+arrays-dynLEN;
//to add IDNUM, BPATH we have to introduce outLLEN.
//   int outLLEN;

  for (unsigned int i=0; i<d->size(); i++)
    {
      //get dimensions for array

      int D1=d->getDescriptor(i)->firstDimension;
      int D2=d->getDescriptor(i)->secondDimension;

      if (gcflag_>1) cout <<" D1,D2= "<< D1 <<", "<< D2<<endl;

      if (d->getDescriptor(i)->mode == kBinary)
	{
	  if (gcflag_>1) {
	    cout << "var NAME \""
		 << d->getDescriptor(i)->name <<"\" "
		 << d->getDescriptor(i)->comment<<endl;
	    cout <<"TYPE "
		 << dictionary::GetTypeName(d->getDescriptor(i)->type) << endl;
	  }	
	  if (gcflag_>1) cout <<"special Case read D1,D2= "
			      << D1 <<", "<< D2<<endl;

	  //for dyn arrays just add array header
	  switch(d->getDescriptor(i)->type)
	    {
	    case kFloat:
	      Pobj->putType(outIndex,"fDynArray");
	      break;
	    case kInt:
	      Pobj->putType(outIndex,"iDynArray");
	      break;
	    case kText:
	      Pobj->putType(outIndex,"tDynArray");
	      break;
	    default:
	      cerr << "dblib converter ERROR unsupported DynArray type: "
		   << d->getDescriptor(i)->type <<endl;
	      break;
	    }
	  Pobj->putName(outIndex,d->getDescriptor(i)->name);
	  sprintf(temp, "[%d][%d]", D1,D2);
	  Pobj->putValue(outIndex,temp);
	  Pobj->putCommen(outIndex,d->getDescriptor(i)->comment);

	  int IADR=0;
	  int *pIADR=&IADR;
	  //this dyn array number index in fortran
	  int IDYN=outBytesIndex+1;
	  int *pIDYN=&IDYN;
	      
	  getdynbank_ (pIDYN, pIADR);
	  Pobj->putBytes(outBytesIndex, (const char*)IADR);
	  Pobj->putPosition(outBytesIndex,outIndex);

//
//debug printout
//
// 	  if (gcflag_>1) cout<<"i="<<i<<", outIndex="<<outIndex
//                    <<" variable "<<ppName[outIndex]<<ppValue[outIndex]<<endl;
	      
// 	      for (int ii=0;ii<D1;ii++) {
// 		for (int jj=0;jj<D2;jj++) {
// 		  int off=D1*jj+ii;

// // 		  //do not print out zero values
// 		  switch(TYPE[0])
// 		    {
// 		    case 'R':
// 		      if ( *((float*)ppBytes[outBytesIndex]+off) != 0 )
// 			cout<<" i="<<jj<<" j="<<ii<<" float "
//                       <<*((float*)ppBytes[outBytesIndex]+off)<<endl;
// 		      break;
// 		    case 'I':
// 		      if ( *((int*)ppBytes[outBytesIndex]+off) != 0 )
// 			cout<<" i="<<jj<<" j="<<ii<<"  int " 
//                        <<*((int*)ppBytes[outBytesIndex]+off)<<endl;
// 		      break;
// 		    case 'H':
// 		      if ( *((char*)ppBytes[outBytesIndex]+off) != 0 )
// 			cout<<" i="<<jj<<" j="<<ii<<" char " 
//                          <<((char*)ppBytes[outBytesIndex]+off)[0]
// 			               <<((char*)ppBytes[outBytesIndex]+off)[1]
// 			               <<((char*)ppBytes[outBytesIndex]+off)[2]
// 			               <<((char*)ppBytes[outBytesIndex]+off)[3]
//                                     <<endl;
// 		      break;
// 		    default:
// 		      cout << " unknown DynArray type: "<< TYPE <<endl;
// 		      break;
// 		    }
// 		}
// 	    }
//
//debug printout

	  outIndex++;

	  Pobj->putByteSize(outBytesIndex,4*D1*D2);
	  outBytesIndex++;
	  
	  offset+=D1*D2-1;
	  continue;
	}

      //save parameters info
      
      int headerDone;
      headerDone=0;
      
      int j;

      for (int k=0;k<D1;k++){
	for (int l=0;l<D2;l++){
	  count++;
	  
	  pCURRENT=pBANK+i+2+k+l+offset;
	  
 	if (gcflag_>2) {
	  cout << "offset " << offset << endl;
	  cout << "i, k, l "<<i<<", "<<k<<", "<<l<<" (void *) pCURRENT "
	       <<(void *)pCURRENT<<endl;
	}
	  switch(d->getDescriptor(i)->type)
	    {
	    case kFloat:
	      if (gcflag_>2) cout <<"(" << k+1 <<","<< l+1 << ") = "
				  << *pCURRENT<<endl;
	      //	    sprintf(VALUE, "%g", *pCURRENT );
	      //7 needed for calorimeter
	      sprintf(VALUE, "%.7g", *pCURRENT );
	      break;
	    case kInt:
	      if (gcflag_>2) cout<<"(" <<k+1<<","<<l+1<<") = "
				 << *((int*) pCURRENT)<<endl;
	      sprintf(VALUE, "%d", *((int*) pCURRENT) );
	      break;
	    case kText:
	      //where to get this 4 from?
	      // 	    strncpy(VALUE,(char*)pCURRENT,4);
	      if (gcflag_>2) cout <<"(" << k+1 <<","<< l+1 << ") = "
				  << VALUE <<endl;
	      
	      //remove trailing spaces from text (= char[4]) values
	      j=4;
	      while (j != 0 && *(((char*)pCURRENT)+j-1) == ' ') j--;
	      strncpy(VALUE,(char*)pCURRENT,j);
	      VALUE[j]='\0';

// incompatible types in assignment of `const char*' to `char[17]'
//  	      VALUE=convert2string ((const char*)pCURRENT, 4);

	      if (gcflag_>2) cout <<"(" << k+1 <<","<< l+1 << ") = \""
				  << VALUE<<"\"" <<endl;
	      break;
	    default:
	      cerr << "dblib converter ERROR: unsupported type!"<<endl;
	      break;
	    }

	  if (D1*D2==1)//not an array
	    {
	      Pobj->putName(outIndex,d->getDescriptor(i)->name);
              Pobj->putType(outIndex,
			    dictionary::GetTypeName(d->getDescriptor(i)->type));
	      Pobj->putValue(outIndex,VALUE);
	      Pobj->putCommen(outIndex,d->getDescriptor(i)->comment);

	      outIndex++;
	    }
	  else//for arrays add array header
	    {
	      if (headerDone==0)
		{
		  switch(d->getDescriptor(i)->type)
		    {
		    case kFloat:
		      Pobj->putType(outIndex,"fArray");
		      break;
		    case kInt:
		      Pobj->putType(outIndex,"iArray");
		      break;
		    case kText:
		      Pobj->putType(outIndex,"tArray");
		      break;
		    default:
		      cout << "dblib converter ERROR: unsupported array type: "
			   << d->getDescriptor(i)->type <<endl;
		      break;
		    }
		  Pobj->putName(outIndex,d->getDescriptor(i)->name);
		  sprintf(temp, "[%d]", D1*D2);
		  Pobj->putValue(outIndex,temp);
		  Pobj->putCommen(outIndex,d->getDescriptor(i)->comment);
		  outIndex++;
		  headerDone=1;
		}
	      
	      sprintf(temp, "%s[%d]", d->getDescriptor(i)->name, k+l);
	      Pobj->putName(outIndex,temp);
	      Pobj->putCommen(outIndex,"");
	      Pobj->putType(outIndex,
			    dictionary::GetTypeName(d->getDescriptor(i)->type));
	      Pobj->putValue(outIndex,VALUE);
	      
	      outIndex++;
	    }
	}
      }
      if (D1*D2>1)
	offset+=D1*D2-1;
    } //end of parameters loop

} //end of convert
//------------------------------------------------------------------------------/
unsigned int addIDNUM (persistent *Pobj, const int I1, const int* IDNUM)
{
  //IDNUMs are added at the top of the structure data
  unsigned int outIndex=0;
  char temp[64];
  //Add IDNUM array values as non-array variables
  for (int i=2; i<I1; i++) {
	
    sprintf(temp, "IDNUM%d", i);
    Pobj->putName(outIndex,temp);
    sprintf(temp, "value of IDNUM[%d] array element", i);
    Pobj->putCommen(outIndex,temp);
    Pobj->putType(outIndex,"int");
    sprintf(temp, "%d", IDNUM[i]);
    Pobj->putValue(outIndex,temp);
    
    if (gcflag_>1) cout <<"IDNUM [ "<< i <<"] "<< Pobj->getName(outIndex)
			<<" = "<<Pobj->getValue(outIndex) <<endl;
    
    outIndex++;
  }
  
  return outIndex;
}
//*****************************************************************************//
//
//   convert bank path into string for NOVA database fill
//
//*****************************************************************************//
const char* convertPath(const char* BPATH, const int I1, const int* IDNUM)
{
  char **ppPATH = new char*[I1];
  char temp[512];
  int i,j;
      // parse BPATH="//DETM/MUCH/NMDT/AADT*" from the end
      j = strlen(BPATH);
      for (i=0; i<I1; i++) {
	j--; while (j != 0  && BPATH[j-1] != '/') j--;
	strncpy(temp, BPATH+j ,4); temp[4]='\0';
	ppPATH[i]=strdup(temp);
      }

      for (i=2; i<I1-1; i++) {
	sprintf(temp, "/%s(%d)", ppPATH[I1-1-i],IDNUM[i]);
      }

      for (i=0;i<I1;i++) {	
	free((void*)ppPATH[i]);
      }
      delete [] ppPATH;

      if (gcflag_>1) for (i=0;i<I1;i++) {
	cout << "IDNUM[" << i << "] = " << IDNUM[i] << endl;
      }

      return strdup(temp);
}
//*****************************************************************************//
//
//    dbwrite(): Insert the persistent object data into the NOVA database  
// 
//    first check if these parameters are already exists in NOVA DB
//
// optional write logic:
//
// if (Pobj->instance() > 0 do not do the whole check - write structure to DB
// if ( Pobj->PATH() != NULL ) add the PATH information to the structure table
//
//*****************************************************************************//
void dbwrite(persistent *Pobj)
{
//for blobs
//   const int BINBUF=1048576; // 1MB limit
  const int BINBUF=4194304; // 4MB limit

  if (mysqlBinaryQueryBuffer==0) {
    mysqlBinaryQueryBuffer = new char[BINBUF];
    if (gcflag_>2) cout<<"DEBUG in dbwrite - allocated binary data buffer"<<endl;
  }
  if (mysqlBinaryQueryBuffer==0) {
    cerr<<"ERROR in dbwrite - failed to allocate buffer for binary data"<<endl;
  }

  int count;
  int strID;
  int parID;

//MYSQL mysql;
  MYSQL_RES *result;
  MYSQL_ROW row;
  MYSQL_RES *result2;

  unsigned int num_fields;
  unsigned int num_rows;
  unsigned int num_struct;
  unsigned int num_instances;

//   const int MAXBUF=8192;
    // buffer for constructing queries
#if defined(__GNUC__) &&  (__GNUC__ >= 3)
  ostringstream Query;
  // can't call seekp(0) on an empty sstream object
  Query << "dummy initialization needed for seekp(0) call"<<ends;
#else //.h for gcc-2 and SunOS
  const int MAXBUF=1024;
  char buf[MAXBUF];
  ostrstream Query(buf,MAXBUF);
#endif

  char *end;

  unsigned int i, j, k, m;

  if (gcflag_>3) {
    cout << "DBWRITE trying to insert: "<<endl;
    cout << "  structure " << Pobj->get()->STRUCT_NAME() << endl;
    cout << "  directory " << Pobj->get()->DIRECTORY() << endl;
    for (i=0;i<Pobj->getOutLEN();i++) {
      cout << "   "<< Pobj->getName(i)<< "="<<Pobj->getValue(i)<<" "<<endl;
    }
  }

// Check whether or not the connection to the server is working
// reconnect if necessary.
  if (mysql_ping(&mysql))
    {
      cerr << "Failed to reconnect to database: Error: "
	   <<  mysql_error(&mysql) << endl;
    }
  int querySize;
//   unsigned long *lengths;

// move bytes to the temp table on the server, calculate MD5 sum for bytes
  if (Pobj->getDynLEN()>0) {

    Query.seekp(0);
//     Query << "DELETE FROM temp"<<ends;
    Query << "TRUNCATE TABLE temp"<<ends;

//    if (mysql_real_query(&mysql,Query.str(),Query.pcount()-1))
     if (my_query(&Query))
      {
	cerr << "Failed to query: Error: " << mysql_error(&mysql) << endl;
	cerr << "database query: " << queryStr(&Query) << endl;
	return;
      }

    for (unsigned int i=0;i<Pobj->getDynLEN();i++)
      {
	Query.seekp(0);
	Query << "INSERT INTO temp SET position="<<Pobj->getPosition(i)
	      << ", bytes=\""<<ends;
	//make space for escaped bin data of length*2+1
#if defined(__GNUC__) &&  (__GNUC__ >= 3)
	querySize = 2*Pobj->getByteSize(i)+Query.tellp();
	if (querySize>BINBUF) {
	  cout << " skipped filling dyn array "<< i << endl;
	  continue;
	}
	end = strcpy(mysqlBinaryQueryBuffer,(const char *)Query.str().c_str());
	end += int(Query.tellp())-1;
#else
 	querySize = 2*Pobj->getByteSize(i)+Query.pcount();
	if (querySize>BINBUF) {
	  cout << " skipped filling dyn array "<< i << endl;
	  continue;
	}
	end = strcpy(mysqlBinaryQueryBuffer,(const char *)Query.str());
	end += Query.pcount()-1;
#endif
	end += mysql_real_escape_string(&mysql,end,
					Pobj->getBytes(i),Pobj->getByteSize(i));
	*end++ = '\"';
	if (gcflag_>2) cout<<"end - mysqlBinaryQueryBuffer "
			   <<(int) (end - mysqlBinaryQueryBuffer)<<endl;
	
	if (mysql_real_query(&mysql,mysqlBinaryQueryBuffer,
			     (int) (end - mysqlBinaryQueryBuffer)))
	  {
	    cerr << "Failed to insert mysqlBinaryQueryBuffer: Error: "
		 <<mysql_error(&mysql)<<endl;
	    cerr << "start of query: " << queryStr(&Query) << endl;
	    //      mysql_close(&mysql);
	  }

	Query.seekp(0);
	Query << "SELECT MD5(bytes) FROM temp WHERE position="
	      <<Pobj->getPosition(i)<<ends;
 	if (my_query(&Query))
	  {
	    cerr << "Failed to query: Error: " << mysql_error(&mysql) << endl;
	    cerr << "database query: " << queryStr(&Query) << endl;
	    return;
	  }
	else // query succeeded, get result
	  {
	    result = mysql_store_result(&mysql);
	    if (result)// query OK
	      {
		num_fields = mysql_num_fields(result);
		if (num_fields!=1) {
		  cerr << "ERROR: wrong size of MD5 sum query"<<endl;
		}
		// //must be just one row here
		if (mysql_num_rows(result) != 1) {
		  cerr << "ERROR: wrong num_rows in MD5 sum query"<<endl;
		  cerr << "   database query: " << queryStr(&Query) << endl;
		}
		row = mysql_fetch_row(result);
		if (row)
		  {
		    Pobj->putMD5sum(i,row[0]);
 		    if (gcflag_>3) cout<<"    position="<<Pobj->getPosition(i)
				       <<" md5sum="<<Pobj->getMD5sum(i)<<endl;
		  }
		mysql_free_result(result);
	      }
	  }// end of else query succeeded, get result
      }//end for (int i=0;i<dynLEN;i++)
  }//end if (dynLEN>0)

// // add top directory to structure table
// strcpy(query, "INSERT INTO structure SET\n" );
// sprintf(temp, "type=\"%s\",\n", "directory"); strcat(query,temp);
// sprintf(temp, "name=\"%s\",\n", "STAR"); strcat(query,temp);
// sprintf(temp, "instance=\"%u\",\n", 1); strcat(query,temp);
// sprintf(temp,"comment=\"%s\",\n","Top directory for all");strcat(query,temp);
// sprintf(temp, "author=\"%s\",\n", "Sasha Vanyashin"); strcat(query,temp);
// strcat(query, "flag=54321,\n");
// strcat(query, "entered=NOW(), parent=0\n");

// int topdirID;

// if (mysql_real_query(&mysql,query,strlen(query)))
//    {
//      cerr << "Failed to query: Error: " <<  mysql_error(&mysql) << endl;
//      mysql_close(&mysql);
//      return;
//    }
// else // insert succeeded, get last_insert_id();
//    {
//      topdirID = mysql_insert_id(&mysql);
//      if (gcflag_>1) cout << "top dir ID: " << topdirID << endl;
//    }

//top directory ID
//int topdirID=1;

//check directory name first
  int *existingDirID=NULL;
  int *existingDirInstance=NULL;
  unsigned int Instance=1;
  unsigned int directoryID=0;
  unsigned int dirInstance=0;
  unsigned int num_dir=0;

  Query.seekp(0);
//here should add and type=\"dataset\"
  Query << "SELECT ID, instance FROM structure"
	<<" WHERE type=\"dataset\""
	<<" AND name=\""<< Pobj->get()->DIRECTORY()<<"\""<<ends;
	

  if (gcflag_>4) cout << "database query: " << queryStr(&Query) << endl;

  if (my_query(&Query))
    {
      cerr << "Failed to query: Error: " <<  mysql_error(&mysql) << endl;
//      mysql_close(&mysql);
      cerr << "database query: " << queryStr(&Query) << endl;
      return;
    }
  else // query succeeded, get result
    {
      result = mysql_store_result(&mysql);
      if (result)
	{
	num_fields = mysql_num_fields(result);
	if (num_fields!=2) cerr <<"ERROR: wrong size of dir query"<<endl;

	num_dir = mysql_num_rows(result);
	if (num_dir==0)// this is the new directory name to insert
	  {
 	    if (gcflag_>1) cout<< "directory "<<Pobj->get()->DIRECTORY()
			       <<" does not exist in database" <<endl;

// add directory to structure table

	    Query.seekp(0);
	    Query << "INSERT INTO structure SET type=\"dataset\", name=\""
		  <<Pobj->get()->DIRECTORY()
		  <<"\", instance=1, comment=\""<<Pobj->get()->DCOMMENT()
		  <<"\", author=\""<<Pobj->get()->AUTHOR()
		  << "\", flag=54321, parent=0" << ends;
	    //parent="<<topdirID<<ends;

	    if (gcflag_>4) cout << "database query: "<<queryStr(&Query)<<endl;
	    if (!my_query(&Query))
	      {
		directoryID = mysql_insert_id(&mysql);
 		if (gcflag_>1) cout << "new directory ID: "<<directoryID<<endl;
		cout << "new NOVA DB directory "<<Pobj->get()->DIRECTORY()
		     <<" with ID: " << directoryID << endl;
	      }
	  }
	else   // this directory name already exists
	  {
	    existingDirID = new int[num_dir];
	    existingDirInstance = new int[num_dir];
	    for (i=0;i<num_dir;i++)
	      {
		row = mysql_fetch_row(result);
		existingDirID[i]=atoi(row[0]);
		existingDirInstance[i]=atoi(row[1]);
		if (gcflag_>1) cout<< "There is directory "
				   << Pobj->get()->DIRECTORY() <<" for version "
				   << existingDirInstance[i] <<endl;
		//FOR NOW USE THE LAST DIR ONLY:
		directoryID=existingDirID[i];
		dirInstance=existingDirInstance[i];
	      }
	  }
	mysql_free_result(result);
      }
    else   // query OK but, no result?!
      {
	cerr << "no result: Error: " <<  mysql_error(&mysql) << endl;
//      mysql_close(&mysql);
	cerr << "database query: " << queryStr(&Query) << endl;
	return;
      }
 }

//check if this structure is already exists in the structure table

//check if the directory+structure pair already exist
int existingDirInstanceForFirstStr;
existingDirInstanceForFirstStr=0;
int *existingStrID=NULL;
int *existingStrInstance=NULL;
Instance=1;
int same=0;
// int foundInstance = -1;

Query.seekp(0);
//Query<<"SELECT ID,Instance FROM structure WHERE name=\""
//<<Pobj->get()->STRUCT_NAME()<<"\""<<ends;

    Query << "SELECT structure.ID, structure.instance,"
	  << " directory.ID, directory.instance"
	  << " FROM structure as directory"
	  << " LEFT JOIN structure ON directory.ID=structure.parent"
	  << " WHERE directory.name=\"" << Pobj->get()->DIRECTORY()
	  << "\" AND structure.name=\"" << Pobj->get()->STRUCT_NAME() << "\"";

//Dec 15 add IDNUM to the query to speed-up filling for the simple case
//    if (I1==1 && IDNUM[0]>1) Query << " AND structure.instance=" << IDNUM[I1-1] << ends;
//Jan 14
    if (Pobj->get()->instance()>0) {
      Query << " AND structure.instance=" << Pobj->get()->instance();
      if ( Pobj->get()->PATH() != NULL ) {
	Query << " AND structure.path=\"" << Pobj->get()->PATH() << "\"";
      }
    }
    Query << ends;

if (gcflag_>4) cout << "database query: " << queryStr(&Query) << endl;
if (my_query(&Query))
  {
    cerr << "Failed to query: Error: " <<  mysql_error(&mysql) << endl;
//      mysql_close(&mysql);
    cerr << "database query: " << queryStr(&Query) << endl;
    return;
  }
else // query succeeded, get result
  {
    result = mysql_store_result(&mysql);
    if (result)
      {
	num_fields = mysql_num_fields(result);
	if (num_fields!=4) cerr << "ERROR: wrong size of dir+str query"<<endl;

	num_struct = mysql_num_rows(result);
	if (num_struct==0)// this is the new structure name to insert
	  {
	    if (gcflag_>1) {
	      cout << "structure " << Pobj->get()->STRUCT_NAME();
	    //tut neponyatno?!
// 	    if ( instanceProvided>1 && I1>2 ) cout <<" ["<< instanceProvided <<"]";
	      if ( Pobj->get()->instance()>0) {
		cout <<" ["<< Pobj->get()->instance() <<"]";
		if ( Pobj->get()->PATH() != NULL ) {
		  cout <<" with path \""<< Pobj->get()->PATH()<<"\" ";
		}
	      }
	      cout  << " does not exist in database directory "
		    << Pobj->get()->DIRECTORY()	<<endl;
	    }
	    mysql_free_result(result);
	    //not good for directory names
	    goto insertStructure;
	  }
	else   // this structure name already exists
	  {
	    existingStrID = new int[num_struct];
	    existingStrInstance = new int[num_struct];

	    if (gcflag_>1) {
	      cout<< "There are "<<num_struct<<" instances of "
		  << Pobj->get()->STRUCT_NAME()<<endl;
	      if (Pobj->get()->instance()>0) {
		cout<< "   This is an overwrite request for instanceProvided="
		    <<Pobj->get()->instance()<<endl;
		if ( Pobj->get()->PATH() != NULL ) {
		  cout<< "     at path \""<< Pobj->get()->PATH()<<"\""<<endl;
		}
	      }
	    }

	    for (i=0;i<num_struct;i++)
	      {
		row = mysql_fetch_row(result);
		existingStrID[i]=atoi(row[0]);
		existingStrInstance[i]=atoi(row[1]);

		if ( existingStrID[i] == atoi(row[2])+1 )
		  existingDirInstanceForFirstStr=atoi(row[3]);
		else
		  existingDirInstanceForFirstStr=0;

		if (gcflag_>1) cout<< "There is "<< Pobj->get()->STRUCT_NAME()
				   <<" [" << existingStrInstance[i] <<"]"<<endl;

    //is this a very first structure in the directory?
    // then we have to insert the new directory instance as well
		if (gcflag_>1) {
		  if ( existingDirInstanceForFirstStr > 0 )
		    cout<< "which is the first structure in "
			<< Pobj->get()->DIRECTORY()
			<<" DIRECTORY with version = "
			<< existingDirInstanceForFirstStr << endl;
		}
	      }
	  }
	mysql_free_result(result);
      }
    else   // query OK but, no result?!
      {
	cerr << "no result: Error: " <<  mysql_error(&mysql) << endl;
//      mysql_close(&mysql);
	cerr << "database query: " << queryStr(&Query) << endl;
	return;
      }
  }

//this structure name already exists in the structure table

// until the IDNUM is properly handled, fill all dynamic structures
// this slow down to check does not speed up - just slows down?!
// relation required between bytes and structures

 if (Pobj->getDynLEN()>0)
   goto insertStructure;

//to speed up checking
//first compare dyn arrays

 for (unsigned int i=0;i<Pobj->getDynLEN();i++)
   {
     Query.seekp(0);
     Query << "SELECT parID FROM bytes WHERE md5sum=\'"<<Pobj->getMD5sum(i)
	   <<"\' AND strID IN (";
     for (j=0;j<num_struct-1;j++)
       {
	 Query << existingStrID[j] <<", ";
       }
     Query << existingStrID[num_struct-1] <<")";
     Query <<ends;
     if (gcflag_>4) cout << "database query: " << queryStr(&Query) << endl;
     
     if (my_query(&Query))
       {
	 cerr << "Failed to query: Error: " <<  mysql_error(&mysql) << endl;
	 cerr << "database query: " << queryStr(&Query) << endl;
	 return;
       }
     else // query succeeded, get result
       {
	 result = mysql_store_result(&mysql);
	 if (result)// query OK
	   {
	     num_fields = mysql_num_fields(result);
	     if (num_fields!=1) cerr<<"ERROR: wrong size of md5sum query"<<endl;
	     
	     num_rows = mysql_num_rows(result);
	     mysql_free_result(result);
	     
 	     if (gcflag_>2) cout<<"matching md5sums: "<< num_rows <<endl;
	     
	     if (num_rows==0) //size is different from this ID
	       {
		 if (gcflag_>1) cout<<"structure "<<Pobj->get()->STRUCT_NAME()
		      <<" has no matching md5sum for dyn variable "<<i <<endl;
		 goto insertStructure;
	       }
	   }
       }// end of else query succeeded, get result
   }//end for (int i=0;i<dynLEN;i++)

//fetch structure parameters (for comparison)
for (i=0;i<num_struct;i++)
{
  //same variables counter
  unsigned int count=0;

  if (gcflag_>2) cout << "i, num_struct, existingStrID[i]: " << i<<" "
		      <<num_struct<<" "<<existingStrID[i] << endl;

  //the alternative to tmp is to use UNION and use the position mapping
  // in data member comparison 
  // unfortunately, no position mapping exists now for array elements position

  //Truncate operations drop and re-create the table, which is much faster
  // than deleting rows one by one.
   Query.seekp(0);
    Query << "TRUNCATE TABLE tmp"<<ends;
    if (my_query(&Query))
      {
	cerr << "Failed to query: Error: " << mysql_error(&mysql) << endl;
	cerr << "database query: " << queryStr(&Query) << endl;
	return;
      }
  //fill non-array values
  Query.seekp(0);
  Query << "INSERT INTO tmp"
	<<" SELECT parameter.name, parameter.value, parameter.comment,"
	<<"        parameter.type, parameter.ID, position"
	<<" FROM structure, relation, parameter"
	<<" WHERE structure.ID=relation.strID AND parameter.ID=relation.parID"
	<<" AND structure.ID="<<existingStrID[i]<<ends;
  if (gcflag_>4) cout << "database query: " << queryStr(&Query) << endl;
  if (my_query(&Query))
    {
      cerr << "Failed to query: Error: " <<  mysql_error(&mysql) << endl;
      cerr << "database query: " << queryStr(&Query) << endl;
      return;
    }

  //add array elements values
  Query.seekp(0);
  Query << "INSERT INTO tmp"
	<<" SELECT array.name, array.value, array.comment,"
	<<"        array.type, array.ID, position"
	<<" FROM structure, relation, parameter, parameter AS array"
	<<" WHERE structure.ID=relation.strID AND parameter.ID=relation.parID"
	<<" AND parameter.ID=array.arrayID"
	<<" AND structure.ID="<<existingStrID[i]<<ends;
  if (gcflag_>4) cout << "database query: " << queryStr(&Query) << endl;
  if (my_query(&Query))
    {
      cerr << "Failed to query: Error: " <<  mysql_error(&mysql) << endl;
      cerr << "database query: " << queryStr(&Query) << endl;
      return;
    }

  //now do the SELECT
  Query.seekp(0);
  Query << "SELECT name, value, comment, type, ID FROM tmp ORDER BY position, ID"<<ends;

  if (gcflag_>4) cout << "database query: " << queryStr(&Query) << endl;
  if (my_query(&Query))
    {
      cerr << "Failed to query: Error: " <<  mysql_error(&mysql) << endl;
//       mysql_close(&mysql);
      cerr << "database query: " << queryStr(&Query) << endl;
      return;
    }
  else // query succeeded, get result
    {
      result = mysql_store_result(&mysql);
      if (result)// query OK
	{
	  num_fields = mysql_num_fields(result);
	  if (num_fields!=5) cerr << "ERROR: wrong size of parID query"<<endl;
	
	  num_rows = mysql_num_rows(result);

	  if (gcflag_>2) cout<<"total variables: "<< num_rows <<endl;

	  if (num_rows!=Pobj->getOutLEN()) //size is different from this ID
	    {
	      cout<<"database structure "<<i<<" is of "<< num_rows
		  <<" rows, this structure is "<< Pobj->getOutLEN() <<endl;
	      cout<<"database structure name is "<< Pobj->get()->STRUCT_NAME()<<endl;
	      mysql_free_result(result);
	      continue;
	    }
	  else //same size size: check names
	    {
	      int dynIndex=0;

	      for (unsigned int j=0;j<num_rows;j++)
		{
		  row = mysql_fetch_row(result);

		  //most often the first variable value is different
                  //different value, new structure?
		  if (strcmp(Pobj->getValue(j),row[1])) {
 		    if (gcflag_>3) cout << "variable " << j
					<< " has different Value: \""
					<<Pobj->getValue(j)<<"\" \""
					<<row[1]<<"\""<<endl;
		    break;
		  }

 		  if (gcflag_>3) cout<<"Name: \""<<Pobj->getName(j)<<"\" \""<<row[0]<<"\""<<endl;
		  if (strcmp(Pobj->getName(j),row[0])) {//this variable is different
 		    if (gcflag_>3) cout<<"variable "<<j<<" has different Name: \""<<Pobj->getName(j)<<"\" \""<<row[0]<<"\""<<endl;
		    break;
		  }
		  //same name: check value
		  if (gcflag_>3) cout<<"Values: \""<<Pobj->getValue(j)
                      <<"\" \""<<row[1]<<"\""<<endl;

		  //same value: check comment
		  if (gcflag_>3) cout<<"Comment: \n\""<<Pobj->getCommen(j)
                  <<"\"\n\""<<row[2]<<"\""<<endl;

                  //different comment, new structure?
		  if (strcmp(Pobj->getCommen(j),row[2])) {
 		    if (gcflag_>3) cout<<"variable "<<j<<" has different Comment: \""
 			<<Pobj->getCommen(j)<<"\" \""<<row[2]<<"\""<<endl;
		      break;
		    }

		  //same comment: check type
                  //different type, new structure?
		  if (strcmp(Pobj->getType(j),row[3])) {
 		    if (gcflag_>3) cout<<"variable "<<j<<" has different Type: \""<<Pobj->getType(j)<<"\" \""<<row[3]<<"\""<<endl;
		    break;
		  }
		  //same type: check md5sum for blobs
		  if (Pobj->getType(j)[1]=='D') {
		    if (gcflag_>1) cout<<"this is dyn varable "<<Pobj->getName(j)<<", parID="<<row[4]<<endl;

		    //while in the first structure, check md5sum for all structures

		    Query.seekp(0);
// 		    Query << "SELECT md5sum FROM bytes WHERE strID="<<existingStrID[i]
// 			  <<" AND parID="<<row[4]<<ends;
		    Query << "SELECT count(*) FROM bytes WHERE strID="<<existingStrID[i]
			  <<" AND parID="<<row[4]<<" AND md5sum=\'"<<Pobj->getMD5sum(dynIndex)<<"\'"<<ends;
 		    if (gcflag_>4) cout << "database query: " << queryStr(&Query) << endl;
		    if (my_query(&Query))
		      {
			cerr << "Failed to query: Error: " << mysql_error(&mysql) << endl;
			cerr << "database query: " << queryStr(&Query) << endl;
			return;
		      }
		    else // query succeeded, get result
		      {
			result2 = mysql_store_result(&mysql);
			if (result2)// query OK
			  {
			    num_fields = mysql_num_fields(result2);
			    if (num_fields!=1) cerr << "ERROR: wrong size of md5sum query"<<endl;
			    if (mysql_num_rows(result2) != 1) // must be just one row here
			      {
				cerr << "ERROR: wrong num_rows in md5sum query"<<endl;
				cerr << "database query: " << queryStr(&Query) << endl;
			      }
			    else
			      {
				row = mysql_fetch_row(result2);
				if (row)
				  {
				    if (atoi(row[0])==0) {
				      cout<<"variable "<<j<<" has a different blob"<<endl;
				      cout<< "query "<<queryStr(&Query)<<" found no rows "<<row[0]<<endl;
 				      mysql_free_result(result2);
 				      break;
				    }

 				    if (gcflag_>2) cout<< "query "<<queryStr(&Query)<<" found md5sum="<<row[0]<<endl;
// 				    if (strcmp(ppMD5sum[dynIndex],row[0])) {
// 				      if (gcflag_>2) cout<<"  but variable at position="<<j
// 					  <<"          has md5sum="<<ppMD5sum[dynIndex]<<endl;
// 				      mysql_free_result(result2);
// 				      break;
// 				    }
 				    if (gcflag_>2) cout<<"              same as variable at position="<<j
 					<<"          that has md5sum="<<Pobj->getMD5sum(dynIndex)<<endl;
				  }
			      }
			    mysql_free_result(result2);
			  }
		      }
		    dynIndex++;
		  }
 		  if (gcflag_>1) cout<<"variable "<< j+1 <<" is the same"<<endl;
		  count++;
		}//end of loop over all variables
	
	      if (count==Pobj->getOutLEN())
		{
		  if (gcflag_>0) cout<<"The "<< Pobj->get()->STRUCT_NAME()
		      <<" is found in database as Instance "
		      <<existingStrInstance[i] <<" ID "<<existingStrID[i]<<endl;
		  same=1;
		  // break out of the loop over DB structures when found same
		  mysql_free_result(result);
		  break;
		}//end if (count==outLEN)
	    }//end if equal size structures
	  mysql_free_result(result);
	}
      else   // something wrong
	{
	  cerr << "no result, Error: " <<  mysql_error(&mysql) << endl;
// 	  mysql_close(&mysql);
	  cerr << "database query: " << queryStr(&Query) << endl;
	  return;
	}
    }

}//end of loop over all existing structure IDs

insertStructure:

//for insert structure query
//  if (dynLEN>0) //because we do not loop over all existing structures in case of dyn arrays
//    {
//      if (existingStrInstance != NULL)
//        num_instances = existingStrInstance[0];
//      else
//        num_instances = 0;
//    }
//  else
//    {
     num_instances = num_struct;
//    }
      if (gcflag_>2) cout<<" num_instances "<<num_instances;
      if (gcflag_>2) cout<<" num_struct "<<num_struct<<endl;

int *existingArrayID=NULL;
char * DIM=NULL;
char * Dim=NULL;

if (same==0)//we have to insert this structure
  {
    //is this a very first structure in the directory?
    //there can be only one first structure in the directory
    // we have to insert the new directory version
    if ( existingDirInstanceForFirstStr > 0 )
      {
      if (gcflag_>1) cout << "we have to insert the new directory instance as well"<<endl;
      //decrement instance count for this structure
      num_instances=0;
      //      num_struct=0;

      // add directory to structure table (no path)

      Query.seekp(0);
      Query << "INSERT INTO structure SET"
            << " type=\"dataset\", name=\""<<Pobj->get()->DIRECTORY()
            <<"\", instance="<< existingDirInstanceForFirstStr+1
            <<", comment=\""<<Pobj->get()->DCOMMENT()
            <<"\", author=\""<<Pobj->get()->AUTHOR()
            <<"\", flag=54321, parent=0" << ends;
//parent="<<topdirID<<ends;

      if (gcflag_>4) cout << "database query: " << queryStr(&Query) << endl;
      if (!my_query(&Query))
	{
	  directoryID = mysql_insert_id(&mysql);
	  if (gcflag_>1) cout << "new directory ID: " << directoryID << endl;
	}
      else
	{
	  cerr << "Failed to query: Error: " <<  mysql_error(&mysql) << endl;
// 	  mysql_close(&mysql);
	  cerr << "database query: " << queryStr(&Query) << endl;
	  return;
	}
      }
    Query.seekp(0);
    Query << "INSERT INTO structure SET"
          << " type=\"structure\", name=\""<<Pobj->get()->STRUCT_NAME();

    //    if (I1==1 && IDNUM[0]>1)
    if (Pobj->get()->instance()>0) {
      Query << "\", instance="<<Pobj->get()->instance();
      if (Pobj->get()->PATH()!=NULL) {
	Query << ", path=\""<<Pobj->get()->PATH()<<"\"";
      }
    }
    else {
      Query << "\", instance="<<num_instances+1;
    }

    Query << ", comment=\""<<Pobj->get()->BANK_TITLE()
          << "\", author=\"\", flag=54321, parent="<<directoryID<<ends;

    if (gcflag_>4) cout << "database query: " << queryStr(&Query) << endl;
    if (!my_query(&Query))
      {
	strID = mysql_insert_id(&mysql);

	if (gcflag_>1) cout << "new strID: " << strID << endl;
	//keep the strID to check for the second call in case of dyn array
// 	lastInsertedStrID=strID;

	
	//now insert parameters
	//first check if this parameter already there
	int offs;
	offs=0;
	//was
	//	for (int j=0;j<LLEN;j++)
	//with IDNUM, BPATH
	for (unsigned int j=0;j<Pobj->getOutLLEN();j++)
	  {
	    int i;
	    i=j+offs;

	    //if array - more checking to be done
	    //check that array element values are the same
	
	    if (Pobj->getType(i)[1]=='A')
	      {
		int sameArray=0;

		//int num_elem;
		int dim;
		int len;
		dim=0;
		DIM=strdup(Pobj->getValue(i));
		Dim=strdup(DIM);
		len=strlen(DIM)-2;
		for (int l=0;l<len;l++) Dim[l]=DIM[l+1];
		Dim[len]='\0';
		dim=atoi(Dim);
		
		if (gcflag_>3) cout<<"dimensions "<<dim<<endl;

		unsigned int num_arrays;
		
		Query.seekp(0);
		Query << "SELECT DISTINCT parameter.ID FROM structure"
		  //Query << "SELECT parameter.ID FROM structure"
		      <<" LEFT JOIN relation ON structure.ID=relation.strID"
		      <<" LEFT JOIN parameter ON parameter.ID=relation.parID"
		  //can't use structure.ID instead of the structure.name - see below
		      <<" WHERE structure.name=\""<< Pobj->get()->STRUCT_NAME() <<"\""
		      <<" AND parameter.name=\""<< Pobj->getName(i) <<"\""
		      <<" AND parameter.type=\""<< Pobj->getType(i)<<"\""
		      <<" AND parameter.value=\""<< Pobj->getValue(i)<<"\""
		      <<" AND parameter.comment=\""<< Pobj->getCommen(i) <<"\""
		      <<" AND relation.position="<<i+1<<ends;

if (gcflag_>4) cout << "database query: " << queryStr(&Query) << endl;
		if (my_query(&Query))
		  {
		    cerr << "Failed to query: Error: "
                         <<  mysql_error(&mysql) << endl;
// 		    mysql_close(&mysql);
		    cerr << "database query: " << queryStr(&Query) << endl;
		    return;
		  }
		else
		  {
		    result = mysql_store_result(&mysql);
		    if (!result)
		      {
			cerr << "No result?!: Error: "
			     <<  mysql_error(&mysql) << endl;
// 			mysql_close(&mysql);
			cerr << "database query: " << queryStr(&Query) << endl;
			return;
		      }
		    else // query succeded
		      {
			num_fields = mysql_num_fields(result);
			if (num_fields!=1)
			  cerr << "ERROR: size of parID query not as expected"
			       << endl;
			num_arrays = mysql_num_rows(result);
                        // this is the new array+dimensions insert
			if (num_arrays==0)
			  {
			    if (gcflag_>1) cout << "array " << Pobj->getName(i)<<Pobj->getValue(i)
				 << " is new for structure " << Pobj->get()->STRUCT_NAME()
				 <<endl;
			    goto new_header;
			  }
			else   // this array name already exists
			  {
			    existingArrayID = new int[num_arrays];
			    for (k=0;k<num_arrays;k++)
			      {
				row = mysql_fetch_row(result);
				existingArrayID[k]=atoi(row[0]);
			      }
			  }
			mysql_free_result(result);
		      }
		  }
//this array name already exists in the structure table
//fetch array element values (for comparison)

for (k=0;k<num_arrays;k++)
{
  //same variables counter
  count=0;

  Query.seekp(0);
  Query << "SELECT value FROM parameter WHERE arrayID="
	<<existingArrayID[k]<<ends;
  if (gcflag_>4) cout << "database query: " << queryStr(&Query) << endl;

  if (my_query(&Query))
    {
      cerr << "Failed to query: Error: " <<  mysql_error(&mysql) << endl;
//       mysql_close(&mysql);
      cerr << "database query: " << queryStr(&Query) << endl;
      return;
    }
  else // query succeeded, get result
    {
      result = mysql_store_result(&mysql);
      if (result)// query OK
	{
	  num_fields = mysql_num_fields(result);
	  if (num_fields!=1) cerr << "ERROR: wrong size of arrayID query"
				  <<endl;

	  unsigned int num_elem = mysql_num_rows(result);
	  if ((int)num_elem!=dim)
	    {
	      if (gcflag_>1) cout << "new size of existing array"<<endl;
	      continue;//looking for the same array among all arrays
	    }
	  else
	    {
	      for(m=0;m<num_elem;m++)
		{
		  row = mysql_fetch_row(result);

// //remove trailing spaces
// 		  if (ppType[i][0]=='t')
// 		    {
// // 		      j=4; while (j != 0  && *ppValue[i+m+j] == ' ') j--;
// // 		      ppValue[i+m+j]='\0';
		
// 		      if (gcflag_>2) cout <<"ppValue[i+1+m] = \"" <<ppValue[i+1+m]<<"\""<<endl;
// 		      if (gcflag_>2) cout<<"         row[0] = \""<<row[0]<<"\""<<endl<<endl;
// 		    }
		  if(strcmp(Pobj->getValue(i+1+m),row[0]))
		    break;
		  count++;
		}

	      if (count==dim)//same array is found (all values are the same)
		{
		  if (gcflag_>1) cout<<"The "<< Pobj->getName(i)<<Pobj->getValue(i)
		      <<" is found in database with ID = "
		      <<existingArrayID[k]<<endl;

		    // add new structure, old variable pair to relation table
		    Query.seekp(0);
		    Query <<"INSERT INTO relation SET strID="
			  << strID <<", parID="
			  << existingArrayID[k] <<", position="<<i+1<<ends;
		
		    if (my_query(&Query))
		      {
			cerr << "Failed to query: Error: "
			     << mysql_error(&mysql) << endl;
// 			mysql_close(&mysql);
			cerr << "database query: " << queryStr(&Query) << endl;
			return;
		      }
		    else // insert succeeded, Ura!
		      {
			if (gcflag_>1) cout << "inserted relation pair for old array  "
			     << strID
			     << ", " << existingArrayID[k] << endl;
		      }
		    sameArray=1;//this will skip to next variable
		    break;//out of the loop over number of existing arrays
		}
	    }//end if equal size arrays
	  mysql_free_result(result);
	}
      else   // something wrong
	{
	  cerr << "no result, Error: " <<  mysql_error(&mysql) << endl;
// 	  mysql_close(&mysql);
	  cerr << "database query: " << queryStr(&Query) << endl;
	  return;
	}
    }

}//end of loop over all existing arrays IDs

new_header:

if (sameArray==0)//we have to insert this array
  {
    // new parameter name - insert and get parID to fill relation

    if (gcflag_>1) cout<< Pobj->getName(i)<<Pobj->getValue(i)
	<<" does not exist in "<<Pobj->get()->STRUCT_NAME() <<endl;
    if (gcflag_>1) cout << "Inserting array header: "<< Pobj->getName(i)<<Pobj->getValue(i)<<endl;

    Query.seekp(0);
    Query <<"INSERT INTO parameter SET"
          <<" name=\""<< Pobj->getName(i)
          <<"\", type=\""<< Pobj->getType(i)
          <<"\", value=\""<< Pobj->getValue(i)
          <<"\", comment=\""<< Pobj->getCommen(i) <<"\""<<ends;

    if (gcflag_>4) cout << "database query: " << queryStr(&Query) << endl;

    if (my_query(&Query))
      {
	cerr << "Failed to query: Error: " <<  mysql_error(&mysql) << endl;
// 	mysql_close(&mysql);
	cerr << "database query: " << queryStr(&Query) << endl;
	return;
      }
    else // insert succeeded, get last_insert_id();
      {
	parID = mysql_insert_id(&mysql);
			
	// add new structure, new variable pair to relation table
	Query.seekp(0);
	Query <<"INSERT INTO relation SET strID="<< strID
              <<", parID="<< parID <<", position="<<i+1<<ends;
	
	if (my_query(&Query))
	  {
	    cerr << "Failed to query: Error: " <<  mysql_error(&mysql) << endl;
// 	    mysql_close(&mysql);
	    cerr << "database query: " << queryStr(&Query) << endl;
	    return;
	  }		
	//else // insert succeeded, Ura!
        if (gcflag_>1) cout << "inserted new array header relation pair "
	     << strID << ", " << parID << endl;
      }

    // add array elements to par table
    for (int m=0; m<dim; m++)
      {

	Query.seekp(0);
	Query << "INSERT INTO parameter SET name=\""<< Pobj->getName(i+1+m)
              <<"\", type=\""<< Pobj->getType(i+1+m)
              <<"\", value=\""<< Pobj->getValue(i+1+m)
              <<"\", comment=\"\", arrayID="<<parID << ends;

	if (gcflag_>4) cout << "database query: " << queryStr(&Query) << endl;

	if (my_query(&Query))
	  {
	    cerr << "Failed to query: " <<  mysql_error(&mysql) << endl;
// 	    mysql_close(&mysql);
	    cerr << "database query: " << queryStr(&Query) << endl;
	    return;
	  }
      }

  }//end of sameArray==0 if

    offs+=dim; //take into account need to skip array elements

	      }
	    else//not an array
	      {
	    Query.seekp(0);
	    //without distinct: new NOVA DB directory PIXBGEO with ID: 13 ERROR: 2 parID found
	    //	    Query << "SELECT parameter.ID FROM structure,relation,parameter"

	    //DISTINCT causes use of temporary
	    Query << "SELECT DISTINCT parameter.ID FROM structure,relation,parameter"
		  <<" WHERE structure.ID=relation.strID AND parameter.ID=relation.parID"
	      //cant' use structure.ID instead of the structure.name - this strID is for the new structure without this parameter
		  <<" AND structure.name=\""<< Pobj->get()->STRUCT_NAME() <<"\""
		  <<" AND parameter.name=\""<< Pobj->getName(i) <<"\""
		  <<" AND parameter.type=\""<< Pobj->getType(i) <<"\""
		  <<" AND parameter.value=\""<< Pobj->getValue(i) <<"\""
		  <<" AND parameter.comment=\""<< Pobj->getCommen(i) <<"\""
		  <<" AND relation.position="<<i+1<<ends;

if (gcflag_>4) cout << "database query: " << queryStr(&Query) << endl;
	if (!my_query(&Query))
	  {
	    result = mysql_store_result(&mysql);
	    if (!result)
	      {
		cerr << "No result?!: Error: " <<  mysql_error(&mysql) << endl;
// 		mysql_close(&mysql);
		cerr << "database query: " << queryStr(&Query) << endl;
		return;
	      }
	    else // query succeded
	      {
		num_fields = mysql_num_fields(result);
		if (num_fields!=1)
		  cerr << "ERROR: size of parID query not as expected" << endl;

		num_rows = mysql_num_rows(result);
		switch (num_rows)
		  {
		  case 1:
		    // this parameter name and value already exists
                    // for this structure
		    //get parID to fill relation
		    row = mysql_fetch_row(result);
		    parID=atoi(row[0]);
		    if (gcflag_>1) cout << "There is "<<Pobj->getName(i)<<" = "<<Pobj->getValue(i)
			 <<" with ID: "<< parID <<endl;
		
		    // add new structure, old variable pair to relation table
		    Query.seekp(0);
		    Query <<"INSERT INTO relation SET strID="
			  << strID <<", parID="
			  << parID <<", position="<<i+1<<ends;
if (gcflag_>4) cout << "old par database query: " << queryStr(&Query) << endl;

		    if (my_query(&Query))
		      {
			cerr << "Failed to query: Error: "
			     <<  mysql_error(&mysql) << endl;
// 			mysql_close(&mysql);
			cerr << "database query: " << queryStr(&Query) << endl;
			return;
		      }
		    else // insert succeeded, Ura!
 		      if (gcflag_>2) cout << "inserted  old variable relation pair " << strID
 			   << ", " << parID << endl;
		    break;
		  case 0:

// 		  new_parameter:

		    // new parameter name - insert
                    // and get parID to fill relation

		    if (gcflag_>2) cout<< Pobj->getName(i)<<" = "<<Pobj->getValue(i)
				       <<" does not exist in "<<Pobj->get()->STRUCT_NAME() <<endl;

		    if (gcflag_>2) cout << "Inserting variable: " << Pobj->getName(i) << endl;

		  Query.seekp(0);
		  Query <<"INSERT INTO parameter SET"
			<<" name=\""<<  Pobj->getName(i)
			<<"\", type=\""<< Pobj->getType(i)
			<<"\", value=\""<< Pobj->getValue(i)
			<<"\", comment=\""<< Pobj->getCommen(i) <<"\""<<ends;

		  if (gcflag_>4) cout << "database query: " << queryStr(&Query) << endl;

		  if (my_query(&Query))
		      {
			cerr << "Failed to query: Error: "
			     <<  mysql_error(&mysql) << endl;
// 			mysql_close(&mysql);
			cerr << "database query: " << queryStr(&Query) << endl;
			return;
		      }
		    else // insert succeeded, get last_insert_id();
		      {
			parID = mysql_insert_id(&mysql);
			
			// add new structure, new variable pair
                        // to relation table
			Query.seekp(0);
			Query <<"INSERT INTO relation SET strID="
                              << strID <<", parID="<< parID
                              <<", position="<<i+1<<ends;

			if (my_query(&Query))
			  {
			    cerr << "Failed to query: Error: "
				 <<  mysql_error(&mysql) << endl;
// 			    mysql_close(&mysql);
			    cerr << "database query: " << queryStr(&Query) << endl;
			    return;
			  }		
			//else // insert succeeded, Ura!
			  if (gcflag_>2) cout << "inserted relation pair "
					      << strID << ", " << parID << endl;
		      }
		  break;
		  default: //(num_rows>1)
		    cerr << "ERROR: "<<num_rows <<" parID found" << endl;
		    cerr << "database query: " << queryStr(&Query) << endl;
// 		    mysql_close(&mysql);
		    return;
		    break;
		  }
		mysql_free_result(result);
	      }
	  }
	else
	  {
	    cerr << "Failed to query: Error: " <<  mysql_error(&mysql) << endl;
// 		    mysql_close(&mysql);
	    cerr << "database query: " << queryStr(&Query) << endl;
	    return;
	  }
	      }//end not an array ifelse
      }//end loop over parameters
  }
else
  {
    cerr << "Failed to query: Error: " <<  mysql_error(&mysql) << endl;
// 		    mysql_close(&mysql);
    cerr << "database query: " << queryStr(&Query) << endl;
    return;
  }

// insertBytes:
    if (gcflag_>3) cout << "start insertBytes: " << endl;

if (Pobj->getDynLEN()>0)//we have to insert bytes for dyn arrays
  {
    //get dyn arrays IDs
    Query.seekp(0);
    Query << "SELECT parameter.ID"
	  << " FROM structure "
	  << " LEFT JOIN relation ON structure.ID=relation.strID"
	  << " LEFT JOIN parameter ON parameter.ID=relation.parID"
	  << " WHERE parameter.type LIKE '%DynArray' "
	  << " AND structure.ID="<<strID
	  << " ORDER BY relation.position"<<ends;

    if (gcflag_>4) cout << "database query: " << queryStr(&Query) << endl;
    if (my_query(&Query))
      {
	cerr << "Failed to query: Error: " <<  mysql_error(&mysql) << endl;
	//       mysql_close(&mysql);
	cerr << "database query: " << queryStr(&Query) << endl;
	return;
      }
    else // query succeeded, get result
      {
	result = mysql_store_result(&mysql);
	if (result)// query OK
	  {
	    num_fields = mysql_num_fields(result);
	    if (num_fields!=1) cerr << "ERROR: wrong size of dyn array ID query"<<endl;
	
	    num_rows = mysql_num_rows(result);
	
	    if (num_rows!=Pobj->getDynLEN()) //size is different from this ID
	    {
	      cerr<<"database structure "<<strID
		  <<" has "<< num_rows
		  <<" dyn Arrays, this structure has "<< Pobj->getDynLEN() <<endl;
	    }
	  else //same size size: insert bytes
	    {
	      for (unsigned int i=0;i<num_rows;i++)
		{
		  row = mysql_fetch_row(result);

		  Query.seekp(0);

 		  Query << "INSERT INTO bytes (strID, parID, md5sum, bytes) SELECT "
			<< strID<<", "<<row[0]<<", \'"<<Pobj->getMD5sum(i)
			<<"\', bytes FROM temp WHERE position="<<Pobj->getPosition(i)<<ends;

 		  if (gcflag_>4) cout << "database query: " << queryStr(&Query) << endl;

		  if (my_query(&Query))
		    {
		      cerr << "Failed to query: Error: " <<  mysql_error(&mysql) << endl;
		      cerr << "database query: " << queryStr(&Query) << endl;
		      return;
		    }
		}
	    }//end of else //same size size: insert bytes
	    mysql_free_result(result);
	  }
      }
  }// end of if (dynLEN>0)//we have to insert bytes for dyn arrays

  }//end of same==0 if

//close:

// We are done with the connection, call mysql_close() to terminate it

//mysql_close(&mysql);

if (gcflag_>3) cout <<"freeing DIM"<<endl;

 if (DIM !=NULL) free((void*)DIM);
 if (Dim !=NULL) free((void*)Dim);

if (gcflag_>3) cout <<"deleting existing... pointers"<<endl;

 if (existingDirID       != NULL) delete [] existingDirID;
 if (existingDirInstance != NULL) delete [] existingDirInstance;
 if (existingStrID       != NULL) delete [] existingStrID;
 if (existingStrInstance != NULL) delete [] existingStrInstance;
 if (existingArrayID     != NULL) delete [] existingArrayID;

 delete Pobj;

if (gcflag_>3) cout <<"DBWRITE: end cleanup, returning"<<endl;

//return OK status
//*pI1=0;
return;

}
//*****************************************************************************//
// idea: to get data from database for geant geometry
// DBUSE to be called from fortran instead of AgDGETP
//
// to be done:
// 1) put selectors to meta class
// 2) make cross-checking for names and types
//     with requested names and database names
//    of structures data members, so that obsolete structures can be filled
// 3) add configuration level
// 4) implement possibility for user to modify just one data member
//Mon May  1 15:53:24 EDT 2000
// 5) return error conditions in ISTAT
// error codes:
// -1 Failed to connect to database
// -2 Failed to query
// -3 wrong size of query
// -4 Requested structure+dataset pair do not exist in database
// -5 query OK but, no result
// -6 special Case D1,D2 (unimplemented yet)
// -7 dbuse called with wrong username (must beging with 'r')
// >0 number of variables not filled (because of evolution)
//
// USAGE: from stardev staf: gex dbusesl.csl
// then: make geometry (with AgDGETP version calling DBUSE)
// then: dcut cave x 0 10 10 0.03 0.03
// and: next; dcut cave z 0 10 10 0.3 0.3
//

//standard:               // names used in calling routine AgDGETP:
//       CALL DBUSE('CAVEGEO','*',-999,'CAVE',NCAVE_VERSION,                 21
//      *RE_CAVE_VERSION,BPATH_CAVE,IDNUM_CAVE,1,MAP_CAVE,NAMES_CAVE,        21
//      *BANK_CAVE,LEN_CAVE,LLEN_CAVE,LINK_CAVE,FLAG_CAVE)                   21
//*****************************************************************************//
extern "C" void dbuse_(
		       char * pDIRECTORY,//'USE_TPCDIMENSIONS',
		       char * pBANK_TITLE, //'*'
		       int  * ISTAT,//-999
		       char * pSTRUCT_NAME,//'CAVE'
		       char * pNVERSION,//NCAVE_VERSION*12/'R.VERSION'/
		       float * pVERSION,//REAL RE_CAVE_VERSION
		       char* pBPATH,// BPATH_CAVE
		       int* pIDNUM,//IDNUM_CAVE
		       int* pI1,//1,
		       int* pMAP,//MAP_CAVE
		       char* pNAME, // NAMES_CAVE
		       float* pBANK, //BANK_CAVE
		       int* pLEN,//LEN_
		       int* pLLEN,//LLEN_
		       int* pLINK,//LINK_
		       int* pFLAG,//FLAG_
		       int lDIRECTORY,
		       int lBANK_TITLE,
		       int lSTRUCT_NAME,
		       int lNVERSION,
		       int lBPATH,
		       int lNAME)
{

if (gcflag_>1) cout << "DBUSE: entering" << endl;

 if (!DBAS.user || DBAS.user[0]!='r') { *ISTAT=-7; return;}

  //AV, 16.06.01 to suppress compile warning use
 void *myDummyP = pIDNUM;
 myDummyP= pI1;
 myDummyP= pLINK;
 myDummyP= pFLAG;

//       cout << "lDIRECTORY " << lDIRECTORY << endl;
//       cout << "lBANK_TITLE " << lBANK_TITLE << endl;
//       cout << "lSTRUCT_NAME " <<lSTRUCT_NAME  << endl;
//       cout << "lNVERSION" <<lNVERSION  << endl;
//       cout << "lBPATH " << lBPATH << endl;
//       cout << "lNAME " << lNAME << endl;
// lDIRECTORY 7
// lBANK_TITLE 1
// lSTRUCT_NAME 4
// lNVERSION12
// lBPATH 60
// lNAME 12
// DIRECTORY CAVEGEO
// BANK_TITLE *
// STRUCT_NAME CAVE
// NVERSION R.VERSION
// BPATH CAVE*
// var DIRECTORY "CAVEGEO"
// var BANK_TITLE "*"
// var STRUCT_NAME "CAVE"


//MYSQL mysql;
MYSQL_RES *result;
MYSQL_ROW row;

unsigned int num_fields;
unsigned int num_rows;
int num_struct;

#if defined(__GNUC__) &&  (__GNUC__ >= 3)
  ostringstream Query;
  Query << "neccessary dummy initialization - can't call seekp(0) on an empty sstream object "<<ends;
#else //.h for gcc-2 and SunOS
  const int MAXBUF=1024;
  char buf[MAXBUF];
  ostrstream Query(buf,MAXBUF);
#endif

const int MAXBUFF=1024;
char temp[MAXBUFF];

// save structure info
//DIRECTORY cannot be new

char DIRECTORY[256];
strncpy(DIRECTORY,pDIRECTORY,lDIRECTORY); DIRECTORY[lDIRECTORY]='\0';
if (gcflag_>2) cout << "DIRECTORY " << DIRECTORY << endl;

char* BANK_TITLE = new char[lBANK_TITLE+1];
strncpy(BANK_TITLE,pBANK_TITLE,lBANK_TITLE);BANK_TITLE[lBANK_TITLE]='\0';
if (gcflag_>3) cout << "BANK_TITLE " << BANK_TITLE << endl;

char* STRUCT_NAME = new char[lSTRUCT_NAME+1];
strncpy(STRUCT_NAME,pSTRUCT_NAME,lSTRUCT_NAME);STRUCT_NAME[lSTRUCT_NAME]='\0';
if (gcflag_>2) cout << "STRUCT_NAME " <<STRUCT_NAME  << endl;

char* NVERSION = new char[lNVERSION+1];
strncpy(NVERSION,pNVERSION,lNVERSION);NVERSION[lNVERSION]='\0';
if (gcflag_>3) cout << "NVERSION " <<NVERSION  << endl;

char* BPATH = new char[lBPATH+1];
strncpy(BPATH,pBPATH,lBPATH); BPATH[lBPATH]='\0';
 if (gcflag_>2) cout << "BPATH " << BPATH << endl;

int i;
int j;
//remove trailing spaces

j=lDIRECTORY; while (j != 0  && DIRECTORY[j-1] == ' ') j--; DIRECTORY[j]='\0';
if (gcflag_>3) cout << "var DIRECTORY \"" << DIRECTORY <<"\""<< endl;

j=lBANK_TITLE; while (j != 0  && BANK_TITLE[j-1] == ' ') j--;
BANK_TITLE[j]='\0';
if (gcflag_>3) cout << "var BANK_TITLE \"" << BANK_TITLE <<"\""<< endl;

j=lSTRUCT_NAME; while (j != 0  && STRUCT_NAME[j-1] == ' ') j--;
STRUCT_NAME[j]='\0';
if (gcflag_>3) cout << "var STRUCT_NAME \"" << STRUCT_NAME <<"\""<< endl;

j=lBPATH; while (j != 0  && BPATH[j-1] == ' ') j--; BPATH[j]='\0';
if (gcflag_>3)  cout << "var BPATH \"" << BPATH <<"\""<< endl;

    //resolve ATLAS calling sequence
static char currentDirectory[8]={'\0'};
static char callingDirectory[8]={'\0'};
static char firstModule[5]={'\0'};

char dir[8];
char mod[5];
//char * pSearch;
//very first call
if (!strcmp(currentDirectory,""))
  {
    strcpy(currentDirectory,DIRECTORY);
    strcpy(callingDirectory,DIRECTORY);
    strcpy(firstModule,BANK_TITLE);
    if (gcflag_>3) cout << "DBUSE: first call: Directory \""
			<< currentDirectory <<"\""<< endl;
  }

if (strcmp(callingDirectory,DIRECTORY))//new calling directory
  {
    strcpy(callingDirectory,DIRECTORY);
    strcpy(currentDirectory,DIRECTORY);
    strcpy(firstModule,BANK_TITLE);
    if (gcflag_>3)  cout << "new calling DIRECTORY \""
			 << currentDirectory <<"\""<< endl;
  }

if (strcmp(firstModule,BANK_TITLE))//cd to calling directory
  {
    strcpy(currentDirectory,callingDirectory);
    if (gcflag_>3) cout << "firstModule: cd to calling DIRECTORY \""
			<< currentDirectory <<"\""<< endl;
  }

if (BPATH[0]=='/')//cd to new directory
  {
      cout << "BPATH \"" << BPATH <<"\""<< endl;

      //find start of module name
      char * pSearch=strstr(BPATH,"DETM/");
      strncpy(dir,pSearch+5,4); dir[4]='\0';
    if (gcflag_>3) cout << "dir \"" << dir <<"\", ";

      //find name of the first structure in the module
      pSearch=strchr(pSearch+5,'/');
      strncpy(mod,pSearch+1,4); mod[4]='\0';
      if (gcflag_>3) cout << "1st mod \"" << mod <<"\""<< endl;

      switch (mod[3])
	{
	case 'D':
	  strcat (dir,"DIG");
	  break;
	case 'G': //in ATLAS there are many choices
	  if (mod[1]=='D')
            strcat (dir,"DIG");
          else
	    strcat (dir,"GEO");
	    break;
	default:
	  strcat (dir,"GEO");
	  break;
	}
        strcpy(currentDirectory,dir);
        if (gcflag_>3) cout << "changed currentDIRECTORY to \""
			    << currentDirectory <<"\""<< endl;
  }
// else
//   {
//if (strncmp(currentDirectory,DIRECTORY,4))//if !=, cd to the new directory
//     if (strcmp(currentDirectory,DIRECTORY))//if !=, cd to the new directory
//       {
// 	strcpy(currentDirectory,DIRECTORY);
//         cout << "changed currentDIRECTORY to \""
//              << currentDirectory <<"\""<< endl;
//       }
//   }

//always ignore the DIRECTORY where the call is made from
strcpy(DIRECTORY,currentDirectory);

if (gcflag_>2) cout << "using DIRECTORY \"" << DIRECTORY <<"\""<< endl;

// int IDNUM;
// int I1;
int LEN;
int LLEN;

if (gcflag_>1) cout << STRUCT_NAME << endl;

char VTYPE[2+1];
char* SELECTOR;

//if (!strcmp(STRUCT_NAME,"TPRS")) return ;//SEC?
//if (strcmp(STRUCT_NAME,"TECW")) return ;//SEC?
 // if ( ((int)*pVERSION) == 1) return ;//SEC?

if (lNVERSION==1)//no selector
  {
    SELECTOR = new char;
    SELECTOR[0] = '\0';
    VTYPE[0]='\0';
  }
else
  {
    SELECTOR = new char[lNVERSION-2+1];

    strncpy(VTYPE,pNVERSION,2);VTYPE[2]='\0';
    if (gcflag_>3) cout << "VTYPE " << VTYPE << endl;

    strncpy(SELECTOR,pNVERSION+2,lNVERSION-2);SELECTOR[lNVERSION-2]='\0';
    if (gcflag_>3) cout << "selector  NAME \"" << SELECTOR <<"\""<< endl;
  }

//remove trailing spaces
j=lNVERSION-2;
while (j != 0  && SELECTOR[j-1] == ' ') j--;
SELECTOR[j]='\0';
if (gcflag_>3) cout << "selector  NAME \"" << SELECTOR <<"\""<< endl;

//IDNUM = *pIDNUM;
//cout << "IDNUM: " << IDNUM << endl;

//I1 = *pI1; //reserved for requests
//cout << "I1: " << I1 << endl;

LEN = *pLEN; 
if (gcflag_>3) cout << "LEN: " << LEN << endl;

LLEN = *pLLEN; 
if (gcflag_>3) cout << "LLEN: " << LLEN << endl;

//save parameters info

char TYPE[2+1]="";
char* NAME = new char[lNAME-2+1];
const char * Type="float";
float * pCURRENT;

const int Lvalue=16;
char VALUE[Lvalue+1]="";

int outLEN;
outLEN=LEN-2;//exclude two first system words in the BANK

char **Names = new char*[outLEN];
char **Types = new char*[outLEN];
char **Values = new char*[outLEN];
char *Found = new char[outLEN];

int *d1= new int[outLEN];
int *d2= new int[outLEN];

int count=0;
int offset=0;
int outIndex;
outIndex=0;

int M = 1000000;

for (i=0; i<LLEN; i++)
  {
    strncpy(TYPE,pNAME+i*lNAME,2);TYPE[2]='\0';
    if (gcflag_>3) cout << "TYPE " << TYPE << endl;

    strncpy(NAME,pNAME+i*lNAME+2,lNAME-2);NAME[lNAME-2]='\0';
    if (gcflag_>3) cout << "var NAME " << NAME << endl;

    //remove trailing spaces
    j=lNAME-2;
    while (j != 0  && NAME[j-1] == ' ') j--;
    NAME[j]='\0';
    if (gcflag_>3) cout << "var NAME \"" << NAME <<"\""<< endl;

    //get dimensions for array

     int D1=*(pMAP+2*i);
     int D2=*(pMAP+2*i+1);

     if (gcflag_>3) cout <<"DBUSE: D1,D2= "<< D1 <<", "<< D2<<endl;

    if (D1<0 || D2<0)
      {
	if (D1<0)
           {
             D1=-D1;
             if (D1<M)
               D1= *( (int*)pBANK + (D1-1) );
             else
               D1= (int) *( pBANK + (D1-1 - M) );
           }
         if (D2<0)
           {
             D2=-D2;
             if (D2<M)
               D2= *( (int*)pBANK + (D2-1) );
             else
               D2= (int) *( pBANK + (D2-1 - M) );
           }
         cerr <<"DBUSE: special Case read D1,D2= "<< D1 <<", "<< D2<<endl;
	 *ISTAT=-6;
	 //	 return;
	 D1=1;
         D2=1;
    }

    d1[i]=D1;
    d2[i]=D2;

    //determine parameter Type as filled in database

    switch(TYPE[0])
      {
      case 'R':
	Type="float";
	break;
      case 'I':
	Type="int";
	break;
      case 'H':
	Type="text";
	break;
      default:
	cerr << "DBUSE ERROR: unsupported type: "<<TYPE[0]<<endl;
	break;
      }

    //save parameters info

    int headerDone;
    headerDone=0;

    for (int k=0;k<D1;k++){
      for (int l=0;l<D2;l++){
	count++;

	//this is strange!? why read the pBANK in dbuse

	pCURRENT=pBANK+i+2+k+l+offset;
	switch(TYPE[0])
	  {
	  case 'R':
	    if (gcflag_>3) cout <<"(" << k+1 <<","<< l+1 << ") = " << *pCURRENT<<endl;
	    sprintf(VALUE, "%.7g", *pCURRENT );
	    break;
	  case 'I':
	    cout<<"(" <<k+1<<","<<l+1<<") = "<< *((int*) pCURRENT)<<endl;
	    if (gcflag_>3) sprintf(VALUE, "%d", *((int*) pCURRENT) );
	    break;
	  case 'H':
	    //where to get this 4 from?
	    strncpy(VALUE,(char*)pCURRENT,4);
	    if (gcflag_>3) cout <<"(" << k+1 <<","<< l+1 << ") = " << VALUE <<endl;
	    break;
	  default:
	    cerr << "DBUSE ERROR: unsupported type: "<< TYPE[0] <<endl;
	    break;
	  }

	if (D1*D2==1)//not an array
	  {
	    Names[outIndex]=strdup(NAME);
	  }
	else//for arrays add []
	  {
	    sprintf(temp, "%s[%d]", NAME, k+l);
	    Names[outIndex]=strdup(temp);
	  }
	Types[outIndex]=strdup(Type);
	Values[outIndex]=strdup(VALUE);
	outIndex++;
      }
    }
    if (D1*D2>1)
      offset+=D1*D2-1;
  } //end of parameters loop

// cout << "trying to fetch: "<<endl;
//  for (i=0;i<outLEN;i++)
//    cout << "   "<< Names[i] << "="<<Values[i]<<" "<<endl;

// Initialize a connection handler

//mysql_init(&mysql);

// Establish a connection to the MySQL database engine
//cout<<"In dbuse_: \""<<DBAS.server << "\", \""
//    <<DBAS.name << "\", \"" <<DBAS.user << "\", \""
//    <<DBAS.passwd<<"\"" <<endl;
// if (!mysql_real_connect(&mysql,DBAS.server,
//                         DBAS.user,DBAS.passwd,DBAS.name,0,NULL,0))
 if (mysql_ping(&mysql))
   {
     cerr << "DBUSE: Failed to reconnect to database: "
 	 <<  mysql_error(&mysql) << endl;
     *ISTAT=-1;
     return;
   }

//check if the directory+structure pair already exist
int *existingDirID=NULL;
int *existingDirInstance=NULL;
int *existingStrID=NULL;
int *existingStrInstance=NULL;

Query.seekp(0);
// DISTINCT is used for the no selector case
Query << "SELECT DISTINCT structure.ID, structure.instance,"
      << " directory.ID, directory.instance"
      << " FROM structure as directory"
      << " LEFT JOIN structure ON directory.ID=structure.parent"
      << " LEFT JOIN relation ON structure.ID=relation.strID"
      << " LEFT JOIN parameter as version ON version.ID=relation.parID"
      << " WHERE structure.name=\"" << STRUCT_NAME << "\"";

if (!strcmp(STRUCT_NAME,"PEDI"))
  {
	Query << " AND directory.name=\"" << DIRECTORY << "\"";
  }

if (lNVERSION==1) //no selector
  {
	Query << ends;
  }
else //add selector
  {
	Query << " AND version.name=\"" << SELECTOR << "\"";

	//determine selector Type
	char NAMED[5];
	
	switch(VTYPE[0])
	  {
	  case 'R':
	    Query << " AND version.value=\"" << *pVERSION << "\"" << ends;
	    break;
	  case 'I':
	    Query << " AND version.value=\""
		  << *( (int*) pVERSION )<< "\"" << ends;
	    break;
	  case 'H':
	    strncpy(NAMED, (char*)pVERSION, 4);
	    NAMED[4]='\0';
	    Query << " AND version.value=\"" << NAMED << "\"" << ends;
	    break;
	  default:
	    cerr << "DBUSE: unknown VERSION SELECTOR type!"<<endl;
	    break;
	  }
  }

if (gcflag_>3) cout << "DBUSE: database query: " << queryStr(&Query) << endl;

if (my_query(&Query))
  {
    cerr << "DBUSE: Failed to query: " <<  mysql_error(&mysql) << endl;
    cerr << "database query: " << queryStr(&Query) << endl;
//      mysql_close(&mysql);
     *ISTAT=-2;
     return;
  }
else // query succeeded, get result
  {
    result = mysql_store_result(&mysql);
    if (result)
      {
	num_fields = mysql_num_fields(result);
	if (num_fields!=4) {//this should never happen
	  cerr << "DBUSE: Wrong size of dir+str query"<<endl;
// 	  mysql_close(&mysql);
	  *ISTAT=-3;
	  return;
	}
	num_struct = mysql_num_rows(result);
	if (num_struct==0)// found nothing
	  {
	    cerr << "DBUSE: Requested structure+dataset pair "
		 << STRUCT_NAME<<"+"
		 << DIRECTORY<<" do not exist in database"<<endl;
	    cerr << "provided BPATH " << BPATH<<endl;
	    cerr << "database query: " << queryStr(&Query) << endl;
	    //do not close the connection in this case, or ping will not work
	    //	    mysql_close(&mysql);
	    *ISTAT=-4;
	    return;
	  }
	else   // this structure name already exists
	  {

if (gcflag_>3) cout<<"Found "<<num_struct<<" struct "<<STRUCT_NAME
		   <<" for module "<<DIRECTORY<<endl;

	    existingStrID = new int[num_struct];
	    existingStrInstance = new int[num_struct];
 	    existingDirID = new int[num_struct];
 	    existingDirInstance = new int[num_struct];
	    for (i=0;i<num_struct;i++)
	      {
		row = mysql_fetch_row(result);
		existingStrID[i]=atoi(row[0]);
		existingStrInstance[i]=atoi(row[1]);
		existingDirID[i]=atoi(row[2]);
		existingDirInstance[i]=atoi(row[3]);

// 		cout<< "There is "<< STRUCT_NAME<<" Instance "
// 		    << existingStrInstance[i] <<endl;

	      }
	  }
	mysql_free_result(result);
      }
    else   // query OK but, no result?!
      {
	cerr << "DBUSE: no result: " <<  mysql_error(&mysql) << endl;
// 	mysql_close(&mysql);
	*ISTAT=-5;
	return;
      }
  }

//this structure name already exists in the structure table
//fetch structure parameters (for comparison)

for (i=0;i<(int)num_struct;i++)
{
  //same variables counter
  int count=0;

  //cout << "i, num_struct, existingStrID[i]: " << i<<" "
  //     <<num_struct<<" "<<existingStrID[i] << endl;

  Query.seekp(0);
  //  Query << "SELECT parameter.type,IFNULL(array.name, parameter.name),
  Query << "SELECT IFNULL(array.name, parameter.name),"<<
" IFNULL(array.value, parameter.value), parameter.comment"<<
" FROM structure as directory"<<
" LEFT JOIN structure ON directory.ID=structure.parent"<<
" LEFT JOIN relation ON structure.ID=relation.strID"<<
" LEFT JOIN parameter ON parameter.ID=relation.parID"<<
" LEFT JOIN parameter as array ON parameter.ID=array.arrayID"<<
" WHERE structure.ID="<<existingStrID[i]
	<<" ORDER BY relation.position,array.ID"<<ends;

  //cout << "database query: " << queryStr(&Query) << endl;
  if (my_query(&Query))
    {
      cerr << "DBUSE: Failed to query: " <<  mysql_error(&mysql) << endl;
      cerr << "database query: " << queryStr(&Query) << endl;
//       mysql_close(&mysql);
      *ISTAT=-2;
      return;
    }
  else // query succeeded, get result
    {
      result = mysql_store_result(&mysql);
      if (result)// query OK
	{
	  num_fields = mysql_num_fields(result);
	  if (num_fields!=3) {
	    cerr << "DBUSE: Wrong size of parID query"<<endl;
// 	    mysql_close(&mysql);
	    *ISTAT=-3;
	    return;
	  }
	  num_rows = mysql_num_rows(result);

//        cout<<"total variables: "<< num_rows <<endl;
	  if (num_rows!=(unsigned int)outLEN) //size is different from this ID
	    {
//              cout<<"DBUSE: struct "<<STRUCT_NAME
//                  <<" for module "<<DIRECTORY<<endl;
//	      cout<<"DBUSE: database structure "<< i+1 <<" is of "<< num_rows
//		  <<" rows, user structure is "<< outLEN <<endl;
// AV Mon May  1 13:01:28 EDT
// implement schema evolution
	      memset(Found,'n',outLEN);
	      //find matches in the database
	      for (unsigned int jDB=0;jDB<num_rows;jDB++)
		{
		  row = mysql_fetch_row(result);
// 		  for (unsigned int jUser=0;jUser<outLEN;jUser++)
		  for (int jUser=0;jUser<outLEN;jUser++)
		    {
		      //to check names array dim has to be taken into account
		      if (!strcmp(Names[jUser],row[0]))
			{
			  Values[jUser]=strdup(row[1]);
// 			  cout<<"DBUSE: \""<<Names[jUser]
//  			      <<"\" = \"" <<Values[jUser]<<"\""<<endl;
			  count++;
			  Found[jUser]='y';
			}
		    }
		}	
 	      if (count<outLEN)
 		{
 		  cout<<"WARNING! "<<outLEN-count
		      <<" variables in "<< STRUCT_NAME
		      <<" from " << DIRECTORY
		      <<" are not found by DBUSE: "<<endl;
		  for (int jUser=0;jUser<outLEN;jUser++)
		    {
		      if (Found[jUser]=='n')
			{
 			  cout<<Names[jUser]<<" = "<<Values[jUser]<<endl;
			}	
		    }
 		}
	      else
		{
 		  cout<<"DBUSE : stripped "<<count-outLEN
		      <<" values from DB Structure "<< STRUCT_NAME
 		      <<" in the module " << DIRECTORY<<endl;
		}
	    }

	  else //same size size: check names
	    {
	      for (unsigned int j=0;j<num_rows;j++)
		{
		  row = mysql_fetch_row(result);
                  //to check names array dim has to be taken into account
 		  if (strcmp(Names[j],row[0]))//this variable is different
                  cout<<"DBUSE: Names dont match \""<<Names[j]<<"\" \""
		      <<row[0]<<"\""<<endl;
		  //break;
		
		  //same name: fetch value
//       		  strcpy(Values[j],row[1]);
 		  Values[j]=strdup(row[1]);
                 //cout<<"db Values: = \"" <<  Values[j] << "\"" <<endl;

		  //cout<<"variable "<< j+1 <<" is the same"<<endl;
		  count++;
		}
	
	      if (count==outLEN)
		{
		  if (gcflag_>3) cout<<"Structure "<< STRUCT_NAME
				     <<" is found with number " << existingStrInstance[i]
				     <<" in the module " << DIRECTORY
				     <<" with version " << existingDirInstance[i]<<endl;
		}
	    }//end if equal size structures
	  mysql_free_result(result);
	}
      else   // something wrong
	{
	  cerr << "DBUSE: no result: " <<  mysql_error(&mysql) << endl;
// 	  mysql_close(&mysql);
	  *ISTAT=-5;
	  return;
	}
    }

}//end of loop over all existing structure IDs


for (i=0; i<outLEN; i++)
  {
	pCURRENT=pBANK+2+i;

	switch(Types[i][0])
	  {
	  case 'f':
	    *pCURRENT=atof(Values[i]);
	    if (gcflag_>3) cout <<Names[i]<<" = " << *pCURRENT<<endl;
	    break;
	  case 'i':
	  case 't':
	    *((int*) pCURRENT)=atoi(Values[i]);
	    if (gcflag_>3) cout<<Names[i]<<" = "<< *((int*) pCURRENT)<<endl;
	    break;
	  default:
	    cerr << "DBUSE ERROR: unknown type: "<< Types[i][0] <<endl;
	    break;
	  }
  } //end of parameters loop

// We are done with the connection, call mysql_close() to terminate it

//mysql_close(&mysql);

delete [] BANK_TITLE;
delete [] STRUCT_NAME;
delete [] NVERSION;
delete [] BPATH;
delete [] SELECTOR;
delete [] NAME;

for (i=0;i<outLEN;i++)
{	
  free((void*)Names[i]);
  free((void*)Types[i]);
  free((void*)Values[i]);
}

delete [] Names;
delete [] Types;
delete [] Values;
delete [] Found;
delete [] d1;
delete [] d2;

if (existingDirID       != NULL) delete [] existingDirID;
if (existingDirInstance != NULL) delete [] existingDirInstance;
if (existingStrID       != NULL) delete [] existingStrID;
if (existingStrInstance != NULL) delete [] existingStrInstance;

//return OK status
*ISTAT=0;

//count not found variables
for (char *p=Found;p<Found+outLEN;p++) {
  if (*p=='n') (*ISTAT)++;
}

return;

}

#else
/*               dummy MySQL interface                 */
#        include <stdio.h>
#        include <stdlib.h>
#        include <assert.h>
extern "C" void  dbset_  (){assert(0);};
extern "C" void  dbget_  (){assert(0);};
extern "C" void  dbuse_  (){assert(0);};
extern "C" void  dbfill_ (){assert(0);};
extern "C" void  adbfill_(){assert(0); printf(" in empty adbfill \n"); }
extern "C" void  dbls_   (){assert(0);};
extern "C" void  dbprint_(){assert(0);};

#endif
#endif//if 0
