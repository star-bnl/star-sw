/***************************************************************************
 * 
 * $Id: StHbook.hh,v 1.1 1999/01/30 03:59:02 fisyak Exp $
 *
 * Author:  Thomas Ullrich, Yale University
 *          adapted for SCL by bl July 1998
 ***************************************************************************
 *
 * Note that all initialization of HBOOK and ZEBRA is already
 * performed within the classes. No additional compile flags
 * are needed. Link packlib only. If the /pawc/ common is too
 * small to cope your requirements increase the value of the 
 * constant 'SizeOfPawCommon' to whatever you need and recompile.
 *
 * You may prevent the initialization of HBOOK by defining
 * the macro NO_HBOOK_INIT in case it is initialized in other
 * parts of your program.
 *
 * Member function of the various objects:
 *
 * StHbookHisto (1-dim histogram):
 *       StHbookHisto(const char* name, int nbins, float x1, float x2)
 *       StHbookHisto(int id, const char* name, int nbins, float x1, float x2)
 *       StHbookHisto(const StHbookHisto&)
 *       StHbookHisto& operator= (const StHbookHisto&)
 *       void  StHbookHisto::fill(float x, float weight = 1)
 *       void  StHbookHisto::fastFill(float x, float weight = 1)
 *       int   StHbookHisto::id()
 *       int   StHbookHisto::entries()
 *       float StHbookHisto::max()
 *       float StHbookHisto::min()
 *       float StHbookHisto::sum()
 *       void  StHbookHisto::setOpt(const char*)
 *       void  StHbookHisto::print()
 *       void  StHbookHisto::reset()
 *       float StHbookHisto::mean()
 *       float StHbookHisto::sigma()
 *
 * StHbookHisto2 (2-dim histogram):
 *       StHbookHisto2(const char* name, int nxbins, float x1, float x2,
 *                                      int nybins, float y1, float y2)
 *       StHbookHisto2(int id, const char* name, int nxbins, float x1, float x2
 *                                              int nybins, float y1, float y2)
 *       StHbookHisto2(const StHbookHisto2&)
 *       StHbookHisto2& operator= (const StHbookHisto2&)
 *       void  StHbookHisto2::fill(float x, float weight = 1)
 *       void  StHbookHisto2::fastFill(float x, float weight = 1)
 *       int   StHbookHisto2::id()
 *       int   StHbookHisto2::entries()
 *       float StHbookHisto2::max()
 *       float StHbookHisto2::min()
 *       float StHbookHisto2::sum()
 *       void  StHbookHisto2::setOpt(const char*)
 *       void  StHbookHisto2::print()
 *       void  StHbookHisto2::reset()
 *
 * StHbookTuple (RWN tuples):
 *       StHbookTuple(int id, const char* name, int ntags)
 *       StHbookTuple(const char* name, int ntags)
 *       StHbookTuple& StHbookTuple::setTag(const char* tag)
 *       void   StHbookTuple::book()
 *       int    StHbookTuple::id()
 *       int    StHbookTuple::length()
 *       int    StHbookTuple::entries()
 *       void   StHbookTuple::fill(float *vec)
 *       StBool StHbookTuple::getEvent(int eventNumber, float *vec)
 *
 * StHbookFile (ZEBRA RZ file):
 *       StHbookFile(const char* filename, int recordSize = 1024, int lun = 10)
 *       int isGood() const
 *       void StHbookFile::list(const char* = 0) // see hldir for possible options
 *       void StHbookFile::saveAndClose()
 *
 * Note that recordSize can take values between 1024 and 8192 only.
 *
 ***************************************************************************
 *
 * $Log: StHbook.hh,v $
 * Revision 1.1  1999/01/30 03:59:02  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:27:49  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_HBOOK_HH
#define ST_HBOOK_HH

#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>


const int SizeOfPawCommon     = 200000;
const int MaxValueForIQuest10 = 64000;    // max num. of records in hbook file (16 bit)
const int MaxTagLength        = 8;        // HBOOK limitation

//
//  PAWC common block (external linkage)
//
extern long pawc_[SizeOfPawCommon];      
extern long quest_[100];                  // /QUEST/ has fix size

//
//  Prototypes for F77 HBOOK routines
//
extern "C" {
    void  hlimit_(int*);
    void  hbook1_(int*, char*, int*, float*, float*, float*, int);
    void  hbook2_(int*, char*, int*, float*, float*, int*, float*, float*, float*, int);
    void  hfill_(int*, float*, float*, float*);
    void  hf1_(int*, float*, float*);
    void  hf2_(int*, float*, float*, float*);
    void  hdelet_(int*);
    void  hropen_(int*, char*, char*, char*, int*, int*, int, int, int);
    void  hcdir_(char*, char*, int, int);
    void  hldir_(char*, char*, int, int);
    void  hmdir_(char*, char*, int, int);
    void  hrout_(int*, int*, char*, int);
    void  hrend_(char*, int);
    void  hbookn_(int*, char*, int*, char*, int*, char*, int, int, int);
    void  hfn_(int*, float*);
    void  hgnf_(int*, int*, float*, int*);
    void  hnoent_(int*, int*);
    void  hprint_(int*);
    void  hreset_(int*, char*, int);
    void  hcopy_(int*, int*, char*, int);
    void  hindex_();
    int   hexist_(int*);
    float hmax_(int*);
    float hmin_(int*);
    float hsum_(int*);
    float hstati_(int*, int*, char*, int*, int);
    float hidopt_(int*, char*, int);
}

//=====================================================================
//   StHObject:  Implicit 'virtual' base class.
//
//   Although instances can be created they are of no use.
//   The only purpose is to ensure that HBOOK is initialized
//   whenever a derived class is used.
//=====================================================================
class StHObject {
public:
    StHObject();
    
private:
    static bool mHbookInitialized;
};


//=====================================================================
// StHbook:  Base class common to all histograms and tuple.
//=====================================================================
class StHbook : public StHObject {
public:
    StHbook();
    virtual ~StHbook();
    int id() const { return mId; }
    int entries();
    
protected:
    bool isBooked() const { return mBooked; }
    int  nextId(int);
    
protected:
    int   mId;
    bool  mBooked;
    char* mTitle;
};


//=====================================================================
// StHbookHistogram:  Base class common to all histograms (1-2 dim)
//=====================================================================
class StHbookHistogram : public StHbook {
public:
    StHbookHistogram();
    StHbookHistogram(const StHbookHistogram&);
    StHbookHistogram& operator= (const StHbookHistogram&);
    virtual ~StHbookHistogram();
    
public:
    float max();
    float min();
    float sum();
    void  setOpt(const char*);
    void  print();               // ascii print out
    void  reset();               // reset contents
};


//=====================================================================
// StHbookHisto:  One-dimensional histogram class
//
//  Create/book and fill HbOOK histograms.
//  The usual id may be defined by the user by passing the id
//  as first argument. However, if the specified id already exists
//  the next higher free id is used. The object will find its own
//  id if non is specified.
//  The destructor ~StHbookHisto() deletes the histogram from the
//  (ZEBRA) memory by invoking HDELET.
//  See class StHbookFile for how to store histograms on file.
//
//  Example:
//
//  StHbookHisto  ptHisto("pt-dist", 100, 0., 2.);
//  ....
//  ptHisto.fill(pt);
//  cout << ptHisto.mean() << endl;
//  cout << ptHisto.entries() << endl;
//
//  Note that fill() uses the HbOOK option 'STAT', in order to
//  calculate the mean and sigma from the passed values directly.
//  This feature is ignored by fastFill(). In this case mean and
//  sigma are derived from the binned data.
//  (See also HF1, HFILL in the HbOOK manual).
//=====================================================================
class StHbookHisto : public StHbookHistogram {
public:
    StHbookHisto(const char* name, int nbins, float x1, float x2);
    StHbookHisto(int id, const char* name, int nbins, float x1, float x2);
    void fill(float x, float weight = 1);
    void fastFill(float x, float weight = 1);
    float mean();
    float sigma();

private:
    StHbookHisto();
    void _init(const char*, int, float, float);
};


//=====================================================================
// StHbookHisto2:  Two-dimensional histogram class
//
//  Example:
//
//  StHbookHisto2  xyHisto("xy plane", 100, 0., 2., 200, 0., 3.);
//  ....
//  xyHisto.fill(x, y);
//  cout << xyHisto.sum() << endl;
//
//  See StHbookHisto for more info.
//=====================================================================
class StHbookHisto2 : public StHbookHistogram {
public:
    StHbookHisto2(const char* name, int nxbins, float x1, float x2,
		  int nybins, float y1, float y2);
    StHbookHisto2(int id, const char* name, int nxbins, float x1, float x2,
		  int nybins, float y1, float y2);
    void fill(float x, float y, float weight = 1);
    void fastFill(float x, float y, float weight = 1);
    
private:
    StHbookHisto2();
    void _init(const char*, int, float, float, int, float, float);
};


//=====================================================================
// StHbookTuple:  RWN tuple class.
//
//  A StHbookFile must be instantiated in advance in order
//  to store the tuple on file. (See class StHbookFile below).
//  The usual id may be defined by the user by passing the id
//  as first argument. However, if the specified id already exists
//  the next higher free id is used. The object will find its own
//  id if non is specified.
//  The tuple is defined in 3 steps.
//  - the constructor only defines the # of tags, the title and
//    (optional) the id
//  - the individual tags are defined via the setTag() method
//  - method book() finally books the referring tuple.
//
//  Setting tags:
//  The order is important. Tags not defined by setTag() are stored
//  as "unknown". Max tag lenght is 8 character.
//
//  Example:
//
//  StHbookTuple mytuple("foo",4);
//  mytuple.setTag("x").setTag("y").setTag("z").book()
//  defines at tuple with the following tags
//  1    x
//  2    y
//  3    z
//  4    unknown
//  float vec[4];
//  ...
//  mytuple.fill(vec);                // fill the referring tuple
//
//  Instead of using the setTag() and book() member one may also use
//  the 'put to' operator << in the following way:
//  mytuple << "x" << "y" << "z" << book;
//=====================================================================
class StHbookTuple : public StHbook {
public:
    StHbookTuple(int id, const char* name, int ntags);
    StHbookTuple(const char* name, int ntags);
    ~StHbookTuple();
    
public:
    StHbookTuple& setTag(const char*);
    void          book();
    void          fill(float*);
    int           getEvent(int, float*);
    StHbookTuple& operator<< (const char*);
    void          operator<< (void (*pf)(StHbookTuple&));
    int           length() const { return mNTags; }
    
private:
    void _init(const char*);
    StHbookTuple();
    StHbookTuple(const StHbookTuple&);
    StHbookTuple& operator= (const StHbookTuple&);
    
private:
    int   mNTags;
    char* mTags;
    int   mITag;
};
void book(StHbookTuple&);


//=====================================================================
// StHbookFile:  Class represents a HBOOK RZ file
//
//  After creating one instance all ntuples are automatically 
//  written to file. Histograms (and the residual tuples still 
//  in memory are dumped to file by invoking the saveAndClose()
//  method. Note that a StHbookFile must be defined prior to 
//  objects of type StHbookTuple.
//=====================================================================
class StHbookFile : public StHObject {
public:
    StHbookFile(const char*, int = 1024, int = 10);
    ~StHbookFile();
    void saveAndClose();
    void list(const char* = 0);
    int  isGood() const;
    
private:
    StHbookFile();
    StHbookFile(const StHbookFile&);
    StHbookFile& operator= (const StHbookFile&);
    
private:
    int  mRc;
    char *mMode;
    char *mFilename;
};
#endif
