/***************************************************************************
 *
 * $Id: StHbook.cc,v 1.1 1999/01/30 03:59:02 fisyak Exp $
 *
 * Author: Thomas Ullrich, Yale University
 ***************************************************************************
 *
 * Description:
 * C++ HBOOK wrapper classes.
 * Adapted from CERES/NA45 COOL library, and from CHbook.h by brian
 *
 ***************************************************************************
 *
 * $Log: StHbook.cc,v $
 * Revision 1.1  1999/01/30 03:59:02  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/30 03:59:02  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:29:13  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include "StGlobals.hh"
#include "StHbook.hh"
#ifdef __SUNPRO_CC
#include <strings.h>
#endif

long pawc_[SizeOfPawCommon];
long quest_[100];

#ifdef NO_HBOOK_INIT
bool StHObject::mHbookInitialized = true;
#else
bool StHObject::mHbookInitialized = false;
#endif

StHObject::StHObject()
{
    if (!mHbookInitialized) {
	int size = SizeOfPawCommon;
	hlimit_(&size);
	mHbookInitialized = true;
    }
}

StHbook::StHbook()
{
    mId     = 0;
    mBooked = false;
    mTitle  = 0;
}

StHbook::~StHbook()
{
    delete [] mTitle;
}

StInt StHbook::nextId(int wish)
{
    while(hexist_(&wish)) wish++;
    return wish;
}

StInt StHbook::entries()
{
    int n = 0;
    if (mBooked) hnoent_(&mId, &n);
    return n;
}

StHbookHistogram::StHbookHistogram() {/* nop */}

StHbookHistogram::StHbookHistogram(const StHbookHistogram& old)
{
    mId = nextId(old.mId);
    mTitle = new char[strlen(old.mTitle)+1];
    strcpy(mTitle, old.mTitle);
    hcopy_((int*)&old.mId, &mId, mTitle, strlen(mTitle));
    mBooked = true;
}

StHbookHistogram& StHbookHistogram::operator= (const StHbookHistogram& from)
{
    if (this == &from) {                    // same class object
	return *this;
    }
    else {
	delete [] mTitle;                   // free old store
	hdelet_(&mId);
	strcpy(mTitle, from.mTitle);
	hcopy_((int*)&from.mId, &mId, mTitle, strlen(mTitle));
	mBooked = true;
	return *this;
    }
}

StHbookHistogram::~StHbookHistogram() { hdelet_(&mId); }

float StHbookHistogram::max() { return hmax_(&mId); }

float StHbookHistogram::min() { return hmin_(&mId); }

float StHbookHistogram::sum() { return hsum_(&mId); }

void StHbookHistogram::setOpt(const char *chopt)
{
    hidopt_(&mId, (char*)chopt, strlen(chopt));
}

void StHbookHistogram::reset()
{
    hreset_(&mId, mTitle, strlen(mTitle));
}

void StHbookHistogram::print()
{
    hprint_(&mId);
}

StHbookHisto::StHbookHisto(int id, const char* text, int nbins, float x1, float x2)
{
    mId = nextId(id);
    _init(text, nbins, x1, x2);
}

StHbookHisto::StHbookHisto() {}

StHbookHisto::StHbookHisto(const char* text, int nbins, float x1, float x2)
{
    mId = nextId(1);
    _init(text, nbins, x1, x2);
}

void StHbookHisto::_init(const char* text, int nbins, float x1, float x2)
{
    mTitle = new char[strlen(text)+1];
    strcpy(mTitle, text);
    float words = 0;
    hbook1_(&mId, mTitle, &nbins, &x1, &x2, &words, strlen(mTitle));
    setOpt("STAT");
    mBooked = true;
}

float StHbookHisto::mean()
{
    int icase = 1;
    int num = 0;
    char choice = 0;
    return hstati_(&mId, &icase, &choice, &num, 0);
}

float StHbookHisto::sigma()
{
    int icase = 2;
    int num = 0;
    char choice = 0;
    return hstati_(&mId, &icase, &choice, &num, 0);
}

StHbookHisto2::StHbookHisto2(int id, const char* text, int nxbins, float x1, float x2,
			     int nybins, float y1, float y2)
{
    mId = nextId(id);
    _init(text, nxbins, x1, x2, nybins, y1, y2);
}

StHbookHisto2::StHbookHisto2() {}

StHbookHisto2::StHbookHisto2(const char* text, int nxbins, float x1, float x2,
			     int nybins, float y1, float y2)
{
    mId = nextId(1);
    _init(text, nxbins, x1, x2, nybins, y1, y2);
}

void StHbookHisto2::_init(const char* text, int nxbins, float x1, float x2,
			  int nybins, float y1, float y2)
{
    mTitle = new char[strlen(text)+1];
    strcpy(mTitle, text);
    float words = 0;
    hbook2_(&mId, mTitle, &nxbins, &x1, &x2, &nybins, &y1, &y2, &words, strlen(mTitle));
    setOpt("STAT");
    mBooked = true;
}

StHbookFile::StHbookFile() {}

StHbookFile::~StHbookFile() {}

StHbookFile::StHbookFile(const StHbookFile&) {}

StHbookFile::StHbookFile(const char* file, int recordSize, int lun)
{
    char *context = new char[strlen("HISTOS")+1];
    strcpy(context, "HISTOS");
    quest_[9] = MaxValueForIQuest10;
    mFilename = new char[strlen(file)+1];
    strcpy(mFilename, file);
    mMode = new char[strlen("NQ")+1];
    strcpy(mMode, "NQ");
    hropen_(&lun, context, mFilename, mMode, &recordSize, &mRc,
	    strlen(context), strlen(mFilename), strlen(mMode));
    delete [] context;
}

StInt StHbookFile::isGood() const
{
    // 
    // mRc:       return code from hropen()
    // iquest[0]: zebra error flag
    //
    return (mRc == 0 && quest_[0] == 0);
}
    
StHbookFile& StHbookFile::operator=(const StHbookFile&)
{
    return *this;
}

void StHbookFile::list(const char* opt)
{
    char option[4];
    char *context = new char[strlen("//HISTOS")+1];
    strcpy(context, "//HISTOS");
    if (opt)
	strcpy(option, opt);
    else
	strcpy(option, " ");
    hldir_(context, option, strlen(context), strlen(option));
    delete [] context;
}

void StHbookFile::saveAndClose() {
    int icycle = 0;
    char *context = new char[strlen("//HISTOS")+1];
    strcpy(context, "//HISTOS");
    char blank[4];
    strcpy(blank, " ");
    hcdir_(context, blank, strlen(context), strlen(blank));
    int selectID = 0;
    hrout_(&selectID, &icycle, blank, strlen(blank));
    hrend_(context, strlen(context));
    hcdir_(context, blank, strlen(context), strlen(blank));    // needed
    delete [] mFilename;
    delete [] mMode;
    delete [] context;
}

StHbookTuple::StHbookTuple() {}

StHbookTuple::StHbookTuple(const StHbookTuple&) {}

StHbookTuple::StHbookTuple(const char* name, int n) : mNTags(n)
{
    mId = nextId(1);
    _init(name);
}

StHbookTuple::StHbookTuple(int id, const char* name, int n) : mNTags(n)
{
    mId = nextId(id);
    _init(name);
}

void StHbookTuple::_init(const char* name)
{
    mTitle = new char[strlen(name)+1];
    strcpy(mTitle, name);
    mITag = 0;
    mTags = new char[MaxTagLength*mNTags+mNTags];
    for (int i=0; i<mNTags; i++)
	sprintf(&mTags[i*MaxTagLength],"%-8.8s", "unknown");
}

StHbookTuple::~StHbookTuple()
{
    delete [] mTags;
    hdelet_(&mId);
}

StHbookTuple& StHbookTuple::operator=(const StHbookTuple&)
{
    return *this;
}

StHbookTuple& StHbookTuple::setTag(const char *tag)
{
    if (mBooked) {
	cerr << "StHbookTuple::setTag():" << endl;
	cerr << "\tWARNING" << endl;
	cerr << "\tTuple already booked. No further tags can be added." << endl;
    }
    else {
	if (mITag < mNTags) {
	    sprintf(&mTags[mITag*MaxTagLength],"%-8.8s", tag);
	    mITag++;
	}
    }
    return *this;
}

StHbookTuple& StHbookTuple::operator<< (const char *tag)
{
    return this->setTag(tag);
}

void StHbookTuple::book()
{
    if (!mBooked) {
	char *context = new char[strlen("HISTOS")+1];
	strcpy(context, "HISTOS");
	int nPrime = 4096;
	hbookn_(&mId, mTitle, &mNTags, context, &nPrime, mTags,
		strlen(mTitle), strlen(context), MaxTagLength);
	mBooked = true;
        delete [] context;
    }
}

void book(StHbookTuple& tuple) { tuple.book(); }

void StHbookTuple::operator<< (void (*pf)(StHbookTuple& tuple))
{
    (*pf)(*this);
}

void StHbookHisto::fill(float x, float weight)
{
    float dummy = 0;
    hfill_(&mId, &x, &dummy, &weight);
}

void StHbookHisto::fastFill(float x, float weight)
{
    hf1_(&mId, &x, &weight);
}

void StHbookHisto2::fill(float x, float y, float weight)
{
    hfill_(&mId, &x, &y, &weight);
}

void StHbookHisto2::fastFill(float x, float y, float weight)
{
    hf2_(&mId, &x, &y, &weight);
}

void StHbookTuple::fill(float *tuple)
{
    hfn_(&mId, tuple);
}

StInt StHbookTuple::getEvent(int eventNumber, float *vector)
{
    int ierror = 0;   
    hgnf_(&mId, &eventNumber, vector, &ierror);
    return ierror;
}
