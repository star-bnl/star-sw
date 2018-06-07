// $Id: StTrgDatReader.cxx,v 1.11 2018/05/29 20:01:36 akio Exp $
//
// $Log: StTrgDatReader.cxx,v $
// Revision 1.11  2018/05/29 20:01:36  akio
// Fixing run# problem when running with StFile list
//
// Revision 1.10  2010/01/25 17:41:53  akio
// Remove RecordSize, and RecordUnixTime to return 2019686401 (Sat Dec 31 19:00:01 2033)
//
// Revision 1.9  2010/01/15 21:26:51  fine
// RT #1816. Eliminate the side effect from RT 1803 fix
//
// Revision 1.8  2010/01/07 17:51:45  fine
// fix regexp to match the  Akio filename format
//
// Revision 1.7  2010/01/07 17:37:58  fine
// introduce closeFileSignal to process several DAT files at once. RT # 1794
//
// Revision 1.6  2010/01/06 20:57:24  fine
// Adjust  file pattern
//
// Revision 1.5  2010/01/06 20:42:26  fine
// Fix type EventNumber shoould be RunNumber . Thanks Akio
//
// Revision 1.4  2010/01/06 20:24:29  fine
// fix typo
//
// Revision 1.3  2010/01/06 20:09:39  fine
// RT #1794. Add EventNumber method to the StStreamFile interface RT # 1794
//
// Revision 1.2  2009/10/27 14:23:20  jeromel
// Fixed declaration of assert
//
// Revision 1.1  2009/10/13 15:53:31  fine
// Akio\'s DAT file format reader
//
// 

/***************************************************************************
 * Author: akio ogawa
 * Description: Trigger Data file (run*.*.dat) reader
 **************************************************************************/

#include "StTrgDatReader.h"
#include "StArchInfo.h"
#include "StMessMgr.h"
#include <iostream>
#include <assert.h>
#include <regex.h>

namespace {
//__________________________________________________________________________
inline unsigned int swapI(unsigned int var){
  return
    (var & 0xff000000) >> 24 |
    (var & 0x00ff0000) >> 8  |
    (var & 0x0000ff00) << 8  |
    (var & 0x000000ff) << 24 ;
}
}
const int StTrgDatReader::mLheader=8;
//__________________________________________________________________________
StTrgDatReader::StTrgDatReader():StStreamFile(), mLength(0),mVersion(0), mRunNumber(-1)
                                               , mData(0)  , mAllocated(0) 
{
   Buffer(mLheader);
}

//__________________________________________________________________________
StTrgDatReader::StTrgDatReader(const char *fileName, ios_base::openmode mode)
      :StStreamFile(fileName,mode), mLength(0), mVersion(0),mRunNumber(-1), mData(0),mAllocated(0)
{
   Buffer(mLheader);
}


//__________________________________________________________________________
StTrgDatReader::~StTrgDatReader() { Buffer(0) ; }

//__________________________________________________________________________
char *StTrgDatReader::Buffer(streamsize n)
{
   if ( (n >mAllocated) || (n==0)) {
      mData = (char *)realloc(mData,n);
      assert(!n || (mData && "There is no memory to allocate the I/O buffer"));
      mAllocated=n;
   }
   return mData;
}

//__________________________________________________________________________
fstream &StTrgDatReader::Read(){
  if (is_open() ) {
     read(Buffer(mLheader), mLheader);
     if (good()) {
        mVersion = swapI(*(unsigned int*)mData);
        mLength  = swapI(*(unsigned int*)(mData+4));
        assert( (mLength - mLheader > 0) && "The file is too short!!!");
        if(Debug() >0) { 
           printf("Version = %x\nLength  = %d; data=%p\n",Version(), Length(),mData);
        }
        read(Buffer(mLength)+mLheader, mLength - mLheader);
    }
    if (!good()) {
       mLength = 0;
       if (!eof()) Perror("*********** Attention DAT errors:");
    }
  }
  return stream();
}
//__________________________________________________________________________
int StTrgDatReader::RunNumber()  const  {
   if (mRunNumber == -1) { 
      TString ts(filename());
      ts.ReplaceAll("_",".");
      Ssiz_t last=ts.Last('/'); 
      if(last>0) {last++; ts.Remove(0,last);}
      string f = ts.Data();
      //string f = filename();
      //cout << "filename ="<<f<<endl;
      regex_t rx;
      const   char    *pattern =  "^.*run([0-9]+)\\..+\\.dat$";
      int     rc;
      char    buffer[100];
      if ((rc = regcomp(&rx, pattern, REG_EXTENDED))) {
         regerror(rc, &rx, buffer, 100);
         LOG_ERROR << "Can not extract the event number from the file name \'" 
                << f << "\' because \'" 
                << buffer << "\'" 
                << endm;
         assert(0&&"Can not extract the event number from the file name");
      } else {
	//cout << "1filename ="<<f<<" run#="<<mRunNumber<<endl;
         regmatch_t matchptr[2];	 
         if ( !(regexec (&rx, f.c_str(), sizeof(matchptr)/sizeof(regmatch_t), matchptr, 0)) ) {
           ((StTrgDatReader*) this)->mRunNumber = atoi(f.substr(matchptr[1].rm_so,matchptr[1].rm_eo).c_str());
	   //cout << "2filename ="<<f<<" run#="<<mRunNumber<<endl;
         }
      }
      //cout << "3filename ="<<f<<" run#="<<mRunNumber<<endl;
   }
   return mRunNumber;  
}

//__________________________________________________________________________
int  StTrgDatReader::RecordUnixTime() const
{ 
   // the current record /event unix time (-1 = "unknown") 
   return 2019686401; 
}

//__________________________________________________________________________
int StTrgDatReader::Length() const   { return mLength;  }

//__________________________________________________________________________
int  StTrgDatReader::Version() const { return  mVersion;} 

//__________________________________________________________________________
char* StTrgDatReader::Record()       { return mData;    }
//__________________________________________________________________________
bool  StTrgDatReader::closeFileSignal()
{
//< the method to be overriden in subclass to customize the "new file has been open" status
    mRunNumber = -1;
    return true;
}
