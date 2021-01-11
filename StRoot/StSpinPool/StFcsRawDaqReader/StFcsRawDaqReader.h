/*
 *
 * \class StFcsRawMaker
 *
 */

#ifndef STAR_StFcsRawDaqReader_HH
#define STAR_StFcsRawDaqReader_HH

#include "StRoot/St_base/Stypes.h"
#include "StMaker.h"
class daqReader;

class StEvent;
class StFcsCollection;
class StTriggerData;
class StFcsDbMaker;

class StFcsRawDaqReader : public StMaker {
 public: 
   StFcsRawDaqReader( const Char_t* name = "fcsRawDaqReader", const Char_t *daqFileName = "");
   virtual ~StFcsRawDaqReader();

   void setFilename( std::string filename ){mDaqFileName = filename;};

   virtual Int_t Init();
   virtual Int_t Make();
   virtual void Clear( Option_t *opts = "" );

   void setReadMode(int v) {mReadMode=v;}
   void setRun(int v) {mRun=v;}
   void setDebug(int v=1) {mDebug=v;}   

   //reading multiple sfs files from different daq conputers(sectors)
   //with limit of maxevt per file
   void setMaxSector(int max, int min=1, int maxevt=100) {mMaxSector=max; mSector=min; mMaxEvtPerSector=maxevt;}

   //this gives event# in a file (sector)
   int getEvtInSec() {return mEvtInSector;}

   virtual Int_t prepareEnvironment();   
   unsigned long long trgMask(){return mTrgMask;}
   StTriggerData* trgdata(){return mTrg;}

 protected:
   Int_t mDate, mTime;
   StEvent *mEvent;
   StFcsCollection *mFcsCollectionPtr;

 private:
   std::string mDaqFileName, mDbMkrName;
   daqReader *mRdr=0;
   unsigned int mRun=0;
   unsigned int mMaxSector=0;
   unsigned int mSector=0;
   unsigned int mMaxEvtPerSector=100;
   unsigned int mEvtInSector=0;
   StTriggerData* mTrg=0;
   StFcsDbMaker* mFcsDbMkr=0;
   unsigned int mReadMode=0;
   unsigned long long mTrgMask=0;
   int mDebug=0;

   virtual const Char_t *GetCVS() const {static const Char_t cvs[]="Tag " __DATE__ " " __TIME__ ; return cvs;}
   ClassDef(StFcsRawDaqReader,1)   
};

#endif

/*
 * $Id: StFcsRawDaqReader.h,v 1.2 2021/01/11 14:39:12 akio Exp $
 * $Log: StFcsRawDaqReader.h,v $
 * Revision 1.2  2021/01/11 14:39:12  akio
 * Change logic to skip over none standard events at the begining of files.
 * Added function to get event# in a sector=file.
 *
 * Revision 1.1  2019/03/14 14:45:35  akio
 * FCS raw daq reader for online monitoring
 *
 * Revision 1.1  2019/03/13 20:31:34  akio
 * FCS daqfile/evp reader for online
 *
 *
 */
