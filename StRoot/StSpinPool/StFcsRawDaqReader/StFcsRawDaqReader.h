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

   void setFilename( std::string filename );

   virtual Int_t Init();
   virtual Int_t Make();
   virtual void Clear( Option_t *opts = "" );

   void setReadMode(int v) {mReadMode=v;}
   void setRun(int v) {mRun=v;}
   void setDebug(int v=1) {mDebug=v;}

   virtual Int_t prepareEnvironment();   
   unsigned long long trgMask(){return mTrgMask;}
   StTriggerData* trgdata(){return mTrg;}

   // Get CVS
   virtual const char *GetCVS() const;

 protected:
   Int_t mDate, mTime;
   StEvent *mEvent;
   StFcsCollection *mFcsCollectionPtr;

 private:
   std::string mDaqFileName, mDbMkrName;
   daqReader *mRdr=0;
   unsigned int mRun=0;
   StTriggerData* mTrg=0;
   StFcsDbMaker* mFcsDbMkr=0;
   unsigned int mReadMode=0;
   unsigned long long mTrgMask=0;
   int mDebug=0;

   ClassDef(StFcsRawDaqReader,1);
};

// inline functions
inline void StFcsRawDaqReader::setFilename( std::string filename ){ mDaqFileName = filename; };
inline const char *StFcsRawDaqReader::GetCVS() const {
   static const char cvs[] = "Tag $Name:  $ $Id: StFcsRawDaqReader.h,v 1.1 2019/03/14 14:45:35 akio Exp $ built " __DATE__ " " __TIME__ ;
   return cvs;
};

#endif

/*
 * $Id: StFcsRawDaqReader.h,v 1.1 2019/03/14 14:45:35 akio Exp $
 * $Log: StFcsRawDaqReader.h,v $
 * Revision 1.1  2019/03/14 14:45:35  akio
 * FCS raw daq reader for online monitoring
 *
 * Revision 1.1  2019/03/13 20:31:34  akio
 * FCS daqfile/evp reader for online
 *
 *
 */
