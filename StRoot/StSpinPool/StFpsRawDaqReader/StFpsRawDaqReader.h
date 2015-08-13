/*
 *
 * \class StFpsRawMaker
 *
 */

#ifndef STAR_StFpsRawDaqReader_HH
#define STAR_StFpsRawDaqReader_HH

#include "StRoot/St_base/Stypes.h"
#include "StMaker.h"
class daqReader;
class StFmsDbMaker;

class StEvent;
class StFmsCollection;

class StFpsRawDaqReader : public StMaker {
 public: 
   StFpsRawDaqReader( const Char_t* name = "fpsRawDaqReader", const Char_t *daqFileName = "");
   virtual ~StFpsRawDaqReader();

   void setFilename( std::string filename );

   virtual Int_t Init();
   virtual Int_t Make();
   virtual void Clear( Option_t *opts = "" );

   virtual Int_t prepareEnvironment();
   unsigned long long trgMask(){return mTrgMask;}

   // Get CVS
   virtual const char *GetCVS() const;

 protected:
   Int_t mDate, mTime;
   StEvent *mEvent;
   StFmsCollection *mFmsCollectionPtr;

 private:
   std::string mDaqFileName, mDbMkrName;
   daqReader *mRdr;
   StFmsDbMaker *mFmsDbMkr;
   unsigned long long mTrgMask;

   ClassDef(StFpsRawDaqReader,1);
};

// inline functions
inline void StFpsRawDaqReader::setFilename( std::string filename ){ mDaqFileName = filename; };
inline const char *StFpsRawDaqReader::GetCVS() const {
   static const char cvs[] = "Tag $Name:  $ $Id: StFpsRawDaqReader.h,v 1.4 2015/08/13 16:45:48 jeromel Exp $ built " __DATE__ " " __TIME__ ;
   return cvs;
};

#endif

/*
 * $Id: StFpsRawDaqReader.h,v 1.4 2015/08/13 16:45:48 jeromel Exp $
 * $Log: StFpsRawDaqReader.h,v $
 * Revision 1.4  2015/08/13 16:45:48  jeromel
 * Quick fix for spin folks
 *
 * Revision 1.3  2015/05/21 18:23:51  akio
 * *** empty log message ***
 *
 * Revision 1.2  2015/02/28 02:58:56  akio
 * Some bug fixes
 *
 * Revision 1.1  2015/02/26 20:26:38  akio
 * Adding raw daq file (or EVP) reader for FPS (not for offline BFC, but for online use)
 *
 */
