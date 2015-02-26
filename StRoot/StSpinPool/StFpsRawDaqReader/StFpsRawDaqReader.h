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

class StFmsCollection;

class StFpsRawDaqReader : public StMaker {
 public: 
   StFpsRawDaqReader( const Char_t* name = "fmsRawDaqReader", const Char_t *daqFileName = "");
   virtual ~StFpsRawDaqReader();

   void setFilename( std::string filename );

   virtual Int_t Init();
   virtual Int_t Make();
   virtual void Clear( Option_t *opts = "" );
   virtual Int_t prepareEnvironment();

   // Get CVS
   virtual const char *GetCVS() const;

 protected:
   Int_t mDate, mTime;
   StFmsCollection *mFmsCollectionPtr;

 private:
   std::string mDaqFileName, mDbMkrName;
   daqReader *mRdr;
   StFmsDbMaker *mFmsDbMkr;

   ClassDef(StFpsRawDaqReader,1);
};

// inline functions
inline void StFpsRawDaqReader::setFilename( std::string filename ){ mDaqFileName = filename; };
inline const char *StFpsRawDaqReader::GetCVS() const {
   static const char cvs[] = "Tag $Name:  $ $Id: StFpsRawDaqReader.h,v 1.1 2015/02/26 20:26:38 akio Exp $ built "__DATE__" "__TIME__ ;
   return cvs;
};

#endif

/*
 * $Id: StFpsRawDaqReader.h,v 1.1 2015/02/26 20:26:38 akio Exp $
 * $Log: StFpsRawDaqReader.h,v $
 * Revision 1.1  2015/02/26 20:26:38  akio
 * Adding raw daq file (or EVP) reader for FPS (not for offline BFC, but for online use)
 *
 */
