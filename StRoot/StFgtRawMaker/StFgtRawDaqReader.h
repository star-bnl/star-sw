/*
 *
 * \class StFgtRawMaker
 * \author S. Gliske (sgliske@anl.gov) based on StFgtComsicReader v1.15 written by A. Vossen (avossen@indiana.edu)
 *
 * Reads in a DAQ or SFS file taken on the cosmic test stand or from
 * the actual DAQ machine, and fills the StEvent FGT containers.
 * Should replace the StFgtComsicReader.
 *
 */

#ifndef STAR_StFgtRawDaqReader_HH
#define STAR_StFgtRawDaqReader_HH

#include "StRoot/St_base/Stypes.h"
#include "StMaker.h"
class daqReader;
class StFgtDbMaker;
class StFgtDb;

class StFgtCollection;

class StFgtRawDaqReader : public StMaker {
 public: 
   StFgtRawDaqReader( const Char_t* name = "fgtRawDaqReader", const Char_t *daqFileName = "", const Char_t* dbMkrName = "fgtDbMaker" );
   virtual ~StFgtRawDaqReader();

   void setFilename( std::string filename );

   virtual Int_t Init();
   virtual Int_t Make();
   virtual void Clear( Option_t *opts = "" );
   virtual Int_t prepareEnvironment();

   void cutShortEvents( Bool_t doIt = 1 );
   void setIsCosmic( Bool_t itIs = 1 );

 protected:
   Bool_t mCutShortEvents, mIsCosmic;
   Int_t mDate, mTime;
   StFgtCollection *mFgtCollectionPtr;

 private:
   std::string mDaqFileName, mDbMkrName;
   daqReader *mRdr;
   StFgtDbMaker *mFgtDbMkr;

   ClassDef(StFgtRawDaqReader,1);
};

// inline functions

inline void StFgtRawDaqReader::setFilename( std::string filename ){ mDaqFileName = filename; };
inline void StFgtRawDaqReader::cutShortEvents( Bool_t doIt ){ mCutShortEvents = doIt; };
inline void StFgtRawDaqReader::setIsCosmic( Bool_t itIs ){ mIsCosmic = itIs; };

#endif

/*
 * $Id: StFgtRawDaqReader.h,v 1.1 2012/01/17 20:10:54 sgliske Exp $
 * $Log: StFgtRawDaqReader.h,v $
 * Revision 1.1  2012/01/17 20:10:54  sgliske
 * creation
 *
 *
 */
