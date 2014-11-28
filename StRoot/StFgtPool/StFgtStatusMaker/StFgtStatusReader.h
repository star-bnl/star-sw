/***************************************************************************
 *
 * $Id: StFgtStatusReader.h,v 1.1 2012/01/31 08:59:43 sgliske Exp $
 * Author: C. K. Riley, Nov 2011
 *
 ***************************************************************************
 *
 * Description: Reads the elecId status from file, and provide querying of 
 * pedestals in a similar manner as the DB. Note: this is not a Maker.
 *
 ***************************************************************************
 *
 * $Log: StFgtStatusReader.h,v $
 * Revision 1.1  2012/01/31 08:59:43  sgliske
 * moved StFgtStatus maker to StFgtPool
 *
 * Revision 1.3  2012/01/27 13:38:08  sgliske
 * updated to correspond with new StatusMakers,
 * Now keyed by elecId
 *
 * Revision 1.2  2011/12/01 00:41:34  ckriley
 * make compatible with db stuff
 *
 * Revision 1.1  2011/11/25 20:22:37  ckriley
 * creation of statusmaker
 *
 * 
 *
 **************************************************************************/


#ifndef _ST_FGT_STATUS_READER_
#define _ST_FGT_STATUS_READER_

#include <string>
#include <map>
#include <Rtypes.h>

class StFgtStatusReader {
 public:
   // constructors
   StFgtStatusReader( const Char_t* filename = "" );

   // default OK
   // StFgtStatusReader(const StFgtStatusReader&);

   // equals operator -- default OK
   // StFgtStatusReader& operator=(const StFgtStatusReader&);

   // deconstructor
   virtual ~StFgtStatusReader();

   // initialize
   Int_t Init();

   // accessor: input is elecId, output is status of strip
   Int_t getStatus( Int_t elecId ) const;

 protected:
   // input file
   std::string mFilename;

   typedef std::vector< Int_t > StatusVec_t;
   StatusVec_t mStatusVec;

 private:   
   ClassDef(StFgtStatusReader,1);

}; 

// inline functions

// accessor: input is elecId, output is status
inline Int_t StFgtStatusReader::getStatus( Int_t elecId ) const { return mStatusVec.at( elecId ); };

// deconstructor
inline StFgtStatusReader::~StFgtStatusReader(){ /* */ };

#endif
