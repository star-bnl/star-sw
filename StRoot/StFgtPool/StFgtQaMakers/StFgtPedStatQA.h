/***************************************************************************
 *
 * $Id: StFgtPedStatQA.h,v 1.2 2014/08/06 11:43:12 jeromel Exp $
 * Author: S. Gliske, Jan 2012
 *
 ***************************************************************************
 *
 * Description: QA plots, hists, and text file for pedestals and status
 *
 ***************************************************************************
 *
 * $Log: StFgtPedStatQA.h,v $
 * Revision 1.2  2014/08/06 11:43:12  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.1  2012/01/31 09:26:16  sgliske
 * StFgtQaMakers moved to StFgtPool
 *
 * Revision 1.2  2012/01/25 11:24:55  sgliske
 * Added GetCVS tag
 *
 * Revision 1.1  2012/01/17 20:10:02  sgliske
 * creation
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_PED_STAT_QA_H_
#define _ST_FGT_PED_STAT_QA_H_

#include <string>
#include "StMaker.h"
class StFgtPedMaker;
class StFgtStatusMaker;

class StFgtPedStatQA : public StMaker {
 public:
   // constructors
   StFgtPedStatQA( const Char_t* name = "FgtPedStatQA",
                   const Char_t* pedMkrName = "FgtPedMaker",
                   const Char_t* statMkrName = "FgtStatusMaker" );

   // default OK
   // StFgtPedStatQA(const StFgtPedStatQA&);

   // equals operator -- default OK
   // StFgtPedStatQA& operator=(const StFgtPedStatQA&);

   // deconstructor
   virtual ~StFgtPedStatQA();

   Int_t Init();
   Int_t Finish();

   // Int_t Make();  No make--just uses output from StFgtPedMaker and StFgtStatusMaker

   // modifiers
   void setSaveTxtFile( const Char_t* filename );  // filename == "" forces not to save to file
   void setSaveRootFile( const Char_t* filename );  // filename == "" forces not to save to file
   void setSavePdfFile( const Char_t* filename );  // filename == "" forces not to save to file
   void setTimeBin( Short_t timeBin = 2 );

   // get CVS
   virtual const char *GetCVS() const;

 protected:
   // for the ped maker
   std::string mPedMkrName;
   StFgtPedMaker *mPedMkr;

   // for the status maker
   std::string mStatMkrName;
   StFgtStatusMaker *mStatMkr;

   // which time bins to use
   Short_t mTimeBin;

   // for saving to file
   std::string mFilenameTxt, mFilenameRoot, mFilenamePdf;
   std::string* mFilenameArr[3];

   // parameters
   Float_t maxAdcPed;
   Float_t maxAdcRMS;
   Float_t maxAdcFrac;

   // functions that actually does the saving
   Int_t saveTxtFile();
   Int_t saveHists();

 private:   
   ClassDef(StFgtPedStatQA,1);

}; 

// inline functions

// modifiers
inline void StFgtPedStatQA::setSaveTxtFile( const Char_t* filename ){ mFilenameTxt = filename; };
inline void StFgtPedStatQA::setSaveRootFile( const Char_t* filename ){ mFilenameRoot = filename; };
inline void StFgtPedStatQA::setSavePdfFile( const Char_t* filename ){ mFilenamePdf = filename; };
inline void StFgtPedStatQA::setTimeBin( Short_t bin ){ mTimeBin = bin; };
inline const char *StFgtPedStatQA::GetCVS() const {
   static const char cvs[] = "Tag $Name:  $ $Id: StFgtPedStatQA.h,v 1.2 2014/08/06 11:43:12 jeromel Exp $ built " __DATE__ " " __TIME__ ;
   return cvs;
}

#endif
