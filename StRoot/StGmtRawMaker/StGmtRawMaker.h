// \class StGmtRawMaker
// \authors K.S. Engle and Richard Witt (witt@usna.edu)
// based on StFgtRawMaker

#ifndef STAR_StGmtRawMaker_HH
#define STAR_StGmtRawMaker_HH

#include <math.h>
#include "StRoot/StChain/StRTSBaseMaker.h"

class StGmtCollection;
// class StGmtDb;

/**
   This is the raw maker for the GMT data. It makes use of its base class functions to read daq 
   files into the StGmtEvent Data structure.
*/
class StGmtRawMaker : public StRTSBaseMaker
{
 public: 
  StGmtRawMaker(const Char_t* name="GmtRaw");
  ~StGmtRawMaker();
  /**
     Init function. Not doing anything at the moment.
  */
  Int_t Init();
  Int_t  InitRun(Int_t runNumber);

  /**
     Maker main function. Getting pointer to StEvent and fills the event structure
  */
  Int_t Make();
  void Clear( Option_t *opts = "" );

  /**
     sets the pointer to the StGmtDb
  */
//   void setGmtDb(StGmtDb *x) {mGmtDb=x;}

 protected:

  /**
   utility function to get the data from the daq file and fill the StEvent structure
  */
  Int_t fillHits();
  /**
     Function to get pointer to StEvent datastructures. Creates them if they do not exist already.
  */
  Int_t prepareEnvironment();
  StGmtCollection *mGmtCollectionPtr;

 private:
  ////omitted copy constructor and assignment operator on purpose
  /** copy contructor. It is private since there is no use case for this
   */
  //  StGmtRawMaker(const StGmtRawMaker &source);

  /**
     private asignment operator
  */
  //  StGmtRawMaker& operator=(const StGmtRawMaker &source);
//   StGmtDb *mGmtDb;
  ClassDef(StGmtRawMaker,1);
};

#endif
