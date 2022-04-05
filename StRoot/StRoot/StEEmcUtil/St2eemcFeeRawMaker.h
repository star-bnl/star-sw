// $Id: St2eemcFeeRawMaker.h,v 1.5 2014/08/06 11:43:04 jeromel Exp $
// $Log: St2eemcFeeRawMaker.h,v $
// Revision 1.5  2014/08/06 11:43:04  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.4  2009/02/04 20:33:28  ogrebeny
// Moved the EEMC database functionality from StEEmcDbMaker to StEEmcUtil/database. See ticket http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1388
//
// Revision 1.3  2003/11/17 15:47:04  balewski
// fix of bug
//
// Revision 1.2  2003/09/10 19:47:09  perev
// ansi corrs
//
// Revision 1.1  2003/01/28 23:15:25  balewski
// start
//
// Revision 1.1  2002/12/17 19:41:35  balewski

/* \class St2eemcFeeRawMaker 
\author Jan Balewski

Reads EEMC hits from StEvent and converts them back to the DAQ raw data format using  inverse crate/channel mapping from DB.
Resulting TTree is stored as a ROOT file.<br>  

Example how to use this maker:
www.star.bnl.gov/STAR/eemc -->How To

*/

#ifndef STAR_St2eemcFeeRawMaker
#define STAR_St2eemcFeeRawMaker


#ifndef StMaker_H
#include "StMaker.h"
#endif

class TTree;
class StEEmcDb;
class EEfeeDataBlock;
class EEfeeRawEvent;
class EEmcEventHeader;

class St2eemcFeeRawMaker : public StMaker {
 private:
  // static Char_t  m_VersionCVS = "$Id: St2eemcFeeRawMaker.h,v 1.5 2014/08/06 11:43:04 jeromel Exp $";
  TTree *moutTTree; ///< output TTree
  StEEmcDb *meeDb; ///< to assess DB
  EEfeeRawEvent *meveTT; ///< output event
  EEmcEventHeader *mrunTT; ///< header of the output event
  int mNFeeCrate; ///<  numbers of FEE Data blocks
  EEfeeDataBlock *mcrateData; ///<holds data from crates

 protected:
 public: 
  St2eemcFeeRawMaker(const char *name="St2eemcFeeRaw");
  virtual       ~St2eemcFeeRawMaker();
  void setOutTTree(TTree *t ) {moutTTree=t;} ///< TTree must be initialized externaly
//  void setDb(StEEmcDb *aa){meeDb=aa;} ///< DB-reader must exist
  virtual Int_t Init();
  virtual Int_t InitRun  (int runumber);///< to change time stamp in TTree
  virtual Int_t  Make();
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: St2eemcFeeRawMaker.h,v 1.5 2014/08/06 11:43:04 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
  
  ClassDef(St2eemcFeeRawMaker,0) 
};

#endif

