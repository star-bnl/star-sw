// $Id: St2eemcFeeRawMaker.h,v 1.2 2003/09/10 19:47:09 perev Exp $
// $Log: St2eemcFeeRawMaker.h,v $
// Revision 1.2  2003/09/10 19:47:09  perev
// ansi corrs
//
// Revision 1.1  2003/01/28 23:15:25  balewski
// start
//
// Revision 1.1  2002/12/17 19:41:35  balewski

/* \class StEEmcDbMaker 
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
class StEEmcDbMaker;
class EEfeeDataBlock;
class EEfeeRawEvent;
class EEfeeRunDescr;

class St2eemcFeeRawMaker : public StMaker {
 private:
  // static Char_t  m_VersionCVS = "$Id: St2eemcFeeRawMaker.h,v 1.2 2003/09/10 19:47:09 perev Exp $";
  TTree *moutTTree; ///< output TTree
  StEEmcDbMaker * meeDb; ///< to assess DB
  EEfeeRawEvent *meveTT; ///< output event
  EEfeeRunDescr *mrunTT; ///< header of the output event
  int mNFeeCrate; ///<  numbers of FEE Data blocks
  EEfeeDataBlock *mcrateData; ///<holds data from crates

 protected:
 public: 
  St2eemcFeeRawMaker(const char *name="St2eemcFeeRaw");
  virtual       ~St2eemcFeeRawMaker();
  void setOutTTree(TTree *t ) {moutTTree=t;} ///< TTree must be initialized externaly
  void setDb(StEEmcDbMaker *aa){meeDb=aa;} ///< DB-reader must exist
  virtual Int_t Init();
  virtual Int_t InitRun  (int runumber);///< to change time stamp in TTree
  virtual Int_t  Make();
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: St2eemcFeeRawMaker.h,v 1.2 2003/09/10 19:47:09 perev Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }
  
  ClassDef(St2eemcFeeRawMaker,0) 
};

#endif

