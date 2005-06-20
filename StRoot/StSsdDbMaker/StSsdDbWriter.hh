// $Id: StSsdDbWriter.hh,v 1.2 2005/06/20 14:21:38 lmartin Exp $
//
// $Log: StSsdDbWriter.hh,v $
// Revision 1.2  2005/06/20 14:21:38  lmartin
// CVS tags added
//

/***************************************************************************
 *
 *  StSsdDbWriter.hh,v 1.01 2004/05/19 Reinnarth
 *
 * Author: Joerg Reinnarth joerg.reinnarth@subatech.in2p3.fr
 *
 * Description: SSD DB access Maker
 *
 ***************************************************************************
 *
 * $Log: StSsdDbWriter.hh,v $
 * Revision 1.2  2005/06/20 14:21:38  lmartin
 * CVS tags added
 *
 * Revision 1.1  2004/07/20 13:56:46  croy
 * 1st version of a writer for SSD databases
 *
 * Revision 1.01  2005/05/19 reinnarth
 * Getting started and testing
 *
 *
 **************************************************************************/

#ifndef STSSDDBWRITER_H
#define STSSDDBWRITER_H

#include "StDbLib/StDbDefs.hh"                         //

#ifdef __ROOT__
#include "TROOT.h"                         //
#endif

class StDbManager;
class StDbConfigNode;


class StSsdDbWriter
{
 private:
  Text_t *mTimeStamp;            //!
  Int_t   mUnixTimeStamp;

  StDbManager* mDbMgr;           //!

  StDbConfigNode* mConfigCalib;  //!
  StDbConfigNode* mConfigGeom;   //!
  StDbConfigNode* mConfigCond;   //!

 protected:

 public: 
  StSsdDbWriter(Text_t *timestamp);
  StSsdDbWriter(Int_t timestamp = 0);
  virtual ~StSsdDbWriter();

  void setDbManager();
  void setTimeStamp(Text_t *timestamp) {mTimeStamp = timestamp;}
  void setTimeStamp(Int_t timestamp) {mUnixTimeStamp = timestamp;}
  void testwriting();

#ifdef __ROOT__
  ClassDef(StSsdDbWriter, 1)   //StAF chain virtual base class for Makers
#endif
};

#endif
