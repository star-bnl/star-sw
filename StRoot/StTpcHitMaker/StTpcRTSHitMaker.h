#ifndef StTpcMcHitMaker_H
#define StTpcRTSHitMaker_H

/***************************************************************************
 *
 * $Id: StTpcRTSHitMaker.h,v 1.1.1.1 2008/05/27 14:22:41 fisyak Exp $
 * StTpcRTSHitMaker - class to runonline (RTS) cluster maker over StTpcRawData
 * $Log: StTpcRTSHitMaker.h,v $
 * Revision 1.1.1.1  2008/05/27 14:22:41  fisyak
 * Maker to access TPC DAQ information via EVP_READER
 *
 * Revision 1.1  2008/04/28 14:37:16  fisyak
 * Rearrage TpcHitMaker to make it run for parallel taks, add the first version of online clustering
 *
 * Revision 1.1.1.1  2008/04/03 20:16:39  fisyak
 * Initial version
 *
 *--------------------------------------------------------------------------
 *
 ***************************************************************************/

#include "StMaker.h"
class StTpcDigitalSector;
class rts_reader;
class StTpcRTSHitMaker : public StMaker {
 public:
  StTpcRTSHitMaker(const char *name="tpc_hits") : StMaker(name), m_Rts_Reader(0) {}
  virtual ~StTpcRTSHitMaker();
  
  Int_t               InitRun(Int_t runumber);
  Int_t               Make();
 private:
  rts_reader *m_Rts_Reader; //!
  // cvs
  virtual const char *GetCVS() const    {
    static const char cvs[]="Tag $Name:  $Id: built "__DATE__" "__TIME__ ; return cvs;
  }
  ClassDef(StTpcRTSHitMaker, 1)    //StTpcRTSHitMaker - class to fille the StEvewnt from DAQ reader
};

#endif
