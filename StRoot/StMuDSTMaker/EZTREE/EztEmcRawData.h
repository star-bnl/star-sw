/***************************************************************************
 *
 * $Id: EztEmcRawData.h,v 1.6 2009/04/20 16:36:20 tone421 Exp $
 *
 * Author: Alex Suaide, Mar 2004, JB
 ***************************************************************************
 *
 * Description: ~1:1 copy of StEztEmcRawData but without StElephant dependence
 *
 ***************************************************************************/

#ifndef EztEmcRawData_hh
#define EztEmcRawData_hh

#include "TObject.h"
#include <TArrayS.h>

class EztEmcRawData : public TObject {
public:
  enum { MAXEMCDATABANK=60}; // 48 would be enough for EEMC
  enum { WRDCNT=0,ERRFLG=1,TOKEN=2,CRATE=3}; // header words
  enum { bitCrateID=1<<0, bitToken=1<<1, bitLenCount=1<<2, bitTrigComm=1<<3,
	 bitErrFlag=1<<4, bitN256=1<<5, bitGhost=1<<6, bitCrateOff=1<<7, 
	 bitStall=1<<8};

  EztEmcRawData();
  EztEmcRawData(const EztEmcRawData&);
  ~EztEmcRawData();
  
  const UShort_t* header(int) const;
  const UShort_t* data(int) const; 
  
  const int     sizeHeader(int) const;
  const int     sizeData(int) const;
  
  const int     getNBlocks() const { return MAXEMCDATABANK;}
  void          createBank(int, int, int);
  
  void          setHeader(int, unsigned short*);
  void          setData(int, unsigned short*);
  
  const UShort_t getCorruption(int ib) const { return  mCorrupt[ib];}
  void          setCorruption(int ib, UShort_t x) {mCorrupt[ib]=x;}
  bool          isCrateVoid(int ib) const { return sizeData(ib)<=0; }

  static UShort_t  getErrFlag(const UShort_t* hd)  { 
    return  hd[ERRFLG] & 0x0FFF; }
  static UShort_t  getLenCount(const UShort_t* hd) { 
    return  hd[WRDCNT] & 0x0FFF; }
  static UShort_t  getToken(const UShort_t* hd)    { 
    return  hd[TOKEN] & 0x0FFF; }
  static UShort_t   getTrigComm(const UShort_t* hd) { 
    return (hd[CRATE] / 0x0100) &0x000F ; }
  static UShort_t   getCrateID(const UShort_t* hd)  { 
    return  hd[CRATE] & 0x00FF ; }

  UShort_t  getErrFlag(int ib)  const { return  getErrFlag(header(ib)); }
  UShort_t  getLenCount(int ib) const { return  getLenCount(header(ib)); }
  UShort_t  getToken(int ib)    const { return  getToken(header(ib)); }
  UShort_t  getTrigComm(int ib) const { return  getTrigComm(header(ib)); }
  UShort_t  getCrateID(int ib)  const { return  getCrateID(header(ib)); }

  //........ corruption tests ......
  // scans whole block
  bool doHeadCorruptionTest(int token,int  lenCount, int firstCrId, int errFlag=0) ;

  bool doTowerHeadCorruptionTest(int token) { 
    //before 2004 another params are needed
    return  doHeadCorruptionTest(token,0xa4,1); }

  bool doMapmtHeadCorruptionTest(int token) { 
    //before 2005 another params are needed
    return  doHeadCorruptionTest(token,0xc4,64); }

  bool purgeCrateOFF(int ib);// discards header & data for one crate
  void tagHeadValid(int ib, int token, int crId, int len, int trigComm, int errFlag, int dbg=0); // test one header & fill saninty

  //..... working horse(s) 
  static bool  isCrateOFF( const UShort_t* hd);
  static UShort_t  isHeadValid(const UShort_t* hd, int token, int crId, int len, int trigComm, int errFlag, int dbg=0);

  void print(int ib, int flag); // one block
  void print(int flag=0); // all nonzero blocks
  static void print(const UShort_t* hd, const UShort_t* d=0, int nd=0);
  
 protected:
  TArrayS       mHeader[MAXEMCDATABANK];
  TArrayS       mData[MAXEMCDATABANK];
  UShort_t      mCorrupt[MAXEMCDATABANK];// encodes all corruptions, filled in fly
  
 private:
  void          deleteBank(int);
  
  ClassDef(EztEmcRawData,2)
};
#endif

/**************************************************************************
 *
 * $Log: EztEmcRawData.h,v $
 * Revision 1.6  2009/04/20 16:36:20  tone421
 * Added const to isCrateVoid function
 *
 * Revision 1.5  2005/03/10 16:59:54  mvl
 * New routine for corruption checking by Jan Balewski
 *
 * Revision 1.4  2005/01/11 21:33:40  mvl
 * Minor mod to hearder checking (do not delete data for certain error states ) By Jan Balewski.
 *
 * Revision 1.2  2004/11/29 18:36:59  mvl
 * New code for header checks and some printing (by Jan Balewski)
 *
 * Revision 1.1  2004/10/28 00:10:19  mvl
 * Initial revision of ezTree classes (for EEmc raw data)
 *
 *
 **************************************************************************/

