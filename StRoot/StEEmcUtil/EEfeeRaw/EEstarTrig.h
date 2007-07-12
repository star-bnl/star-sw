#ifndef EEstarTrig_h
#define EEstarTrig_h
/*********************************************************************
 * $Id: EEstarTrig.h,v 1.10 2007/07/12 19:30:13 fisyak Exp $
 *********************************************************************
 * container for STAR trigger data
 */

#include <TObject.h>
#include <string.h>
#define u_int unsigned int
#define u_char unsigned char
#define u_short unsigned short

class EEstarTrig : public TObject {
 private:
 public:
  u_int bX48hi, bX48lo, bX7bit; // bXing counter

  //       u_int spinBits ( format not yet specified )  
  u_int daqbits ;	// fired triggers
  u_short offline_id[32] ;	// trigged ID for fired triggers
  
  // E-EMC DSM  inputs level-0, 1
  u_char EEMC[144] ;
  u_short EEMC_l1[16] ;
  
  // B-EMC DSM  inputs
  u_char  BEMC[2][240] ;  
  u_short BEMC_l1[48] ;
  
  // CTB hits
  u_char   CTB[240] ;
  u_short  lastDSM[8];  //level-3 inputs , results of all DSM trees 
  u_short  npre,npost; // used in a trivial way for now
  u_short  VTX[8];  // level-2 inputs for BBC and ZDC
  u_short  EMC[8];  // level-2 inputs, results of separate BEMC and EEMC DSMs 
     
  u_char spinBits() const { return (lastDSM[7]>>4) & 0xff; }
  u_short bbcTimeDiff() const { return VTX[3] & 0x1ff; }
  int isTrigID(int id);
  int get48bXing() const;
  EEstarTrig();
  virtual ~EEstarTrig();
  void  print(int k=0, FILE *fd=stdout) const;
  void  clear();
  ClassDef(EEstarTrig,3) 

};
#endif

/*************************************************************
 * $Log: EEstarTrig.h,v $
 * Revision 1.10  2007/07/12 19:30:13  fisyak
 * Add includes for ROOT 5.16
 *
 * Revision 1.9  2004/09/07 20:32:01  balewski
 * more methods, remove questionable spin bits interpetation
 *
 * Revision 1.8  2004/06/21 19:50:21  balewski
 * mre detailed monitoring of data corruption
 *
 * Revision 1.7  2003/09/11 19:41:03  zolnie
 * updates for gcc3.2
 *
 * Revision 1.6  2003/06/16 16:03:54  zolnie
 * updated root version number
 *
 * Revision 1.5  2003/06/02 04:36:40  balewski
 * added check if trigID
 *
 * Revision 1.4  2003/05/29 21:43:54  zolnie
 * back to Jas order
 *
 * Revision 1.3  2003/05/29 21:42:11  zolnie
 * add file destriptor to print() method
 *
 * Revision 1.2  2003/05/28 21:01:00  balewski
 * added spin bits
 *
 * Revision 1.1  2003/05/22 19:12:27  balewski
 * add trigger data to ezTree
 *
 *
 *********************************************************************/


#if 0 // probably all is wrong, JB
  u_char bitYellFill()  const { return ( spinBits()>>0) & 0x1; }
  u_char bitYellUp()    const { return ( spinBits()>>1) & 0x1; }
  u_char bitYellDown()  const { return ( spinBits()>>2) & 0x1; }
  u_char bitYellUnpol() const { return ( spinBits()>>3) & 0x1; }
  u_char bitBlueFill()  const { return ( spinBits()>>4) & 0x1; }
  u_char bitBlueUp()    const { return ( spinBits()>>5) & 0x1; }
  u_char bitBlueDown()  const { return ( spinBits()>>6) & 0x1; }
  u_char bitBlueUnpol() const { return ( spinBits()>>7) & 0x1;}
#endif
