#ifndef EEstarTrig_h
#define EEstarTrig_h
/*********************************************************************
 * $Id: EEstarTrig.h,v 1.1 2003/05/22 19:12:27 balewski Exp $
 *********************************************************************
 * container for STAR trigger data
 * $Log: EEstarTrig.h,v $
 * Revision 1.1  2003/05/22 19:12:27  balewski
 * add trigger data to ezTree
 *
 *
 *********************************************************************/
//#include <time.h>

#include <TObject.h>
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
  
  // E-EMC DSM  inputs
  u_char EEMC[144] ;
  u_short EEMC_l1[16] ;
  
  // B-EMC DSM  inputs
  u_char  BEMC[2][240] ;  
  u_short BEMC_l1[48] ;
  
  // CTB hits
  u_char	CTB[240] ;
  
  /* ADD:
    - BBC vertex
    - token
    - DSM last layer
    - spin bits

   */
  
   
  EEstarTrig();
  virtual ~EEstarTrig();
  void  print(int k=0) const;
  void  clear();
  ClassDef(EEstarTrig,1) 

};
#endif


