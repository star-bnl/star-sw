#ifndef EEdsm3_h
#define EEdsm3_h
/**************************************************************
 * $Id: EEdsm3.h,v 1.1 2004/11/29 20:12:59 balewski Exp $
 **************************************************************/

class EEdsm3  {
  static const int nc=1; // # of used channels to DSM
  ushort data[nc];
  
 private:
 public:
  
  EEdsm3();
  virtual ~EEdsm3();
  void  print(int k=0);
  void  clear();
  void  setWord(int ch, ushort val);
  int   getNc(){return nc;}
  
  ushort getEthr(); // energy trigger
  ushort getHTthr(); // high tower trigger bits
  ushort getJPthr(); // jet patch trigger bits
  
};
#endif


/* container for STAR trigger data
 * $Log: EEdsm3.h,v $
 * Revision 1.1  2004/11/29 20:12:59  balewski
 * first
 *
 * Revision 1.1  2004/02/17 03:09:17  balewski
 * *** empty log message ***
 *
 * Revision 1.1  2003/05/22 19:39:00  balewski
 * analysis of DSM data
 *
 * 

 *
 **************************************************************/

