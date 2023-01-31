#ifndef EEdsm3_h
#define EEdsm3_h
/**************************************************************
 * $Id: EEdsm3.h,v 1.1 2009/10/12 18:04:26 pibero Exp $
 **************************************************************/

class EEdsm3  {
  static const int nc=1; // # of used channels to DSM
  unsigned short data[nc];
  int mYear;// unpacking changed in 2006

 private:
 public:
  
  EEdsm3();
  virtual ~EEdsm3();
  void  print(int k=0) const;
  void  clear();
  void  setWord(int ch, unsigned short val);
  void  setYear(int y) { mYear=y;}
  int   getNc(){return nc;}
  
  unsigned short getBarreJPthr2bit() const;                
  unsigned short getBarreHTthr2bit() const;                                     
  unsigned short getBarreEsumThr1bit() const;                         
  unsigned short getJpsi1bit() const;                       
  unsigned short getBarreHTTPthr1bit() const; 
  unsigned short getBarreTPthr1bit() const; 
  unsigned short getEndcapJPthr2bit() const;                    
  unsigned short getEndcapHTthr2bit() const;
  unsigned short getEndcapEsumthr1bit() const;
  unsigned short getEndcapHTTPthr1bit() const;
  unsigned short getEndcapTPthr1bit() const;
  unsigned short getEtotThr1bit() const;
  
};
#endif


/* container for STAR trigger data
 * $Log: EEdsm3.h,v $
 * Revision 1.1  2009/10/12 18:04:26  pibero
 * Moved StEEmcUtil/EEdsm to StTriggerUtilities/Eemc
 *
 * Revision 1.5  2009/02/24 03:56:19  ogrebeny
 * Corrected const-ness
 *
 * Revision 1.4  2007/08/17 01:15:36  balewski
 * full blown Endcap trigger simu, by Xin
 *
 * Revision 1.3  2006/04/05 18:34:10  balewski
 * new DSM bit assignment in 2006,
 * possibly lost backward compatibility
 * use tagged 2005 version if needed
 *
 * Revision 1.2  2005/02/01 22:13:41  perev
 * Compatibility to redhat
 *
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

