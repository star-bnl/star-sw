#ifndef EEsmdPlain_h
#define EEsmdPlain_h
/*********************************************************************
 * $Id: EEsmdPlain.h,v 1.3 2004/07/10 18:40:54 balewski Exp $
 *********************************************************************
 * Descripion:
 *  response of single SMD plain to MIPs, 
 *********************************************************************/

#include "StEEmcUtil/EEfeeRaw/EEdims.h"

class EEsmdPlain {
 private:
  enum {oneOff=50}; // add extra dots before & after real smd strips
  char dotArray[2*oneOff+MaxSmdStrips+1]; // result of comparison strips to threshold
  

 public: // nothing to hide, utility class
  // parameters, fixed :
  float thresE; // energy threshold for MIP
  char pattXX[MaxSmdStrips]; // MIP pattern with double hit
  char pattX[MaxSmdStrips]; // MIP pattern with one hit
  int nDot; // offset of first x in the pattern  ....xx.....
  char uv; // orientaton

  // event, cleared :
  char *hitOver; // result of comparison strips to threshold
  int iStrip[MaxSmdStrips]; // location of first matched strip,  counted from 0
  int type[MaxSmdStrips]; // 1 for ..x.. or 2 for ..xx..
  int nMatch; // # of matched MIP's

  EEsmdPlain();
  void set(float th, int nd, char uv0);
  void clear(); // only event part
  void print(int x=1);
  void scanAdc(float *val, float thr);
  void findMipPattern();
 };



#endif
/*****************************************************************
 * $Log: EEsmdPlain.h,v $
 * Revision 1.3  2004/07/10 18:40:54  balewski
 * use now first and last 8 strips in 00xx00
 *
 * Revision 1.2  2004/06/15 20:03:26  balewski
 * to match web-descriptio
 *
 * Revision 1.1  2004/06/12 04:09:22  balewski
 * start
 *
 *
 ********************************************************************/

