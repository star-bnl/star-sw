#ifndef EEsmdPlain_h
#define EEsmdPlain_h
/*********************************************************************
 * $Id: EEsmdPlain.h,v 1.1 2004/06/12 04:09:22 balewski Exp $
 *********************************************************************
 * Descripion:
 *  response of single SMD plain to MIPs, 
 *********************************************************************/

#include "StEEmcUtil/EEfeeRaw/EEdims.h"

class EEsmdPlain {
 public: // nothing to hide, utility class
  // parameters, fixed :
  float thresE; // energy threshold for MIP
  char pattXX[MaxSmdStrips]; // MIP pattern with double hit
  char pattX[MaxSmdStrips]; // MIP pattern with one hit
  int xOffset; // offset of first x in the pattern
  char uv; // orientaton

  // event, cleared :
  char hitOver[MaxSmdStrips+1]; // result of comparison strips to threshold
  int iStrip[MaxSmdStrips]; // location of first matched strip,  counted from 0
  int type[MaxSmdStrips]; // 1 for ..x.. or 2 for ..xx..
  int nMatch; // # of matched MIP's

  EEsmdPlain();
  void set(float th, int Xoff, char uv0);
  void clear(); // only event part
  void print(int);
  void scanAdc(float *val, float thr);
  void findMipPattern();
 };



#endif
/*****************************************************************
 * $Log: EEsmdPlain.h,v $
 * Revision 1.1  2004/06/12 04:09:22  balewski
 * start
 *
 *
 ********************************************************************/

