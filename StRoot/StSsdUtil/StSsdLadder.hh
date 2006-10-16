// $Id: StSsdLadder.hh,v 1.1 2006/10/16 16:43:29 bouchet Exp $
//
// $Log: StSsdLadder.hh,v $
// Revision 1.1  2006/10/16 16:43:29  bouchet
// StSsdUtil regroups now methods for the classes StSsdStrip, StSsdCluster and StSsdPoint
//
// Revision 1.5  2005/03/18 14:06:30  lmartin
// missing CVS header added
//

/*!
 * \class StSsdLadder
 * \author to be filled - doc by L.Martin
 * \date 02/27/04 for the documentation

This class is the description of the SSD ladder objects.
A ladder is made of :

- An unique Id
- A layer number (must be 7)
- the number of wafers on that ladder and the number of strips per wafer side
- An array of pointers to the wafers

 */
#ifndef STSSDLADDER_HH
#define STSSDLADDER_HH

#include "StSsdUtil/StSsdWafer.hh"
class St_ssdWafersPosition;
class StSsdLadder
{
 public:
  StSsdLadder(Int_t rLadderNumb,Int_t rSsdLayer, Int_t rNWaferPerLadder, Int_t rNStripPerSide);
 ~StSsdLadder();

  void         initWafers(St_ssdWafersPosition *wafpos);
  Int_t        getLadderNumb()     { return mLadderNumb; }
  Int_t        getWaferPerLadder() { return mNWaferPerLadder; } 
  StSsdWafer*  getWafer(Int_t i)   { return mWafers[i];}
  void         debugUnPeu(Int_t monwafer);
  Int_t        idWaferToWafer(Int_t idWafer) {return (idWafer-7000)/100-1;}
  Int_t idWaferToWaferNumb(Int_t idWafer);
  Int_t waferNumbToIdWafer(Int_t waferNumb);
 private:
  Char_t   first[1];
 public:
  StSsdWafer** mWafers;
 private:
  Int_t    mLadderNumb;
  Int_t    mSsdLayer;
  Int_t    mNWaferPerLadder;
  Int_t    mNStripPerSide;
  Char_t   last[1];
};

#endif
