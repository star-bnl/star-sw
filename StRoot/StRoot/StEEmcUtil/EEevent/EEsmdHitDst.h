// \class  EEtwHitDst
// \author Piotr A. Zolnierczuk, Aug 2002
#ifndef EEsmdHitDst_h
#define EEsmdHitDst_h
/*********************************************************************
 * $Id: EEsmdHitDst.h,v 1.1 2003/01/28 23:16:07 balewski Exp $
 *********************************************************************
 * Descripion:
 * STAR Endcap Electromagnetic Calorimeter Raw Hits
 *********************************************************************
 * $Log: EEsmdHitDst.h,v $
 * Revision 1.1  2003/01/28 23:16:07  balewski
 * start
 *
 * Revision 1.2  2002/11/11 21:22:48  balewski
 * EEMC added to StEvent
 *
 * Revision 1.1  2002/10/01 14:41:54  balewski
 * SMD added
 *
 * Revision 1.3  2002/09/25 16:47:56  balewski
 * cleanup , cut in geant time for twoer-like detectors
 *
 * Revision 1.2  2002/09/20 21:58:13  balewski
 * sum of MC hits over activ detectors
 * produce total tower energy with weight 1 1 1 1
 *
 * Revision 1.1.1.1  2002/09/19 18:58:54  zolnie
 * Imported sources
 *
 * Revision 1.1.1.1  2002/08/29 19:32:01  zolnie
 * imported sources
 *
 * Revision 1.2  2002/08/28 01:44:03  zolnie
 * version alpha - 2
 *
 * Revision 1.1  2002/08/26 19:46:19  zolnie
 * Initial revision
 *
 *********************************************************************/
#include "TObject.h"


class EEsmdHitDst : public TObject {
 private:
  Float_t  mEnergy;  // GeV
  int      mStrip; // 1-~288
public:
  EEsmdHitDst();

  virtual ~EEsmdHitDst();

  Float_t             energy()    const { return mEnergy;    }
  int                 strip() {return mStrip;}
  void                set( int,  Float_t e);
  void                get(  int &strip, Float_t &e);
  void                print();
  
  ClassDef(EEsmdHitDst,1) 
};
#endif



