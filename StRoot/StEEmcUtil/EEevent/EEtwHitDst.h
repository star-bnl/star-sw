// \class  EEtwHitDst
// \author Piotr A. Zolnierczuk, Aug 2002
#ifndef EEtwHitDst_h
#define EEtwHitDst_h
/*********************************************************************
 * $Id: EEtwHitDst.h,v 1.2 2003/02/20 05:15:15 balewski Exp $
 *********************************************************************
 * Descripion:
 * STAR Endcap Electromagnetic Calorimeter Raw Hits
 *********************************************************************
 * $Log: EEtwHitDst.h,v $
 * Revision 1.2  2003/02/20 05:15:15  balewski
 * reorganization
 *
 * Revision 1.1  2003/01/28 23:16:07  balewski
 * start
 *
 * Revision 1.5  2002/11/11 21:22:48  balewski
 * EEMC added to StEvent
 *
 * Revision 1.4  2002/10/01 06:03:16  balewski
 * added smd & pre2 to TTree, tof removed
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


class EEtwHitDst : public TObject {
 private:
  Float_t  mEnergy;  // GeV
  char     mSubSec; // 'A' - 'E'
  char     mEta   ; // 1 - 12  stored as int

public:
  EEtwHitDst();

  virtual ~EEtwHitDst();

  Float_t             energy()     const { return mEnergy;    }
  Int_t               eta()        const { return mEta;    }
  Int_t               sub()        const { return mSubSec;    }
  void                set( char, int, Float_t e);
  void                get( char &sub, int &eta, Float_t &e);
  void                print();
  
  ClassDef(EEtwHitDst,1) // Endcap Emc event
};
#endif

