// Hey Emacs this is really -*-c++-*- ! 
// \class  EEevent
// \author Piotr A. Zolnierczuk, Aug 2002
#ifndef EEevent_h
#define EEevent_h
/*********************************************************************
 * $Id: EEevent.h,v 1.1 2003/01/28 23:16:06 balewski Exp $
 *********************************************************************
 * Descripion:
 * STAR Endcap Electromagnetic Calorimeter Raw Hits
 *********************************************************************
 * $Log: EEevent.h,v $
 * Revision 1.1  2003/01/28 23:16:06  balewski
 * start
 *
 * Revision 1.5  2002/11/11 21:22:48  balewski
 * EEMC added to StEvent
 *
 * Revision 1.4  2002/10/01 06:03:15  balewski
 * added smd & pre2 to TTree, tof removed
 *
 * Revision 1.3  2002/09/25 16:47:55  balewski
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

class TClonesArray;
class EEsectorDst;

class EEevent :public TObject{

public:

  enum DataType { 
    kUnknown = 0,
    kRawMC=1, //  1:1 copy of all geant hits
    kTrigMC=2  //  hits from many tracks added for each detector 
  };

  int  ID;
  DataType type; // not seen in TTree ???, but if you swap it with 'ID' then 'ID' is not seen, JB

  // hits  sorted by sectors
  TClonesArray  *Sec;   
  
  EEevent();
  virtual ~EEevent();
  void print();
  EEsectorDst *addSectorDst(int ID);
  EEsectorDst *getSec(int secID);
  void clear();
  int getID(){return ID;}
  int getNSectors();
  void setID(int id){ ID=id;}
  void setType(DataType t){ type=t;}
  DataType getType(){return type;}
  void sumRawMC(EEevent*,float minE=0.0001); //(GeV),(sec)
  
  ClassDef(EEevent,1) // Endcap Emc event
};
#endif


