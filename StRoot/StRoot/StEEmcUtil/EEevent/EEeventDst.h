// Hey Emacs this is really -*-c++-*- ! 
// \class  EEeventDst
// \author Piotr A. Zolnierczuk, Aug 2002
#ifndef EEeventDst_h
#define EEeventDst_h
/*********************************************************************
 * $Id: EEeventDst.h,v 1.4 2004/04/08 21:34:32 perev Exp $
 *********************************************************************
 * Descripion:
 * STAR Endcap Electromagnetic Calorimeter Raw Hits
 *********************************************************************
 * $Log: EEeventDst.h,v $
 * Revision 1.4  2004/04/08 21:34:32  perev
 * Leak off
 *
 * Revision 1.3  2003/07/01 14:13:13  balewski
 * no clue
 *
 * Revision 1.2  2003/02/21 22:21:47  balewski
 * time stamp added
 *
 * Revision 1.1  2003/02/20 05:15:14  balewski
 * reorganization
 *
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


class EEeventDst :public TObject{

public:

  enum DataType { 
    kUnknown = 0,
    kRawMC=1, //  1:1 copy of all geant hits
    kTrigMC=2,  //  hits from many tracks added for each detector 
    kMiniDaq=3  //  real hits from miniDAQ 
  };

  int  ID;
  DataType type; 
  int token;
  unsigned int timeStamp;         //(unix time, GMT) 
 
  // hits  sorted by sectors
  TClonesArray  *Sec;   
  
  EEeventDst();
  virtual ~EEeventDst();
  void print(int k=0);
  EEsectorDst *addSectorDst(int ID);
  EEsectorDst *getSec(int secID, int create=0);
  void clear();
  void Clear(const char* opt="");
  int getID(){return ID;}
  int getNSectors();
  void setID(int id){ ID=id;}
  void setType(DataType t){ type=t;}
  DataType getType(){return type;}
  void sumRawMC(EEeventDst*,float minE=0.0001); //(GeV),(sec)
  
  ClassDef(EEeventDst,1) // Endcap Emc event
};
#endif


