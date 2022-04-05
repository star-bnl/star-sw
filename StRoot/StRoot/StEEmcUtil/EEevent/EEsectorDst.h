// Hey Emacs this is really -*-c++-*- ! 
// \class  EEevent
// \author Piotr A. Zolnierczuk, Aug 2002
#ifndef EEsectorDst_h
#define EEsectorDst_h
/*********************************************************************
 * $Id: EEsectorDst.h,v 1.4 2003/10/02 20:52:45 balewski Exp $
 *********************************************************************
 * Descripion:
 * STAR Endcap Electromagnetic Calorimeter Raw Hits
 *********************************************************************
 * $Log: EEsectorDst.h,v $
 * Revision 1.4  2003/10/02 20:52:45  balewski
 * more functionality for print()
 *
 * Revision 1.3  2003/02/20 20:13:15  balewski
 * fixxy
 * xy
 *
 * Revision 1.2  2003/02/20 05:15:14  balewski
 * reorganization
 *
 * Revision 1.1  2003/01/28 23:16:07  balewski
 * start
 *
 * Revision 1.4  2002/11/12 20:08:30  balewski
 * some cleanup
 *
 * Revision 1.3  2002/10/01 06:03:16  balewski
 * added smd & pre2 to TTree, tof removed
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

class EEsectorDst : public TObject{

  // hits from each subdetector in depth, for a single
public:  
  TClonesArray *Pre1Hits; 
  TClonesArray *Pre2Hits; 
  TClonesArray *SmdUHits; 
  TClonesArray *SmdVHits; 
  TClonesArray *TwHits; 
  TClonesArray *PostHits; 
  int ID ;// range 1-12

  void addTwHit(char, int,float,TClonesArray *hitA); //generic for tower-like entries
  void addSmdHit(int, float,TClonesArray *hitA); //generic for smd-like entries

  EEsectorDst(int=1000);
  virtual ~EEsectorDst();
  void print(int k=0);
  int getID(){return ID;}

  void addPre1Hit(char sub, int eta,float ener) {addTwHit(sub,eta,ener,Pre1Hits);}
  void addPre2Hit(char sub, int eta,float ener) {addTwHit(sub,eta,ener,Pre2Hits);}
  void addTwHit(char sub, int eta,float ener) {addTwHit(sub,eta,ener,TwHits);}
  void addPostHit(char sub, int eta,float ener) {addTwHit(sub,eta,ener,PostHits);}
  void addSmdUHit(int strip,float ener) {addSmdHit(strip,ener,SmdUHits);}
  void addSmdVHit(int strip,float ener) {addSmdHit(strip,ener,SmdVHits);}

  void sumRawMC(EEsectorDst *outSec, float minE); 
  void sumRawMCtw(TClonesArray *inH, float* sum, int mx);// add hist from many tracks within one detector element
  void sumRawMCsmd(TClonesArray *inH, float* sum, int mx);// add hist from many tracks within one detector element

  void clear();

  TClonesArray *getPre1Hits(){return Pre1Hits;}
  TClonesArray *getPre2Hits(){return Pre2Hits;}
  TClonesArray *getSmdUHits(){return SmdUHits;}
  TClonesArray *getSmdVHits(){return SmdVHits;}
  TClonesArray *getTwHits()  {return TwHits;}
  TClonesArray *getPostHits(){return PostHits;}

  ClassDef(EEsectorDst,1) // Endcap Emc event
};
#endif


