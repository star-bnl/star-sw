// $Id: EEeventDst.cxx,v 1.9 2007/05/30 02:38:48 balewski Exp $

#include <cassert>
#include <TClonesArray.h>
#include <StMessMgr.h>

#include "EEsectorDst.h"
#include "EEeventDst.h"

ClassImp(EEeventDst)
//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
EEeventDst::EEeventDst(){
  // printf("EEeventDst() constructed\n");
  Sec= new TClonesArray("EEsectorDst",1000);
  clear(); 
}

//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
EEeventDst::~EEeventDst() 
{
  delete Sec; Sec = 0;
}

//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
int EEeventDst::getNSectors(){return Sec->GetEntries();}

//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
void EEeventDst::clear(){ // only content of sectors, leave sectors
  //  printf("EEvent Dst-hits cleared in %d sectors\n",Sec->GetEntries());
  
  ID=-999;
  type=kUnknown;
  token=-2;
  timeStamp=0;
  Sec->Delete();
}
//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
void EEeventDst::Clear(const char*){ clear();}

//-------------------------------------------------
//-------------------------------------------------
//-------------------------------------------------
void EEeventDst::print(int k){
  LOG_INFO<<Form("Event ID=%d, type=%d  token=%d nSect=%d time stamp= %d\n",ID,type,token,Sec->GetEntries(),timeStamp)<<endm;
  int is;
  for(is=0;is<Sec->GetEntriesFast();is++) {
    EEsectorDst *sec=(EEsectorDst*)Sec->At(is);
    sec->print(k-1);
  }

}


//-------------------------------------------------
//-------------------------------------------------
//-------------------------------------------------
EEsectorDst * EEeventDst::addSectorDst(int IDx) {
   // To avoid calling the very time consuming operator new for each track,
   // the standard but not well know C++ operator "new with placement"
   // is called. If tracks[i] is 0, a new Track object will be created
   // otherwise the previous Track[i] will be overwritten.

   TClonesArray &SEC1 = *Sec;
   int nSec=SEC1.GetEntriesFast();
   EEsectorDst *sec= new(SEC1[nSec]) EEsectorDst(IDx);;
   sec->clear();
   //printf("Dst-hits add sector %d Array \n",IDx);
   //sec->print();
   
   return sec;
}

//---------------------------------------------------
//---------------------------------------------------
//---------------------------------------------------
EEsectorDst* EEeventDst::getSec(int secID, int create){
  int is;
  //printf("EEeventDst::getSec(%d), nSec=%d\n",secID,Sec->GetEntries());
  for(is=0;is<Sec->GetEntries();is++) {
      EEsectorDst *sec=(EEsectorDst*)Sec->At(is);
      if(sec->getID()==secID) return sec;
    }
  //printf("EEeventDst() :  sector ID=%d not found\n",secID);
  if(create==0)   return 0; 
  return  addSectorDst(secID);
}


//---------------------------------------------------
//---------------------------------------------------
//---------------------------------------------------
void EEeventDst:: sumRawMC(EEeventDst* eveOut,float minE) {
  assert(eveOut);
  eveOut->clear();
  eveOut->ID=ID;
  eveOut->type=kTrigMC;
  
  int is;
  for(is=0;is<Sec->GetEntries();is++) {
    
    // access In & Out sectors with the same ID
    
    EEsectorDst *secIN=(EEsectorDst*)Sec->At(is);
    int secID=secIN->getID();
    EEsectorDst *secOut=eveOut->getSec(secID);
    if(secOut==0) { // add new sector if needed
      secOut= eveOut->addSectorDst(secID);
    }   
  
    secIN->sumRawMC(secOut,minE);
  }
     
}


// $Log: EEeventDst.cxx,v $
// Revision 1.9  2007/05/30 02:38:48  balewski
// replace printf -->LOG_XXX
//
// Revision 1.8  2004/04/08 21:34:17  perev
// Leak off
//
// Revision 1.7  2004/04/07 18:54:47  perev
// Cleanup delete in destructor added
//
// Revision 1.6  2003/11/12 19:59:06  balewski
// I forgot what has changed
//
// Revision 1.5  2003/10/02 20:52:45  balewski
// more functionality for print()
//
// Revision 1.4  2003/09/11 19:40:56  zolnie
// updates for gcc3.2
//
// Revision 1.3  2003/07/01 14:13:13  balewski
// no clue
//
// Revision 1.2  2003/02/21 22:21:47  balewski
// time stamp added
//
// Revision 1.1  2003/02/20 05:15:14  balewski
// reorganization
//
// Revision 1.1  2003/01/28 23:16:06  balewski
// start
//
// Revision 1.8  2002/11/30 20:03:15  balewski
// consistent with FeeRawTTree
//
// Revision 1.7  2002/11/12 20:08:29  balewski
// some cleanup
//
// Revision 1.6  2002/11/11 21:22:48  balewski
// EEMC added to StEvent
//
// Revision 1.5  2002/10/01 06:03:15  balewski
// added smd & pre2 to TTree, tof removed
//
// Revision 1.4  2002/09/26 14:48:28  balewski
// biger buffer
//
// Revision 1.3  2002/09/25 16:47:55  balewski
// cleanup , cut in geant time for twoer-like detectors
//
// Revision 1.2  2002/09/20 21:58:13  balewski
// sum of MC hits over activ detectors
// produce total tower energy with weight 1 1 1 1
//
// Revision 1.1.1.1  2002/09/19 18:58:54  zolnie
// Imported sources
//
// Revision 1.1.1.1  2002/08/29 19:32:01  zolnie
// imported sources
//
// Revision 1.2  2002/08/28 01:43:42  zolnie
// version alpha - 2
//
// Revision 1.1  2002/08/26 19:46:12  zolnie
// Initial revision
//

