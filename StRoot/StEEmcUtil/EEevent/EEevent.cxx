// $Id: EEevent.cxx,v 1.1 2003/01/28 23:16:06 balewski Exp $
// $Log: EEevent.cxx,v $
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

#include <assert.h>
#include <TClonesArray.h>

#include "EEsectorDst.h"
#include "EEevent.h"

ClassImp(EEevent)
//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
EEevent::EEevent(){
  // printf("EEevent() constructed\n");
  type=kUnknown;
  ID=-1;
  Sec= new TClonesArray("EEsectorDst",1000);
  Sec->Clear();
}

//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
EEevent::~EEevent() {/* noop */}

//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
int EEevent::getNSectors(){return Sec->GetEntries();}

//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
void EEevent::clear(){ // only content of sectors, leave sectors
  //  printf("EEvent Dst-hits cleared in %d sectors\n",Sec->GetEntries());
  
  ID=-999;
  type=kUnknown;
  int is;
  for(is=0;is<Sec->GetEntries();is++) {
      EEsectorDst *sec=(EEsectorDst*)Sec->At(is);
      // printf("aa ID=%d sector cleared\n",sec->getID());
      sec->clear();
    }

}

//-------------------------------------------------
//-------------------------------------------------
//-------------------------------------------------
void EEevent::print(){
  printf("Event ID=%d, type=%d nSec= %d\n",ID,type,Sec->GetEntries());
  int is;
  for(is=0;is<Sec->GetEntries();is++) {
    EEsectorDst *sec=(EEsectorDst*)Sec->At(is);
    sec->print();
  }

}


//-------------------------------------------------
//-------------------------------------------------
//-------------------------------------------------
EEsectorDst * EEevent::addSectorDst(int IDx) {
   // To avoid calling the very time consuming operator new for each track,
   // the standard but not well know C++ operator "new with placement"
   // is called. If tracks[i] is 0, a new Track object will be created
   // otherwise the previous Track[i] will be overwritten.

   TClonesArray &SEC1 = *Sec;
   int nSec=SEC1.GetEntries();
   EEsectorDst *sec= new(SEC1[nSec]) EEsectorDst(IDx);;
   sec->clear();
   //printf("Dst-hits add sector %d Array \n",IDx);
   //sec->print();
   
   return sec;
}

//---------------------------------------------------
//---------------------------------------------------
//---------------------------------------------------
EEsectorDst* EEevent::getSec(int secID){
  int is;
  for(is=0;is<Sec->GetEntries();is++) {
      EEsectorDst *sec=(EEsectorDst*)Sec->At(is);
      if(sec->getID()==secID) return sec;
    }
  //  printf("EEevent() :  sector ID=%d not found\n",secID);
  return 0; // 
}


//---------------------------------------------------
//---------------------------------------------------
//---------------------------------------------------
void EEevent:: sumRawMC(EEevent* eveOut,float minE) {
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

