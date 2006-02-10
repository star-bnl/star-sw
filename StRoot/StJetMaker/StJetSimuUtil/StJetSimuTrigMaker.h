// $Id: StJetSimuTrigMaker.h,v 1.2 2006/02/10 18:08:32 mmiller Exp $

#ifndef STAR_StJetSimuTrigMaker
#define STAR_StJetSimuTrigMaker

/*!
 *                                                                     
 * \class  StJetSimuTrigMaker
 * \author rfatemi
 * \date   9/10/03
 * 
 * Reads in MuDst members and creates TP and Jet Trigger output
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#include "TrigDims.h"
#endif

class StChain;
class TH1F;
class TH2F;
class StMuDst;
class StMuEmcCollection;
class StEmcCollection;
class StMuDstMaker;
class StEmcGeom;
class StEvent;
class StMuEvent;
class StBbcTriggerDetector;
class StMuEmcPoint;
class StMuTrack;
class StEventInfo;
class StBemcTables;
class StJetEmcTrigSim;
//class StEmcTriggerMaker;
//class StBemcTrigger;
class StJetSimuTrigMaker : public StMaker {

 private:
 StMuDstMaker *muDstMaker;
 StMuEmcCollection *muEmcCol;
 StEmcGeom *emcGeom; 
 StMuEvent *muEvent;
 StBemcTables *myTable;
 StJetEmcTrigSim *htTrig;
 //Uncomment once you cvs co StEmcTriggerMaker
 //StEmcTriggerMaker *trgMaker;
 //StBemcTrigger *bemcTrig;

 int det;//detector number =1 for BTOW
 int id;
 int Bmod;//BEMC module from 1-120
 int Bsub;//BEMC submodule 1- 2
 int Beta;//BEMC tower 1-20
 float Teta;//BEMC actual eta
 float Tphi;//BEMC actual phi
 int BTowADC;//BEMC tower ADC (not pedestal subtracted)
 float Benergy;//BEMC tower Energy
 float Bgain;//BEMC tower gain
 float Bped;//BEMC tower ped
 int Bstat;//BEMC tower status
 float Brms;//BEMC tower ped rm
 int NumETow;//#Endcap towers
 int Esec;//EEMC sector 1-12
 int Esub;//EEMC subsector 1-5
 int Eeta;//EEMC eta bin 1-12
 int ETowADC;//EEMC Tower ADC
 bool print;

 protected:

 public: 
  StJetSimuTrigMaker(const char *name="SimuTrig");
  virtual       ~StJetSimuTrigMaker();
  virtual Int_t Init();
  virtual Int_t  Make();
  void ReneeBEMC();
  void ReneeEEMC();
  void Sum(int *,int *);
  void Max(int *,int *);

  int evtID;
  int bbcTrig;
  int BBCadc[BBCadcNum];

  int BHTmaxt;  //Hold HT for whole Barrel
  int BJPmaxt;  //Holds max JP sum for whole Barrel
  int BJPsumt;  //Holds sum of all JP in Barrel
  int EHTmaxt;  //Hold HT for whole EEMC
  int EJPmaxt;  //Holds max JP sum for whole EEMC
  int EJPsumt;  //Holds sum of all JP in EEMC

  int jpBsum[BemcJP];//Barrel -Holds jet patch energy sum
  int jpBmax[BemcJP];//Holds jet patch HT max
  int jpB_hit_num[BemcJP];//Holds number of hits per JP
  int tpBsum[BemcTP];// Holds Trigger Patch energy sum
  int tpBmax[BemcTP];// Holds Trigger Patch HT 
  float TowEt[20];//Holds max Et tower from each eta ring

  int jpEsum[EemcJP];//Endcap -Holds jet patch energy sum
  int jpEmax[EemcJP];//Holds jet patch HT max
  int jpE_hit_num[EemcJP];//Holds number of hits per JP
  int tpEsum[EemcTP];// Holds Trigger Patch energy sum
  int tpEmax[EemcTP];// Holds Trigger Patch HT 
  
  float Alex_ht_Et;//from StEmcTrigSimu hightower et
  int Alex_ht_DSM;//from StEmcTrigSimu hightower 6bit DSM
  int Alex_ht_id;//from StEmcTrigsimu hightower id

  int HT1_2004_dsm,HT1_2004_evt,HT1_2004_id;
  int JP1_2004_dsm,JP1_2004_evt,JP1_2004_id;
 
  void setPrintOption(int p)
    { print = p;
    };

  
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StJetSimuTrigMaker.h,v 1.2 2006/02/10 18:08:32 mmiller Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }

  ClassDef(StJetSimuTrigMaker, 1)   //StAF chain virtual base class for Makers
};

#endif


// $Log: StJetSimuTrigMaker.h,v $
// Revision 1.2  2006/02/10 18:08:32  mmiller
// Added Renee's modifications to incorporate 2005 Jet patch trigger.
//
// Revision 1.1  2004/10/12 18:49:12  mmiller
// Added StJetSimuUtil (should have added before, not sure why it didn't)
//
// Revision 1.1  2004/09/24 13:50:08  rfatemi
// Jet Simulation Makers
//
// Revision 1.14  2002/11/26 23:49:40  jeromel
// Small modif after Art's note ... doxygen issue + cleanup
//
// Revision 1.13  2002/04/28 01:28:36  jeromel
// Reshaped comments for doxygen. Hopefully, users will propagate this good
// habit.
//
// Revision 1.12  1999/09/24 22:03:09  perev
// Add InitRun & FinishRun to template maker
//
// Revision 1.11  1999/07/15 13:57:44  perev
// cleanup
//
// Revision 1.10  1999/07/10 22:59:17  fine
// Some comments have been introduced to show html docs
//
// Revision 1.9  1999/03/11 03:33:16  perev
// new schema
//
// Revision 1.8  1999/03/10 15:02:07  fine
// HTML link to STAR problem report form has been introduced
//
// Revision 1.7  1998/10/31 00:25:45  fisyak
// Makers take care about branches
//
// Revision 1.6  1998/10/06 18:00:31  perev
// cleanup
//
// Revision 1.5  1998/08/26 12:15:13  fisyak
// Remove asu & dsl libraries
//
// Revision 1.4  1998/08/14 15:25:58  fisyak
// add options
//
// Revision 1.3  1998/08/10 02:32:07  fisyak
// Clean up
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//
