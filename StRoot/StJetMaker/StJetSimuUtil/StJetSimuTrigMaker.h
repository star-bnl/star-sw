// $Id: StJetSimuTrigMaker.h,v 1.1 2004/10/12 18:49:12 mmiller Exp $

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
class StMuDstMaker;
class StEmcGeom;
class StEvent;
class StMuEvent;
class StBbcTriggerDetector;
class StMuEmcPoint;
class StMuTrack;
class StEventInfo;

class StJetSimuTrigMaker : public StMaker {

 private:
 StMuDstMaker *muDstMaker;   
 StMuEmcCollection *muEmcCol;
 StEmcGeom *emcGeom; 
 StMuEvent *muEvent;
 StMuEmcPoint *points;
 StMuTrack *primTrack;

 int pointArray;//number of values in array for point E,phi,eta,sinTheta
 int det;//detector number =1 for BTOW
 int Bmod;//BEMC module from 1-120
 int Bsub;//BEMC submodule 1- 2
 int Beta;//BEMC tower 1-20
 int BTowADC;//BEMC tower ADC
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

  int jpEsum[EemcJP];//Endcap -Holds jet patch energy sum
  int jpEmax[EemcJP];//Holds jet patch HT max
  int jpE_hit_num[EemcJP];//Holds number of hits per JP
  int tpEsum[EemcTP];// Holds Trigger Patch energy sum
  int tpEmax[EemcTP];// Holds Trigger Patch HT 
    
  int numPoints; //number of points in barrel
  float pointE[1000];//Energy of points
  float pointPhi[1000];//Phi of points
  float pointEta[1000];//Eta of points
  float sinTheta[1000];//sin(theta)

  int nTracks;//number of tracks in the event
  float ptTrack[100];//pT of each track
  float phiTrack[100];//phi of each track
  float etaTrack[100];//eta of each track
  int qTrack[100];//charge of each track
 
  void setPrintOption(int p)
    { print = p;
    };

  
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StJetSimuTrigMaker.h,v 1.1 2004/10/12 18:49:12 mmiller Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }

  ClassDef(StJetSimuTrigMaker, 1)   //StAF chain virtual base class for Makers
};

#endif


// $Log: StJetSimuTrigMaker.h,v $
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
