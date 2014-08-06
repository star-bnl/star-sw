
#ifndef STAR_StBbcSimulationMaker
#define STAR_StBbcSimulationMaker

/*!
 *                                                                     
 * \class  StBbcSimulationMaker
 * \author <a href="http://www.star.bnl.gov/~kopytin> M.Kopytine </a>
 * \date   2002/08/20
 * \brief  BBC simulation
 *
 * This is a standard BFC maker to simulate BBC response and fill StEvent.
 * External dependencies: GEANT description of BBC geometry in 
 * /pams/geometry/bbcmgeo
 * Especially, the numbering of sensitive elements !
 * Read README file for the info on the simulation "concept"/approximation.
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include<map>
#include<list>
#ifdef BbcSimQa
#include "TFile.h"
#include "TH1.h"
#endif

class StBbcSimulationMaker : public StMaker {
 private:
  map<short,short> Geant2PMT; /* maps PMT # onto g2t volume ID (set up for BBC
				 in g2t_volume_id.g)
			  */
  list<short> DeadTiles; /* g2t volume IDs of tiles to ignore */

#ifdef BbcSimQa
  map<short,short>PMT2Geant; /* maps g2t volume ID onto PMT# for QA purp.*/
  TFile* QaFile;
  TH1F* QaBbcPmtdE;
  TH1F* QaBbcPmtTime;
  TH1F* QaBbcEastVid;
  TH1F* QaBbcWestVid;
  TH1F* QaBbcEastPmt;
  TH1F* QaBbcWestPmt;
#endif
 protected:
 public: 
                  StBbcSimulationMaker(const char *name="BbcSimulation");
   virtual       ~StBbcSimulationMaker();
   virtual Int_t Init();
   virtual Int_t  Make();
   virtual Int_t Finish();
// virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
// virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 

   virtual const char *GetCVS() const {
     static const char cvs[]="Tag $Name:  $ $Id: StBbcSimulationMaker.h,v 1.3 2014/08/06 11:42:54 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
     return cvs;
   }

   ClassDef(StBbcSimulationMaker,0)  
};

#endif



