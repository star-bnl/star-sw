// $Id: StPeCMaker.cxx,v 1.36 2018/02/26 23:30:18 smirnovd Exp $
// $Log: StPeCMaker.cxx,v $
// Revision 1.36  2018/02/26 23:30:18  smirnovd
// StPeCMaker: Init BTOF geometry using proper interface
//
// Revision 1.35  2016/02/24 16:50:00  ramdebbe
// just removed commented lines
//
// Revision 1.34  2015/07/22 18:29:51  ramdebbe
// added choice to skip writting the tree to output file and added summary histograms
//
// Revision 1.33  2015/02/25 01:19:38  ramdebbe
// added a setter to select writing Roman Pot Collection to output tree
//
// Revision 1.32  2014/06/18 17:29:49  ramdebbe
// ignore flag from StPeCEvent and writes all events with a summary
//
// Revision 1.31  2013/12/27 16:44:27  ramdebbe
// added extrapolation of tracks to TOF, geometry setup in InitRun
//
// Revision 1.30  2013/07/10 19:28:06  ramdebbe
// added returnValue in StEvent input mode
//
// Revision 1.29  2013/01/24 15:43:08  ramdebbe
// added more flags to choose input or output tracks tof etc.
//
// Revision 1.28  2012/06/13 15:45:42  ramdebbe
// Added flags to include TOF and Vertex branches in tree
//
// Revision 1.27  2007/04/28 17:56:34  perev
// Redundant StChain.h removed
//
// Revision 1.26  2004/02/07 01:40:46  meissner
// bug in check of un-analylized MC events, all MC events are now filled
//
// Revision 1.25  2003/11/25 01:54:30  meissner
// correct several bugs: eta cut for tracks, charge sorting, add counting of FTPC and TPC primary tracks, Add bbc information
//
// Revision 1.24  2002/12/19 18:09:53  yepes
// MuDST input added
//
// Revision 1.23  2002/06/04 17:55:02  meissner
// filtering: filter all  UPC triggerwords
//
// Revision 1.21  2002/04/18 19:02:11  meissner
// Change Init to  InitRun
//
// Revision 1.20  2002/03/19 22:23:44  meissner
// New variables: zdc unatt., Trigger word, MC tree if Geant Branch, DCA  for primary pairs, all tracks for secondary pairs (Test)
//
// Revision 1.19  2002/02/11 20:20:09  akio
// remove SetFormat
//
// Revision 1.18  2001/08/07 19:52:35  akio
// added a flag to make udst can have more than 1 depth of branches.
//
// Revision 1.17  2001/04/23 21:44:33  meissner
// add dEdx z variable to tree, setFormat(1) for tree, use private BetheBloch (temp solution)
//
// Revision 1.16  2001/02/21 20:42:05  yepes
// Add ctb signals to tree
//
// Revision 1.15  2001/02/14 18:34:44  yepes
// bug in deleting StEvent and the of of Make
//
// Revision 1.14  2001/02/13 17:54:41  yepes
// still problems on differnt platforms
//
// Revision 1.13  2001/02/13 16:33:58  yepes
// fix problem on Sun
//
// Revision 1.12  2001/02/12 21:15:55  yepes
// New version of StPeCMaker, lots of changes
//
// Revision 1.9  2000/04/21 19:09:49  nystrand
// Update StPeCPair class, new histograms
//
// Revision 1.8  2000/03/24 22:36:24  nystrand
// First version with StPeCEvent
//
// Revision 1.7  2000/01/20 23:03:08  nystrand
// First Version of StPeCMaker with new StEvent
//
// Revision 1.6  1999/09/24 01:23:19  fisyak
// Reduced Include Path
//
// Revision 1.5  1999/07/15 13:57:20  perev
// cleanup
//
// Revision 1.4  1999/06/27 22:45:29  fisyak
// Merge StRootEvent and StEvent
//
// Revision 1.3  1999/05/01 00:57:02  fisyak
// Change Clear function to defualt
//
// Revision 1.2  1999/04/08 16:37:15  nystrand
// MakeBranch,SetBranch removed
//
// Revision 1.1  1999/04/06 20:47:27  akio
// The first version
//
// Revision 1.0  1999/03/05 11:00:00  Nystrand
// initial version
//
//
///////////////////////////////////////////////////////////////////////////////
//
// StPeCMaker
//
// Description: 
//  Maker for Peripheral Collisions DST analysis
//  This version uses StPeCEvent
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Joakim Nystrand, LBNL
//
// History:
//
///////////////////////////////////////////////////////////////////////////////
#include "StPeCMaker.h"
#include "StPeCEnumerations.h"
#include "StEventTypes.h"
#include "Stypes.h"
#include "StMessMgr.h"
#include "TH1.h"
#include "TH2.h"
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

#include "StTriggerDetectorCollection.h"
#include "StCtbTriggerDetector.h"
#include "StMwcTriggerDetector.h"
#include "StVpdTriggerDetector.h"
#include "StZdcTriggerDetector.h"
#include "StFtpcHitCollection.h"
#include "StFtpcPlaneHitCollection.h"
#include "StFtpcSectorHitCollection.h"

#include "StMcEventTypes.hh"
#include "StMcEventMaker/StMcEventMaker.h"
#include "StIOMaker/StIOMaker.h"





static const char rcsid[] = "$Id: StPeCMaker.cxx,v 1.36 2018/02/26 23:30:18 smirnovd Exp $";

ClassImp(StPeCMaker)

StPeCMaker::StPeCMaker(const Char_t *name) : StMaker(name), infoLevel(0), filter(0), outputPerRun(0), muDst(0)
{
   treeFileName = "StPeCMaker.tree.root" ;
   triggerChoice = "something";
   return;
}


StPeCMaker::~StPeCMaker()
{
   return;
}


Int_t StPeCMaker::Init() {

   LOG_INFO << "StPeCMake INIT: tree output file:  " << treeFileName << endm;
   infoLevel = 0;
   LOG_INFO << "StPeCMake INIT: infoLevel:  " << infoLevel << endm;
   LOG_INFO <<"StPeCMaker INIT: readStMuDst " << readStMuDst << endm;
   LOG_INFO <<"StPeCMaker INIT: readStEvent " << readStEvent << endm;
   LOG_INFO <<"StPeCMaker INIT: readBoth "    << readStMuDst_and_StEvent << endm;
   LOG_INFO <<"trigger to be selected: "      << triggerChoice << endm;   // triggerChoice can be changed with the setter setTriggerOfInterest (const char * selectTrigger = "UPC_Main" ) see StPeCTrigger for details
   LOG_INFO <<"choice for output: "           << treeOff << endm;         // if argument of setter setSuppressTreeOut is kTRUE, the TTree will not be writte in output file

   //Get the standard root format to be independent of Star IO   
   m_outfile = new TFile(treeFileName, "recreate");
   m_outfile->SetCompressionLevel(1);


   if(!treeOff)uDstTree = new TTree("uDst", "Pcol uDst", 99);

   //Instantiate StPeCEvent

   pevent = new StPeCEvent(useBemc, useTOF, useVertex, useTracks, useRP, readStMuDst, readStEvent, readStMuDst_and_StEvent);
   pevent->setInfoLevel(infoLevel);
   if(!pevent) LOG_INFO << "StPeCMaker Init: unable to make instance of StPeCEvent ---------- " << endm;
   trigger = new StPeCTrigger();
   trigger->setInfoLevel(infoLevel);
   char test[] = "ZDC_monitor";

   geant = new StPeCGeant();
    LOG_INFO << "StPeCMaker Init: before branch add ---------- " << endm;

   //Add branches
    if(!treeOff) {
      uDstTree->Branch("Trigger", "StPeCTrigger", &trigger, 64000, 99);
      uDstTree->Branch("Geant", "StPeCGeant", &geant, 64000, 99);
      LOG_INFO << "StPeCMaker Init: after Geant branch add ---------- " << endm;
      uDstTree->Branch("Event", "StPeCEvent", &pevent, 64000, 99);
      LOG_INFO << "StPeCMaker Init: after Event branch add ---------- " << endm;
    }
   //define 2-D histogram to display snapshots of individual events
   //store then in another directory
   TDirectory * saveDir = gDirectory;
   TDirectory * snapShots = gDirectory->mkdir("snapShots");
   snapShots->cd();
   fSnapShots = new TList();
   Int_t snapLimit = 0;
   for (Int_t i=0;i<snapLimit;i++){

     fSnapShots->Add(new TH2F(Form("snapShot%d",i), Form("y z view of event %d", i) , 100, -250., 250., 100, -200., 200.));
   }
   hNumVtx           = new TH1F("hNumVtx", "number of vertices in event" , 10, -0.5, 9.5);
   hNumAccVtx        = new TH1F("hNumAccVtx", "number of accepted vertices in event" , 10, -0.5, 9.5);
   hNumAccWithTOF    = new TH1F("hNumAccWithTOF", "number of accepted vertices with at least one TOF hit" , 10, -0.5, 9.5);
   hNumRejecWithTOF  = new TH1F("hNumRejecWithTOF", "number of rejected vertices with TOF hits" , 10, -0.5, 9.5);
   hNumTrkRejecVtx   = new TH1F("hNumTrkRejecVtx", "number of tracks in rejected vertex with TOF hits" , 100, 1., 1000.);
   hzVertexAccTOF    = new TH1F("hzVertexAccTOF", "z vertex accepted with TOF  hits" , 100, -200., 200.);
   hzVertexRejectTOF = new TH1F("hzVertexRejectTOF", "z vertex rejected with TOF  hits" , 100, -200., 200.);

   fSnapShots->Add(hNumVtx);
   fSnapShots->Add(hNumAccVtx);
   fSnapShots->Add(hNumAccWithTOF);
   fSnapShots->Add(hNumRejecWithTOF);
   fSnapShots->Add(hNumTrkRejecVtx);
   fSnapShots->Add(hzVertexAccTOF);
   fSnapShots->Add(hzVertexRejectTOF);

   gDirectory = saveDir;



     LOG_INFO << "StPeCMaker leaving Init ---------- " << endm;
     return StMaker::Init();
}


Int_t StPeCMaker::InitRun(Int_t runnr) {
   treeFileName="StPeCMaker.tree.root";
       LOG_INFO << "InitRun StPeCMaker run  "  << runnr<<endm;
   // get TOF geometry to extrapolate tracks to it
   //
     mBTofGeom = new StBTofGeometry("btofGeometry","btofGeometry in MatchMaker");  //commenting this line stops the extrapolation

     geantU  = dynamic_cast<St_geant_Maker *>(GetMaker("myGeant"));

     TVolume * mstarHall = (TVolume *)geantU->GetDataSet("HALL");
 

     if(mBTofGeom && !mBTofGeom->IsInitDone()) {
       LOG_INFO << " BTofGeometry initialization ... " << endm;
       mstarHall->Print();
       mBTofGeom->Init(this, mstarHall);
       LOG_INFO << "starHall defined in StPeCMaker InitRun value of mBTofGeom  "  << mBTofGeom<<endm;
     }
   if ( outputPerRun ) {
      StIOMaker* pIOMaker = (StIOMaker*)GetMaker("IO");
      if (pIOMaker)
      {
         treeFileName = pIOMaker->GetFile();
         char* ccc = "/";
         Ssiz_t slashPosition = treeFileName.Last(*ccc);

         if (slashPosition != -1 && slashPosition < treeFileName.Length()) 
	    treeFileName.Remove(0,slashPosition+1);
      }

   //Make token replacements
      treeFileName.ReplaceAll("dst", "tree");
      treeFileName.ReplaceAll("event", "tree");
      treeFileName.ReplaceAll("evtsel", "treeSel");

      LOG_INFO << "tree output file: " << treeFileName << endm;

   //Get the standard root format to be independent of Star IO   
      m_outfile = new TFile(treeFileName, "recreate");
      m_outfile->SetCompressionLevel(1);
      LOG_INFO << "Initrun: open and compression "  << endm;
   }

     LOG_INFO << "StPeCMaker leaving InitRun ---------- " << endm;



   return StMaker::InitRun(runnr);
}


Int_t StPeCMaker::Make()
{
   StEvent* event = 0;
   // Int_t flag = kStOk;
      LOG_INFO << "StPeCMaker make: just in ---------- "  << endm;

   Int_t NTracks = 0 ;
   int tw        = 0 ;
   int ok = 0 ;
   int returnValue = 0;
   int gReturn = 0;
   //
   // 26-APR 2012 RD we comment the whole muDst part NEEDS TO BE FIXED
   // 12-JULY2012 RD this is still in need of a fix, I return to StMuDst for triggerEfficiency studies
   //
   if(readStMuDst){
    if(!muDst )  LOG_INFO << "StPeCMaker make: muDst not present "  << endm;


//       LOG_INFO << "StPeCMaker make: trigger selected ---------- "  << triggerChoice<< endm;
      NTracks = muDst->globalTracks()->GetEntries();
       
      StL0Trigger &trig = muDst->event()->l0Trigger();
      tw = trig.triggerWord();

      returnValue = trigger->process(muDst, triggerChoice);
//       LOG_INFO << "StPeCMaker make: trigger return ---------- "  <<returnValue<< endm;
      pevent->setTOFgeometry(mBTofGeom);
      ok = pevent->fill(muDst);
      //
      // read MC information from MuDst
   //Fill geant simulations             //RD
//       LOG_INFO << "StPeCMaker make: using muDst to fill Geant +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=---------- "  << endm;
//       gReturn = geant->fill(muDst);
   }   

   if(readStEvent){
     LOG_INFO << "StPeCMaker make: using StEvent---------- "  << endm;  //to read StEvent 18-NOV-2012
     event = (StEvent *)GetInputDS("StEvent");
     if (!event)
       {
	 LOG_ERROR << "There was no StEvent! Exiting..." << endm;
	 return kStOK; 
       }
     //Process StEvent trigger simulations
     returnValue = trigger->process(event, triggerChoice);

     StSPtrVecTrackNode& tempn = event->trackNodes();
     NTracks = tempn.size();

     tw = event->l0Trigger()->triggerWord();                  // end of StEvent 18-NOV
     pevent->setTOFgeometry(mBTofGeom);
     ok = pevent->fill(event);

   }         //rd 9DEC09
   if(readStMuDst_and_StEvent){

     event = (StEvent *)GetInputDS("StEvent");
     if (!event)
       {
	 LOG_ERROR << "There was no StEvent! Exiting..." << endm;
	 return kStOK; 
       }
      LOG_INFO << "StPeCMaker make: using both StMuDst and StEvent ---------- "  << endm;
      NTracks = muDst->globalTracks()->GetEntries();
       
      StL0Trigger &trig = muDst->event()->l0Trigger();
      tw = trig.triggerWord();

      returnValue = trigger->process(muDst, triggerChoice);
      pevent->setTOFgeometry(mBTofGeom);
      LOG_INFO << "StPeCMaker make: using both StMuDst and StEvent  mBTofGeom after set  "  << mBTofGeom<<endm;

      std::vector<int> v = pevent->fill(event, muDst);  
      LOG_INFO << "StPeCMaker make: vector return "<<v.size()<<" first element "<<v.at(0)<<" second el: "<<v.at(1)<<" rejected with TOF hits "<<v.at(2)<<endm;

      hNumAccVtx->Fill(v.at(0));
      hNumAccWithTOF->Fill(v.at(1));
      hNumRejecWithTOF->Fill(v.at(2));
      hNumVtx->Fill(v.at(3));
      hNumTrkRejecVtx->Fill(v.at(4));
      hzVertexAccTOF->Fill(v.at(5));
      hzVertexRejectTOF->Fill(v.at(6));
   }   
   

   LOG_INFO << "ok returnValue " <<ok<<"  "<<returnValue<< endm;


//    if ( !ok && returnValue>0) {    // || geantBranch  ) {  RD 15-SEP  ok=FALSE; good vertex in event returnValue>0; good trigger
   if ( returnValue>0) {    // || geantBranch  ) {  RD 3-JUNE2014 ignore the flag from StPeCEvent to write summary for all events

     LOG_INFO << "Fill Event to Tree!**********************" << endm;
     if(!treeOff)uDstTree->Fill();
   }

     
     //      //Select only 4 prong candidates
     //      //NOTE: This does not appear to do anything because the return code isn't used
     //      if (event)
     //        {
     //        if (filter == 1)
     // 	 flag = Cuts(event, pevent);
     //        else if (filter == 2)
     // 	 flag = Cuts4Prong(event, pevent);
     //        }
//    } else {                                                                   //turn off trigger
//      LOG_INFO << "Do Not fill Event to Tree!**********************" << endm;
//    }           //turn off trigger 
   
   //Cleanup
   //LOG_INFO << "Before clean up!**********************" << endm;
   pevent->clear();
   geant->clear();
   trigger->clear();

   //cout << "Exiting StPeCMaker::Make()" << endl;
   
   
   return kStOk;
}


Int_t StPeCMaker::Cuts(StEvent *event, StPeCEvent *pevent)
{
	StPrimaryVertex* vtx = event->primaryVertex();


	cout << "Entering StPeCMaker::Cuts(StEvent *event, StPeCEvent *pevent)" << endl;

	//Get vertex info
	if (!vtx)
		return kStErr;

	if (vtx->position().x() < -5.)
		return kStErr;
	if (vtx->position().x() >  5.)
		return kStErr;
	if (vtx->position().y() < -5.)
		return kStErr;
	if (vtx->position().y() >  5.)
		return kStErr;

	if (fabs(vtx->position().z()) >  200.)
		return kStErr ;

	//Select interesting events
	if (pevent->getNPriPairs() != 1)
		return kStErr;

	StPeCPair *pair = pevent->getPriPair(0);
	if (pair->getOpeningAngle() > 3.0)
		return kStErr;
	if (pair->getSumCharge())
		return kStErr;



	return kStOK;
}


Int_t StPeCMaker::Cuts4Prong(StEvent *event, StPeCEvent *pevent)
{

	if (pevent->getNTot() != 4)
		return kStErr;
	if (fabs(pevent->getZVertex()) > 200)
		return kStErr;
	if (pevent->getNPriPairs() != 1)
		return kStErr;
	if (pevent->getNSecPairs() != 1)
		return kStErr;
	if (pevent->getPriPair(0)->getSumCharge())
		return kStErr;
	if (pevent->getSecPair(0)->getSumCharge())
		return kStErr;
	if (pevent->getPriPair(0)->getOpeningAngle() > 3)
		return kStErr;
	if (pevent->getSecPair(0)->getOpeningAngle() > 3)
		return kStErr;



	return kStOK;
}


Int_t StPeCMaker::Finish()
{
// 	cout << "Entering StPeCMaker::Finish()" << endl;
	TDirectory * saveDir = gDirectory;
	m_outfile->cd("snapShots");
	fSnapShots->Write();
	gDirectory = saveDir;
	m_outfile->ls();
	m_outfile->Write();
	m_outfile->Close();
	StMaker::Finish();



	return kStOK;
}


