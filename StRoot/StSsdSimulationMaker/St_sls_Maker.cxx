 /**************************************************************************
 * Class      : St_sls_maker.cxx
 **************************************************************************
 * $Id: St_sls_Maker.cxx,v 1.24 2017/04/26 20:15:49 perev Exp $
 *
 * $Log: St_sls_Maker.cxx,v $
 * Revision 1.24  2017/04/26 20:15:49  perev
 * Hide m_DataSet
 *
 * Revision 1.23  2015/08/06 17:46:53  smirnovd
 * Removed unused local variables
 *
 * Revision 1.22  2013/11/19 14:48:55  jeromel
 * Corrections for %d format use for long int
 *
 * Revision 1.21  2009/01/26 15:00:34  fisyak
 * Take care about sector number packed in volume Id
 *
 * Revision 1.20  2008/08/12 22:48:38  bouchet
 * retrieve positions and dimensions tables using Get methods
 *
 * Revision 1.19  2008/07/17 02:51:15  bouchet
 * initialize StMcSsd hits collections
 *
 * Revision 1.18  2008/07/15 23:08:13  bouchet
 * fix bug (1232) related to a segmentation fault to the SsdHitCollection
 *
 * Revision 1.17  2008/05/29 03:07:27  bouchet
 * remove inactive variables;fix a potential memory leak
 *
 * Revision 1.16  2008/05/07 22:59:11  bouchet
 * EmbeddingMaker:initial version ; modified reading of GEANT hits
 *
 * Revision 1.15  2008/01/15 14:42:51  bouchet
 * Set a default value for uninitialized variables
 *
 * Revision 1.14  2007/04/28 17:56:59  perev
 * Redundant StChain.h removed
 *
 * Revision 1.13  2007/03/21 17:19:56  fisyak
 * use new StSsdBarrel
 *
 * Revision 1.12  2007/01/17 18:14:37  bouchet
 * replace printf, cout statements with LOG statements
 *
 * Revision 1.11  2006/10/16 16:36:08  bouchet
 * Unify classes : Remove StSlsStrip, StSlsPoint, StSpaStrip, StSpaNoise by the same classes used in StSsdPointMaker (StSsdStrip,StSsdPoint) ; The methods for these classes are in StSsdUtil
 *
 * Revision 1.10  2006/09/15 21:09:52  bouchet
 * read the noise and pedestal from ssdStripCalib
 *
 * Revision 1.9  2005/05/13 15:08:58  bouchet
 * reading svt/ssd tables
 *
 * Revision 1.8  2005/05/13 09:28:24  lmartin
 * geant information read from g2t_ssd_hit table
 *
 * Revision 1.7  2005/05/13 08:39:33  lmartin
 * CVS tags added
 *
 * Revision 1.6  2003/10/08 03:46:34  suire
 * *** empty log message ***
 *
 * Revision 1.4  2002/03/25 20:06:43  suire
 * Doxygen documentation, cleaning
 *
 *
 **************************************************************************/
#include <Stiostream.h>
#include <stdlib.h>
#include "St_sls_Maker.h"
#include "TDataSetIter.h"
#include "TFile.h"
#include "StMessMgr.h"

#include "StSsdUtil/StSsdBarrel.hh"
#include "StSsdUtil/StSsdPoint.hh"
#include "StSsdUtil/StSsdPointList.hh"
#include "StSsdUtil/StSsdStrip.hh"
#include "StSsdUtil/StSsdStripList.hh"
#include "tables/St_sls_strip_Table.h"
#include "tables/St_g2t_svt_hit_Table.h"
#include "tables/St_g2t_ssd_hit_Table.h"
#include "tables/St_ssdDimensions_Table.h"
//#include "tables/St_ssdWafersPosition_Table.h"
#include "tables/St_slsCtrl_Table.h"
#include "StSsdUtil/StSsdBarrel.hh"

#include "StBFChain.h"
#include "StChain.h"
#include "StMaker.h"

#include "tables/St_g2t_track_Table.h"
#include "StThreeVectorD.hh"
#include "StPhysicalHelixD.hh"
#include "SystemOfUnits.h"
#include "StarMagField.h"
#include "StMcEvent/StMcSsdHitCollection.hh"
#include "StMcEvent.hh"
#include "StEventTypes.h"
#include "StMcEventTypes.hh"
#include "StDAQMaker/StDAQReader.h"
#include "St_ObjectSet.h"
#include "StThreeVectorF.hh"
#include "StSsdDbMaker/StSsdDbMaker.h"

ClassImp(St_sls_Maker)
St_sls_Maker::St_sls_Maker(const char *name):StMaker(name){
  mHit  = 0;
  m_ctrl = 0;
};
//____________________________________________________________________________
St_sls_Maker::~St_sls_Maker(){ }
//_____________________________________________________________________________
Int_t St_sls_Maker::Init(){
  if (IAttr(".histos")) {
    hRejected = new TH1F("hRejected","hits after removal / hits GEANT",110,0,1.1);
  }
  return kStOk;
}
//_____________________________________________________________________________
Int_t  St_sls_Maker::InitRun(Int_t runNumber) {
  assert(StSsdBarrel::Instance());
  LOG_DEBUG << " instance of barrel ... done " << endm;

  m_ctrl       = gStSsdDbMaker->GetSlsCtrl();
  m_dimensions = 0;
  m_dimensions = gStSsdDbMaker->GetssdDimensions();
  m_positions  = 0;
  m_positions  = gStSsdDbMaker->GetssdWafersPos();
  
  if (!m_ctrl) {
    LOG_ERROR << "No  access to control parameters" << endm;
    return kStFatal;
  }   
  if ((!m_dimensions)||(!m_positions)) {
    LOG_ERROR << "No  access to geometry parameters" << endm;
    return kStFatal;
  }
  if(m_positions){
    LOG_DEBUG << " m_positions found " << endl;
    positions = m_positions->GetTable(); 
    N = m_positions->GetNRows();
    LOG_DEBUG << " size is : " <<N  << endl;
  }
  
  Float_t center[3]={0,0,0}; 
  Float_t B[3]={0,0,0};  
  StarMagField::Instance()->BField(center,B);
  mBField   = B[2]*kilogauss;
  return kStOk;
}
//_____________________________________________________________________________
Int_t St_sls_Maker::Make()
{ 
  if (Debug()==true)  {LOG_DEBUG << "Make() ..." << endm;}
  // 		Create output tables
 
  ssdDimensions_st     *dimensions = m_dimensions->GetTable();
  
  setSsdParameters(dimensions);
  if(Debug()){
    printSsdParameters();}
  
  Int_t res = 0;
  St_sls_strip  *sls_strip = new St_sls_strip("sls_strip",40000);
  AddData(sls_strip);
  
  TDataSetIter geant(GetInputDS("geant"));
  St_g2t_svt_hit *g2t_svt_hit = (St_g2t_svt_hit *) geant("g2t_svt_hit");
  St_g2t_ssd_hit *g2t_ssd_hit = (St_g2t_ssd_hit *) geant("g2t_ssd_hit");
  
  //table is retrieved directly, not the structure
  //slsCtrl_st *ctrl = m_ctrl->GetTable();
  
   LOG_INFO<<"#################################################"<<endm;
   LOG_INFO<<"####       START OF SSD LAZY SIMULATOR       ####"<<endm;
   LOG_INFO<<"####        SSD BARREL INITIALIZATION        ####"<<endm;
   LOG_INFO<<"####        SSD WAFERS INITIALIZATION        ####"<<endm;
   Int_t nSsdHits = 0;
   if (g2t_ssd_hit)
     {
       //nSsdHits = readPointFromTable(g2t_ssd_hit);
       // Get g2t tracks
       St_DataSet *g2t_tracks  =  GetDataSet("g2t_track");
       St_DataSetIter g2t_track_it(g2t_tracks);
       St_g2t_track *g2t_track = (St_g2t_track *) g2t_track_it.Find("g2t_track");
       if(!g2t_track){
	 LOG_WARN <<" no track table , abort event because we cannot procede IdealTorealMethod()" << endm;
	 return kStErr;
       }
       LOG_INFO<<Form("Num of SSD geant hits =%ld",g2t_ssd_hit->GetNRows());
       //nSsdHits = readPointFromTable(g2t_ssd_hit);
       nSsdHits = readPointFromTableWithEmbedding(g2t_ssd_hit,g2t_track,N,positions);
       if(g2t_ssd_hit->GetNRows())hRejected->Fill((float)nSsdHits/g2t_ssd_hit->GetNRows());
     }
   if (nSsdHits == 0)
     {
       if (g2t_svt_hit)
         {
           nSsdHits = readPointFromTable( g2t_svt_hit);
         }
     }    
   LOG_INFO <<"####    ->  "<<nSsdHits<<" HITS READ FROM TABLE        ####"<<endm;
   StSsdBarrel::Instance()->convertGlobalFrameToOther();
   Int_t inactiveHit = 0;
   if (g2t_ssd_hit)
     {
       inactiveHit = removeInactiveHitInTable(g2t_ssd_hit);
     }
   else
     {
       if (g2t_svt_hit)
         {
           inactiveHit = removeInactiveHitInTable(g2t_svt_hit);
         }
     }    
   LOG_INFO<<"####    ->   "<<inactiveHit<<" DEAD ZONE HITS REMOVED      ####"<<endm;
   //chargeSharingOverStrip(ctrl);
   chargeSharingOverStrip(m_ctrl);
   Int_t nSsdStrips = writeStripToTable(sls_strip);
   sls_strip->Purge();
   LOG_INFO<<"####    -> "<<nSsdStrips<<" FIRED STRIPS INTO TABLE     ####"<<endm;
   LOG_INFO<<"####        END OF SSD LAZY SIMULATOR        ####"<<endm;
   LOG_INFO<<"#################################################"<<endm;
   StSsdBarrel::Instance()->Reset();//   delete mySsd;
   if (nSsdStrips) res = kStOK;

   if(res!=kStOK){
     LOG_WARN<<"no output"<<endm;
     return kStWarn;
   }
   if(Debug()==true){ LOG_DEBUG << "Make():end ... "<< endm;}
  return kStOK;
}
//_____________________________________________________________________________
void St_sls_Maker::PrintInfo() {
  if (Debug()==true){ StMaker::PrintInfo();}
}
//_____________________________________________________________________________
Int_t St_sls_Maker::Finish() {
  if (Debug()==true){LOG_DEBUG << "Finish() ... " << endm;}
  return kStOK;
}
//________________________________________________________________________________
Int_t St_sls_Maker::readPointFromTable(St_g2t_ssd_hit *g2t_ssd_hit) {
  g2t_ssd_hit_st *g2t = g2t_ssd_hit->GetTable();

  // LOG_INFO<< "NumberOfRows = " <<  g2t_ssd_hit->GetNRows() << " size " <<  g2t_ssd_hit->GetTableSize() << endm ;
  // g2t_ssd_hit->Print(0,g2t_ssd_hit->GetNRows());

  Int_t minWaf      = StSsdBarrel::Instance()->getSsdLayer()*1000;
  Int_t currWafId   = 0;
  Int_t currWafNumb = 0;
  Int_t currLadder  = 0;
  Int_t counter     = 0;
  Int_t i           = 0 ;
  Int_t j           = 0 ;
  //  Float_t *p        = new float[3];
  Float_t p[3]      = {0,0,0};
  for (i = 0; i < g2t_ssd_hit->GetNRows() ; i++)    {
      currWafId=g2t[i].volume_id%10000;
      if (currWafId > minWaf)	{
	counter++;
	currLadder=StSsdBarrel::Instance()->idWaferToLadderNumb(currWafId);
	currWafNumb=StSsdBarrel::Instance()->idWaferToWafer(currWafId);
	for (j = 0; j<3; j++) {p[j] = g2t[i].p[j];}
	StSsdBarrel::Instance()->mLadders[currLadder]->mWafers[currWafNumb]->
	  addHit(g2t[i].id, g2t[i].id, g2t[i].track_p, g2t[i].x, g2t[i].de, p);
      }
  }
  return counter;
}
//________________________________________________________________________________
Int_t St_sls_Maker::removeInactiveHitInTable(St_g2t_ssd_hit *g2t_ssd_hit) {
  g2t_ssd_hit_st *g2t = g2t_ssd_hit->GetTable();
  StSsdPointList *inactiveHits = StSsdBarrel::Instance()->getInactiveHitList();
  Int_t localSize = 0;
  localSize=inactiveHits->getSize();
  if (localSize)
    {
      Int_t firstSsdPoint=0;
      Int_t iP1 = 0;
      for (iP1 = 0; ((iP1 < g2t_ssd_hit->GetNRows())&&(g2t[iP1].volume_id%10000 < StSsdBarrel::Instance()->getSsdLayer()*1000)) ; iP1++) 
	                                                                                  firstSsdPoint=iP1;
      firstSsdPoint++;
      Int_t isG2tSorted = 1;
      Int_t iP2 = 0;
      for (iP2 = firstSsdPoint+1 ; (iP2 < g2t_ssd_hit->GetNRows())&&(isG2tSorted) ;iP2++)
	{
	  if (g2t[iP2].id < g2t[iP2 - 1].id) isG2tSorted = 0;
	}
      StSsdPoint *currToDele = inactiveHits->first();
      Int_t nDeleted = 0;
      Int_t isAllRemove = 0;
      if (isG2tSorted)
	{
	  Int_t ipScan = 0;
	  Int_t ipKeep = firstSsdPoint;
	  for (ipScan = firstSsdPoint; (ipScan<g2t_ssd_hit->GetNRows()); ipScan++)
	    {
	      if ((!isAllRemove)&&(g2t[ipScan].id == currToDele->getNId()))
		{
		  currToDele=inactiveHits->next(currToDele);
		  if (currToDele == 0) isAllRemove = 1;
		  nDeleted++;
		}
	      else
		{
		  g2t[ipKeep]=g2t[ipScan];
		  ipKeep++;
		}
	    }
	  g2t_ssd_hit->SetNRows( g2t_ssd_hit->GetNRows() - nDeleted );
	}
      else
	{
	  Int_t iLoop = 0;
	  for (iLoop = 0 ; (iLoop < localSize)&&(!isAllRemove); iLoop++)
	    {
	      Int_t ipScan = 0;
	      Int_t nLoopDeleted = 0;
	      for (ipScan = firstSsdPoint; ipScan < g2t_ssd_hit->GetNRows() ; ipScan++)
		{
		  if (!isAllRemove)
		    {
		      if (g2t[ipScan].id == currToDele->getNId())
			{
			  currToDele=inactiveHits->next(currToDele);
			  if (currToDele == 0)
			    {
			      isAllRemove = 1;
			    }
			  else
			    {
			      nDeleted++;
			      nLoopDeleted++;
			    }
			}
		    }
		  g2t[ipScan]=g2t[ipScan + nLoopDeleted];
		}
	      g2t_ssd_hit->SetNRows( g2t_ssd_hit->GetNRows() - nDeleted );
	    }
	}
    }
  StSsdBarrel::Instance()->renumHitAfterRemove();
  delete inactiveHits;
  return localSize;  
}
//________________________________________________________________________________
void St_sls_Maker::chargeSharingOverStrip(slsCtrl_st  *ctrl)
{
  StSsdBarrel::Instance()->convertToStrip(ctrl[0].pairCreationEnergy,
			ctrl[0].nstripInACluster,
			ctrl[0].parDiffP,
			ctrl[0].parDiffN,
			ctrl[0].parIndRightP,
			ctrl[0].parIndRightN,
			ctrl[0].parIndLeftP,
			ctrl[0].parIndLeftN);
}
//________________________________________________________________________________
Int_t St_sls_Maker::writeStripToTable(St_sls_strip *sls_strip) {
  sls_strip_st strip;
  
  Int_t currRecord = 0;
  
  for (Int_t iLad = 0; iLad < StSsdBarrel::Instance()->getNumberOfLadders(); iLad++)
    for (Int_t iWaf = 0; iWaf < StSsdBarrel::Instance()->getNWaferPerLadder(); iWaf++) {
      StSsdWafer *wafer = StSsdBarrel::Instance()->getLadder(iLad)->getWafer(iWaf);
      //Int_t idCurrentWaf = StSsdBarrel::Instance()->waferNumbToIdWafer(iWaf);
      Int_t idCurrentWaf = StSsdBarrel::Instance()->getSsdLayer()*1000 +((iWaf+1)*100)+(iLad+1);
      StSsdStripList *stripP = wafer->getStripP();
      StSsdStripList *stripN = wafer->getStripN();
      StSsdStrip *pStripP = stripP->first();
      Int_t iP = 0;
      for (iP = 0; iP < stripP->getSize() ; iP++) {
	strip.id          = currRecord + 1;
	strip.adc_count   = pStripP->getDigitSig();
	strip.noise_count = 0;
	strip.id_strip    = 10000*(10*pStripP->getNStrip() + 0)+idCurrentWaf;
	strip.id_cluster  = 0;
	strip.N_hits      = pStripP->getNHits();
	strip.de          = pStripP->getAnalogSig();
	for (Int_t i = 0 ; i < 5 ; i++) {
	  strip.id_hit[i]     = pStripP->getIdHit(i);
	  strip.id_mchit[i]   = pStripP->getIdMcHit(i);
	  strip.id_mctrack[i] = pStripP->getIdMcTrack(i);
	  LOG_DEBUG<<Form("idwafer=%d strip_id=%d side P i=%d IdMcHit=%d IdMcTrack=%d signal(adc)=%d signal(GeV)=%f",
			  idCurrentWaf,pStripP->getNStrip(),i,pStripP->getIdMcHit(i),pStripP->getIdMcTrack(i),pStripP->getDigitSig(),pStripP->getAnalogSig())
		   <<endm;
	}
	sls_strip->AddAt(&strip);
	currRecord++;
	pStripP    = stripP->next(pStripP);
      }
      
      StSsdStrip *pStripN = stripN->first();
      Int_t iN = 0;
      for (iN = 0 ; iN < stripN->getSize() ; iN++) {
	strip.id          = currRecord + 1;
	strip.adc_count   = pStripN->getDigitSig();
	strip.noise_count = 0;
	strip.id_strip    = 10000*(10*pStripN->getNStrip() + 1)+idCurrentWaf;
	strip.id_cluster  = 0;
	strip.N_hits      = pStripN->getNHits() ;
	strip.de          = pStripN->getAnalogSig();
	for (Int_t i = 0 ; i < 5 ; i++) {
	  strip.id_hit[i]     = pStripN->getIdHit(i);
	  strip.id_mchit[i]   = pStripN->getIdMcHit(i);
	  strip.id_mctrack[i] = pStripN->getIdMcTrack(i);
	  LOG_DEBUG <<Form("idwafer=%d strip_id=%d side N i=%d IdMcHit=%d IdMcTrack=%d signal(adc)=%d signal(GeV)=%f",
			   idCurrentWaf,pStripN->getNStrip(),i,pStripN->getIdMcHit(i),pStripN->getIdMcTrack(i),pStripN->getDigitSig(),pStripN->getAnalogSig())
		    <<endm;
	}
	sls_strip->AddAt(&strip);
	currRecord++;
	pStripN    = stripN->next(pStripN);
      }
    }
  return currRecord;
}
//_______________________________________________________________

Int_t St_sls_Maker::readPointFromTableWithEmbedding(St_g2t_ssd_hit *g2t_ssd_hit, St_g2t_track *g2t_track,Int_t N,ssdWafersPosition_st *positions){
  StMcEvent* mcEvent = 0;
  mcEvent = (StMcEvent*) GetDataSet("StMcEvent");
 
  StMcSsdHitCollection *mcCol  = 0; 
  StMcSsdHitCollection *mcCol1 = 0; 
  if(mcEvent)
    {
      mcCol  = mcEvent->ssdHitCollection();
      if (!mcCol)  
	{ 
	  LOG_WARN <<"StSsdEmbeddingMaker -E- no SsdHitCollection!" << endm; 
	  mcCol = new StMcSsdHitCollection; 
	  mcEvent->setSsdHitCollection(mcCol); 
	  LOG_WARN <<"Make() has added a non existing StSsdHitCollection" <<endm; 
	}
      else {
	//SafeDelete(mcCol);
	//assert(mcEvent);
	mcCol1 = new StMcSsdHitCollection; 
	mcEvent->setSsdHitCollection(mcCol1);
      }
    }
  
  g2t_ssd_hit_st *g2t             = g2t_ssd_hit->GetTable(); 
  g2t_track_st *g2tTrack          = 0;
  g2tTrack                        = g2t_track->GetTable();
  LOG_DEBUG << "Size of track Table=" << g2t_track->GetNRows()   << endm;
  LOG_DEBUG << "Size of SSD Table="   << g2t_ssd_hit->GetNRows() << endm;
  //ssdWafersPosition_st *positions = m_positions->GetTable(); 
  if(Debug()){
    for(Int_t i =0;i<N;i++)
      {
	for(Int_t ii=0;ii<3;ii++){
	  LOG_INFO <<"Id " << positions[i].id <<" drift["<<ii<<"] ="<< positions[i].driftDirection[ii]<< endm;
	  LOG_INFO <<" Transverse["<<ii<<"] ="<< positions[i].transverseDirection[ii]<< endm;
	  LOG_INFO <<" Id " << positions[i].id <<" normal["<<ii<<"] ="<< positions[i].normalDirection[ii]<< endm; 
	  LOG_INFO <<" center["<<ii<<"] ="<< positions[i].centerPosition[ii]<< endm;  
	}
      }
  }
  StThreeVector<double> VecG(0,0,0);
  StThreeVector<double> VecL(0,0,0);
  StThreeVector<double> mtm(0,0,0);
  Double_t myVectG[3] = {0,0,0};
  Double_t myVectL[3] = {0,0,0};
  Int_t    NumOfHits    = 0;
  Int_t   foundGoodHits = 0;
  Int_t   currWafId     = 0;
  Int_t   currLadder    = 0;
  Int_t   currWafNumb   = 0;
  Int_t   trackId       = 0;
  Int_t   FinalLadder   = 0;
  Int_t   FinalWafer    = 0;
  Int_t   idWafer       = 0;
  Float_t Flag_Energy   = 1.0;
  N         = m_positions->GetNRows();
  NumOfHits = g2t_ssd_hit->GetNRows();
  if (NumOfHits>0){
    for (int j=0;j<NumOfHits ;j++)
      {
	Flag_Energy =1.0;
	currWafId = g2t[j].volume_id%10000;
	trackId   = g2t[j].track_p;
	if( currWafId < mSsdLayer*1000) continue; // ssd hit
	currLadder  = StSsdBarrel::Instance()->idWaferToLadderNumb(currWafId); 
	currWafNumb = StSsdBarrel::Instance()->idWaferToWafer(currWafId); 
	LOG_DEBUG <<"geant hit #"<<j<<" volumeID="<< currWafId << " x=" << g2t[j].x[0] << " y=" << g2t[j].x[1] << " z=" <<  g2t[j].x[2]<<endm;
	VecG.setX(g2t[j].x[0]);
	VecG.setY(g2t[j].x[1]);
	VecG.setZ(g2t[j].x[2]);
	mtm.setX(g2t[j].p[0]);
	mtm.setY(g2t[j].p[1]);
	mtm.setZ(g2t[j].p[2]);
	for (Int_t i = 0; i < N; i++)    // loop over the full wafer table now.
	  {
	    idWafer = positions[i].id;
	    if ((idWafer > mSsdLayer*1000)&&
		(idWafer == currWafId)){ // find the good wafer
	      if(Debug()){
		for(Int_t ii=0;ii<3;ii++){
		  LOG_INFO <<"Id " << idWafer <<" drift["<<ii<<"] ="<< positions[i].driftDirection[ii]<< endm;
		  LOG_INFO <<" Transverse["<<ii<<"] ="<< positions[i].transverseDirection[ii]<< endm;
		  LOG_INFO <<" Id " << idWafer <<" normal["<<ii<<"] ="<< positions[i].normalDirection[ii]<< endm; 
		  LOG_INFO <<" center["<<ii<<"] ="<< positions[i].centerPosition[ii]<< endm;  
		}
	      }
	      LOG_DEBUG << " idWafer =" << idWafer << " TrackId="<<trackId <<" hit id="<<g2t[j].id <<endm; 
	      Int_t iok = ideal2RealTranslation(&VecG,&mtm,(double)g2tTrack[trackId-1].charge,currWafId,i,positions,&FinalLadder,&FinalWafer);
	      if(iok==kStSkip){
		LOG_DEBUG << "Not found correct wafer "<<endm;
		Flag_Energy =-1.0;// we mark the energy as -1*energy for this hit : do not use hit 
	      }
	      // fill stMcEvent
	      g2t[j].de = Flag_Energy*g2t[j].de;
	      //StMcSsdHit *mHit =0;
	      //mHit = new StMcSsdHit(&g2t[j]);
	      Int_t finalVolumeId = 7000+(FinalLadder+1)+(FinalWafer+1)*100;
	      LOG_DEBUG<<Form("New ladder=%d New Wafer=%d New volume id =%d",FinalLadder,FinalWafer,finalVolumeId) << endm;
	      g2t[j].volume_id = 10000*(g2t[j].volume_id/10000) + (long)finalVolumeId; // miss sector 
	      StMcTrack *t = 0;
	      t = new StMcTrack(&(g2tTrack[trackId-1]));
	      //mHit = new StMcSsdHit(g2t[j].x,g2t[j].p,g2t[j].de,0,0,0,finalVolumeId,t);
	      mHit = new StMcSsdHit(&g2t[j]);
	      LOG_DEBUG<<Form("from mHit:ladder=%ld wafer=%ld energy =%f x=%f y=%f z=%f",mHit->ladder(),mHit->wafer(),mHit->dE(),mHit->position().x(),mHit->position().y(),mHit->position().z())<<endm;
	      mHit->setParentTrack(t);
	      //mHit->setVolumeId(finalVolumeId);
	      t->addSsdHit(mHit);
	      mcEvent->ssdHitCollection()->addHit(mHit);
	      LOG_DEBUG<<Form("check :finalVolumeid =%d fromhit ladder=%ld wafer=%ld ParentTrack=%ld",finalVolumeId,mHit->ladder(),mHit->wafer(),mHit->parentTrack()->key())<<endm;
	      if (iok != kStOK) continue;
	      foundGoodHits++;
	      //dump the 3-vector to Double_t[3] for MasterToLocal
	      myVectG[0] = VecG.x();
	      myVectG[1] = VecG.y();
	      myVectG[2] = VecG.z();
	      StSsdBarrel::Instance()->mLadders[currLadder]->mWafers[currWafNumb]->MasterToLocal(myVectG,myVectL);
	      //dump the local Double_t[3] to a 3-vector for the FillGeantHits method 
	      VecL.setX(myVectL[0]);
	      VecL.setY(myVectL[1]);
	      VecL.setZ(myVectL[2]);
	      //########### fill the barrel with proper hits ##########
	      Float_t p[3],tempo[3];
	      for (Int_t jj = 0; jj<3; jj++) {p[jj] = g2t[j].p[jj];tempo[jj] = myVectG[jj];}
	      trackId   = g2t[j].track_p;
	      LOG_DEBUG<<Form("Final Ladder=%d FinalWafer=%d geantId=%d xg=%f yg=%f zg=%f energy=%f trackId=%d",FinalLadder,FinalWafer,g2t[j].id,myVectG[0],myVectG[1],myVectG[2],g2t[j].de,trackId)<<endm;
	      StSsdBarrel::Instance()->mLadders[FinalLadder]->mWafers[FinalWafer]->addHit(g2t[j].id, g2t[j].id, trackId, tempo, g2t[j].de, p);
	    }
	  }
      }
  }
  if(mcEvent){
    LOG_DEBUG << Form("Size of collection =%ld",mcEvent->ssdHitCollection()->numberOfHits())<<endm;
}
  return foundGoodHits;
}

//_______________________________________________________________________
Int_t St_sls_Maker::idWaferToWaferNumb(Int_t idWafer)
{
  // idwafer = layer*1000+waf*100+ladder
  Int_t iW = (int)((idWafer - mSsdLayer*1000)/100);
  Int_t iL = idWafer - mSsdLayer*1000 - iW*100;
  return ((iL-1)*mNWaferPerLadder + iW -1);
}
//________________________________________________________________________
Int_t St_sls_Maker::idWaferToLadderNumb(Int_t idWafer)
{
  // idwafer = layer*1000+waf*100+ladder
  Int_t iW = (int)((idWafer - mSsdLayer*1000)/100);
  Int_t iL = idWafer - mSsdLayer*1000 - iW*100;
  return iL-1;
}
//_________________________________________________________________________
Int_t St_sls_Maker::waferNumbToIdWafer(Int_t waferNumb)
{
  Int_t iL = 1+(int)((waferNumb)/mNLadder);
  Int_t iW = waferNumb-((iL-1)*mNLadder)+1;
  return mSsdLayer*1000 + iW*100 + iL;
}
//__________________________________________________________________________
void St_sls_Maker::setSsdParameters(ssdDimensions_st *geom_par){
  mDimensions          = geom_par;
  mSsdLayer            = 7; 
  mDetectorLargeEdge   = 2.*geom_par[0].waferHalfActLength;
  mDetectorSmallEdge   = 2.*geom_par[0].waferHalfActWidth;
  mNLadder             = 20;
  mNWaferPerLadder     = geom_par[0].wafersPerLadder;
  mNStripPerSide       = geom_par[0].stripPerSide;
  mStripPitch          = geom_par[0].stripPitch;
  mTheta               = geom_par[0].stereoAngle;
}
//___________________________________________________________________________
void St_sls_Maker::printSsdParameters(){
  LOG_INFO << "###Ladders = " <<mNLadder<<"###"<< endm;
  LOG_INFO << "###Wafers per Ladder = " <<mNWaferPerLadder<<"###"<< endm;
  LOG_INFO << "###half Active LargeEdge = " << mDetectorLargeEdge << "###" << endm;
  LOG_INFO << "###half Active SmallEdge = " << mDetectorSmallEdge << "###" << endm;
}
//_____________________________________________________________________________
void St_sls_Maker::debugUnPeu() { 
  Int_t monladder,monwafer; 
  for(monladder=0;monladder<mNLadder;monladder++)
    for(monwafer=0;monwafer<mNWaferPerLadder;monwafer++)
      StSsdBarrel::Instance()->debugUnPeu(monladder,monwafer);
}
//_____________________________________________________________________________
 int St_sls_Maker::IsOnWafer(const StThreeVector<double>& LocalPosition){
   //Find out for a given z coord and Hardware pos is it on the wafer
   if((LocalPosition[0] >(mDetectorLargeEdge/2.)) || (LocalPosition[0] < (-mDetectorLargeEdge/2.)) || 
      ( LocalPosition[1]>(mDetectorSmallEdge/2.)) || (LocalPosition[1] < (-mDetectorSmallEdge/2.)))
     return 0;
   else return 1;
   /*
   if((LocalPosition[0] <(mDetectorLargeEdge/2.)) || (LocalPosition[0] > (-mDetectorLargeEdge/2.)) ||
      (LocalPosition[1] <(mDetectorSmallEdge/2.)) || (LocalPosition[1] > (-mDetectorSmallEdge/2.)))
     return 1;   
   return 0;
   */
 }
//____________________________________________________________________________
Int_t St_sls_Maker::ideal2RealTranslation(StThreeVector<double> *pos, StThreeVector<double> *mtm, double charge, int wafId, int index, ssdWafersPosition_st *positions,Int_t *IL,Int_t *IW){
  StThreeVector<double> x(0,0,0);
  // Get normal and center position of the wafer geom
  StThreeVector<double> wafCent(0,0,0);
  StThreeVector<double> wafNorm(0,0,0);
  wafCent.setX(positions[index].centerPosition[0]);
  wafCent.setY(positions[index].centerPosition[1]);
  wafCent.setZ(positions[index].centerPosition[2]);
  wafNorm.setX(positions[index].normalDirection[0]);
  wafNorm.setY(positions[index].normalDirection[1]);
  wafNorm.setZ(positions[index].normalDirection[2]);
  Int_t Ladder  = idWaferToLadderNumb(wafId);  
  Int_t Wafer   = idWaferToWafer(wafId);  
  // Move helix of track from IDEAL geom to find where it hit REAL wafer geom
  StPhysicalHelixD tHelix( *mtm, *pos, mBField, charge);
  LOG_DEBUG <<  "pos going in : "<< *pos << endm;
  double s = tHelix.pathLength(wafCent,wafNorm);
  x = tHelix.at(s); 
  LOG_DEBUG << "Helix pathLength="<< s <<" x="<<x.x()<<" y="<<x.y()<<" z="<<x.z()<<endm;
  pos->setX(x.x());
  pos->setY(x.y());
  pos->setZ(x.z());
  LOG_DEBUG << "Track projection on Global x =" << pos->x() <<" y =" << pos->y() <<" z =" << pos->z() <<endm; 
  Double_t xg[3] = {0,0,0};
  xg[0] = pos->x();
  xg[1] = pos->y();
  xg[2] = pos->z();
  Double_t xl[3] = {0,0,0};
  // get the local coordinate of the track projection to the real wafer
  StSsdBarrel::Instance()->mLadders[Ladder]->mWafers[Wafer]->MasterToLocal(xg,xl); 
  if (Debug()){
    for(Int_t i=0;i<3;i++){
      LOG_DEBUG << "xg["<<i<<"] = "<<xg[i] << "--> to local --> xl["<<i<<"] = "<<xl[i] <<endm;}
  }
  if(IsOnWafer(xl))
    {
      LOG_DEBUG << " First Pass Coming out " << *pos << endm;
      x = tHelix.momentumAt(s,mBField);
      mtm->setX(x.x());
      mtm->setY(x.y());
      mtm->setZ(x.z());
      *IL = Ladder;
      *IW = Wafer;
      return kStOK;
    }
  LOG_DEBUG <<"not found at the first pass, continue "<<endm; 
  // If the hit is now on a different wafer look for it by looping
  // over one ladder before and one after
  Int_t iladder,theLadder = 0;
  StThreeVector<double> newx(0,0,0);
  for( iladder = -1; iladder <= 1; iladder++){
    theLadder = Ladder + iladder;
    if( theLadder==20) theLadder=0;
    else 
      if( theLadder==-1) theLadder=19;
    for( Int_t iwaf = 0;  iwaf < mNWaferPerLadder; iwaf++){
      LOG_DEBUG << "Wafer ="<< iwaf << " Ladder="<<theLadder << endm;
      wafId = 1000*mSsdLayer + 100*(iwaf+1) + (theLadder+1);
      Int_t NewLadder  = idWaferToLadderNumb(wafId);  
      Int_t NewWafer   = idWaferToWafer(wafId);  
      index = idWaferToWaferNumb(wafId);
      wafCent.setX(positions[index].centerPosition[0]);
      wafCent.setY(positions[index].centerPosition[1]);
      wafCent.setZ(positions[index].centerPosition[2]);
      wafNorm.setX(positions[index].normalDirection[0]);
      wafNorm.setY(positions[index].normalDirection[1]);
      wafNorm.setZ(positions[index].normalDirection[2]);
      double news = tHelix.pathLength(wafCent,wafNorm);
      if(TMath::Abs(news)>5) continue;
      newx = tHelix.at(news); 
      pos->setX(newx.x());
      pos->setY(newx.y());
      pos->setZ(newx.z());
      Double_t XG[3]={0,0,0};
      XG[0] = newx.x();
      XG[1] = newx.y();
      XG[2] = newx.z();
      Double_t XL[3]={0,0,0};
      StSsdBarrel::Instance()->mLadders[NewLadder]->mWafers[NewWafer]->MasterToLocal(XG,XL);  
        if(IsOnWafer(XL))
	{
	  LOG_DEBUG << " after search, found it Coming out " << *pos << endm;
	  newx = tHelix.momentumAt(news,mBField);
	  mtm->setX(newx.x());
	  mtm->setY(newx.y());
	  mtm->setZ(newx.z());
	  *IL = NewLadder;
	  *IW = NewWafer;
	  return kStOk;
	}
      else{LOG_DEBUG <<" Not found wafer for this hit/track projection, will reject the hit"<<endm;}
    }
  }
  return kStSkip;
}
//____________________________________________________________________________
void St_sls_Maker::Clear(Option_t *option)
{
  LOG_DEBUG << "Clear()" <<endm;
  StMaker::Clear();
}
//________________________________________________________________________________
