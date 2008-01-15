 /**************************************************************************
 * Class      : St_sls_maker.cxx
 **************************************************************************
 * $Id: St_sls_Maker.cxx,v 1.15 2008/01/15 14:42:51 bouchet Exp $
 *
 * $Log: St_sls_Maker.cxx,v $
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
#include "tables/St_ssdConfiguration_Table.h"
#include "StSsdUtil/StSsdBarrel.hh"

ClassImp(St_sls_Maker);
//_____________________________________________________________________________
Int_t  St_sls_Maker::InitRun(Int_t runNumber) {
  assert(StSsdBarrel::Instance());
  TDataSet *ssdparams = GetInputDB("Geometry/ssd");
  if (! ssdparams) {
    LOG_ERROR << "No  access to Geometry/ssd parameters" << endm;
    return kStFatal;
  }
  TDataSetIter    local(ssdparams);
  m_ctrl        = (St_slsCtrl           *)local("slsCtrl");
  if (!m_ctrl) {
    LOG_ERROR << "No  access to control parameters" << endm;
    return kStFatal;
  }   
  return kStOK;
}
//_____________________________________________________________________________
Int_t St_sls_Maker::Make()
{ 
  if (Debug()==true)  {LOG_DEBUG << "Make() ..." << endm;}
  // 		Create output tables
   Int_t res = 0;
   St_sls_strip  *sls_strip = new St_sls_strip("sls_strip",40000);
   m_DataSet->Add(sls_strip);

   TDataSetIter geant(GetInputDS("geant"));
   St_g2t_svt_hit *g2t_svt_hit = (St_g2t_svt_hit *) geant("g2t_svt_hit");
   St_g2t_ssd_hit *g2t_ssd_hit = (St_g2t_ssd_hit *) geant("g2t_ssd_hit");

   slsCtrl_st *ctrl = m_ctrl->GetTable();

   LOG_INFO<<"#################################################"<<endm;
   LOG_INFO<<"####       START OF SSD LAZY SIMULATOR       ####"<<endm;
   LOG_INFO<<"####        SSD BARREL INITIALIZATION        ####"<<endm;
   LOG_INFO<<"####        SSD WAFERS INITIALIZATION        ####"<<endm;
   Int_t nSsdHits = 0;
   if (g2t_ssd_hit)
     {
       nSsdHits = readPointFromTable(g2t_ssd_hit);
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
   chargeSharingOverStrip(ctrl);
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
      currWafId=g2t[i].volume_id;
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
      for (iP1 = 0; ((iP1 < g2t_ssd_hit->GetNRows())&&(g2t[iP1].volume_id < StSsdBarrel::Instance()->getSsdLayer()*1000)) ; iP1++) 
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
	}
	sls_strip->AddAt(&strip);
	currRecord++;
	pStripN    = stripN->next(pStripN);
      }
    }
  return currRecord;
}
