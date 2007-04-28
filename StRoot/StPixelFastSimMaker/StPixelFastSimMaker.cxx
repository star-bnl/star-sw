/*
 * $Id: StPixelFastSimMaker.cxx,v 1.26 2007/04/28 17:56:36 perev Exp $
 *
 * Author: A. Rose, LBL, Y. Fisyak, BNL, M. Miller, MIT
 *
 * 
 **********************************************************
 * $Log: StPixelFastSimMaker.cxx,v $
 * Revision 1.26  2007/04/28 17:56:36  perev
 * Redundant StChain.h removed
 *
 * Revision 1.25  2007/04/27 18:41:29  wleight
 * Removed smearing of the coordinate not controlled by the strips in the 17cm layer
 *
 * Revision 1.24  2007/04/27 14:59:10  wleight
 * Corrected another error in the creation of new hits
 *
 * Revision 1.23  2007/04/26 04:08:41  perev
 * Remove StBFChain dependency
 *
 * Revision 1.22  2007/04/25 17:44:59  wleight
 * Corrected error in assignment of reconstructed IST hits
 *
 * Revision 1.21  2007/04/23 18:11:30  andrewar
 * Removed references to Hpd (includes were obsolete)
 *
 * Revision 1.19  2007/04/23 16:32:47  wleight
 * Added explicit casting for double to int in calculating strip number
 *
 * Revision 1.18  2007/04/22 22:57:23  wleight
 * The two hits in the 17 cm layer are no longer combined into 1
 *
 * Revision 1.17  2007/04/16 19:10:52  wleight
 * Added IST simulation (digitization but no clustering)
 *
 * Revision 1.16  2007/04/13 19:17:15  andrewar
 * Removed misleading errors. Changed cout and printf to gMessMgr.
 *
 * Revision 1.15  2007/04/06 21:46:36  andrewar
 * Removed some debug messages.
 *
 * Revision 1.14  2007/04/06 14:55:11  andrewar
 * Shift of HFT hit to face of ladder.
 *
 * Revision 1.13  2007/03/28 13:33:45  mmiller
 * Removed cout/printf's.
 *
 * Revision 1.12  2006/12/21 18:11:59  wleight
 * Fixed UPGR09 compatibility so it works with all versions
 *
 * Revision 1.11  2006/12/20 16:50:21  wleight
 * Added fix for UPGR09 problem with layer number mismatch
 *
 * Revision 1.10  2006/12/15 02:17:20  wleight
 * Ist now gets hit smearing parameters from the database
 *
 * Revision 1.9  2006/12/14 23:52:51  andrewar
 * Added Sevil's hit error db loader.
 *
 * Revision 1.7  2006/11/29 21:42:07  andrewar
 * Update with Pixel resolution smearing.
 *
 * Revision 1.6  2006/11/28 22:37:42  wleight
 * Fixed minor smearing bug
 *
 * Revision 1.4  2006/10/13 20:15:45  fisyak
 * Add Hpd fast simulation (Sevil)
 *
 * Revision 1.3  2006/02/17 21:44:29  andrewar
 * Remover streaming of each Pixel hit.
 *
 * Revision 1.2  2006/02/08 20:57:33  fisyak
 * Set proper Detector Id
 *
 * Revision 1.1  2006/02/03 20:11:56  fisyak
 * The initial revision
 *
 *
 */

#include "Stiostream.h"
#include "StPixelFastSimMaker.h"
#include "StHit.h"
#include "StEventTypes.h"
#include "StEvent.h"
#include "StRnDHit.h"
#include "StMcEvent.hh"
#include "StMcHit.hh"
#include "StMcIstHit.hh"

#include "StMcPixelHit.hh"
#include "StMcEventTypes.hh"
#include "Sti/Base/Factory.h"
#include "Sti/StiPlanarShape.h"
#include "Sti/StiCylindricalShape.h"
#include "Sti/StiMaterial.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiDetector.h"
#include "Sti/StiToolkit.h"
#include "Sti/StiDetectorBuilder.h"
#include <stdio.h>
#include <map>
#include <exception>
using namespace std;
#include <stdexcept>
#include "Sti/StiHitErrorCalculator.h"
#include "Sti/StiIsActiveFunctor.h"
#include "Sti/StiNeverActiveFunctor.h"
#include "Sti/StiElossCalculator.h"
#include "Sti/StiVMCToolKit.h"
#include "StarClassLibrary/StRandom.hh"
#include "tables/St_HitError_Table.h"
#include <fstream>

ClassImp(StPixelFastSimMaker)

  
StPixelFastSimMaker::~StPixelFastSimMaker(){ /*noop*/ }

int StPixelFastSimMaker::Init()
{
  int seed=time(NULL);
  myRandom=new StRandom();
  myRandom->setSeed(seed);

 
  mSmear=1;
  return kStOk;
}

//____________________________________________________________
int StPixelFastSimMaker::InitRun(int RunNo)
{

  // Define various HPD hit errors from database

  TDataSet *set = GetDataBase("Calibrations/tracker");
  St_HitError *ist1TableSet = (St_HitError *)set->Find("ist1HitError");
  St_HitError *ist2TableSet = (St_HitError *)set->Find("ist2HitError");
  HitError_st* ist1HitError = ist1TableSet->GetTable();
  resXIst1 = sqrt(ist1HitError->coeff[0]);
  resZIst1 = sqrt(ist1HitError->coeff[3]);
  HitError_st* ist2HitError = ist2TableSet->GetTable();
  resXIst2 = sqrt(ist2HitError->coeff[0]);
  resZIst2 = sqrt(ist2HitError->coeff[3]);


     bool pileUpOut=false;
     fstream file_op("pileup.dat",ios::in);
     StThreeVectorD * pH=0;
     if(!pileUpOut)
       {
        if(file_op && ! file_op.eof())
	 {
	  double x,y,z,layer,ladder;
	  file_op>>x>>y>>z>>layer>>ladder;
	  if(x!=-999)
	    {
	      pH=new StThreeVectorD(x,y,z);
	      pileupHits.push_back(pH);
	      pair<double, double>* pD = new pair<double,double>(layer,ladder);
	      pileupDet.push_back(pD);
	      
	    }
       	
	 }//end if file op
       }//end if pileup output
    else
      { //try file input
      }




  return kStOk;
}
//____________________________________________________________

void StPixelFastSimMaker::Clear(Option_t *option){ /*noop*/}
//____________________________________________________________
int StPixelFastSimMaker::Finish(){return kStOk;}
//____________________________________________________________
Int_t StPixelFastSimMaker::Make()
{

  // Get the input data structures from StEvent and StMcEvent
    StEvent* rcEvent =  (StEvent*) GetInputDS("StEvent");
    if (! rcEvent) {cout << "No StEvent on input" << endl; return kStWarn;}
    StMcEvent* mcEvent = (StMcEvent *) GetInputDS("StMcEvent");
    if (! mcEvent) {cout << "No StMcEvent on input" << endl; return kStWarn;}
    if (! gGeoManager) GetDataBase("VmcGeometry");

    
    // Store hits into RnD Hit Collection until we have our own
    StRnDHitCollection *col = new StRnDHitCollection;
    if (!col ) 
      {
        gMessMgr->Info()<<"StPixelFastSimMaker -E- no RnDHitCollection!\n";
        abort();
      }

    // Don't use realistic hit errors for now. When we transit to smeared
    // hits, this would be a good place to store offset info
    StThreeVectorF mHitError(0.,0.,0.);

    //Get MC Pixel hit collection. This contains all pixel hits.
  const StMcPixelHitCollection* pixHitCol = mcEvent->pixelHitCollection();			     

  bool pileupOut=false;
  if(pileupOut)
    {
      fstream file_op("pileup.dat",ios::app);
      
      if(file_op)
	{
	    Int_t nhits = pixHitCol->numberOfHits();				
	    if (nhits)								
	      {									
		Int_t id = 0;							
		for (UInt_t k=0; k<pixHitCol->numberOfLayers(); k++)		       
		  if (pixHitCol->layer(k))						
		    {								
		      UInt_t nh = pixHitCol->layer(k)->hits().size();		
		      for (UInt_t i = 0; i < nh; i++) {
		  
			StMcHit *mcH = pixHitCol->layer(k)->hits()[i];
			if (!mcH) continue;

			StThreeVectorD mRndHitError(0.,0.,0.);
			smearGaus(mRndHitError, 8.6, 8.6);
			//8.6 is the design resolution of the detector
			StMcPixelHit *mcP=dynamic_cast<StMcPixelHit*>(mcH);
			file_op<< mcH->position().x() + mRndHitError.x()<<" "
			       <<mcH->position().y() + mRndHitError.y()<<" "
			       <<mcH->position().z() + mRndHitError.z()<<" "
			       <<mcP->layer()<<" "<<mcP->ladder()<<endl;
       		      }                                                           
		    }								 
	      }//end if nhits
	    file_op<<"-999 -999 -999 0. 0."<<endl; //send end of event flag
	    file_op.close();
	}//end if file op
    }//end if pileup output
  else
    { //try file input
    }

  if (pixHitCol)							
    {									
      Int_t nhits = pixHitCol->numberOfHits();				
      if (nhits)								
	{									
	  Int_t id = 0;							
	  for (UInt_t k=0; k<pixHitCol->numberOfLayers(); k++)		       
	    if (pixHitCol->layer(k))						
	      {								
		UInt_t nh = pixHitCol->layer(k)->hits().size();		
		for (UInt_t i = 0; i < nh; i++) {
		  
		  StMcHit *mcH = pixHitCol->layer(k)->hits()[i];
		  if (!mcH) continue;

		  StThreeVectorD mRndHitError(0.,0.,0.);
		  smearGaus(mRndHitError, 8.6, 8.6);

		  StThreeVectorF pos(mcH->position());
		  StMcPixelHit *mcP=dynamic_cast<StMcPixelHit*>(mcH);
		  StThreeVectorF mom(mcH->localMomentum());
		  shiftHit(pos, mom ,mcP->layer(), mcP->ladder());
		  //8.6 is the design resolution of the detector

		  StRnDHit* tempHit = new StRnDHit(mcP->position(), 
						   mRndHitError, 1, 1., 0, 
						   1, 1, id++, kHftId);
		  //cout <<"StPixelFastSimMaker::Make() -I- Pix Hit: "
		  //     <<*tempHit<<endl;
		  tempHit->setDetectorId(kHftId);
		  tempHit->setVolumeId(mcP->volumeId());                   
		  tempHit->setKey(mcP->key());                             
		  //StMcPixelHit *mcP=dynamic_cast<StMcPixelHit*>(mcH);     
		  if(mcP){                                                
		    tempHit->setLayer(mcP->layer());           
		    tempHit->setLadder(mcP->ladder());           
		  }                                                          
		  col->addHit(tempHit);                                 
		}                                                           
	      }								 
	}									 
      gMessMgr->Info() <<"StPixelFastSimMaker::Make() -I- Loaded "
	   <<nhits<<"pixel hits. \n";
    }
  else
    {
      gMessMgr->Info() <<"No pixel hits found.\n";
    }

    const StMcIstHitCollection* istHitCol = mcEvent->istHitCollection();					
  int nLadders[2]={19,27};
  int nWafers[2]={10,13};
  double pitch=.006; //note, all lengths in centimeters unless explicitly noted
  int nStrips=640;
  unsigned int ladderCount;
  unsigned int waferCount;
  double icept;
  double sTotE;
  double pos[3];
  double localpos[3];
  double gpos[3];
  int id=0;
	
  TString PathIn("");
  TString PathOut("");
  if(istHitCol){
    LOG_INFO<<"ist hit collection found"<<endm;
    int nhits=istHitCol->numberOfHits();
    vector<StMcIstHit*> ladderHits;
    multimap<int, int> stripToKey;
    multimap<int, int> strip2ToKey;
    multimap<int, int> strip1ToKey;
    istStrip strips1[640];
    istStrip strips2[640];
    istStrip sStrips[1280];
    if(nhits){
      for(unsigned int i=0;i<2;i++){
	if(istHitCol->layer(i)){
	  ladderCount=1;
	  for(int jj=0;jj<nLadders[i];jj++){
	    LOG_DEBUG<<"now dealing with ladder "<<ladderCount<<endm;
	    waferCount=1;
	    for(unsigned int kk=0;kk<istHitCol->layer(i)->hits().size();kk++){
	      StMcHit *mcH = istHitCol->layer(i)->hits()[kk];
	      StMcIstHit *mcI = dynamic_cast<StMcIstHit*>(mcH); 
	      if(mcI->ladder()==ladderCount) ladderHits.push_back(mcI);
	    }
	    for(int ll=0;ll<nWafers[i];ll++){
	      LOG_DEBUG<<"now dealing with wafer "<<waferCount<<endm;
	      if(i+1==1) PathIn = Form("/HALL_1/CAVE_1/IBMO_1/IBMY_%i/IBAM_%i/IBLM_%i/IBSS_%i",i+1,ladderCount,waferCount,1);
	      else{
		PathIn = Form("HALL_1/CAVE_1/IBMO_1/IBMY:IBM1_%i/IBAM:IBA1_%i/IBLM:IBL1_%i/IBSS:IBS1_%i",i+1,ladderCount,waferCount,1);
		PathOut= Form("HALL_1/CAVE_1/IBMO_1/IBMY:IBM1_%i/IBAM:IBA1_%i/IBLM:IBL1_%i/IBSS:IBS1_%i",i+1,ladderCount,waferCount,2);
	      }
	      for(unsigned int nn=0;nn<ladderHits.size();nn++){
		if(ladderHits[nn]->wafer()==waferCount){
		  StMcIstHit* mcIw=ladderHits[nn];
		  pos[0]=mcIw->position().x();
		  pos[1]=mcIw->position().y();
		  pos[2]=mcIw->position().z();
		  localpos[0]=0;
		  localpos[1]=0;
		  localpos[2]=0;
		  gGeoManager->RestoreMasterVolume();
		  if(mcIw->side()==1){
		    gGeoManager->cd(PathIn);
		    LOG_DEBUG<<"pathIn: "<<PathIn<<endm;
		  }
		  if(mcIw->side()==2){
		    gGeoManager->cd(PathOut);
		    LOG_DEBUG<<"pathOut: "<<PathOut<<endm;
		  }
		  //TGeoNode* node=gGeoManager->GetCurrentNode();
		  gGeoManager->GetCurrentMatrix()->MasterToLocal(pos,localpos);
		  double x=localpos[0];
		  double z=localpos[2];
		  //note that in these local coordinates the strips give good resolution in the x coordinate in layer 1 and layer 2 side 1 and z in layer 2 side 2
		  if(fabs(z)<1.92 && fabs(x)<1.92){
		    if(i==0){
		      LOG_DEBUG<<"layer 1: local x: "<<localpos[0]<<"; local y: "<<localpos[1]<<"; local z: "<<localpos[2]<<endm;
		      stripHit sh;
		      sh.localX=x;
		      sh.e=mcIw->dE();
		      int sindex;
		      sindex=static_cast<int>(x/pitch);
		      sindex=sindex+nStrips/2+1;
		      if(0<sindex && sindex<641){
			if(z<0){
			  sStrips[sindex-1].stripHits.push_back(sh);
			}
			else{
			  sindex=sindex+640;
			  sStrips[sindex-1].stripHits.push_back(sh);
			}
			stripToKey.insert(std::pair<int,int>(sindex,mcIw->key()));
												
		      }
		      else{ LOG_INFO<<"bad strip index! "<<sindex<<endm;}
		      LOG_DEBUG<<"stripHit created with local x value "<<localpos[0]<<" and e value "<<mcIw->dE()<<" and assigned to strip "<<sindex<<endm;
		    }
		    else{
		      LOG_DEBUG<<"layer 2 side "<<mcIw->side()<<"; local x: "<<localpos[0]<<"; local y: "<<localpos[1]<<"; local z: "<<localpos[2]<<endm;
		      if(mcIw->side()==2){
			stripHit sh;
			sh.localX=z;
			sh.e=mcIw->dE();
			int sindex; 
			sindex=static_cast<int>(z/pitch);
			sindex=sindex+nStrips/2+1;
			if(0<sindex && sindex<641){
			  strips2[sindex-1].stripHits.push_back(sh);
			  strip2ToKey.insert(std::pair<int,int>(sindex,mcIw->key()));
			}
			else{ LOG_INFO<<"bad strip index! "<<sindex<<endm;}
			LOG_DEBUG<<"stripHit created with local z value "<<localpos[2]<<" and e value "<<mcIw->dE()<<" and assigned to strip "<<sindex<<endm;
		      }
		      if(mcIw->side()==1){
			stripHit sh;
			sh.localX=x;
			sh.e=mcIw->dE();
			int sindex;
			sindex=static_cast<int>(x/pitch);
			sindex=sindex+nStrips/2+1;
			if(0<sindex && sindex<641){
			  strips1[sindex-1].stripHits.push_back(sh);
			  strip1ToKey.insert(std::pair<int,int>(sindex,mcIw->key()));
			}
			else{ LOG_INFO<<"bad strip index! "<<sindex<<endm;}
			LOG_DEBUG<<"stripHit created with local x value "<<localpos[0]<<" and e value "<<mcIw->dE()<<" and assigned to strip "<<sindex<<endm;
		      }											
		    }
		  }
		}
	      }
	      if(i==0){
		for(unsigned int oo=0;oo<1280;oo++){
		  icept=0;
		  sTotE=0;
		  if(sStrips[oo].stripHits.size()){
		    for(unsigned int pp=0;pp<sStrips[oo].stripHits.size();pp++){
		      icept=icept+sStrips[oo].stripHits[pp].localX*sStrips[oo].stripHits[pp].e;
		      sTotE=sTotE+sStrips[oo].stripHits[pp].e;
		    }
		    sStrips[oo].intercept=icept/sTotE;
		    double smearedX;
		    smearedX=distortHit(sStrips[oo].intercept,pitch/sqrt(12.),100);
		    gGeoManager->RestoreMasterVolume();
		    gGeoManager->cd(PathIn);
		    //TGeoNode* node=gGeoManager->GetCurrentNode();
		    localpos[0]=smearedX;
		    if(oo>639) localpos[2]=distortHit(.96,1.92/sqrt(12.),100);
		    else localpos[2]=distortHit(-.96,.96/sqrt(12.),100);
		    localpos[1]=-.0005;
		    LOG_DEBUG<<"final local x: "<<localpos[0]<<"; final local y: "<<localpos[1]<<" final local z: "<<localpos[2]<<endm;
		    LOG_DEBUG<<"layer ladder wafer: "<<i+1<<" "<<ladderCount<<" "<<waferCount<<endm;
		    LOG_DEBUG<<"path: "<<PathIn<<endm;
		    gpos[0]=0;
		    gpos[1]=0;
		    gpos[2]=0;
		    gGeoManager->GetCurrentMatrix()->LocalToMaster(localpos,gpos);
		    StThreeVectorF gposv(gpos);
		    StRnDHit* tempHit = new StRnDHit(gposv, mHitError, 1, 1., 0, 1, 1, id++, kIstId);  
		    tempHit->setDetectorId(kIstId); 
		    tempHit->setVolumeId(0);
		    multimap<int,int>::iterator iter=stripToKey.find(oo+1);
		    if(iter!=stripToKey.end()){
		      tempHit->setKey((*iter).second);
		      stripToKey.erase(iter);
		    }
		    else tempHit->setKey(99999);
		    tempHit->setLayer(i+1);           
		    tempHit->setLadder(ladderCount);           
		    tempHit->setWafer(waferCount);
		    tempHit->setExtraByte0(1);                                                                
		    col->addHit(tempHit);
		  }
		}
	      }
	      if(i==1){
		for(unsigned int o=0;o<640;o++){
		  icept=0;
		  sTotE=0;
		  if(strips1[o].stripHits.size()){
		    for(unsigned int p=0;p<strips1[o].stripHits.size();p++){
		      icept=icept+strips1[o].stripHits[p].localX*strips1[o].stripHits[p].e;
		      sTotE=sTotE+strips1[o].stripHits[p].e;
		    }
		    strips1[o].intercept=icept/sTotE;
		    gGeoManager->RestoreMasterVolume();
		    gGeoManager->cd(PathIn);
		    //TGeoNode* node=gGeoManager->GetCurrentNode();
		    localpos[0]=distortHit(strips1[o].intercept,pitch/sqrt(12.),100);
		    localpos[2]=0.;
		    localpos[1]=.0005;
		    LOG_DEBUG<<"final local x: "<<localpos[0]<<"; final local y: "<<localpos[1]<<" final local z: "<<localpos[2]<<endm;
		    LOG_DEBUG<<"layer ladder wafer: "<<i+1<<" "<<ladderCount<<" "<<waferCount<<endm;
		    LOG_DEBUG<<"path: "<<PathIn<<endm;
		    gpos[0]=0;
		    gpos[1]=0;
		    gpos[2]=0;
		    gGeoManager->GetCurrentMatrix()->LocalToMaster(localpos,gpos);
		    StThreeVectorF gposv(gpos);
		    StRnDHit* tempHit = new StRnDHit(gposv, mHitError, 1, 1., 0, 1, 1, id++, kIstId);  
		    tempHit->setDetectorId(kIstId); 
		    tempHit->setVolumeId(0);
		    multimap<int,int>::iterator iter=strip1ToKey.find(o+1);
		    if(iter!=strip1ToKey.end()){
		      tempHit->setKey((*iter).second);
		      strip1ToKey.erase(iter);
		    }
		    else tempHit->setKey(99999);
		    tempHit->setLayer(i+1);           
		    tempHit->setLadder(ladderCount);           
		    tempHit->setWafer(waferCount);
		    tempHit->setExtraByte0(1);                                                                
		    col->addHit(tempHit);
		  }
		  icept=0;
		  sTotE=0;
		  if(strips2[o].stripHits.size()){
		    for(unsigned int s=0;s<strips2[o].stripHits.size();s++){
		      icept=icept+strips2[o].stripHits[s].localX*strips2[o].stripHits[s].e;
		      sTotE=sTotE+strips2[o].stripHits[s].e;
		    }
		    strips2[o].intercept=icept/sTotE;
		    gGeoManager->RestoreMasterVolume();
		    gGeoManager->cd(PathOut);
		    //TGeoNode* node=gGeoManager->GetCurrentNode();
		    localpos[0]=0.;
		    localpos[2]=distortHit(strips2[o].intercept,pitch/sqrt(12.),100);
		    localpos[1]=-.0005;
		    LOG_DEBUG<<"final local x: "<<localpos[0]<<"; final local y: "<<localpos[1]<<" final local z: "<<localpos[2]<<endm;
		    LOG_DEBUG<<"layer ladder wafer: "<<i+1<<" "<<ladderCount<<" "<<waferCount<<endm;
		    LOG_DEBUG<<"path: "<<PathOut<<endm;
		    gpos[0]=0;
		    gpos[1]=0;
		    gpos[2]=0;
		    gGeoManager->GetCurrentMatrix()->LocalToMaster(localpos,gpos);
		    StThreeVectorF gposv(gpos);
		    StRnDHit* tempHit2 = new StRnDHit(gposv, mHitError, 1, 1., 0, 1, 1, id++, kIstId);  
		    tempHit2->setDetectorId(kIstId); 
		    tempHit2->setVolumeId(0);
		    multimap<int,int>::iterator iter=strip2ToKey.find(o+1);
		    if(iter!=strip2ToKey.end()){
		      tempHit2->setKey((*iter).second);
		      strip2ToKey.erase(iter);
		    }
		    else tempHit2->setKey(99999);
		    tempHit2->setLayer(i+1);           
		    tempHit2->setLadder(ladderCount);           
		    tempHit2->setWafer(waferCount);
		    tempHit2->setExtraByte0(2);                                                                
		    col->addHit(tempHit2);
		  }
		}
	      }
	      waferCount++;
	      for(unsigned int kl=0;kl<1280;kl++){
		if(kl<640){
		  strips1[kl].stripHits.clear();
		  strips2[kl].stripHits.clear();
		}
		sStrips[kl].stripHits.clear();
	      }
	      stripToKey.clear();
	      strip2ToKey.clear();
	      strip1ToKey.clear();
	    }
	    ladderCount++;
	    ladderHits.clear();
	  }
	}
      }
    }
  }									 
  else
    {
      cout <<"No Ist hits found."<<endl;
    }

    
  /*const StMcHpdHitCollection* hpdHitCol = mcEvent->hpdHitCollection();	       			
    if (hpdHitCol)							
    {									
      Int_t nhits = hpdHitCol->numberOfHits();
      if (nhits)								
	{									
	  Int_t id = 0;							
	 
	  for (UInt_t k=0; k<hpdHitCol->numberOfLayers(); k++){	
	    if (hpdHitCol->layer(k))						
	      {								
		UInt_t nh = hpdHitCol->layer(k)->hits().size();	
		for (UInt_t i = 0; i < nh; i++) { 
		  StMcHit *mcH = hpdHitCol->layer(k)->hits()[i];                          
		  StMcHpdHit *mcI = dynamic_cast<StMcHpdHit*>(mcH); 
		  if(mcI){

		    StThreeVectorF local = global2LocalHpd(mcI->position(), mcI->ladder()-1);
		    local.setX(distortHit(local.x(), resXHpd, ladderWidthHpd));
		    local.setZ(distortHit(local.z(), resZHpd, 2*waferLengthHpd));
		    StThreeVectorF global = local2GlobalHpd(local, mcI->ladder()-1);
    
		    StRnDHit* tempHit = new StRnDHit(global, mHitError, 1, 1., 0, 1, 1, id++, kHpdId);  
		    tempHit->setDetectorId(kHpdId); 
		    tempHit->setVolumeId(mcH->volumeId());                   
		    tempHit->setKey(mcH->key());     
		    tempHit->setLayer(mcI->layer());           
		    tempHit->setLadder(mcI->ladder());             
                                                       
		    col->addHit(tempHit); 
		  }                                
		}                                                           
	      }	
	  }							 
	}									 
     
     cout <<"StPixelFastSimMaker::Make() -I- Loaded Hpd  "
			 <<nhits<<" hpd hits. "<<endl;
    }
  else
    {
      cout <<"No hpd hits found."<<endl;
    }
  */
    
   const StMcIgtHitCollection* igtHitCol= mcEvent->igtHitCollection();					
    
      if (igtHitCol)							
    {									
      Int_t nhits = igtHitCol->numberOfHits();				
      if (nhits)								
	{									
	  Int_t id = 0;							
	  //StSPtrVecHit *cont = new StSPtrVecHit();				
	  //rcEvent->addHitCollection(cont, # Name );				
	  for (UInt_t k=0; k<igtHitCol->numberOfLayers(); k++)		       
	    if (igtHitCol->layer(k))						
	      {								
		UInt_t nh = igtHitCol->layer(k)->hits().size();		
		for (UInt_t i = 0; i < nh; i++) {
		  StMcHit *mcH = igtHitCol->layer(k)->hits()[i];          
		  StRnDHit* tempHit = new StRnDHit(mcH->position(), mHitError, 1, 1., 0, 1, 1, id++);  
		  tempHit->setVolumeId(mcH->volumeId());                   
		  tempHit->setKey(mcH->key());                             
		  StMcPixelHit *mcP=dynamic_cast<StMcPixelHit*>(mcH);     
		  if(mcP){                                                
		    tempHit->setLayer(mcP->layer());           
		    tempHit->setLadder(mcP->ladder());           
		  }                                                  
		  StMcIstHit *mcI = dynamic_cast<StMcIstHit*>(mcH); 
		  if(mcI){                                                
		    tempHit->setLayer(mcI->layer());           
		    tempHit->setLadder(mcI->ladder());           
		    tempHit->setWafer(mcI->wafer());           
		    tempHit->setExtraByte0(mcI->side());         
		  }                                                         
		  col->addHit(tempHit);                                 
		}                                                           
	      }								 
	}									 
    }

     const StMcFstHitCollection* fstHitCol= mcEvent->fstHitCollection();				
 
        if (fstHitCol)							
    {									
      Int_t nhits = fstHitCol->numberOfHits();				
      if (nhits)								
	{									
	  Int_t id = 0;							
	  //StSPtrVecHit *cont = new StSPtrVecHit();				
	  //rcEvent->addHitCollection(cont, # Name );				
	  for (UInt_t k=0; k<fstHitCol->numberOfLayers(); k++)		       
	    if (fstHitCol->layer(k))						
	      {								
		UInt_t nh = fstHitCol->layer(k)->hits().size();		
		for (UInt_t i = 0; i < nh; i++) {
		  StMcHit *mcH = fstHitCol->layer(k)->hits()[i];          
		  StRnDHit* tempHit = new StRnDHit(mcH->position(), mHitError, 1, 1., 0, 1, 1, id++);  
		  tempHit->setVolumeId(mcH->volumeId());                   
		  tempHit->setKey(mcH->key());                             
		                                                 
		  StMcIstHit *mcI = dynamic_cast<StMcIstHit*>(mcH); 
		  if(mcI){                                                
		    tempHit->setLayer(mcI->layer());           
		    tempHit->setLadder(mcI->ladder());           
		    tempHit->setWafer(mcI->wafer());           
		    tempHit->setExtraByte0(mcI->side());         
		  }                                                         
		  col->addHit(tempHit);                                 
		}                                                           
	      }								 
	}									 
    }

    const StMcFgtHitCollection* fgtHitCol = mcEvent->fgtHitCollection();					
	  if (fgtHitCol)							
    {									
      Int_t nhits = fgtHitCol->numberOfHits();				
      if (nhits)								
	{									
	  Int_t id = 0;							
	  //StSPtrVecHit *cont = new StSPtrVecHit();				
	  //rcEvent->addHitCollection(cont, # Name );				
	  for (UInt_t k=0; k<fgtHitCol->numberOfLayers(); k++)		       
	    if (fgtHitCol->layer(k))						
	      {								
		UInt_t nh = fgtHitCol->layer(k)->hits().size();		
		for (UInt_t i = 0; i < nh; i++) {
		  StMcHit *mcH = fgtHitCol->layer(k)->hits()[i];          
		  StRnDHit* tempHit = new StRnDHit(mcH->position(), mHitError, 1, 1., 0, 1, 1, id++);  
		  tempHit->setVolumeId(mcH->volumeId());                   
		  tempHit->setKey(mcH->key());                             
		                                                
		  StMcIstHit *mcI = dynamic_cast<StMcIstHit*>(mcH); 
		  if(mcI){                                                
		    tempHit->setLayer(mcI->layer());           
		    tempHit->setLadder(mcI->ladder());           
		    tempHit->setWafer(mcI->wafer());           
		    tempHit->setExtraByte0(mcI->side());         
		  }                                                         
		  col->addHit(tempHit);                                 
		}                                                           
	      }								 
	}									 
    }		
  
  rcEvent->setRnDHitCollection(col);
  return kStOK;
}
//________________________________________________________________________________
Bool_t StPixelFastSimMaker::accept(StEvent* event){
  return event ? true : false;
}
//________________________________________________________________________________
Bool_t StPixelFastSimMaker::accept(StMcEvent* event){
  return event ? true : false;
}

double StPixelFastSimMaker::distortHit(double x, double res, double detLength){
  double test;
  if(mSmear){
    test = x + myRandom->gauss(0,res);
    while( fabs(x) > detLength){
      test = x + myRandom->gauss(0,res);
    }
    //cout << " x was " <<x<< " and is now " << test<< endl;
    return test;
  }
  else return x;
}
//____________________________________________________________


//____________________________________________________________
void StPixelFastSimMaker::smearGaus(StThreeVectorD &mError, 
                                    double sigma1, double sigma2)
{

    // smear hit in transverse plane, 
    // sigma's are in microns
    double u1=-1;
    double u2=-1.; 
    double v1=-1.;
    double v2=-1.;;
    double r = 2.;
    double z1 = 10.;
    double z2 = 10.;
    while(fabs(z1)>2. || fabs(z2)>2.) // sigma
      {
         r = 2.;
         while(r>1.)
           {
              u1 = rand()/double(RAND_MAX); 
              u2 = rand()/double(RAND_MAX);
              v1 = 2*u1 - 1.;  
              v2 = 2*u2 - 1.; 
              r = pow(v1,2) + pow(v2,2);
           }
           z1 = v1*sqrt(-2.*log(r)/r);  z2 = v2*sqrt(-2.*log(r)/r);
       }

    //set to be cumulative with other transforms 
    mError.setX(mError.x()+z1*sigma1/1.e04);
    mError.setY(mError.y()+z1*sigma1/1.e04);
    mError.setZ(mError.z()+z2*sigma2/1.e04);
}

int StPixelFastSimMaker::sector(int layer, int ladder)
{
 
  if (layer ==1)
    {
      if ( ladder < 4 ) return 1;
      if ( ladder < 7 ) return 2;
      return 3;
    }
  else
    {
      if ( ladder < 9 ) return 1;
      if ( ladder < 18 ) return 2;
      return 3;
    }

  
}

int StPixelFastSimMaker::secLadder(int layer, int ladder)
{
  if (layer ==1 )
      return ladder - 3*(sector(layer, ladder)-1);
  else
    return ladder - 8*(sector(layer,ladder)-1);
}



double StPixelFastSimMaker::phiForLadder(int layer, int ladder)
{
  int sec = sector(layer,ladder);
  int secLad = secLadder(layer,ladder);
  double phi=0.;
  double secPhi=0.;
  double ladPhi=0.;
  switch (sec)
    {
    case 1:
      secPhi=0.;
      break;
    case 2:
      secPhi=120.;
      break;
    case 3:
      secPhi=240.;
      break;
    }

    switch (secLad)
      {
	case 1:
	  ladPhi=100.;
	  break;
	case 2:
	  ladPhi=60.;
	  break;
	case 3:
	  ladPhi=20.;
	  break;
	case 4:
	  ladPhi=105.;
	  break;
	case 5:
	  ladPhi=90.;
	  break;
	case 6:
	  ladPhi=75.;
	  break;
	case 7:
	  ladPhi=60.;
	  break;
	case 8:
	  ladPhi=45.;
	  break;
	case 9:
	  ladPhi=30.;
	  break;
	case 10:
	  ladPhi=15.;
	  break;
	case 11:
	  ladPhi=0.;
	  break;
      }

    return secPhi+ladPhi;
}

 
void StPixelFastSimMaker::shiftHit(StThreeVectorF &position,StThreeVectorF &mom, int layer, int ladder)
{

  //printf("Entering hit shift code. %i %i\n",
  // sector(layer,ladder), secLadder(layer,ladder));

  TString Path("");
  Path = Form("/HALL_1/CAVE_1/PXMO_1/PSEC_%i/PLMO_%i/PLAC_1",
	      sector(layer,ladder),secLadder(layer,ladder));
  gGeoManager->RestoreMasterVolume();
  //printf("Master volume.\n");
 
  gGeoManager->CdTop();
  gGeoManager->cd(Path);
  TGeoPhysicalNode* node= (TGeoPhysicalNode*)(gGeoManager->GetCurrentNode());
  if (!node ) printf("Failed to get node for %i %i",
		     sector(layer,ladder), secLadder(layer, ladder));
 
  double pos[3]={position.x(),position.y(),position.z()};
  double localpos[3]={0,0,0};
  gGeoManager->GetCurrentMatrix()->MasterToLocal(pos,localpos);
  //printf("old hit: %g %g %g\n",localpos[0],localpos[1],localpos[2]);
 
  //get ladder phi 
  //TGeoHMatrix  *hmat   = (TGeoHMatrix*)(node->GetMatrix());
  TGeoHMatrix  *hmat   =  gGeoManager->GetCurrentMatrix();
 
  if (! hmat )
    {
      printf("Can't shift hit - no hmat.\n");
    }
  
  Double_t     *rot    = hmat->GetRotationMatrix();
  if (! rot )
    {
      gMessMgr->Info()<<"Can't shift hit - no rotation matrix.\n";
    }
 
  StThreeVectorD normalVector(rot[1],rot[4],rot[7]);
  double momentum = mom.magnitude();
  StThreeVectorF momUnit(mom);
  momUnit/=momUnit.magnitude();
 
  //momentum mag, pt and z stay the same; angle changes 
  momUnit.setPhi( momUnit.phi() - phiForLadder(layer,ladder)*3.141592654/180.);
  //printf("Mom dir: %g\n",momUnit.phi());
  
  // shift = x + y * tan(phi)
  //.006 is the half thickness of the active area. Hardcoded; not good; blah, blah.
  double dx = (.006)*(momUnit.y()/momUnit.x());
  double dz = (.006)*(momUnit.y()/momUnit.z());
  localpos[0] = localpos[0] - dx;
  localpos[2] = localpos[2] - dz;
  localpos[1] = localpos[1] - .006; //this isn't exactly right. The local position is just off of radius -.006, but it's close (~1-5 um).
   
  gGeoManager->GetCurrentMatrix()->LocalToMaster(localpos,pos);

  //printf("exit shift code.\n");

}
