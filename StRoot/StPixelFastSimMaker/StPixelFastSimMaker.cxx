/*
 * $Id: StPixelFastSimMaker.cxx,v 1.14 2007/04/06 14:55:11 andrewar Exp $
 *
 * Author: A. Rose, LBL, Y. Fisyak, BNL, M. Miller, MIT
 *
 * 
 **********************************************************
 * $Log: StPixelFastSimMaker.cxx,v $
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
#include "StChain.h"
#include "StEvent.h"
#include "StRnDHit.h"
#include "StMcEvent.hh"
#include "StMcHit.hh"
#include "StMcIstHit.hh"
#include "StMcHpdHit.hh"
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
#include "StBFChain.h"
#include <fstream.h>

ClassImp(StPixelFastSimMaker)

  
StPixelFastSimMaker::~StPixelFastSimMaker(){ /*noop*/ }

int StPixelFastSimMaker::Init()
{
  int seed=time(NULL);
  myRandom=new StRandom();
  myRandom->setSeed(seed);

  // Define various HPD specific geom. variables
  resXHpd = 0.0012;
  resZHpd = 0.0120;
  double raddeg = acos(-1.)/180.0;
  tiltAngleHpd = 60.*raddeg;
  int nLadders = 48;
  rotAngleHpd = 360*raddeg/(double)nLadders;
  offsetAngleHpd = 3.75*raddeg;
  radiusHpd = 9.1;
  double activeLength = 1.36;
  double guardLength = 0.112/2.;
  double pLength = 0.001/2.;
  double cellLength = activeLength+2*guardLength+pLength;
  waferLengthHpd = 5*cellLength;
  ladderWidthHpd = 1.28;
  mSmear=1;
  return kStOk;
}

//____________________________________________________________
int StPixelFastSimMaker::InitRun(int RunNo)
{

  // Define various HPD hit errors from database
  TDataSet *set = GetDataBase("Calibrations/tracker");
  St_HitError *tableSet = (St_HitError *)set->Find("hpdHitError");
  HitError_st* hitError = tableSet->GetTable();
  resXHpd = sqrt(hitError->coeff[0]);
  resZHpd = sqrt(hitError->coeff[3]);
 
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
        printf("StPixelFastSimMaker -E- no RnDHitCollection!\n");
        abort();
      }

    // Don't use realistic hit errors for now. When we transit to smeared
    // hits, this would be a good place to store offset info
    StThreeVectorF mHitError(0.,0.,0.);

    //Get MC Pixel hit collection. This contains all pixel hits.
  const StMcPixelHitCollection* pixHitCol = mcEvent->pixelHitCollection();			     

  bool pileupOut=true;
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
      cout <<"StPixelFastSimMaker::Make() -I- Loaded "
	   <<nhits<<"pixel hits. "<<endl;
    }
  else
    {
      cout <<"No pixel hits found."<<endl;
    }

    const StMcIstHitCollection* istHitCol = mcEvent->istHitCollection();					
    if (istHitCol)							
    {									
      Int_t nhits = istHitCol->numberOfHits();
      if (nhits)								
	{						 			
	  Int_t id = 0;							
	  //StSPtrVecHit *cont = new StSPtrVecHit();				
	  //rcEvent->addHitCollection(cont, # Name );				
	  for (UInt_t k=0; k<istHitCol->numberOfLayers(); k++)		       
	    if (istHitCol->layer(k))						
	      {								
		UInt_t nh = istHitCol->layer(k)->hits().size();	
		for (UInt_t i = 0; i < nh; i++) { 
		  StMcHit *mcH = istHitCol->layer(k)->hits()[i];
		  /*StRnDHit* tempHit = new StRnDHit(mcH->position(), mHitError, 1, 1., 0, 1, 1, id++, kIstId);  
		  tempHit->setDetectorId(kIstId); 
		  tempHit->setVolumeId(mcH->volumeId());                   
		  tempHit->setKey(mcH->key());   */                          
		                                                 
		  //		  char path[100];
		  TString Path("");
		  StMcIstHit *mcI = dynamic_cast<StMcIstHit*>(mcH); 
		  if(((StBFChain *)GetChain())->GetOption("UPGR09",kFALSE)){
		    Path = Form("/HALL_1/CAVE_1/IBMO_1/IBMY_%i/IBAM_%i/IBLM_%i/IBSS_%i",1,mcI->ladder(),mcI->wafer(),mcI->side());
		  }
		  else{
		    if(mcI->layer()==1) Path = Form("/HALL_1/CAVE_1/IBMO_1/IBMY_%i/IBAM_%i/IBLM_%i/IBSS_%i",mcI->layer(),mcI->ladder(),mcI->wafer(),mcI->side());
		    else                Path = Form("HALL_1/CAVE_1/IBMO_1/IBMY:IBM1_%i/IBAM:IBA1_%i/IBLM:IBL1_%i/IBSS:IBS1_%i",mcI->layer(),mcI->ladder(),mcI->wafer(),mcI->side());
		  }
		  gGeoManager->RestoreMasterVolume();
		  gGeoManager->cd(Path);
		  TGeoNode* node=gGeoManager->GetCurrentNode();
		  //MLM cout<<"hit location: "<<mcH->position()<<endl;
		  double pos[3]={mcH->position().x(),mcH->position().y(),mcH->position().z()};
		  double localpos[3]={0,0,0};
		  gGeoManager->GetCurrentMatrix()->MasterToLocal(pos,localpos);
		  if(mcI->layer()==1) localpos[0]=distortHit(localpos[0],resXIst1,100);
		  else localpos[0]=distortHit(localpos[0],resXIst2,100);
		  gGeoManager->GetCurrentMatrix()->LocalToMaster(localpos,pos);
		  if(mcI->layer()==1) pos[2]=distortHit(pos[2],resZIst1,100);
		  else pos[2]=distortHit(pos[2],resZIst2,100);
		  StThreeVectorF smearedpos(pos);
		  //MLM cout<<"smeared hit location: "<<smearedpos<<endl;
		  StRnDHit* tempHit = new StRnDHit(smearedpos, mHitError, 1, 1., 0, 1, 1, id++, kIstId);  
		  tempHit->setDetectorId(kIstId); 
		  tempHit->setVolumeId(mcH->volumeId());                   
		  tempHit->setKey(mcH->key());    
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
     
      cout <<"StPixelFastSimMaker::Make() -I- Loaded Ist  "
	   <<nhits<<"pixel hits. "<<endl;
    }									 
  else
    {
      cout <<"No Ist hits found."<<endl;
    }
    const StMcHpdHitCollection* hpdHitCol = mcEvent->hpdHitCollection();	       			
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
StThreeVectorF StPixelFastSimMaker::global2LocalHpd(const StThreeVectorF& global, int ladder){
  // Local X direction is in r/phi of STAR global coords.
  // Local Z is in STAR's Z direction
  // Local Y is into the thickness of the wafer and ==0
  StThreeVectorF local;

   // Find mid point of detector

  double x0 = radiusHpd*cos(rotAngleHpd*(double)ladder+offsetAngleHpd);
  double y0 = radiusHpd*sin(rotAngleHpd*(double)ladder+offsetAngleHpd);

  double x = global.x()-x0;
  double y = global.y()-y0;


  // Rotate coords by tilt angle
  local.setX(x*cos(tiltAngleHpd) + y*sin(tiltAngleHpd));
  local.setY(y*cos(tiltAngleHpd) - x*sin(tiltAngleHpd));   

  x = local.x();
  y = local.y();
  // Rotate coords by global rotation angle and shift to average radius


   local.setX(x*cos(rotAngleHpd*(double)ladder+offsetAngleHpd) +
  	     y*sin(rotAngleHpd*(double)ladder+offsetAngleHpd));
  
   local.setY(y*cos(rotAngleHpd*(double)ladder+offsetAngleHpd) -
	      x*sin(rotAngleHpd*(double)ladder+offsetAngleHpd));


  // Z stays z
   // local.setZ(global.z()-z0);
   local.setZ(global.z());
   return local;
}

//__________________________________________________________________________
StThreeVectorF StPixelFastSimMaker::local2GlobalHpd(const StThreeVectorF& local, int ladder){
  // Local X direction is in r/phi of STAR global coords.
  // Local Z is in STAR's Z direction
  // Local Y is into the thickness of the wafer and ==0
  StThreeVectorF global;

  // Find mid point of detector
  double x0 = radiusHpd*cos(rotAngleHpd*(double)ladder+offsetAngleHpd);
  double y0 = radiusHpd*sin(rotAngleHpd*(double)ladder+offsetAngleHpd);

  // Rotate coords by global rotation angle and shift to average radius


  global.setX(local.x()*cos(rotAngleHpd*(double)ladder+offsetAngleHpd) -
	      local.y()*sin(rotAngleHpd*(double)ladder+offsetAngleHpd));
  
  global.setY(local.x()*sin(rotAngleHpd*(double)ladder+offsetAngleHpd) +
	      local.y()*cos(rotAngleHpd*(double)ladder+offsetAngleHpd));
  

  // Rotate coords by tilt angle

  double x = global.x();
  double y = global.y();
  global.setX(x*cos(tiltAngleHpd) - y*sin(tiltAngleHpd));
  global.setY(x*sin(tiltAngleHpd) + y*cos(tiltAngleHpd)); 

  global.setX(global.x()+ x0);
  global.setY(global.y()+ y0);

  // Z stays z
  global.setZ(local.z() );
  
  return global;

}


//____________________________________________________________
void StPixelFastSimMaker::smearGaus(StThreeVectorD &mError, 
                                    double sigma1, double sigma2)
{

    // smear hit in transverse plane, 
    // sigma's are in microns
    double u1, u2, v1, v2;
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
  printf("Layer %i ladder %i\n",layer, ladder);
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
   printf("Master volume.\n");
 
  gGeoManager->CdTop();
  gGeoManager->cd(Path);
  TGeoPhysicalNode* node= (TGeoPhysicalNode*)(gGeoManager->GetCurrentNode());
  if (!node ) printf("Failed to get node for %i %i",
		     sector(layer,ladder), secLadder(layer, ladder));
 
  double pos[3]={position.x(),position.y(),position.z()};
  double localpos[3]={0,0,0};
  gGeoManager->GetCurrentMatrix()->MasterToLocal(pos,localpos);
    printf("old hit: %g %g %g\n",localpos[0],localpos[1],localpos[2]);
 
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
      printf("Can't shift hit - no rotation matrix.\n");
    }
 
  StThreeVectorD normalVector(rot[1],rot[4],rot[7]);
  double momentum = mom.magnitude();
  StThreeVectorF momUnit(mom);
  momUnit/=momUnit.magnitude();
 
  //momentum mag, pt and z stay the same; angle changes 
  momUnit.setPhi( momUnit.phi() - phiForLadder(layer,ladder)*3.141592654/180.);
  printf("Mom dir: %g\n",momUnit.phi());
  
  // shift = x + y * tan(phi)
  //.006 is the half thickness of the active area. Hardcoded; not good; blah, blah.
  double dx = (.006)*(momUnit.y()/momUnit.x());
  double dz = (.006)*(momUnit.y()/momUnit.z());
  localpos[0] = localpos[0] - dx;
  localpos[2] = localpos[2] - dz;
  localpos[1] = localpos[1] - .006; //this isn't exactly right. The local position is just off of radius -.006, but it's close (~1-5 um).
   

  printf("new hit: %g %g %g\n",localpos[0],localpos[1],localpos[2]);

  if ( fabs( localpos[0] ) > 2. ) 
    printf("StPixelFastSimMaker::shiftHit -E- shifted hit too far!!!! value=%g\n", localpos[0]);
      
  if ( fabs( localpos[2] ) > 20. ) 
    printf("StPixelFastSimMaker::shiftHit -E- shifted hit too far!!!! value=%g\n", localpos[2]);

  gGeoManager->GetCurrentMatrix()->LocalToMaster(localpos,pos);

  printf("exit shift code.\n");

}
