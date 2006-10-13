/*
 * $Id: StPixelFastSimMaker.cxx,v 1.4 2006/10/13 20:15:45 fisyak Exp $
 *
 * Author: A. Rose, LBL, Y. Fisyak, BNL, M. Miller, MIT
 *
 * 
 **********************************************************
 * $Log: StPixelFastSimMaker.cxx,v $
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

ClassImp(StPixelFastSimMaker)

  
StPixelFastSimMaker::~StPixelFastSimMaker(){ /*noop*/ }

int StPixelFastSimMaker::Init()
{
  return kStOk;
}
void StPixelFastSimMaker::Clear(Option_t *option){ /*noop*/}
int StPixelFastSimMaker::Finish(){return kStOk;}

Int_t StPixelFastSimMaker::Make()
{

  // Get the input data structures from StEvent and StMcEvent
    StEvent* rcEvent =  (StEvent*) GetInputDS("StEvent");
    if (! rcEvent) {cout << "No StEvent on input" << endl; return kStWarn;}
    StMcEvent* mcEvent = (StMcEvent *) GetInputDS("StMcEvent");
    if (! mcEvent) {cout << "No StMcEvent on input" << endl; return kStWarn;}
    
    // Store hits into RnD Hit Collection until we have our own
    StRnDHitCollection *col = new StRnDHitCollection;
    
    // Don't use realistic hit errors for now. When we transit to smeared
    // hits, this would be a good place to store offset info
    StThreeVectorF mHitError(0.,0.,0.);

    //Get MC Pixel hit collection. This contains all pixel hits.
  const StMcPixelHitCollection* pixHitCol = mcEvent->pixelHitCollection();					  
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
		  StRnDHit* tempHit = new StRnDHit(mcH->position(), 
						   mHitError, 1, 1., 0, 
						   1, 1, id++, kHftId);
		  //cout <<"StPixelFastSimMaker::Make() -I- Pix Hit: "
		  //     <<*tempHit<<endl;
		  tempHit->setDetectorId(kHftId);
		  tempHit->setVolumeId(mcH->volumeId());                   
		  tempHit->setKey(mcH->key());                             
		  StMcPixelHit *mcP=dynamic_cast<StMcPixelHit*>(mcH);     
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
		  StRnDHit* tempHit = new StRnDHit(mcH->position(), mHitError, 1, 1., 0, 1, 1, id++, kIstId);  
		  tempHit->setDetectorId(kIstId); 
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
		  StRnDHit* tempHit = new StRnDHit(mcH->position(), mHitError, 1, 1., 0, 1, 1, id++, kHpdId);  
		  tempHit->setDetectorId(kHpdId); 
		  tempHit->setVolumeId(mcH->volumeId());                   
		  tempHit->setKey(mcH->key());                             
		                                                 
		  StMcHpdHit *mcI = dynamic_cast<StMcHpdHit*>(mcH); 
		  if(mcI){
		    tempHit->setLayer(mcI->layer());           
		    tempHit->setLadder(mcI->ladder());                   
		  }                                                         
		  col->addHit(tempHit);                                 
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








