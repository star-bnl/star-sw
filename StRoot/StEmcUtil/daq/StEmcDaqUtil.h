// GetEmcCollectionFromDaq
// first version A. Suaide, 2001
//
// this utility is used to convert the EMC Daq array into
// StEvent format
//
// usage:
//
//    #include "StEmcUtil/StEmcDaqUtil.h"
//    TDataSet* daq = GetDataSet("StDAQReader");
//    StEmcCollection* emc=GetEmcCollectionFromDaq(daq);
//
//

#include "StEvent.h"
#include "StEventTypes.h"
#include "TDataSet.h"
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/EMC/EMC_Reader.hh"
#include "StDAQMaker/StDAQReader.h"
//--------------------------------------------------------------------
StEmcCollection* GetEmcCollectionFromDaq(TDataSet* daq)
{
  UInt_t eta[]={20,20,150,10};
  UInt_t sub[]={2,2,1,15};
  
  StEmcCollection* emcDaqUtil=new StEmcCollection();
  
  StDAQReader* TheDataReader=(StDAQReader*)(daq->GetObject());
  if(!TheDataReader) return 0;
  if(!TheDataReader->EMCPresent()) return 0;
  
  StEMCReader* TheEmcReader=TheDataReader->getEMCReader();
  if(!TheEmcReader) return 0;
   
  for(Int_t det=1;det<=4;det++)
  {
    StDetectorId id = static_cast<StDetectorId>((det-1)+kBarrelEmcTowerId); 
    StEmcDetector* detector = new StEmcDetector(id,120);
    
    for(UInt_t m=1;m<=120;m++)
      for(UInt_t e=1;e<=eta[det-1];e++)
        for(UInt_t s=1;s<=sub[det-1];s++)
        {
          unsigned short ADC=0;
          if(det==1) if(!TheEmcReader->getTowerADC((int)m,(int)e,(int)s,ADC)) goto next;
          if(det==3) if(!TheEmcReader->getSMDE_ADC((int)m,(int)e,ADC)) goto next; 
          if(det==4) if(!TheEmcReader->getSMDP_ADC((int)m,(int)e,(int)s,ADC)) goto next;
          if(ADC>0)
          {
            StEmcRawHit* hit=new StEmcRawHit(id,m,e,s,(UInt_t)ADC);
            detector->addHit(hit);
          }
          next: continue;
        }
    
    emcDaqUtil->setDetector(detector);
  }
  
  return emcDaqUtil;
}
