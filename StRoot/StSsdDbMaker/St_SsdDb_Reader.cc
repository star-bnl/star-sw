/***************************************************************************
 * Author: christelle roy
 * Description: SSD DB access Maker
 **************************************************************************/

#include "St_SsdDb_Reader.hh"

#include "StMessMgr.h"
#include "StSsdUtil/StSsdConfig.hh"
#include "StSsdUtil/StSsdHybridCollection.hh"
#include "StSsdUtil/StSsdGeometry.hh"
#include "StSsdUtil/StSsdWaferGeometry.hh"
#include "tables/St_ssdWafersPosition_Table.h"
#include "tables/St_ssdConfiguration_Table.h"
#include "tables/St_ssdDimensions_Table.h"

#include "StDbLib/StDbManager.hh"                      // Database Libraries
#include "StDbLib/StDbConfigNode.hh"                   //
#include "StDbLib/StDbTable.h"     

#ifdef __ROOT__
ClassImp(St_SsdDb_Reader)
#endif



//_____________________________________________________________________________
St_SsdDb_Reader::St_SsdDb_Reader()
{
  mSsdConfig = NULL;
  mSsdGeom   = NULL;
  memset(ssdDb,0,sizeof(ssdDb));
}

//_____________________________________________________________________________
St_SsdDb_Reader::~St_SsdDb_Reader()
{
  if(mSsdConfig)
    delete mSsdConfig;
  if(mSsdGeom)
    delete mSsdGeom;
}
//_____________________________________________________________________________
void St_SsdDb_Reader::setDataBase(St_DataSet *input, int number)
{

  if (input)
    ssdDb[number] = input;    
  else
    gMessMgr->Message("Error setting St_SsdDb_Reader: Need to specify input DataSet","E");  
}
//___________________________________________________________________________________________
StSsdConfig* St_SsdDb_Reader::getConfiguration()
{
  gMessMgr->Info()<<"_____________getConfiguration via Databases______________"<<endm;

  if(!mSsdConfig)
    mSsdConfig = new StSsdConfig();  

  int totLadderPresent = 0;

  St_ssdConfiguration *configuration ;

  configuration =  (St_ssdConfiguration*)ssdDb[0]->Find("ssdConfiguration");

  if ( ! configuration ){
    gMessMgr->Error() << "St_SsdDb_Reader::getConfiguration() ssdConfiguration -- ssdDb[0] " << ssdDb[0] << " " << endm;    
    return NULL;
  }

  ssdConfiguration_st *config  = (ssdConfiguration_st*) configuration->GetTable() ;

  for (int ladder = 1; ladder<=config->nMaxLadders;ladder++) 
    {
      gMessMgr->Info() <<" Run-IV : Ladder = "<< ladder 
	  << " on sector = " << config->ladderIsPresent[ladder-1] 
	  << endm;
      if(config->ladderIsPresent[ladder-1] != 0)
	 totLadderPresent++;
      mSsdConfig->setLadderIsActive(ladder,config->ladderIsPresent[ladder-1]);
    }   

  gMessMgr->Info() << " Run-IV : totLadderPresent = "<<totLadderPresent<<endm;  

  mSsdConfig->setNumberOfLadders(totLadderPresent);
  mSsdConfig->setNumberOfWafers(config->nMaxWafers/config->nMaxLadders);
  mSsdConfig->setNumberOfHybrids(2);
  mSsdConfig->setTotalNumberOfHybrids(2*16*totLadderPresent);
  mSsdConfig->setTotalNumberOfLadders(config->nMaxLadders);
  mSsdConfig->setNumberOfStrips(768);

  mSsdConfig->setConfiguration();

  gMessMgr->Info() << "_____________________________________" << endm;
  gMessMgr->Info() << "St_SsdDb_Reader...Via  Datababase...." << endm;
  gMessMgr->Info() << "SSD : ...........numberOfSectors =   " << config->nMaxSectors << endm;
  gMessMgr->Info() << "SSD : ...........numberOfLadders =   " << totLadderPresent << endm;
  gMessMgr->Info() << "SSD : ..numberOfWafersPerLadder  =   " << config->nMaxWafers/config->nMaxLadders << endm;
  gMessMgr->Info() << "_____________________________________" << endm;
  return mSsdConfig;
}
//___________________________________________________________________________________________
StSsdGeometry* St_SsdDb_Reader::getGeometry()
{
  gMessMgr->Info() <<"_____________getGeometry via Databases______________"<<endm;

  if (!mSsdGeom)
    mSsdGeom = new StSsdGeometry(mSsdConfig);

  St_ssdWafersPosition *wafersPosition;

  wafersPosition              = (St_ssdWafersPosition*) ssdDb[0]->Find("ssdWafersPosition");

  if ( ! wafersPosition ){
    gMessMgr->Error() << "St_SsdDb_Reader::getGeometry() no ssdWafersPosition -- ssdDb[0] " << ssdDb[0]->Find("ssdWafersPosition") << endm;
    return NULL;
  }

  ssdWafersPosition_st *geom  = (ssdWafersPosition_st*) wafersPosition->GetTable();  

  if (!geom)
    gMessMgr->Error() << " mSsdConfig Not Defined in getGeometry " << endm;
    
  StSsdWaferGeometry* waferGeom;
  gMessMgr->Info() <<" getGeometry : numberOfLadders = "<<mSsdGeom->getNumberOfLadders()
		   <<" with numberOfWafersPerLadder  = "<<mSsdGeom->getNumberOfWafers()<<endm;

  int   index  = -1;
  int   barrel =  1;
  float radius;
  for (int ladder  = 1;ladder <= mSsdGeom->getNumberOfLadders();ladder++) {
    for (int wafer = 1;wafer <= mSsdGeom->getNumberOfWafers();wafer++) {
      index++;
      if (index < 0) continue;	        

      // fill StSsdGeometry object
      waferGeom   = (StSsdWaferGeometry*)mSsdGeom->at(index);
      if (!waferGeom)
	waferGeom = new StSsdWaferGeometry(barrel,ladder,wafer);
      
      waferGeom->setDriftDirection(geom[index].driftDirection[0],geom[index].driftDirection[1],geom[index].driftDirection[2]);
      waferGeom->setNormalDirection(geom[index].normalDirection[0],geom[index].normalDirection[1],geom[index].normalDirection[2]);
      waferGeom->setTransverseDirection(geom[index].transverseDirection[0],geom[index].transverseDirection[1],geom[index].transverseDirection[2]);
      waferGeom->setCenterPosition(geom[index].centerPosition[0],geom[index].centerPosition[1],geom[index].centerPosition[2]);
      radius = sqrt(geom[index].centerPosition[0]*geom[index].centerPosition[0]+
		    geom[index].centerPosition[1]*geom[index].centerPosition[1]);
      waferGeom->setID(geom[index].id);
      mSsdGeom->put_at(waferGeom,index);
      //gMessMgr->Info() << " Wafer : Index = " << index 
      //	       << " Id = " << geom[index].id 
      //	       << " at a radius " << radius 
      //	       << endm; 
    }
  }

  gMessMgr->Info()<<" End of getGeometry______________"<<endm;
  return mSsdGeom;
}

