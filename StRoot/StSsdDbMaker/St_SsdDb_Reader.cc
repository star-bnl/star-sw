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
// void St_SsdDb_Reader::setDataBase(St_DataSet* input, dbSsdType type)
// {
//   if (input)
//     ssdDb[type] = input;    
//   else
//     gMessMgr->Message("Error setting St_SsdDb_Reader: Need to specify input DataSet","E");  
// }
//_____________________________________________________________________________
void St_SsdDb_Reader::setWafersPosition(St_ssdWafersPosition* aWafersPosition)
{
  mWafersPosition=aWafersPosition;
}
void St_SsdDb_Reader::setWafersPosition(ssdWafersPosition_st* geom)
{
  mWP=geom;
}
//_____________________________________________________________________________
//St_ssdWafersPosition* St_SsdDb_Reader::getWafersPosition()
ssdWafersPosition_st* St_SsdDb_Reader::getWafersPosition()
{
  //  return mWafersPosition;
  return mWP;
}
//_____________________________________________________________________________
void St_SsdDb_Reader::setSsdConfiguration(St_ssdConfiguration* aSsdConfiguration)
{
  mSsdConfiguration=aSsdConfiguration;
}
void St_SsdDb_Reader::setSsdConfiguration(ssdConfiguration_st* config)
{
  mSC=config;
}
//_____________________________________________________________________________
//St_ssdConfiguration* St_SsdDb_Reader::getSsdConfiguration()
ssdConfiguration_st* St_SsdDb_Reader::getSsdConfiguration()
{
  //  return mSsdConfiguration;
  return mSC;
}
//_____________________________________________________________________________
void St_SsdDb_Reader::setSsdDimensions(St_ssdDimensions* aSsdDimensions)
{
  mSsdDimensions=aSsdDimensions;
}
void St_SsdDb_Reader::setSsdDimensions(ssdDimensions_st* dimensions)
{
  mSD=dimensions;
}
//_____________________________________________________________________________
//St_ssdDimensions* St_SsdDb_Reader::getSsdDimensions()
ssdDimensions_st* St_SsdDb_Reader::getSsdDimensions()
{
  //  return mSsdDimensions;
  return mSD;
}
//_____________________________________________________________________________
void St_SsdDb_Reader::setDataBase(St_DataSet* input, dbSsdType type)
{
  if (input)
    ssdDb[type] = input;    
  else
    gMessMgr->Message("Error setting St_SsdDb_Reader: Need to specify input DataSet","E");  
}

//___________________________________________________________________________________________
StSsdGeometry* St_SsdDb_Reader::getDimensions(St_ssdDimensions *ssdDimensions)
{

  if (ssdDimensions)
      gMessMgr->Info()<<"_____________getDimensions via Tables ______________"<<endm;
  else
    {
      gMessMgr->Error() << "St_SsdDb_Reader::getDimensions : No  access to dimensions table " << endm;
      return 0; 
    }
  
  ssdDimensions_st *dimensions = ssdDimensions->GetTable();  
  gMessMgr->Info() <<" Half Length of a wafer = " <<dimensions->waferHalfLength<<endm;
  mSsdGeom = new StSsdGeometry();

  mSsdGeom->setWaferLength(2*dimensions->waferHalfLength);
  mSsdGeom->setWaferThickness(2*dimensions->waferHalfThickness);
  mSsdGeom->setWaferWidth(2*dimensions->waferHalfWidth);
    
  gMessMgr->Info() << "____________________________________" << endm;
  gMessMgr->Info() << "St_SsdDb_Reader.......via tables    " << endm;
  gMessMgr->Info() << "SSD : ................waferLength = " << mSsdGeom->getWaferLength()<< endm;
  gMessMgr->Info() << "SSD : .................waferWidth = " << mSsdGeom->getWaferWidth()<< endm;
  gMessMgr->Info() << "SSD : .............waferThickness = " << mSsdGeom->getWaferThickness()<< endm;
  gMessMgr->Info() << "____________________________________" << endm;

  return mSsdGeom;
}

//___________________________________________________________________________________________
StSsdGeometry* St_SsdDb_Reader::getDimensions(ssdDimensions_st *dimensions)
{
  gMessMgr->Info()<<"_____________getDimensions via Databases______________"<<endm;
  
  if (dimensions)
    gMessMgr->Info() <<"St_SsdDb_Reader::getDimensions "<<endm; 
  else
    gMessMgr->Error() << "St_SsdDb_Reader::getDimensions : No  access to dimensions table " << endm;
  

  mSsdGeom = new StSsdGeometry();

  mSsdGeom->setWaferLength(2*dimensions->waferHalfLength);
  mSsdGeom->setWaferThickness(2*dimensions->waferHalfThickness);
  mSsdGeom->setWaferWidth(2*dimensions->waferHalfWidth);
    
  gMessMgr->Info() << "____________________________________" << endm;
  gMessMgr->Info() << "St_SsdDb_Reader via Database        " << endm;
  gMessMgr->Info() << "SSD : ................waferLength = " << mSsdGeom->getWaferLength()<< endm;
  gMessMgr->Info() << "SSD : .................waferWidth = " << mSsdGeom->getWaferWidth()<< endm;
  gMessMgr->Info() << "SSD : .............waferThickness = " << mSsdGeom->getWaferThickness()<< endm;
  gMessMgr->Info() << "____________________________________" << endm;

  return mSsdGeom;
}

//___________________________________________________________________________________
StSsdConfig* St_SsdDb_Reader::getConfiguration(ssdConfiguration_st *config)
{
  gMessMgr->Info()<<"_____________getConfiguration via Databases______________"<<endm;
  //  if (mTimeStamp!="1970-01-01 00:00:00")
  //    mTimeStamp = "1970-01-01 00:00:00";
  if (mTimeStamp!="2004-07-01 00:00:00")
    mTimeStamp = "2004-07-01 00:00:00";
  gMessMgr->Info() << " St_SsdDb_Reader::mTimeStamp Still need to be fixed in getConfiguration" <<endm;
  gMessMgr->Info() << " St_SsdDb_reader::mTimeStamp =  "<<mTimeStamp<<endm;
  
  if(!mSsdConfig)
    mSsdConfig = new StSsdConfig();  

  int totLadderPresent = 0;

  for (int ladder = 1; ladder<=config->nMaxLadders;ladder++) 
    {
      gMessMgr->Info() <<" Run-IV : Ladder = "<< ladder 
	  << " on sector = " << config->ladderIsPresent[ladder-1] 
	  << endm;
      if (config->ladderIsPresent[ladder-1] != 0)
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
  gMessMgr->Info() << "SSD : ...........numberOfSectors = " << config->nMaxSectors << endm;
  gMessMgr->Info() << "SSD : ...........numberOfLadders = " << totLadderPresent << endm;
  gMessMgr->Info() << "SSD : ..numberOfWafersPerLadder  = " << config->nMaxWafers/config->nMaxLadders << endm;
  gMessMgr->Info() << "_____________________________________" << endm;
  return mSsdConfig;
}

//___________________________________________________________________________________________
StSsdConfig* St_SsdDb_Reader::getConfiguration(St_ssdConfiguration *ssdConfiguration)
{

  if (ssdConfiguration)
    gMessMgr->Info()<<"_____________getConfiguration via Tables______________"<<endm;
  else
    {      
      gMessMgr->Error() << "St_SsdDb_Reader::getConfiguration : No  access to configuration table " << endm;
      return 0; 
    }

  int totLadderPresent = 0;
  ssdConfiguration_st *config = ssdConfiguration->GetTable();  
  gMessMgr->Info() <<" Maximum number of ladders = " <<config->nMaxLadders<<endm;
  mSsdConfig = new StSsdConfig();

  for (int ladder = 1; ladder<=config[0].nMaxLadders;ladder++) 
    {
      if (config[0].ladderIsPresent[ladder-1] != 0)
	{
	  gMessMgr->Info()<< "Ladders "<<ladder<<" on sector "<<config[0].ladderIsPresent[ladder-1]<<endm;
	  totLadderPresent++;
	}
      mSsdConfig->setLadderIsActive(ladder,config[0].ladderIsPresent[ladder-1]);
    }   
  gMessMgr->Info() << " Number of ladders in 2004 : "<<totLadderPresent<<endm;  
  mSsdConfig->setNumberOfLadders(totLadderPresent);
  
  
  mSsdConfig->setNumberOfWafers(16);
  mSsdConfig->setNumberOfHybrids(2);
  mSsdConfig->setTotalNumberOfHybrids(2*16*totLadderPresent);
  mSsdConfig->setTotalNumberOfLadders(config->nMaxLadders);
  mSsdConfig->setNumberOfStrips(768);
  mSsdConfig->setConfiguration();
    
  gMessMgr->Info() << "____________________________________" << endm;
  gMessMgr->Info() << "St_SsdDb_Reader...Via  Tables...    " << endm;
  gMessMgr->Info() << "SSD : ........numberOfLadders = " << mSsdConfig->getNumberOfLadders()     << endm;
  gMessMgr->Info() << "SSD : ...totalNumberOfLadders = " << mSsdConfig->getTotalNumberOfLadders()<< endm;
  gMessMgr->Info() << "SSD : .........numberOfWafers = " << mSsdConfig->getNumberOfWafers()      << endm;
  gMessMgr->Info() << "SSD : ........numberOfHybrids = " << mSsdConfig->getNumberOfHybrids()     << endm;
  gMessMgr->Info() << "SSD : ...totalNumberOfHybrids = " << mSsdConfig->getTotalNumberOfHybrids()<< endm;
  gMessMgr->Info() << "SSD : numberOfStripsPerHybrid = " << mSsdConfig->getNumberOfStrips()      << endm;
  gMessMgr->Info() << "____________________________________" << endm;

  return mSsdConfig;
}

//____________________________________________________________________________________________
StSsdGeometry* St_SsdDb_Reader::getGeometry(St_ssdWafersPosition *wafersPosition)
{
  gMessMgr->Info()<<"_____________getGeometry via Tables______________"<<endm; 

  if (wafersPosition != 0) 
    {
      gMessMgr->Info()<<wafersPosition->GetNRows()<< " rows in wafersPosition table" <<endl;
      ssdWafersPosition_st *position = wafersPosition->GetTable();
      
      mSsdGeom = new StSsdGeometry(mSsdConfig);

      int index  = -1;    
      int barrel = 1;

      for (int ladder = 1;ladder <= mSsdGeom->getNumberOfLadders();ladder++) {
 	for (int wafer = 1;wafer <= mSsdGeom->getNumberOfWafers();wafer++) {
	  index++;
	  if (index < 0) continue;	  

	  // fill StSsdGeometry object
	  StSsdWaferGeometry* waferGeom;
	  waferGeom = (StSsdWaferGeometry*)mSsdGeom->at(index);
	  if (!waferGeom)
	    waferGeom = new StSsdWaferGeometry(barrel,ladder,wafer);	    

 	  waferGeom->setDriftDirection(position[index].driftDirection[0],position[index].driftDirection[1],position[index].driftDirection[2]);
	  waferGeom->setNormalDirection(position[index].normalDirection[0],position[index].normalDirection[1],position[index].normalDirection[2]);
	  waferGeom->setTransverseDirection(position[index].transverseDirection[0],position[index].transverseDirection[1],position[index].transverseDirection[2]);
	  waferGeom->setCenterPosition(position[index].centerPosition[0],position[index].centerPosition[1],position[index].centerPosition[2]);	    
	  waferGeom->setID(position[index].id);	    
	  mSsdGeom->put_at(waferGeom,index);
 	} 
      }   
    } 
  else {
    gMessMgr->Info() <<"St_SsdDb_Maker::getGeometry  :: can not find wafersPosition table "<<endm;
    return 0;
  }
  return mSsdGeom;
}

//_____________________________________________________________________________
StSsdGeometry* St_SsdDb_Reader::getGeometry(ssdWafersPosition_st *geom)
{
  gMessMgr->Info() <<"_____________getGeometry via Databases______________"<<endm;
  
  //  mTimeStamp = "1970-01-01 00:00:00";
  mTimeStamp = "2004-07-01 00:00:00";
  gMessMgr->Info() << " St_SsdDb_reader::mTimeStamp Still need to be fixed in getGeometry" <<endm;
  gMessMgr->Info() << " St_SsdDb_reader::mTimeStamp =  "<<mTimeStamp<<endm;
  
  if (!mSsdConfig)
    gMessMgr->Error() << " mSsdConfig Not Defined in getGeometry " << endm;
  
  if (!mSsdGeom)
    mSsdGeom = new StSsdGeometry(mSsdConfig);
  
  StSsdWaferGeometry* waferGeom;
  gMessMgr->Info() <<" getGeometry : numberOfLadders = "<<mSsdGeom->getNumberOfLadders()
		   <<" with numberOfWafersPerLadder  = "<<mSsdGeom->getNumberOfWafers()<<endm;
  int index = -1;
  int barrel = 1;
  float radius;
  for (int ladder = 1;ladder <= mSsdGeom->getNumberOfLadders();ladder++) {
    for (int wafer = 1;wafer <= mSsdGeom->getNumberOfWafers();wafer++) {
      index++;
      if (index < 0) continue;	  
      

      // fill StSsdGeometry object
      waferGeom = (StSsdWaferGeometry*)mSsdGeom->at(index);
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

