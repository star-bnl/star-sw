/***************************************************************************
 *
 * $Id: St_SsdDb_Reader.cc,v 1.1 2004/03/12 04:56:55 jeromel Exp $
 *
 * Author: cr
 ***************************************************************************
 *
 * Description: SSD DB access Maker
 *
 **************************************************************************/

#include "St_SsdDb_Reader.hh"

#include "StMessMgr.h"
#include "StSsdUtil/StSsdConfig.hh"
#include "StSsdUtil/StSsdHybridCollection.hh"
#include "StSsdUtil/StSsdGeometry.hh"
#include "StSsdUtil/StSsdWaferGeometry.hh"
// #include "tables/St_ssdConfiguration_Table.h"
// #include "tables/St_ssdWafersPosition_Table.h"
// #include "tables/St_ssdDimensions_Table.h"

// #include "tables/St_ssdPedestals_Table.h"
#include "tables/St_svg_geom_Table.h"


#ifdef __ROOT__
ClassImp(St_SsdDb_Reader)
#endif

//_____________________________________________________________________________
St_SsdDb_Reader::St_SsdDb_Reader()
{
  mSsdConfig = NULL;
  mSsdGeom   = NULL;
  mSvgGeom   = NULL;
  memset(ssdDb,0,sizeof(ssdDb));
}

//_____________________________________________________________________________
St_SsdDb_Reader::~St_SsdDb_Reader()
{
  delete mSsdConfig;
}

//_____________________________________________________________________________
void St_SsdDb_Reader::setSvgGeom(St_svg_geom* aSvgGeom)
{
  mSvgGeom=aSvgGeom;
}
//_____________________________________________________________________________
St_svg_geom* St_SsdDb_Reader::getSvgGeom()
{
  return mSvgGeom;
}
//_____________________________________________________________________________
void St_SsdDb_Reader::setDataBase(St_DataSet* input, dbSsdType type)
{
  if (input)
    ssdDb[type] = input;    
  else
    gMessMgr->Message("Error setting St_SsdDb_Reader: Need to specify input DataSet","E");  
}

//_____________________________________________________________________________
StSsdConfig* St_SsdDb_Reader::getConfiguration()
{
  gMessMgr->Message("St_SsdDb_Reader::getConfiguration");

  //  St_ssdConfiguration *configuration;
  //  const int dbIndex = kGeometry;

  //  if (ssdDb[dbIndex]){
  //     {    
  //       configuration = (St_ssdConfiguration*)ssdDb[dbIndex]->Find("ssdConfiguration");
  //     }
  //     if (!(configuration && configuration->HasData()) ){
  //      gMessMgr->Message("Error Finding SSD Configuration","E");
  //      return 0;
  //     }
  //   }
  //   else {
  gMessMgr->Message("Error Finding SSD Configuration Db","E");

  mSsdConfig = new StSsdConfig();
  mSsdConfig->setNumberOfBarrels(1);
  mSsdConfig->setNumberOfLadders(1,20);
  mSsdConfig->setNumberOfWafers(1,16);
  mSsdConfig->setNumberOfHybrids(2);
  mSsdConfig->setTotalNumberOfHybrids(640);
  mSsdConfig->setNumberOfStrips(768);
  mSsdConfig->setNumberOfTimeBins(128);
  mSsdConfig->setConfiguration();
    
  gMessMgr->Info() << "____________________________________" << endm;
  gMessMgr->Info() << "St_SsdDb_Reader...Hard coding...    " << endm;
  gMessMgr->Info() << "SSD :........ numberOfBarrels = " << mSsdConfig->getNumberOfBarrels() << endm;
  gMessMgr->Info() << "SSD : ........numberOfLadders = " << 
    mSsdConfig->getNumberOfLadders(mSsdConfig->getNumberOfBarrels()) << endm;
  gMessMgr->Info() << "SSD : .........numberOfWafers = " << 
    mSsdConfig->getNumberOfWafers(mSsdConfig->getNumberOfBarrels()) << endm;
  gMessMgr->Info() << "SSD : ........numberOfHybrids = " << mSsdConfig->getNumberOfHybrids() << endm;
  gMessMgr->Info() << "SSD : numberOfStripsPerHybrid = " << mSsdConfig->getNumberOfStrips()  << endm;
  gMessMgr->Info() << "____________________________________" << endm;

  return mSsdConfig;
}

StSsdGeometry* St_SsdDb_Reader::getGeometry(St_svg_geom *SsdGeom)
{
  if (SsdGeom != 0) 
    {
      //      cout<<"St_SsdDb_Reader..GeomTable NRows = "<<SsdGeom->GetNRows()<<endl;
      svg_geom_st *geom = SsdGeom->GetTable();
      
      mSsdGeom = new StSsdGeometry(mSsdConfig);
      mSsdGeom->setBarrelRadius(22.8);
      mSsdGeom->setWaferLength(7.5);
      mSsdGeom->setWaferThickness(0.015);
      mSsdGeom->setWaferWidth(4.2);
      mSsdGeom->setStripPitch(0.095);
      
      cout<<"St_SsdDb_Reader::getGeometry....NumberOfBarrel = "<<mSsdConfig->getNumberOfBarrels()<< endl;
      cout<<"St_SsdDb_Reader::getGeometry......BarrelRadius = "<<mSsdGeom->getBarrelRadius(1)<<endl;

      int i, index;    //index sur detecteurs (320) et non pas sur hybrides comme le SVT
      
      StSsdWaferGeometry* waferGeom;
//       St_ssdWafersPosition *positions;
//       ssdWafersPosition_st *position;

      cout<<"St_SsdDb_Reader::getGeometry......LoadOfWaferPosition........"<<endl;  
      cout<<"_______________________________________________________________________________"<<endl;
      printf("St_SsdDb_Reader:getGeometry --> %d barrel, %d ladders, %d wafers per ladder\n",mSsdGeom->getNumberOfBarrels(),mSsdGeom->getNumberOfLadders(mSsdGeom->getNumberOfBarrels()),mSsdGeom->getNumberOfWafers(mSsdGeom->getNumberOfBarrels()));
      index = -1;

      int barrel =1;
      for (int ladder = 1;ladder <= mSsdGeom->getNumberOfLadders(barrel);ladder++) {
	for (int wafer = 1;wafer <= mSsdGeom->getNumberOfWafers(barrel);wafer++) {
	    
// 	    index++;
// 	    if (index < 0) continue;	  
// 	    int idCurrentWaf = 7000 + (wafer)*100 + (ladder); /*7000 = mSsdLayer*1000*/

	  //  printf("Barrel %d / Ladder %d / Wafer %d \n",barrel,ladder,wafer);
	    
	    /* Old SvtAdaptedMethod
	       index = mSsdGeom->getWaferIndex(barrel,ladder,wafer);
	       obsolete now; indexation has been used for the time being 
	       + switch (barrel)
	       obsolete since SSD = 1 barrel
	    */
	    
	    // get wafers position table
	    
	    // 	  position = positions->GetTable();
	    
	   // fill StSsdGeometry object
	  index++;
	  i = 216 + index;
	  waferGeom = new StSsdWaferGeometry(barrel,ladder,wafer);	    
	  waferGeom->setDriftDirection(geom[i].d[0],geom[i].d[1],geom[i].d[2]);
	  waferGeom->setNormalDirection(geom[i].n[0],geom[i].n[1],geom[i].n[2]);
	  waferGeom->setTransverseDirection(geom[i].t[0],geom[i].t[1],geom[i].t[2]);
	  waferGeom->setCenterPosition(geom[i].x[0],geom[i].x[1],geom[i].x[2]);	    
	  mSsdGeom->put_at(waferGeom,index);
	  //	  cout << "index = "  << index << " i = "<<i<< " idwaf = "<<geom[i].id<<" , x = " << geom[i].x[0] 
	  //      << " radius = "<< sqrt(geom[i].x[0]*geom[i].x[0] + geom[i].x[1]*geom[i].x[1])<<endl;  
	  if (wafer == 1)
	    cout << " idwaf = "<< geom[i].id
		 << "              "
		 << "     x = " << geom[i].x[0] 
		 << "              "
		 << "     y = " << geom[i].x[1]
		 << "              "
		 << "     z = " << geom[i].x[2]
		 << "              "
		 <<endl;  
	} // end of loop over wafers
      } // end of loop over ladders
      
    } 
  return mSsdGeom;
}

//_____________________________________________________________________________
StSsdGeometry* St_SsdDb_Reader::getGeometry()
{
    return NULL;
}

