/***************************************************************************
 *
 * $Id: StSsdDbMaker.cxx,v 1.1 2004/03/12 04:56:55 jeromel Exp $
 *
 * Author: cr
 ***************************************************************************
 *
 * Description: SSD DB access Maker
 *
 **************************************************************************/

#include "StSsdDbMaker.h"

#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_ObjectSet.h"
#include "StMessMgr.h"

#include "St_SsdDb_Reader.hh"
#include "St_db_Maker/St_db_Maker.h"
//#include "StDbUtilities/StCoordinates.hh"  
// #include "StDbUtilities/StSsdCoordinateTransform.hh"

#include "StSsdUtil/StSsdGeometry.hh"
#include "StSsdUtil/StSsdEnumerations.hh"
#include "tables/St_svg_geom_Table.h"
StSsdDbMaker* gStSsdDbMaker=NULL; 
St_ObjectSet *ssdSetConfig;
St_ObjectSet *ssdSetPed;
St_ObjectSet *ssdSetGeom;

ClassImp(StSsdDbMaker)
//_____________________________________________________________________________
StSsdDbMaker::StSsdDbMaker(const char *name):StMaker(name)
{
  mTimeStamp = "2003-10-01 00:00:01";

  m_Reader   = NULL;
  gStSsdDbMaker = this;
}

//_____________________________________________________________________________
StSsdDbMaker::~StSsdDbMaker()
{

 gStSsdDbMaker = NULL;
  //delete m_Reader;
}

//_____________________________________________________________________________
Int_t StSsdDbMaker::Init()
{
  St_svg_geom* SsdGeom=0;
  if (Debug()) gMessMgr->Debug() << "StSsdDbMaker::Init" << endm;
  cout << " StSsdDbMaker :: Init "<<endl;


  St_DataSet *svtparams = GetInputDB("svt");
  St_DataSetIter       local(svtparams);
  SsdGeom = (St_svg_geom    *) local("svgpars/geom");

  //svg_geom_st *geom = SsdGeom->GetTable();
  //  cout<<"StSsdDbMaker :: Init ----> GetNRows Of geom Table = "<<SsdGeom->GetNRows()<<endl;
  
  setSsdDb_Reader();
    
  setSsdConfig();
  readSsdConfig();

  setSsdGeometry();
  readSsdGeometry();

  return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StSsdDbMaker::InitRun(int runumber)
{
  if (Debug()) gMessMgr->Debug() << "StSsdDbMaker::InitRun" << endm;
  cout << "StSsdDbMaker::InitRun" << endl;
  readSsdGeometry();
  return kStOk;
}

//_____________________________________________________________________________
void StSsdDbMaker::setSsdDb_Reader()  
{
  if (Debug()) gMessMgr->Debug() << "StSsdDbMaker::set_SsdDb_Reader" << endm;
  m_Reader = new St_SsdDb_Reader();


  St_svg_geom* SsdGeom=0;

  St_DataSet *svtparams = GetInputDB("svt");
  St_DataSetIter       local(svtparams);
  SsdGeom = (St_svg_geom    *) local("svgpars/geom");
  svg_geom_st *geom = SsdGeom->GetTable();

  m_Reader->setSvgGeom(SsdGeom) ;  

  cout << "St_SsdDbReader :  Nombre d'entree = "<<SsdGeom->GetNRows()<<endl;
  
  for (int i = 0; i < SsdGeom->GetNRows() ; i++)
    if (geom[i].id == 7106)
      cout << "St_SsdDbReader :  wafer = "<<geom[i].id<<" Xcoord = "<<geom[i].x[0]<<endl;
    
}

//_____________________________________________________________________________
void StSsdDbMaker::setSsdConfig()
{    
  ssdSetConfig = new St_ObjectSet("StSsdConfig");
  AddConst(ssdSetConfig);  
}

//_____________________________________________________________________________
void StSsdDbMaker::readSsdConfig()
{    
  if (m_Reader)
    ssdSetConfig->SetObject((TObject*)m_Reader->getConfiguration());
}

//_____________________________________________________________________________
void StSsdDbMaker::setSsdPedestals()
{
  ssdSetPed = new St_ObjectSet("StSsdPedestal");
  AddConst(ssdSetPed);  
}

//_____________________________________________________________________________
void StSsdDbMaker::readSsdPedestals()
{
  if (m_Reader)
    ssdSetPed->SetObject((TObject*)m_Reader->getPedestals());
}

//_____________________________________________________________________________
void StSsdDbMaker::setSsdGeometry()
{
  ssdSetGeom = new St_ObjectSet("StSsdGeometry");
  AddConst(ssdSetGeom);  
}

//_____________________________________________________________________________
void StSsdDbMaker::readSsdGeometry()
{
  St_svg_geom* SsdGeom;
  St_DataSet *svtparams = GetInputDB("svt");
  St_DataSetIter       local(svtparams);
  SsdGeom = (St_svg_geom    *) local("svgpars/geom");
  svg_geom_st *geom = SsdGeom->GetTable();
  
  if (m_Reader) 
    {
      ssdSetGeom->SetObject((TObject*)m_Reader->getGeometry(SsdGeom));
      cout<<" readSsdGeometry...... "<<SsdGeom->GetNRows()<<endl;
      for (int i = 0; i < SsdGeom->GetNRows() ; i++)
	{
	  if (geom[i].id == 7102)
	    cout << "St_SsdDbReader :: readSsdGeometry :  Test lecture wafer = " 
		 << geom[i].id   <<" coord = "
		 << geom[i].x[1] << endl;
	    
	}
    }
}

//_____________________________________________________________________________
Int_t StSsdDbMaker::Make()
{
  if (Debug()) gMessMgr->Debug() << "StSsdDbMaker::Make" << endm;

  return kStOK;
}

//_____________________________________________________________________________
void StSsdDbMaker::Clear(const char*)
{
  if (Debug()) gMessMgr->Debug() << "StSsdDaqMaker::Clear" << endm;

  StMaker::Clear();
}

//_____________________________________________________________________________
Int_t StSsdDbMaker::Finish()
{
  if (Debug()) gMessMgr->Debug() << "StSsdDbMaker::Finish" << endm;
  return kStOK;
}
