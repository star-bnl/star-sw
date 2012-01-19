// $Id: StFgtDbMaker.cxx,v 1.12 2012/01/19 22:59:31 rfatemi Exp $
/* \class StFgtDbMaker        
\author Stephen Gliske

*/

#include "StFgtDbMaker.h"
#include "TDataSetIter.h"
#include "StMessMgr.h"
StFgtDbMaker* gStFgtDbMaker=NULL; 

// accessing Eloss cut off table for slow-simu

#include "tables/St_fgtElosCutoff_Table.h"
#include "tables/St_fgtPedestal_Table.h"
#include "tables/St_fgtMapping_Table.h"
#include "tables/St_fgtGain_Table.h"
#include "tables/St_fgtStatus_Table.h"
#include "St_db_Maker/St_db_Maker.h"

ClassImp(StFgtDbMaker)

//_____________________________________________________________________________
StFgtDbMaker::StFgtDbMaker(const char *name)
    : StMaker(name)
{
    gStFgtDbMaker = this;
    m_tables = new StFgtDbImpl();
    m_rmap = 0;
    m_isIdeal = false;
    geom=0;
}

//_____________________________________________________________________________
StFgtDbMaker::~StFgtDbMaker()
{
    if ( m_tables )
	delete m_tables;
}

//-----------------------------------------------------------------------------
//  You should call getDbTables after calling this with "ideal".  If you do
//  not, then your StFgtDb object will be out of date.
void StFgtDbMaker::SetFlavor( const char * flav, const char * tabname )
{
    if ( flav && strcmp( "ideal", flav ) == 0 )
	m_isIdeal = true;
    else
	m_isIdeal = false;

    StMaker::SetFlavor( flav, tabname );
}

//_____________________________________________________________________________
Int_t StFgtDbMaker::Init()
{
    LOG_DEBUG << "StFgtDbMaker::Init()"<<endm;

    return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StFgtDbMaker::InitRun(Int_t runNumber)
{
    LOG_INFO << Form("StFgtDbMaker::InitRun(), run=%d",runNumber)<<endm;

    // clear old pointers to DB tables, just in case. Do not delete the data which are owned by StDbMaker
    mLossTab =0;


    ////start here
    St_fgtElosCutoff *eLossDataset=0;
    St_fgtMapping * mapDataset = 0;
    St_fgtGain * gainDataset = 0;
    St_fgtPedestal * pedDataset = 0;
    St_fgtStatus * statusDataset = 0;
    if ( m_isIdeal )
    {
	if ( m_tables )
	    delete m_tables;
	m_tables = new StFgtDbNaiveImpl();
    }
    else
    {
	if ( m_tables && dynamic_cast< StFgtDbNaiveImpl * >(m_tables) )
	{
	    delete m_tables;
	    m_tables = 0;
	}
	if ( !m_tables )
	{
	    m_tables = new StFgtDbImpl();
	}

	LOG_INFO <<  "::RequestDataBase() for Elos cut off ..."<< endl;    
	TDataSet *ELossDB = GetDataBase("Calibrations/fgt/fgtElosCutoff");
	if (!ELossDB)
	{
	  LOG_FATAL
	    << "ERROR: no table found in db for eloss, or malformed local db config" 
	    << endm;
	  assert(10==1);
	}


	LOG_INFO <<  "::RequestDataBase() for calibration tables..."<< endl;  
	TDataSet *MapDB = GetDataBase("Calibrations/fgt/fgtMapping");
	if (!MapDB)
	{
	    LOG_FATAL
		<< "ERROR: no table found in db for map, or malformed local db config" 
		<< endm;
	    assert(13==1);
	}
	TDataSet *PedDB = GetDataBase("Calibrations/fgt/fgtPedestal");
	if (!PedDB)
	{
	    LOG_FATAL
		<< "ERROR: no table found in db for pedestals, or malformed local db config" 
		<< endm;
	    assert(14==1);
	}
	TDataSet *StatusDB = GetDataBase("Calibrations/fgt/fgtStatus");
	if (!StatusDB)
	{
	    LOG_FATAL
		<< "ERROR: no table found in db for pedestals, or malformed local db config" 
		<< endm;
	    assert(15==1);
	}
	TDataSet *GainDB = GetDataBase("Calibrations/fgt/fgtGain");
	if (!GainDB)
	{
	    LOG_FATAL
		<< "ERROR: no table found in db for pedestals, or malformed local db config" 
		<< endm;
	    assert(16==1);
	}


	eLossDataset=(St_fgtElosCutoff*)ELossDB->Find("fgtElosCutoff"); assert(eLossDataset);
	Int_t rows = eLossDataset->GetNRows();
	if (rows > 1) 
	  {
	    LOG_FATAL   << " found INDEXED table with " << rows
		    << " rows, this is fatal, fix DB content" << endm;
	    assert(11==1);
	  }


	mapDataset = (St_fgtMapping *)
	  MapDB->Find("fgtMapping"); assert(mapDataset);
	rows = mapDataset->GetNRows();
	if (rows > 1) 
	{
	    LOG_FATAL   << " found INDEXED table for mapping with " << rows
			<< " rows, this is fatal, fix DB content" << endm;
	    assert(17==1);
	}
	gainDataset = (St_fgtGain*)
	    GainDB->Find("fgtGain"); assert(gainDataset);
	rows = gainDataset->GetNRows();
	if (rows > 1) 
	{
	    LOG_FATAL   << " found INDEXED table for gains with " << rows
			<< " rows, this is fatal, fix DB content" << endm;
	    assert(18==1);
	}
	pedDataset = (St_fgtPedestal*)
	    PedDB->Find("fgtPedestal"); assert(pedDataset);
	rows = pedDataset->GetNRows();
	if (rows > 1) 
	{
	    LOG_FATAL   << " found INDEXED table for pedestals with " << rows
			<< " rows, this is fatal, fix DB content" << endm;
	    assert(19==1);
	}
	statusDataset = (St_fgtStatus*)
	    StatusDB->Find("fgtStatus"); assert(statusDataset);
	rows = statusDataset->GetNRows();
	if (rows > 1) 
	{
	    LOG_FATAL   << " found INDEXED table for status with " << rows
			<< " rows, this is fatal, fix DB content" << endm;
	    assert(20==1);
	}
    }
  
    if (pedDataset && statusDataset && mapDataset && gainDataset && eLossDataset)
    {
	if ( m_rmap )
	    delete m_rmap;
	m_rmap = new fgtMapping_st();

	fgtMapping_st * map = mapDataset->GetTable();

	for ( int ii = 0; ii < 51200; ++ii )
	{
	  if (map->Mapping[ii]>=0)
	    m_rmap->Mapping[ map->Mapping[ii] ] = ii;
	  
	}

	dynamic_cast< StFgtDbImpl * >(m_tables)->updateTables(
	    map, m_rmap,
	    statusDataset->GetTable(),
	    pedDataset->GetTable(),
	    gainDataset->GetTable(),
	    eLossDataset->GetTable()
	);

	LOG_INFO
	    <<
		Form(
		    "%s :: map, gain, pedestal and status tables received, comments",
		    GetName()
		)
	    << endm;
 
	#if 0 // disable it later 
	    for (int i = 0; i < 10000; i++) { 
            std::cout << i <<"tmp eLoss val: " << mLossTab[0].cutoff[i] 
		      << std::endl;
	    if(i>15) break;    }
	#endif   
    }
    else if (!m_isIdeal)
    {
	if ( !pedDataset )
	{
	    LOG_FATAL << Form("%s :: pedestal table failed,",GetName())
		      << endm;
	}
	if ( !mapDataset )
	{
	    LOG_FATAL << Form("%s :: map table failed,",GetName())
		      << endm;
	}
	if ( !gainDataset )
	{
	    LOG_FATAL << Form("%s :: gain table failed,",GetName())
		      << endm;
	}
	if ( !statusDataset )
	{
	    LOG_FATAL << Form("%s :: status table failed,",GetName())
		      << endm;
	}
	if ( !eLossDataset )
	{
	    LOG_FATAL << Form("%s :: eLoss table failed,",GetName())
		      << endm;
	}

	assert(21==1);
    }

    //    geom=new StFgtGeom();   //	for now it is static, but later may be time
			    //	dependent
    return kStOK;
}
//_____________________________________________________________________________
Int_t StFgtDbMaker::Make()
{
    LOG_DEBUG << "Make" << endm;

    return kStOK;
}

//_____________________________________________________________________________
void StFgtDbMaker::Clear(const char*)
{
    LOG_DEBUG << "Clear" << endm;
    StMaker::Clear();
}

//_____________________________________________________________________________
Int_t StFgtDbMaker::Finish()
{
    LOG_DEBUG << "Finish" << endm;
    return kStOK;
}

//_____________________________________________________________________________


// $Log: StFgtDbMaker.cxx,v $
// Revision 1.12  2012/01/19 22:59:31  rfatemi
// Don't read negative DB numbers into reverse maps
//
// Revision 1.11  2012/01/18 17:24:55  sgliske
// fixed bug on line 225 that made ideal flavor always fail
//
// Revision 1.10  2011/12/02 03:53:18  avossen
// fixed eLoss db tables
//
// Revision 1.9  2011/11/14 02:31:15  wwitzke
// Fixed bug with where getTables() can be called.
//
// Revision 1.8  2011/11/14 02:17:26  wwitzke
// Update to fix seg fault. Again.
//
// Revision 1.7  2011/11/14 01:49:33  wwitzke
// Fixed seg fault bug.
//
// Revision 1.5  2011/11/13 23:51:49  wwitzke
// Modified StFgtDbMaker to pull calibration data from the database.
//
// Revision 1.4  2011/10/26 19:32:34  balewski
// now fgt-geom is owned by fgtDb-maker
//
// Revision 1.3  2011/10/06 19:03:58  balewski
// access Elos table from STAR DB
//
// Revision 1.2  2011/10/04 02:59:34  balewski
// added guestimates of gains, grid absorption, charge sharing
//
