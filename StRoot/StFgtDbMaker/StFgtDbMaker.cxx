// $Id: StFgtDbMaker.cxx,v 1.6 2011/11/14 01:18:00 wwitzke Exp $
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
    m_tables = 0;
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

    LOG_INFO <<  "::RequestDataBase() for Elos cut off ..."<< endl;  
    TDataSet *DB = GetDataBase("Calibrations/fgt/fgtElosCutoff");
    if (!DB)
    {
	LOG_FATAL
	    << "ERROR: no table found in db, or malformed local db config" 
	    << endm;
	assert(10==1);
    }

    St_fgtElosCutoff *eLosDataset= (St_fgtElosCutoff*)
	DB->Find("fgtElosCutoff"); assert(eLosDataset);
    Int_t rows = eLosDataset->GetNRows();
    if (rows > 1) 
    {
	LOG_FATAL   << " found INDEXED table with " << rows
		    << " rows, this is fatal, fix DB content" << endm;
	assert(11==1);
    }

    if (eLosDataset)
    {
	mLossTab = eLosDataset->GetTable(); assert(mLossTab);
	LOG_INFO
	    <<
		Form(
		    "%s :: fgtElosCutoff table received, comment=%s",
		    GetName(),mLossTab[0].comment
		)
	    << endm;
 
	#if 0 // disable it later 
	    for (int i = 0; i < 10000; i++) { 
            std::cout << i <<"tmp eLoss val: " << mLossTab[0].cutoff[i] 
		      << std::endl;
	    if(i>15) break;    }
	#endif   
    }
    else
    {
	LOG_FATAL << Form("%s :: fgtElosCutoff table  failed,",GetName())
		  << endm;
	assert(12==1);
    }
  
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
	if ( dynamic_cast< StFgtDbNaiveImpl * >(m_tables) )
	{
	    delete m_tables;
	}
	if ( !m_tables )
	{
	    m_tables = new StFgtDbImpl();
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
  
    if (pedDataset && statusDataset && mapDataset && gainDataset )
    {
	if ( m_rmap )
	    delete m_rmap;
	m_rmap = new fgtMapping_st();

	fgtMapping_st * map = mapDataset->GetTable();

	for ( int ii = 0; ii < 51200; ++ii )
	{
	    m_rmap->Mapping[ map->Mapping[ii] ] = ii;
	}

	dynamic_cast< StFgtDbImpl * >(m_tables)->updateTables(
	    map, m_rmap,
	    statusDataset->GetTable(),
	    pedDataset->GetTable(),
	    gainDataset->GetTable()
	);

	mLossTab = eLosDataset->GetTable(); assert(mLossTab);
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
    else
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

	assert(21==1);
    }

    geom=new StFgtGeom();   //	for now it is static, but later may be time
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
Float_t StFgtDbMaker::eLossTab(int bin)
{
    assert(bin>=0);
    assert(bin<10000);
    return mLossTab[0].cutoff[bin];
}

// $Log: StFgtDbMaker.cxx,v $
// Revision 1.6  2011/11/14 01:18:00  wwitzke
// Fixes so that we have better errors in StFgtDbMaker.cxx.
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
