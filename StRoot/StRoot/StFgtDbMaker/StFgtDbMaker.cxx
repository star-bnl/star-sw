// $Id: StFgtDbMaker.cxx,v 1.22 2013/01/31 15:42:19 akio Exp $
/* \class StFgtDbMaker        
\author Stephen Gliske
*/

#include "StFgtDbMaker.h"
#include "TDataSetIter.h"
#include "StMessMgr.h"
#include "tables/St_fgtElosCutoff_Table.h"
#include "tables/St_fgtSimuParams_Table.h"
#include "tables/St_fgtPedestal_Table.h"
#include "tables/St_fgtMapping_Table.h"
#include "tables/St_fgtGain_Table.h"
#include "tables/St_fgtStatus_Table.h"
#include "tables/St_fgtAlignment_Table.h"
#include "St_db_Maker/St_db_Maker.h"

ClassImp(StFgtDbMaker)

//_____________________________________________________________________________
StFgtDbMaker::StFgtDbMaker(const char *name)
    : StMaker(name)
{
  
    m_tables = new StFgtDb();
    m_rmap = 0;
    m_geom=0;
}

//_____________________________________________________________________________
StFgtDbMaker::~StFgtDbMaker()
{
    if ( m_tables )
	delete m_tables;
}

//-----------------------------------------------------------------------------
void StFgtDbMaker::setFlavor( const char * flav, const char * tabname )
{
    StMaker::SetFlavor( flav, tabname );
}

//_____________________________________________________________________________
Int_t StFgtDbMaker::Init()
{
    LOG_DEBUG << "StFgtDbMaker::Init()" <<endm;

    return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StFgtDbMaker::InitRun(Int_t runNumber)
{
    LOG_INFO << Form("StFgtDbMaker::InitRun(), run=%d",runNumber)<<endm;
    LOG_INFO << Form("StFgtDbMaker::InitRun(), Event DateTime=%s",GetDateTime().AsString())<<endm;
    LOG_INFO << Form("StFgtDbMaker::InitRun(), Database Time=%s",GetDBTime().AsString())<<endm;

    // clear old pointers to DB tables, just in case. Do not delete the data which are owned by StDbMaker
    m_LossTab =0;


    ////start here
    St_fgtElosCutoff *eLossDataset=0;
    St_fgtSimuParams *simuParamsDataset=0;
    St_fgtMapping * mapDataset = 0;
    St_fgtGain * gainDataset = 0;
    St_fgtPedestal * pedDataset = 0;
    St_fgtStatus * statusDataset = 0;
    St_fgtAlignment *alignmentDataset = 0;

    /// Go get database tables
    LOG_INFO <<  "::RequestDataBase() for Elos cut off ..."<< endl;    
    TDataSet *ELossDB = GetDataBase("Calibrations/fgt/fgtElosCutoff");
    if (!ELossDB)
      {
	LOG_FATAL
	  << "ERROR: no table found in db for eloss, or malformed local db config" 
	  << endm;
      }
    
    /// Go get database tables
    LOG_INFO <<  "::RequestDataBase() for slow-simu params  ..."<< endl;    
    TDataSet *SimuParamsDB = GetDataBase("Calibrations/fgt/fgtSimuParams");
    if (!SimuParamsDB)
      {
	LOG_FATAL
	  << "ERROR: no table found in db for slow-simu params , or malformed local db config" 
	  << endm;
      }
    
    LOG_INFO <<  "::RequestDataBase() for calibration tables..."<< endl;  
    TDataSet *MapDB = GetDataBase("Calibrations/fgt/fgtMapping");
    if (!MapDB)
      {
	LOG_FATAL
	  << "ERROR: no table found in db for map, or malformed local db config" 
	  << endm;
      }
    TDataSet *PedDB = GetDataBase("Calibrations/fgt/fgtPedestal");
    if (!PedDB)
      {
	LOG_FATAL
	  << "ERROR: no table found in db for pedestals, or malformed local db config" 
	  << endm;
      }
    TDataSet *StatusDB = GetDataBase("Calibrations/fgt/fgtStatus");
    if (!StatusDB)
      {
	LOG_FATAL
		<< "ERROR: no table found in db for pedestals, or malformed local db config" 
		<< endm;
      }
    TDataSet *GainDB = GetDataBase("Calibrations/fgt/fgtGain");
    if (!GainDB)
      {
	LOG_FATAL
	  << "ERROR: no table found in db for pedestals, or malformed local db config" 
	  << endm;
      }
    TDataSet *AlignmentDB = GetDataBase("Calibrations/fgt/fgtAlignment");
    if (!AlignmentDB)
      {
	LOG_FATAL
	  << "ERROR: no table found in db for pedestals, or malformed local db config" 
	  << endm;
      }
    
    
    eLossDataset=(St_fgtElosCutoff*)ELossDB->Find("fgtElosCutoff"); 
    Int_t rows = eLossDataset->GetNRows();
    if (rows > 1) 
      {
	LOG_FATAL   << " found INDEXED fgtElosCutoff table with " << rows
		    << " rows, this is fatal, fix DB content" << endm;
      }
    
    
    simuParamsDataset=(St_fgtSimuParams*)SimuParamsDB->Find("fgtSimuParams"); 
    rows =simuParamsDataset->GetNRows();
    if (rows > 1) 
      {
	LOG_FATAL   << " found INDEXED fgtSimuParams table with " << rows
		    << " rows, this is fatal, fix DB content" << endm;
      }
    
    
    mapDataset = (St_fgtMapping *)
      MapDB->Find("fgtMapping");
    rows = mapDataset->GetNRows();
    if (rows > 1) 
      {
	LOG_FATAL   << " found INDEXED table for mapping with " << rows
		    << " rows, this is fatal, fix DB content" << endm;
      }

    gainDataset = (St_fgtGain*)
      GainDB->Find("fgtGain");
    rows = gainDataset->GetNRows();
    if (rows > 1) 
      {
	LOG_FATAL   << " found INDEXED table for gains with " << rows
		    << " rows, this is fatal, fix DB content" << endm;
      }

    pedDataset = (St_fgtPedestal*)
      PedDB->Find("fgtPedestal");
    rows = pedDataset->GetNRows();
    if (rows > 1) 
      {
	LOG_FATAL   << " found INDEXED table for pedestals with " << rows
		    << " rows, this is fatal, fix DB content" << endm;
      }
    statusDataset = (St_fgtStatus*)
      StatusDB->Find("fgtStatus");
    rows = statusDataset->GetNRows();
    if (rows > 1) 
      {
	LOG_FATAL   << " found INDEXED table for status with " << rows
		    << " rows, this is fatal, fix DB content" << endm;
      }
    
    alignmentDataset = (St_fgtAlignment*) AlignmentDB->Find("fgtAlignment");
    rows = alignmentDataset->GetNRows();
    if (rows > 1)
      {
        LOG_FATAL   << " found INDEXED table for alignment with " << rows
                    << " rows, this is fatal, fix DB content" << endm;
      }

    
    if (pedDataset && statusDataset && mapDataset && gainDataset && eLossDataset && simuParamsDataset && alignmentDataset)
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
	
	dynamic_cast< StFgtDb* >(m_tables)->updateTables(
	    map, m_rmap,
	    statusDataset->GetTable(),
	    pedDataset->GetTable(),
	    gainDataset->GetTable(),
	    eLossDataset->GetTable(),
	    simuParamsDataset->GetTable(),
	    alignmentDataset->GetTable()
	);

	LOG_INFO
	    <<
		Form(
		    "%s :: map, gain, pedestal and status tables received, comments",
		    GetName()
		)
	    << endm;
	
	displayBeginEndTime(mapDataset);
	displayBeginEndTime(statusDataset);
	displayBeginEndTime(pedDataset);
	displayBeginEndTime(gainDataset);
	displayBeginEndTime(alignmentDataset); 
	displayBeginEndTime(eLossDataset);  LOG_INFO <<eLossDataset->GetTable()->comment<<endl;
	displayBeginEndTime(simuParamsDataset); LOG_INFO <<simuParamsDataset->GetTable()->comment<<endl;

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
	if ( !eLossDataset )
	{
	    LOG_FATAL << Form("%s :: eLoss table failed,",GetName())
		      << endm;
	}
	if ( !simuParamsDataset )
	{
	    LOG_FATAL << Form("%s :: SimuParams table failed,",GetName())
		      << endm;
	}
	if ( !alignmentDataset )
	{
	    LOG_FATAL << Form("%s :: Alignment table failed,",GetName())
		      << endm;
	}
    }

    return kStOK;
}

//_____________________________________________________________________________
void StFgtDbMaker::displayBeginEndTime(TTable* table) {
    TDatime datime[2];
    St_db_Maker::GetValidity(table,datime);
    string tableName = table->GetName();
    string beginTime = datime[0].AsSQLString();
    string endTime   = datime[1].AsSQLString();
    
    map<string, pair<string, string> >::iterator iter = mValidRanges.find(tableName);
    if(iter == mValidRanges.end()) {
        mValidRanges[tableName] = make_pair(beginTime, endTime);
        LOG_INFO << Form("loaded a new %20s table with beginTime %s and endTime %s", tableName.c_str(), beginTime.c_str(), endTime.c_str()) << endm; 
	cout << Form("loaded a new %20s table with beginTime %s and endTime %s", tableName.c_str(), beginTime.c_str(), endTime.c_str()) << endl; 
    }
    else if( beginTime != (iter->second).first ) {
        (iter->second).first    = beginTime;
        (iter->second).second   = endTime;
        LOG_INFO << Form("loaded a new %20s table with beginTime %s and endTime %s", tableName.c_str(), beginTime.c_str(), endTime.c_str()) << endm; 
        cout << Form("loaded a new %20s table with beginTime %s and endTime %s", tableName.c_str(), beginTime.c_str(), endTime.c_str()) << endl;
    }
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
// Revision 1.22  2013/01/31 15:42:19  akio
// Adding Alignment table and getStarXYZ()
//
// Revision 1.21  2012/07/31 18:24:30  jeromel
// Removed virtual + fixed name
//
// Revision 1.20  2012/06/03 16:52:18  balewski
// added access to fgtSimuParam table, all I/O .C code is saved in macros
//
// Revision 1.19  2012/03/19 01:18:52  rfatemi
// Modified for removal of StFgtDbImpl and StFgtDbIdealImpl
//
// Revision 1.18  2012/03/14 01:07:11  rfatemi
// added comments
//
// Revision 1.17  2012/03/10 01:59:22  rfatemi
// Review comments
//
// Revision 1.16  2012/02/22 20:07:44  rfatemi
// Changed name from updateValidity to displayBeginEndTime
//
// Revision 1.15  2012/02/22 04:04:18  rfatemi
// Added beginTime's for each table
//
// Revision 1.14  2012/01/31 21:07:43  rfatemi
// changes for StFgtDbIdealImpl.h
//
// Revision 1.13  2012/01/31 17:02:46  wwitzke
// Added date/time printout to StFgtDbMaker.cxx InitRun().
//
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
