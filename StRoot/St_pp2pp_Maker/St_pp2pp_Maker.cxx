/*!
 *                                                                     
 * \class  St_pp2pp_Maker
 * \author Kin Yip
 * \date   2009/11/19
 * \brief  For pp2pp analysis : mainly to create clusters from raw data silicon hits
 *
 * Revision 2015/2/22 (Kin Yip) : Now it can deal with 2009 as well as >=2015 data
 *                                which read in different database for pedestal/rms
 *
 * Revision 2015/10/3 (Kin Yip) : Add positionRMS to each cluster
 *
 */                                                                      

#include "St_pp2pp_Maker.h"
#include "StRtsTable.h"
#include "TDataSetIter.h"
#include "RTS/src/DAQ_PP2PP/pp2pp.h"

#include "St_db_Maker/St_db_Maker.h"

#include "tables/St_pp2ppPedestal_Table.h"
#include "tables/St_pp2ppPedestal160_Table.h" // K. Yip : Feb. 20, 2015 : New pedestal table (one per SVX)
#include "tables/St_pp2ppOffset_Table.h"
#include "tables/St_pp2ppZ_Table.h"
#include "tables/St_pp2ppRPpositions_Table.h" // K. Yip : Aug. 10, 2015 : LVDT readings for RP positions
#include "tables/St_pp2ppAcceleratorParameters_Table.h" // K. Yip : Aug. 10, 2015 : LVDT readings for RP positions
#include "tables/St_pp2ppPMTSkewConstants_Table.h" // K. Yip : Aug. 10, 2015 : LVDT readings for RP positions

#include "StEvent/StEvent.h"
#include "StEvent/StRunInfo.h"
#include "StEvent/StRpsCollection.h"
#include "StEvent/StRpsCluster.h"
#include "StEvent/StRpsTrackPoint.h" // added by Rafal
#include "StEvent/StRpsTrack.h" // added by Rafal

#include "StEvent/StTriggerData2009.h"
#include "StEvent/StTriggerData2012.h"
#include "StEvent/StTriggerData2013.h"

using namespace std;

ClassImp(St_pp2pp_Maker)

  St_pp2pp_Maker::St_pp2pp_Maker(const char *name) : StRTSBaseMaker("pp2pp",name),   
						     mPedestalPerchannelFilename("pedestal.in.perchannel"), mLDoCluster(kTRUE),
						     maxPitchesToMatch(3.0){ // added by Rafal
  // ctor
  //  nevt_count = 0 ;

  Pitch[X] = kpitch_6svx; // added by Rafal
  Pitch[Y] = kpitch_4svx; // added by Rafal
}


St_pp2pp_Maker::~St_pp2pp_Maker() {
}

//_____________________________________________________________________________
/// Init - is a first method the top level StChain calls to initialize all its makers 
Int_t St_pp2pp_Maker::Init() {
  mLastSvx   = ErrorCode;
  mLastChain = ErrorCode;
  mLastSeq   = ErrorCode ;
  return StMaker::Init();
}

Int_t St_pp2pp_Maker::InitRun(int runumber) {

  if ( runumber < 16000000 )
    mVersion = 1 ; // from 2009 to < 2015
  else
    mVersion = 2 ; // >= 2015

  cout << "St_pp2pp_Maker: Timestamp - Day: " << GetDateTime().GetDate() << " , DB-Time: " << GetDBTime().GetDate() << endl ;
  cout << "St_pp2pp_Maker: Timestamp - Time: " << GetDateTime().GetTime() << " , DB-Date: " << GetDBTime().GetTime() << endl ;

  if ( mLDoCluster ) {
    readPedestalPerchannel() ;
    readOffsetPerplane() ;
    readZPerplane() ;
    
    // K. Yip (2015-10-22): for Rafal's Tracking really
    if ( mVersion >= 2 ) {
      readSkewParameter() ;
      readAccelerateParameter() ;
    }
  }

  return kStOk ;
}

Int_t St_pp2pp_Maker::readPedestalPerchannel() {

  //  cout << "Size of each struct in DB : " << sizeof(pp2ppPedestal_st) << endl ;

  //  cout << "Size of mPedave : " << sizeof(mPedave) << " , Size of mPedrms : " << sizeof(mPedrms) << endl ;

  memset(mPedave,0,sizeof(mPedave));
  memset(mPedrms,0,sizeof(mPedrms));

  Int_t s, c, sv, ch, idb = 0 ;

  /*
  //  cout << GetTime() << " " << GetDate() << endl ;

  St_db_Maker *dbMk = (St_db_Maker*) GetMaker("db");
  if ( ! dbMk ) {
    LOG_WARN << "No St_db_Maker existed ?! " << endm ;
  }
  else {
    //    cout << "I got St_db_Maker ?? " << endl ;
    dbMk->SetDateTime(this->GetDate(), this->GetTime());
  }
  */

  // Database
  TDataSet *DB = 0;
  if ( mVersion == 1 ) { // before 2015

    DB = GetInputDB("Calibrations/pp2pp/pp2ppPedestal");
    if (!DB) {
      LOG_ERROR << "ERROR: cannot find database Calibrations_pp2pp/pp2ppPedestal ?" << endm ;
    }
    else {
      // fetch ROOT descriptor of db table
      St_pp2ppPedestal *descr = 0;
      descr = (St_pp2ppPedestal*) DB->Find("pp2ppPedestal");
      // fetch data and place it to appropriate structure
      if (descr) {
        pp2ppPedestal_st *table = descr->GetTable();
	//	cout << "Reading pp2ppPedestal table with nrows = " << descr->GetNRows() << endl ;
	for ( idb = 0; idb < descr->GetNRows(); idb++ ) {
	  s = (Int_t) table[idb].sequencer ;
	  c = (Int_t) table[idb].chain ;
	  sv = (Int_t) table[idb].SVX ;
	  ch = (Int_t) table[idb].channel ;

	  if ( s > 0 ) { /// protect against zero entries (such as 10185015 with 2D switched off)
	    mPedave[s-1][c][sv][ch] = table[idb].mean ;
	    mPedrms[s-1][c][sv][ch] = table[idb].rms ;
	  }
	  //		  cout << s << " " << c << " "  << sv << " " << ch << " " << mPedave[s-1][c][sv][ch] << " " << mPedrms[s-1][c][sv][ch] << endl ; 

	}
      } else {
	LOG_ERROR << "St_pp2pp_Maker: No data in pp2ppPedestal table (wrong timestamp?). Nothing to return, then." << endm ;
      }
    }
  }
  else { // 2015 and afterwards
    // K. Yip : Feb. 20, 2015 : Use a new table of pedestal/rms per SVX (160 of them only)
    DB = GetDataBase("Calibrations/pp2pp/pp2ppPedestal160");
    if (!DB) {
      LOG_ERROR << "ERROR: cannot find database Calibrations/pp2pp/pp2ppPedestal160 ?" << endm ;
    }
    else {

      St_pp2ppPedestal160 *dataset = 0;
      dataset = (St_pp2ppPedestal160*) DB->Find("pp2ppPedestal160");

      if (dataset) {
	if ( dataset->GetNRows() > 1 ) {
	  LOG_ERROR << "St_pp2pp_Maker : Found INDEXED table with " <<  dataset->GetNRows() << " rows \?!" << endm ;
	}

	pp2ppPedestal160_st *table = dataset->GetTable();
	for (Int_t j = 0; j < 160; j++) {

	  s = j/20 ; // == Sequence - 1
	  if ( (j%20) < 4 ) {
	    c = 0 ;
	    sv = j%20 ;
	  }
	  else if ( (j%20) < 10 ) {
	    c = 1 ;
	    sv = j%20 - 4  ;
	  }
	  else if ( (j%20) < 14 ) {
	    c = 2 ;
	    sv = j%20 - 10  ;
	  }
	  else {
	    c = 3 ;
	    sv = j%20 - 14  ;
	  }

	  LOG_DEBUG << j << "th element: seq = " << s+1 << " chain = " << c << " svx = " << sv 
		    << " => mean: " << table[0].mean[j] << ", rms: " << table[0].rms[j] << endm ;

	  mPedrms[s][c][sv][0] = table[0].rms[j] ;

	} 
      }
      else {
	LOG_ERROR << "St_pp2pp_Maker: dataset does not contain requested table" << endm ;
      }

    }
  }

  //  LOG_DEBUG << idb << " pedestal entries read from DB table Calibration/pp2pp read. " << endm ;


  return kStOk ;

}

Int_t St_pp2pp_Maker::readOffsetPerplane() {

  mOffsetTable = 0;
  mRPpositionsTable = 0 ; // >= 2015

  TDataSet *DB = 0;
  DB = GetInputDB("Geometry/pp2pp/pp2ppOffset");
  if (!DB) { 
    LOG_ERROR << "ERROR: cannot find database Geometry_pp2pp/pp2ppOffset ?" << endm ; 
  }
  else {

    // fetch ROOT descriptor of db table
    St_pp2ppOffset *descr = 0;
    descr = (St_pp2ppOffset*) DB->Find("pp2ppOffset");
    // fetch data and place it to appropriate structure
    if (descr) {
      mOffsetTable = descr->GetTable();
      LOG_DEBUG << "St_pp2pp_Maker : Reading pp2ppOffset table with nrows = " << descr->GetNRows() << endm ;
      /*
      for (Int_t i = 0; i < descr->GetNRows(); i++) {
	for ( Int_t j = 0; j< 32 ; j++ )
	  std::cout << mOffsetTable[i].rp_offset_plane[j] << " "  ; 
	cout << endl ;
      }
      */
    } else {
      LOG_ERROR << "St_pp2pp_Maker : No data in pp2ppOffset table (wrong timestamp?). Nothing to return, then" << endm ;
    }

  }

  if ( mVersion >= 2 ) { // only for >=2015

    DB = GetInputDB("Calibrations/pp2pp/pp2ppRPpositions");
    if (!DB) {
      LOG_ERROR << "ERROR: cannot find database Calibrations_pp2pp/pp2ppRPpositions ?" << endm ;
    }
    else {
      // fetch ROOT descriptor of db table
      St_pp2ppRPpositions *descr = 0;
      descr = (St_pp2ppRPpositions*) DB->Find("pp2ppRPpositions"); 
      if (descr) {
	LOG_DEBUG << "St_pp2pp_Maker : Reading pp2ppRPpositions table with nrows = " << descr->GetNRows() << endm ;
	mRPpositionsTable = descr->GetTable();
	mLVDT_pos[0] = mRPpositionsTable[0].y_right_E1U ;
	mLVDT_pos[1] = mRPpositionsTable[0].y_left_E1D ;
	mLVDT_pos[2] = mRPpositionsTable[0].y_top_E2U ;
	mLVDT_pos[3] = mRPpositionsTable[0].y_bot_E2D ;
	mLVDT_pos[4] = mRPpositionsTable[0].b_right_W1U ;
	mLVDT_pos[5] = mRPpositionsTable[0].b_left_W1D ;
	mLVDT_pos[6] = mRPpositionsTable[0].b_top_W2U ;
	mLVDT_pos[7] = mRPpositionsTable[0].b_bot_W2D ; 
	/*
	  for (Int_t i = 0; i < descr->GetNRows(); i++) {
	  std::cout << i << "th row : " << mRPpositionsTable[i].b_left_W1D << " "  << mRPpositionsTable[i].b_right_W1U
	  << " " << mRPpositionsTable[i].b_bot_W2D << " "  << mRPpositionsTable[i].b_top_W2U
	  << " " << mRPpositionsTable[i].y_left_E1D << " "  << mRPpositionsTable[i].y_right_E1U
	  << " " << mRPpositionsTable[i].y_bot_E2D << " "  << mRPpositionsTable[i].y_top_E2U << std::endl;
	  }
	*/
      } else {
	LOG_ERROR << "St_pp2pp_Maker : No data in pp2ppRPpositions table (wrong timestamp?). Nothing to return, then" << endm ;
      }


    }

  }

  return kStOk ;

}


Int_t St_pp2pp_Maker::readZPerplane() {

  mZTable = 0;

  TDataSet *DB = 0;
  DB = GetInputDB("Geometry/pp2pp/pp2ppZ");
  if (!DB) { 
    LOG_ERROR << "ERROR: cannot find database Geometry_pp2pp/pp2ppZ ?" << endm ; 
  }
  else {

    // fetch ROOT descriptor of db table
    St_pp2ppZ *descr = 0;
    descr = (St_pp2ppZ*) DB->Find("pp2ppZ");
    // fetch data and place it to appropriate structure
    if (descr) {
      mZTable = descr->GetTable();
      LOG_DEBUG << "Reading pp2ppZ table with nrows = " << descr->GetNRows() << endm ;
      /*
      for (Int_t i = 0; i < descr->GetNRows(); i++) {
	for ( Int_t j = 0; j< 32 ; j++ )
	  std::cout << mZTable[i].rp_z_plane[j] << " "  ; 
	cout << endl ;
      }
      */
    } else {
      LOG_ERROR << "St_pp2pp_Maker : No data in pp2ppZ table (wrong timestamp?). Nothing to return, then" << endm ;
    }

  }

  return kStOk ;

}


Int_t St_pp2pp_Maker::readSkewParameter() {

  memset(mskew_param,0,sizeof(mskew_param));

  int s, ipmt, ipar ;

  // Database
  TDataSet *DB = 0;
  DB = GetDataBase("Calibrations/pp2pp/pp2ppPMTSkewConstants");
  if (!DB) {
    LOG_ERROR << "ERROR: cannot find database Calibrations/pp2pp ?" << endm ;
  }
  else {

    St_pp2ppPMTSkewConstants *dataset = 0;
    dataset = (St_pp2ppPMTSkewConstants*) DB->Find("Calibrations/pp2pp/pp2ppPMTSkewConstants");

      if (dataset) {

	if ( dataset->GetNRows() > 1 ) {
	  LOG_ERROR << "St_pp2pp_Maker : Found INDEXED table with " <<  dataset->GetNRows() << " rows \?!" << endm ;
	}

	pp2ppPMTSkewConstants_st *table = dataset->GetTable();
	for (int j = 0; j < 64; j++) {

	  s = j/kMAXSEQ ; // RP 0 .. 7
	  ipmt = (j/4) % 2  ; // 0 or 1
	  ipar = j - s*kMAXSEQ ; // 0 .. 3

	  LOG_DEBUG << j << "th element: RP = " << s << " PMT = " << ipmt << " parameter = " << ipar 
		    << " with parameter : " << table[0].skew_param[j] << endm ;

	  mskew_param[s][ipmt][ipar] = table[0].skew_param[j] ;

	} 
      }
      else {
	LOG_ERROR << "St_pp2pp_Maker: dataset does not contain requested table pp2ppPMTSkewConstants ." << endm ;
      }

  }

  return kStOk ;

}


Int_t St_pp2pp_Maker::readAccelerateParameter() {

  TDataSet *DB = 0;
  DB = GetInputDB("Geometry/pp2pp/pp2ppAcceleratorParameters");
  if (!DB) { 
    LOG_ERROR << "ERROR: cannot find database Geometry_pp2pp/pp2ppAcceleratorParameters ?" << endm ; 
  }
  else {

    // fetch ROOT descriptor of db table
    St_pp2ppAcceleratorParameters *descr = 0;
    descr = (St_pp2ppAcceleratorParameters*) DB->Find("pp2ppAcceleratorParameters");
    // fetch data and place it to appropriate structure
    if (descr) {
      pp2ppAcceleratorParameters_st *table = descr->GetTable();
      LOG_DEBUG << "St_pp2pp_Maker : Reading pp2ppAcceleratorParameters table with nrows = " << descr->GetNRows() << endm ;

      mx_IP = table[0].x_IP ;
      my_IP = table[0].y_IP ;
      mz_IP = table[0].z_IP ;

      mtheta_x_tilt = table[0].theta_x_tilt ;
      mtheta_y_tilt = table[0].theta_y_tilt ;

      mdistancefromDX_east = table[0].distancefromDX_east ;
      mdistancefromDX_west = table[0].distancefromDX_west ;

      mLDX_east = table[0].LDX_east ;
      mLDX_west = table[0].LDX_west ;

      mbendingAngle_east = table[0].bendingAngle_east ;
      mbendingAngle_west = table[0].bendingAngle_west ;

      /*
      for (Int_t i = 0; i < descr->GetNRows(); i++) {
	for ( Int_t j = 0; j< 32 ; j++ )
	  std::cout << mOffsetTable[i].rp_offset_plane[j] << " "  ; 
	cout << endl ;
      }
      */
    } else {
      LOG_ERROR << "St_pp2pp_Maker : No data in pp2ppAcceleratorParameters table (wrong timestamp?). Nothing to return, then" << endm ;
    }

  }

  return kStOk ;

}



//_____________________________________________________________________________
/// Clear - this method is called in loop for prepare the maker for the next event
void  St_pp2pp_Maker::Clear(Option_t *) {

  // Deleting previous cluster info.
  for ( Int_t i=0; i<kMAXSEQ; i++)
    for ( Int_t j=0; j<kMAXCHAIN; j++)
      (mValidHits[i][j]).clear();

  StMaker::Clear(); // perform the basic clear (mandatory)

}


//_____________________________________________________________________________
/// Make - this method is called in loop for each event
Int_t St_pp2pp_Maker::Make(){

  //  if ( nevt_count%10000 == 0 ) cout << "St_pp2pp_Maker:: Event count : " << nevt_count << endl ;

  //  nevt_count++ ;

  if ( Token() == 0 )  /// if Token == 0, this is not a real event.
    return kStOK ;

  //
  //  PrintInfo();
  //
  //  ls (0);

  int counter = -1; 
  
  TGenericTable *pp2ppRawHits = new TGenericTable("pp2ppRawHit_st","pp2ppRawHits");


  // Each GetNextAdc would get a SVX ...
  if ( mVersion == 1 ) { // before 2015
    while ( GetNextAdc() ) {
     counter++;
     TGenericTable::iterator iword = DaqDta()->begin();
     for (;iword != DaqDta()->end();++iword) {
        pp2pp_t &d = *(pp2pp_t *)*iword;
        // do something
	if ( DoerPp2pp(d,*pp2ppRawHits) != kStOK )
	  return kStERR ;
	if ( counter == 0 ) mSiliconBunch = d.bunch_xing ;
     }
    }
  } else { // >= 2015
    while ( GetNext("adc_ped_sub") ) { // K. Yip : Feb. 20, 2015 : to get the pedestal-subtracted ADC's
      counter++;
      TGenericTable::iterator iword = DaqDta()->begin();
      for (;iword != DaqDta()->end();++iword) {
        pp2pp_t &d = *(pp2pp_t *)*iword;
        // do something
	if ( DoerPp2pp(d,*pp2ppRawHits) != kStOK )
	  return kStERR ;
	if ( counter == 0 ) mSiliconBunch = d.bunch_xing ;
      }
    }
  }

  if (counter < 0) {
    LOG_DEBUG << "There was no pp2pp data for this event. " << endm;
  } else {
    LOG_DEBUG << "End of pp2pp data for this event : " << GetEventNumber() << ", Total = "  << counter+1 
	     << " records were found" << endm;
  }

  AddData(pp2ppRawHits);  // publish RawHits to make it available for other makers in the chain
  // one may not call AddData if the result should not be published.
  // to discard the result one should call  "delete pp2ppRawHits"


  if ( mLDoCluster ) { 

    for ( Int_t i=0; i<kMAXSEQ; i++)
      for ( Int_t j=0; j<kMAXCHAIN; j++) {
	sort( (mValidHits[i][j]).begin(), (mValidHits[i][j]).end(), hitcompare);
	//	cout << "Size of vector of sequencer " << i+1 << " chain " << j << " " << dec << (mValidHits[i][j]).size() << endl ;
      }

    MakeClusters();

  }

  return kStOK;

}

//_____________________________________________________________________________
/// DoerPp2pp - this method is called as soon as next pp2pp record is read in
Int_t St_pp2pp_Maker::DoerPp2pp(const pp2pp_t &d, TGenericTable &hitsTable) {

  pp2ppRawHit_st       oneSihit = {0}; // This essentially gives adc the value of "0"
  oneSihit.sec       = Sector() ;
  oneSihit.sequencer = d.seq_id ;
  oneSihit.chain     = d.chain_id ;
  oneSihit.svx       = d.svx_id ;

  // For clustering purpose
  HitChannel onehit ;

  // Mar. 14, 2009 (K. Yip) : checking for wrong SVX_ID
  // One known case is for SEQ 3, CHAIN 2 and SVX is 7 but it should be 3.
  // Mostly, just some debugging codes that we've used in the past and shouldn't happen

  if ( mVersion == 1 ) { // before 2015
    if ( (oneSihit.svx != mLastSvx) && (mLastSvx != ErrorCode) ) {

      if (  Int_t(oneSihit.svx-1) != mLastSvx )

	if (  ( (oneSihit.svx-mLastSvx) != -3 && ( (oneSihit.chain%2)==1 ) ) ||
	      ( (oneSihit.svx-mLastSvx) != -5 && ( (oneSihit.chain%2)==0 ) ) ) {

	  if ( oneSihit.svx == 7 && oneSihit.sequencer == 3 && oneSihit.chain == 2 )
	    oneSihit.svx = 3 ;
	  //		  else if ( oneSihit.svx < mLastSvx ) {
	  else if ( oneSihit.svx < mLastSvx && ( GetRunNumber()<10185015 || (mLastSeq!=2 && mLastChain!=2)) ) { // bad seq 2 and chain D

	    LOG_WARN << "Decreased ? " <<  GetEventNumber() << " : mLastSeq = " << mLastSeq << ", mLastChain = " << mLastChain << ", mLastSvx = " << mLastSvx << endm ;
	    LOG_WARN << "Decreased ?  " << GetEventNumber() << " : Now, seq = " << (int) oneSihit.sequencer << ", chain = " << (int) oneSihit.chain << ", svx = " << (int) oneSihit.svx << endm ;
	  
	    oneSihit.svx = mLastSvx + 1 ;
		    
	    LOG_WARN << "Decreased ? : So -> " << " svx is now = " << (int) oneSihit.svx << endm ;	      

	  }
	  //	else if ( mLastSeq!=2 && mLastChain!=2 ) { // bad seq 2 and chain D
	  else if ( GetRunNumber()<10185015 || ( mLastSeq!=2 && mLastChain!=2 ) ) { // bad seq 2 and chain D

	    LOG_WARN << GetEventNumber() << " : mLastSeq = " << mLastSeq << ", mLastChain = " << mLastChain << ", mLastSvx = " << mLastSvx << endm ;
	    LOG_WARN << GetEventNumber() << " : Now, seq = " << (int) oneSihit.sequencer << ", chain = " << (int) oneSihit.chain << ", svx = " << (int) oneSihit.svx << endm ;

	  }

	}
	      

    }
    else if ( (oneSihit.chain==mLastChain) && (mLastChain != ErrorCode) ) {
      LOG_WARN << "Repeated ? :" << GetEventNumber() << " : mLastSeq = " << mLastSeq << ", mLastChain = " << mLastChain << ", mLastSvx = " << mLastSvx << endm ;
      LOG_WARN << "Repeated ? : " << GetEventNumber() << " : Now, seq = " << (int) oneSihit.sequencer << ", chain = " << (int) oneSihit.chain << ", svx = " << (int) oneSihit.svx << endm ;

      oneSihit.svx = mLastSvx + 1 ;

      LOG_WARN << "Repeated : So -> " << " svx is now = " << (int) oneSihit.svx << endm ;	      
    }

  } else { // >= 2015

    // K. Yip : Feb. 20, 2015 : Since we're reading "adc_ped_sub" bank, the continuity is no longer there.
    
    // K. Yip : Feb. 20, 2015 : Now, it's for SEQ 7, CHAIN 2 => SVX is 7 but it should be 3.
    if ( oneSihit.svx == 7 && oneSihit.sequencer == 7 && oneSihit.chain == 2 )
      oneSihit.svx = 3 ;
  }

  mRpStatus[oneSihit.sequencer - 1] = d.bunch_xing ; // hack to store the silicon_bunch

  mLastSeq = oneSihit.sequencer; 
  mLastChain = oneSihit.chain;
  mLastSvx = oneSihit.svx;

  //  cout << "Seq: " << mLastSeq << " , chain " << mLastChain << ", SVX = " << mLastSvx << endl ;

  for(unsigned int c=0;c<sizeof(d.adc);c++) {
    //	      if( d.adc[c] ) printf("   %3d: %3d [0x%02X]\n",c,d.adc[c],d.adc[c]) ;
    //	      adc[nsvx][c] = d.adc[c];
    if ( d.trace[c] == 1 ) {
      oneSihit.channel = c ;
      oneSihit.adc = d.adc[c];
      hitsTable.AddAt(&oneSihit);

      //      cout << "channel " << c << " , adc " << (int) d.adc[c] << endl ;

      if ( mLDoCluster && (c != (kMAXSTRIP-1)) && (c != 0) ) { // Avoid the channels at 2 ends of SVX
	
	// Getting rid of the 1st channel (0) and the last channel (127)
	// K. Yip : Feb. 20, 2015 : 
	// The plane E2D.A installed on Jan. 30, 2015 had an old BNL made silicon in it. In this version _all_ SVX channels were connected to the silicon.
	if ( ( mLastSeq == 4 ) && ( mLastChain == 0 ) && ( mVersion==2 ) ) // for 2015
	  onehit.first = mLastSvx*(kMAXSTRIP) + oneSihit.channel  ; 
	else
	  onehit.first = mLastSvx*(kMAXSTRIP-2) + oneSihit.channel - 1  ; 
	

	if ( mVersion<=1 ) {

	  onehit.second = oneSihit.adc -  mPedave[mLastSeq-1][mLastChain][mLastSvx][oneSihit.channel] ;

	  if ( onehit.second > 5*mPedrms[mLastSeq-1][mLastChain][mLastSvx][oneSihit.channel] ) {
	    (mValidHits[mLastSeq-1][mLastChain]).push_back(onehit);
	    //	  cout << "mValidHits : position " << onehit.first << " , energy " << onehit.second << endl ;
	  }

	} else {

	  onehit.second = oneSihit.adc ; // as it's pedestal-subtracted already

	  if ( onehit.second > 5*mPedrms[mLastSeq-1][mLastChain][mLastSvx][0] ) {
	    (mValidHits[mLastSeq-1][mLastChain]).push_back(onehit);
	  }

	}

      }

    } 
    else if ( d.trace[c] == 2 ) { // 2015-1-25 (K. Yip) : Tonko's advice if there is trace =2 anywhere, just drop this event
      LOG_ERROR << "St_pp2pp_Maker : d->trace[c] == 2 ! " << endm ;
      return kStERR ; 
    } 
    else if ( d.trace[c] != 0 )
      std::cout << GetEventNumber() << " : trace = " << (Int_t) d.trace[c] << ", Seq " << (Int_t) oneSihit.sequencer 
		<< ", chain " << (Int_t) oneSihit.chain << ", SVX " << (Int_t) oneSihit.svx << ", channel " << c 
		<< " is duplicated ? ==> " << (Int_t) d.adc[c] << std::endl ;
  }

  return kStOk;

}

Int_t St_pp2pp_Maker::MakeClusters() {

  //  const Int_t MAX_Cls_L = 5 ;
  //  const Int_t MIN_Charge = 20 ;
  /// Orientations for each silicon plane
  // 2009
  //                                               EHI          EHO          EVU        EVD         WHI          WHO          WVD        WVU
  const short orientations[kMAXCHAIN*kMAXSEQ] = {-1,1,-1,1,  1,-1,1,-1,  1,1,1,1, -1,-1,-1,-1,  -1,-1,-1,-1,  1,1,1,1,  -1,1,-1,1, 1,-1,1,-1 };
  // >=2015
  //                                               E1U          E1D          E2U        E2D         W1U          W1D          W2U        W2D
  const short orientations2[kMAXCHAIN*kMAXSEQ] = {1,1,1,1,  -1,-1,-1,-1,  1,1,1,1, -1,-1,-1,-1,  1,-1,1,-1,  -1,1,-1,1,  1,-1,1,-1, -1,1,-1,1 };
  /// Assume 4 planes have the same z at least for now
  const double zcoordinates[kMAXSEQ] = { -55.496, -55.496, -58.496, -58.496, 55.496, 55.496, 58.496, 58.496 };

  /// Mappings to deal with the trigger data
  const short EW[kMAXSEQ]   = { 0, 0, 0, 0, 1, 1, 1, 1 } ; /// East = 0, West = 1
  const short VH[kMAXSEQ]   = { 1, 1, 0, 0, 1, 1, 0, 0 } ; /// Vertical = 0, Horizontal = 1
  const short UDOI[kMAXSEQ] = { 1, 0, 0, 1, 1, 0, 1, 0 } ; /// Up=0, Down=1; Outer=0, Inner=1
  // >=2015 --- W2U (~WVU) is sequencer 7 and W2D (~WVD) is sequencer 8
  const short UD[kMAXSEQ]  =  { 1, 0, 0, 1, 1, 0, 0, 1 } ; 

  // Bogdan's alignment-corrected offsets (in mm)
  const double LVDT_OFFSET[32] = { 
    5.4561,-36.8055,  5.4104,-36.7974, -4.4512, 41.6271, -4.4076, 41.6106,
    5.4395,-14.6417,  5.5217,-14.7002, -4.6579, 63.7561, -4.8961, 63.5281,
    6.2214, 44.4315,  6.2917, 44.0845, -5.0226,-36.1377, -4.9259,-36.4143,
    5.6669, 65.6778,  5.6349, 65.6091, -3.1667,-14.8623, -3.1441,-15.0686,
  };

  const double LVDT_SCALE[32] = { 
    0.9989,  0.0000,  0.9989,  0.0000,  0.9991,  0.0000,  0.9991,  0.0000,
    0.9969,  0.0051,  0.9969,  0.0051,  0.9974,  0.0000,  0.9974,  0.0000,
    0.9933, -0.0142,  0.9933, -0.0142,  0.9658,  0.0058,  0.9655,  0.0058,
    1.0040,  0.0000,  1.0040,  0.0000,  1.0499,  0.0000,  1.0499,  0.0000,
  };

  Bool_t is_candidate_to_store ;

  Int_t NCluster_Length, Diff_Bunch ;
  Double_t ECluster, POStimesE, POStimesESq, position, positionRMS, offset, pitch ;

  StTriggerData* trg_p = 0 ;
  /// Fetching the pointer to the Trigger Data
  TObjectSet *os = (TObjectSet*)GetDataSet("StTriggerData");
  if (os) {
    trg_p = (StTriggerData*)os->GetObject();
  }


  /// Creating a new StEvent object
  StRpsCollection * pp2ppColl = new StRpsCollection(); 

  /// Set Silicon-Bunch
  pp2ppColl->setSiliconBunch(mSiliconBunch) ;

  vector< HitChannel >::iterator it, it_next ;

  for ( Int_t i=0; i<kMAXSEQ; i++) /// each sequencer/roman-pot
    for ( Int_t j=0; j<kMAXCHAIN; j++) { /// each chain/silicon-plane


      // Put in trigger stuff 

      if(trg_p){ 

	if ( mVersion == 1 ) {  // 2009
	  pp2ppColl->romanPot(i)->setAdc((u_int) trg_p->pp2ppADC( (StBeamDirection) EW[i],VH[i],UDOI[i],0),/// u_short -> u_int
					 (u_int) trg_p->pp2ppADC( (StBeamDirection) EW[i],VH[i],UDOI[i],1) ); 
	  pp2ppColl->romanPot(i)->setTac((u_int) trg_p->pp2ppTAC( (StBeamDirection) EW[i],VH[i],UDOI[i],0),/// u_short -> u_int
					 (u_int) trg_p->pp2ppTAC( (StBeamDirection) EW[i],VH[i],UDOI[i],1) ); 
	}
	else { // >= 2015
	  pp2ppColl->romanPot(i)->setAdc((u_int) trg_p->pp2ppADC( (StBeamDirection) EW[i],VH[i],UD[i],0),/// u_short -> u_int
					 (u_int) trg_p->pp2ppADC( (StBeamDirection) EW[i],VH[i],UD[i],1) ); 
	  pp2ppColl->romanPot(i)->setTac((u_int) trg_p->pp2ppTAC( (StBeamDirection) EW[i],VH[i],UD[i],0),/// u_short -> u_int
					 (u_int) trg_p->pp2ppTAC( (StBeamDirection) EW[i],VH[i],UD[i],1) ); 	  
	}

	// for now (Jan. 2010) : use the status byte as "silicon_bunch - bunchId7Bit()"
	Diff_Bunch = mRpStatus[i] - trg_p->bunchId7Bit() ;
	if ( Diff_Bunch < 0 ) Diff_Bunch += 120 ;
	pp2ppColl->romanPot(i)->setStatus( (unsigned char) Diff_Bunch ) ;

      }
      else
	LOG_WARN << "No StTriggerData ?! " << endm ;


      NCluster_Length = 0 ;
      ECluster = 0 ;
      POStimesE = 0 ;
      POStimesESq = 0 ;

      if ( mZTable )
	pp2ppColl->romanPot(i)->plane(j)->setZ( mZTable[0].rp_z_plane[4*i+j] ) ; /// z coordinates all in m
      else
	pp2ppColl->romanPot(i)->plane(j)->setZ(zcoordinates[i]) ; 

      if ( mVersion < 2 ) {
	if ( mOffsetTable )
	  offset = mOffsetTable[0].rp_offset_plane[4*i+j]/1000. ; /// offsets all in m
	else
	  offset = double(ErrorCode) ;
	//      cout << "Offsets : " <<  i << " " << j << " " << mOffsetTable[0].rp_offset_plane[4*i+j] << endl ; 
      }
      else {
	if ( mRPpositionsTable )
	  // K. Yip (Oct. 19, 2015) : set offsets back to m (as LVDT arrays are in mm)
	  offset = ( LVDT_OFFSET[4*i+j] + LVDT_SCALE[4*i+j]*mLVDT_pos[i] )/1000. ; 
	else
	  offset = double(ErrorCode) ;
	//	cout << "Offsets : " <<  i << " " << j << " " << offset << endl ; 
      }

      pp2ppColl->romanPot(i)->plane(j)->setOffset( offset ) ; 

      if ( mVersion < 2 )
	pp2ppColl->romanPot(i)->plane(j)->setOrientation( orientations[4*i+j] ) ;
      else
	pp2ppColl->romanPot(i)->plane(j)->setOrientation( orientations2[4*i+j] ) ;

      it = (mValidHits[i][j]).begin() ;

      while ( it != (mValidHits[i][j]).end() ) {

	//	cout << "Seq: " << i+1 << " , chain " << j << ", channel : " << it->first << " , energy : " << it->second << endl ;
	NCluster_Length++ ;
	ECluster += it->second ;
	POStimesE += it->first*it->second ;
	POStimesESq += it->first * it->first * it->second ;

	it_next = it + 1 ;

	is_candidate_to_store = kFALSE ;

	// Deciding whether it's time to finish this particular clustering process
	if ( it_next != (mValidHits[i][j]).end() ) {

	  // if the next one is not a neighbor --> a candidate cluster
	  if ( (it_next->first - it->first)!=1  ) 
	    is_candidate_to_store = kTRUE ;

	}
	else { 	// if already at the end --> a candidate cluster
	  is_candidate_to_store = kTRUE ;
	}

	if ( is_candidate_to_store == kTRUE ) {

	  //	  if ( NCluster_Length <= MAX_Cls_L && ECluster >= MIN_Charge ) {

	    // StEvent Clusters
	    StRpsCluster * oneStCluster = new StRpsCluster() ;

	    oneStCluster->setEnergy(ECluster);
	    oneStCluster->setLength(NCluster_Length);
	    position = POStimesE/ECluster ;
	    // K. Yip : Oct. 3, 2015 : Added positionRMS
	    positionRMS = POStimesESq/ECluster - position*position ; 
	    if ( positionRMS > 0 ) // protecting against possibly numbers very close to 0 which may be -ve
	      positionRMS = TMath::Sqrt( positionRMS ) ;
	    else
	      positionRMS = 0.0 ;

	    if ( (j % 2) == 0 ) { // A or C : pitch_4svx = 0.00974 cm
	      // K. Yip : Aug. 14, 2015 : 
	      // The plane E2D.A installed on Jan. 30, 2015 had an old BNL made silicon in it, where _all_ SVX channels were connected to the silicon and the pitch is smaller.
	      if ( ( mVersion == 2 ) && ( i == 3 ) && ( j == 0 ) ) { // Here the sequence nos. are from 0 to 7 (as they're from mValidHits arrays)
		pitch = kpitch_4svx2 ; // in m
	      }
	      else {
		pitch = kpitch_4svx ; // in m
	      }
	    }
	    else {               // B or D : pitch_6svx = 0.01050 cm
	      pitch = kpitch_6svx ; // in m
	    }

	    position = position*pitch ;
	    positionRMS = positionRMS*pitch ;

	    oneStCluster->setPosition(position); // in m
	    oneStCluster->setPositionRMS(positionRMS); // in m
	   
	    if ( mVersion < 2 )
	      oneStCluster->setXY( offset + orientations[4*i+j]*position ) ; // all in m
	    else
	      oneStCluster->setXY( offset + orientations2[4*i+j]*position ) ; // all in m

	    pp2ppColl->romanPot(i)->plane(j)->addCluster(oneStCluster);

	  //	  } 
	  /*
	  else
	    cout << "NOT Stored ! seq/chain : " << i+1 << "/" << j 
		 << " , length = " << NCluster_Length << " , energy = " << ECluster
		 << " , position = " << POStimesE/ECluster  << endl ;
	  */

	  ECluster = 0 ;
	  POStimesE = 0 ;
	  POStimesESq = 0 ;
	  NCluster_Length = 0 ;

	}

	it++ ;

      } // while

    } // for ( Int_t j=0; j<kMAXCHAIN; j++) {

  mEvent = (StEvent *) GetInputDS("StEvent");
  if ( mEvent ) {

   if ( mVersion>1 )
     MakeTracks(*pp2ppColl, mEvent->runInfo()->beamEnergy(StBeamDirection::blue), mEvent->runInfo()->beamEnergy(StBeamDirection::yellow) );

    // Store into StEvent
    mEvent->setRpsCollection(pp2ppColl);
  }
  else
    LOG_ERROR << "St_pp2pp_Maker : StEvent not found !" << endm ;
  
  return kStOk ;

}


//BEGIN  ------------------------ Rafal's code ------------------------


Int_t St_pp2pp_Maker::MakeTracks(StRpsCollection &RpsColl, float blue_beamenergy, float yellow_beamenergy) {

  vector< StRpsTrackPoint* > trackPointsVec[kMAXSEQ];
  vector< StRpsTrack* > trackVec;
  
  // reconstructing track-points
  formTrackPoints( RpsColl, trackPointsVec );
  
  // reconstructing tracks
  formTracks( &trackVec, trackPointsVec, blue_beamenergy, yellow_beamenergy );
      
  //filling StRpsCollection with track-points and tracks
  for(int i=0; i<kMAXSEQ; ++i){
    for(unsigned int j=0; j<trackPointsVec[i].size(); ++j){
      RpsColl.addTrackPoint( trackPointsVec[i][j] );
    }
  }
  for(unsigned int j=0; j<trackVec.size(); ++j){
    RpsColl.addTrack( trackVec[j] );
  }
  
  return kStOk ;

}


void St_pp2pp_Maker::formTracks( vector< StRpsTrack* > *trackVec, const vector< StRpsTrackPoint* > *trackPointVec, const float beamMomentumWest, const float beamMomentumEast ) const{

  // constants necessary for momentum reconstruction

  //------------ THOSE WILL BE READ FROM DATABASE ------------
  static const double alpha0 = 0.01886; // bending angle of DX magnet [rad]
  static const double d1 = 9.8; 	// distance from IP to DX magnet [m]
  static const double lDX = 3.7; 	// length of DX magnet [m]
  static const double vertexPosition[2] = { 0.0013, 0.0010 }; // average coordinates of vertex [m]
  //------------ -------------------------------- ------------
  
  for(int branch=0; branch<nBranches; ++branch){ // loop over all branches in the Roman Pot system

    int nPts[2]; // reading number of track-points found in the branch
    nPts[0] = trackPointVec[ RpInBranch[branch][0] ].size();
    nPts[1] = trackPointVec[ RpInBranch[branch][1] ].size();

    if( nPts[0] && nPts[1] ){ // if track-points reconstructed in both stations in branch

      for(int i=0; i<nPts[0]; ++i){ // loops over all combinations of track-points
	for(int j=0; j<nPts[1]; ++j){
	  StRpsTrack* track = new StRpsTrack();

	  track->setBranch( branch ); // setting ID of branch
	  track->setType( StRpsTrack::rpsGlobal ); // setting the type of the track
	  track->setTrackPoint( trackPointVec[ RpInBranch[branch][0] ][i], 0 ); // setting constituent track-points
	  track->setTrackPoint( trackPointVec[ RpInBranch[branch][1] ][j], 1 ); // setting constituent track-points

	  // below calculating momentum vector
	  double localThetaX = track->thetaRp( StRpsTrack::rpsAngleThetaX ); // REMINDER: sensitive to changes in StRpsTrack::thetaRp() !
	  double localThetaY = track->thetaRp( StRpsTrack::rpsAngleThetaY ); // REMINDER: sensitive to changes in StRpsTrack::thetaRp() !

	  double d2 = abs( trackPointVec[ RpInBranch[branch][0] ][i]->z() ) - lDX - d1; // distance from DX magnet exit to first RP station
	  double thetaX_IP = ( trackPointVec[ RpInBranch[branch][0] ][i]->x() - vertexPosition[X] - (d2 + 0.5*lDX)*localThetaX ) / ( d1 /* +/- z_IP */ + 0.5*lDX );
	  double xi = 1. / ( 1 + (alpha0*(d1 + 0.5*lDX /* +/- z_IP */)) / ( localThetaX*( abs( trackPointVec[ RpInBranch[branch][0] ][i]->z() ) /* +/- z_IP */ ) + vertexPosition[X] - trackPointVec[ RpInBranch[branch][0] ][i]->x() ) );
	  double momentumValue = (branch < nBranches/2 ? beamMomentumEast : beamMomentumWest ) * (1.-xi);

	  StThreeVectorF momentumVector( 0, 0, (branch < nBranches/2 ? -momentumValue : momentumValue ) );
	  momentumVector.rotateX( (branch < nBranches/2 ? localThetaY : -localThetaY ) );
	  momentumVector.rotateY( (branch < nBranches/2 ? -thetaX_IP : thetaX_IP ) );
	  track->setP( momentumVector ); // setting the momentum vector

	  trackVec->push_back( track ); // storing the track
	}
      }
    }
    else if( nPts[0] || nPts[1] ){ // if track-point reconstructed only in one station in branch

      int station = nPts[0] ? 0 : 1; // checking ID of station where track-point was found
      for(int i=0; i<nPts[station]; ++i){ // loop over all track-points
	StRpsTrack* track = new StRpsTrack();

	track->setBranch( branch ); // setting ID of branch
	track->setType( StRpsTrack::rpsLocal ); // setting the type of the track
	track->setTrackPoint( trackPointVec[ RpInBranch[branch][station] ][i], station ); // setting constituent track-point

	// below calculating momentum vector (assuming no momentum loss == elastic track)
	double localThetaX = ( trackPointVec[ RpInBranch[branch][station] ][i]->x() - vertexPosition[X] ) / abs( trackPointVec[ RpInBranch[branch][station] ][i]->z() /* +/- z_IP */ );
	double localThetaY = ( trackPointVec[ RpInBranch[branch][station] ][i]->y() - vertexPosition[Y] ) / abs( trackPointVec[ RpInBranch[branch][station] ][i]->z() /* +/- z_IP */ );
	double momentumValue = (branch < nBranches/2 ? beamMomentumEast : beamMomentumWest );

	StThreeVectorF momentumVector( 0, 0, (branch < nBranches/2 ? -momentumValue : momentumValue ) );
	momentumVector.rotateX( (branch < nBranches/2 ? localThetaY : -localThetaY ) );
	momentumVector.rotateY( (branch < nBranches/2 ? -localThetaX : localThetaX ) );
	track->setP( momentumVector ); // setting the momentum vector

	trackVec->push_back( track ); // storing the track
      }
    }

  }
}


void St_pp2pp_Maker::formTrackPoints(const StRpsCollection& RpsColl, vector< StRpsTrackPoint* > *trackPointVec ) const{

  for(int i=0; i<kMAXSEQ; ++i){ // loop over all Roman Pots

    // looking for hits in X and Y direction (necessary to determine (x,y) coordinates of track-point)
    vector<St_pp2pp_Maker::StRpsHit> hits[2];
    hits[Y] = formHits(RpsColl.romanPot(i), Y);
    if( hits[Y].size()==0 ) continue; // if no hits in planes A&C => cannot reconstruct a track-point
    hits[X] = formHits(RpsColl.romanPot(i), X);
    if( hits[X].size()==0 ) continue; // if no hits in planes B&D => cannot reconstruct a track-point

    // loops over all combinations of hits in X and Y directions
    for(unsigned int j=0; j<hits[X].size(); ++j){
      for(unsigned int k=0; k<hits[Y].size(); ++k){
	StRpsTrackPoint* trackPoint = new StRpsTrackPoint();

	// setting position of the track-point
	double x = hits[X][j].positionXY;
	double y = hits[Y][k].positionXY;
	double z = (hits[X][j].positionZ + hits[Y][k].positionZ)/2.;
	StThreeVectorF pos( x, y, z );
	trackPoint->setPosition( pos );

	// setting ID of Roman Pot
	trackPoint->setRpId( i );

	// setting IDs of clusters used to form a track-point
	for(int l=0; l<kMAXCHAIN/2; ++l){
	  trackPoint->setClusterId( hits[Y][k].clusterId[l], Planes[Y][l] );
	  trackPoint->setClusterId( hits[X][j].clusterId[l], Planes[X][l] );
	}

	// setting time of the hit (in time units)
	for(unsigned int pmt=0; pmt<trackPoint->mNumberOfPmtsInRp; ++pmt){
	  trackPoint->setTime( RpsColl.romanPot(i)->tac(pmt), pmt ); // TEMPORARILY using the raw TAC value !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	}

	// setting flag of track-point quality
	if( hits[X][j].golden && hits[Y][k].golden ) trackPoint->setQuality( StRpsTrackPoint::rpsGolden );
	else trackPoint->setQuality( StRpsTrackPoint::rpsNormal );

	// storing a track-point in a vector
	trackPointVec[i].push_back( trackPoint );
      }
    }

  }
}


vector<St_pp2pp_Maker::StRpsHit> St_pp2pp_Maker::formHits(const StRpsRomanPot* Rp, const int coordinate) const{

  vector<St_pp2pp_Maker::StRpsHit> hitVec;

  vector<double> pos[2];
  vector<int> en[2];
  vector<int> len[2];
  vector<int> id[2];

  preselectClusters(Rp, coordinate, pos, en, len, id);
  int clCase = classifyClustersCase(pos);

  if(clCase>0){
    int validClusters[2];
    double deltaPitches;
    bool matched = matchClusters(coordinate, clCase, pos, validClusters, &deltaPitches);

    if( matched ){
      St_pp2pp_Maker::StRpsHit hit;
      hit.positionXY = ( (validClusters[0]<0 ? pos[1][validClusters[1]] : pos[0][validClusters[0]]) + (validClusters[1]<0 ? pos[0][validClusters[0]] : pos[1][validClusters[1]]) )/2;
      hit.positionZ = ( (validClusters[0]<0 ? Rp->plane(Planes[coordinate][1])->z() : Rp->plane(Planes[coordinate][0])->z()) + (validClusters[1]<0 ? Rp->plane(Planes[coordinate][0])->z() : Rp->plane(Planes[coordinate][1])->z()) )/2;
      for(int j=0; j<kMAXCHAIN/2; ++j){
	hit.clusterId[j] = validClusters[j]<0 ? -1 : id[j][validClusters[j]];
      }
      if(clCase==5) hit.golden = true; // golden hit -> 1/1
      else hit.golden = false;

      hitVec.push_back( hit );
      return hitVec;
    }
  }

  return hitVec;
}


void St_pp2pp_Maker::preselectClusters(const StRpsRomanPot* Rp, const int coordinate, vector<double>* pos, vector<int>* en, vector<int>* len, vector<int>* id) const{
  for(int j=0; j<kMAXCHAIN/2; ++j){ // loop over planes measuring given _coordinate_
    const StRpsPlane* SiPlane = Rp->plane( Planes[coordinate][j] );
    int nClusters = SiPlane->numberOfClusters();
    if(nClusters < maxNumberOfClusterPerPlane) // continue only if nClusters is small, otherwise plane is not used in reconstruction
      for(int k=0; k < nClusters; ++k){ // loop over clusters in this plane
	const StRpsCluster* Cluster = SiPlane->cluster( k );
	int lenCluster = Cluster->length();
	if(lenCluster <= maxClusterLength && lenCluster>0){
	  int enCluster = Cluster->energy();
	  if(enCluster >= Emin[ Rp->romanPotId() ][lenCluster-1]){ // allow using this cluster only if it passes the energy cut
	    pos[j].push_back( Cluster->xy() );
	    en[j].push_back( enCluster );
	    len[j].push_back( lenCluster );
	    id[j].push_back( k );
	  }
	}
      }
  }
}


Int_t St_pp2pp_Maker::classifyClustersCase(vector<double>* pos) const{
  int lA = pos[0].size();
  int lB = pos[1].size();

  if( lA==0 && lB==0 )	return -1; else
  if( lA==1 && lB==1 )	return 5; else
  if( lA==1 && lB >1 )	return 6; else
  if( lA >1 && lB==1 )	return 7; else
  if( lA==0 && lB==1 )	return 2; else
  if( lA==1 && lB==0 )	return 1; else
  if( lA==2 && lB==2 )	return 8; else
  if( lA==0 && lB >1 )	return -4; else
  if( lA >1 && lB==0 )	return -3; else
  if( lA>=2 && lB>=2 )	return 9; else
  return -100;
}


Bool_t St_pp2pp_Maker::matchClusters(const int coordinate, const int clCase, const vector<double>* pos, int* validClusters, double* deltaPitches) const{
  switch(clCase){
    case 5: if( areMatched(coordinate, pos[0][0], pos[1][0], deltaPitches) ){
	      validClusters[0] = 0;
	      validClusters[1] = 0;
	      return true;
	    } break;
    case 2:
    case 1: validClusters[0] = clCase==2 ? -1 : 0;
	    validClusters[1] = clCase==2 ? 0 : -1;
	    *deltaPitches = -1e9;
	    return true;
    case 6:
    case 7:
    case 8:
    case 9: {int nOfMatchingCl = 0;
	    double DeltaPosition = 1e9;
	    double minDeltaPosition = 1e9;
	    int index1 = 0;
	    int index2 = 0;
	    for(unsigned int c1=0; c1<pos[0].size(); ++c1){
	      for(unsigned int c2=0; c2<pos[1].size(); ++c2){
		if(areMatched(coordinate, pos[0][c1], pos[1][c2], &DeltaPosition)){
		  ++nOfMatchingCl;
		  if(abs(DeltaPosition) < minDeltaPosition){
		    minDeltaPosition = DeltaPosition;
		    index1 = c1;
		    index2 = c2;
		  }
		}
	      }
	    }
	    if(nOfMatchingCl>0){
	      validClusters[0] = index1;
	      validClusters[1] = index2;
	      *deltaPitches = minDeltaPosition;
	      return true;
	    } else return false;}
    default: return false;
  }
  return false;
}


Bool_t St_pp2pp_Maker::areMatched(const int coordinate, const double p1, const double p2, double *deltaPitches) const{
  if(deltaPitches) *deltaPitches = (p1 - p2) / Pitch[coordinate];
  return  abs( p1 - p2 ) < maxPitchesToMatch*Pitch[coordinate] ? true : false;
}



const double St_pp2pp_Maker::Emin[8][5] ={{20, 20, 20, 20, 20},
					  {20, 20, 20, 20, 20},
					  {20, 20, 20, 20, 20},
					  {20, 20, 20, 20, 20},
					  {20, 20, 20, 20, 20},
					  {20, 20, 20, 20, 20},
					  {20, 20, 20, 20, 20},
					  {20, 20, 20, 20, 20}};

const int St_pp2pp_Maker::Planes[2][2] = {{1, 3},  // local x (vertical strips)
					  {0, 2}}; // local y (horizontal strips)

const int St_pp2pp_Maker::RpInBranch[4][2] = {{0, 2}, {1, 3},
					      {4, 6}, {5, 7}};

//END  ------------------------ Rafal's code ------------------------











Int_t St_pp2pp_Maker::Finish() {


  return StMaker::Finish();

}


