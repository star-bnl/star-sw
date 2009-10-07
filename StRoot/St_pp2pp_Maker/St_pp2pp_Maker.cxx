#include "St_pp2pp_Maker.h"
#include "StRtsTable.h"
#include "TDataSetIter.h"
#include "RTS/src/DAQ_PP2PP/pp2pp.h"

#ifdef NO_GLOBAL_VARIABLE_IS_ALLOWED
// Silicon stuff
// const Int_t MAXSEC = 2 ;  // 2 sides
 ! const Int_t MAXCHAIN = 4 ;
 ! const Int_t MAXSVX = 6 ;
 ! const Int_t MAXSEQ = 8 ;
 ! static TTree *PP2PPTree =  0 ;


// --------- End of Root Tree stuff ----------

  ! const int ErrorCode = -999 ;
  ! int fLast_svx = ErrorCode, fLast_chain = ErrorCode, fLast_seq = ErrorCode ;
#endif

// struct pp2ppRawHit oneSihit ;

ClassImp(St_pp2pp_Maker)

  St_pp2pp_Maker::St_pp2pp_Maker(const char *name) : StRTSBaseMaker("pp2pp",name),   
						     pedestal_perchannel_filename("pedestal.in.perchannel"), LDoCluster(kTRUE) {
  // ctor
}


//_____________________________________________________________________________
/// This is TLA destructor
/*!
  The first comment lines after the opening bracket
  ({) of a member function are considered as a member function description 
  
  The first comment lines after the opening bracket
  ({) of a member function are considered as a member function description 
  see: <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A> 

 */
St_pp2pp_Maker::~St_pp2pp_Maker() {
  // 

  /*
  for ( Int_t i=0; i<MAXSEQ; i++) 
    for ( Int_t j=0; j<MAXCHAIN; j++)
      for ( Int_t k=0; k<MAXSVX ; k++) {
	delete [] pedave[i][j][k] ; pedave[i][j][k] = 0 ;
	delete [] pedrms[i][j][k] ; pedrms[i][j][k] = 0 ;
      }
  */

}

//_____________________________________________________________________________
/// Init - is a first method the top level StChain calls to initialize all its makers 
Int_t St_pp2pp_Maker::Init() {
  fLast_svx   = ErrorCode;
  fLast_chain = ErrorCode;
  fLast_seq   = ErrorCode ;
  if ( LDoCluster ) read_pedestal_perchannel() ;
  return StMaker::Init();
}

Int_t St_pp2pp_Maker::read_pedestal_perchannel() {

  Int_t i, j, k, l, ncounts = 0 ;
  
  //  cout << "Size of pedave : " << sizeof(pedave) << " , Size of pedrms : " << sizeof(pedrms) << endl ;

  memset(pedave,0,sizeof(pedave));
  memset(pedrms,0,sizeof(pedrms));

  /*
  for ( i=0; i<MAXSEQ; i++) 
    for ( j=0; j<MAXCHAIN; j++)
      for ( k=0; k<MAXSVX ; k++) {
	pedave[i][j][k] = new Double_t[MAXSTRIP] ;
	pedrms[i][j][k] = new Double_t[MAXSTRIP] ;
      }
  */


  ifstream ipedestal(pedestal_perchannel_filename.c_str(), ifstream::in);

  Double_t mean, rms ;
  string linestring ;

  while ( ipedestal.good() && !ipedestal.eof() && ipedestal.peek() != EOF ) {
    getline(ipedestal, linestring,'\n');
    istringstream linestore(linestring);
    linestore >> i >> j >> k >> l >> mean >> rms ;
    if ( i >MAXSEQ || j>=MAXCHAIN || k>=MAXSVX || l>=MAXSTRIP) 
      LOG_WARN << " sequence[1-8] = " << i << ", chain = " << j << ", svx = " << k << ", channel = " << l << " : " << mean << " " << rms << endm ;

    pedave[i-1][j][k][l] = mean ;
    pedrms[i-1][j][k][l] = rms ;

    //    cout << i << " " << j << " " << k << " " << pedave[i-1][j][k][l] << endl ;
    ncounts++ ;
  } ;

  cout << ncounts << " pedestal lines read. " << endl ;

  ipedestal.close();

  return 0 ;

}



//_____________________________________________________________________________
/// Clear - this method is called in loop for prepare the maker for the next event
void  St_pp2pp_Maker::Clear(Option_t *) {

  // Deleting previous cluster info.
  for ( Int_t i=0; i<MAXSEQ; i++)
    for ( Int_t j=0; j<MAXCHAIN; j++)
      (validhits[i][j]).clear();

  StMaker::Clear(); // perform the basic clear (mandatory)

}


//_____________________________________________________________________________
/// Make - this method is called in loop for each event
Int_t St_pp2pp_Maker::Make(){

  //
  //  PrintInfo();
  //
  int counter = -1; 
  
  TGenericTable *pp2ppRawHits = new TGenericTable("pp2ppRawHit_st","pp2ppRawHits");

  //  ls (0);

  // Each GetNextAdc would get a SVX ...
  while ( GetNextAdc() ) {
     counter++;
       
     TGenericTable::iterator iword = DaqDta()->begin();
     for (;iword != DaqDta()->end();++iword) {
        pp2pp_t &d = *(pp2pp_t *)*iword;
        // do something
        DoerPp2pp(d,*pp2ppRawHits);
     }
  }


  if (counter < 0) {
    LOG_INFO << "There was no pp2pp data for this event. " << endm;
  } else {
    LOG_INFO << "End of pp2pp data for this event : " << GetEventNumber() << ", Total = "  << counter+1 
	     << " records were found" << endm;
  }

  AddData(pp2ppRawHits);  // publish RawHits to make it available for other makers in the chain
  // one may not call AddData if the result should not be published.
  // to discard the result one should call  "delete pp2ppRawHits"


  if ( LDoCluster ) { 

    for ( Int_t i=0; i<MAXSEQ; i++)
      for ( Int_t j=0; j<MAXCHAIN; j++) {
	sort( (validhits[i][j]).begin(), (validhits[i][j]).end(), hitcompare);
	//	cout << "Size of vector of sequencer " << i+1 << " chain " << j << " " << dec << (validhits[i][j]).size() << endl ;
      }

    TGenericTable *pp2ppClusters = new TGenericTable("pp2ppCluster_st","pp2ppClusters");

    MakeClusters(*pp2ppClusters);

    AddData(pp2ppClusters);  

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
  // The "known case" should be kept in Db (calibration Db is Ok) and available from there for the
  // current event. I'll show that next time ;-). It is a job of StDbMaker
  // see $STAR/StarDb/tpc/tptpars/tpt_pars

  if ( (oneSihit.svx != fLast_svx) && (fLast_svx != ErrorCode) ) {

    if (  Int_t(oneSihit.svx-1) != fLast_svx )

      if (  ( (oneSihit.svx-fLast_svx) != -3 && ( (oneSihit.chain%2)==1 ) ) ||
	    ( (oneSihit.svx-fLast_svx) != -5 && ( (oneSihit.chain%2)==0 ) ) ) {

	if ( oneSihit.svx == 7 && oneSihit.sequencer == 3 && oneSihit.chain == 2 )
	  oneSihit.svx = 3 ;
	//		  else if ( oneSihit.svx < fLast_svx ) {
	else if ( oneSihit.svx < fLast_svx && ( GetRunNumber()<10185015 || (fLast_seq!=2 && fLast_chain!=2)) ) { // bad seq 2 and chain D

	  LOG_WARN << "Decreased ? " <<  GetEventNumber() << " : fLast_seq = " << fLast_seq << ", fLast_chain = " << fLast_chain << ", fLast_svx = " << fLast_svx << endm ;
	  LOG_WARN << "Decreased ?  " << GetEventNumber() << " : Now, seq = " << (int) oneSihit.sequencer << ", chain = " << (int) oneSihit.chain << ", svx = " << (int) oneSihit.svx << endm ;
	  
	  oneSihit.svx = fLast_svx + 1 ;
		    
	  LOG_WARN << "Decreased ? : So -> " << " svx is now = " << (int) oneSihit.svx << endm ;	      

	}
	//	else if ( fLast_seq!=2 && fLast_chain!=2 ) { // bad seq 2 and chain D
	else if ( GetRunNumber()<10185015 || ( fLast_seq!=2 && fLast_chain!=2 ) ) { // bad seq 2 and chain D

	  LOG_WARN << GetEventNumber() << " : fLast_seq = " << fLast_seq << ", fLast_chain = " << fLast_chain << ", fLast_svx = " << fLast_svx << endm ;
	  LOG_WARN << GetEventNumber() << " : Now, seq = " << (int) oneSihit.sequencer << ", chain = " << (int) oneSihit.chain << ", svx = " << (int) oneSihit.svx << endm ;

	}

      }
	      

  }
  else if ( (oneSihit.chain==fLast_chain) && (fLast_chain != ErrorCode) ) {
    LOG_WARN << "Repeated ? :" << GetEventNumber() << " : fLast_seq = " << fLast_seq << ", fLast_chain = " << fLast_chain << ", fLast_svx = " << fLast_svx << endm ;
    LOG_WARN << "Repeated ? : " << GetEventNumber() << " : Now, seq = " << (int) oneSihit.sequencer << ", chain = " << (int) oneSihit.chain << ", svx = " << (int) oneSihit.svx << endm ;

    oneSihit.svx = fLast_svx + 1 ;

    LOG_WARN << "Repeated : So -> " << " svx is now = " << (int) oneSihit.svx << endm ;	      
  }


  fLast_seq = oneSihit.sequencer; 
  fLast_chain = oneSihit.chain;
  fLast_svx = oneSihit.svx;

  //  cout << "Seq: " << fLast_seq << " , chain " << fLast_chain << ", SVX = " << fLast_svx << endl ;

  for(unsigned int c=0;c<sizeof(d.adc);c++) {
    //	      if( d.adc[c] ) printf("   %3d: %3d [0x%02X]\n",c,d.adc[c],d.adc[c]) ;
    //	      adc[nsvx][c] = d.adc[c];
    if ( d.trace[c] == 1 ) {
      oneSihit.channel = c ;
      oneSihit.adc = d.adc[c];
      hitsTable.AddAt(&oneSihit);

      //      cout << "channel " << c << " , adc " << (int) d.adc[c] << endl ;

      if ( LDoCluster && (c != 127) && (c != 0) ) { // Avoid the channels at 2 ends of SVX
	
	// Getting rid of the 1st channel (0) and the last channel (127)
	onehit.first = fLast_svx*(MAXSTRIP-2) + oneSihit.channel - 1  ; 

	onehit.second = oneSihit.adc -  pedave[fLast_seq-1][fLast_chain][fLast_svx][oneSihit.channel] ;

	if ( onehit.second > 5*pedrms[fLast_seq-1][fLast_chain][fLast_svx][oneSihit.channel] ) {
	  (validhits[fLast_seq-1][fLast_chain]).push_back(onehit);
	  //	  cout << "validhits : position " << onehit.first << " , energy " << onehit.second << endl ;
	}
      }

    }
    else if ( d.trace[c] != 0 )
      std::cout << GetEventNumber() << " : trace = " << (Int_t) d.trace[c] << ", Seq " << (Int_t) oneSihit.sequencer 
		<< ", chain " << (Int_t) oneSihit.chain << ", SVX " << (Int_t) oneSihit.svx << ", channel " << c 
		<< " is duplicated ? ==> " << (Int_t) d.adc[c] << std::endl ;
  }

  return 1;

}

Int_t St_pp2pp_Maker::MakeClusters(TGenericTable &clustersTable) {

  const Int_t MAX_Cls_L = 5 ;
  const Int_t MIN_Charge = 20 ;
  Bool_t is_candidate_to_store ;

  Int_t NCluster_Length ;
  Double_t ECluster, POStimesE ;

  pp2ppCluster_st one_cluster = { 0, 0, 0, 0.0E0, 0.0E0, 0.0E0, 0.0E0 , 0.0E0 };

  vector< HitChannel >::iterator it, it_next ;

  for ( Int_t i=0; i<MAXSEQ; i++)
    for ( Int_t j=0; j<MAXCHAIN; j++) {

      NCluster_Length = 0 ;
      ECluster = 0 ;
      POStimesE = 0 ;

      it = (validhits[i][j]).begin() ;

      while ( it != (validhits[i][j]).end() ) {

	//	cout << "Seq: " << i+1 << " , chain " << j << ", channel : " << it->first << " , energy : " << it->second << endl ;
	NCluster_Length++ ;
	ECluster += it->second ;
	POStimesE += it->first*it->second ;
	
	it_next = it + 1 ;

	is_candidate_to_store = kFALSE ;

	// Deciding whether it's time to finish this particular clustering process
	if ( it_next != (validhits[i][j]).end() ) {

	  // if the next one is not a neighbor --> a candidate cluster
	  if ( (it_next->first - it->first)!=1  ) 
	    is_candidate_to_store = kTRUE ;

	}
	else { 	// if already at the end --> a candidate cluster
	  is_candidate_to_store = kTRUE ;
	}

	if ( is_candidate_to_store == kTRUE ) {

	  if ( NCluster_Length <= MAX_Cls_L && ECluster >= MIN_Charge ) {
	    one_cluster.sequencer = i+1 ; 
	    one_cluster.chain = j ;
	    one_cluster.position = POStimesE/ECluster  ;
	    one_cluster.energy = ECluster ;
	    one_cluster.length = NCluster_Length ;
	    
	    clustersTable.AddAt(&one_cluster);    

	    /*
	    cout << "Stored ! seq/chain : " << i+1 << "/" << j
		 << " , length = " << (int) one_cluster.length << " , energy = " << one_cluster.energy
		 << " , position = " << one_cluster.position << endl ;
	    */
	  } 
	  /*
	  else
	    cout << "NOT Stored ! seq/chain : " << i+1 << "/" << j 
		 << " , length = " << NCluster_Length << " , energy = " << ECluster
		 << " , position = " << POStimesE/ECluster  << endl ;
	  */

	  ECluster = 0 ;
	  POStimesE = 0 ;
	  NCluster_Length = 0 ;

	}

	it++ ;

      } // while

    } // for ( Int_t j=0; j<MAXCHAIN; j++) {

  return 1 ;

}


Int_t St_pp2pp_Maker::Finish() {


  return StMaker::Finish();

}


