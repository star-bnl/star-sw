void Fix( )
{ 

  gROOT   ->  LoadMacro("bfc.C") ;
  bfc(-1,"NoDefault tpcDB detDb") ; 

  // Create the makers to be called by the current chain
 //  StChain           *chain    =  new StChain() ;
//   St_db_Maker       *dbMk     =  new St_db_Maker("db", 
 
// "MySQL:StarDb","$STAR/StarDb","StarDb") ;
//   StTpcDbMaker      *tpcDbMk  =  new StTpcDbMaker("tpcDb");
//   StDetectorDbMaker *detDbMk  =  new StDetectorDbMaker( ) ; 

  Int_t idate = 20011119 ;  Int_t itime = 102800 ;
  StEvtHddr *hd = (StEvtHddr*)chain->DataSet("EvtHddr");
  hd->SetRunNumber(-99);
  St_db_Maker       *dbMk     = (St_db_Maker       *) chain->GetMaker("db");
  dbMk  -> SetDateTime(idate,itime) ;  
  chain -> Init() ;      
  chain -> Make() ;     
}
