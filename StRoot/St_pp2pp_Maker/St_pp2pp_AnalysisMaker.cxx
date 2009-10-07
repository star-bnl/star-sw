#include "St_pp2pp_AnalysisMaker.h"
#include "TDataSetIter.h"
//#include "TGenericTable.h"
//#include "StEvent/StEvent.h"
//#include "StEvent/StTriggerData.h"
#include "StRtsTable.h"
#include "RTS/src/DAQ_PP2PP/pp2pp.h"

//#include "StDaqLib/TRG/trgStructures2009.h"
#include "StEvent/StTriggerData2009.h"

#include "TTree.h"
#include "TFileIter.h"
#include "TFile.h"

ClassImp(St_pp2pp_AnalysisMaker)

St_pp2pp_AnalysisMaker::St_pp2pp_AnalysisMaker(const char *name) : StRTSBaseMaker("pp2pp",name), // "pp2pp" is needed to specify the detector
								   fTreeFile(0),fClusterTree(0),nevt_count(0)
  //								   fTreeFile(0),fClusterTree(0),fBuffer(0)
{  
   // ctor 
}


St_pp2pp_AnalysisMaker::~St_pp2pp_AnalysisMaker() {  
}

//_____________________________________________________________________________
/// Init - is a first method the top level StChain calls to initialize all its makers 
Int_t St_pp2pp_AnalysisMaker::Init() {

  // create the TTree 
  fTreeFile    = new TFile("pp2ppClustersTree.root","RECREATE");
  fClusterTree = new TTree("pp2pp",GetName());

  fClusterTree->Branch("xing", &xing,"xing_lo/i:xing_hi:bunch_number:b120");

  fClusterTree->Branch("event_info", &event_info,"run_number/i:event_number:seq:daqbits:token");

  fClusterTree->Branch("tcubits", &tcubits,"tcubits/s");
  fClusterTree->Branch("silicon_bunch", &silicon_bunch,"silicon_bunch/b");


  char rpname[St_pp2pp_Maker::MAXSEQ][5] = { "EHI", "EHO", "EVU", "EVD", "WHI", "WHO", "WVD", "WVU" };
  char title[25], format[35];

  for ( int s=0; s<St_pp2pp_Maker::MAXSEQ ; s++)
    for ( int c=0; c<St_pp2pp_Maker::MAXCHAIN; c++) {

      sprintf(title,"%s.ch%d_ncls", rpname[s], c);
      sprintf(format,"%s.ch%d_ncls/I", rpname[s], c);
      fClusterTree->Branch(title, &(allclusters[s][c].nclusters), format) ;

      // Oct. 7, 2009 :
      // I'm not able to put them (length, position ... ) in one branch because
      // 1. length (of char) has different byte counts from others (of double)
      // 2. Even if we make length to be of double, the no. of the elements in each event is most probably different from the array size in the struct (MAXClusters)
      // ==> energy may be considered as length and so on.

      sprintf(title,"%s.ch%d_length", rpname[s], c);
      sprintf(format,"%s.ch%d_length[%s.ch%d_ncls]/b", rpname[s], c, rpname[s], c);
      fClusterTree->Branch(title, allclusters[s][c].length, format) ;
    
      sprintf(title,"%s.ch%d_position", rpname[s], c);
      sprintf(format,"%s.ch%d_position[%s.ch%d_ncls]/D", rpname[s], c, rpname[s], c);
      fClusterTree->Branch(title, allclusters[s][c].position, format) ;

      sprintf(title,"%s.ch%d_energy", rpname[s], c);
      sprintf(format,"%s.ch%d_energy[%s.ch%d_ncls]/D", rpname[s], c, rpname[s], c);
      fClusterTree->Branch(title, allclusters[s][c].energy, format) ;

      sprintf(title,"%s.ch%d_x", rpname[s], c);
      sprintf(format,"%s.ch%d_x[%s.ch%d_ncls]/D", rpname[s], c, rpname[s], c);
      fClusterTree->Branch(title, allclusters[s][c].x, format) ;

      sprintf(title,"%s.ch%d_y", rpname[s], c);
      sprintf(format,"%s.ch%d_y[%s.ch%d_ncls]/D", rpname[s], c, rpname[s], c);
      fClusterTree->Branch(title, allclusters[s][c].y, format) ;

      sprintf(title,"%s.ch%d_z", rpname[s], c);
      sprintf(format,"%s.ch%d_z[%s.ch%d_ncls]/D", rpname[s], c, rpname[s], c);
      fClusterTree->Branch(title, allclusters[s][c].z, format) ;

  }

  fClusterTree->Branch("P2P", &P2P, "RPWVD2_ADC/s:RPWVD1_ADC:RPWVU2_ADC:RPWVU1_ADC:RPEVD2_ADC:RPEVD1_ADC:RPEVU2_ADC:RPEVU1_ADC:RPWVD2_TAC:RPWVD1_TAC:RPWVU2_TAC:RPWVU1_TAC:RPEVD2_TAC:RPEVD1_TAC:RPEVU2_TAC:RPEVU1_TAC:RPWHI2_ADC:RPWHI1_ADC:RPWHO2_ADC:RPWHO1_ADC:RPEHI2_ADC:RPEHI1_ADC:RPEHO2_ADC:RPEHO1_ADC:RPWHI2_TAC:RPWHI1_TAC:RPWHO2_TAC:RPWHO1_TAC:RPEHI2_TAC:RPEHI1_TAC:RPEHO2_TAC:RPEHO1_TAC");

  return StMaker::Init();
}

//_____________________________________________________________________________
/// Clear - this method is called in loop for prepare the maker for the next event
void  St_pp2pp_AnalysisMaker::Clear(Option_t *) {
  StMaker::Clear(); // perform the basic clear (mandatory)
}


//_____________________________________________________________________________
/// Make - this method is called in loop for each event
Int_t St_pp2pp_AnalysisMaker::Make(){

  int s, c, k ; 

  Bool_t NotGotIt = kTRUE ;

  event_info.run_number = GetRunNumber() ;
  event_info.event_number = GetEventNumber() ;
  event_info.seq = GetIventNumber() ;

  while ( GetNextAdc() && NotGotIt ) {
    event_info.token = Token();
    event_info.daqbits = Daqbits();    

    TGenericTable::iterator iword = DaqDta()->begin();
    if ( iword != DaqDta()->end() ) {
      //        pp2pp_t &d = *(pp2pp_t *)*iword;
      //        silicon_bunch = d.bunch_xing ;
      silicon_bunch = ( (pp2pp_t *) *iword )->bunch_xing ;
    }
    NotGotIt = kFALSE ;
  }
  
  if ( event_info.token == 0 ) return kStSKIP ; //

  memset( allclusters, 0, sizeof(allclusters) ) ;
  /*
  for ( s=0; s<St_pp2pp_Maker::MAXSEQ ; s++ )
    for ( c=0; c<St_pp2pp_Maker::MAXCHAIN ; c++ )
      memset(allclusters[s][c],0, sizeof(allclusters[s][c]));
      //      (allclusters[s][c]).nclusters = 0 ;
  */

  //
  //  PrintInfo();
  //
  TGenericTable *pp2ppRawHits = (TGenericTable *)GetDataSet("pp2ppRawHits");

  TObjectSet *os = (TObjectSet*)GetDataSet("StTriggerData");
  if (os) {
    StTriggerData* trg_p = (StTriggerData*)os->GetObject();

    if(trg_p){

      tcubits = trg_p->tcuBits() ;

      //      cout << "pp2pp : " << trg_p->pp2ppADC(east,0,0,0) << endl;

      P2P.RPEVU1_ADC = (u_short) trg_p->pp2ppADC( (StBeamDirection) 0,0,0,0)  ;
      P2P.RPEVU2_ADC = (u_short) trg_p->pp2ppADC( (StBeamDirection) 0,0,0,1)  ;
      P2P.RPEVD1_ADC = (u_short) trg_p->pp2ppADC( (StBeamDirection) 0,0,1,0)  ;
      P2P.RPEVD2_ADC = (u_short) trg_p->pp2ppADC( (StBeamDirection) 0,0,1,1)  ;

      P2P.RPEVU1_TAC = (u_short) trg_p->pp2ppTAC( (StBeamDirection) 0,0,0,0)  ;
      P2P.RPEVU2_TAC = (u_short) trg_p->pp2ppTAC( (StBeamDirection) 0,0,0,1)  ;
      P2P.RPEVD1_TAC = (u_short) trg_p->pp2ppTAC( (StBeamDirection) 0,0,1,0)  ;
      P2P.RPEVD2_TAC = (u_short) trg_p->pp2ppTAC( (StBeamDirection) 0,0,1,1)  ;


      P2P.RPWVU1_ADC = (u_short) trg_p->pp2ppADC( (StBeamDirection) 1,0,0,0)  ;
      P2P.RPWVU2_ADC = (u_short) trg_p->pp2ppADC( (StBeamDirection) 1,0,0,1)  ;
      P2P.RPWVD1_ADC = (u_short) trg_p->pp2ppADC( (StBeamDirection) 1,0,1,0)  ;
      P2P.RPWVD2_ADC = (u_short) trg_p->pp2ppADC( (StBeamDirection) 1,0,1,1)  ;

      P2P.RPWVU1_TAC = (u_short) trg_p->pp2ppTAC( (StBeamDirection) 1,0,0,0)  ;
      P2P.RPWVU2_TAC = (u_short) trg_p->pp2ppTAC( (StBeamDirection) 1,0,0,1)  ;
      P2P.RPWVD1_TAC = (u_short) trg_p->pp2ppTAC( (StBeamDirection) 1,0,1,0)  ;
      P2P.RPWVD2_TAC = (u_short) trg_p->pp2ppTAC( (StBeamDirection) 1,0,1,1)  ;


      P2P.RPEHO1_ADC = (u_short) trg_p->pp2ppADC( (StBeamDirection) 0,1,0,0)  ;
      P2P.RPEHO2_ADC = (u_short) trg_p->pp2ppADC( (StBeamDirection) 0,1,0,1)  ;
      P2P.RPEHI1_ADC = (u_short) trg_p->pp2ppADC( (StBeamDirection) 0,1,1,0)  ;
      P2P.RPEHI2_ADC = (u_short) trg_p->pp2ppADC( (StBeamDirection) 0,1,1,1)  ;

      P2P.RPEHO1_TAC = (u_short) trg_p->pp2ppTAC( (StBeamDirection) 0,1,0,0)  ;
      P2P.RPEHO2_TAC = (u_short) trg_p->pp2ppTAC( (StBeamDirection) 0,1,0,1)  ;
      P2P.RPEHI1_TAC = (u_short) trg_p->pp2ppTAC( (StBeamDirection) 0,1,1,0)  ;
      P2P.RPEHI2_TAC = (u_short) trg_p->pp2ppTAC( (StBeamDirection) 0,1,1,1)  ;


      P2P.RPWHO1_ADC = (u_short) trg_p->pp2ppADC( (StBeamDirection) 1,1,0,0)  ;
      P2P.RPWHO2_ADC = (u_short) trg_p->pp2ppADC( (StBeamDirection) 1,1,0,1)  ;
      P2P.RPWHI1_ADC = (u_short) trg_p->pp2ppADC( (StBeamDirection) 1,1,1,0)  ;
      P2P.RPWHI2_ADC = (u_short) trg_p->pp2ppADC( (StBeamDirection) 1,1,1,1)  ;

      P2P.RPWHO1_TAC = (u_short) trg_p->pp2ppTAC( (StBeamDirection) 1,1,0,0)  ;
      P2P.RPWHO2_TAC = (u_short) trg_p->pp2ppTAC( (StBeamDirection) 1,1,0,1)  ;
      P2P.RPWHI1_TAC = (u_short) trg_p->pp2ppTAC( (StBeamDirection) 1,1,1,0)  ;
      P2P.RPWHI2_TAC = (u_short) trg_p->pp2ppTAC( (StBeamDirection) 1,1,1,1)  ;

      xing.xing_lo = trg_p->bunchCounterLow();
      xing.xing_hi = trg_p->bunchCounterHigh();
      xing.bunch_number = trg_p->bunchId7Bit() ;
      xing.b120 = trg_p->bunchId48Bit() ;

    }
  }


  if ( nevt_count%1000 == 0 ) cout << "St_pp2pp_AnalysisMaker:: Event count : " << nevt_count << ", Run : " << event_info.run_number 
				  << ", Event no.: " << event_info.event_number << ", Token : " << event_info.token <<  ", Daqbits : " << event_info.daqbits 
				  << ", tcubits : " << tcubits << endl ;
  nevt_count++ ;


  if (pp2ppRawHits) {
      //     pp2ppRawHits->Print(0,3);
  } else {
     LOG_WARN << " Chain has produced no pp2pp data" << endm;
  } 

  TGenericTable *Tclusters = (TGenericTable *)GetDataSet("pp2ppClusters");

  pp2ppCluster_st one_cluster ;
  if ( Tclusters ) {

    //    Tclusters->Print(0,8);
    for ( int i=0; i< Tclusters->GetNRows(); i++ ) {
      one_cluster = *( (pp2ppCluster_st*) Tclusters->At(i) ) ;

      //      cout << "Position : " << one_cluster.position << " , Energy : " << one_cluster.energy << " , Length = " << (int) one_cluster.length << endl ;

      s = one_cluster.sequencer - 1 ;
      c = one_cluster.chain ;
      k = allclusters[s][c].nclusters ;

      if ( k < MAXClusters ) {
	  
	allclusters[s][c].length[k] = one_cluster.length ;
	allclusters[s][c].position[k] = one_cluster.position ;
	allclusters[s][c].energy[k] = one_cluster.energy ;
	allclusters[s][c].x[k] = one_cluster.x ;
	allclusters[s][c].y[k] = one_cluster.y ;
	allclusters[s][c].z[k] = one_cluster.z ;
	(allclusters[s][c].nclusters)++ ;

      }

    } 

  }

  fClusterTree->Fill();    

  return kStOK;

}

//_____________________________________________________________________________
Int_t St_pp2pp_AnalysisMaker::Finish() {

  if (fTreeFile) {
    fTreeFile->Write();
    fTreeFile->Close();
    delete fTreeFile; 
    fTreeFile = 0;
  }

  return StMaker::Finish();

}

