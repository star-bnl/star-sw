//*-- Author : Dominik Flierl
// 
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// This Maker reads l3 data as they come with the raw data stream       //
// from the experiment and fills them into StEvent or into a            //
// TTree Structure which can then be used as a l3 Mini DST              //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "Stl3RawReaderMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StDAQMaker/StDAQReader.h"
#include "StEventTypes.h"
#include "TTree.h"
#include "Stl3MiniEvent.h"
#include "Rtypes.h"
#include "TMath.h"
#include "TH1.h"
#include "TF1.h"

#include "St_l3_Coordinate_Transformer.h"
#include "St_l3_Coordinates.h"

ClassImp(Stl3RawReaderMaker)

//_____________________________________________________________________________
Stl3RawReaderMaker::Stl3RawReaderMaker(const char *name):StMaker(name){
 //  l3RawReader constructor
}
//_____________________________________________________________________________
Stl3RawReaderMaker::~Stl3RawReaderMaker(){
}
//_____________________________________________________________________________
Int_t Stl3RawReaderMaker::Init(){
  //  Init - is a first method the top level StChain calls to initialize all its makers

  // Make Connection to raw data
  DAQReaderSet = GetDataSet("StDAQReader") ; 
 
  // set switches
  WriteMiniEvent = 0 ;
  WriteStEvent   = 1 ;

  // create TTree and Branch
  if (WriteMiniEvent == 1)
    {    
      mGlobalTrackTree = new TTree("L3GTracks","L3Globaltracks") ;
      mL3Event = new Stl3MiniEvent() ;
      mGlobalTrackTree->Branch("L3Event","Stl3MiniEvent",&mL3Event,128000,1);
    }

  // return
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t Stl3RawReaderMaker::Make()
{
    //
    //  Make - this method is called in loop for each event
    //
       
    // here we start
    cout << "Now we start l3RawReader Maker. \n" ;
    
    // get the l3 daqreader
    StDAQReader *daqReader = (StDAQReader*)(DAQReaderSet->GetObject()) ;
    if (daqReader) 
	{ 
	    ml3reader = daqReader->getL3Reader() ;
	    Int_t sec = 1 ;
	    if ( ml3reader && ml3reader->getGlobalTrackReader() )  
	      { cout << ml3reader->getGlobalTrackReader()->getNumberOfTracks() << " global tracks found.\n" ; } ;
	    if ( ml3reader && ml3reader->getSl3ClusterReader(sec) )  
	      { cout << ml3reader->getSl3ClusterReader(sec)->getNumberOfClusters() << " sl3 clusters found in sec " << sec <<" .\n" ; } ;
	    if ( ml3reader && ml3reader->getSl3TrackReader(sec) )  
	      { cout << ml3reader->getSl3TrackReader(sec)->getNumberOfTracks() << " sl3 tracks found in sec " << sec <<" .\n" ; } ;
	    if ( ml3reader && ml3reader->getI960ClusterReader(sec) )
	      { cout << ml3reader->getI960ClusterReader(sec)->getNumberOfClusters() << " i960 clusters found in sec " << sec <<" .\n" ; } ;
	}
    else 
	{
	    cout << "no l3 daq reader found.\n" ;
	    return kStWarn ;
	} 


    // fill tree
    if ( WriteMiniEvent == 1 )
      {
	if ( fillTree() != 0 )
	  {
	    cout << "problems filling l3 tree.\n" ;
	    return kStWarn ;
	  }
      }

    // fill StEvent
    if ( WriteStEvent == 1 )
      {
	if ( fillStEvent() != 0 )
	  {
	    cout << "problems filling l3 into StEvent.\n" ;
	    return kStWarn ;
	  }
      }
    
    // delete ml3reader ;

    // go home
    return kStOk;
}
//_____________________________________________________________________________
Int_t Stl3RawReaderMaker::fillTree()
{
  // global tracks
  if (  ml3reader->getGlobalTrackReader()->getTrackList() )
    { if ( fillMiniEventWithL3GlobalTracks() !=0 ) return 1; }

  // i960 hits
  if (  ml3reader->getI960ClusterReader(1) )
    { if ( fillMiniEventWithi960Clusters() != 0 ) return 1; }

  // sl3Tracks and sl3Hits may be added here

  
  //////
  // Fill Tree
  //////
  // tracks and possibly hits are already in 
  mL3Event->SetNTracks(ml3reader->getGlobalTrackReader()->getNumberOfTracks()) ;
  mL3Event->SetNHits(ml3reader->getGlobalTrackReader()->getNumberOfHits()) ;
  mL3Event->SetVertex(ml3reader->getGlobalTrackReader()->getVertex().x,
		      ml3reader->getGlobalTrackReader()->getVertex().y,
 		      ml3reader->getGlobalTrackReader()->getVertex().z ) ;

  mGlobalTrackTree->Fill();

  // all right go home
  return 0 ;  
}
//_____________________________________________________________________________
Int_t Stl3RawReaderMaker::fillMiniEventWithL3GlobalTracks()
{
  // get l3tracks out of .daq file
  globalTrack* globalL3Tracks = ml3reader->getGlobalTrackReader()->getTrackList() ;

  // get TClonesArray 
  TClonesArray& trackArray = *(mL3Event->GetTrackArray()) ;
  trackArray.Clear() ;

  // loop over tracks and fill them into TClonesArray
  Int_t numTracks = ml3reader->getGlobalTrackReader()->getNumberOfTracks() ;
  cout << numTracks <<" global Tracks expected.\n" ;
  for(Int_t trackid = 0 ; trackid < numTracks ; trackid++)
    {
      if (trackid%1000 ==0 ) cout << trackid << "hea" << endl ;
      new(trackArray[trackid]) Stl3Track(
					 globalL3Tracks[trackid].nHits ,
					 globalL3Tracks[trackid].q ,
					 globalL3Tracks[trackid].flag ,
					 globalL3Tracks[trackid].innerMostRow , 
					 globalL3Tracks[trackid].outerMostRow ,
					 globalL3Tracks[trackid].pt ,
					 globalL3Tracks[trackid].psi ,
					 globalL3Tracks[trackid].tanl ,
					 globalL3Tracks[trackid].z0 ,
					 globalL3Tracks[trackid].phi0 ,
					 globalL3Tracks[trackid].r0 ,
					 globalL3Tracks[trackid].length
					 ) ; 
	  ml3reader->getGlobalTrackReader()->getTrackList() ;
	  ml3reader->getGlobalTrackReader()->getNumberOfTracks() ;
    }
  //ok
  return 0;
}
//_____________________________________________________________________________
Int_t Stl3RawReaderMaker::fillMiniEventWithi960Clusters()
{
  // get TClonesArray 
  TClonesArray& hitArray = *(mL3Event->GetHitArray()) ;
  hitArray.Clear() ;

  //loop over sectors
  Int_t hitArrayIndex  = 0 ;
  for(Int_t sec=1 ;sec <=24 ; sec +=2 )
    {
      // get clusters for this sector 
      l3_cluster* i960cluster = ml3reader->getI960ClusterReader(sec)->getClusterList() ;    
      
      // loop over clusters and fill them into TClonesArray
      Int_t a = ml3reader->getI960ClusterReader(sec)->getNumberOfClusters(); 
      cout << a <<  "  clusters expected in sec  " << sec << endl ; 
      for(Int_t clusindex = 0 ; clusindex < a  ;clusindex++)
	    {
	      //if (clusindex%10000 ==0 ) cout << clusindex <<endl ;
	      new(hitArray[hitArrayIndex]) Stl3Hit(
						   i960cluster[clusindex].pad ,
						   i960cluster[clusindex].time ,
						   i960cluster[clusindex].padrow ,
						   i960cluster[clusindex].charge 
						   ) ;
	      ml3reader->getI960ClusterReader(sec)->getNumberOfClusters() ;
	      hitArrayIndex++ ; 
	    }
        }
  // ok
  cout << hitArrayIndex << " i960 clusters found.\n" ;
  return 0;
}
//_____________________________________________________________________________
Int_t Stl3RawReaderMaker::fillStEvent() 
{
  // get StEvent if not return
  StEvent* myStEvent = (StEvent *) GetInputDS("StEvent") ;
  if (!myStEvent) { cout << "No StEvent\n" ; return 1 ; } ;
  
  // create Stl3Trigger and connect it
  myStL3Trigger = new StL3Trigger() ;
  if (!myStL3Trigger) { cout <<"No Stl3Trigger.\n" ; return 1 ; } ;
  myStEvent->setL3Trigger(myStL3Trigger) ;

  // call filling routines
  // global tracks
  if (  ml3reader->getGlobalTrackReader()->getTrackList() )
    { if ( fillStEventWithL3GlobalTracks() !=0 ) return 1; 
    } ;

  // i960 hits
  //if (  ml3reader->getI960ClusterReader(1) )
  //  { if (  fillStEventWithi960Hits() != 0 ) return 1; } ;
  

  // calculate vertex offline and fill into StEvent
  if (  ml3reader->getGlobalTrackReader()->getTrackList() )
      { 
	  StPrimaryVertex* mVertex1 = new StPrimaryVertex() ;
	  if ( findVertexMethod1(*mVertex1) !=0 ) 
	      {
		  delete mVertex1 ;
		  return 1 ; 
	      }
	  else
	      {
		  myStL3Trigger-> addPrimaryVertex(mVertex1) ;
		  cout << "mVertex straight lines :  " << mVertex1->position().x() << "\t" ;
		  cout << mVertex1->position().y() << "\t" << mVertex1->position().z() << endl ;
	      }


	  StPrimaryVertex* mVertex2 = new StPrimaryVertex() ;
	  if ( findVertexMethod2(*mVertex2) !=0 ) 
	      {
		  delete mVertex2 ;
		  return 1 ; 
	      }
	  else
	      {
		  myStL3Trigger-> addPrimaryVertex(mVertex2) ;
		  cout << "mVertex helixes :  " << mVertex2->position().x() << "\t" ;
		  cout << mVertex2->position().y() << "\t" << mVertex2->position().z() << endl ;
	      }
    } ;


  // all right go home
  return 0 ;  
}
//_____________________________________________________________________________
Int_t Stl3RawReaderMaker::fillStEventWithL3GlobalTracks() 
{ 
  // get Track Nod
  StSPtrVecTrackNode& myTrackNodevec = myStL3Trigger->trackNodes() ;
  
  // get L3 raw tracks
  globalTrack* globalL3Tracks = ml3reader->getGlobalTrackReader()->getTrackList() ;
      
  // loop over rawdata tracks and fill them into StEvent
  Int_t numberOfTracks = ml3reader->getGlobalTrackReader()->getNumberOfTracks() ;
  cout << "Try to fill " << numberOfTracks << " tracks into StEvent.\n" ; 
  for(Int_t trackindex = 0 ; trackindex < numberOfTracks ;  trackindex++)
    {
      // StTrackDetectorInfo
      StTrackDetectorInfo* detecInfo = new StTrackDetectorInfo() ;
      detecInfo->setNumberOfPoints(globalL3Tracks[trackindex].nHits) ;
		
      // StTrackGeometry
      StThreeVectorF* origin = new  StThreeVectorF( globalL3Tracks[trackindex].r0 * TMath::Cos(globalL3Tracks[trackindex].phi0),
						    globalL3Tracks[trackindex].r0 * TMath::Sin(globalL3Tracks[trackindex].phi0),
						    globalL3Tracks[trackindex].z0 ) ;
      StThreeVectorF* momentum = new StThreeVectorF( globalL3Tracks[trackindex].pt * TMath::Cos(globalL3Tracks[trackindex].psi),
						     globalL3Tracks[trackindex].pt * TMath::Sin(globalL3Tracks[trackindex].psi),
						     globalL3Tracks[trackindex].pt * globalL3Tracks[trackindex].tanl ) ;
      StHelixModel* helixModel = new StHelixModel( globalL3Tracks[trackindex].q,
						   globalL3Tracks[trackindex].psi,
						   0.0,
						   globalL3Tracks[trackindex].tanl, 
						   *origin, 
						   *momentum ) ;		
      // StGlobalTrack
      StGlobalTrack* globalTrack = new StGlobalTrack() ;
      globalTrack->setFlag(globalL3Tracks[trackindex].flag) ;
      globalTrack->setLength(globalL3Tracks[trackindex].length) ;
      globalTrack->setDetectorInfo(detecInfo) ;
      globalTrack->setGeometry(helixModel) ;

      // StTrackNode
      StTrackNode* trackNode = new StTrackNode() ;
      trackNode->addTrack(globalTrack) ;
      myTrackNodevec.push_back(trackNode) ;

      if ( !detecInfo || !origin || !momentum || !helixModel || !globalTrack || !trackNode )  { return 1;} ;  
    }
  // ok
  return 0 ;
}
//_____________________________________________________________________________
Int_t Stl3RawReaderMaker::fillStEventWithi960Hits() 
{   
  // create StTpcHitCollection and connect it to StEvent
  StTpcHitCollection* mHitCollection = new StTpcHitCollection() ; 
  myStL3Trigger->setTpcHitCollection(mHitCollection);

  // prepare transformation from pad,time,padrow to x,y,z
  St_l3_Coordinate_Transformer transformer ;
  transformer.Print_parameters() ;
  St_l3_xyz_Coordinate XYZ(0,0,0) ;
  St_l3_ptrs_Coordinate PTRS(0,0,0,0) ;

  // loop over clusters and fill them into StEvent
  Int_t totalcluster = 0 ;
  for ( Int_t secindex=1 ; secindex<=24 ; secindex+=2 )
    {    
      if ( ml3reader->getI960ClusterReader(secindex)->getClusterList() )
	{
	  cout << "Found some i960 clusters in sector:" << secindex <<endl ;
	  l3_cluster* myl3cluster = ml3reader->getI960ClusterReader(secindex)->getClusterList() ;
	  Int_t numOfClusters = ml3reader->getI960ClusterReader(secindex)->getNumberOfClusters() ;
	  
	  for (Int_t clindex=0 ;  clindex < numOfClusters ; clindex++)
	    {
	      totalcluster++ ;
	      // pad,time,row,q,flag
	      Double_t pad  = ((Double_t)(myl3cluster[clindex].pad)) / 64 ;
	      Double_t time = ((Double_t)(myl3cluster[clindex].time)) / 64 ;
	      Int_t row  = myl3cluster[clindex].padrow ;
	      Int_t q    = myl3cluster[clindex].charge ;
	      Char_t flag = myl3cluster[clindex].flags  ;

	      // upper 4 bits are RB, lower 4 bits are MZ
	      Int_t RBMZ = myl3cluster[clindex].RB_MZ  ;
	      Int_t rb   = RBMZ >> 4 ;
	      Int_t mz   = RBMZ & 15 ;
	      
	      // determine sector
	      Int_t sector = 0 ;
	      if (rb<=6) { sector = secindex; } else { sector = secindex+1; }      
	      
	      // coordinate transformation
	      PTRS.Setptrs((Double_t) pad, (Double_t) time,(Double_t) row, (Double_t) sector) ;
	      transformer.raw_to_global(PTRS,XYZ) ;
	      
	      // some output
	      if (clindex%500==0)
		{
		  cout << XYZ.Getx() <<"\t" << XYZ.Gety() <<"\t" << XYZ.Getz() <<"\t";
		  cout << row  <<"\t" << sector << "\t" << rb  << "\t" << mz  << "\t" << q << "\t";
		  cout << ((Double_t)(myl3cluster[clindex].pad)) / 64 <<"\t" ;
		  cout << ((Double_t)(myl3cluster[clindex].time)) / 64 <<"\t" ;
		  cout << (Int_t)(myl3cluster[clindex].padrow)  <<"\n" ;
		}
	      
	      // Fill it
	      // position and error
	      StThreeVectorF* pos = new StThreeVectorF(XYZ.Getx(),XYZ.Gety(),XYZ.Getz()) ;
	      StThreeVectorF* poserror = new StThreeVectorF(0,0,0) ;
	      // pack sec and row : bits 4-8 = sector[1,24] and bits 9-14 = padrow[1-45]
	      ULong_t hw = 0 ;
	      ULong_t hrow = 0 ;
	      ULong_t hsec = 0 ;
	      if ( row >=1 && row <=45 )  { hrow = row << 9 ; } else { hrow=0 ; } 
	      if ( sector >=1 && sector <=24 )  { hsec = sector << 4 ; } else { hsec=0 ; } 
	      hw = hw | hrow ;
	      hw = hw | hsec ;
	      // track reference counter set always to 0
	      UChar_t c = 0 ;
	      // create hit
	      StTpcHit* tpcHit = new StTpcHit(*pos,*poserror,hw,q,c) ;
	      tpcHit->setFlag(flag) ;
	      // add to hit collection
	      if (tpcHit) { mHitCollection->addHit(tpcHit) ;} else { delete tpcHit; return 1;}				 
	    } // clusters
	} // if ...
    } // sectors
  cout <<"total found clusters " << totalcluster << endl ;
 // ok
  return 0 ;
}
//_____________________________________________________________________________
Int_t Stl3RawReaderMaker::findVertexMethod1(StPrimaryVertex& mvertex)
{
    // get L3 raw tracks
    globalTrack* globalL3Tracks = ml3reader->getGlobalTrackReader()->getTrackList() ;

    // loop over rawdata tracks and fill them into StEvent
    Double_t b = 0 ;
    Int_t countz = 0 ;
    Double_t vertexZ = 9999 ;
    TH1D* vertexZdis = new TH1D("vz","vz",800,-200,200) ;
    vertexZdis->Reset();
    Int_t numberOfTracks = ml3reader->getGlobalTrackReader()->getNumberOfTracks() ;
    cout << "Try to calculate vertex with " << numberOfTracks << " tracks and straight line approximation .\n" ; 
    for(Int_t trackid = 0 ; trackid < numberOfTracks ;  trackid++)
	{
	    if ( globalL3Tracks[trackid].nHits>14  && globalL3Tracks[trackid].pt >1.0)
	    	{
		    double psi = globalL3Tracks[trackid].psi ;
		    double phi = globalL3Tracks[trackid].phi0 ;
		    b = globalL3Tracks[trackid].z0  - globalL3Tracks[trackid].r0 * cos(psi-phi) * globalL3Tracks[trackid].tanl ;
		    
		    if (b<200 && b>-200)
			{
			  //if (countz%1000 ==0) {     cout << " b: \t" << b ; } ;
			    vertexZ += b;
			    countz++;
			    vertexZdis->Fill(b) ;
			}
		}
  	}
 
    if (countz !=0)
	{
	  Int_t maxbin = vertexZdis->GetMaximumBin() ;
	  Double_t maxval = vertexZdis->GetBinCenter(maxbin) ;
	  
	  TF1 mygaus("mygaus","[0]*exp(-0.5*( (x-[1])/[2])^2)", maxval-10 ,maxval+10 );
	  mygaus.SetParNames("Constant","Mean_value","Sigma");
	  mygaus.SetParameter(0,20);
	  mygaus.SetParameter(1,maxval);
	  mygaus.SetParameter(2,2);
	  vertexZdis->Fit("mygaus","Q0R");
	  
	  //cout << "fit :" << mygaus->GetParameter(1)  << endl ;
	  // vertexZ = vertexZ/countz ;
	  vertexZ = mygaus.GetParameter(1);
	  //cout << endl << "vertexZ: " << vertexZ << "\t";
	  //cout << "  count: " << countz << endl ; 
	  
 	}
    else
	{
	    vertexZ = 9999 ;
	}

   
    // fill it
    StThreeVectorF* pos = new StThreeVectorF(0.0,0.0,vertexZ);
    mvertex.setPosition(*pos);

    // clean up
    delete vertexZdis ;

    // ok go home
    return 0;
}
//________________________________________________________________________
Int_t Stl3RawReaderMaker::findVertexMethod2(StPrimaryVertex& mvertex)
{
    // get L3 raw tracks
    globalTrack* globalL3Tracks = ml3reader->getGlobalTrackReader()->getTrackList() ;

    // some preparation
    Int_t numberOfTracks = ml3reader->getGlobalTrackReader()->getNumberOfTracks() ;
    cout << "Try to calculate vertex with  " << numberOfTracks << " tracks and helix extrapolation.\n" ; 
    StHelixD hel;
    
    /////
    // find vertex z position
    /////
    Double_t B = 0.25 * 0.01 ; // this is important ! B-field in right dimension : * 0.01 
    TH1D* vertexZdis = new TH1D("vz2","vz2",800,-200,200) ;
    vertexZdis->Reset();
    for(Int_t trackid = 0 ; trackid < numberOfTracks ;  trackid++)
	{
	  if ( globalL3Tracks[trackid].nHits>14  && globalL3Tracks[trackid].pt > 0.2  )
	    { 
	      // make a StHelix out of l3 track
	      Double_t c     = fabs(0.3 * globalL3Tracks[trackid].q * B / (globalL3Tracks[trackid].pt)) ;
	      Double_t dip   = atan(globalL3Tracks[trackid].tanl) ;
	      Double_t h     = -((globalL3Tracks[trackid].q*B)/fabs(globalL3Tracks[trackid].q*B));
	      Double_t phase = globalL3Tracks[trackid].psi-h*TMath::Pi()/2 ;
	      StThreeVectorD orig(globalL3Tracks[trackid].r0*cos(globalL3Tracks[trackid].phi0),
				  globalL3Tracks[trackid].r0*sin(globalL3Tracks[trackid].phi0),
				  globalL3Tracks[trackid].z0);
	     
	     
	  	     
	      hel.setParameters( c , dip , phase , orig ,  h ) ;

	      Double_t ver = hel.z(hel.pathLength(0,0)) ;
	      //cout << ver << "\t" ;
	      vertexZdis->Fill(ver) ;
	    }
	}
    // fit z vertex
    Double_t vertexZ = 9999 ;
    TF1 *mygaus = new TF1("mygaus","[0]*exp(-0.5*( (x-[1])/[2])^2)", -10 ,10 );
    if (vertexZdis->GetEntries() > 3)
      {
	Int_t maxbin = vertexZdis->GetMaximumBin() ;
	Double_t maxval = vertexZdis->GetBinCenter(maxbin) ;
	
	mygaus->SetRange(maxval-10 ,maxval+10 );
	mygaus->SetParNames("Constant","Mean_value","Sigma");
	mygaus->SetParameter(0,20);
	mygaus->SetParameter(1,maxval);
	mygaus->SetParameter(2,2);
	vertexZdis->Fit("mygaus","Q0R");
	
	//cout << "fit :" << mygaus->GetParameter(1)  << endl ;
	vertexZ = mygaus->GetParameter(1) ; 	
      }
    else
      {
	vertexZ = 9999 ;
      }

    ////
    // now calculate the x-y position 
    ////
    StThreeVectorD* normale = new   StThreeVectorD(0,0,1) ;
    StThreeVectorD* center  = new   StThreeVectorD(0,0,vertexZ) ;
    TH1D* vertexX = new TH1D("vertexX","vertexX",100,-5,5);
    TH1D* vertexY = new TH1D("vertexY","vertexY",100,-5,5);

    for(Int_t trackid = 0 ; trackid < numberOfTracks ;  trackid++)
      {
	if ( globalL3Tracks[trackid].nHits>14  && globalL3Tracks[trackid].pt >0.2 )
	  { 
	    // make a StHelix out of l3 track
	    Double_t c     = fabs(0.3 * globalL3Tracks[trackid].q * B / (globalL3Tracks[trackid].pt)) ;
	    Double_t dip   = atan(globalL3Tracks[trackid].tanl) ;
	    Double_t h     = -((globalL3Tracks[trackid].q*B)/fabs(globalL3Tracks[trackid].q*B));
	    Double_t phase = globalL3Tracks[trackid].psi-h*TMath::Pi()/2 ;
	    StThreeVectorD orig(globalL3Tracks[trackid].r0*cos(globalL3Tracks[trackid].phi0),
				globalL3Tracks[trackid].r0*sin(globalL3Tracks[trackid].phi0),
				globalL3Tracks[trackid].z0);
	    
	    
	    hel.setParameters( c , dip , phase , orig ,  h ) ;
	    
	    
	    Double_t vertexXp = hel.x(hel.pathLength(*center, *normale)) ;
	    Double_t vertexYp = hel.y(hel.pathLength(*center, *normale)) ;
	    //cout << vertexXp << "\t" ;
	    //cout << vertexYp << endl ;
	    vertexX->Fill(vertexXp) ;
	    vertexY->Fill(vertexYp) ;
	  }
      }
     
    /////
    // fit xy vertex
    /////
    Double_t vertexXpos ;
    Double_t vertexYpos ; 
    if (vertexX->GetEntries() > 3 && vertexY->GetEntries() > 3 )
      {
	//x
	Int_t maxbin = vertexX->GetMaximumBin() ;
	Double_t maxval = vertexX->GetBinCenter(maxbin) ;
	
	mygaus->SetRange(maxval-2 ,maxval+2 );
	mygaus->SetParameter(0,20);
	mygaus->SetParameter(1,maxval);
	mygaus->SetParameter(2,1);
	vertexX->Fit("mygaus","Q0R");
	
	//cout << "vertex x position fit :" << mygaus->GetParameter(1)  << endl ;
	vertexXpos = mygaus->GetParameter(1) ;  
	if ( fabs(vertexXpos) >100 ) vertexXpos = 100 ;

	//y
	maxbin = vertexY->GetMaximumBin() ;
	maxval = vertexY->GetBinCenter(maxbin) ;
	mygaus->SetRange(maxval-2 ,maxval+2 );
	mygaus->SetParameter(0,20);
	mygaus->SetParameter(1,maxval);
	mygaus->SetParameter(2,1);
	vertexY->Fit("mygaus","Q0R");
	
	//cout << "vertex y position fit :" << mygaus->GetParameter(1)  << endl ;
	vertexYpos = mygaus->GetParameter(1) ;  
	if ( fabs(vertexYpos) >100 ) vertexYpos = 100 ;
      }
    else
	{
	    vertexXpos = vertexYpos = 9999 ;
	}

    // fill it
    StThreeVectorF* pos = new StThreeVectorF(vertexXpos,vertexYpos,vertexZ);
    mvertex.setPosition(*pos);

    // clean up 
    delete  mygaus ;
    delete  vertexX ;
    delete  vertexY ;
    delete  normale ;
    delete  center ;
    delete  vertexZdis ;

    // ok
    return 0 ;
}

