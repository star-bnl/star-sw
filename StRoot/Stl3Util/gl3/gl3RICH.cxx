//-----------------------------------------------------------------
//: FILE:       gl3RICH.cc
//: HISTORY:
//:             22Jun2K version 1.00
//:<------------------------------------------------------------------
#include "gl3RICH.h"

#include <math.h>
//#include <L3io.h>

//####################################################################
//
//####################################################################
int gl3RICH::init( )
{
    // RICH coordinates
    // 
    mGlobalRichEdgeXmin=  83.988;
    mGlobalRichEdgeXmax= 156.503;
    mGlobalRichEdgeYmin=-228.489;
    mGlobalRichEdgeYmax=-186.983;
    mGlobalRichEdgeZmin= -65.781;
    mGlobalRichEdgeZmax=  65.163;

    // Cylindrical coordinates: 5 o'clock = -60 degrees
    mLocalOriginR   = 243.13;    
    mLocalOriginPhi = -M_PI/3.;    

    // histograms
#ifdef GL3ROOT
    hXVertex = new TH1D("hXVertex","event vertices x [cm]",50,-2.5,2.5);
    hYVertex = new TH1D("hYVertex","event vertices y [cm]",50,-2.5,2.5);
    hZVertex = new TH1D("hZVertex","event vertices z [cm]",800,-200,200);

    hPtGlobalPosRICH = new TH1D("hPtGlobalPosRICH","Pt global h+ in RICH",300,0,6);
    hPGlobalPosRICH = new TH1D("hPGlobalPosRICH","P global h + in RICH",300,0,6);

    hPtGlobalNegRICH = new TH1D("hPtGlobalNegRICH","Pt global h- in RICH",300,0,6);
    hPGlobalNegRICH = new TH1D("hPGlobalNegRICH","P global h- in RICH",300,0,6);

    hnHitsGlobalPosRICH = new TH1D("hnHitsGlobalPosRICH","nHits globals + in RICH",46,0,46);
    hnHitsGlobalNegRICH = new TH1D("hnHitsGlobalNegRICH","nHits globals - in RICH",46,0,46);

    hdcaGlobal = new TH1D("hdcaGlobal", "dca global tracks", 104, -1, 25);
    hetaRICH = new TH1D("hethaRICH","etha global RICH",200,-1,1);
    hRLast = new TH1D("hRLast","last hit of TPC track",200,0,200);
#endif
   
    return 0 ;
}

//####################################################################
//
//####################################################################
int gl3RICH::setParameters(int maxAbsVertZ, int minNoOfHits, int are, int place , int holders, float minP, float minPt, float minR, float maxDCA, float minimumAbsEta )
{
 // zvert
 if(maxAbsVertZ>=0 && maxAbsVertZ<=200) maxAbsEventVertZ = maxAbsVertZ;
 else maxAbsEventVertZ=30.0;      // zvert
 // nHits
 if(minNoOfHits>=5 && minNoOfHits<=45) minNoOfHitsOnTrack = minNoOfHits;
 else minNoOfHitsOnTrack=23;      // nhits
 // P min to trigger on
 if(minP>=0.0 && minP<10.0) minPExtrapolated2Rich=minP;
 else minPExtrapolated2Rich=1.0;// GeV
 // Pt min to trigger on
 if(minPt>=0.0 && minPt<10.0) minPtExtrapolated2Rich=minPt;
 else minPtExtrapolated2Rich=1.0;// GeV
 // min Radius of last point in tpc
 if(minR>=50.0 && minR<200.0 ) minRofLastPointOnTrack=minR;
 else minRofLastPointOnTrack=160; // cm
 // DCA
 if(maxDCA>=0.0 && maxDCA<=100) maxDCAToEventVertex=maxDCA;
 else maxDCAToEventVertex=10;    // cm
 // eta
 if(minimumAbsEta>0.0 && minimumAbsEta<0.5)  minAbsEta=minimumAbsEta;
 else minAbsEta=2;

 return 0;
}


//####################################################################
//
//####################################################################
int gl3RICH::decide ( )
{
    // DEBUG
    // printf ( "doing gl3RICH process!!!\n");

    // init
//VPunused    short sector = 0 ;
    gl3Track* gTrack ;


    int noOfTracksExtrapolated2RICH=0;

    event->makeVertex();

    // DEBUG
    //cout<<"Vx:"<<event->getVertex().Getx()<<" Vy:"<<event->getVertex().Gety()<<" Vz:"<<event->getVertex().Getz()<<endl;

    if( fabs(event->getVertex().Getz()) <= maxAbsEventVertZ)
    {
#ifdef GL3ROOT
    hXVertex->Fill(event->getVertex().Getx());
    hYVertex->Fill(event->getVertex().Gety());
    hZVertex->Fill(event->getVertex().Getz());
#endif
    
    // RICH plane parameters as used by intersectorZLine
    const float a = -1./tan(mLocalOriginPhi);
    const float b = ( mLocalOriginR*sin(mLocalOriginPhi)
		      - a*mLocalOriginR*cos(mLocalOriginPhi) );

    Ftf3DHit closestHitToEventVertex;
    Ftf3DHit richHit1, /*richHit2,*/ lastHit; 

    // loop over gtracks
    for (int trkcnt = 0 ; trkcnt<event->getNTracks(); trkcnt++ )
    {
	 gTrack = event->getTrack(trkcnt);

	 //momentum
	 double px = gTrack->pt * cos(gTrack->psi);
	 double py = gTrack->pt * sin(gTrack->psi);
	 double pz = gTrack->pt * gTrack->tanl;
	 double p = ::sqrt(px*px+py*py+pz*pz);
	 // pseudorapidity
	 double eta = -::log(tan(acos(pz/p)/2));


     // cut nHits, P, Pt and eta
    if( gTrack->nHits>=minNoOfHitsOnTrack && p>=minPExtrapolated2Rich && gTrack->pt>=minPtExtrapolated2Rich && fabs(eta)<=minAbsEta ) 
    {

    // traversing the rich ???
	if (
//	    0==gTrack->intersectorZLine( a, b, richHit1, richHit2) 
	    0==gTrack->intersectorZLine( a, b, richHit1 ) 

	    &&
//	    (
	     ( richHit1.x > mGlobalRichEdgeXmin && 
		richHit1.x < mGlobalRichEdgeXmax &&
		richHit1.y > mGlobalRichEdgeYmin && 
		richHit1.y < mGlobalRichEdgeYmax &&
		richHit1.z > mGlobalRichEdgeZmin && 
		richHit1.z < mGlobalRichEdgeZmax ) 
//	      || 
//	      ( richHit2.x > mGlobalRichEdgeXmin && 
//		richHit2.x < mGlobalRichEdgeXmax &&
//		richHit2.y > mGlobalRichEdgeYmin && 
//		richHit2.y < mGlobalRichEdgeYmax &&
//		richHit2.z > mGlobalRichEdgeZmin && 
//		richHit2.z < mGlobalRichEdgeZmax ) 
//	      ) 
	    ) {  

        // length
        double trackLength= gTrack->length/cos(atan(gTrack->tanl));
        lastHit=gTrack->extrapolate2PathLength(trackLength);

        // check if extrapolation makes sense
        if( lastHit.z>=mGlobalRichEdgeZmin && lastHit.z<=mGlobalRichEdgeZmax )
        {
        // Rlast
        double Rlast=::sqrt(lastHit.x*lastHit.x+lastHit.y*lastHit.y);
	    
        // dca
        closestHitToEventVertex = gTrack->closestApproach( event->getVertex().Getx(), event->getVertex().Gety() );    
        double dx=fabs( event->getVertex().Getx() - closestHitToEventVertex.x );
        double dy=fabs( event->getVertex().Gety() - closestHitToEventVertex.y );
        double dz=fabs( event->getVertex().Getz() - closestHitToEventVertex.z );
        double dca=::sqrt(dx*dx+dy*dy+dz*dz);

#ifdef GL3ROOT

#endif
        
	    if(dca<maxDCAToEventVertex && Rlast>minRofLastPointOnTrack)
	    {
	     noOfTracksExtrapolated2RICH++;

         // DEBUG
	     //cout<<"# "<<noOfTracksExtrapolated2RICH<<" nHits:"<<gTrack->nHits<<" dca:"<<dca<<" trlen:"<<trackLength<<" Rlast:"<<Rlast<<" pt:"<<gTrack->pt<<" p:"<<p<<" eta:"<<eta<<endl;

         // DEBUG
	     // ftfLog("Found a RICH track!\n");
	     // cout << richHit1 << "  " << richHit2 << endl;
	    
#ifdef GL3ROOT
         hetaRICH->Fill(eta);
         hdcaGlobal->Fill(dca);
         hRLast->Fill(Rlast);
         
	     if(gTrack->q>0)
	     {
		  hPtGlobalPosRICH->Fill(gTrack->pt);
		  hPGlobalPosRICH->Fill(p);
		  hnHitsGlobalPosRICH->Fill(gTrack->nHits);
	     }
	     else
	     {
		  hPtGlobalNegRICH->Fill(gTrack->pt);
		  hPGlobalNegRICH->Fill(p);
		  hnHitsGlobalNegRICH->Fill(gTrack->nHits);
	     }
#endif
	    } // if(dca<maxDCAToEventVertex)

	    } // zLast check
	    
	   } // traversing the RICH
	
      } // if( gTrack->nHits>=minNoOfHitsOnTrack && p>=minPExtrapolated2Rich && eta>=minAbsEta ) 

    } // trackloop	    

    } //if( fabs(event->getVertex().Getz()) <= maxAbsEventVertz)

    // DECISION
    if( noOfTracksExtrapolated2RICH>0 ) {
    // DEBUG
	  //cout<<"Event triggered! #tracks:"<<noOfTracksExtrapolated2RICH<<endl;
	return 1;
    } else {
    // DEBUG
	  //cout<<"Event rejectetd! "<<endl;
	return 0;
    }
}

//####################################################################
//
//####################################################################
int gl3RICH::end ( ) {
    
#ifdef GL3ROOT
    hPtGlobalPosRICH->Write();
    hPtGlobalNegRICH->Write();

    hPGlobalPosRICH->Write();
    hPGlobalNegRICH->Write();

    hnHitsGlobalPosRICH->Write();
    hnHitsGlobalNegRICH->Write();

    hXVertex->Write();
    hYVertex->Write();
    hZVertex->Write();

    hdcaGlobal->Write();
    hetaRICH->Write();
    hRLast->Write();
#endif

    return 0 ;
}





