#include "gl3LMVertexFinder.h"

#include "gl3Event.h"
#include "gl3HistoManager.h"

#include <assert.h>
#include <rtsLog.h>

//#define GL3ROOT
#ifdef GL3ROOT
static int lmvdbg=1;
#else
static int lmvdbg=0;
#endif
const float  gl3LMVertexFinder::C_PI=3.1416926;

//####################################################################
// Constructor
//####################################################################
gl3LMVertexFinder::gl3LMVertexFinder(int   _minHitsOnTrack,
				     int   _minCTBadc,
				     float _maxZdca,
				     float _zMargin,
				     float _phiMargin,
				     float _delVertMax)
{
    setParameters(_minHitsOnTrack, _minCTBadc, _maxZdca,
		  _zMargin, _phiMargin, _delVertMax); 

    init();
    registerHistos();
}


//####################################################################
// Destructor
//####################################################################
gl3LMVertexFinder::~gl3LMVertexFinder()
{
} 

//####################################################################
// 
//####################################################################
int gl3LMVertexFinder::setParameters(int   _minHitsOnTrack,
				     int   _minCTBadc,
				     float _maxZdca,
				     float _zMargin,
				     float _phiMargin,
				     float _delVertMax)
{
    CtbRadius=213.; //(cm)
    CtbLen2=255.; //(cm) for eta=[0,1]
    
    minHitsOnTrack = _minHitsOnTrack; 
    minCTBadc      = _minCTBadc;
    
    maxZdca        = _maxZdca;
    MatchCtbMaxZ   = CtbLen2/4. + _zMargin; //(cm)  =slat+margin/cm
    MatchCtbMaxPhi = C_PI/30+_phiMargin/CtbRadius;//slat half width+margin/cm
    DelVertMax     = _delVertMax; //(cm) rejection of outliers in ppLMV 

    return 1;
}



//####################################################################
//
//####################################################################
int gl3LMVertexFinder::setParameters(const char *filename)
{
    int   minHitsOnTrack;
    int   minCTBadc;
    float maxZdca;
    float zMargin;
    float phiMargin;
    float delVertMax;
    
    // set defaults, if reading form file fails ... 
    minHitsOnTrack = 10;
    minCTBadc      =  3;  // set to -1 if no CTB data avaliable, without CTB works only for low luminosity and bXing ID is not sure any more
    
    maxZdca    = 250.0;
    zMargin    =  10.0;
    phiMargin  =   3.0;
    delVertMax =   3.0;

    // open jan's para txt file
    FILE *fp = fopen(filename,"r");
    if(fp == NULL) return -1;
    
    int ret=-1;
    if(fp) {
      ret=fscanf(fp,"%d %d %f %f %f %f", 
		 &minHitsOnTrack, &minCTBadc,  &maxZdca, 
		 &zMargin, &phiMargin, &delVertMax);
	fclose(fp);
    }
    if(ret!=6) LOG(ERR,"gl3LMVertexFinder::setParameters() ERROR while reading %s file, ret=%d, default params used\n",filename,ret,0,0,0);
    
    if(minCTBadc<0) LOG(ERR, "gl3LMVertexFinder::setParameters() WARN : CTB matching is off\n",0,0,0,0,0);
    
    setParameters(minHitsOnTrack, minCTBadc, maxZdca, 
		  zMargin, phiMargin, delVertMax);
    
    return 0;
}

//####################################################################
//
//####################################################################
int gl3LMVertexFinder::init () {

    totInpEve=0;
    setCtbMap();
    
    if(lmvdbg) LOG(ERR, "CTB matching limits del_Phi(deg)=%.1f, delZ(cm)=%f\n",
	   MatchCtbMaxPhi/C_PI*180, MatchCtbMaxZ,0,0,0);
   
    //
    //   Book histos
    //
    hjan[0]= new gl3Histo ( "ppLMV0","ZDC vertex in Z (cm)", 
			    100, -200, 200. ) ;
    hjan[1]= new gl3Histo ( "ppLMV1","raw event track multiplicity", 
			    100, 0.,500.);
    hjan[2]= new gl3Histo ( "ppLMV2"," event multiplicity,with any CTB slat", 
			    50, 0., 500. );
    hjan[3]= new gl3Histo ( "ppLMV3"," HITS IN TRACK, all,with any CTB slat ",  
			    50, -0.5, 49.5 );
    hjan[4]= new gl3Histo ( "ppLMV4"," Zdca (cm), long tracks",  
			    50, -300., 300. );
    hjan[5]= new gl3Histo ( "ppLMV5"," Zdca-ZvertexZDC (cm)",  
			    50, -200., 200. );
    hjan[6]= new gl3Histo ( "ppLMV6","non-zero ADC's for CTB",  
			    51, -0.5, 50.5 );
    hjan[7]= new gl3Histo ( "ppLMV7"," CTB slats with ADC>th",  
			    240, -0.5, 239.5 );
    hjan[8]= new gl3Histo ( "ppLMV8"," <z> of CTB with ADC>th",  
			    10, -300., 300. );
    hjan[9]= new gl3Histo ( "ppLMV9","pasX #Delta#phi (deg) from MatchCTB",  
			    100, -10., 10. );
    hjan[10]= new gl3Histo("ppLMV10","pasX #DeltaZ (cm) from MatchCTB",  
			   50, -100., 100. );
    hjan[11]= new gl3Histo("ppLMV11","number of tracks matched to CTB/eve",  
			   50, -0.5, 49.5 );
    hjan[12]= new gl3Histo("ppLMV12","number of accepted CTB slats/eve",  
			   250, -0.5, 249.5 );
    hjan[13]= new gl3Histo( "ppLMV13","counter of accepted events", 
			    7,0.5,7.5);
    hjan[14]= new gl3Histo( "ppLMV14","counter of accepted tracks", 
			    9,0.5,9.5);
    hjan[15]= new gl3Histo ( "ppLMV15","my vertex in Z (cm)",  
			     50, -300, 300. ) ;   
    hjan[16]= new gl3Histo ( "ppLMV16","my-ZDC vertex in Z (cm)",  
			     50, -20, 20. ) ;
    hjan[17]= new gl3Histo("ppLMV17","end: #DeltaZ (cm) (matched track-myVertex)",
			   50, -5., 5. );
    hjan[18]= new gl3Histo("ppLMV18","#DeltaZ (cm) (matched track-myVertex), Nmatch=[2-5]",
			   50, -5., 5. );   
    hjan[19]= new gl3Histo("ppLMV19","#DeltaZ (cm) (matched track-myVertex), Nmatch=[6-10]",
			   50, -5., 5. );
    hjan[20]= new gl3Histo("ppLMV20","#DeltaZ (cm) (matched track-myVertex), Nmatch=[10++]",
			   50, -5., 5. );    
    hjan[21]= new gl3Histo("ppLMV21","sigma of tracks extrapolated to vertex, a.u.",
			   50,.0,10.);

    nhjan=22;

    if(lmvdbg) {
	  for(int i=0;i<16;i++) {
		char t1[100], t2[100];
		sprintf(t1,"ppeve%d",i);
		sprintf(t2,"Zdca-Zvertex (cm) for accepted eve=%2d",i);
		heve[i]= new gl3Histo ( t1,t2, 50, -50., 50. );
	  }
    }

    neve=0;
    
    return 0 ; 
}


//####################################################################
//
//####################################################################
int gl3LMVertexFinder::registerHistos() {
    
    int j;
    for(j=0;j<nhjan;j++) gl3HistoManager::instance()->add(hjan[j]);

    if(lmvdbg) for(j=0;j<16;j++)    gl3HistoManager::instance()->add(heve[j]);

    return 0;
}


//####################################################################
//
//####################################################################
int gl3LMVertexFinder::makeVertex( gl3Event *event ) {
    int j;
    
    totInpEve++;
    myVert.z=2001;
    myVert.x=2002;    
    myVert.y=2003;
    
    NtrackL=0;

    memset(trackL,0,sizeof(trackL));
    hjan[13]->Fill(1);
    hjan[1]->Fill(event->getNTracks(),1);

    if (event->getNTracks() > 5000)
	return 0;
   

    if( getCtbHits(event) <=0) return 0; // no CTB slats found
   
    hjan[13]->Fill(2);
    //   if( NctbH> maxCtbSlats) return 0; // too many  CTB slats found

    hjan[13]->Fill(3);

    float zVertexZDC=event->getZDCVertex();
    
    hjan[0]->Fill(zVertexZDC);      
    hjan[2]->Fill(event->getNTracks(),1);
     
    //
    // loop over gtracks
    //
    
    for ( int trkcnt = 0 ; trkcnt <  event->getNTracks() ; trkcnt++ ) {
	hjan[14]->Fill(1);
	gl3Track* gTrack = event->getTrack(trkcnt);
	
	gTrack->flag=0;  // reset track flag to NULL

	hjan[3]->Fill(gTrack->nHits);
	if ( gTrack->nHits < minHitsOnTrack ) continue ;
	  
	hjan[14]->Fill(2);

	//  Extrapolate track to beam-DCA
        Ftf3DHit dcaHit1=gTrack->closestApproach ( 0., 0. ) ;
        float zDCA = dcaHit1.z ;

	//gTrack->updateToClosestApproach ( 0., 0. ) ;	  
	//float zDCA = gTrack->z0 ;

	if(fabs(zDCA)>maxZdca) continue;
	
	hjan[14]->Fill(3);

	if(minCTBadc>=0) { // do match to CTB
	    Ftf3DHit hit=gTrack->extraRadius(CtbRadius);
	    if(fabs(hit.x)<0.1 && fabs(hit.y)<0.1) continue; // extrapolation to CTB failed
	    
	    hjan[14]->Fill(4);
	    if( matchTrack2Ctb(&hit) <0) continue; // no match
	    
	    hjan[14]->Fill(5);
	} else { // no matching to CTB, take all tracks
	    hjan[14]->Fill(6);
	}
	
	if(lmvdbg) printf("track DCA x=%f y=%f z=%f\n",dcaHit1.x, dcaHit1.y,dcaHit1.z );
	//	 gTrack->Print(30);
	
	if(NtrackL>= MAXTRACK) {
	  LOG(ERR, " PileupFilter WARN: %d=NtrackL>= MAXTRACK, skip this matched track\n",NtrackL,0,0,0,0);
	    continue;
	}
	trackL[NtrackL]=gTrack;
	XdcaHitL[NtrackL]=dcaHit1.x;
	YdcaHitL[NtrackL]=dcaHit1.y;
	ZdcaHitL[NtrackL]=dcaHit1.z;
	NtrackL++;
	 
	hjan[4]->Fill(zDCA);
	hjan[5]->Fill(zDCA-zVertexZDC);
    }// end of loop over tracks

    neve++;   
    hjan[11]->Fill(NtrackL);

    if(lmvdbg) printf(" - end of eve: accepted tracks=%d, ZDCvert=%f.1 (neve=%d)\n",NtrackL,zVertexZDC,neve);

    // add beam line at X=Y=0 as as high momentum track
    gl3Track beamTrack;
    beamTrack.r0=0;
    beamTrack.phi0=0;
    beamTrack.z0=0;
    beamTrack.tanl=888999.; // almost parallel to Z
    beamTrack.psi=0;
    beamTrack.id=888999;
    beamTrack.lastHit=0;

    trackL[NtrackL]=&beamTrack;
    XdcaHitL[NtrackL]=0.;// should not be used
    YdcaHitL[NtrackL]=0.;// should not be used
    ZdcaHitL[NtrackL]=0.;// should not be used
    NtrackL++;

    if(NtrackL<2) return 0;
   
    hjan[13]->Fill(4);
   
    // vertex finder
    int NtrackVertex=ppLMV4b(&myVert);
    if( NtrackVertex <2) return 0; // no vertex found


    // output:
    //    NtrackMatch;
    //  myVert.z;
 
    hjan[13]->Fill(5);
    hjan[15]->Fill(myVert.z);
    hjan[16]->Fill(myVert.z-zVertexZDC);

   
    for( j=0;j<NtrackL;j++) {
	if( trackL[j]==NULL) continue;
	if(trackL[j]->lastHit==NULL) continue; // ignore 'beamTrack'
	float zVer = ZdcaHitL[j] ;
	hjan[17]->Fill(zVer-myVert.z);
	if(lmvdbg && neve<16)  heve[neve]->Fill(zVer-myVert.z);
    }
 
 
    for( j=0;j<NtrackL;j++) {
	if( trackL[j]==NULL) continue;
	if(trackL[j]->lastHit==NULL) continue; // ignore 'beamTrack'
	float zDca = ZdcaHitL[j] ;
	hjan[17]->Fill(zDca-myVert.z);
	if(      NtrackVertex <=5   )   hjan[18]->Fill(zDca-myVert.z);
	else if(  NtrackVertex <=10 )   hjan[19]->Fill(zDca-myVert.z);
	else    hjan[20]->Fill(zDca-myVert.z);
    }

   
    if (NtrackVertex<2)
	return 0; // failed to find vertex
    else
	return 1; // Found a vertex
}

//qTotal += gTrack->q ;
//pxTotal += gTrack->pt * cos(gTrack->psi);
//pyTotal += gTrack->pt * sin(gTrack->psi);
//pzTotal += gTrack->pt * gTrack->tanl ;


//####################################################################
//
//####################################################################
int gl3LMVertexFinder::getCtbHits (gl3Event *event) {
      
    memset(ctbH,0,sizeof(ctbH));
    NctbH=0; // clear old response

    int nSlat=0;
    
    float phiZero1 = 72 ; // magic lines from Pablo & Herb
    float phiZero2 = 108 ;
    float deltaPhi = 6 ;

    for(int ss=0;ss<2;ss++) {
	for(int tt=0;tt<120;tt++) {
	    int indx=ctbMap[tt][ss]; 
	    if(indx<0||indx>255) { 
		//printf ( "gl3LMVertexFinder::getCtbHits ( ) Ctb index %d out of bounds, skip hit\n", indx ) ;
		continue ;
	    }
	    int ctbAdc =event->getCTB(indx);
	    //printf("ss=%d, tt=%d, indx=%d  Adc=%d\n",ss,tt,indx,ctbAdc);

	    if(ctbAdc)hjan[6]->Fill(ctbAdc);
	    //assert(ctbAdc==0);
	    if(ctbAdc< minCTBadc) continue;

	    int iz ;	    
	    float phi ;

	    if ( tt < 60 )  {
		phi = phiZero1 - tt * deltaPhi ; 
		iz = 0 ;
	    }  else {
		phi = phiZero2 + (tt-60) * deltaPhi ;
		iz = 1 ;
	    }
	    if ( phi <   0. ) phi += 360 ;
	    if ( phi > 360. ) phi -= 360 ;
	    
	    //printf ( "ctb-hit tt=%d ss=%d adc=%d phi=%f z=%f idx=%d\n", 
	    // printf ( "%d %d %d %f %f %d\n", 
// 		     tt+1, ss+1, ctbAdc, phi/180*C_PI, 
// 		     zSlats[iz][ss], indx) ;
	    	    
	    if(nSlat>= MAXSLATS) {
		if(lmvdbg)
		    printf(" PileupFilter WARN:  %d=nSlat>= MAXSLATS,nSlat, skip this CTB  hit\n",nSlat);
		continue;
	    }
	    
	    ctbH[nSlat].z= zSlats[iz][ss];
	    ctbH[nSlat].phi=phi/180*C_PI;
	    ctbH[nSlat].adc=ctbAdc;
	    ctbH[nSlat].slatIndex=indx;
	    // printf("slat Indx: %i ADC: %i  z=%.1f phi/deg=%.1f\n",indx,ctbAdc,ctbH[nSlat].z,ctbH[nSlat].phi/C_PI*180);
	    hjan[7]->Fill(tt+120*ss);
	    hjan[8]->Fill(ctbH[nSlat].z);
	    nSlat++;
	} // end of loop over tt
    }	 // end of loop over ss
    if(lmvdbg)  printf(" %d CTB slats with ADC>=%d\n",nSlat,minCTBadc); //tmp
    NctbH=nSlat;
    hjan[12]->Fill(NctbH);
    return NctbH;
}

//####################################################################
//
//####################################################################
int gl3LMVertexFinder::matchTrack2Ctb( Ftf3DHit * h) {
    assert(h);

    float phi=atan2(h->y,h->x);
    if(phi<0) phi+=2*C_PI;
    // printf("track at CTB x=%f, y=%f, z=%f, phi/deg=%.1f\n",h->x,h->y,h->z,phi/3.1416*180);
    for(int is=0;is<NctbH;is++) {

	float del_z=h->z-ctbH[is].z;
	if(fabs(del_z) >MatchCtbMaxZ) continue;

	float del_phi=phi-ctbH[is].phi;
	if(del_phi>C_PI) del_phi-=2*C_PI;
	if(del_phi<-C_PI) del_phi+=2*C_PI;

	// printf("Match is=%d DZ=%f, Dphi=%f %f\n",is,del_z,del_phi,MatchCtbMaxPhi);

	if(fabs(del_phi) >MatchCtbMaxPhi) continue;

	hjan[9]->Fill(del_phi/C_PI*180.);
	hjan[10]->Fill(del_z);
	//printf("Match OK ctbHit_is=%d DZ=%f, Dphi=%f\n",is,del_z,del_phi);
	return is;
    }

    return -1 ; // no match
}

//####################################################################
//
//####################################################################
int gl3LMVertexFinder::ppLMV4b (  Ftf3DHit *myV) {// vertex finder

    myV->x=1000; 
    myV->y=1001; 
    myV->z=1002; 
    int NtrackUsed=NtrackL;

    //Do the actual vertex fitting, continue until good
    double A11=0.0,A12=0.0,A13=0.0,A21=0.0,A22=0.0,A23=0.0;
    double A31=0.0,A32=0.0,A33=0.0; // Matrix Elements
    double C11=0.0,C12=0.0,C13=0.0,C21=0.0,C22=0.0,C23=0.0;
    double C31=0.0,C32=0.0,C33=0.0; // C = A^-1
    int done = 0;
    double chi2=0;

    while( done != 1 ){

	// Check that there at least are 2 tracks
	if( NtrackUsed <= 1 ){
	    if(lmvdbg)cout<<"ppLMV4b: Fewer than 2 track remains. No vertex found."<<endl;
	    return -1;
	}
  
	// Begin by doing a fit
	A11=0.0,A12=0.0,A13=0.0,A21=0.0,A22=0.0,A23=0.0;
	A31=0.0,A32=0.0,A33=0.0; // Matrix Elements
	C11=0.0,C12=0.0,C13=0.0,C21=0.0,C22=0.0,C23=0.0;
	C31=0.0,C32=0.0,C33=0.0; // C = A^-1
	double b1=0.0,b2=0.0,b3=0.0;
	// Compute matrix A and vector b
	int itr;
	for(itr=0; itr < NtrackL; itr++){ 

	    gl3Track *trk=trackL[itr];
	    if(trk==NULL) continue;
	    double xp=XdcaHitL[itr];
	    double yp=YdcaHitL[itr];
	    double zp=ZdcaHitL[itr];
      
	    double mag=sqrt(1+trk->tanl*trk->tanl); 
	    double xhat=cos(trk->psi)/mag;    // wrong, temp not at DCA
	    double yhat=sin(trk->psi)/mag;    // wrong, temp not at DCA
	    double zhat=trk->tanl/mag;

	    /* the transverse extrapolation error of the track DR= Dtheta*L
	       where Dtheta is angular straggling, L is path from vertex to closest point on the track
	       Below following approximations are made: 
	         - all particles are pions 
		 - Dtheta=const /beta/P
		 - P= PT**sqrt(1+tanl**2)
		 - L = Rxy(hit)*sqrt(1+tanl**2)
		 so DR=const * Rxy /beta/PT
	    */

	    double sigma=1;
	    gl3Hit *h=(gl3Hit *)trk->lastHit;
	    if(h) {
		  float Rxy=sqrt(h->getX()*h->getX() + h->getY()*h->getY());  
		  float pmom =trk->pt*mag;
		  float beta= pmom/sqrt(pmom*pmom+ 0.139*0.139);
		  sigma=0.0136 *Rxy/beta/trk->pt; // average sigma is 2.2
	    } else {
		  if(trk->id==888999 && trk->tanl>88899.) sigma=0.5; // 'beamTrack'
	    }

	    hjan[21]->Fill(sigma);
		 
	    A11=A11+(yhat*yhat+zhat*zhat)/sigma;
	    A12=A12-(xhat*yhat)/sigma;
	    A13=A13-(xhat*zhat)/sigma;
	    A22=A22+(xhat*xhat+zhat*zhat)/sigma;
	    A23=A23-(yhat*zhat)/sigma;
	    A33=A33+(xhat*xhat+yhat*yhat)/sigma;
	    b1=b1 + ( (yhat*yhat+zhat*zhat)*xp - xhat*yhat*yp - xhat*zhat*zp )/sigma;
	    b2=b2 + ( (xhat*xhat+zhat*zhat)*yp - xhat*yhat*xp - yhat*zhat*zp )/sigma;
	    b3=b3 + ( (xhat*xhat+yhat*yhat)*zp - xhat*zhat*xp - yhat*zhat*yp )/sigma;
	}// end of  1-st loop over tracks

	A21 = A12; A31=A13; A32=A23;

	// Invert A
	double detA =   A11*A22*A33 + A12*A23*A31 + A13*A21*A32;
	detA = detA   - A31*A22*A13 - A32*A23*A11 - A33*A21*A12;
	//    cout<<"Determinant= "<<detA<<endl;
	//    cout<<"A11,A12,A13: "<<A11<<" "<<A12<<" "<<A13<<endl;
	//    cout<<"A21,A22,A23: "<<A21<<" "<<A22<<" "<<A23<<endl;
	//    cout<<"A31,A32,A33: "<<A31<<" "<<A32<<" "<<A33<<endl;
	//    cout<<"b1,b2,b3 "<<b1<<" "<<b2<<" "<<b3<<endl;
	C11=(A22*A33-A23*A32)/detA; C12=(A13*A32-A12*A33)/detA; C13=(A12*A23-A13*A22)/detA;
	C21=C12;                    C22=(A11*A33-A13*A31)/detA; C23=(A13*A21-A11*A23)/detA;
	C31=C13;                    C32=C23;                    C33=(A11*A22-A12*A21)/detA;

	// Find Vertex Position
	double Xv = C11*b1 + C12*b2 + C13*b3;
	double Yv = C21*b1 + C22*b2 + C23*b3;
	double Zv = C31*b1 + C32*b2 + C33*b3;

	if(lmvdbg) {
	    cout<<"current Vertex Position   : "<<Xv<<" "<<Yv<<" "<<Zv<<"  nTR used="<<NtrackUsed<<endl;
	    cout<<"        Vertex Error      : "<<sqrt(C11)<<" "<<sqrt(C22)<<" "<<sqrt(C33)<<endl;
	}
	
	// Check if the fit is any good
	// Loop over tracks again to get Chi2 and check each track's deviation
	double dmax=0.0;
	int iworse=-1;

	for(itr=0; itr < NtrackL; itr++){ 
	    gl3Track *trk=trackL[itr];
	    if(trk==NULL) continue;
	    if(trk->lastHit==NULL) continue; // ignore 'beamTrack'

	    double xp=XdcaHitL[itr];
	    double yp=YdcaHitL[itr];
	    double zp=ZdcaHitL[itr];

	    if(lmvdbg) printf("itr=%d, Zdca=%f Zvert=%f\n",itr,zp,Zv);
	    double d2=(xp-Xv)*(xp-Xv) +(yp-Yv)*(yp-Yv) +(zp-Zv)*(zp-Zv);
	    double d=sqrt(d2);
	    double sig=1; // temp2
	    chi2 = chi2 + d2/(sig*sig);
	    double drel = d/sig;
	    if(lmvdbg)  printf(" itr=%d, drel/sig=%f d=%f/sig sig=%f\n",itr,drel,d, sig);
	    if( drel > dmax ){
		// Save the track that deviates the most from vertex
		dmax = drel;
		iworse = itr;
	    }
	}

	if( dmax > DelVertMax ){
	      if(lmvdbg)  cout<<"Removing a track "<<iworse<< " with dmax= "<<dmax<<endl;
	    trackL[iworse]=NULL;
	    NtrackUsed--;
	    done=0;
	}
	else{
	    done=1;
	    myV->x=Xv; 
	    myV->y=Yv; 
	    myV->z=Zv; 
	}
    } // End While Loop

    //double  chi2pdof = chi2/(NtrackUsed-1);
    if(lmvdbg) {
	cout << "ppLMV4b: Primary Vertex found:  used tracks=" 
	     << NtrackUsed << " out of " << NtrackL << " matched and " 
	     << /*event->getNTracks() << */" L3-tracks" << endl
	     <<"   at  Position   : " << myV->x << " " << myV->y << " " 
	     << myV->z << endl;
    }
    //tag used tracks
    for(int itr=0; itr < NtrackL; itr++){ 
	gl3Track *trk=trackL[itr];
	if(trk==NULL) continue;
	trk->flag+=0x2; // mark track as used by lmv-l3
    }
    
    return NtrackUsed;
}



//####################################################################
//
//####################################################################
void gl3LMVertexFinder::setCtbMap() {
    ctbMap[1-1][1-1]=109;
    ctbMap[2-1][1-1]=108;
    ctbMap[3-1][1-1]=107;
    ctbMap[4-1][1-1]=106;
    ctbMap[5-1][1-1]=105;
    ctbMap[6-1][1-1]=7;
    ctbMap[7-1][1-1]=6;
    ctbMap[8-1][1-1]=5; 
    ctbMap[9-1][1-1]=4;
    ctbMap[10-1][1-1]=3;
    ctbMap[11-1][1-1]=2;
    ctbMap[12-1][1-1]=1;
    ctbMap[13-1][1-1]=0;
    ctbMap[14-1][1-1]=15;
    ctbMap[15-1][1-1]=14;
    ctbMap[16-1][1-1]=13;
    ctbMap[17-1][1-1]=12;
    ctbMap[18-1][1-1]=11;
    ctbMap[19-1][1-1]=10;
    ctbMap[20-1][1-1]=9;
    ctbMap[21-1][1-1]=39;
    ctbMap[22-1][1-1]=38;
    ctbMap[23-1][1-1]=37;
    ctbMap[24-1][1-1]=36;
    ctbMap[25-1][1-1]=35;
    ctbMap[26-1][1-1]=34;
    ctbMap[27-1][1-1]=33;
    ctbMap[28-1][1-1]=32;
    ctbMap[29-1][1-1]=47;
    ctbMap[30-1][1-1]=46;
    ctbMap[31-1][1-1]=45;
    ctbMap[32-1][1-1]=44;
    ctbMap[33-1][1-1]=43;
    ctbMap[34-1][1-1]=42;
    ctbMap[35-1][1-1]=41;
    ctbMap[36-1][1-1]=71;
    ctbMap[37-1][1-1]=70;
    ctbMap[38-1][1-1]=69;
    ctbMap[39-1][1-1]=68;
    ctbMap[40-1][1-1]=67;
    ctbMap[41-1][1-1]=66;
    ctbMap[42-1][1-1]=65;
    ctbMap[43-1][1-1]=64;
    ctbMap[44-1][1-1]=79;
    ctbMap[45-1][1-1]=78;
    ctbMap[46-1][1-1]=77;
    ctbMap[47-1][1-1]=76;
    ctbMap[48-1][1-1]=75;
    ctbMap[49-1][1-1]=74;
    ctbMap[50-1][1-1]=73;
    ctbMap[51-1][1-1]=103;
    ctbMap[52-1][1-1]=102;
    ctbMap[53-1][1-1]=101;
    ctbMap[54-1][1-1]=100;
    ctbMap[55-1][1-1]=99;
    ctbMap[56-1][1-1]=98;
    ctbMap[57-1][1-1]=97;
    ctbMap[58-1][1-1]=96;
    ctbMap[59-1][1-1]=111;
    ctbMap[60-1][1-1]=110;
    ctbMap[1-1][2-1]=125;
    ctbMap[2-1][2-1]=124;
    ctbMap[3-1][2-1]=123;
    ctbMap[4-1][2-1]=122;
    ctbMap[5-1][2-1]=121;
    ctbMap[6-1][2-1]=23;
    ctbMap[7-1][2-1]=22;
    ctbMap[8-1][2-1]=21;
    ctbMap[9-1][2-1]=20;
    ctbMap[10-1][2-1]=19;
    ctbMap[11-1][2-1]=18;
    ctbMap[12-1][2-1]=17;
    ctbMap[13-1][2-1]=16;
    ctbMap[14-1][2-1]=31;
    ctbMap[15-1][2-1]=30;
    ctbMap[16-1][2-1]=29;
    ctbMap[17-1][2-1]=28;
    ctbMap[18-1][2-1]=27;
    ctbMap[19-1][2-1]=26;
    ctbMap[20-1][2-1]=25;
    ctbMap[21-1][2-1]=55;
    ctbMap[22-1][2-1]=54;
    ctbMap[23-1][2-1]=53;
    ctbMap[24-1][2-1]=52;
    ctbMap[25-1][2-1]=51;
    ctbMap[26-1][2-1]=50;
    ctbMap[27-1][2-1]=49;
    ctbMap[28-1][2-1]=48;
    ctbMap[29-1][2-1]=63;
    ctbMap[30-1][2-1]=62;
    ctbMap[31-1][2-1]=61;
    ctbMap[32-1][2-1]=60;
    ctbMap[33-1][2-1]=59;
    ctbMap[34-1][2-1]=58;
    ctbMap[35-1][2-1]=57;
    ctbMap[36-1][2-1]=87;
    ctbMap[37-1][2-1]=86;
    ctbMap[38-1][2-1]=85;
    ctbMap[39-1][2-1]=84;
    ctbMap[40-1][2-1]=83;
    ctbMap[41-1][2-1]=82;
    ctbMap[42-1][2-1]=81;
    ctbMap[43-1][2-1]=80;
    ctbMap[44-1][2-1]=95;
    ctbMap[45-1][2-1]=94;
    ctbMap[46-1][2-1]=93;
    ctbMap[47-1][2-1]=92;
    ctbMap[48-1][2-1]=91;
    ctbMap[49-1][2-1]=90;
    ctbMap[50-1][2-1]=89;
    ctbMap[51-1][2-1]=119;
    ctbMap[52-1][2-1]=118;
    ctbMap[53-1][2-1]=117;
    ctbMap[54-1][2-1]=116;
    ctbMap[55-1][2-1]=115;
    ctbMap[56-1][2-1]=114;
    ctbMap[57-1][2-1]=113;
    ctbMap[58-1][2-1]=112;
    ctbMap[59-1][2-1]=127;
    ctbMap[60-1][2-1]=126;
    ctbMap[61-1][1-1]=141;
    ctbMap[62-1][1-1]=140;
    ctbMap[63-1][1-1]=139;
    ctbMap[64-1][1-1]=138;
    ctbMap[65-1][1-1]=137;
    ctbMap[66-1][1-1]=167;
    ctbMap[67-1][1-1]=166;
    ctbMap[68-1][1-1]=165;
    ctbMap[69-1][1-1]=164;
    ctbMap[70-1][1-1]=163;
    ctbMap[71-1][1-1]=162;
    ctbMap[72-1][1-1]=161;
    ctbMap[73-1][1-1]=160;
    ctbMap[74-1][1-1]=175;
    ctbMap[75-1][1-1]=174;
    ctbMap[76-1][1-1]=173;
    ctbMap[77-1][1-1]=172;
    ctbMap[78-1][1-1]=171;
    ctbMap[79-1][1-1]=170;
    ctbMap[80-1][1-1]=169;
    ctbMap[81-1][1-1]=199;
    ctbMap[82-1][1-1]=198;
    ctbMap[83-1][1-1]=197;
    ctbMap[84-1][1-1]=196;
    ctbMap[85-1][1-1]=195;
    ctbMap[86-1][1-1]=194;
    ctbMap[87-1][1-1]=193;
    ctbMap[88-1][1-1]=192;
    ctbMap[89-1][1-1]=207;
    ctbMap[90-1][1-1]=206;
    ctbMap[91-1][1-1]=205;
    ctbMap[92-1][1-1]=204;
    ctbMap[93-1][1-1]=203;
    ctbMap[94-1][1-1]=202;
    ctbMap[95-1][1-1]=201;
    ctbMap[96-1][1-1]=231;
    ctbMap[97-1][1-1]=230;
    ctbMap[98-1][1-1]=229;
    ctbMap[99-1][1-1]=228;
    ctbMap[100-1][1-1]=227;
    ctbMap[101-1][1-1]=226;
    ctbMap[102-1][1-1]=225;
    ctbMap[103-1][1-1]=224;
    ctbMap[104-1][1-1]=239;
    ctbMap[105-1][1-1]=239;
    ctbMap[106-1][1-1]=237;
    ctbMap[107-1][1-1]=236;
    ctbMap[108-1][1-1]=235;
    ctbMap[109-1][1-1]=234;
    ctbMap[110-1][1-1]=233;
    ctbMap[111-1][1-1]=135;
    ctbMap[112-1][1-1]=134;
    ctbMap[113-1][1-1]=133;
    ctbMap[114-1][1-1]=132;
    ctbMap[115-1][1-1]=131;
    ctbMap[116-1][1-1]=130;
    ctbMap[117-1][1-1]=129;
    ctbMap[118-1][1-1]=128;
    ctbMap[119-1][1-1]=143;
    ctbMap[120-1][1-1]=142;
    ctbMap[61-1][2-1]=157;
    ctbMap[62-1][2-1]=156;
    ctbMap[63-1][2-1]=155;
    ctbMap[64-1][2-1]=154;
    ctbMap[65-1][2-1]=153;
    ctbMap[66-1][2-1]=183;
    ctbMap[67-1][2-1]=182;
    ctbMap[68-1][2-1]=181;
    ctbMap[69-1][2-1]=180;
    ctbMap[70-1][2-1]=179;
    ctbMap[71-1][2-1]=178;
    ctbMap[72-1][2-1]=177;
    ctbMap[73-1][2-1]=176;
    ctbMap[74-1][2-1]=191;
    ctbMap[75-1][2-1]=190;
    ctbMap[76-1][2-1]=189;
    ctbMap[77-1][2-1]=188;
    ctbMap[78-1][2-1]=187;
    ctbMap[79-1][2-1]=186;
    ctbMap[80-1][2-1]=185;
    ctbMap[81-1][2-1]=215;
    ctbMap[82-1][2-1]=214;
    ctbMap[83-1][2-1]=213;
    ctbMap[84-1][2-1]=212;
    ctbMap[85-1][2-1]=211;
    ctbMap[86-1][2-1]=210;
    ctbMap[87-1][2-1]=209;
    ctbMap[88-1][2-1]=208;
    ctbMap[89-1][2-1]=223;
    ctbMap[90-1][2-1]=222;
    ctbMap[91-1][2-1]=221;
    ctbMap[92-1][2-1]=220;
    ctbMap[93-1][2-1]=219;
    ctbMap[94-1][2-1]=218;
    ctbMap[95-1][2-1]=217;
    ctbMap[96-1][2-1]=247;
    ctbMap[97-1][2-1]=246;
    ctbMap[98-1][2-1]=245;
    ctbMap[99-1][2-1]=244;
    ctbMap[100-1][2-1]=243;
    ctbMap[101-1][2-1]=242;
    ctbMap[102-1][2-1]=241;
    ctbMap[103-1][2-1]=240;
    ctbMap[104-1][2-1]=255;
    ctbMap[105-1][2-1]=254;
    ctbMap[106-1][2-1]=253;
    ctbMap[107-1][2-1]=252;
    ctbMap[108-1][2-1]=251;
    ctbMap[109-1][2-1]=250;
    ctbMap[110-1][2-1]=249;
    ctbMap[111-1][2-1]=151;
    ctbMap[112-1][2-1]=150;
    ctbMap[113-1][2-1]=149;
    ctbMap[114-1][2-1]=148;
    ctbMap[115-1][2-1]=147;
    ctbMap[116-1][2-1]=146;
    ctbMap[117-1][2-1]=145;
    ctbMap[118-1][2-1]=144;
    ctbMap[119-1][2-1]=159;
    ctbMap[120-1][2-1]=158;

    zSlats[0][0] =  64;;
    zSlats[0][1] =  191. ;
    zSlats[1][0] = -64.;
    zSlats[1][1] = -191.;

   
#ifdef GL3ROOT
    printf("the Ctb Map from Pablo from herb is set\n");
#endif
    return  ;
}



