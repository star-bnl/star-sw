{
   gSystem->Load("/afs/rhic/star/users/yepes/wrk/pams/l3/example/FtfSl3.so");
   FtfGraphic tracker ;
   int debugLevel = 5 ;
//
   hxyChi2 = new TH1F("hxyChi2","xy chi2",50,0,100);
   hszChi2 = new TH1F("hszChi2","sz chi2",50,0,100);
   hCharge = new TH1F("hCharge","charge ",50,0,3000);
   hChaTrk = new TH1F("hChaTrk","charge in tracks",50,0,3000);
   jPsiMass= new TH1F("jPsiMass","JPsi inv. mass",100,0,5.);
//
//   Setup tracker
//
   tracker.debugLevel = debugLevel ;
   tracker.para.fillTracks = 1 ;
   FtfPara* para = &(tracker.para);
   tracker.setup(100000,10000);
   para->minHitsPerTrack = 5;
   para->hitChi2Cut=900. ;
   para->trackChi2Cut =900 ;
// para->goodHitChi2  = 30 ;
// para->trackRowSearchRange = 2 ;
   para->ptMinHelixFit = 0 ;
   para->nPrimaryPasses = 1 ;
   para->nSecondaryPasses = 0 ;
   para->phiMin = 0 ;
   para->phiMax = 2.*acos(-1) ;
   para->etaMin = -2.2 ;
   para->etaMax =  2.2 ;
   para->fillTracks = 1 ;
   para->vertexConstrainedFit = 1 ;
   para->init = 0 ;
//
//  Setup input file
//
   char* fileName = "jpsi_1.xyzdat";
// char* fileName = "jpsi1000rest.xyzdat";
   FILE* datafile = fopen( fileName, "ra");
   if (datafile == NULL) {
      printf ( " \n Error opening input file \n " ) ;
      return 1 ;
   }
//
//   read ascii file with cosmic data
//
  int i ;
  int counter = 0 ;
  float x,y,z ;
  int row ;
  for ( i=0 ; i<100000000 ; i++ )
  {
    if ( fscanf ( datafile, "%f %f %f %d", &x,&y,&z,&row) == EOF ) break ;
//
    tracker.hit[counter].id       = i ;
    tracker.hit[counter].row      = (int)fmod(row,100);
    tracker.hit[counter].x        = x;
    tracker.hit[counter].y        = y;
    tracker.hit[counter].z        = z;
    tracker.hit[counter].dx       = 0.2F ;
    tracker.hit[counter].dy       = 0.2F ;
    tracker.hit[counter].dz       = 0.2F ;
//  printf ( "x y z %e %e %e\n", x, y, z ) ;
    counter++ ;
   }
   printf ( "Done reading space poitns\n");
   tracker.nHits = counter ;
//
//   Process sector
//
   tracker.processSector ( );
//
//    Set range view for graphics
//
   tracker.xMin   = -120. ;
   tracker.xMax   =  120. ;
   tracker.yMin   = -120. ; 
   tracker.yMax   =  120. ; 
   tracker.zMin   = -150. ; 
   tracker.zMax   =  150. ; 
   tracker.trackWidth = 3 ;

// char cc ;
//
//   Set rapidity window for graphics
//
   tracker.etaMin =-2.4 ;
   tracker.etaMax = 2.4 ;
//
   tracker.plotDetector();   
   tracker.plotHits ( ) ;
   tracker.plotFitsa (1,1000 ) ;
// cc = getchar();
//
   int i, j ;
   double e1, px1, py1, pz1 ;
   double e2, px2, py2, pz2 ;
   double e , px , py , pz  ;
   double mass, mass2 ;   
   int nTracks = 10 ;
   nTracks = tracker.nTracks ;
   for ( i = 0 ; i < nTracks ; i++ ) {
      px1 = tracker.track[i].pt * cos(tracker.track[i].psi);    
      py1 = tracker.track[i].pt * sin(tracker.track[i].psi);    
      pz1 = tracker.track[i].pt * tracker.track[i].tanl;    
      e1  = tracker.track[i].pt * sqrt ( 1. + tracker.track[i].tanl*
                                              tracker.track[i].tanl ) ;
      for ( j = 0 ; j < nTracks ; j++ ) {
         px2 = tracker.track[j].pt * cos(tracker.track[j].psi);    
         py2 = tracker.track[j].pt * sin(tracker.track[j].psi);    
         pz2 = tracker.track[j].pt * tracker.track[j].tanl;    
         e2  = tracker.track[j].pt * sqrt ( 1. + tracker.track[j].tanl*
                                                 tracker.track[j].tanl ) ;
         px = px1 + px2 ;
         py = py1 + py2 ;
         pz = pz1 + pz2 ;
         e  = e1  + e2 ;
         mass2 = e*e-px*px-py*py-pz*pz;
         if ( mass2 > 0 ) mass = sqrt(e*e-px*px-py*py-pz*pz);
         else mass = 0 ;
         jPsiMass->Fill ( mass, 1. ) ;
      // printf ( "mass %e \n",mass ) ;
      }
   }
//
//   Fill histos
//
   for ( i = 0 ; i < tracker.nHits ; i++ ) {
      hCharge->Fill ( tracker.hit[i].q, 1. ) ;
      if ( tracker.hit[i].track != 0 ) {
         hxyChi2->Fill ( tracker.hit[i].xyChi2, 1. ) ;
         hszChi2->Fill ( tracker.hit[i].szChi2, 1. ) ;
         hChaTrk->Fill ( tracker.hit[i].q, 1. ) ;
      } 
   }

};
