/*
StiKalmanTrackFinder::extendTracksToVertex: _trackContainer->size() 3616
StiKalmanTrackFinder::extendTracksToVertex:Track 1
StiKalmanTrackFinder::extendTracksToVertex:Track  Chi2: 121.916 q: 1 pt: 40.3814 eta: 0.782944 tanLambda: 0.865423 points/fit/max: 14/7/14
StiKalmanTrack::extendToVertex: chi2 @ vtx: 14338.6 dx:0 dy:88.3861 dz:-52.7791 d: 102.945 dca: 102.945 npoints tpc/svt: 14/0
StiKalmanTrackFinder::extendTracksToVertex:Track 2
StiKalmanTrackFinder::extendTracksToVertex:Track  Chi2: 15402.7 q: 1 pt: 39.8791 eta: 0.483336 tanLambda: 0.502376 points/fit/max: 10/4/10
StiKalmanTrack::extendToVertex: chi2 @ vtx: 11066.4 dx:0 dy:-54.5844 dz:54.1684 d: 76.9004 dca: 76.9004 npoints tpc/svt: 10/0
*/
struct BPoint_t {
  Float_t NoTrk, Track, Chi2, Prob, q, pt, eta, tanLambda, points, fit, max, chi2vtx, dx, dy, dz, d, dca, tpc, svt;
};
BPoint_t BPoint;
//________________________________________________________________________________
void MakeNtupleFromCheck(const Char_t *FileName="CheckDCA.data"){
  FILE *fp = fopen(FileName,"r");
  if (! fp) {cout << "Can't open " << FileName << endl; return;}
  TString fName(FileName);
  fName.ReplaceAll(".data",".root");
  f = new TFile(fName.Data(),"RECREATE");
  FitP = new TNtuple("FitP","CheckDCA","NoTrk:Track:Chi2:Prob:q:pt:eta:tanLambda:points:fit:max:chi2vtx:dx:dy:dz:d:dca:tpc:svt");
  FitP->SetMarkerStyle(20);
  FitP->SetLineWidth(2);
  char line[181];
  Int_t i = 0;
  //  fgets(&line[0],180,fp);
  Float_t  NoTrk, Track, Chi2, Prob, q, pt, eta, tanLambda, points, fit, max, chi2vtx, dx, dy, dz, d, dca, tpc, svt;
  NoTrk = Track = Chi2 = Prob = q = pt = eta = tanLambda = points = fit = max = chi2vtx = dx = dy = dz = d = dca = tpc = svt = 0;
  while (fgets(&line[0],180,fp)) {
    if (i%10000 == 1) cout << "read:\t" << line << endl;
    if (strstr(line,"StiKalmanTrackFinder::extendTracksToVertex: _trackContainer->size()")) {
      NoTrk = Track = Chi2 = Prob = q = pt = eta = tanLambda = points = fit = max = chi2vtx = dx = dy = dz = d = dca = tpc = svt = 0; 
      sscanf(&line[0],"StiKalmanTrackFinder::extendTracksToVertex: _trackContainer->size() %f",&NoTrk);
      if (i%10000 == 1) printf("NoTrk = %f\n",NoTrk);
      continue;
    }
    if (strstr(line,"StiKalmanTrackFinder::extendTracksToVertex:Track  Chi2")) {
      sscanf(&line[0],"StiKalmanTrackFinder::extendTracksToVertex:Track  Chi2: %f q: %f pt: %f eta: %f tanLambda: %f points/fit/max: %f/%f/%f",
	     &Chi2,&q,&pt,&eta,&tanLambda,&points,&fit,&max);
      if (fit > 3) Prob = TMath::Prob(Chi2, 2*fit - 5);
      if (i%10000 == 1) printf(" Chi2: %f q: %f pt: %f eta: %f tanLambda: %f points/fit/max: %f/%f/%f\n",
	     Chi2,q,pt,eta,tanLambda,points,fit,max);
      chi2vtx = dx = dy = dz = d = dca = tpc = svt = 0;
      continue;
    }
    if (strstr(line,"StiKalmanTrackFinder::extendTracksToVertex:Track")) {
      sscanf(&line[0],"StiKalmanTrackFinder::extendTracksToVertex:Track %f",&Track);
      if (i%10000 == 1) printf("Track = %f\n",Track);
      Chi2 = Prob = q = pt = eta = tanLambda = points = fit = max = chi2vtx = dx = dy = dz = d = dca = tpc = svt = 0;
      continue;
    }
    if (strstr(line,"StiKalmanTrack::extendToVertex: chi2 @ vtx:")) {
      sscanf(&line[0],"StiKalmanTrack::extendToVertex: chi2 @ vtx: %f dx:%f dy:%f dz: %f d: %f dca: %f npoints tpc/svt: %f/%f",
	     &chi2vtx,&dx,&dy,&dz,&d,&dca,&tpc,&svt);
      if (i%10000 == 1) printf("chi2 @ vtx: %f dx:0 dy:%f dz: %f d: %f dca: %f npoints tpc/svt: %f/%f\n",
	     chi2vtx,dx,dy,dz,d,dca,tpc,svt);
      BPoint.NoTrk = NoTrk ;
      BPoint.Track = Track ;
      BPoint.Chi2 = Chi2 ;
      BPoint.Prob = Prob ;
      BPoint.q = q ;
      BPoint.pt = pt ;
      BPoint.eta = eta ;
      BPoint.tanLambda = tanLambda ;
      BPoint.points = points ;
      BPoint.fit = fit ;
      BPoint.max = max ;
      BPoint.chi2vtx = chi2vtx ;
      BPoint.dx = dx ;
      BPoint.dy = dy ;
      BPoint.dz = dz ;
      BPoint.d = d ;
      BPoint.dca = dca ;
      BPoint.tpc = tpc ;
      BPoint.svt 	= svt; 	
      FitP->Fill(&BPoint.NoTrk);
    }
    i++;
    if (i%10000 == 1) cout << "i:" << i << " =======================" << endl;
    //    if (i > 5000) break;
  }
  fclose(fp);
  f->Write();
}
