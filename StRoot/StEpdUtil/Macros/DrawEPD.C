class StEpdGeom;

void DrawEPD(){

  gSystem->Load("StEpdUtil");
  StEpdGeom* geo = new StEpdGeom();

  TCanvas* cEast = new TCanvas("EastCan","EastCan",600,600);
  cEast->Draw();
  TH2D* hEast = new TH2D("East","East",500,-100,100,500,-100,100);
  hEast->Draw();

  Short_t EW=-1;  // East Side
  Double_t x[6];
  Double_t y[6];
  Int_t nCorners;
  for (Short_t PP=1; PP<=12; PP++){
    for (Short_t TT=1; TT<=31; TT++){

      // Example of StEpdGeom::GetCorners
      geo->GetCorners(PP,TT,EW,&nCorners,x,y);
      x[nCorners]=x[0];    y[nCorners]=y[0];   // closes the polyline
      TPolyLine* pline = new TPolyLine(nCorners+1,x,y);
      pline->Draw();

      // Example of StEpdGeom::RandomPointOnTile
      if ((PP*TT)%2==1){
	for (Int_t nPoint=0; nPoint<300; nPoint++){
	  TVector3 point = geo->RandomPointOnTile(PP,TT,EW);
	  hEast->Fill(point.X(),point.Y());
	}
      }

      // Example of StEpdGeom::TileCenter and StEpdGeom::Row
      if ((PP%2==0)&&(TT%2==0)){
	Double_t xcent = geo->TileCenter(PP,TT,EW).X();
	Double_t ycent = geo->TileCenter(PP,TT,EW).Y();
	TText* lab = new TText(xcent,ycent,Form("Row %d",geo->Row(PP,TT,EW)));
	lab->SetTextAlign(22);  lab->SetTextSize(0.01);  lab->SetTextColor(2);
	lab->Draw();
      }

    }
  }
}


