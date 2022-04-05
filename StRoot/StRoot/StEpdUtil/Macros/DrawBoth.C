class StEpdGeom;
class StBbcGeom;

void DrawBoth(){

  gSystem->Load("StEpdUtil");
  StBbcGeom* Bgeo = new StBbcGeom();
  StEpdGeom* geo = new StEpdGeom();


  TCanvas* cBoth = new TCanvas("BothCan","BothCan",1400,700);
  cBoth->Divide(2);
  cBoth->Draw();
  TPad* EastPad=cBoth->cd(1);
  TPad* WestPad=cBoth->cd(2);


  TH2D* hEastFrame = new TH2D("East","East",500,-100,100,500,-100,100);
  EastPad->cd();
  EastPad->SetGrid(0,0);
  hEastFrame->Draw();

  Short_t EW=-1;
  Double_t x[6];
  Double_t y[6];
  Int_t nCorners;
  for (Short_t PP=1; PP<=12; PP++){
    for (Short_t TT=1; TT<=31; TT++){

      geo->GetCorners(PP,TT,EW,&nCorners,x,y);

      x[nCorners]=x[0];    y[nCorners]=y[0];   // closes the polyline
      TPolyLine* pline = new TPolyLine(nCorners+1,x,y);
      pline->Draw();
      
      Double_t xcent = geo->TileCenter(PP,TT,EW).X();
      Double_t ycent = geo->TileCenter(PP,TT,EW).Y();
      TText* lab = new TText(xcent,ycent,Form("%d.%d",PP,TT));
      lab->SetTextAlign(22);  lab->SetTextSize(0.01);  lab->SetTextColor(1);
      lab->Draw();

    }
  }
  
  for (short tileId=-18; tileId<0; tileId++){     // NOTE negative tileId because East Bbc
    TVector3 cent = Bgeo->TileCenter(tileId);

    TText* txt = new TText(cent.X(),cent.Y(),Form("%d",tileId));
    txt->SetTextSize(.02);
    txt->SetTextAlign(22);
    txt->Draw();

    double xc[7];
    double yc[7];
    Bgeo->GetCorners(tileId,xc,yc);
    xc[6]=xc[0];    yc[6]=yc[0];
    TPolyLine* pline = new TPolyLine(7,xc,yc);
    pline->SetLineColor(4);
    pline->SetLineWidth(2);
    pline->Draw();


    unsigned short pmtNumber = Bgeo->PmtOfTile(abs(tileId));
    TText* Ptxt = new TText(cent.X(),cent.Y()-5.0,Form("%d",pmtNumber));
    Ptxt->SetTextSize(0.01);
    Ptxt->SetTextColor(2);
    Ptxt->Draw();

  }


  //-----------------------------

  TH2D* hWestFrame = new TH2D("West","West",500,-100,100,500,-100,100);
  WestPad->cd();
  WestPad->SetGrid(0,0);
  hWestFrame->Draw();

  Short_t EW=1;
  Double_t x[6];
  Double_t y[6];
  Int_t nCorners;
  for (Short_t PP=1; PP<=12; PP++){
    for (Short_t TT=1; TT<=31; TT++){

      geo->GetCorners(PP,TT,EW,&nCorners,x,y);

      x[nCorners]=x[0];    y[nCorners]=y[0];   // closes the polyline
      TPolyLine* pline = new TPolyLine(nCorners+1,x,y);
      pline->Draw();
      
      Double_t xcent = geo->TileCenter(PP,TT,EW).X();
      Double_t ycent = geo->TileCenter(PP,TT,EW).Y();
      TText* lab = new TText(xcent,ycent,Form("%d.%d",PP,TT));
      lab->SetTextAlign(22);  lab->SetTextSize(0.01);  lab->SetTextColor(1);
      lab->Draw();

    }
  }
  
  for (short tileId=1; tileId<19; tileId++){
    TVector3 cent = Bgeo->TileCenter(tileId);

    TText* txt = new TText(cent.X(),cent.Y(),Form("%d",tileId));
    txt->SetTextSize(.02);
    txt->SetTextAlign(22);
    txt->Draw();

    double xc[7];
    double yc[7];
    Bgeo->GetCorners(tileId,xc,yc);
    xc[6]=xc[0];    yc[6]=yc[0];
    TPolyLine* pline = new TPolyLine(7,xc,yc);
    pline->SetLineColor(4);
    pline->SetLineWidth(2);
    pline->Draw();


    unsigned short pmtNumber = Bgeo->PmtOfTile(abs(tileId));
    TText* Ptxt = new TText(cent.X(),cent.Y()-5.0,Form("%d",pmtNumber));
    Ptxt->SetTextSize(0.01);
    Ptxt->SetTextColor(2);
    Ptxt->Draw();

  }


  cBoth->SaveAs("Both.pdf");



}
      
      


