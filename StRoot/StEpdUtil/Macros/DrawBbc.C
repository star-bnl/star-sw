class StBbcGeom;

void DrawBbc(){

  gSystem->Load("StEpdUtil");
  StBbcGeom* Bgeo = new StBbcGeom();

  
  TCanvas* cEast = new TCanvas("EastCan","EastCan",1000,1000);
  cEast->Draw();
  TH2D* hEast = new TH2D("East","East",500,-100,100,500,-100,100);
  hEast->Draw();
  

  for (short tileId=-18; tileId<0; tileId++){     // NOTE negative tileId because East Bbc
    cout << "tileID = " << tileId << endl;
    unsigned short tid = abs(tileId);
    TVector3 cent = Bgeo->TileCenter(tid,0);

    //    TVector3 cent = Bgeo->TileCenter(tileId);
    cout << cent.X() << " " << cent.Y() << " " << cent.Z() << endl;

    TText* txt = new TText(cent.X(),cent.Y(),Form("%d",tileId));
    txt->SetTextSize(.02);
    txt->SetTextAlign(22);
    txt->Draw();

    double x[7];
    double y[7];
    Bgeo->GetCorners(tileId,x,y);
    x[6]=x[0];    y[6]=y[0];
    TPolyLine* pline = new TPolyLine(7,x,y);
    pline->SetLineColor(4);
    pline->SetLineWidth(2);
    pline->Draw();


    unsigned short pmtNumber = Bgeo->PmtOfTile(tid);
    TText* Ptxt = new TText(cent.X(),cent.Y()-5.0,Form("%d",pmtNumber));
    Ptxt->SetTextSize(0.01);
    Ptxt->SetTextColor(2);
    Ptxt->Draw();

  }


}
      
      


