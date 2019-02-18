//
// This macro assumes that there is a file containing TH2Ds with the name like:
//   Epd_X_WhenBbc_Y_Pmt_Z_fires   where _X_ is East or West.  _Y_ is East or West.  _Z_ is the phototube number
//   EpdEastWhenBbcEastPmt7fires
// That file is produced by analyzing STAR datafiles

// you might need this "R__LOAD_LIBRARY" line if you are running on your laptop with root6
// otherwise the damned thing won't even *try*
// I guess it's some kind of "super-pre-compiler directive" or something...
//R__LOAD_LIBRARY(StEpdUtil/StEpdUtil.so)    

class StEpdGeom;
class StBbcGeom;

void DrawBbcOutline(unsigned short ipmt, short iew);

//---------------------------------------------------------------
void SeeEpdBbcCosmics(TString filein){

  gSystem->Load("StEpdUtil");        // this is what you will want at RCF.  On my laptop, root says not found but there is no problem.

  TFile* tfin = new TFile(filein.Data(),"READ");

  TCanvas* tc = new TCanvas("EpdBbcCorrel","EpdBbcCorrel",1200,700);
  tc->SaveAs("EpdBbcCorrel.pdf[");
  tc->Divide(2,1);
  for (short iewBBC=0; iewBBC<2; iewBBC++){
    TString ewb = (iewBBC==0)?"East":"West";
    for (unsigned short ipmt=1; ipmt<17; ipmt++){
      TH2D* EpdEast = (TH2D*)tfin->Get(Form("EpdEastWhenBbc%sPmt%dfires",ewb.Data(),ipmt));
      tc->cd(1);      EpdEast->Draw();
      if (iewBBC==0) DrawBbcOutline(ipmt,iewBBC);
      TH2D* EpdWest = (TH2D*)tfin->Get(Form("EpdWestWhenBbc%sPmt%dfires",ewb.Data(),ipmt));
      tc->cd(2);      EpdWest->Draw();
      if (iewBBC==1) DrawBbcOutline(ipmt,iewBBC);
      tc->SaveAs("EpdBbcCorrel.pdf");
    }
  }
  tc->SaveAs("EpdBbcCorrel.pdf)");
}

//---------------------------------------------------------------
void DrawBbcOutline(unsigned short ipmt, short iew){

  StBbcGeom g;
  unsigned short ntiles,tiles[2];

  g.GetTilesOfPmt(ipmt,&ntiles,tiles);
  for (int i=0; i<ntiles; i++){
    double x[7],y[7];
    g.GetCorners(tiles[i],iew,x,y);
    x[6]=x[0];    y[6]=y[0];         // must make a closed shape
    TPolyLine* pline = new TPolyLine(7,x,y);
    pline->SetLineColor(kRed);
    pline->Draw();
  }
}
      


