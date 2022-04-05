// draws boxes representing FMS Pb-glass cells, in units of mm

void draw_survey()
{
  TFile * infile = new TFile("geotr.root","READ");
  TTree * tr = (TTree*) infile->Get("geotr");
  Int_t nstb,row,col;
  tr->SetBranchAddress("nstb",&nstb);
  tr->SetBranchAddress("row",&row);
  tr->SetBranchAddress("col",&col);

  const Double_t large_width = 58.0; // large cell width
  const Double_t small_width = 38.0; // small cell width

  Double_t x,y,z,x1,y1,x2,y2;
  Int_t it=0;
  Int_t color;
  TLine * cell[4][1264];

  for(Int_t i=0; i<tr->GetEntries(); i++)
  {
    tr->GetEntry(i);
    switch(nstb)
    {
      case 1:
        x = -24.0 - 25.0 - 58.6 * col;
        y = 965.5 - 58.5 * row;
        z = 7208;
        color = (Int_t) kMagenta;
        break;
      case 2:
        x = -24.0 - 8.0 + 66.6 + 58.4 * col; //new position May 5 2015
        y = 965.1 - 58.6 * row;
        z = 7188;
        color = (Int_t) kRed;
        break;
      case 3:
        if(row>16.5)
        {
          x = -22.0 - 17.7 - 38.2 * col;
          y = 422.7 - 37.9 * row;
          z = 7208;
          color = (Int_t) kBlue;
        }
        else
        {
          x = -22.0 - 17.7 - 38.2 * col;
          y = 4.75 + 438.9 - 39.0 * row;
          z = 7204;
          color = (Int_t) kCyan;
        };
        break;
      case 4:
        x = -22.0 - 8.0 + 60. + 38.4 * col; // new position May 5 2015
        y = 444. - 38.7 * row;
        z = 7188;
        color = (Int_t) kGreen+2;
        break;
    };

    if(nstb<=2) 
    {
      x1 = x - large_width/2.0;
      x2 = x + large_width/2.0;
      y1 = y - large_width/2.0;
      y2 = y + large_width/2.0;
    }
    else
    {
      x1 = x - small_width/2.0;
      x2 = x + small_width/2.0;
      y1 = y - small_width/2.0;
      y2 = y + small_width/2.0;
    };

    cell[0][it] = new TLine(x1,y1,x2,y1); //t
    cell[1][it] = new TLine(x1,y2,x2,y2); //b
    cell[2][it] = new TLine(x1,y1,x1,y2); //l
    cell[3][it] = new TLine(x2,y1,x2,y2); //r
    for(Int_t j=0; j<4; j++) cell[j][it]->SetLineColor(color);
    it++;
  };

  Double_t md = 1200;
  Int_t factor = 1; // bins per millimeter... set this too high and you'll get a memory leak!
  factor *= 2;
  TH2D * bg = new TH2D("survey","FMS cells survey [mm]",factor*md,-1*md,md,factor*md,-1*md,md);
  TCanvas * survey = new TCanvas("survey","survey",700,700);
  gStyle->SetOptStat(0);
  bg->Draw();
  for(Int_t i=0; i<1264; i++) for(Int_t j=0; j<4; j++) cell[j][i]->Draw();
};
