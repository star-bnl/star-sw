/********************************************************************

Author: Mike Daugherity
Description:  This macro reads a dNevent/dNch distribution and finds arbitrary centrality bins
                based on the number of analyzed tracks (surviving cuts) per event.
              Assumes that we're taking output from doEStructEmpty.C


Finding Centrality Bins:
1) Fix all track and event cuts first, everything else depends on these.
2) Produce a histogram of dNevent/dNch with decent statistics using above cuts.
     The fastest way is to run an 'empty' analysis (no pair analysis) using
     doEStructEmpty.C.  You can also use the NEventsSame plot from a standard
     correlations analysis (rename the plot loaded into nev).
3) Set the bins (given a percent of cross-section) in the const cent[] array below.
     Terminate this list with a -1.
4) Run this macro, the (text) output will show the number of tracks that corresponds
     to each centrality bin division. 

**********************************************************************/

void centrality(const char* infile)  {
  
  tf = new TFile(infile);
  TH1F* nev = tf->Get("hNEvent");  // load dN/dnch; change this line if not using output from doEStructEmpty.C
  TH1F* hvar = tf->Get("hvar");    // a test to compare with, not used in centrality

  nev->SetBinContent(1,0);  // make sure we have no events with zero mult. 

  nev->Scale( 1.0/nev->Integral() );
  hvar->Scale( 1.0/hvar->Integral("width") );
  
  float x1,y1,x2,y2;
  float gx1[1199],  gx2[1199], gy[1199];
  for(int i=1; i<=nev->GetNbinsX(); i++) {
    x1 = nev->GetBinLowEdge(i);
    x2 = pow(x1, 0.25);   // x2 = n^1/4
    y1 = nev->GetBinContent(i);
    y2 = pow(x1, 0.75)*y1; // y2 = n^3/4 * dNev/dNch 
    if (i>1) { gx1[i-2]=x1; gx2[i-2]=x2; gy[i-2]=y2; }  // skipping 1st bin (nch=0) 
  }
  TGraph* tg = new TGraph(1199, gx2,gy);

  float sum;
  int num,i;

  const float cent[]={0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.05, -1};  // centrality bins; don't erase the -1 !
    
  cout << "*** Using dN/dnch " << endl;
  sum=0;
  num=0;
  float xint = 0;
  float width = nev->GetBinWidth(1);
  TH1F* nevint = nev->Clone("nevint");
  for(i=1; i<=nev->GetNbinsX(); i++) {
    sum+=nev->GetBinContent(i);
    nevint->SetBinContent(i,sum);
    if(1-sum<=cent[num]) {
      y2 = nev->GetBinContent(i-1);
      xint = (1-cent[num]-y2)*width/(sum-y2);  // interpolate 
      cout << "Crossed " << cent[num] << " at " << nev->GetBinCenter(i);
      cout << "\t\tInterpolated value = " << nev->GetBinLowEdge(i-1)+xint << endl;
      num++;
    }
  }
  
  // Finds bins using several methods, eventually I choose just one....

  //cout << endl << "*** Using hvar" << endl;   
  //for(i=1; i<=hvar->GetNbinsX(); i++)  hvar->SetBinContent(i, hvar->GetBinContent(i)*4*pow(hvar->GetBinLowEdge(i),3) );
  hvar->Scale( 1.0/hvar->Integral("width") );
  //c1->cd(2);  hvar->Draw();
  sum=0;
  num=0;
  TH1F* hvarint = hvar->Clone("hvarint");
  for(i=1; i<=hvar->GetNbinsX(); i++) {
    sum+=hvar->GetBinContent(i) * hvar->GetBinWidth(i);
    hvarint->SetBinContent(i,sum);
    if(1-sum<=cent[num]) {
      //cout << cent[num] << " at " << pow(hvar->GetBinCenter(i),4) << endl;
      num++;
    }
  }
  
  cout << endl << "*** Using graph" << endl;
  float gsum = 0;
  for(i=0; i<1199-1; i++) gsum+= gy[i] * (gx2[i+1]-gx2[i]);
  sum=0;
  num=0;
  float gyint[1199];
  float xint; //interpolated value
  for(i=0; i<1199-1; i++) {
    sum += (gy[i]/gsum) * (gx2[i+1]-gx2[i]);
    gyint[i]=sum;
    if(1-sum<=cent[num]) {
      if (i>0) xint = (1-cent[num]-gyint[i-1])*(gx1[i]-gx1[i-1])/(gyint[i]-gyint[i-1]);
      else xint = 0;
      int foo = (int)(xint + 0.5);
      cout << "Crossed " << cent[num] << " at " << gx1[i]; 
      cout << "\t\tInterpolated value = " << gx1[i-1]+xint << endl;
      num++;
    }
  }
  // I skipped the endpoints above to avoid i+1 boundary problems, need to correct for this
  gyint[1198]=1.0;

  tgint = new TGraph(1199, gx2, gyint);
  tgint2 = new TGraph(1199, gx1, gyint);

  /*
    // single window output
  c1 = new TCanvas(infile,infile, 700,700);
  c1->Divide(2,2);
  c1->GetPad(1)->SetLogy();
  c1->cd(1);  nev->Draw();
  c1->cd(2);  hvar->Draw();
  c1->cd(3);  tg->Draw("ALP");
  c1->cd(4);  tgint->Draw("ALP");
  */

  // Multiple-window output
  c1 = new TCanvas("c1","STAR Standard:  dNevent/dnch",800,450);
  c1->Divide(2,1);
  c1->GetPad(1)->SetLogy();
  c1->cd(1); nev->Draw();
  nevint->SetStats(0);
  c1->cd(2); nevint->Draw();

  c2 = new TCanvas("c2","dNevent/dnch^{1/4}", 900,350);
  c2->Divide(3,1);
  tg->SetMarkerStyle(7);
  c2->cd(1); tg->Draw("ALP");
  c2->cd(2); tgint->Draw("ALP");
  c2->cd(3); tgint2->Draw("ALP");

  c3 = new TCanvas("c3","TEST: comparison with hvar",800,450);
  c3->Divide(2,1);
  c3->cd(1);  hvar->Draw();
  hvarint->SetStats(0);
  c3->cd(2);  hvarint->Draw();
		   
  //c4 = new TCanvas();

}
