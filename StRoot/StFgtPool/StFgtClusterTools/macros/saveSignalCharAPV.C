
void saveSignalCharAPV(Char_t* signalFile="signalShapes.root")
{
  TFile f(signalFile);
  Char_t buffer[100];
  Char_t quadName[10];
  Char_t layerName[10];

  /*CANVAS LIST
    c0-c1: numFSig
    c2-c4: maxTb
    c5-c6: maxAdc
    c7-c9: firstHigh
  */

  cout << "So many APV plots... please be patient." << endl;

  sprintf(buffer,"Canvas_mean&max_maxTb_APVbin");
  TCanvas c2(buffer,buffer,10,10,1600,800);
  c2.Divide(2,2);

  sprintf(buffer,"Canvas_mean&max_firsthigh_APVbin");
  TCanvas c7(buffer,buffer,10,10,1600,800);
  c7.Divide(2,2);

  const Int_t n = 240;
  Double_t meanmaxTblayerP[n] = {0};
  Double_t meanmaxTblayerR[n] = {0};
  Double_t meanmaxTblayerRerr[n] = {0};
  Double_t meanmaxTblayerPerr[n] = {0};
  Double_t maxmaxTblayerP[n] = {0};
  Double_t maxmaxTblayerR[n] = {0};
  Double_t meanmaxfhlayerP[n] = {0};
  Double_t meanmaxfhlayerR[n] = {0};
  Double_t meanmaxfhlayerRerr[n] = {0};
  Double_t meanmaxfhlayerPerr[n] = {0};
  Double_t maxmaxfhlayerP[n] = {0};
  Double_t maxmaxfhlayerR[n] = {0};
  Double_t nbinAPV[n] = {0};
  Double_t nbinAPVerr[n] = {0};

  for(Int_t ibin=0;ibin<n;ibin++)
    {
      nbinAPV[ibin]=(Double_t*)ibin;
    }

  for(int iD=1;iD<7;iD++)
    {
      for(int iL=0;iL<2;iL++)
	{
	  if(iL==0)
	    sprintf(layerName,"P");
	  else
	    sprintf(layerName,"R");

	  sprintf(buffer,"Canvas0_Disc_%id_QuadAandB_Layer%s",iD,layerName);
          TCanvas c0(buffer,buffer,10,10,3000,2000);
          c0.Divide(5,4);

	  sprintf(buffer,"Canvas3_Disc_%id_QuadAandB_Layer%s",iD,layerName);
	  TCanvas c3(buffer,buffer,10,10,3000,2000);
	  c3.Divide(5,4);

	  sprintf(buffer,"Canvas5_Disc_%id_QuadAandB_Layer%s",iD,layerName);
          TCanvas c5(buffer,buffer,10,10,3000,2000);
          c5.Divide(5,4);

	  sprintf(buffer,"Canvas8_Disc_%id_QuadAandB_Layer%s",iD,layerName);
          TCanvas c8(buffer,buffer,10,10,3000,2000);
          c8.Divide(5,4);

	  for(int binAPVi=0;binAPVi<20;binAPVi++)
	    {
	      int truAPV = -1;
	      int iQ = -1;
	      if((binAPVi>= 0) && (binAPVi<= 9)){
		iQ=0;
		sprintf(quadName,"A");}
	      if((binAPVi>=10) && (binAPVi<=19)){
		iQ=1;
		sprintf(quadName,"B");}
	      if((iD-1==0) || ((iD-1)%2==1)){
		if((binAPVi>= 0) && (binAPVi<= 4)) truAPV = binAPVi;
		if((binAPVi>= 5) && (binAPVi<= 9)) truAPV = binAPVi+12;
		if((binAPVi>=10) && (binAPVi<=14)) truAPV = binAPVi+2;
		if((binAPVi>=15) && (binAPVi<=19)) truAPV = binAPVi-10;
	      }
	      else{
		if((binAPVi>= 0) && (binAPVi<= 4)) truAPV = binAPVi+12;
		if((binAPVi>= 5) && (binAPVi<= 9)) truAPV = binAPVi;
		if((binAPVi>=10) && (binAPVi<=14)) truAPV = binAPVi-10;
		if((binAPVi>=15) && (binAPVi<=19)) truAPV = binAPVi+2;
	      }

	      //this is a brute-force way to map the truAPV to the rdo, arm, grp number, just for more labels
	      //it is ugly looking and not optimal but it works. (until the mapping changes..)
              int rdo = -1;
              int arm = -1;
              int grp = -1;
              if(truAPV>= 0 && truAPV<= 9) grp = 0;
              if(truAPV>=12 && truAPV<=21) grp = 1;
              if(iD==1){
                arm = 0;
                if(iQ==0) rdo = 1;
                if(iQ==1){
                  if(truAPV > 10) rdo = 1;
                  if(truAPV < 10) rdo = 2;
                }
                if(iQ==3){
                  if(truAPV > 10) rdo = 2;
                  if(truAPV < 10) rdo = 1;
                }
                if(iQ==2) rdo = 2;
              }
              if(iD==2){
                arm = 1;
                if(truAPV>=5 && truAPV<=9) rdo = 2;
                else rdo = 1;
              }
              if(iD==3){
                if(truAPV>=12 && truAPV<=16) {
                  rdo = 2;
                  arm = 1;
                }
                else {
                  rdo = 1;
                  arm = 2;
                }
              }
              if(iD==4){
                if(truAPV>=5 && truAPV<=9){
                  rdo = 1;
                  arm = 3;
		}
                else{
                  rdo = 2;
                  arm = 2;
		}
              }
              if(iD==5){
                arm = 3;
                if(truAPV>=12 && truAPV<=16) rdo = 1;
                else rdo = 2;
              }
              if(iD==6){
                arm = 4;
                if(truAPV>=5 && truAPV<=9) rdo = 2;
                else rdo = 1;
              }

              c0.cd(binAPVi+1);
              sprintf(buffer,"APVnumFSigCloseCluster%s_disc%d_quad%d_apvBIN%d",layerName,iD,iQ,binAPVi);
              TH1D* h=(TH1D*)f.Get(buffer);
              sprintf(buffer,"APVnumFSigCloseCluster%sd%dq%s_rdo%darm%dgrp%dapv%d",layerName,iD,quadName,rdo,arm,grp,truAPV);
              if(h!=0)
                {
                  h->SetTitle(buffer);
                  h->SetName(buffer);
                  h->SetFillColor(1);
                  h->Draw();
                }

	      c3.cd(binAPVi+1);
	      sprintf(buffer,"APVmaxTbCloseCluster%s_disc%d_quad%d_apvBIN%d",layerName,iD,iQ,binAPVi);
	      //cout <<"loading " << buffer <<endl;
	      //	     TCanvas c2;
	      TH1D* h=(TH1D*)f.Get(buffer);
	      sprintf(buffer,"APVmaxTbCloseCluster%sd%dq%s_rdo%darm%dgrp%dapv%d",layerName,iD,quadName,rdo,arm,grp,truAPV);
	      if(h!=0)
		{
		  Double_t meanval = h->GetMean(1);
		  Double_t meanvalerr = h->GetRMS(1);
		  Double_t maxbinnum = h->GetMaximumBin();
		  //cout << "iD, quad, apv, maxbinnum: " << iD << ", " << quadName << ", " << truAPV << ", " << maxbinnum << endl;
		  if(iL==0){
		    meanmaxTblayerP[(iD-1)*40+binAPVi] = meanval;
		    meanmaxTblayerPerr[(iD-1)*40+binAPVi] = meanvalerr; 
		    maxmaxTblayerP[(iD-1)*40+binAPVi] = maxbinnum-1;
		  }
		  if(iL==1){
		    meanmaxTblayerR[(iD-1)*40+binAPVi] = meanval;
		    meanmaxTblayerRerr[(iD-1)*40+binAPVi] = meanvalerr; 
		    maxmaxTblayerR[(iD-1)*40+binAPVi] = maxbinnum-1;
		  }
		  h->SetTitle(buffer);
		  h->SetName(buffer);
		  h->SetFillColor(1);
		  h->Draw();
		}
	      c5.cd(binAPVi+1);
              sprintf(buffer,"APVmaxAdcCloseCluster%s_disc%d_quad%d_apvBIN%d",layerName,iD,iQ,binAPVi);
              TH1D* h=(TH1D*)f.Get(buffer);
              sprintf(buffer,"APVmaxAdcCloseCluster%sd%dq%s_rdo%darm%dgrp%dapv%d",layerName,iD,quadName,rdo,arm,grp,truAPV);
              if(h!=0)
                {
                  h->SetTitle(buffer);
                  h->SetName(buffer);
                  h->SetFillColor(1);
                  h->Draw();
                }

	      c8.cd(binAPVi+1);
              sprintf(buffer,"APVnumFirstHighCloseCluster%s_disc%d_quad%d_apvBIN%d",layerName,iD,iQ,binAPVi);
              TH1D* h=(TH1D*)f.Get(buffer);
              sprintf(buffer,"maxFirstHighCloseCluster%sd%dq%s_rdo%darm%dgrp%dapv%d",layerName,iD,quadName,rdo,arm,grp,truAPV);
              if(h!=0)
                {
                  Double_t meanval = h->GetMean(1);
                  Double_t meanvalerr = h->GetRMS(1);
		  Double_t maxbinnum = h->GetMaximumBin();
                  if(iL==0){
                    meanmaxfhlayerP[(iD-1)*40+binAPVi] = meanval;
                    meanmaxfhlayerPerr[(iD-1)*40+binAPVi] = meanvalerr;
		    maxmaxfhlayerP[(iD-1)*40+binAPVi] = maxbinnum-1;
                  }
                  if(iL==1){
                    meanmaxfhlayerR[(iD-1)*40+binAPVi] = meanval;
                    meanmaxfhlayerRerr[(iD-1)*40+binAPVi] = meanvalerr;
		    maxmaxfhlayerR[(iD-1)*40+binAPVi] = maxbinnum-1;
                  }
                  h->SetTitle(buffer);
                  h->SetName(buffer);
                  h->SetFillColor(1);
                  h->Draw();
                }

	      
	    }
          sprintf(buffer,"APVnumFSigCloseInfoForDisk%d_QuadAandB_layer%s.png",iD,layerName);
          c0.SaveAs(buffer);

	  sprintf(buffer,"APVmaxTbCloseInfoForDisk%d_QuadAandB_layer%s.png",iD,layerName);
	  c3.SaveAs(buffer);
	  
	  sprintf(buffer,"APVmaxAdcCloseInfoForDisk%d_QuadAandB_layer%s.png",iD,layerName);
          c5.SaveAs(buffer);

	  sprintf(buffer,"APVmaxFirstHighCloseInfoForDisk%d_QuadAandB_layer%s.png",iD,layerName);
          c8.SaveAs(buffer);

	  if(iD>1) continue;	      

	  sprintf(buffer,"Canvas1_Disc_%id_QuadCandD_Layer%s",iD,layerName);
          TCanvas c1(buffer,buffer,10,10,3000,2000);
          c1.Divide(5,4);
	  
	  sprintf(buffer,"Canvas4_Disc_%id_QuadCandD_Layer%s",iD,layerName);
	  TCanvas c4(buffer,buffer,10,10,3000,2000);
	  c4.Divide(5,4);

	  sprintf(buffer,"Canvas6_Disc_%id_QuadCandD_Layer%s",iD,layerName);
          TCanvas c6(buffer,buffer,10,10,3000,2000);
          c6.Divide(5,4);

	  sprintf(buffer,"Canvas9_Disc_%id_QuadCandD_Layer%s",iD,layerName);
          TCanvas c9(buffer,buffer,10,10,3000,2000);
          c9.Divide(5,4);


	  for(int binAPVi=20;binAPVi<40;binAPVi++)
	    {
	      int truAPV = -1;
	      int iQ = -1;
	      if((binAPVi>=20) && (binAPVi<=29)){
		iQ=2;
		sprintf(quadName,"C");}
	      if((binAPVi>=30) && (binAPVi<=39)){
		iQ=3;
		sprintf(quadName,"D");}
	      if((iD>1) && (iQ>1)) continue;
	      if((iD-1==0) || ((iD-1)%2==1)){
		if((binAPVi>=20) && (binAPVi<=24)) truAPV = binAPVi-20;
		if((binAPVi>=25) && (binAPVi<=29)) truAPV = binAPVi-8;
		if((binAPVi>=30) && (binAPVi<=34)) truAPV = binAPVi-18;
		if((binAPVi>=35) && (binAPVi<=39)) truAPV = binAPVi-30;
	      }
	      else{
		if((binAPVi>=20) && (binAPVi<=24)) truAPV = binAPVi-8;
		if((binAPVi>=25) && (binAPVi<=29)) truAPV = binAPVi-20;
		if((binAPVi>=30) && (binAPVi<=34)) truAPV = binAPVi-26;
		if((binAPVi>=35) && (binAPVi<=39)) truAPV = binAPVi-18;
	      }
              int rdo = -1;
              int arm = -1;
              int grp = -1;
              if(truAPV>= 0 && truAPV<= 9) grp = 0;
              if(truAPV>=12 && truAPV<=21) grp = 1;
              if(iD==1){
                arm = 0;
                if(iQ==0) rdo = 1;
                if(iQ==1){
                  if(truAPV > 10) rdo = 1;
                  if(truAPV < 10) rdo = 2;
                }
                if(iQ==3){
                  if(truAPV > 10) rdo = 2;
                  if(truAPV < 10) rdo = 1;
                }
                if(iQ==2) rdo = 2;
              }
              if(iD==2){
                arm = 1;
                if(truAPV>=5 && truAPV<=9) rdo = 2;
                else rdo = 1;
              }
              if(iD==3){
                if(truAPV>=12 && truAPV<=16) {
                  rdo = 2;
                  arm = 1;
                }
                else {
                  rdo = 1;
                  arm = 2;
                }
              }
              if(iD==4){
                if(truAPV>=5 && truAPV<=9){
                  rdo = 1;
                  arm = 3;
                }
                else{
                  rdo = 2;
                  arm = 2;
                }
              }
              if(iD==5){
                arm = 3;
                if(truAPV>=12 && truAPV<=16) rdo = 1;
                else rdo = 2;
              }
              if(iD==6){
                arm = 4;
                if(truAPV>=5 && truAPV<=9) rdo = 2;
                else rdo = 1;
              }

              c1.cd(binAPVi-19);
              sprintf(buffer,"APVnumFSigCloseCluster%s_disc%d_quad%d_apvBIN%d",layerName,iD,iQ,binAPVi);

              TH1D* h=(TH1D*)f.Get(buffer);
	      sprintf(buffer,"APVnumFSigCloseCluster%s_d%dq%s_rdo%darm%dgrp%dapv%d",layerName,iD,quadName,rdo,arm,grp,truAPV);
              if(h!=0)
                {
                  h->SetTitle(buffer);
                  h->SetName(buffer);
                  h->SetFillColor(1);
                  h->Draw();
                }

	      c4.cd(binAPVi-19);
	      sprintf(buffer,"APVmaxTbCloseCluster%s_disc%d_quad%d_apvBIN%d",layerName,iD,iQ,binAPVi);
	      
	      TH1D* h=(TH1D*)f.Get(buffer);
	      sprintf(buffer,"APVmaxTbCloseCluster%sd%dq%s_rdo%darm%dgrp%dapv%d",layerName,iD,quadName,rdo,arm,grp,truAPV);
	      if(h!=0)
		{
                  Double_t meanval = h->GetMean(1);
		  Double_t meanvalerr = h->GetRMS(1);
                  if(iL==0){
		    meanmaxTblayerP[(iD-1)*40+binAPVi] = meanval;
		    meanmaxTblayerPerr[(iD-1)*40+binAPVi] = meanvalerr;
		  }
                  if(iL==1){
		    meanmaxTblayerR[(iD-1)*40+binAPVi] = meanval;
		    meanmaxTblayerRerr[(iD-1)*40+binAPVi] = meanvalerr;
		  }
		  h->SetTitle(buffer);
		  h->SetName(buffer);
		  h->SetFillColor(1);
		  h->Draw();
		}

	      c6.cd(binAPVi-19);
              sprintf(buffer,"APVmaxAdcCloseCluster%s_disc%d_quad%d_apvBIN%d",layerName,iD,iQ,binAPVi);

              TH1D* h=(TH1D*)f.Get(buffer);
              sprintf(buffer,"APVmaxAdcCloseCluster%sd%dq%s_rdo%darm%dgrp%dapv%d",layerName,iD,quadName,rdo,arm,grp,truAPV);
              if(h!=0)
                {
                  h->SetTitle(buffer);
                  h->SetName(buffer);
                  h->SetFillColor(1);
                  h->Draw();
                }

	      c9.cd(binAPVi-19);
              sprintf(buffer,"APVnumFirstHighCloseCluster%s_disc%d_quad%d_apvBIN%d",layerName,iD,iQ,binAPVi);

              TH1D* h=(TH1D*)f.Get(buffer);
              sprintf(buffer,"maxFirstHighCloseCluster%sd%dq%s_rdo%darm%dgrp%dapv%d",layerName,iD,quadName,rdo,arm,grp,truAPV);
              if(h!=0)
                {
                  Double_t meanval = h->GetMean(1);
                  Double_t meanvalerr = h->GetRMS(1);
                  if(iL==0){
                    meanmaxfhlayerP[(iD-1)*40+binAPVi] = meanval;
                    meanmaxfhlayerPerr[(iD-1)*40+binAPVi] = meanvalerr;
                  }
                  if(iL==1){
                    meanmaxfhlayerR[(iD-1)*40+binAPVi] = meanval;
                    meanmaxfhlayerRerr[(iD-1)*40+binAPVi] = meanvalerr;
                  }
                  h->SetTitle(buffer);
                  h->SetName(buffer);
                  h->SetFillColor(1);
                  h->Draw();
                }


	    }

	  sprintf(buffer,"APVnumFSigCloseInfoForDisk%d_QuadCandD_layer%s.png",iD,layerName);
          c1.SaveAs(buffer);

	  sprintf(buffer,"APVmaxTbCloseInfoForDisk%d_QuadCandD_layer%s.png",iD,layerName);
	  c4.SaveAs(buffer);

	  sprintf(buffer,"APVmaxAdcCloseInfoForDisk%d_QuadCandD_layer%s.png",iD,layerName);
          c6.SaveAs(buffer);

	  sprintf(buffer,"APVmaxFirstHighCloseInfoForDisk%d_QuadCandD_layer%s.png",iD,layerName);
          c9.SaveAs(buffer);

	}
    }
  c2.cd(1);
  TGraphErrors *gr1 = new TGraphErrors(n,nbinAPV,meanmaxTblayerP,nbinAPVerr,meanmaxTblayerPerr);
  gr1->SetTitle("Layer P mean maxTb per APV");
  gr1->SetMinimum(0);
  gr1->SetMaximum(7);
  gr1->SetMarkerColor(1);
  gr1->SetMarkerSize(.4);
  gr1->SetMarkerStyle(21);
  gr1->Draw("AP");
  c2.cd(2);
  TGraphErrors *gr2 = new TGraphErrors(n,nbinAPV,meanmaxTblayerR,nbinAPVerr,meanmaxTblayerRerr);
  gr2->SetTitle("Layer R mean maxTb per APV");
  gr2->SetMinimum(0);
  gr2->SetMaximum(7);
  gr2->SetMarkerColor(1);
  gr2->SetMarkerSize(.4);
  gr2->SetMarkerStyle(21);
  gr2->Draw("AP");
  c2.cd(3);
  TGraph *gr3 = new TGraph(n,nbinAPV,maxmaxTblayerP);
  gr3->SetTitle("Layer P max maxTb per APV");
  gr3->SetMinimum(0);
  gr3->SetMaximum(7);
  gr3->SetMarkerColor(1);
  gr3->SetMarkerSize(.4);
  gr3->SetMarkerStyle(21);
  gr3->Draw("AP");
  c2.cd(4);
  TGraph *gr4 = new TGraph(n,nbinAPV,maxmaxTblayerR);
  gr4->SetTitle("Layer R max maxTb per APV");
  gr4->SetMinimum(0);
  gr4->SetMaximum(7);
  gr4->SetMarkerColor(1);
  gr4->SetMarkerSize(.4);
  gr4->SetMarkerStyle(21);
  gr4->Draw("AP");
  c2.Update();
  c2.SaveAs("APVmeanmaxTb.png");

  c7.cd(1);
  TGraphErrors *gr5 = new TGraphErrors(n,nbinAPV,meanmaxfhlayerP,nbinAPVerr,meanmaxfhlayerPerr);
  gr5->SetTitle("Layer P mean maxFirstHigh per APV");
  gr5->SetMinimum(0);
  gr5->SetMaximum(7);
  gr5->SetMarkerColor(1);
  gr5->SetMarkerSize(.4);
  gr5->SetMarkerStyle(21);
  gr5->Draw("AP");
  c7.cd(2);
  TGraphErrors *gr6 = new TGraphErrors(n,nbinAPV,meanmaxfhlayerR,nbinAPVerr,meanmaxfhlayerRerr);
  gr6->SetTitle("Layer R mean maxFirstHigh per APV");
  gr6->SetMinimum(0);
  gr6->SetMaximum(7);
  gr6->SetMarkerColor(1);
  gr6->SetMarkerSize(.4);
  gr6->SetMarkerStyle(21);
  gr6->Draw("AP");
  c7.cd(3);
  TGraph *gr7 = new TGraph(n,nbinAPV,maxmaxfhlayerP);
  gr7->SetTitle("Layer P max maxFirstHigh per APV");
  gr7->SetMinimum(0);
  gr7->SetMaximum(7);
  gr7->SetMarkerColor(1);
  gr7->SetMarkerSize(.4);
  gr7->SetMarkerStyle(21);
  gr7->Draw("AP");
  c7.cd(4);
  TGraph *gr8 = new TGraph(n,nbinAPV,maxmaxfhlayerR);
  gr8->SetTitle("Layer R max maxFirstHigh per APV");
  gr8->SetMinimum(0);
  gr8->SetMaximum(7);
  gr8->SetMarkerColor(1);
  gr8->SetMarkerSize(.4);
  gr8->SetMarkerStyle(21);
  gr8->Draw("AP");
  c7.Update();
  c7.SaveAs("APVmeanmaxFirstHigh.png");

}     
