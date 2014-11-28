TH2S *h2Ped=0, *h2Sig=0;
TH1S *h1Stat=0, *h2CapStat;
TH2D *hT0=0, *hT1=0, *hT2=0;
TCanvas *c=0;

// this is Tonko's metod of computingpeds(cap,softID) - plain average

void doBprsPedTonko(char *crun="R9049049", char * pathIn="/star/data05/scratch/balewski/sched-raw-ped4"){
  gStyle->SetPalette(1,0);
  gStyle->SetOptStat(0);

  char *pathOut="calib-2.3m/";
  char txt[1000], txt2[1000];
  
  sprintf(txt,"%s/%s.barCal.hist.root",pathIn,crun);
  fd1=new TFile(txt); 
  printf("Open=%s=\n",fd1->GetName());
  if(!fd1->IsOpen()) { printf("FAIL run=%s\n", crun); return;}


  sprintf(txt,"%s/pedBprs%s-allCap.hist.root",pathOut,crun);
  fd2=new TFile(txt,"recreate"); 

   hT0= (TH2D *)fd1->Get("bprsTnk0"); assert(hT0);
   hT1= (TH2D *)fd1->Get("bprsTnk1"); assert(hT1);
   hT2= (TH2D *)fd1->Get("bprsTnk2"); assert(hT2);

   axX=hT0->GetXaxis();
   float x1=axX->GetXmin();
   float x2=axX->GetXmax();
   int nbX=axX->GetNbins();
   //  printf("X-axis range  --> [%.1f, %.1f], nb=%d %s\n",x1,x2,nbX,axX->GetTitle());
   
   axY=hT0->GetYaxis();
   float y1=axY->GetXmin();
   float y2=axY->GetXmax();
   int nbY=axY->GetNbins();
   printf("Y-axis range  --> [%.1f, %.1f], nb=%d %s\n",y1,y2,nbY,axX->GetTitle());
   int nCap=nbY;

   sprintf(txt,"pedBPRScap");
   sprintf(txt2,"BPRS pedestal, average method (Z=10*ADC), %s ; BPRS soft ID; capID",crun);
   h2Ped=new TH2S(txt,txt2,nbX,x1,x2,nCap,-0.5,nCap-0.5);

   sprintf(txt,"rmsPedBPRScap");
   sprintf(txt2,"BPRS RMS(ped) (Z=10*ADC), %s ; BPRS soft ID; capID",crun);
   h2Sig=new TH2S(txt,txt2,nbX,x1,x2,nCap,-0.5,nCap-0.5);
      
   sprintf(txt,"statBPRSallCap");
   sprintf(txt2,"BPRS status from all caps, %s ; BPRS soft ID; # of bad caps",crun);
   h1Stat=new TH1S(txt,txt2,nbX,x1,x2);
   
   sprintf(txt,"capStatBPRSallSoft");
   sprintf(txt2,"BPRS status from all softID, %s ; BPRS capID; # of bad softID",crun);
   h1CapStat=new TH1S(txt,txt2,128,-0.5,127.5);
   
   int cut_minEve=20;
   float cut_minRms=0.8;

   printf("H2 created\n");
   int cap1=0, cap2=127;
   for(int cap=cap1;cap<=cap2;cap++) {
     for(int i=1;i<=nbX;i++) {// i=softID
      
       double nEve= hT0->GetBinContent(i,cap+1);
       double sumX= hT1->GetBinContent(i,cap+1);
       double sumX2= hT2->GetBinContent(i,cap+1);
       if(nEve<=cut_minEve) {	 h1Stat->Fill(i);	 continue;       }
       //       printf("%d %d %d\n",cap,i,nEve);
       double ped=sumX/nEve;
       double rms=sqrt(sumX2/nEve -ped*ped);
       // if(i==3) printf("cap=%d id=%d nEve=%f ped=%f  rms=%f\n",cap,i,nEve, ped,rms);     
       if(rms< cut_minRms) {	 h1Stat->Fill(i);	  continue;       }
       h2Ped->SetBinContent(i,cap+1,(int)(10.*ped));
       h2Sig->SetBinContent(i,cap+1,(int)(10.*rms));
       if(0) {
	h1Stat->Fill(i,1);	
	h1CapStat->Fill(cap,1);	
       }
     } // end of loop over soft ID
  }// end of loop over caps    
   
   // count dead tiles
   int nDead=0;
   for(int i=1;i<=nbX;i++) {// i=softID
     if(h1Stat->GetBinContent(i)==0) continue;
     nDead++;    
     printf("%d dead, softID=%d\n",nDead,i);
   }
   h1Stat->SetEntries(nDead);
   printf(" accumulated nDead=%d\n",nDead);
   
  fd2->cd();
  h2Ped->Write();
  h2Sig->Write();
  h1Stat->Write();
  h1CapStat->Write();
  printf("total entries=%d\n",h2Ped->GetEntries());
  return;
  c=new TCanvas();
  c->Divide(1,3);
  c->cd(1); h2Sig->Draw("colz");  h2Sig->SetMaximum(4);
  c->cd(2);

  subtractRefPed();
  return;


  // dump 3D histo ....
  for(int k=120;k<=128;k++) //capID
    for(int i=1;i<=nbX;i++) //chan
      for(int j=1;j<=nbY;j++) //ADC
	{
	  float val=h3->GetBinContent(i,j,k);    
	  if(val<1.) continue;
	  printf("chan(i)=%d,  adc(j)=%d,  cap(k)=%d val=%f\n", i-1,j+100,k-1,val);
	}
}


//=========================
void subtractRefPed() {
  
  fd3=new TFile("calib2.2n/pedBprsR9066001-allCap.hist.root"); 
  assert(fd3->IsOpen());
  printf("Opened=%s=\n",fd3->GetName());

  hRef1= (TH2F *)fd3->Get("sigPedBPRScap"); assert(hRef1);
  hRef1->Draw("colz");
  hRef1->SetMaximum(4);

  c->cd(3);
  hRef= (TH2F *)fd3->Get("pedBPRScap"); assert(hRef);
  hRef->SetName("ab");
  hRef->Draw("colz");
  //  hRef->Add(h2Ped,1.);
  hRef->Divide(h2Ped);
  hRef->SetMaximum(1.01);
  hRef->SetMinimum(0.98);


}
