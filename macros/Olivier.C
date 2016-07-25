{
TH1F* R0      =  new TH1F("R0","R0",10,0.5,10.5);
TH1F* R1      =  new TH1F("R1","R1",10,0.5,10.5);
 
for(int k=1;k<11;k++){
 R0 -> Fill (k,100+10*k);
 R1 -> Fill (k,10*k);
}

TCanvas c;
c.cd();

R0->SetMinimum(0);
R0->SetMaximum(200);

R0->SetFillColor(kGray+1);
R0->Draw("e3"); 


TH1F* CR0=R0->DrawCopy("same hist CL");
CR0->SetMarkerSize(0);
CR0->SetLineColor(kBlack);
CR0->SetLineWidth(2); 
CR0->SetFillColor(0);


R1->SetFillColor(kRed-7);
R1->SetFillStyle(3002);
R1->Draw("same e3"); 

   
TH1F* CR1=R1->DrawCopy("same hist CL");
CR1->SetMarkerSize(0);
CR1->SetLineColor(kRed);
CR1->SetLineWidth(2); 
CR1->SetFillColor(0);
}
