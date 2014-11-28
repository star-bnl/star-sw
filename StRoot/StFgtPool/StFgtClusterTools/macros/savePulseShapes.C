
void savePulseShapes(Char_t* pulseFile="pulses.root")
{


TFile f(pulseFile);
 Char_t buffer[100];
 Char_t quadName[10];
 // TCanvas c;
 for(int iD=1;iD<7;iD++)
   {
     for(int iQ=0;iQ<4;iQ++)
       {
	 if(iD>1 && iQ > 1)
	   continue;
	 if(iQ==0)
	   sprintf(quadName,"A");
	 if(iQ==1)
	   sprintf(quadName,"B");
	 if(iQ==2)
	   sprintf(quadName,"C");
	 if(iQ==3)
	   sprintf(quadName,"D");
	 sprintf(buffer,"pulseShapes_disc%d_quad%s",iD,quadName);
	 TCanvas c(buffer,buffer,2000,2000);
	 c.Divide(2,2);
	 c.cd(1);
	 sprintf(buffer,"validChargesR_D%d_Q%d",iD,iQ);
	 	 cout <<"loading " << buffer <<endl;
	 TH1D* hVC=(TH1D*)f.Get(buffer);
	 sprintf(buffer,"validChargesR_D%d_Q%s",iD,quadName);
	 hVC->SetName(buffer);
	 hVC->Draw();
	 //	 sprintf(buffer,"validChargesR_D%d_Q%s.png",iD,quadName);
	 //	 c.SaveAs(buffer);
	 c.cd(2);
	 sprintf(buffer,"validChargesP_D%d_Q%d",iD,iQ);
	 cout <<"loading " << buffer <<endl;
	 TH1D* hVC=(TH1D*)f.Get(buffer);
	 sprintf(buffer,"validChargesP_D%d_Q%s",iD,quadName);
	 hVC->SetName(buffer);
	 hVC->Draw();
	 sprintf(buffer,"validChargesP_D%d_Q%s.png",iD,quadName);
	 //	 c.SaveAs(buffer);

	 c.cd(3);
	 sprintf(buffer,"validPulsesR_D%d_Q%d",iD,iQ);
	 cout <<"loading " << buffer <<endl;
	 TH1D* hVP=(TH1D*)f.Get(buffer);
	 sprintf(buffer,"validPulsesR_D%d_Q%s",iD,quadName);
	 hVP->SetName(buffer);
	 hVP->Draw();
	 sprintf(buffer,"validPulsesR_D%d_Q%s.png",iD,quadName);
	 //	 c.SaveAs(buffer);
	 c.cd(4);
	 sprintf(buffer,"validPulsesP_D%d_Q%d",iD,iQ);
	 cout <<"loading " << buffer <<endl;
	 TH1D* hVP=(TH1D*)f.Get(buffer);
	 sprintf(buffer,"validPulsesP_D%d_Q%s",iD,quadName);
	 hVP->SetName(buffer);
	 hVP->Draw();
	 sprintf(buffer,"validChargeAndPulses_D%d_Q%s.png",iD,quadName);
	 c.SaveAs(buffer);
       }
   }

}
