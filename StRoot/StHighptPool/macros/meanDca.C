#include "common/Name.cc"
#include "commonmacro/histutil.h"
#include "commonmacro/common.h"
void meanDca(const char* inName=
	     "links/P01hi.minbias.2000.hist/hianalysis_1000.hist.root",
	     const char* psDir="ps",
	     int cut = 1,
	     const char* outDir="./",
	     const char* more = "",
	     float extraValue = 1)
		
{
  
  cout << "--------------------------" << endl;
  cout << "in name=" << inName << endl
       << "ps dir=" << psDir << endl
       << "cut=" << cut << endl;
  cout << "--------------------------" << endl;
 
  inRoot = new TFile(inName);

  if(!inRoot){
    cout << "cannot find the infile" << endl;
    return;
  }
  TCanvas c1("c1","c1",400,525);
  TString sName,sTitle;
  const int nPtType=1;
  const int nChargeType=4;
  const int nZperHalf=4;
  const int nType=2;
  
  bool doPos=1;

  TH1 *ha,*hb,*hc;
  char* type[] = { "Mean","Rms","Num"};
  char* ptType[] = { "PrPt","GlPt" };
  char* chargeType[] = {"Neg","Pos","PosPlusNeg","PosMinusNeg"};
  gStyle->SetOptStat(0);
  //-----------------------------------------------------------------
  // mean dca vs phi, z bins
  for(int it=0; it<nType; it++){
    for(int ipt=0;ipt<nPtType;ipt++){ // pt type
      for(int iew=0;iew<2;iew++){     // east west
	int zBin=0;
	TH1* h1[nChargeType][10];
	
	for(int ic=0; ic<nChargeType; ic++){    // get the histos
	  if(iew<1){ zBin=1;  } else { zBin=5; }
	  for(int iz=0; iz<nZperHalf; iz++){
	    sprintf(name,"h%s%sDcaXY%sVphiS0zBin%dSdcaXYBin1",
		    type[it],chargeType[ic],ptType[ipt],zBin++);
	    //cout << name << endl;
	    h1[ic][iz]=(TH1D*)inRoot.Get(name); if(!h1[ic][iz]) return;
	  }
	}
	// plus and minus
	sprintf(name,"%sPosNegDcaXY%sVphi%s",
		type[it],ptType[ipt],sEW[iew].Data());
	Divide(&c1,2,2,name,inName);
	//c1.Divide(2,2);
	zBin=0;
	for(int i=0;i<4;i++){
	  if(iew<1){ zBin = 3-i;} else { zBin = i; }
	  c1.cd(i+1);
	  
	  ha=h1[0][zBin]; hb=h1[1][zBin]; hc=h1[2][zBin];
	  setMinMax(ha,hb,hc,type[it]);
	  TString s=ha->GetTitle();
	  //cout << ha->GetName() << endl;
	  s.Replace(0,s.First("("),""); 
	  s.Replace(s.First(")")+1,s.Length(),"");
	  ha->SetTitle(s.Data()); 
	  ha->SetMarkerColor(kBlue); ha->SetMarkerStyle(4);
	  
	  ha->Draw("p"); DrawLine(ha); 
	  if(doPos){
	    hb->SetMarkerColor(kRed);
	    hc->SetMarkerColor(kGreen);
	    hb->SetMarkerStyle(8); 
	    hc->SetMarkerStyle(10);
	    hb->Draw("psame");
	    hc->Draw("psame");
	  }
	  
	}
	Print(&c1,psDir,name);
	
	// dca*charge
	if(doPos){
	  sprintf(name,"%sPosMinusNegDcaXY%sVphi%s",
		  type[it],ptType[ipt],sEW[iew].Data());
	  Divide(&c1,2,2,name,inName);
	  for(int i=0;i<4;i++){
	    if(iew<1){ zBin = 3-i;} else { zBin = i; }
	    c1.cd(i+1); 
	    setMinMax(h1[3][zBin],0,0,type[it]);
	    h1[3][zBin]->Draw("p");
	  }
	  Print(&c1,psDir,name);
	}
      }//iew
    } //ipt type
  } // type
  
  //---------------------------------------------
  // dca vs z

  for(int it=0; it<nType; it++){
    for(int ipt=0;ipt<nPtType;ipt++){ // pt type
      TH1* h[nChargeType];
      
      for(int ic=0; ic<nChargeType; ic++){    // get the histos
	sprintf(name,"h%s%sDcaXY%sVzS0phiBin1SdcaXYBin1",
		type[it],chargeType[ic],ptType[ipt],zBin++);
	//cout << name << endl;
	h[ic]=(TH1*)inRoot.Get(name); if(!h[ic]) return;
      }
      
      // plus and minus
      sprintf(name,"%sPosNegDcaXY%sVz",
	      type[it],ptType[ipt]);
      Divide(&c1,1,2,name,inName);
      c1.cd(1);  
      ha=h[0]; hb=h[1]; hc=h[2];
      setMinMax(ha,hb,hc,type[it]);
      ha->SetMarkerColor(kBlue);
      ha->Draw("p"); ha->SetMarkerStyle(4);DrawLine(ha); 
      if(doPos){
	hb->SetMarkerColor(kRed);
	hc->SetMarkerColor(kGreen);
	hb->SetMarkerStyle(8); 
	hc->SetMarkerStyle(10);
	hb->Draw("psame"); hc->Draw("psame");
      }
      // dca*charge
      if(doPos){
	c1.cd(2);
	setMinMax(h[3],0,0,type[it]);
	h[3]->Draw("p");
      }
      Print(&c1,psDir,name);
      
    }
  } 
 


}

void setMinMax(TH1* ha,TH1* hb,TH1* hc,TString t)
{
  if(t=="Mean"){
    SetMinMax(ha,-.1,.1);
  }
  else if(t=="Num"){
    if(hc)SetMinMax(ha,ha->GetMinimum()*0.5,hc->GetMaximum()*1.2);
    else SetMinMax(ha);
  }
  else if(t=="Rms"){
    SetMinMax(ha,0.2,0.8);
  }
  else{ cout << "Unknown type " << t << endl; }
}
    
