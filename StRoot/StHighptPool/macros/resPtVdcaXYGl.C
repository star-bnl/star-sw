#include "commonmacro/histutil.h"
#include "commonmacro/common.h"

void resPtVdcaXYGl(const char* inName="test.hist.root",
		   const char* psDir="ps",
		   int cut = 111,
		   const char* outDir = "",
		   const char* more="",
		   float extraValue = 0) 

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
  
  TCanvas c1("c1","c1",400,500);

  float ptary[] = {1.5,2,3,4,6};
  int nPt=4;
  int nBase = 2; 
  const char* base[] = 
    {  
      "h3ResPtPrGlPtPrDcaXYGl","h3ResPtPrGlPtGlDcaXYGl",
     
    };
  TH3* h3; TH2* h2; TH1* h1; TProfile* p;
  gStyle->SetOptStat(0);
  //---------------------------------------------
  for(int ib=0; ib<nBase; ib++){
    h3=(TH3*)inRoot.Get(base[ib]);
    cout << base[ib] << endl; if(!h3) return;
    
    //** res pt vs dcaxy
    //
    if(ib<6){
      sprintf(title,"%s vs dcaXYGl %s slices (cut %d)",
	      h3->GetXaxis()->GetTitle(),h3->GetYaxis()->GetTitle(),cut);
      Divide(&c1,2,2,title,inName);
      for(int ipt=0;ipt<nPt;ipt++){
	c1.cd(ipt+1);
	h2=HistSlice(h3,name,"",0,ptary[ipt],ptary[ipt+1],"xz");
	h2->Draw("box");
      }
      sName=base[ib];getSub(sName); 
      sprintf(title,"%sVdcaXY%sSlices",sName.Data(),
	      h3->GetYaxis()->GetTitle());
      Print(&c1,psDir,title);
    }

    //if(ib>3) continue;

    //** mean res pt vs dcaxy
    //
    sprintf(title,"mean %s va dcaXYGl %s slices (cut %d)",
	    h3->GetXaxis()->GetTitle(),h3->GetYaxis()->GetTitle(),cut);
    Divide(&c1,2,2,title,inName);
    for(int ipt=0;ipt<nPt;ipt++){

      c1.cd(ipt+1);
      p=Profile(h3,base[ib],"",0,ptary[ipt],ptary[ipt+1],"xz","x");
      SetMinMax(p,-0.4,0.4); SetRange(p->GetXaxis(),-1,1);
      p->Draw(); gPad->SetGridx(); gPad->SetGridy();
    }
    sName=base[ib];getSub(sName); 
    sprintf(title,"mean%sVdcaXY%sSlices",sName.Data(),
	    h3->GetYaxis()->GetTitle());
    Print(&c1,psDir,title);

    //** rms res pt vs dcaxy
    //
    sprintf(title,"rms %s va dcaXYGl %s slices (cut %d)",
	    h3->GetXaxis()->GetTitle(),h3->GetYaxis()->GetTitle(),cut);
    Divide(&c1,2,2,title,inName);
    for(int ipt=0;ipt<nPt;ipt++){
      c1.cd(ipt+1);
      h1=Rms(h3,base[ib],"",0,ptary[ipt],ptary[ipt+1],"xz","x");
      h1->SetMarkerStyle(8);
      SetMinMax(h1,0,0.5); SetRange(h1->GetXaxis(),-0.8,0.8);
      h1->Draw();
    }
    sName=base[ib];getSub(sName); 
    sprintf(title,"rms%sVdcaXY%sSlices",sName.Data(),
	    h3->GetYaxis()->GetTitle());
    Print(&c1,psDir,title);
    //    return;
  }
}
  //-------------------------
void getSub(TString& ss){
  ss.Replace(ss.First("D")-4,ss.Length(),"");
  ss.ReplaceAll("h3","");
 
}
