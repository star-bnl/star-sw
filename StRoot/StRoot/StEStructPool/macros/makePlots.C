void verticleRapidity(TH2F* h){

  for(int ix=1;ix<=h->GetNbinsX();ix++){
    for(int iy=1;iy<=h->GetNbinsY();iy++){
      float xval=h->GetBinContent(ix,iy);
      float zval=log(sqrt(1+xval*xval/0.01)+xval/0.1);
      h->SetBinContent(ix,iy,zval);
   }
  }

  return;
}

int findAxisNames(const char* hname, int& prefixid, int* varid, int* combid){

  // A name manipulation routine ... should be done in perl! but oh well

  // 1st strip off and id any prefix 
  char* prenames[]={"LS","US","CD","CI","pp"};
  char* newname=NULL;
  char* preptr;
  for(int i=0;i<5;i++){
    if((preptr=strstr(hname,prenames[i]))) break;
  }
  // all have 2 prefix chars that I strip here
  prefixid=i;
  preptr++; preptr++;
  newname=new char[strlen(preptr)+1];
  strcpy(newname,preptr);


  // now find the type
  char* tnames[]={"Yt","Eta","Phi","Pt","Xt"};
  char* ptr[5];
  char* ptr1[2]={NULL,NULL};
  int icount=0;
  int nameid[2]={-1,-1};

  for(int i=0;i<5;i++){
    ptr[i]=strstr(newname,tnames[i]);
    if(ptr[i]){
      ptr1[icount]=ptr[i];
      nameid[icount]=i;
      icount++;
    }
  }

  if(icount==0) return 1;
   
  if(icount==1){ 
    char* tmpstr=ptr1[0];
    tmpstr++;
    ptr1[1]=strstr(tmpstr,tnames[nameid[0]]);
    if(ptr1[1])nameid[1]=nameid[0];
    icount++;
  }
  if(icount==1)return 1;

  if(ptr1[0]>ptr1[1]){
    char* tmpptr=ptr1[1];
    ptr1[1]=ptr1[0];
    ptr1[0]=tmpptr;
    int tmpid=nameid[1];
    nameid[1]=nameid[0];
    nameid[0]=tmpid;
  }

  for(int i=0;i<2;i++)varid[i]=nameid[i];
  
  // okay we've got it as (e.g.) XEtaYPhi where X,Y is either S, D, or nothing
  // so find them....

  char* n2names[]={NULL,"D","S"};
  int n2id[2]={-1,-1};
  if(strcmp(ptr1[0],newname)==0){  // X=Y=nothing (we don't yet have EtaDPhi)
    n2id[0]=n2id[1]=0;

  } else {
    for(int k=0;k<2;k++){
     char* tmp2=ptr1[k];
     tmp2--;
     for(int i=1;i<3;i++){
       char* tmp3;
        if((tmp3=strstr(tmp2,n2names[i])) && tmp3==tmp2){
	  n2id[k]=i;
          break;
        }
     }
    }
  }
  for(int i=0;i<2;i++) { 
      if(n2id[i]==-1) return 2;
      combid[i]=n2id[i];
  }
}

//------------------------------------------------------------------
bool makePlots4P(TH2F** hists, const char* plttype, int opt=0){

  int prefixid;
  int varid[2];
  int combid[2];
  int itest=findAxisNames(hists[0]->GetName(),prefixid,varid,combid);

  if(itest){
    cout<<"Failed to find valid name from "<<hists[0]->GetName();
    cout<<" errorid="<<itest<<endl;
    return false;
  }

  c1 = new TCanvas();
  c1->Divide(2,2);


  int ititle=0;
  if(prefixid>3)ititle=1;

  TLatex tl;
  tl.SetTextFont(12);
  char* tlNames[]={"y_{t,", "#eta_{", "#phi_{", "p_{t,", "X_{t,"};
  char* tl2Names[]={"#Delta}","#Sigma}"};
  char* xvsy[]={"1}","2}"};

  TString* axislabel[2];
  for(int i=0;i<2;i++){
    axislabel[i]=new TString(tlNames[varid[i]]);
    if(combid[i]<1){
      *axislabel[i]+=xvsy[i];
    } else {
      *axislabel[i]+=tl2Names[combid[i]-1];
    }
  }

  char* atitle[]={"Like-sign (LS)","Unlike-sign (US)"," CD=LS-US"," CI=LS+US"};
  char* btitle[]={"Pos.Pairs (++)","Unlike Sign (US)"," Neg.Pairs (--)","LS diff. = ++ - --"};  

  float xvl[4]={0.01,0.5,0.28,0.8};
  float yvl[4]={0.5,0.02,0.12,0.15};
  int ivl=0;
  for(int i=0;i<4;i++){
    TH2F* h=(TH2F*)hists[i]->Clone();
    cout<<"hist ="<<h->GetName(); 
    cout<<"  max = "<<h->GetMaximum()<<" min = "<<h->GetMinimum()<<endl;

    if(opt>0)verticleRapidity(h);

    tl.SetTextSize(0.05);
    h->SetNdivisions(5,"Z");
    h->SetStats(0);
    c1->cd(i+1);
    if(strstr(plttype,"surf")){ 
      c1->SetPhi(50);
      c1->SetTheta(50);
     ivl=2;
    }
    h->Draw(plttype);
    tl.SetNDC();
    if(0==ititle){
      tl.DrawLatex(0.31,0.91,atitle[i]);
    } else {
      tl.DrawLatex(0.31,0.91,btitle[i]);
    }

    tl.SetTextSize(0.08);

    tl.DrawLatex(xvl[ivl],yvl[ivl],axislabel[1]->Data());
    tl.DrawLatex(xvl[ivl+1],yvl[ivl+1],axislabel[0]->Data());

  }

  return true;
}


//-------------------------------------------------------------------------  
void makePlots(TH2F** hists, int itype=0){

  /********************************************
      itype=0 surface plots
           =1 colz plots
           =2 colz with rapidity verticle scale
  **********************************************/

  if(!hists) {
    cout<<"What?  input NULL ** hists? we'll give this back to you"<<endl;
    return;
  }
  

  bool isok=false;

  if(itype==0){
    const char* plttype="surf1";
    isok=makePlots4P(hists,plttype);
  } else if(itype==1){
     isok=makePlots4P(hists,"colz");
  } else if(itype==2){
     isok=makePlots4P(hists,"colz",1);
  } else {
     cout<<" undefined itype="<<itype<<endl;
  }

  if(!isok)cout<<"plotting failed "<<endl;

};

















