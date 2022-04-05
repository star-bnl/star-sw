double chi2Eval(TH2D* h, TH2D* data, float beta, float sf, int rscale){

  double retVal=0.;
  double posVal=0.;
  double negVal=0.;
  double numNeg=0.;
  double numPos=0.;

  bool writeIt=true;
  if(!rscale){ // SYtDYt space

    for(int ix=13;ix<=h->GetNbinsX();ix++){   // ix=13, sum_yt~3.4, yt~1.7
      for(int iy=13;iy<=h->GetNbinsY();iy++){ // iy=13, delta_yt=0
        if(data->GetBinContent(ix,iy)<1)continue;
        double testVal=(double)h->GetBinContent(ix,iy);
        double testErr=fabs((double)h->GetBinError(ix,iy));
        if(testErr==0.)continue;
        double val=testVal/testErr;
        val*=val;
        if(testVal<0.){
  	  negVal += val;
          numNeg+=1.0;
        } else {
          posVal += val;
          numPos+=1.0;
        }
      }
    }

  } else {  //YtYt Space 
 
   for(int ix=5;ix<=h->GetNbinsX();ix++){   // ix=5, yt~1.7
      for(int iy=ix+1;iy<=h->GetNbinsY();iy++){ // iy>ix
        if(data->GetBinContent(ix,iy)<1)continue;
        double testVal=(double)h->GetBinContent(ix,iy);
        double testErr=fabs((double)h->GetBinError(ix,iy));
        if(testErr==0.)continue;
        double val=testVal/testErr;
        val*=val;
        if(testVal<0.){
  	  negVal += val;
          numNeg+=1.0;
        } else {
          posVal += val;
          numPos+=1.0;
        }
      }
    }
  }

  if(numPos>0.)posVal=(posVal/numPos);
  if(numNeg>0.)negVal=(1.+beta)*(negVal/numNeg);
  
  retVal=posVal+negVal;
  return retVal;
}
     
//--------------------------------------------------------------------
float* runChi2Min_SFEval(const char* dirName, float beta, int itype, const char* cmult=NULL, int rscale=1){

  /* Notes from Jeff's 7/24 email to estruct
 const char* dirName => input directory (excluding mult sub-dir)
 float beta          => lagrange multiplier (typically ~10)
 int itype           => side-index (all, awayside, sameside, ...)
 const char* cmult   => multiplicity subdir
 int rscale          => 0 do not apply, 1 apply  radialScaling.C
  */

  const char* spaceName[]={"NSYtDYt","NYtYt"};

  TString dirname(dirName);
  if(cmult){
    dirname+="/";
    dirname+=cmult;
    dirname+="/";
  }

  gSystem->Load("StEStructPoolSupport.so");
  gROOT->LoadMacro("radialScaling.C");

  double lsmin=999999.;
  double usmin=999999.;

  float lsscale;
  float usscale;

  const char* cname[]={"all","nearside","awayside"};

    TString fname(cname[itype]);

    TString infile(dirname.Data());
    infile+=fname.Data();
    infile+=".root";
 
   cout<<"Attempting fits from file = "<<infile.Data()<<" with beta="<<beta<<endl;
    TFile* tf=new TFile(infile.Data());
    tf->cd();
    StEStructSupport ehelp(tf,1);
 
    TString ppName("Sibpp"); ppName+=spaceName[rscale];
    TString mmName("Sibmm"); mmName+=spaceName[rscale];
    TString mpName("Sibmp"); mpName+=spaceName[rscale];
    TString pmName("Sibpm"); pmName+=spaceName[rscale];
   
    TH2D* ssdata=(TH2D*)(tf->Get(ppName.Data())->Clone());
    TH2D* mmdata=(TH2D*)tf->Get(mmName.Data());
    ssdata->Add(mmdata);

    TH2D* usdata=(TH2D*)tf->Get(mpName.Data());
    TH2D* mpdata=(TH2D*)tf->Get(pmName.Data());
    usdata->Add(mpdata);

    cout<<" Npairs:: LS="<<ssdata->Integral();
    cout<<" US="<<usdata->Integral()<<endl;

  for(int is=0;is<20;is++){ 
    float fnval=0.9+is*0.005;
    float sf[2];//={1.00,1.00};
    sf[0]=fnval;
    sf[1]=fnval;
        
    TH2D** localHists=(TH2D**)ehelp.buildChargeTypes(spaceName[rscale],1,sf);  
    if(1==rscale){
      radialScaling(localHists[0]);
      radialScaling(localHists[1]);
    } 
    double lval=chi2Eval(localHists[0],ssdata,beta,sf[0],rscale);
    double uval=chi2Eval(localHists[1],usdata,beta,sf[1],rscale);

    if(lval<lsmin){
      lsmin=lval;
      lsscale=fnval;
    }
    if(uval<usmin){
      usmin=uval;
      usscale=fnval;
    }

  }

  tf->Close();

  cout<<" found lsmin="<<lsscale<<" usscale="<<usscale<<endl;
  float* retVal = new float[2];
  retVal[0]=lsscale;
  retVal[1]=usscale;

  return retVal;
}

