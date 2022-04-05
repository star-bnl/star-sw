void radialScaling(TH1* hist){
   
  TAxis *xa=hist->GetXaxis();
  TAxis *ya=hist->GetYaxis();

  for(int ix=1;ix<=xa->GetNbins();ix++){
     double x1 = xa->GetBinCenter(ix);
     //     if(x1<=0.) continue;
     for(int iy=1;iy<=ya->GetNbins();iy++){
       double y1=1.0;

       if(ya->GetNbins()>1 && !strstr(hist->GetName(),"SYtDYt")){ 
          y1=ya->GetBinCenter(iy);
       }
       if(y1<=0.)continue;

       double xv =hist->GetBinContent(ix,iy);
       double exv=hist->GetBinError(ix,iy);
       xv/=sqrt(x1*y1);
       exv/=sqrt(x1*y1);
       hist->SetBinContent(ix,iy,xv);
       hist->SetBinError(ix,iy,exv);

     }

  }

  return;
}
       
