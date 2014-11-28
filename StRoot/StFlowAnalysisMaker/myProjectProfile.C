#include "TH1.h"
#include "TProfile.h"
#include "TProfile2D.h"
#include "iostream.h"

//absoluteBould =1 make, one can use it like 2.6<|eta|<4
//reversedSign =1 make the addition of profile as  ( negProf *-1. + posProf)


TH1D* myProjectionX(TProfile2D* prof2d, double lowBound, double upBound, bool absoluteBound=0, bool reversedSign=0 ){




  TString st(prof2d->GetName());
  st +="_X";


  TH1D* projx = prof2d->ProjectionX(st.Data(), prof2d->GetYaxis()->FindBin(lowBound),prof2d->GetYaxis()->FindBin(upBound));
  projx->Reset();

  TH1D* projy = prof2d->ProjectionY();




  for (int i = 0; i<projx->GetNbinsX(); i++) {

    double contentSum=0.;
    double entriesSum=0.;
    double contentSqrSum=0.;


    for (int j=0; j<projy->GetNbinsX(); j++) {


      float BinHighEdge =   (projy->GetBinLowEdge(j) + projy->GetBinWidth(j));
      float BinLowEdge = projy->GetBinLowEdge(j);

            if (absoluteBound==0){
      if ( BinHighEdge < lowBound) continue;
      if ( BinLowEdge  > upBound) continue;
            } else if (absoluteBound){

       if ( BinHighEdge < 0. && BinLowEdge < 0. && BinHighEdge < -upBound) continue; 
       if ( BinHighEdge < 0. && BinLowEdge < 0. && BinLowEdge > -lowBound) continue; 
       if ( BinHighEdge > 0. && BinLowEdge > 0. && BinHighEdge < lowBound) continue; 
       if ( BinHighEdge > 0. && BinLowEdge > 0. && BinLowEdge >  upBound) continue; 
       if ( BinHighEdge > 0. && BinLowEdge < 0. && (BinHighEdge-BinLowEdge)<lowBound) continue;
            }





      double entries = prof2d->GetBinEntries(prof2d->GetBin(i,j));
      if (entries==0)  continue;
      double content = prof2d->GetBinContent(i,j);
      if (reversedSign && (BinHighEdge+BinLowEdge)/2. < 0.) content *=-1.;
      double error = prof2d->GetBinError(i,j);

      entriesSum += entries;
      contentSum += content * entries;

      if (entries > 1) {
      contentSqrSum += entries * (entries * error * error + content * content);
      } else if (entries ==1) { //special case to calculate the correct error

	contentSqrSum += content*content;
      }
    }

    if (entriesSum) {
      
      projx->SetBinContent(i,contentSum/entriesSum);
      projx->SetBinError(i,(sqrt((contentSqrSum/entriesSum) - pow ( ( contentSum/entriesSum),2.)) / sqrt(entriesSum)));

    }

  }

  if (projy) delete projy;
  return projx;
 
}

TH1D* myProjectionY(TProfile2D* prof2d, double lowBound, double upBound ,bool absoluteBound=0, bool reversedSign=0  ){

  TString st(prof2d->GetName());
  st +="_Y";



  TH1D* projy = prof2d->ProjectionY(st.Data(), prof2d->GetXaxis()->FindBin(lowBound),prof2d->GetXaxis()->FindBin(upBound));
  projy->Reset();

  TH1D* projx = prof2d->ProjectionX();


 for (int j=0; j<projy->GetNbinsX(); j++) {

    double contentSum=0.;
    double entriesSum=0.;
    double contentSqrSum=0.;

     for (int i = 0; i<projx->GetNbinsX(); i++) {

      
      float BinHighEdge =   (projx->GetBinLowEdge(i) + projx->GetBinWidth(i));
      float BinLowEdge = projx->GetBinLowEdge(i);

            if (absoluteBound==0){
      if ( BinHighEdge < lowBound) continue;
      if ( BinLowEdge  > upBound) continue;
            } else if (absoluteBound){

       if ( BinHighEdge < 0. && BinLowEdge < 0. && BinHighEdge < -upBound) continue; 
       if ( BinHighEdge < 0. && BinLowEdge < 0. && BinLowEdge > -lowBound) continue; 
       if ( BinHighEdge > 0. && BinLowEdge > 0. && BinHighEdge < lowBound) continue; 
       if ( BinHighEdge > 0. && BinLowEdge > 0. && BinLowEdge >  upBound) continue; 
       if ( BinHighEdge > 0. && BinLowEdge < 0. && (BinHighEdge-BinLowEdge)<lowBound) continue;
            }




      double entries = prof2d->GetBinEntries(prof2d->GetBin(i,j));
      if (entries==0)  continue;
      double content = prof2d->GetBinContent(i,j);
      if (reversedSign && (BinHighEdge+BinLowEdge)/2. < 0.) content *=-1.; // for directed flow
      double error = prof2d->GetBinError(i,j);

      entriesSum += entries;
      contentSum += content * entries;

      if (entries > 1) {
      contentSqrSum += entries * (entries * error * error + content * content);
      } else if (entries ==1) { //special case to calculate the correct error

	contentSqrSum += content*content;
      }
    }

    if (entriesSum) {
      
      projy->SetBinContent(j,contentSum/entriesSum);
      projy->SetBinError(j,(sqrt((contentSqrSum/entriesSum) - pow ( ( contentSum/entriesSum),2.)) / sqrt(entriesSum)));

    }

  }
 if (projx) delete projx;
  return projy;

}
