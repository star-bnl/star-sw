void setupPalette(){

  // setup primary color map + push the range as far as possible

  const Int_t colSection = 12;
  const Int_t colNum = 5*colSection;
  const Int_t colStart = 51;
  Int_t palette[colNum];;
  

  Int_t i,k; i=0;

   // (1,0,1) -> (0,0,1)
  for (i=0; i<colSection; i++) {
      TColor* cold=gROOT->GetColor(i+colStart);
      delete cold;
    TColor *color = new TColor(i+colStart,1-(i/((colSection)*1.0)),0,1,"");
    palette[i] = i+colStart;
    //    i++;
    }

  // (0,0,1) -> (0,1,1) 
 for (k=0; k<colSection; k++) {
    TColor* cold=gROOT->GetColor(i+colStart);
    delete cold;
    TColor *color = new TColor(i+colStart,0,(k/((colSection)*1.0)),1,"");
    palette[i] = i+colStart;
    i++;
  }

  // (0,1,1) -> (0,1,0)
  for (k=0; k<colSection; k++) {
      TColor* cold=gROOT->GetColor(i+colStart);
      delete cold;
    TColor *color = new TColor(i+colStart,0,1,1-(k/((colSection)*1.0)),"");
    palette[i] = i+colStart;
    i++;
  }

  // (0,1,0) -> (1,1,0)
  for (k=0; k<colSection; k++) {
      TColor* cold=gROOT->GetColor(i+colStart);
     delete cold;
    TColor *color = new TColor(i+colStart,(k/((colSection)*1.0)),1,0,"");
    palette[i] = i+colStart;
    i++;
  }

  // (1,1,0) -> (1,0,0)
  for (k=0; k<colSection; k++) {
     TColor* cold=gROOT->GetColor(i+colStart);
     delete cold;
    TColor *color = new TColor(i+colStart,1,1-(k/((colSection)*1.0)),0,"");
    palette[i] = i+colStart;
    i++;
  }

  for(k=0;k<50;k++){
    TColor* cold=gROOT->GetColor(i+colStart);
    delete cold;
    TColor* color=new TColor(i+colStart,1,0,0,"");
    i++;
  }

  gStyle->SetPalette(colNum,palette);
}  
