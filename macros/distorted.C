void distorted( )
{ 

  TStyle* myStyle = new  TStyle("boldStyle", "Bold Style");

  myStyle->SetOptTitle(0);
  //myStyle->SetOptStat(0);
  myStyle->SetPalette(8,0);
  myStyle->SetCanvasBorderMode(0);
  myStyle->SetPadTickX(1);
  myStyle->SetPadTickY(1);
  myStyle->SetPadBottomMargin(0.15);
  myStyle->SetPadLeftMargin(0.17);
  //  myStyle->SetLabelSize(0.05,"X");
  //  myStyle->SetLabelSize(0.05,"Y");
  //  myStyle->SetLabelSize(0.05,"Z");
  //  myStyle->SetTitleSize(0.06,"X");
  //  myStyle->SetTitleSize(0.06,"Y");
  //  myStyle->SetTitleSize(0.06,"Z");
  myStyle->SetTitleOffset(1.3,"Y");
  myStyle->SetLineWidth(3) ;
  myStyle->SetFrameLineWidth(3);
  myStyle->SetHistLineWidth(3);
  myStyle->SetHistLineColor(kBlue);
  myStyle->SetFuncWidth(3);
  myStyle->SetFuncColor(kGreen);

  gROOT->ForceStyle();
  gStyle->ls();  

  gSystem -> Load("St_base.so") ;
  gSystem -> Load("StDbUtilities.so") ;
  gSystem -> Load("StMagField.so") ;

  StMagField* xx = new StMagField ; // Make the plots persistent 

  enum EField   { kUndef=0, kConstant=1, kMapped=2, kChain=3 } ;

  xx -> Init(kMapped,0.5) ;
  xx -> Plot( 0 ) ;
  xx -> Time ( 500 ) ;
  xx -> Make() ;

}





