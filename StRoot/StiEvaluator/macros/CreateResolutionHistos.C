void CreateResolutionHistos(const char * infilename,
			    char * bin = "Hi",
			    char * partType="08")
 {
  TFile infile(infilename);
  
  if(! infile.IsOpen())
   {
    cerr<<"Can not open "<<infilename<<endl;
    return;
   }
  ostrstream outfilename;
  outfilename<<"Resolution"<<bin<<partType<<infilename<<ends;
  TFile outfile(outfilename.str(),"recreate");
  
  if(! outfile.IsOpen())
   {
    cerr<<"Can not open "<<outfilename<<endl;
    return;
   }
   
  ostrstream ptreshistoname, phireshistoname, etareshistoname;
  ptreshistoname<<"PtDistByPtVsPt"<<bin<<partType<<ends;
  etareshistoname<<"EtaDistByEtaVsPt"<<bin<<partType<<ends;
  phireshistoname<<"PhiDistByPhiVsPt"<<bin<<partType<<ends;
  

  TH2D*  ptresolution  = (TH2D*)infile.Get(ptreshistoname.str());
  TH2D*  etaresolution = (TH2D*)infile.Get(etareshistoname.str());
  TH2D*  phiresolution = (TH2D*)infile.Get(phireshistoname.str());

  ptresolution->SetName("ptres");
  ptresolution->FitSlicesY();
  ptres_1->SetNameTitle("ptresmean","ptresmean");
  ptres_1->Write();
  ptres_2->SetNameTitle("ptressigma","ptressigma");
  ptres_2->Write();
  ptres_chi2->SetNameTitle("ptreschi2","ptreschi2");
  ptres_chi2->Write();

  etaresolution->SetName("etares");
  etaresolution->FitSlicesY();
  etares_1->SetNameTitle("etaresmean","etaresmean");
  etares_1->Write();
  etares_2->SetNameTitle("etaressigma","etaressigma");
  etares_2->Write();
  etares_chi2->SetNameTitle("etareschi2","etareschi2");
  etares_chi2->Write();

  phiresolution->SetName("phires");
  phiresolution->FitSlicesY();
  phires_1->SetNameTitle("phiresmean","phiresmean");
  phires_1->Write();
  phires_2->SetNameTitle("phiressigma","phiressigma");
  phires_2->Write();
  phires_chi2->SetNameTitle("phireschi2","phireschi2");
  phires_chi2->Write();


  infile.Close();  
  outfile.Close();  
  
  cout<<endl;  
  
 }
 
