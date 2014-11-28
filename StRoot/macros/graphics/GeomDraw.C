class St_geant_Maker;
St_geant_Maker *geant=0;
//_________________________________________________________________________________________________________________
void GeomDrawUsage() {
          printf("\n");
          printf("Usage: root4star \'GeomDraw.C(const char *geomDescriptor,Float_t bombFactor=2.0, const char *outRootFile="")\' \n");
          printf("-----  where \"geomDescriptor\" can be either \n");
          printf("        1. STAR geometry version like \"year2003\" (see: http://www.star.bnl.gov/STAR/comp/prod/MCGeometry.html) \n");
          printf("            example: root4star \'GeomDraw.C(\"year2003\")\'\n");                
          printf("        2. the proper ZEBRA fz file, for example \n");
          printf("            example: root4star \'GeomDraw.C(\"/star/u/potekhin/gstardata/y2003x_complete.fz \")\'\n\n"); 
          printf("         \"Float_t bombFactor\" - the so-called \"bombFactor\" to get the exploiding view of the detector.\n");
          printf("                                    To get the normal view the bombFactor has to be set to 1.0\n");
          printf("                                    It is usually useless to apply bombFactor < 1.0 \n");
          printf("---------------\n"); 
          printf(" One can adjust the view via ROOT Browser or ROOT TCanvas \n"); 
          printf(" 1. Select the volume you are interesting in with left mouse button\n"); 
          printf(" 2. Bring the ROOT context menu up with the right mouse button click\n"); 
          printf(" 3. Select class method to execute and click it with left mouse button\n");           
          printf("---------------\n"); 
          printf("     List of the usefull TVolume methods and its parameters:\n"); 
          printf("     1. TVolume::Draw(const char *levels=\"3\") - \n");
          printf("                       - define the number of the levels of the geometry hierarchy to be drawn\n"); 
          printf("     2. TVolume::SetVisibility - \n");
          printf("                         0 - everything visible,\n");
          printf("                         2 - this invisible, but sons are visible\n");
          printf("                         1 - this visible but sons\n");
          printf("                         3 - neither this nor its sons are visible\n\n"); 
          printf("---------------\n"); 
          printf(" To get the OpenGL view one has to\n");
          printf("    1. Turn Qt ROOT Layer on (see: http://www.rhic.bnl.gov/~fine/EventDisplay \n");
          printf("    2. Select \"OpenGL view\" from the \"View\" menu of ROOT TCanvas \n");                
          printf("---------------\n"); 
          printf(" To adjust the so-called bombFactor you can invoke \n");
          printf(" the C++ statement from the ROOT command prompt:\n\n");
          printf("  gGeometry->SetBomb(1.6);\n\n");
          printf(" To change the background color  call:\\nn");
          printf("        gPad->SetFillColor(kWhite);\n\n");
          printf("   \"kWhite\" background is advised \n");
          printf("   if you want to print the image to the paper\n");
          printf("---------------\n"); 
          
          printf("\n$Id: GeomDraw.C,v 1.10 2006/10/27 21:33:33 fine Exp $\n");
}                 
//_____________________________________________________________________________________________________________
void GeomDraw(const char *fzFile="complete",Float_t bombFactor=1.4, const char *out = "")
{
   // Read the ZEBRA file with GEANT geometry
   // Convert it to TVolume format 
   // draw it out with OpenGL Viewer
  TString geomAccess = fzFile;
  TString geomKuipCmd;
  if (gSystem->AccessPathName(geomAccess.Data()) ) 
  {
     // Check 
     geomAccess.Strip(TString::kBoth);
     if (!geomAccess.CountChar(' ') && (geomAccess.First('y')==0 || geomAccess.First("complete") == 0 ) ) {
       geomKuipCmd = "detp geometry ";
       geomKuipCmd += geomAccess; geomAccess = "" ;
     } else {
          printf("\n *** Error ***   Wrong input parameter: <%s>\n", fzFile);
          GeomDrawUsage();
          return;              
     }
   }
   
  // Workaroung of STAR bug with ROOT 4.00.04
  //- TString unixLDPath = "$ROOT/$ROOT_LEVEL.qt/.$STAR_HOST_SYS/rootdeb/lib:";
  //-  unixLDPath += gEnv->GetValue("Root.DynamicPath","");
  //- gEnv->SetValue("Root.DynamicPath",unixLDPath.Data());
  //- gSystem->Load("$ROOT/$ROOT_LEVEL.qt/.$STAR_HOST_SYS/rootdeb/lib/libRQTGL.so");
  //- end of workaroung
  
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("St_g2t.so");
  gSystem->Load("StarMagField");
  gSystem->Load("St_geant_Maker");  
  gSystem->Load("StUtilities");
  chain = new StChain(); 
  geant = new St_geant_Maker();
  geant->SetActive(kFALSE);
  if (! geomAccess.IsNull() ) {
     printf("\n ----------------------------------------------------------\n");
     printf(" Draw the GEANT geometry from <%s> file\n", geomAccess.Data());
     printf(" ----------------------------------------------------------\n\n");
     geant->SetInputFile(geomAccess.Data());
  } else {
     printf("\n ----------------------------------------------------------\n");
     printf(" Draw the GEANT generated geometry <%s> \n", geomKuipCmd.Data());
     printf(" ----------------------------------------------------------\n\n");
     gSystem->Load("geometry");
     geant->LoadGeometry(geomKuipCmd.Data());            
  }
  chain->Init();
  TVolume *v = (TVolume *)geant->Work();
  if (v) {
     // Make CAVE invisible
     TVolume *cave = (TVolume *)v->FindByName("CAVE");
     if (cave) cave->SetVisibility(2);
     TVolume *hall = (TVolume *)v->FindByName("HALL");
     GeomDrawUsage();
     if (hall) {
        hall->SetVisibility(2);
        new TBrowser("STAR Geometry", hall);
        if (bombFactor < 1)  bombFactor = 1.;
        gGeometry->SetBomb(bombFactor);        
        hall->Draw("6");        
        gPad->SetFillColor(kBlack);
    }
    gPad->Modified();
    gPad->Update();
    if (out && out[0]) {
       TFile outFile(out,"RECREATE");
       v->Write();
       outFile.Write();
       outFile.Close();      
    }   
  } else {
     fprintf(stderr,"\n\n, ** Error **, No suitable STAR geometry has been found. Abort !!! \n");   
  }
//  delete chain; chain = 0;
}
