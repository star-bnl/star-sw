class St_geant_Maker;
St_geant_Maker *geant=0;
void GeomDraw(const char *fzFile="complete") 
{
   // Read the ZEBRA file with GEANT geometry
   // Convert it to TVolume format 
   // draw it out with OpenGL Viewer
  TString geomAccess = fzFile;
  TString geomKuipCmd;
  if (gSystem->AccessPathName(geomAccess.Data()) ) 
 //    geant->SetInputFile(fzFile);
  else {
     // Check 
     geomAccess.Strip(TString::kBoth);
     if (!geomAccess.CountChar(' ') && (geomAccess.Contains("y") || geomAccess.Contains("complete")) ) {
       geomKuipCmd = "detp geometry ";
       geomKuipCmd += geomAccess; geomAccess = "" ;
//       geant->LoadGeometry(geomKuipCmd.Data());            
     } else {
          printf("\n");
          printf("Usage: root4star \'GeomDraw.C(const char *geomDescriptor)\' \n");
          printf("-----  where \"geomDescriptor\" can be either \n");
          printf("        1. star geometry version like \"year2003\" (see: http://www.star.bnl.gov/STAR/comp/prod/MCGeometry.html) \n");
          printf("            example: root4star \'GeomDraw.C(\"year2003\")\'\n");                
          printf("        2. the proper ZEBRA fz file, for example \n");
          printf("            example: root4star \'GeomDraw.C(\"/star/u/potekhin/gstardata/y2003x_complete.fz \")\'\n\n"); 
          printf("---------------\n"); 
          printf(" One can adjust the view via ROOT Browser or ROOT TCanvas \n"); 
          printf(" 1. Select the volume you are ineresting in with left mouse button\n"); 
          printf(" 2. Bring up the ROOT context menu with the right mouse button click\n"); 
          printf(" 3. Select class method to execute and click it with left mouse button\n");           
          printf("---------------\n"); 
          printf("     List of the usefull TVolume methods and its parameters:\n"); 
          printf("     1. TVolume::Draw(const char *levels=\"3\") - Define the bumber of the level of the geometry hierachy to be drawn\n"); 
          printf("        Examples:\n");
          printf("        1.1. Draw(\"5\")     - draw 5 levels deep \n");
          printf("        1.2. Draw(\"4:3\")   - draw from the 4th level 3 level deep\n");
          printf("        1.3. Draw(\"4,6,8\") - draw 3 levels, namelt 3d,6th and 8th \n");
          printf("        1.4. Draw(\"4-8\")   - draw from the 3d level till 8th including levels, namely 3d,6th and 8th \n\n");
          printf("     2.  SetVisibility    0 - everything visible,\n");
          printf("                          2 - this unvisible, but sons are visible\n");
          printf("                          1 - this visible but sons\n");
          printf("                          3 - neither this nor its sons are visible\n\n");       
          printf(" $Id: GeomDraw.C,v 1.1 2004/07/23 18:42:34 fine Exp $\n");
          return;              
     }
   }

  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("St_g2t.so");
  gSystem->Load("St_geant_Maker");
  gSystem->Load("StUtilities");

  chain = new StChain(); 
  geant = new St_geant_Maker();
  geant->SetActive(kFALSE);
  printf(" <%s> <%s> \n", geomAccess.Data(), geomKuipCmd.Data());
  if (! geomAccess.IsNull() ) 
     geant->SetInputFile(geomAccess.Data());
  else {
       gSystem->Load("geometry");
       geant->LoadGeometry(geomKuipCmd.Data());            
    }
  chain->Init();
  TVolume *v = (TVolume *)geant->Work();
  new TBrowser("STAR Geometry", v);
  v->Draw();
  gPad->Modified();
  gPad->Update();
}
