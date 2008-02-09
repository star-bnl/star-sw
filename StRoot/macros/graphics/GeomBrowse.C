class  StGeomBrowser;
StGeomBrowser *geoBrowser=0;
//_________________________________________________________________________________________________________________
void GeomDrawUsage() {
          printf("\n");
          printf("Usage: root4star \'GeomBrowse.C(const char *geomDescriptor\"complete\")\' \n");
          printf("-----  where \"geomDescriptor\" can be either \n");
          printf("        1. STAR geometry version like \"year2003\" (see: http://www.star.bnl.gov/STAR/comp/prod/MCGeometry.html) \n");
          printf("            example: root4star \'GeomDraw.C(\"year2003\")\'\n");                
          printf("\n");                
          printf("        2. ROOT macros that instantiates the geometry object\n");
          printf("            example: root4star \'GeomDraw.C(\"$STAR/StarDb/VmcGeometry/Geometry.year2000.C\")\'\n");                
          printf("\n");                
          printf("        3. ROOT file  that contains the geometry object\n");
          printf("            example: root4star \'GeomDraw.C(\"Geometry.root\")\'\n");                
          printf("\n");                
          printf("        4. the proper ZEBRA fz file, for example \n");
          printf("            example: root4star \'GeomDraw.C(\"/star/u/potekhin/gstardata/y2003x_complete.fz \")\'\n\n"); 
          printf("---------------\n"); 
          printf("One always can select the geometry from the \"file menu\" of the GUI interface later on too\n");          
          printf("\n");                
          printf("---------------\n"); 
          printf("\n$Id: GeomBrowse.C,v 1.6 2008/02/09 01:17:10 fine Exp $\n");
}                 
//_____________________________________________________________________________________________________________
void GeomBrowse(const char *fzFile="complete")
{
    GeomDrawUsage();
   // gSystem->Load("libGeomBrowser");  
   gSystem->Load("St_base");
   // check Coin env and load if present
   TString ivrootDir = "$ROOT/5.99.99/Coin2/.$STAR_HOST_SYS/lib/";
   gSystem->ExpandPathName(ivrootDir);
   bool CheckCoin = true;
   if (!gSystem->AccessPathName(ivrootDir.Data())) {
      printf(" Loading ... libSoQt.so %d     \n",gSystem->Load(ivrootDir+"libSoQt"));
      printf(" Loading ... libCoin.so %d     \n",gSystem->Load(ivrootDir+"libCoin"));
      printf(" Loading ... libSmallChange %d \n",gSystem->Load(ivrootDir+"libSmallChange"));
      CheckCoin = false;
   }
   if (!StCheckQtEnv::SetQtEnv(CheckCoin))
   {
      gSystem->Load("StChain");
      gSystem->Load("St_Tables");
      gSystem->Load("St_g2t.so");
      gSystem->Load("StarMagField");
      gSystem->Load("St_geant_Maker");
      gSystem->Load("StUtilities");
      gSystem->Load("St_geom_Maker");  
      StGeomBrowser *a = new StGeomBrowser;
      a->SetFile(fzFile); 
      a->Show();
   }
}
