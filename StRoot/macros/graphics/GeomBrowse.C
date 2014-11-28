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
          printf("\n$Id: GeomBrowse.C,v 1.15 2010/05/27 17:03:56 fine Exp $\n");
}                 
//_____________________________________________________________________________________________________________
StGeomBrowser *GeomBrowse(const char *fzFile="y2009")
{
   GeomDrawUsage();
   // gSystem->Load("libGeomBrowser");  

   gSystem->Load("St_base");
   gSystem->Load("StChain");
   gSystem->Load("St_Tables");
   gSystem->Load("St_g2t.so");
   gSystem->Load("StarMagField");
   gSystem->Load("St_geant_Maker");
   gSystem->Load("StUtilities");
   gSystem->Load("StarClassLibrary");  
   gSystem->Load("StEvent");  
   gSystem->Load("StEventUtilities"); 
   gSystem->Load("St_geom_Maker");  
   StGeomBrowser *a = new StGeomBrowser;
   a->SetFile(fzFile); 
   a->SetSize(360,600); 
   a->Show();
   geoBrowser = a;

   return geoBrowser;
}
