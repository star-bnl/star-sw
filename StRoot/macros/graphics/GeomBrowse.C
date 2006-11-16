class  StGeomBrowser;
StGeomBrowser *geoBrowser=0;
//_________________________________________________________________________________________________________________
void GeomDrawUsage() {
          printf("\n");
          printf("Usage: root4star \'GeomBrowse.C(const char *geomDescriptor,Float_t bombFactor=2.0, const char *outRootFile="")\' \n");
          printf("-----  where \"geomDescriptor\" can be either \n");
          printf("        1. STAR geometry version like \"year2003\" (see: http://www.star.bnl.gov/STAR/comp/prod/MCGeometry.html) \n");
          printf("            example: root4star \'GeomDraw.C(\"year2003\")\'\n");                
          printf("        2. the proper ZEBRA fz file, for example \n");
          printf("            example: root4star \'GeomDraw.C(\"/star/u/potekhin/gstardata/y2003x_complete.fz \")\'\n\n"); 
          printf("         \"Float_t bombFactor\" - the so-called \"bombFactor\" to get the exploiding view of the detector.\n");
          printf("                                    To get the normal view the bombFactor has to be set to 1.0\n");
          printf("                                    It is usually useless to apply bombFactor < 1.0 \n");
          printf("---------------\n"); 
          
          printf("\n$Id: GeomBrowse.C,v 1.2 2006/11/16 00:41:48 fine Exp $\n");
}                 
//_____________________________________________________________________________________________________________
void GeomBrowse(const char *fzFile="complete",Float_t bombFactor=1.4, const char *out = "")
{
  gROOT->ProcessLine("StCheckQtEnv::SetQtEnv();") ;
  GeomDrawUsage();
 // gSystem->Load("libGeomBrowser");  
       gSystem->Load("St_base");
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
