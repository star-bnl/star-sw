class Geometry;
Geometry *build = 0;

void loadStarGeometry( const Char_t *mytag="y2009a", Bool_t agml = true )
{
  gEnv->SetValue("Logger.Colors","YES");

  TString tag = mytag;
  gSystem->AddIncludePath(" -IStRoot -Igeom -IStarVMC -IStarVMC/Geometry/macros ");

  gROOT   -> LoadMacro("Load.C");
  //$$$  Load("libSt_g2t, libStarMagField.so, St_geant_Maker");
  //  Load(".$STAR_HOST_SYS/lib/StarAgmlLib.so");
  Load("libGeom.so");
  Load("StarRoot.so");
  Load("St_base.so");
  Load("StUtilities.so");
  Load("libPhysics.so");
  Load("StarAgmlUtil.so");
  Load("StarAgmlLib.so");
  Load("libStarGeometry.so");
  Load("libGeometry.so");

  if (!mytag) return;

  if (!gMessMgr) gMessMgr = new StMessMgr();

  gErrorIgnoreLevel=9999;

  // ROOT TGeo stacker
  StarTGeoStacker *stacker = new StarTGeoStacker();
  //  stacker -> Debug("TPAD", "position");
  //  stacker -> Debug("TPCM", "position");
  //  stacker -> Debug("TSWH", "position"); 
  //  stacker -> Debug("TPGV", "position");
  //  stacker -> Debug("TSAS", "position");
  //  stacker -> Debug("TWAS", "position");
  //  stacker -> Debug("TSGT", "position");
  AgBlock::SetStacker( stacker );

  // Load the master geometry libraries
  //  gROOT->ProcessLine(".L libGeometry.so");
  //  gROOT->ProcessLine(".L libStarGeometry.so");


  // Instantiate the geometry builder
  build = new Geometry(); 

  // Once the geometry is created we can load in the DB
  gROOT->ProcessLine(".L StarGeometryDb.C");
  gROOT->ProcessLine("StarGeometryDb();");

  //
  // Setup a geometry control structure for testing purposes
  //  
  gROOT->ProcessLine(Form(".!mkdir %s",mytag));
  if ( agml )
    {
      if ( !gGeoManager ) new TGeoManager(tag,tag+" | dyson");	
      build -> ConstructGeometry ( tag );
    }
  else
    {
      loadStarsimGeometry( mytag );
      // gROOT->Macro(Form("$STAR/StarDb/VmcGeometry/%s.h",mytag));
      // gROOT->Macro(Form("scripts/%s.h",mytag));
    }

  //  gGeoManager->CloseGeometry();
  gGeoManager->SetVisLevel(99);        

  return;

}


void loadDevStarGeometry( const Char_t *mytag="upgr2012" )
{

  assert(0);

  TString tag = mytag;
  gSystem->AddIncludePath(" -IStRoot -Igeom -IStarVMC ");

  gROOT   -> LoadMacro("Load.C");
  //$$$  Load("libSt_g2t, libStarMagField.so, St_geant_Maker");
  Load("StarAgmlLib.so");
  Load("libGeometry.so");
  Load("libStarGeometry.so");

  gErrorIgnoreLevel=9999;

  // ROOT TGeo stacker
  AgBlock::SetStacker( new StarTGeoStacker() );

  // Load the master geometry libraries


  // Instantiate the geometry builder
  build = new Geometry(); 

  // Once the geometry is created we can load in the DB
  gROOT->ProcessLine(".L StarGeometryDb.C");
  gROOT->ProcessLine("StarGeometryDb();");

  //
  // Setup a geometry control structure for testing purposes
  //  
  if ( !gGeoManager ) new TGeoManager(tag,tag+" | dyson");	
  gROOT->ProcessLine(Form(".!mkdir %s",mytag));
  build -> ConstructGeometry ( tag );
  
  std::cout << "================================================================" << std::endl;
  std::cout << mytag << " constructed" << std::endl;
  std::cout << "You may addModule( ... ) to add a new detector to the geometry." << std::endl;
  std::cout << "You must closeGeometry() to visualize" << std::endl;

  return;

}

void closeGeometry()
{
  gGeoManager->CloseGeometry();
}


void addModule( const Char_t *module )
{
  build -> CreateModule( module );
}




void printGeometry( const Char_t *name )
{
  Geom_t geom;
  geom.Use("select",name);
  geom.Print();
}



void Invisibles()
{

  const Char_t *invisible[] =
    {
      "MAGP", 
      //      "ZCAL", 
      //      "UPST", 
      "TPSS", 
      "FSEN"
    };

  TGeoVolume *volume = 0;
  for ( UInt_t i=0;i<sizeof(invisible)/sizeof(Char_t *);i++ )
    {
      volume = gGeoManager->FindVolumeFast( invisible[i] );
      if ( volume ) {
	volume->InvisibleAll();
	volume->SetVisDaughters(false);
      }
    } 
}


void ColorScheme()
{
  Int_t color_map[] = {kGray+2,1,42,kGreen+2,/*kBlue-9*/ 11,32,kBlue-9,22,8};
  // loop over all volumes and remap the colors                                                                                                                                                                
  TIter next( gGeoManager->GetListOfVolumes() );
  while ((volume=(TGeoVolume*)next()))
    {
      Int_t color = volume->GetLineColor();
      if ( color < 8 )
        color = color_map[color];
      volume->SetLineColor(color);
      volume->SetFillColor(color);

      TString name=volume->GetName();
      if ( name=="TPAD" || name=="TPA1" || name=="TPSS" )
        {
          volume->SetLineColor(47);
          volume->SetFillColor(47);
        }
    }
}

void ColorScheme2()
{

  TIter next( gGeoManager->GetListOfVolumes() );
  while ((volume=(TGeoVolume*)next()))
    {

      TString name = volume->GetName();
      UShort_t c1 = name(0); c1 -= (UShort_t)'a';
      UShort_t c2 = name(1); c2 -= (UShort_t)'a';

      Int_t color = (c1+c2)%40+8;
      volume->SetLineColor(color);
      volume->SetFillColor(color);
      

    }

}


void loadStarsimGeometry( const Char_t *tag )
{

  std::cout << "Loading starsim geometry: " << tag << std::endl;
  ofstream kumac(Form("%s.kumac",tag));
  kumac << "DETP geom " << tag << std::endl;
  kumac << "GEXEC $STAR_LIB/geometry.so" << std::endl;
  kumac << "GCLOS all" << std::endl;
  kumac << "GRFILE " << tag << ".rz" << std::endl;
  kumac << "EXIT" << std::endl;
  kumac.close();

  gROOT->ProcessLine(Form(".!starsim -w 0 -b %s.kumac",tag));
  gROOT->ProcessLine(Form(".!g2root %s.rz",tag));
  gROOT->LoadMacro(Form("%s.C",tag));
  gROOT->ProcessLine(Form("%s();",tag));

}
