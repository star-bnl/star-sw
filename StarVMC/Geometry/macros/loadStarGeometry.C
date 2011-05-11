class Geometry;
Geometry *build = 0;

void loadStarGeometry( const Char_t *mytag="y2009a", Bool_t agml = true )
{
  TString tag = mytag;
  gSystem->AddIncludePath(" -IStRoot -Igeom -IStarVMC ");

  gROOT   -> LoadMacro("Load.C");
  //$$$  Load("libSt_g2t, libStarMagField.so, St_geant_Maker");
  Load(".$STAR_HOST_SYS/lib/StarAgmlLib.so");

  gErrorIgnoreLevel=9999;

  // ROOT TGeo stacker
  AgBlock::SetStacker( new StarTGeoStacker() );

  // Load the master geometry libraries
  gROOT->ProcessLine(".L .$STAR_HOST_SYS/lib/libGeometry.so");
  gROOT->ProcessLine(".L .$STAR_HOST_SYS/lib/libStarGeometry.so");

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
      build -> ConstructGeometry ( tag );
    }
  else
    {
      gROOT->Macro(Form("$STAR/StarDb/VmcGeometry/%s.h",mytag));
      //      gROOT->Macro(Form("scripts/%s.h",mytag));
    }


  gGeoManager->CloseGeometry();
  gGeoManager->SetVisLevel(99);        

  return;

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
