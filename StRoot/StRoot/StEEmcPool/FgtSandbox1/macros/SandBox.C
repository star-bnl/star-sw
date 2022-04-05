class AgModule;
class Geometry;
//
Geometry *builder = 0;

void SandBox(Bool_t  closeGeo = true,  Bool_t  draw = true ) {
  printf("\nIN SAND_BOX:   closeGeo=%d draw=%d\n",closeGeo,draw);
  gSystem -> Load  (".$STAR_HOST_SYS/lib/libStarAgmlLib.so");
  gSystem -> Load  (".$STAR_HOST_SYS/lib/libStarGeometry.so");
  gSystem -> Load  (".$STAR_HOST_SYS/lib/libGeometry.so");

  // Setup code to create ROOT/TGeo geometry
  AgBlock::SetStacker( new StarTGeoStacker() );

  // Create the geometry builder
  builder = new Geometry();

  // Setup the sandbox environment
  playground();
  
  // Create the SandBox geometry
  builder -> ConstructGeometry("SandBox");

  // Add the FGT to the playground
  ConstructFgtd();
  
  // Close the geometry in preparation to draw it
  if(closeGeo)  gGeoManager -> CloseGeometry();

  // Apply a different color scheme
  ColorScheme();

  // Make different bits and pieces invisible
  Invisibles();

  // And set the maximum visible depth
  gGeoManager -> SetVisLevel(10);

  // And visualize with the OpenGL viewer 
  if (draw)
    gGeoManager -> GetTopVolume() -> Draw("ogl");
    //gGeoManager -> GetTopVolume() -> Draw();
    
  // export this geometry
  //  gGeoManager GetTopVolume() ->Write("myFgtGeom.C");
  // I could  use:
  

}
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///
/// Configure options for sandbox geometry
///
void playground()
{

  Geom_t geom;
  geom.select = "SandBox"; {

    // on/off what I want to see
    geom.sconFlag = "SCON13"; geom.sconStat = 0;
    geom.tpceFlag = "TPCE04"; geom.tpceStat = 0;
    geom.btofFlag = "BTOFc6"; geom.btofStat = 0;
    geom.calbFlag = "CALB02"; geom.calbStat = 0;

    geom.vpddFlag = "VPDD07"; geom.vpddStat = 0;
    geom.ftpcFlag = "FTPC01"; geom.ftpcStat = 0;
    geom.svttFlag = "SVTTof"; geom.svttStat = 0;
    geom.phmdFlag = "PHMDof"; geom.phmdStat = 0;

    geom.ftroFlag = "FTRO01"; geom.ftroStat = 0;
    geom.mutdFlag = "MUTD03"; geom.mutdStat = 0;

    geom.sisdFlag = "SISDof"; geom.sisdStat = 0;
    geom.ecalFlag = "ECALv6"; geom.ecalStat = 0;
    geom.fpdmFlag = "FPDM03"; geom.fpdmStat = 0;

    geom.bbcmFlag = "BBCMon"; geom.bbcmStat = 0;
    geom.caveFlag = "CAVE04"; geom.caveStat = 1;
    geom.pipeFlag = "PIPE06"; geom.pipeStat = 1;
    geom.fgtdFlag = "FGTDon"; geom.fgtdStat = 1;
    geom.fgtdFlag = "FGMOon"; geom.fgtdStat = 0;
    geom.magpFlag = "MAGPon"; geom.magpStat = 0;

    geom.closeGeometry = false;

    geom.SetTitle("STAR Geometry SanBox");
    geom.fill();
  }
   

}
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///
/// FGT "Constructor"
///
Bool_t ConstructFgtd()
{

  // Prepare the interface to the structures defined
  // within the FgtdGeo.xml
  AgStructure::AgDetpNew( "FgtdGeo3", "Forward Gem Tracker Configuration" );
  
  // Here you can change the values assigned to the data structures
  // inside of the geometry module.  i.e. you can change the FGTG
  // structure members with the following call... 
  AgStructure::AgDetpAdd( "Fgtg_t", "configjan", (float)2 );
  // where the second arguement is the name of the member all in 
  // lower case and the third arguement is the value taken.
  // Note that the compiler is picky, and you need to make sure
  // that you pass in the right type for the third arguemnt.
  // 
  // If you need to change an array, you would create an instance
  // of Array_t... e.g.
  //
  // Array_t<float> a(2); a[0]=1; a[1]=2;
  // AgDetpAdd("Fgtd_t", "array", a);
  
  builder -> CreateModule( "FgtdGeo3" );

}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Integration with GeomBrowse.C
//
TDataSet *CreateTable()
{
  
  // We won't be drawing things ourself
  draw = false;

  // Create the geometry
  SandBox();
  
  TObjectSet *mygeom = 0;
  if (gGeoManager) {
    mygeom = new TObjectSet("Geometry",gGeoManager,kFALSE);
    mygeom->SetTitle("Dyson");
  }
  return (TDataSet *) mygeom;

}


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///
/// Some nice display features
///
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
