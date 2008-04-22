void Draw3D(int n,  float *xyz)
{
     TPolyMarker3D *markers = new TPolyMarker3D(n,xyz);
     markers->SetMarkerStyle(5);
     markers->SetMarkerColor(kBlue);
     markers->Draw();
}

void Draw3D(){
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
   if (!StCheckQtEnv::SetQtEnv(CheckCoin))   {
      // define the background image
      const char *backShape = "$STAR/StRoot/macros/graphics/StarTPC.iv";   
      printf(" Setting the background shape to be 	%s\n", backShape);
      gEnv->SetValue("Gui.InventorBackgroundShape",backShape);
   }
   float xyz[] = { 189.195,       27.951,       123.966
                 ,187.195,       28.6187,      122.89
                 ,181.195       ,30.6788       ,119.556
                 ,179.195       ,31.3387       ,118.454
                 ,177.195       ,32.0065       ,117.328
                 ,175.195       ,32.6132       ,116.26
                 ,173.195       ,33.2385       ,115.146
                 ,171.195       ,33.8552       ,114.016
                 ,169.195       ,34.3924       ,112.964
         };

   int sizeXYZ = sizeof(xyz)/sizeof(float)/3;
   draw3D(sizeXYZ,xyz);
}
