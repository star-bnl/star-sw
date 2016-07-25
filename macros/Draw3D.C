void Draw3D()
{
  gROOT->Macro("Load.C");  //< Load STAR framework shared libraries
  // gEventDisplay->Draw3DTest(); //< Invoke the built-in rendering test
  //  gEventDisplay->SetDrawOption("{file:rotation.iv}");//< Add rotation to the scene
  //      gEventDisplay->SetFooter("STAR Event Display Example");
  //      gEventDisplay->Print("Draw3DTest.wrl"); //< Save the 3D scene into the file
  //    gEventDisplay->SetDrawOption("{view:all}"); // zoom the scene in/out to fit the entire screen
  // StDraw3D::ShowTest()
    // More complex test.
    //
    // It creates TWO different widgets
    // One is decorated with the detector geometry, 
    // another one "plain"
    //
    // Method does not recreate the widgets when it is called 
    // for several times
    //
    // It creates the widget at once and reuses it with each call.

   StDraw3D *fine[2]={0};
   if (!fine[0]) {
      fine[0] = new StDraw3D;
      fine[1] = new StDraw3D(0);// View with no detector geometry decoration
   } else {
      fine[0]->Clear();
      fine[1]->Clear();
   }
//        P  G         P  G
  float NodX[100][3]= {{189.195,       27.951,       123.966}
                      ,{187.195,       28.6187,      122.89 }
                      ,{181.195       ,30.6788      ,119.556}
                      ,{179.195       ,31.3387      ,118.454}
                      ,{177.195       ,32.0065      ,117.328}
                      ,{175.195       ,32.6132      ,116.26 }
                      ,{173.195       ,33.2385      ,115.146}
                      ,{171.195       ,33.8552      ,114.016}
                      ,{169.195       ,34.3924      ,112.964}};

  float HitX[100][3]= {{189.195,       27.951,       123.966}
                      ,{187.195,       28.6187,      122.89 }
                      ,{181.195       ,30.6788      ,119.556}
                      ,{179.195       ,31.3387      ,118.454}
                      ,{177.195       ,32.0065      ,117.328}
                      ,{175.195       ,32.6132      ,116.26 }
                      ,{173.195       ,33.2385      ,115.146}
                      ,{171.195       ,33.8552      ,114.016}
                      ,{169.195       ,34.3924      ,112.964}};

  float NodL[100][3]= {{189.195+5,       27.951+10,       123.966-50}
                      ,{187.195+5,       28.6187+10,      122.89-50 }
                      ,{181.195+5       ,30.6788+10      ,119.556-50}
                      ,{179.195+5       ,31.3387+10      ,118.454-50}
                      ,{177.195+5       ,32.0065+10      ,117.328-50}
                      ,{175.195+5       ,32.6132+10      ,116.26-50 }
                      ,{173.195+5       ,33.2385+10      ,115.146-50}
                      ,{171.195+5       ,33.8552+10      ,114.016-50}};
  float HitL[100][3]= {{189.195+5,       27.951+10,       123.966-50}
                      ,{187.195+5,       28.6187+10,      122.89-50 }
                      ,{181.195+5       ,30.6788+10      ,119.556-50}
                      ,{179.195+5       ,31.3387+10      ,118.454-50}
                      ,{177.195+5       ,32.0065+10      ,117.328-50}
                      ,{175.195+5       ,32.6132+10      ,116.26-50 }
                      ,{173.195+5       ,33.2385+10      ,115.146-50}
                      ,{171.195+5       ,33.8552+10      ,114.016-50}};
  int nN=9,nH=9;
  // Draw the test points
  fine[0]->Points(nH, HitX[0], kVtx);
  fine[0]->SetComment("Hits and Geometry");
  
  fine[0]->Line  (nN, NodX[0], kGlobalTrack);  
  fine[0]->SetComment("Track and Geometry");
  
  fine[1]->Points(nH, HitL[0], kVtx);
  fine[1]->SetComment("Hits no Geometry");
  
  fine[1]->Line  (nN, NodL[0], kGlobalTrack);
  fine[1]->SetComment("Track no Geometry");
  for (int i=0;i<2;i++) { fine[i]->UpdateModified(); }
//  while(!gSystem->ProcessEvents()){}; 
}

