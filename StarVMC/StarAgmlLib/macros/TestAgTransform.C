void TestAgTransform
(
 const bool test1=false,
 const bool test2=false,
 const bool test3=false,
 const bool test4=true
)
{
  gROOT->LoadMacro("StarVMC/Geometry/macros/loadStarGeometry.C");
  loadStarGeometry(0);

  if ( test1 )
  { // PHASE 1 TESTS

    // Setup transform class
    AgTransform T("TEST","CAVE","none");
    
    // Test translation
    T.Translate( 10.0, 20.0, 30.0 );
    T.Print("Translation to x=10, y=20, z=30");

    // Test inverse translation
    T.Invert();
    T.Print("Invert the transformation");

    // And should be back to zero
    T.Translate( 10.0, 20.0, 30.0 );
    T.Print("Translate by x=10, y=20, z=30 (expect identity)");

  };

  if ( test2 )
  { // PHASE 2 TESTS

    double M[4][4] = {
      { 0.707107,   -0.707107,    0.000000,    0.000000 },
      { 0.707107,    0.707107,    0.000000,    0.000000 },
      { 0.000000,    0.000000,    1.000000,    0.000000 },
      { 0.000000,    0.000000,    0.000000,    1.000000 } 
    };

    double N[4][4] = {
      { 0.707107,    0.707107,    0.000000,    0.000000 },
      {-0.707107,    0.707107,    0.000000,    0.000000 },
      { 0.000000,    0.000000,    1.000000,    0.000000 },
      { 0.000000,    0.000000,    0.000000,    1.000000 } 
    };

    // Setup transform class
    AgTransform T("TEST","CAVE","none");
    T.Matrix ( M );
    T.Translate( 1.0, 1.0, 1.0 );
    T.Print("Rotation by 45deg about z followed by translation to 1,1,1");

    TGeoRotation rot; rot.RotateZ(45);
    TGeoTranslation trn( 1.0, 1.0, 1.0 );
    TGeoCombiTrans t(trn, rot);
    t.Print();

    T.Matrix( N );
    T.Print("Rotation -45 degrees back");
    
    t.RotateZ(-45);
    t.Print();


  };
  if ( test3 )
  { // PHASE 3 TESTS

    double M[4][4] = {
      { 0.707107,    0.000000,    0.707107,    0.000000},
      { 0.000000,    1.000000,    0.000000,    0.000000},
      {-0.707107,    0.000000,    0.707107,    0.000000},
      { 0.000000,    0.000000,    0.000000,    0.000000}
    };
    double N[4][4] = {
      { 0.707107,    0.000000,   -0.707107,    0.000000},
      { 0.000000,    1.000000,    0.000000,    0.000000},
      { 0.707107,    0.000000,    0.707107,    0.000000},
      { 0.000000,    0.000000,    0.000000,    0.000000}
    };

    // Setup transform class
    AgTransform T("TEST","CAVE","none");
    T.Matrix ( M );
    T.Translate( 1.0, 1.0, 1.0 );
    T.Print("Rotation by 45deg about y followed by translation to 1,1,1");

    TGeoRotation rot; rot.RotateY(45);
    TGeoTranslation trn( 1.0, 1.0, 1.0 );
    TGeoCombiTrans t(trn, rot);
    t.Print();

    T.Matrix( N );
    T.Print("Rotation -45 degrees back");
    
    t.RotateY(-45);
    t.Print();

    T.Invert();
    T.Print("Inverted");

  };
  if ( test4 )
  { // PHASE 4 TESTS

    double M[3][3] = {
      { 0.707107,    0.000000,    0.707107},
      { 0.000000,    1.000000,    0.000000},
      {-0.707107,    0.000000,    0.707107}
    };
    double N[3][3] = {
      { 0.707107,    0.000000,   -0.707107},
      { 0.000000,    1.000000,    0.000000},
      { 0.707107,    0.000000,    0.707107}
    };

    // Setup transform class
    AgTransform T("TEST","CAVE","none");
    T.Rotation( M );
    T.Translate( 1.0, 1.0, 1.0 );
    T.Print("Rotation by 45deg about y followed by translation to 1,1,1");

    TGeoRotation rot; rot.RotateY(45);
    TGeoTranslation trn( 1.0, 1.0, 1.0 );
    TGeoCombiTrans t(trn, rot);
    t.Print();

    T.Rotation( N );
    T.Print("Rotation -45 degrees back");
    
    t.RotateY(-45);
    t.Print();

    T.Invert();
    T.Print("Inverted");

  };
     

};
