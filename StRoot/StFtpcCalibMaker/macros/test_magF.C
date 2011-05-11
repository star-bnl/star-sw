// $Id: test_magF.C,v 1.4 2011/05/11 11:55:46 jcs Exp $
//
// $Log: test_magF.C,v $
// Revision 1.4  2011/05/11 11:55:46  jcs
// had to change order of library loading to avoid undefined symbols
//
// Revision 1.3  2008/05/16 18:36:57  jcs
// update FTPC calibration macros
//
// Revision 1.2  2006/03/15 15:14:06  jcs
// add lines for listing CVS update info
//

void test_magF()
{
// macro to read in and run over the magnetic field map grid.
  gROOT->Reset();

  if (gClassTable->GetID("TTable") < 0) gSystem->Load("libStar");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StUtilities");
  gSystem->Load("StarMagField");
  gSystem->Load("StMagF");
  gSystem->Load("StDetectorDbMaker.so");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StEvent");
  gSystem->Load("StFtpcLaserMaker");
  gSystem->Load("libftpc_Tables");
  gSystem->Load("StFtpcClusterMaker");
  gSystem->Load("StFtpcTrackMaker");

  //  .L StMagF.so;
  // make a field object at reversed polarity;
  //f = new StMagF("Star Full Reversed Field",
  //                 "$STAR/StarDb/StMagF/bfield_full_positive_3D.dat",2,-1.0)

  Float_t pt[3] = new Float_t;
  Float_t B[3] = new Float_t;
  //Float_t Br[3] = new Float_t;
  //Float_t Bz[3] = new Float_t;

  Float_t Br=0;
  Float_t Bz=0;

  for (int i=0;i<3;i++)
    {
      pt[i]=B[i]=0;
      //Br[i]=Bz[i]=0; 
    }
  
  
  //pt[0] = 10.0;
  //pt[1] = 10.0;
  //pt[2] = -275.0;
  
  
  // -----------------------------------------------------
  
  cout<<endl;
  //StMagUtilities *magf=new StMagUtilities();
  
  StMagUtilities *magf=new StMagUtilities(2,1,0);
  // 2 = mapped field => auswahl mit 0,0.5,1
  //StMagUtilities *magf=new StMagUtilities("/.na49/data1pcc/star/packages/DEV/StarDb/StMagF/bfp112.map",1.0,0);
  
  // -----------------------------------------------------
  
  cout<<endl;

  cout<<"2D B-field :"<<endl;
  magf->BField(&pt[],&B[]);
  cout<<endl;

  printf(" xyz = %7.2f %7.2f %7.2f Bxyz= %7.4f %7.4f %7.4f \n", pt[0],pt[1],pt[2],B[0],B[1],B[2]);

 Float_t r=0;
 const Float_t z=275;

  cout<<endl;
  for (int k=0;k<31;k++)
    {
      r=(float) k;
      magf->BrBzField(r,z,Br,Bz);
  
      //cout<<endl;
      //printf(" rz = %7.2f %7.2f Brz= %7.4f %7.4f \n", r,z,Br,Bz);
    }

  cout<<endl;

  // ???? 3D map wie laden usw. !???

  for (int i=0;i<3;i++)
    {
      pt[i]=B[i]=0;
      //Br[i]=Bz[i]=0; 
    }

  cout<<"3D B-field :"<<endl;
  magf->B3DField(&pt[],&B[]);
  
  cout<<endl;

  printf(" xyz = %7.2f %7.2f %7.2f Bxyz= %7.4f %7.4f %7.4f \n", pt[0],pt[1],pt[2],B[0],B[1],B[2]);
  
  cout<<endl;
  delete magf;
  
}
