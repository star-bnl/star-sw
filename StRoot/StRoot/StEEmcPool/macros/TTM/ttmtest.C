// example macro to use test various components of TTM
// Author: Piotr A. Zolnierczuk, IUCF
// Date: 05/04/2004
// 
class Assert;
class EEmcTower;


// Test EEmcTower
void Test_EEmcTower()
{
  EEmcTower   t1("9TA03", 22.2,0.111);
  EEmcTower   t2 = t1;
  EEmcTower   t3("12TC11");
  t2.ADC(33.3);
  t2.dE (0.222);

  Assert::IsEqual("09TA03", t1.TowerLabel() );
  Assert::IsEqual(22.2    , t1.ADC()        ); 
  Assert::IsEqual(0.111   , t1.dE()         );
  Assert::IsEqual(9       , t1.SecLabel()   );
  Assert::IsEqual('A'     , t1.SubSecLabel());
  Assert::IsEqual(3       , t1.EtaLabel()   );

  Assert::IsEqual("09TA03",t2.TowerLabel());
  Assert::IsEqual(33.3    ,t2.ADC());
  Assert::IsEqual(0.222   ,t2.dE());

  Assert::IsTrue (t2==t1);
  Assert::IsFalse(t2!=t1);

  Assert::IsTrue (t3!=t1);
  Assert::IsFalse(t3==t1);

  t1.Out(cerr);
  t2.Out(cerr);

  cerr << " ====> Test_EEmcTower passed" << endl;
}


void
ttmtest()
{ 
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();

  gROOT->LoadMacro("macros/tdd.C");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEEmcPoolTTM");
  
  Test_EEmcTower();
}





