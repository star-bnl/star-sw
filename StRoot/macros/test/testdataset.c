{
 gROOT->Reset();
#include "iostream.h"
  gSystem.Load("libasu.so");
  gSystem.Load("libdsl.so");
  gSystem.Load("St_base.so");
  St_DataSetIter d;
  d.Mkdir("v1/v1_1/v1_1_1");
  d.Ls();
  d.Mkdir("v1/v1_2/v1_2_1");
  d.Pwd()->ls("*");
  d.Mkdir("v1/v1_2/v1_2_2");
  d.Pwd()->ls("*");
  d.Mkdir("v1/v21/v3211");
  d.Pwd()->ls("*");
  cout  << endl << "------------ 1 ------------ " << endl;
  d.Mkdir("v1/v21/v3212/v4");
  cout << "the current path: " << d("v1/v21/v3212/v4")->Path() << endl;
  d.Pwd()->ls("*");

  cout  << endl << "------------ 2 ------------ " << endl;

  cout << "recreating directories" << endl;

  d.Rmdir("v1/v1_1/v1_1_1");
  d.Pwd()->ls("*");
  cout  << endl << "------------ 3 ------------ " << endl;
  d.Mkdir("v1/v1_1/v1_1_1");
  d.Pwd()->ls("*");
  cout  << endl << "------------ 4 ------------ " << endl;
  d.Rmdir("v1/v1_1/v1_1_1");
  d.Pwd()->ls("*");
  cout  << endl << "------------ 5 ------------ " << endl;
  d.Mkdir("v1/v1_1/v1_1_1");
  d.ls("/v1",3);
  cout  << endl << "------------ 6 ------------ " << endl;
  d.Rmdir("v1/v21");
  cout  << endl << "------------ 7 ------------ " << endl;
  d.ls("","*");
  d.Rmdir("v1");
  if (d.Pwd()) cout << "Error the directory should be undefined nowadays " << endl;
  else cout << "Ok! The last dataset has NO active directory anymore" << endl;

  cout  << endl << "------------ 8 ------------ " << endl;
}
