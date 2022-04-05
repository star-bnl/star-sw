{
  St_DataSetIter d;
  d.Mkdir("v1/v1_1/v1_1_1");
  d.Pwd()->ls("*");
  d.Mkdir("v1/v1_2/v1_2_1");
  d.Pwd()->ls("*");
  d.Mkdir("v1/v1_2/v1_2_2");
  d.Pwd()->ls("*");
  d.Mkdir("v1/v21/v3211");
  d.Pwd()->ls("*");
  cout  << endl << "----------- 1 ------------ " << endl;
  d.Mkdir("v1/v21/v3212/v4");
  d.Pwd()->ls("*");

  cout  << endl << "------------ 2 ----------- " << endl;

  cout << "recreating directpories" << endl;

  d.Rmdir("v1/v1_1/v1_1_1");
  d.Pwd()->ls("*");
  cout  << endl << "------------- 3 ---------- " << endl;
  d.Mkdir("v1/v1_1/v1_1_1");
  d.Pwd()->ls("*");
  cout  << endl << "-------------- 4 --------- " << endl;
  d.Rmdir("v1/v1_1/v1_1_1");
  d.Pwd()->ls("*");
  cout  << endl << "------------ 5 ----------- " << endl;
  d.Mkdir("v1/v1_1/v1_1_1");
  d.Pwd()->ls("*");
  cout  << endl << "----------- 6 ------------ " << endl;
  d.Rmdir("v1/v21");
  cout  << endl << "----------- 7 ------------ " << endl;
  d.Pwd()->ls("*");
  d.Rmdir("v1");
  d.Pwd()->ls("*");

  cout  << endl << "----------- 8 ------------ " << endl;
}
