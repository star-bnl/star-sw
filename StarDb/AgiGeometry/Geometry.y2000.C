#include "CreateGeometry.h"
TDataSet *CreateTable() {
  gEnv->SetValue("isvt",3);
  gEnv->SetValue("itpc",4);
  gEnv->SetValue("ibtf",8);
  gEnv->SetValue("ical",12);
  gEnv->SetValue("ivpd",-1);
  gEnv->SetValue("ieem",-1);
  gEnv->SetValue("istb",-1);
  gEnv->SetValue("ifpd",-1);
  gEnv->SetValue("ifms",-1);
  gEnv->SetValue("ifsc",-1);
  gEnv->SetValue("imtd",-1);
  gEnv->SetValue("svtg_version",2);
  gEnv->SetValue("tpcg_version",5);
  gEnv->SetValue("btog_version",5);
  gEnv->SetValue("btog_choice",1);
  gEnv->SetValue("btog_posit1(1)",32);
  gEnv->SetValue("btog_posit1(2)",33);
  gEnv->SetValue("btog_posit2",23);
  gEnv->SetValue("btog_version",5);
  gEnv->SetValue("calg_version",3);
  gEnv->SetValue("calg_nmodule(1)",12);
  gEnv->SetValue("calg_netaT",20);
  gEnv->SetValue("calg_maxmodule",60);
  gEnv->SetValue("calg_nsub",2);
  gEnv->SetValue("calg_netasmdp",10);
  gEnv->SetValue("calg_nphistr",15);
  gEnv->SetValue("calg_netfirst",75);
  gEnv->SetValue("calg_netsecon",75);
  return CreateGeometry("y2000");
}
