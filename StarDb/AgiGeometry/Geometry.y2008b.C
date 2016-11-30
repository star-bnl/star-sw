#include "CreateGeometry.h"
TDataSet *CreateTable() {
  gEnv->SetValue("isvt",-1);
  gEnv->SetValue("itpc",-1);
  gEnv->SetValue("ibtf",8);
  gEnv->SetValue("ical",12);
  gEnv->SetValue("ivpd",-1);
  gEnv->SetValue("istb",-1);
  gEnv->SetValue("ifpd",-1);
  gEnv->SetValue("ifms",3);
  gEnv->SetValue("ifsc",-1);
  gEnv->SetValue("imtd",4);
  gEnv->SetValue("btog_version",6);
  gEnv->SetValue("btog_choice",11);
  gEnv->SetValue("btog_posit1(1)",32);
  gEnv->SetValue("btog_posit1(2)",33);
  gEnv->SetValue("btog_posit2",23);
  gEnv->SetValue("btog_posit3",33);
  gEnv->SetValue("btog_version",6);
  gEnv->SetValue("calg_version",3);
  gEnv->SetValue("calg_nmodule(1)",60);
  gEnv->SetValue("calg_nmodule(2)",60);
  gEnv->SetValue("calg_netaT",20);
  gEnv->SetValue("calg_maxmodule",60);
  gEnv->SetValue("calg_nsub",2);
  gEnv->SetValue("calg_netasmdp",10);
  gEnv->SetValue("calg_nphistr",15);
  gEnv->SetValue("calg_netfirst",75);
  gEnv->SetValue("calg_netsecon",75);
  gEnv->SetValue("emcg_version",6.1);
  gEnv->SetValue("emcg_onoff",1);
  gEnv->SetValue("emcg_fillmode",3);
  gEnv->SetValue("fmcg_version",8);
  gEnv->SetValue("mtdg_version",1);
  return CreateGeometry("y2008b");
}
