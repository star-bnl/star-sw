#include "StarConfiguration.h"
#include <iostream.h>
#include <fstream.h>

TString StarConfiguration::_conFile="";
TString StarConfiguration::_geoFile="";
int     StarConfiguration::_triggers=0;
int     StarConfiguration::_external=0;

StarConfiguration::StarConfiguration(const TString& cf_) {
  _conFile=cf_;
}
//***************************************************
void StarConfiguration::setConFile(const TString& cf_) {

  cout<<"setting configuration file "<<cf_<<endl;
  _conFile=cf_;
  cout<<"set "<<cf_<<endl;

}

void StarConfiguration::setGeoFile(const TString& gf_) {
  _geoFile=gf_;
}

TString StarConfiguration::getGeoFile() {
  return _geoFile;
}

void StarConfiguration::setTriggers(int n_) {
  _triggers=n_;
}

int StarConfiguration::getTriggers() {
  return _triggers;
}

void StarConfiguration::setExternal(void) {
  _external=1;
}

int StarConfiguration::isExternal(void) {
  return _external;
}


void StarConfiguration::print() {
  cout<<"Geometry to be read from "<<getGeoFile()<<endl;
  cout<<"Triggers "<<getTriggers()<<endl;
}


void StarConfiguration::parse() {

  ifstream configFile(_conFile);


  TString command, argument;

  command.ReadToken(configFile);
  argument.ReadToken(configFile);

  while(command.Length()) {

    if(command=="geom") {
      setGeoFile(argument);
    }

    if(command=="trig") {
      setTriggers(atoi(argument));
    }

    command.ReadToken(configFile);
    argument.ReadToken(configFile);

  }


//   cout<<"Geometry to be read from "<<geoFileName<<endl;
//   StarConfiguration::
//   cout<<"******************"<<StarConfiguration::getGeoFile()<<endl;
}
