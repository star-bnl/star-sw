// $Id: StarConfiguration.h,v 1.1 2004/07/12 20:35:58 potekhin Exp $

#ifndef STARCONFIGURATION_H
#define STARCONFIGURATION_H

#include <TString.h>

class StarConfiguration
{
 public:
  StarConfiguration() {};
  StarConfiguration(const TString& cf_);

  static void     setConFile(const TString& cf_);
  static void     parse(void);

  static void     setGeoFile(const TString& gf_);
  static TString  getGeoFile(void);

  static void     setTriggers(int n_);
  static int      getTriggers(void);

  static void     setExternal(void);
  static int      isExternal(void);

  static void     print(void);
 private:
  static TString _conFile;
  static TString _geoFile;
  static int     _triggers;
  static int     _external;
};

#endif //STARCONFIGURATION_H
