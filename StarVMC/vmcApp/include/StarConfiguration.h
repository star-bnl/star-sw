// $Id: StarConfiguration.h,v 1.2 2004/07/16 22:52:53 potekhin Exp $

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

  static void     setSeed(int s_);
  static int      getSeed(void);

  static void     setExternal(void);
  static int      isExternal(void);

  static void     print(void);
 private:
  static TString _conFile;
  static TString _geoFile;
  static int     _triggers;
  static int     _seed;
  static int     _external;
};

#endif //STARCONFIGURATION_H
