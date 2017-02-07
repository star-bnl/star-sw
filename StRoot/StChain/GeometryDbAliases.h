#ifndef __GeometryDbAliases__
#define __GeometryDbAliases__
static const DbAlias_t fDbAlias[] = {// geometry  Comment            old 
//{"sd97",        19970101,     0, "year2000", ""},                   //       {"sd97",        19970101,     0}
//{"sd98",        19980101,     0, "year2000", ""},                   //       {"sd98",        19980101,     0}
//{"y1a",         19990101,     0, "year2000", ""}, 
//{"year_1a",     19990101,     0, "year2000", ""},                   //       {"year_1a",     19990101,     0}
//{"y1b",         19990501,     0, "year2000", ""}, 
//{"year_1b",     19990501,     0, "year2000", ""},                   //       {"year_1b",     19990501,     0}
//{"y1s",         19990501,     0, "year2000", ""}, 
//{"year_1s",     19990501,     0, "year2000", ""}, 
//{"es99",        19990615,     0, "year2000", ""},                   //       {"es99",        19990615,     0}
//{"er99",        19990616,120000, "year2000", ""},                   //       {"er99",        19990616,120000}
//{"year_1c",     19991001,     0, "year2000", ""},                   //       {"year_1c",     19991001,     0}
//{"y1d",         19991101,     0, "year2000", ""}, 
//{"year_1d",     19991101,     0, "year2000", ""},                   //       {"year_1d",     19991101,     0}
//{"y1e",         19991201,     0, "year2000", ""}, 
//{"year_1e",     19991201,     0, "year2000", ""},                   //       {"year_1e",     19991201,     0}
//{"dc99",        19991206, 80000, "year2000", ""},                   //       {"dc99",        19991206, 80000}
  {"y1h",         20000614,175430, "year2000", ""}, 
  {"y2000",       20000614,175430, "year2000", ""},                   //       {"y2000",       20000614,175430}
//{"y2b",         20010501,     0, "year_2b",  ""},
  {"y2001",       20010501,     0, "year2001", "xgeometry"},
//{"year_2b",     20010501,     0, "year_2b",  ""},                   //       {"year_2b",     20010501,     0}
//{"year2002",    20010502,     0, "year2002", ""},
//{"y2a",         20010610,     0, "year_2a",  ""}, 
//{"year_2a",     20010610,     0, "year_2a",  ""},                   //       {"year_2a",     20010610,     0}
  {"y2001n",      20010615,     0, "year2001", "xgeometry"}, 
//{"year2001",    20010615,     0, "year2001", ""},                   //       {"year2001",    20010615,     0}
  {"y2003",       20021215,     0, "year2003", "xgeometry"}, 
//{"year2003",    20021215,     0, "year2003", ""},                   //       {"year2003",    20021215,     0}
  {"y2003x",      20021215,     1, "y2003x",   "xgeometry"},                   //       {"y2003x",      20021215,     0}
//{"y2003a",      20021215,     2, "y2003a",   ""},                   //       {"y2003a",      20021215,     0}
//{"y2003b",      20021215,     3, "y2003b",   ""},                   //       {"y2003b",      20021215,     0}
//{"y2003c",      20021215,     4, "y2003c",   "new SVT"}, 
  {"r2003",       20021120,     0, "y2003x", "xgeometry"}, 
  {"y2004",       20031215,     0, "y2004",    "xgeometry"},                   //       {"y2004",       20031120,     0}
//{"y2004x",      20031215,     1, "y2004x",   ""},                   //       {"y2004x",      20031120,     0}
  {"y2004y",      20031215,     2, "y2004y",   "xgeometry"},  
  {"y2004a",      20031215,     3, "y2004a",   "xgeometry"},                   //       {"y2004a",      20031120,     0}
//{"y2004b",      20031215,     4, "y2004b",   ""},                   //       {"y2004b",      20031120,     0}
  {"y2004c",      20031216,     0, "y2004c",   "xgeometry"},                   //       {"y2004c",      20031125,     0}
//{"y2004d",      20031216,     1, "y2004d",   "new SVT"},
  {"r2004",       20031220,     0, "y2004c",   "xgeometry"},                   //       {"y2004c",      20031125,     0}
  
  // Dead area in SSD, in version y2005f
  {"y2005x",      20041030,     0, "y2005x",   "xgeometry"},                   //       {"y2005x",      20041030,     0}
  {"y2005",       20041030,     0, "y2005",    "xgeometry"},                   //       {"y2005",       20041030,     0}
  {"y2005b",      20041101,     0, "y2005b",   "xgeometry"},                   //       {"y2005b",      20041101,     0}
//{"y2005c",      20041201,     0, "y2005c",   ""},                   //       {"y2005c",      20041201,     0}
//{"y2005d",      20041201,     1, "y2005d",   "y2005c + new SVT"},   //       {"y2005d",      20041201,     0}
//{"y2005e",      20041201,     2, "y2005e",   "y2005d + new SSD"},   //       {"y2005e",      20041201,     0}
  {"y2005f",      20041201,     3, "y2005f",   "y2005e + SSD5/CALB2,AgML,xgeometry"},//       {"y2005e",      20041201,     0}
  {"y2005g",      20041201,     4, "y2005g",   "y2005f + SVT dead material,AgML,xgeometry"},//{"y2005e",      20041201,     0}
  {"y2005h",      20041201,     5, "y2005h",   "y2005g + TPC2009 ,AgML,xgeometry"},
  {"y2005i",      20041201,     6, "y2005i",   "y2005h + ECALv6  ,AgML,xgeometry"},
  {"r2005",       20041220,     0, "y2005i",   "y2005h + ECALv6  ,AgML,xgeometry"},

  // Dead area in SSD, in version y2006b
  {"y2006",       20051201,     0, "y2006",    "base for y2006: y2005e+fixed TPC plane,AgML,xgeometry"},
//{"y2006a",      20051201,     1, "y2006a",   "y2006+new FPD"},
//{"y2006b",      20051201,     2, "y2006b",   "y2006+new FPD+SSD5/CALB2"},// code versions indicated
  {"y2006c",      20051201,     3, "y2006c",   "y2006+new FPD+SSD5/CALB2+noPMD,AgML,xgeometry"},// 
  {"y2006g",      20051201,     4, "y2006g",   "y2006c + SVT dead material,AgML,xgeometry"},
  {"y2006h",      20051201,     5, "y2006h",   "y2006g + ecal6+tpc2009(JW),AgML,xgeometry"},
  {"r2006",       20051220,     0, "y2006h",   "y2006g + ecal6+tpc2009(JW),AgML,xgeometry"},

  // in preparation
  {"y2007",       20061105,     0, "y2007",    "base geometry for y2007,AgML,xgeometry"}, // advertized simu 20061101
//{"y2007a",      20061105,     1, "y2007a",    "the material of the water channels is now carbon "}, // advertized simu 20061101
  {"y2007g",      20061105,     4, "y2007g",   "y2007b + SVT dead material,AgML,xgeometry"},
  {"y2007h",      20061105,     5, "y2007h",   "y2007g + TPC2009,AgML,xgeometry"},
  {"r2007",       20061220,     0, "y2007h",   "y2007g + TPC2009,AgML,xgeometry"},

  // SVT/SSD is out
  {"y2008",       20071101,     0, "y2008",    "base for y2008: SVT/SSD out, cone is lost,AgML,xgeometry"},
  {"y2008a",      20071101,     1, "y2008a",   "base for y2008: SVT/SSD out, cone in separate SCON,AgML,xgeometry"},
  {"y2008b",      20071101,     2, "y2008b",   "base for y2008: SVT/SSD out, latest TPC ECAL CALB,AgML,xgeometry"},
  {"y2008c",      20071101,     3, "y2008c",   "TOF fix & TPCE redused,AgML,xgeometry"},
  {"y2008d",      20071101,     4, "y2008d",   "Honey sandwich fix,AgML,xgeometry"},
  {"y2008e",      20071101,     5, "y2008e",   "LOW_EM central calorimter cuts,AgML,xgeometry"},
  {"r2008",       20071220,     0, "y2008e",   "LOW_EM central calorimter cuts,AgML,xgeometry"},
  // 

  {"y2009",       20081215,     0, "y2009",    "based on TGeomanager of YF,AgML,xgeometry"},
  {"y2009a",      20081215,     1, "y2009a",   "y2009+ecalgeo6(JW),AgML,xgeometry"},
  {"y2009b",      20081215,     2, "y2009b",   "y2009+ecalgeo6(JW) w/ old tracking cuts in eemc.,AgML,xgeometry"},
  {"y2009c",      20081215,     3, "y2009c",   "TOF fix & TPCE redused,AgML,xgeometry"},
  {"y2009d",      20081215,     4, "y2009d",   "Honey sandwich fix,AgML,xgeometry"},
  {"r2009",       20081220,     0, "y2009d",   "Honey sandwich fix,AgML,xgeometry"},

  {"y2010x",      20091215,     0, "y2010x",   "Y2010 asymptotic, AgML,xgeometry"},
  {"y2010",       20091215,     0, "y2010",    "y2009+full BTOF,AgML,xgeometry"},
  {"y2010a",      20091215,     1, "y2010a",   "y2010 production tag,AgML,xgeometry"},
  {"y2010b",      20091215,     2, "y2010b",   "TOF fix & TPCE redused,AgML,xgeometry"},
  {"y2010c",      20091215,     3, "y2010c",   "Honey sandwich fix,AgML,xgeometry"},
  {"r2010",       20091220,     0, "y2010c",   "Honey sandwich fix,AgML,xgeometry"},

  {"y2011",       20101215,     0, "y2011",    "y2011 TOF fix & TPCE redused & honey,AgML,xgeometry"},
  {"y2011a",      20101215,     1, "y2011a",   "y2011a == y2011 now ,AgML,xgeometry"},
  {"y2011b",      20101215,     2, "y2011b",   "y2011a + new TPC + Extended Cave and tunnel,xgeometry"},
  {"r2011",       20101220,     0, "y2011b",   "y2011a + new TPC + Extended Cave and tunnel,xgeometry"},

  {"y2012",       20111215,     0, "y2012",    "y2012 Very preliminary,AgML,xgeometry"},
  {"y2012a",      20111215,     1, "y2012a",   "y2012 geometry (MTD missing),AgML,xgeometry"},
  {"y2012b",      20111215,     2, "y2012b",   "y2012 geometry, now with MTD,AgML,xgeometry"},
  {"y2012c",      20111215,     3, "y2012c",   "y2012 geometry, now with MTD, Extended Cave and tunnel, new TPC,xgeometry"},
  {"r2012",       20111220,     0, "y2012c",   "y2012 geometry, now with MTD, Extended Cave and tunnel, new TPC,xgeometry"},

  {"y2013",       20121215,     0, "y2013_1", "y2013 first cut geometry,AgML,xgeometry"},
  {"y2013_1",     20121215,     1, "y2013_1", "y2013 first cut geometry with pixel,AgML,xgeometry"},
  {"y2013_2",     20121215,  1001, "y2013_2", "y2013 first cut geometry sans pixel,AgML,xgeometry"},

  {"y2013_1a",    20121215,     2, "y2013_1a", "y2013a first prod geometry, Extended Cave and tunnel, AgML,xgeometry"},
  {"y2013_2a",    20121215,  1002, "y2013_2a", "y2013a first prod geometry, Extended Cave and tunnel, sans pixel,AgML,xgeometry"},

  {"y2013_1b",    20121215,     3, "y2013_1b", "y2013b 2nd prod geometry, new TPC,           AgML,xgeometry"},
  {"y2013_2b",    20121215,  1003, "y2013_2b", "y2013b 2nd prod geometry sans pixel,new TPC, AgML,xgeometry"},

  {"y2013_1c",    20121215,     4, "y2013_1c", "y2013c, Extended Cave and tunnel,AgML,xgeometry"},
  {"y2013_2c",    20121215,  1004, "y2013_2c", "y2013c, Extended Cave and tunnel,AgML,xgeometry"},

  {"y2013_1d",    20121215,     5, "y2013_1d", "y2013d, new TPC,  Extended Cave and tunnel,xgeometry"},
  {"y2013_2d",    20121215,  1005, "y2013_2d", "y2013d, new TPC,  Extended Cave and tunnel,xgeometry"},

  {"y2013_1x",   20121215,   2001, "y2013_1x", "y2013x asymptotic geometry.             WARNING: Geometry may change between releases,AgML,xgeometry"},
  {"y2013_2x",   20121215,   3001, "y2013_2x", "y2013x asymptotic geometry sans pixel.  WARNING: Geometry may change between releases,AgML,xgeometry"},

  {"20130509.000000", 20130509, 0, "y2013_1",  "y2013 with pixel, real data,AgML,xgeometry"},
  {"20130509.000001", 20130509, 1, "y2013_1c", "y2013 with pixel, real data,AgML,xgeometry"},
  {"r2013",      20121220,      0, "y2013_2c", "y2013c asymptotic geometry sans pixel.  WARNING: Geometry may change between releases,AgML,xgeometry"},
  {"r2013_c2",   20121220,      0, "y2013_2c", "y2013c asymptotic geometry sans pixel.  WARNING: Geometry may change between releases,AgML,xgeometry"},
  {"r2013_c1",   20130509,      1, "y2013_1c", "y2013x asymptotic geometry with pixel.  WARNING: Geometry may change between releases,AgML,xgeometry"},
    

  {"y2014",       20131215,     0, "y2014",    "y2014 first cut geometry,AgML,xgeometry"},
  {"y2014a",      20131215,     1, "y2014a",   "y2014a 2014 preview geometry,AgML,xgeometry"},
  {"y2014b",      20131215,     2, "y2014b",   "y2014b production plus hcal prototype,AgML,xgeometry"},
  {"y2014c",      20131215,     3, "y2014c",   "y2014c 2014 production geometry,IST overlap correction,AgML,xgeometry"},
  {"r2014",       20131220,     0, "y2014c",   "y2014c new TPC rows,xgeometry"},
  {"y2015",       20141215,     0, "y2015",    "y2015 first cut geometry, AgML,xgeometry"},
  {"y2015a",      20141215,     1, "y2015a",   "y2015a production geometry, AgML,xgeometry"},
  {"r2015",       20141220,     0, "y2015a",   "y2015a production geometry, AgML,xgeometry"},
  {"y2015b",      20141215,     2, "y2015b",   "y2015b production geometry with MTD radii corrections, AgML,xgeometry"},
  {"y2015c",      20141215,     3, "y2015c",   "y2015c production geometry with MTD revised radii corrections, AgML,xgeometry"},
  {"y2015d",      20141215,     4, "y2015d",   "y2015d production geometry with IST overlap correction, AgML,xgeometry"},

  {"y2016",       20151215,     0, "y2016",    "y2016 development geometry, AgML,xgeometry"},   // dev2016 is deprecated for reco
  {"y2016a",      20151215,     1, "y2016a",   "y2016 production geometry, AgML,xgeometry"},   // dev2016 is deprecated for reco
  {"r2016",       20151220,     0, "y2016",    "y2016 development geometry, AgML,xgeometry"},   // dev2016 is deprecated for reco


  {"y2017",       20161215,     0, "y2017",    "y2017 development geometry, AgML,xgeometry"},   
  {"r2017",       20161220,     0, "y2017",    "y2017 development geometry, AgML,xgeometry"},   
 
  // development tags
  //  {"dev2005",     20190101,     0, "dev2005",  "non-production"},
  //  {"complete",    20190101,     1, "complete", "non-production"},
  //  {"ist1",        20190101,     2, "ist1",     "non-production"},
  //  {"pix1",        20190101,     3, "pix1",     "non-production, old is not in present starsim tags"},
  {"dev2017",     20170101,     0, "dev2017",   "dev2017, no HFT,xgeometry"}, // 
  {"dev2018",     20180101,     0, "dev2018",   "dev2018, no HFT,xgeometry"}, // 
  {"dev2019",     20190101,     0, "dev2019",   "dev2019, iTPC, Jim Thomas 05/31/16, no HFT,xgeometry"}, // 
  {"devTA",       20190101,     0, "dev2019",     "dev geo for Inner Tpc Sector Upgrade, Variant 1,xgeometry"}, // 
  {"devTB",       20190101,     0, "dev2019",     "dev geo for Inner Tpc Sector Upgrade, Variant 2,xgeometry"}, // 
  {"devTC",       20190101,     0, "dev2019",     "dev geo for Inner Tpc Sector Upgrade, Variant 3,xgeometry"}, // 
  {"devTD",       20190101,     0, "dev2019",     "dev geo for Inner Tpc Sector Upgrade, Variant 4,xgeometry"}, // 
  {"devTE",       20190101,     0, "dev2019",     "dev geo for Inner Tpc Sector Upgrade, Variant 5,xgeometry"}, // 
  {"devTF",       20190101,     0, "dev2019",     "dev geo for Inner Tpc Sector Upgrade, Variant 5,xgeometry"}, // 
  {"devTX",       20190101,     0, "dev2019",     "dev geo for Inner Tpc Sector Upgrade, Tonko 09/05/14,xgeometry"}, // 
  {"devTY",       20190101,     0, "dev2019",     "dev geo for Inner Tpc Sector Upgrade, Jim Thomas 04/02/15,xgeometry"}, // 
  {"devTZ",       20190101,     0, "dev2019",     "devTY with no HFT,xgeometry"}, // 

  //  {"upgr01",      20190101,     4, "upgr01",   ""},
  //  {"upgr02",      20190101,     5, "upgr02",   ""},
  //  {"upgr03",      20190101,     6, "upgr03",   ""},
  //  {"upgr04",      20190101,     7, "upgr04",   ""},
  //  {"upgr05",      20190101,     8, "upgr05",   ""},
  //  {"upgr06",      20190101,     9, "upgr06",   ""},  // what happened to 6? Historical not re-used
  //  {"upgr07",      20190101,    10, "upgr07",   ""},
  //  {"upgr08",      20190101,    11, "upgr08",   ""},
  //  {"upgr09",      20190101,    12, "upgr09",   ""},
  //  {"upgr10",      20190101,    13, "upgr10",   ""},
  //  {"upgr11",      20190101,    14, "upgr11",   ""},
  //  {"upgr12",      20190101,    15, "upgr12",   ""},
  //  {"upgr13",      20190101,    16, "upgr13",   ""},
  //  {"upgr14",      20190101,    17, "upgr14",   ""},
  //  {"upgr15",      20190101,    18, "upgr15",   ""},
  //  {"upgr16",      20190101,    19, "upgr16",   ""},
  //  {"upgr17",      20190101,    20, "upgr17",   ""},
  //  {"upgr16a",     20190101,    21, "upgr16a",  ""},
  // Future development:
  //  {"simpletpc",   20200102,    16, "simpletpc",""},
  //  {"upgr20",      20200102,    17, "upgr20",   "y2007 +  one TOF"}, // advertized simu 20061101
  //  {"upgr21",      20200102,    18, "upgr21",   "y2007 + full TOF"}, // advertized simu 20061101
  //  {"dev13",       20200102,    19, "dev13",    "dev geo for 2013"}, // 
  //  {"devE",        20200102,    20, "devE",     "dev geo for Erick"}, // 
  //  {"dev14",       20200102,    21, "dev14",    "dev geo for 2014,AgML,xgeometry"},  
  //  {"eStar2",      20200102,    22, "eStar2",   "dev geometry for eStar"},
  // eStar simulations starting w/ timestamp 12/10/2020
  {"eStar2",      20201210,     0, "eStar2",   "dev geometry for eStar,AgML,xgeometry"},
  {"dev2020",     20201210,     1, "dev2020",  "development geometry for 2020+,AgML,xgeometry"},

  {0,                    0,     0,        0,    0}
};
#endif /*  __GeometryDbAliases__ */
