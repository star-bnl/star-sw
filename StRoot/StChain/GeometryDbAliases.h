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
  {"y2001",       20010501,     0, "year2001", ""},
//{"year_2b",     20010501,     0, "year_2b",  ""},                   //       {"year_2b",     20010501,     0}
//{"year2002",    20010502,     0, "year2002", ""},
//{"y2a",         20010610,     0, "year_2a",  ""}, 
//{"year_2a",     20010610,     0, "year_2a",  ""},                   //       {"year_2a",     20010610,     0}
  {"y2001n",      20010615,     0, "year2001", ""}, 
//{"year2001",    20010615,     0, "year2001", ""},                   //       {"year2001",    20010615,     0}
  {"y2003",       20021115,     0, "year2003", ""}, 
//{"year2003",    20021115,     0, "year2003", ""},                   //       {"year2003",    20021115,     0}
  {"y2003x",      20021115,     1, "y2003x",   ""},                   //       {"y2003x",      20021115,     0}
//{"y2003a",      20021115,     2, "y2003a",   ""},                   //       {"y2003a",      20021115,     0}
//{"y2003b",      20021115,     3, "y2003b",   ""},                   //       {"y2003b",      20021115,     0}
//{"y2003c",      20021115,     4, "y2003c",   "new SVT"}, 
  {"y2004",       20031120,     0, "y2004",    ""},                   //       {"y2004",       20031120,     0}
//{"y2004x",      20031120,     1, "y2004x",   ""},                   //       {"y2004x",      20031120,     0}
  {"y2004y",      20031120,     2, "y2004y",   ""},  
  {"y2004a",      20031120,     3, "y2004a",   ""},                   //       {"y2004a",      20031120,     0}
//{"y2004b",      20031120,     4, "y2004b",   ""},                   //       {"y2004b",      20031120,     0}
  {"y2004c",      20031125,     0, "y2004c",   ",AgML"},                   //       {"y2004c",      20031125,     0}
//{"y2004d",      20031125,     1, "y2004d",   "new SVT"},
  // Dead area in SSD, in version y2005f
  {"y2005x",      20041030,     0, "y2005x",   ""},                   //       {"y2005x",      20041030,     0}
  {"y2005",       20041030,     0, "y2005",    ""},                   //       {"y2005",       20041030,     0}
  {"y2005b",      20041101,     0, "y2005b",   ",AgML"},                   //       {"y2005b",      20041101,     0}
//{"y2005c",      20041201,     0, "y2005c",   ""},                   //       {"y2005c",      20041201,     0}
//{"y2005d",      20041201,     1, "y2005d",   "y2005c + new SVT"},   //       {"y2005d",      20041201,     0}
//{"y2005e",      20041201,     2, "y2005e",   "y2005d + new SSD"},   //       {"y2005e",      20041201,     0}
  {"y2005f",      20041201,     3, "y2005f",   "y2005e + SSD5/CALB2,AgML"},//       {"y2005e",      20041201,     0}
  {"y2005g",      20041201,     4, "y2005g",   "y2005f + SVT dead material,AgML"},//{"y2005e",      20041201,     0}
  {"y2005h",      20041201,     5, "y2005h",   "y2005g + TPC2009 ,AgML"},
  {"y2005i",      20041201,     6, "y2005i",   "y2005h + ECALv6  ,AgML"},

  // Dead area in SSD, in version y2006b
  {"y2006",       20051201,     0, "y2006",    "base for y2006: y2005e+fixed TPC plane,AgML"},
//{"y2006a",      20051201,     1, "y2006a",   "y2006+new FPD"},
//{"y2006b",      20051201,     2, "y2006b",   "y2006+new FPD+SSD5/CALB2"},// code versions indicated
  {"y2006c",      20051201,     3, "y2006c",   "y2006+new FPD+SSD5/CALB2+noPMD,AgML"},// 
  {"y2006g",      20051201,     4, "y2006g",   "y2006c + SVT dead material,AgML"},
  {"y2006h",      20051201,     5, "y2006h",   "y2006g + ecal6+tpc2009(JW),AgML"},

  // in preparation
  {"y2007",       20061105,     0, "y2007",    "base geometry for y2007,AgML"}, // advertized simu 20061101
//{"y2007a",      20061105,     1, "y2007a",    "the material of the water channels is now carbon "}, // advertized simu 20061101
  {"y2007g",      20061105,     4, "y2007g",   "y2007b + SVT dead material,AgML"},
  {"y2007h",      20061105,     5, "y2007h",   "y2007g + TPC2009,AgML"},

  // SVT/SSD is out
  {"y2008",       20071101,     0, "y2008",    "base for y2008: SVT/SSD out, cone is lost,AgML"},
  {"y2008a",      20071101,     1, "y2008a",   "base for y2008: SVT/SSD out, cone in separate SCON,AgML"},
  {"y2008b",      20071101,     2, "y2008b",   "base for y2008: SVT/SSD out, latest TPC ECAL CALB,AgML"},
  {"y2008c",      20071101,     3, "y2008c",   "TOF fix & TPCE redused,AgML"},
  {"y2008d",      20071101,     4, "y2008d",   "Honey sandwich fix,AgML"},
  {"y2008e",      20071101,     5, "y2008e",   "LOW_EM central calorimter cuts,AgML"},
  // 

  {"y2009",       20081215,     0, "y2009",    "based on TGeomanager of YF,AgML"},
  {"y2009a",      20081215,     1, "y2009a",   "y2009+ecalgeo6(JW),AgML"},
  {"y2009b",      20081215,     2, "y2009b",   "y2009+ecalgeo6(JW) w/ old tracking cuts in eemc.,AgML"},
  {"y2009c",      20081215,     3, "y2009c",   "TOF fix & TPCE redused,AgML"},
  {"y2009d",      20081215,     4, "y2009d",   "Honey sandwich fix,AgML"},

  {"y2010x",      20091215,     0, "y2010x",   "Y2010 asymptotic, AgML"},
  {"y2010",       20091215,     0, "y2010",    "y2009+full BTOF,AgML"},
  {"y2010a",      20091215,     1, "y2010a",   "y2010 production tag,AgML"},
  {"y2010b",      20091215,     2, "y2010b",   "TOF fix & TPCE redused,AgML"},
  {"y2010c",      20091215,     3, "y2010c",   "Honey sandwich fix,AgML"},

  {"y2011",       20101215,     0, "y2011",    "y2011 TOF fix & TPCE redused & honey,AgML"},
  {"y2011a",      20101215,     1, "y2011a",   "y2011a == y2011 now ,AgML"},
  {"y2011b",      20101215,     2, "y2011b",   "y2011a + wall "},
  {"y2011c",      20101215,     3, "y2011c",   "same as y2011b"},

  {"y2012",       20111215,     0, "y2012",    "y2012 Very preliminary,AgML"},
  {"y2012a",      20111215,     1, "y2012a",   "y2012 geometry (MTD missing),AgML"},
  {"y2012b",      20111215,     2, "y2012b",   "y2012 geometry (now with MTD),AgML"},

  {"y2013",       20121215,     0, "y2013_1", "y2013 first cut geometry,AgML,xgeometry"},
  {"y2013_1",     20121215,     1, "y2013_1", "y2013 first cut geometry with pixel,AgML,xgeometry"},
  {"y2013_2",     20121215,  1001, "y2013_2", "y2013 first cut geometry sans pixel,AgML,xgeometry"},

  {"y2013_1a",    20121215,     2, "y2013_1a", "y2013a first prod geometry,           AgML,xgeometry"},
  {"y2013_2a",    20121215,  1002, "y2013_2a", "y2013a first prod geometry sans pixel,AgML,xgeometry"},

  {"y2013_1b",    20121215,     3, "y2013_1b", "y2013b 2nd prod geometry,            AgML,xgeometry"},
  {"y2013_2b",    20121215,  1003, "y2013_2b", "y2013b 2nd prod geometry sans pixel, AgML,xgeometry"},

  {"y2013_1c",    20121215,     4, "y2013_1c", "y2013c production geometry == y2013x in SL14a (as run)"},
  {"y2013_2c",    20121215,  1004, "y2013_2c", "y2013c production geometry == y2013x in SL14a (as run)"},

  {"y2013_1x",   20121215,   2001, "y2013_1x", "y2013x asymptotic geometry.             WARNING: Geometry may change between releases."},
  {"y2013_2x",   20121215,   3001, "y2013_2x", "y2013x asymptotic geometry sans pixel.  WARNING: Geometry may change between releases."},

  {"20130509.000000", 20130509, 0, "y2013_1",  "y2013 with pixel, real data,AgML,xgeometry"},
  {"20130509.000000", 20130509, 1, "y2013_1c", "y2013 with pixel, real data,AgML,xgeometry"},

  {"y2014x",      20131215,     0, "y2014x",   "y2014x 2014 asymptocit geometry,IST overlap correction,AgML,xgeometry"},
  {"y2014",       20131215,     0, "y2014",    "y2014 first cut geometry,AgML,xgeometry"},
  {"y2014a",      20131215,     1, "y2014a",   "y2014a 2014 production geometry,AgML,xgeometry"},
  {"y2014b",      20131215,     2, "y2014b",   "y2014b 2014 production geometry,AgML,xgeometry"},
  {"y2014c",      20131215,     3, "y2014c",   "y2014c 2014 production geometry,IST overlap correction,AgML,xgeometry"},


  {"y2015x",      20141215,     0, "y2015x",   "y2015x asymptotic geometry with IST overlap correction, AgML,xgeometry"},
  {"y2015",       20141215,     0, "y2015",    "y2015 first cut geometry, AgML,xgeometry"},
  {"y2015a",      20141215,     1, "y2015a",   "y2015a production geometry, AgML,xgeometry"},
  {"y2015b",      20141215,     2, "y2015b",   "y2015b production geometry with MTD radii corrections, AgML,xgeometry"},
  {"y2015c",      20141215,     3, "y2015c",   "y2015c production geometry with MTD revised radii corrections, AgML,xgeometry"},
  {"y2015d",      20141215,     4, "y2015d",   "y2015d production geometry with IST overlap correction, AgML,xgeometry"},


  {"y2016x",      20151215,     0, "y2016x",   "y2016 asymptotic geometry, AgML,xgeometry"},   // dev2016 is deprecated for reco
  {"y2016",       20151215,     0, "y2016",    "y2016 development geometry, AgML,xgeometry"},   // dev2016 is deprecated for reco
  {"y2016a",      20151215,     1, "y2016a",   "y2016 production geometry, AgML,xgeometry"},   // dev2016 is deprecated for reco


  {"y2017",       20161215,     0, "y2017",    "y2017 development geometry, AgML,xgeometry"},   
  {"y2017a",      20161215,     1, "y2017a",   "y2017 production  geometry, AgML,xgeometry"},   

 
  {"y2018x",      20171215,     0, "y2018x",   "y2018x experimental TPC geometry, AgML,xgeometry"},   
  {"y2018",       20171215,     2, "y2018",    "y2018 development geometry, AgML,xgeometry"},   
  {"y2018a",      20171215,     3, "y2018a",   "y2018a physics production geometry version A, AgML,xgeometry"},   

  {"y2019",       20181215,     0, "y2019",    "y2019 development geometry, AgML,xgeometry"},   
  {"y2019a",      20181215,     1, "y2019a",   "y2019a first production release geometry, AgML,xgeometry"},   

  {"y2020",       20191120,     0, "y2020",    "y2020 development geometry, AgML,xgeometry"},   
  {"y2020a",      20191120,     1, "y2020a",   "y2020 fast-offline geometry, AgML,xgeometry"},   
  
  {"dev2021",     20211210,     1, "dev2021",  "development geometry for 2021+ forward program,AgML,xgeometry"},

  //
  // Move DEVT and upgrade series +100 years along timeline else they interfere with y2018+ runs
  //
  {"devT",        21170101,     0, "devT",      "dev geo for Inner Tpc Sector Upgrade, Variant 1,AgML,xgeometry"}, // 
  {"devTA",       21170101,     1, "devTA",     "dev geo for Inner Tpc Sector Upgrade, Variant 1"}, // 
  {"devTB",       21170101,     2, "devTB",     "dev geo for Inner Tpc Sector Upgrade, Variant 2"}, // 
  {"devTC",       21170101,     3, "devTC",     "dev geo for Inner Tpc Sector Upgrade, Variant 3"}, // 
  {"devTD",       21170101,     4, "devTD",     "dev geo for Inner Tpc Sector Upgrade, Variant 4"}, // 
  {"devTE",       21170101,     5, "devTE",     "dev geo for Inner Tpc Sector Upgrade, Variant 5"}, // 
  {"devTF",       21170101,     6, "devTF",     "dev geo for Inner Tpc Sector Upgrade, Variant 5"}, // 

  {"upgr01",      21190101,     4, "upgr01",   ""},
  {"upgr02",      21190101,     5, "upgr02",   ""},
  {"upgr03",      21190101,     6, "upgr03",   ""},
  {"upgr04",      21190101,     7, "upgr04",   ""},
  {"upgr05",      21190101,     8, "upgr05",   ""},
  {"upgr06",      21190101,     9, "upgr06",   ""},  // what happened to 6? Historical not re-used
  {"upgr07",      21190101,    10, "upgr07",   ""},
  {"upgr08",      21190101,    11, "upgr08",   ""},
  {"upgr09",      21190101,    12, "upgr09",   ""},
  {"upgr10",      21190101,    13, "upgr10",   ""},
  {"upgr11",      21190101,    14, "upgr11",   ""},
  {"upgr12",      21190101,    15, "upgr12",   ""},
  {"upgr13",      21190101,    16, "upgr13",   ""},
  {"upgr14",      21190101,    17, "upgr14",   ""},
  {"upgr15",      21190101,    18, "upgr15",   ""},
  {"upgr16",      21190101,    19, "upgr16",   ""},
  {"upgr17",      21190101,    20, "upgr17",   ""},
  {"upgr16a",     21190101,    21, "upgr16a",  ""},
  // Future development:
  //  {"simpletpc",   20200102,    16, "simpletpc",""},
  //  {"upgr20",      20200102,    17, "upgr20",   "y2007 +  one TOF"}, // advertized simu 20061101
  //  {"upgr21",      20200102,    18, "upgr21",   "y2007 + full TOF"}, // advertized simu 20061101
  //  {"dev13",       20200102,    19, "dev13",    "dev geo for 2013"}, // 
  //  {"devE",        20200102,    20, "devE",     "dev geo for Erick"}, // 
  {"dev14",       21200102,    21, "dev14",    "dev geo for 2014,AgML,xgeometry"},  
  //  {"eStar2",      20200102,    22, "eStar2",   "dev geometry for eStar"},
  // eStar simulations starting w/ timestamp 12/10/2020
  {"eStar2",      21201210,     0, "eStar2",   "dev geometry for eStar,AgML,xgeometry"},
  {"dev2020",     21201210,     1, "dev2020",  "development geometry for 2020+,AgML,xgeometry"},

  {0,                    0,     0,        0,    0}
};
#endif /*  __GeometryDbAliases__ */
