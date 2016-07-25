void PrintAliases() {
static const char *aliases[]={
   "sd97",     "sd98", "year_1a", "year_1b",  "year_1c",
   "es99",     "er99",    "dc99", "year_1d",  "year_1e",
"year_1h",  "year_2a", "year_2b","year2001", "year2003", 
 "y2003x",   "y2003a",  "y2003b",   "y2004",         0
};   

static const int   dates[]=  {
 19970101,   19980101,  19990101,  19990501,   19991001,
 19990615,   19990616,  19991206,  19991101,   19991201,
 20000614,   20010610,  20010501,  20010615,   20021115, 
 20021115,   20021115,  20021115,  20031120,         0
};

static const int   times[]=  {
        0,          0,         0,         0,          0,
        0,     120000,     80000,         0,          0,
   175430,          0,         0,         0,          0,        
        0,          0,         0,         0
};
 for (int i = 0; aliases[i]; i++) {
   cout << "\"" << aliases[i] << "\",\t  " << dates[i] << ",\t" << times[i] << "," << endl;
 }
}
