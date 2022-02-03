void rootlogon() 
{
  printf("***** BOT OHO *****\n");
const char* path[] = {
"-I $CWD",
"-I .${STAR_HOST_SYS}/include",
"-I .${STAR_TABLES}/include",
"-I StRoot",
"-I StarVMC",
"-I ${STAR}",
"-I ${STAR}/.${STAR_HOST_SYS}/include",
"-I ${STAR}/StRoot",
"-I ${STAR}/StarVMC",
"-I ${STAR}/StarEtc",
"-I ${ROOTSYS}/include",
0};

 const char *myTab = gSystem->Getenv("STAR_TABLES");
 if (!myTab) path[2] = " ";

 
  for (int i=0;path[i];i++) {
     if (path[i][0]==' ') continue;
     TString ts(path[i]+3);  
//     TString ts(path[i]);  
     gSystem->ExpandPathName(ts);
     gSystem->AddIncludePath(ts);
     gInterpreter->AddIncludePath(ts);
  };

printf("*** InterpreterInclude ***\n");
  const char* g  = 0;
  g = (const char*)gInterpreter->ProcessLine(".include");
//???  assert(g);
  printf("%s\n\n\n",g);

printf(" *** SystemInclude ***\n\n");
  g =   gSystem->GetIncludePath();
  assert(g);
  printf("%s\n\n\n",g);

  gSystem->Setenv("ROOT_INCLUDE_PATH",g);
   g = gInterpreter->GetIncludePath();
  printf("%s\n\n\n",g);

printf(" *** DynamicInclude ***\n\n");
  g =   gSystem->GetDynamicPath();
assert(g);
  printf("%s\n\n\n",g);

  gSystem->SetFlagsOpt("-g -DDEBUG");
  g = gSystem->GetFlagsOpt();
assert(g);
printf("SetFlagsOpt %s\n\n\n",g);

}
