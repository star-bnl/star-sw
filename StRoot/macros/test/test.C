{
  gSystem.Load("St_base.so");
  Int_t first = 111;
  Int_t second = 222;
  St_Module defmodule;
  printf(" The module with NO parameter has been created: <%s> \n",defmodule.CheckParameters()? "Failed" : "Ok" );
  module = new St_Module(&first);
  printf(" Module with 1 parameters is <%s> \n",module->CheckParameters()? "Failed" : "Ok" );
  module(); 
  printf(" \n ------ \n The Warning message should follow:\n");
  defmodule((ULong_t *)0,&second); 
  printf(" \n ------ \n The Error message should follow:\n");
  printf(" Check Parameters: %d \n",defmodule->CheckParameters());
  defmodule();
  printf("  \n ------ \n 2 <good> messages expected: \n");
  module((ULong_t *)0,&second); 
  module();
  module((ULong_t *)0,&second,(ULong_t *)0,(ULong_t *)0,&first,&second); 
  module->CheckParameters();
  const Char_t *name[]= {"First", "Second"};
  module->CheckParameters(name);

  printf("Checking the post-module diagnostics\n");
  table_head_st h1; 
  table_head_st h2; 
  // Everything Ok;
  h1.maxlen=10;
  h1.nok   = 2;

  h2.maxlen=12;
  h2.nok   = 3;

  module_check = new St_Module((ULong_t *)&h1,&first,(ULong_t *)&h2,&second); 
  printf("Everything Ok!\n");
  Int_t res = module_check();
  module_check->CheckResults(res,name);
  
  h1.nok = h1.maxlen;
  printf("First header is wrong!\n");
  module_check->CheckResults(res,name);
  
  h2.nok = h2.maxlen+2;
  Int_t res = module_check();
  printf("First header and second one are wrong!\n");
  module_check->CheckResults(res,name);
  delete module_check;
  delete module;
}
