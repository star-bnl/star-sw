{
  gSystem.Load("./.sun4x_56/lib/libbase.so");
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

}
