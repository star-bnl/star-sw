{
  gSystem.Load("St_Module.dll");
  Int_t first = 111;
  Int_t second = 222;
  module = new St_Module((ULong_t *)(&first));
  module(); 
  module((ULong_t *)0,(ULong_t *)(&second)); 
  module();
}
