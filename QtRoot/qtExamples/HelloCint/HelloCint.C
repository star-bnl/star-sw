{
   // define debug /release extra DLL path for Win32 platform.
  TString p = "debug:release";
     p +=  gSystem->GetDynamicPath();
     gSystem->SetDynamicPath(p);
  gSystem->Load("libHelloCint");
  TMyQButton  *button= new  TMyQButton("Hello ROOT!!!");
  button->Show();
}
