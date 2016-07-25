//LI_weather.C
//
// Display local weather
//
{
  gROOT->Reset();

  // Display local weather
  // this is the URL
  Char_t URL[] = "http://www.weather.com/images/radar/single_site/lgaloc_450x284.gif";

  Char_t UniqueFileName[100];
  Char_t ExecCmd[100];

  TUrl u(URL);

  TSocket s(u.GetHost(), u.GetPort());
  if (!s.IsValid())
    return ;

  const Int_t kMsgLength = 1000;
  Char_t msg[kMsgLength];

  const Int_t kLength = 500;//00; // should be enough to store the GIF
  Char_t Buf[kLength];

  // Construct the HTTP request
  sprintf(msg, "GET %s HTTP/1.0\rUser-Agent: ILikeMyself/99.3\rHost: %s:%d\n\rAccept: */*\n\r\n\r", u.GetFile(), u.GetHost(), u.GetPort());

  s.SendRaw(msg, strlen(msg));
  s.RecvRaw(Buf, kLength);
  s.Close();

  Int_t length;
  Char_t dummy[100];
  // Get the length of the GIF file
  printf("Buf:%s\n",Buf);
  sscanf(strstr(Buf, "Content-length: "), "%s %d", dummy, &length);

  // Find the beginning of the GIF
  Char_t *gif = strstr(Buf, "\r\n\r\n")+4;

  // Make a unique file name
  sprintf(UniqueFileName, "/tmp/%f.gif", gRandom->Gaus(10, 0.2));

  FILE *handle = fopen(UniqueFileName, "w");
  fwrite(gif, length, sizeof(Char_t), handle);
  fclose(handle);

  // Display GIF
  TCanvas *c = new TCanvas("Browser");
  c->Update();
  gGXW->ReadGIF(50, 100, UniqueFileName);
  c->Update();
  sprintf(ExecCmd, "rm -f %s", UniqueFileName);
  gSystem->Exec(ExecCmd);
}

