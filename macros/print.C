void print() {
  Int_t N =32;
  Double_t x[32], p[32];
  for (int i=0; i<N; i++) {
    for (int j=0; j<N;j++) {
      x[j]=0; 
      if(i==j) x[j]=1;
    }
    X2P(x,p); 
    printf("%i:",i);
    for(j=0;j<N;j++) printf(" %5.2f",p[j]);
    printf("\n");
  }
}
