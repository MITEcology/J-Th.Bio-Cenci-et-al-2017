require('Rcpp')
cppFunction(
'NumericMatrix SetMatrix(NumericMatrix x, NumericMatrix y, NumericVector a, NumericVector b) {
  int r = a.size(), c = b.size();
  NumericMatrix g = clone(x);
  for(int i = 0; i < r; i++){
    for(int j = 0; j < c; j++){
       g(a(i)-1,b(j) - 1) = y(i, j);
    }
  }
 return g;
}')
cppFunction(
  'NumericVector BiT_cpp(int x, int dig) {
  int i = 1;
  NumericVector st(dig);
  while(x > 0){
  st(dig - i) = x%2;
  x /= 2;
  i += 1; 
  }        
  return st;
  }')