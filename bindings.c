#include <stdio.h>

extern double putchard(double x) {
  putchar((char) x);
  return 0;  
}
