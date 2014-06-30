; [Function "True" [] (Var "c_1"),Function "c_1" ["x_3","y_4"] (Var "x_3")]

; ModuleID = 'my sweet llvm'

define double* @c_1(double %x_3, double %y_4) {
entry:
  %0 = alloca double
  store double %x_3, double* %0
  %1 = alloca double
  store double %y_4, double* %1
  ret double* %0
}

define double* (double, double)* @True() {
entry:
  ret double* (double, double)* @c_1
}

