MODULE parameters
  USE HTYPE
  
  IMPLICIT NONE
  REAL(DP), PARAMETER :: x0 = -3.0
  REAL(DP), PARAMETER :: xf = 3.0

  REAL(DP), PARAMETER :: y0 = -3.0
  REAL(DP), PARAMETER :: yf = 3.0


  INTEGER(DP), PARAMETER :: N = 2**11


  REAL(DP), PARAMETER :: h = (xf - x0)/REAL(N)
  REAL(DP), PARAMETER :: dt  = .3*h

  !FFTW SPECIFIC
  INTEGER(DP), PARAMETER :: Nc = N/2 + 1
  INTEGER, PARAMETER :: fftw_estimate = 64
  
  !MR PARAMETERS
  INTEGER(DP), PARAMETER :: N_root = 2**3
  INTEGER(DP), PARAMETER :: L_max = 8
  REAL(DP), PARAMETER :: h_root = (xf-x0)/REAL(N_root)
  REAL(DP), PARAMETER :: h_min = h_root*2.0**(-L_max)
  REAL(DP), PARAMETER :: eps = 10.**(-6)
  REAL(DP), PARAMETER :: zero = 10.**(-10)


ENDMODULE parameters
