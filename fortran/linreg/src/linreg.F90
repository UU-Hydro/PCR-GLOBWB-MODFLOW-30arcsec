program linreg
  use utilsmod, only: i4b, r8b, linear_regression, ta, logmsg
  
  implicit none
  
  integer(i4b) :: i, n
  real(r8b), dimension(:), allocatable :: x, y
  real(r8b) :: slope, yint, corr
  
  n = 15
  allocate(x(n),y(n)); i = 0
  i = i + 1; x(i) = 1.47d0; y(i) = 52.21d0
  i = i + 1; x(i) = 1.50d0; y(i) = 53.12d0
  i = i + 1; x(i) = 1.52d0; y(i) = 54.48d0
  i = i + 1; x(i) = 1.55d0; y(i) = 55.84d0
  i = i + 1; x(i) = 1.57d0; y(i) = 57.20d0
  i = i + 1; x(i) = 1.60d0; y(i) = 58.57d0
  i = i + 1; x(i) = 1.63d0; y(i) = 59.93d0
  i = i + 1; x(i) = 1.65d0; y(i) = 61.29d0
  i = i + 1; x(i) = 1.68d0; y(i) = 63.11d0
  i = i + 1; x(i) = 1.70d0; y(i) = 64.47d0
  i = i + 1; x(i) = 1.73d0; y(i) = 66.28d0
  i = i + 1; x(i) = 1.75d0; y(i) = 68.10d0
  i = i + 1; x(i) = 1.78d0; y(i) = 69.92d0
  i = i + 1; x(i) = 1.80d0; y(i) = 72.19d0
  i = i + 1; x(i) = 1.83d0; y(i) = 74.46d0
  !
  call linear_regression(x, y, slope, yint, corr)
  !
  call logmsg('Slope, u-interception, correlation :'//ta((/slope, yint, corr/)))
  
end program














