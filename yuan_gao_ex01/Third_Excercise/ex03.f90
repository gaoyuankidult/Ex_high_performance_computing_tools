!Problem 3. (8 points)
!Check what intrinsic math functions in the following list support complex
!arguments:
!sin, tan, sinh, tanh, exp, log, bessel_j01

program check_avability
	implicit none
	integer, parameter :: dp = &
	selected_real_kind(12)
	integer,parameter :: i = 42
	real,parameter :: x = 3.14
	complex(dp)::complex_example
	complex_example=(i,x)
	print *, 'complex_example',complex_example
	print *, 'sin',sin(complex_example) 
	print *, 'tan',tan(complex_example)
	print *, 'sinh',sinh(complex_example)
	print *, 'tanh',tanh(complex_example)
	print *, 'exp',exp(complex_example)
	print *, 'log',log(complex_example)
	!print *, 'bessel_j0',bessel_j0(complex_example)
end program check_avability

!Answer
!	sin, tan, sinh, tanh, exp and log support the complex numbers
!	bessel_j01 only supports real numbers
